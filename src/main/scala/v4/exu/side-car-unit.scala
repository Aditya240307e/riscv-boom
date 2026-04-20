package boom.v4.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import boom.v4.common._
import boom.v4.util._

class SidecarUnit(implicit p: Parameters)
    extends BoomModule
    with HasBoomCoreParameters {
  val numIntWakeups = coreWidth + lsuWidth + 1
  val numVetoWakeups = numIntWakeups + 1
  val io = IO(new Bundle {
    val dis_uops = Flipped(Decoupled(new MicroOp()))
    val enq = Vec(numVetoWakeups, Flipped(Valid(new Wakeup())))
    val iss_resps = new Bundle {
      val rs1_ready = Input(Bool())
      val rs2_ready = Input(Bool())
      val rs1_data = Input(UInt(xLen.W))
      val rs2_data = Input(UInt(xLen.W))
      val task_queue_head_valid = Output(Bool())
    }
    val sidecar_res = Decoupled(new ExeUnitResp(xLen))
    val br_update = Input(new BrUpdateInfo())
    val veto_trigger = Output(Bool())
    val veto_threshold = Input(UInt(16.W))
    val veto_enable = Input(Bool())
    val deq_uop = Output(new MicroOp)
    val task_queue_head_valid = Output(Bool())
  })

  val q_depth = 32
  val q_uops = Reg(Vec(q_depth, new MicroOp()))
  val q_val = RegInit(VecInit(Seq.fill(q_depth)(false.B)))

  val q_tail = PriorityEncoder(q_val.map(!_) :+ true.B)
  val q_full = q_val.reduce(_ && _)

  val head_uop = q_uops(0)
  val head_valid = q_val(0)

  val next_br_mask = GetNewBrMask(io.br_update, head_uop.br_mask)

  val is_safe = head_valid && (next_br_mask === 0.U)

  val op0_killed =
    head_valid && ((io.br_update.b1.mispredict_mask & head_uop.br_mask) =/= 0.U)

  val do_enq = io.dis_uops.fire
  val do_deq = (io.sidecar_res.ready && is_safe) || op0_killed

  io.dis_uops.ready := !q_full
  io.sidecar_res.valid := is_safe && !op0_killed
  io.sidecar_res.bits.uop := head_uop

  // Note: io.sidecar_res.bits.uop.is_tainted is now driven by the registered
  // value updated in the loop below to ensure timing closure.

  io.sidecar_res.bits.uop.br_mask := next_br_mask
  io.sidecar_res.bits.data := 0.U // Main ALU will compute data

  for (i <- 0 until q_depth) {
    val current_uop = q_uops(i)
    val current_val = q_val(i)

    val updated_mask = GetNewBrMask(io.br_update, current_uop.br_mask)
    val killed =
      current_val && ((io.br_update.b1.mispredict_mask & current_uop.br_mask) =/= 0.U)

    when(do_deq) {
      if (i < q_depth - 1) {
        q_uops(i) := q_uops(i + 1)
        q_uops(i).br_mask := GetNewBrMask(io.br_update, q_uops(i + 1).br_mask)
        q_val(i) := q_val(i + 1) && !((io.br_update.b1.mispredict_mask & q_uops(
          i + 1
        ).br_mask) =/= 0.U)
      } else {
        q_val(i) := false.B
      }
    }.elsewhen(do_enq && q_tail === i.U) {
      q_uops(i) := io.dis_uops.bits
      q_uops(i).br_mask := GetNewBrMask(io.br_update, io.dis_uops.bits.br_mask)
      q_val(i) := true.B
    }.otherwise {
      q_uops(i).br_mask := updated_mask

      // PRODUCTION SIGN-OFF FIX: Clear the taint bit early in the register state.
      // This ensures the signal is stable and registered before re-injection.
      when(updated_mask === 0.U) {
        q_uops(i).is_tainted := false.B
      }

      when(killed) { q_val(i) := false.B }
    }
  }

  io.task_queue_head_valid := head_valid
  io.iss_resps.task_queue_head_valid := head_valid
  io.deq_uop := head_uop

  val veto_counter = RegInit(0.U(16.W))

  val is_stalled = head_valid && !is_safe && !op0_killed

  when(is_stalled && io.veto_enable) {
    veto_counter := Mux(
      veto_counter === 0xffff.U,
      veto_counter,
      veto_counter + 1.U
    )
  }.otherwise {
    veto_counter := 0.U
  }

  val timeout_fired =
    (veto_counter >= io.veto_threshold) && (io.veto_threshold =/= 0.U)
  io.veto_trigger := timeout_fired && io.veto_enable && head_valid
}
