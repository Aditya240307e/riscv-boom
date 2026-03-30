package boom.v4.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import boom.v4.common._
import boom.v4.util._
import boom.v4.util.IsKilledByBranch
import freechips.rocketchip.regmapper.RegField.w

class SidecarUnit(implicit p: Parameters)
    extends BoomModule
    with HasBoomCoreParameters {
  val io = IO(new Bundle {
    val dis_uops = Flipped(Decoupled(new MicroOp()))
    val iss_resps = Flipped(new Bundle {
      val rs1_data = Input(UInt(xLen.W))
      val rs2_data = Input(UInt(xLen.W))
    })
    val sidecar_res = Decoupled(new ExeUnitResp(xLen))
    val br_update = Input(new BrUpdateInfo())
  })

  val task_queue = Module(new Queue(new MicroOp(), 8))
  task_queue.io.enq <> io.dis_uops

  val uop_reg = Reg(new MicroOp())
  val rs1_reg = Reg(UInt(xLen.W))
  val rs2_reg = Reg(UInt(xLen.W))
  val val_reg = RegInit(false.B)

  val deq_uop = task_queue.io.deq.bits
  val deq_killed = (io.br_update.b1.mispredict_mask & deq_uop.br_mask) =/= 0.U
  val reg_killed = (io.br_update.b1.mispredict_mask & uop_reg.br_mask) =/= 0.U

  val next_br_mask = GetNewBrMask(io.br_update, uop_reg.br_mask)

  when(task_queue.io.deq.fire) {
    uop_reg := deq_uop
    uop_reg.br_mask := GetNewBrMask(io.br_update, deq_uop.br_mask)
    rs1_reg := io.iss_resps.rs1_data
    rs2_reg := io.iss_resps.rs2_data
    val_reg := !deq_killed
  }.elsewhen(val_reg) {
    uop_reg.br_mask := next_br_mask

    when(io.sidecar_res.ready || reg_killed) {
      val_reg := false.B
    }
  }

  val sum = rs1_reg + rs2_reg

  val alu_out = Mux(uop_reg.fcn_dw === DW_64, sum, sum(31, 0).asSInt.asUInt)

  io.sidecar_res.valid := val_reg && !reg_killed
  io.sidecar_res.bits.uop := uop_reg
  io.sidecar_res.bits.uop.br_mask := next_br_mask
  io.sidecar_res.bits.data := alu_out

  task_queue.io.deq.ready := !val_reg || io.sidecar_res.ready
}
