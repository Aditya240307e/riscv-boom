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
    // We repurpose sidecar_res to be our Re-injection Port
    val sidecar_res = Decoupled(new ExeUnitResp(xLen))
    val br_update = Input(new BrUpdateInfo())
    val veto_trigger = Output(Bool())
    val veto_threshold = Input(UInt(16.W))
    val veto_enable = Input(Bool())
    val deq_uop = Output(new MicroOp)
    val task_queue_head_valid = Output(Bool())
  })

  // 1. The Holding Pen (FIFO Buffer)
  // 16 entries to handle the "Burst" release without backpressuring dispatch
  val task_queue = Module(
    new Queue(new MicroOp(), 16, pipe = true, flow = true)
  )
  task_queue.io.enq <> io.dis_uops

  // 2. Head-of-Line Logic
  val head_uop = task_queue.io.deq.bits
  val head_valid = task_queue.io.deq.valid

  // Branch Mask Update: Calculate what the mask WILL be in the next cycle
  val next_br_mask = GetNewBrMask(io.br_update, head_uop.br_mask)

  // An instruction is "Safe" if its branch mask is zeroed out by resolutions
  val is_safe = head_valid && (next_br_mask === 0.U)

  // An instruction is "Killed" if its mask matches a misprediction
  val op0_killed =
    head_valid && ((io.br_update.b1.mispredict_mask & head_uop.br_mask) =/= 0.U)

  // 3. Re-injection Handshake (Repurposing sidecar_res)
  // We only signal valid if the instruction is Safe and NOT killed.
  io.sidecar_res.valid := is_safe && !op0_killed
  io.sidecar_res.bits.uop := head_uop
  io.sidecar_res.bits.uop.br_mask := 0.U // Strip the mask; it is now non-speculative
  io.sidecar_res.bits.data := 0.U // No data needed; main ALU will compute it

  // Handshake Logic:
  // Pop the queue if the Core accepted the re-injection OR if the instruction was killed.
  task_queue.io.deq.ready := (io.sidecar_res.ready && is_safe) || op0_killed

  // 4. Output Wiring for Metadata Compatibility
  io.task_queue_head_valid := head_valid
  io.iss_resps.task_queue_head_valid := head_valid
  io.deq_uop := head_uop

  // 5. Veto Logic (Inhibition Counter)
  val veto_counter = RegInit(0.U(16.W))

  // Stall occurs when the head is valid but is still speculative (not safe) and not yet killed
  val is_stalled = head_valid && !is_safe && !op0_killed

  when(is_stalled && io.veto_enable) {
    veto_counter := veto_counter + 1.U
  }.otherwise {
    veto_counter := 0.U
  }

  val timeout_fired = veto_counter > io.veto_threshold
  io.veto_trigger := timeout_fired && io.veto_enable
}
