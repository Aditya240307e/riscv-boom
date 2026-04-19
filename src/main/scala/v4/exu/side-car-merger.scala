package boom.v4.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import boom.v4.common._
import boom.v4.util._

class SidecarMerger(implicit p: Parameters)
    extends BoomModule
    with HasBoomCoreParameters {
  val io = IO(new Bundle {
    val sidecar_res = Flipped(Decoupled(new ExeUnitResp(xLen)))
    val main_wb_valid =
      Input(Bool()) // Keep for IO compatibility, though less critical now
    val br_update = Input(new BrUpdateInfo())

    // out_wb is repurposed: bits.uop will be used for Re-injection
    val out_wb = Decoupled(new ExeUnitResp(xLen))

    val rob_done = Output(Valid(new Bundle {
      val rob_idx = UInt(robAddrSz.W)
    }))

    val buffer_critical = Output(Bool())
  })

  // 1. Re-injection Buffer
  // Decouples Sidecar timing from Issue Unit arbitration
  val res_fifo = Module(new Queue(new ExeUnitResp(xLen), 16))
  res_fifo.io.enq <> io.sidecar_res

  // 2. Kill Logic
  // Check if the instruction was killed by a mispredict while sitting in this buffer
  val head_uop = res_fifo.io.deq.bits.uop
  val head_killed = (io.br_update.b1.mispredict_mask & head_uop.br_mask) =/= 0.U

  // 3. Re-injection Logic
  // We present the instruction to the IssueUnit.
  // We only signal valid if the instruction is actually alive.
  io.out_wb.valid := res_fifo.io.deq.valid && !head_killed
  io.out_wb.bits := res_fifo.io.deq.bits
  // Ensure the mask is fully cleared for the main pipeline
  io.out_wb.bits.uop.br_mask := head_uop.br_mask & ~io.br_update.b1.resolve_mask

  // 4. Handshake
  // Pop if the IssueUnit accepts it OR if the branch mispredict killed it
  res_fifo.io.deq.ready := io.out_wb.ready || head_killed

  // 5. Status & ROB
  io.buffer_critical := res_fifo.io.count > 12.U

  // Signal to the ROB/Core that this instruction has "exited" the Sidecar phase
  io.rob_done.valid := io.out_wb.fire
  io.rob_done.bits.rob_idx := head_uop.rob_idx
}
