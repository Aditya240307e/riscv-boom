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
    val main_wb_valid = Input(Bool())
    val br_update = Input(new BrUpdateInfo()) // Required for correctness

    val out_wb = Valid(new ExeUnitResp(xLen))
    val rob_done = Output(Valid(new Bundle {
      val rob_idx = UInt(robAddrSz.W)
    }))

    val buffer_critical = Output(Bool())
  })

  // 1. Result Buffer
  val res_fifo = Module(new Queue(new ExeUnitResp(xLen), 16))
  res_fifo.io.enq <> io.sidecar_res

  // 2. Manual Kill Logic (Avoids the "overloaded method" compiler error)
  val head_uop = res_fifo.io.deq.bits.uop
  // An instruction is killed if any bit in its mask matches the mispredicted branch mask
  val head_killed =
    (io.br_update.b1.mispredict_mask & head_uop.br_mask) =/= 0.U

  // 3. The "Steal" Logic
  // We pop the FIFO if the main ALU is idle OR if the head instruction is dead
  val can_steal = res_fifo.io.deq.valid && (!io.main_wb_valid || head_killed)

  io.buffer_critical := res_fifo.io.count > 12.U

  // 4. Drive Writeback to PRF
  // Only valid if we stole the cycle AND the instruction is actually alive
  io.out_wb.valid := can_steal && !head_killed
  io.out_wb.bits := res_fifo.io.deq.bits
  // Update mask for downstream logic
  io.out_wb.bits.uop.br_mask := head_uop.br_mask & ~io.br_update.b1.resolve_mask

  // 5. Handshake
  res_fifo.io.deq.ready := can_steal

  // 6. Drive ROB
  io.rob_done.valid := can_steal && !head_killed
  io.rob_done.bits.rob_idx := head_uop.rob_idx
}
