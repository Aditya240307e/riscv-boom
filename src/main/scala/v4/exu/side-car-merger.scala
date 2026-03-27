package boom.v4.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import boom.v4.common._
import boom.v4.util._

class SidecarMerger(implicit p: Parameters) extends BoomModule with HasBoomCoreParameters {
  val io = IO(new Bundle {
    //Input from the Sidecar Execution Unit
    val sidecar_res = Flipped(Decoupled(new ExeUnitResp(xLen)))

    //Monitoring the main ALU writeback port
    val main_wb_valid = Input(Bool())

    //The "Steal" Port
    val out_wb = Valid(new ExeUnitResp(xLen))

    //Notification to the ROB
    val rob_done = Output(Valid(new Bundle {
      val rob_idx = UInt(robAddrSz.W)
    }))

    val buffer_critical = Output(Bool())
  })

  //The Result Buffer
  val res_fifo = Module(new Queue(new ExeUnitResp(xLen), 16))
  res_fifo.io.enq <> io.sidecar_res

  //The "Steal" Logic
  val can_steal = res_fifo.io.deq.valid && !io.main_wb_valid

  io.buffer_critical := res_fifo.io.count > 12.U

  //Drive Output to PRF
  io.out_wb.valid := can_steal
  io.out_wb.bits  := res_fifo.io.deq.bits
  
  res_fifo.io.deq.ready := can_steal

  //Drive Output to ROB
  io.rob_done.valid        := can_steal
  io.rob_done.bits.rob_idx := res_fifo.io.deq.bits.uop.rob_idx
}
