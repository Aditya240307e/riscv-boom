package boom.v4.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import boom.v4.common._
import boom.v4.util._

class SidecarUnit(implicit p: Parameters) extends BoomModule with HasBoomCoreParameters {
  val io = IO(new Bundle {
    //Input from Dispatcher (IQ_VETO lane)
    val dis_uops = Flipped(Decoupled(new MicroOp()))
    
    //Read Ports from the PRF (to get operand values)
    val iss_resps = Flipped(new Bundle {
        val rs1_data = Input(UInt(xLen.W))
        val rs2_data = Input(UInt(xLen.W))
    })

    //Output to the Merger
    val sidecar_res = Decoupled(new ExeUnitResp(xLen))
  })

  //A small FIFO to hold uops while they wait for operands
  val task_queue = Module(new Queue(new MicroOp(), 8))
  task_queue.io.enq <> io.dis_uops

  //Simple ALU Logic for Vetoed instructions
  val uop = task_queue.io.deq.bits
  val rs1 = io.iss_resps.rs1_data
  val rs2 = io.iss_resps.rs2_data

  val alu_out = Mux(uop.fcn_dw === DW_64, rs1 + rs2, (rs1 + rs2)(31,0)) 

  //Drive the response to the Merger
  io.sidecar_res.valid := task_queue.io.deq.valid
  io.sidecar_res.bits.uop := uop
  io.sidecar_res.bits.data := alu_out
  
  //Ready when the Merger's buffer can take it
  task_queue.io.deq.ready := io.sidecar_res.ready
}
