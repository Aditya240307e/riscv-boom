package boom.v4.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import boom.v4.common._
import freechips.rocketchip.rocket.ALU._

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
    // TODO: Sensitivity analysis on veto_threshold
    val veto_threshold = Input(UInt(16.W))
    val veto_enable = Input(Bool())
    val deq_uop = Output(new MicroOp)
    val task_queue_head_valid = Output(Bool())
  })

  // task_queue remains 8 entries, but now feeds our Station
  val task_queue = Module(new Queue(new MicroOp(), 8, pipe = true, flow = true))
  task_queue.io.enq <> io.dis_uops

  // In-Order Station (2 Entries for HoL Mitigation)
  val station_uops = Reg(Vec(2, new MicroOp()))
  val station_valids = RegInit(VecInit(Seq.fill(2)(false.B)))

  // Execution Registers (Original Variables)
  val uop_reg = Reg(new MicroOp())
  val rs1_reg = Reg(UInt(xLen.W))
  val rs2_reg = Reg(UInt(xLen.W))
  val val_reg = RegInit(false.B)

  // Dependency & HoL Mitigation Logic
  // If station(0) is stalled on operands, we can't easily skip in this
  // simplified IO setup, so we treat it as a strict In-Order "Pipeline Station".
  val op0_killed =
    (io.br_update.b1.mispredict_mask & station_uops(0).br_mask) =/= 0.U
  val op0_ready = io.iss_resps.rs1_ready && io.iss_resps.rs2_ready

  // Logic to determine if we can move an instruction into the execution reg
  val can_execute =
    station_valids(0) && op0_ready && (!val_reg || io.sidecar_res.ready)

  // Branch Mask Update
  val next_br_mask = GetNewBrMask(io.br_update, uop_reg.br_mask)

  when(can_execute) {
    uop_reg := station_uops(0)
    uop_reg.br_mask := GetNewBrMask(io.br_update, station_uops(0).br_mask)
    rs1_reg := io.iss_resps.rs1_data
    rs2_reg := io.iss_resps.rs2_data
    val_reg := !op0_killed
  }.elsewhen(val_reg) {
    uop_reg.br_mask := next_br_mask
    // Clear reg if it completes or is killed by a branch
    when(
      io.sidecar_res.ready || (io.br_update.b1.mispredict_mask & uop_reg.br_mask) =/= 0.U
    ) {
      val_reg := false.B
    }
  }

  // Station Management (Shift Logic)
  task_queue.io.deq.ready := !station_valids(0) || (!station_valids(
    1
  ) && can_execute)

  when(reset.asBool || io.br_update.b1.mispredict_mask =/= 0.U) {
    station_valids.foreach(_ := false.B)
  }.otherwise {
    when(can_execute) {
      // Shift: station(1) moves to (0), Queue moves to (1)
      station_valids(0) := station_valids(
        1
      ) && !((io.br_update.b1.mispredict_mask & station_uops(
        1
      ).br_mask) =/= 0.U)
      station_uops(0) := station_uops(1)
      station_valids(
        1
      ) := task_queue.io.deq.valid && !((io.br_update.b1.mispredict_mask & task_queue.io.deq.bits.br_mask) =/= 0.U)
      station_uops(1) := task_queue.io.deq.bits
    }.elsewhen(!station_valids(0)) {
      // Fill empty slot 0
      station_valids(0) := task_queue.io.deq.valid
      station_uops(0) := task_queue.io.deq.bits
      station_valids(1) := false.B
    }.elsewhen(!station_valids(1)) {
      // Fill empty slot 1
      station_valids(1) := task_queue.io.deq.valid
      station_uops(1) := task_queue.io.deq.bits
    }
    // Update masks for instructions sitting in the station
    station_uops(0).br_mask := GetNewBrMask(
      io.br_update,
      station_uops(0).br_mask
    )
    station_uops(1).br_mask := GetNewBrMask(
      io.br_update,
      station_uops(1).br_mask
    )
  }

  // ALU unit
  // Only supports ADD, SUB, AND, OR, XOR
  val alu_out = MuxLookup(uop_reg.fcn_op, rs1_reg + rs2_reg)(
    Seq(
      FN_ADD -> (rs1_reg + rs2_reg),
      FN_SUB -> (rs1_reg - rs2_reg),
      FN_AND -> (rs1_reg & rs2_reg),
      FN_OR -> (rs1_reg | rs2_reg),
      FN_XOR -> (rs1_reg ^ rs2_reg)
    )
  )

  val final_alu_out =
    Mux(uop_reg.fcn_dw === DW_64, alu_out, alu_out(31, 0).asSInt.asUInt)

  // Output Wiring (Same Variables)
  io.task_queue_head_valid := station_valids(0)
  io.iss_resps.task_queue_head_valid := station_valids(0) // Internal check

  io.sidecar_res.valid := val_reg && !((io.br_update.b1.mispredict_mask & uop_reg.br_mask) =/= 0.U)
  io.sidecar_res.bits.uop := uop_reg
  io.sidecar_res.bits.uop.br_mask := next_br_mask
  io.sidecar_res.bits.data := final_alu_out

  io.deq_uop := station_uops(0)

  // Veto Logic (Inhibition Counter)
  val veto_counter = RegInit(0.U(16.W))

  // We veto if station(0) is valid but cannot move because of dependencies
  // or because the execution register is stalled.
  val is_stalled = station_valids(0) && !can_execute && !op0_killed

  when(is_stalled && io.veto_enable) {
    veto_counter := veto_counter + 1.U
  }.otherwise {
    veto_counter := 0.U
  }

  val timeout_fired = veto_counter > io.veto_threshold
  io.veto_trigger := timeout_fired && io.veto_enable
}
