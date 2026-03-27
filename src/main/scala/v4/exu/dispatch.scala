//******************************************************************************
// Copyright (c) 2012 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// BOOM Instruction Dispatcher
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


package boom.v4.exu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters

import boom.v4.common._
import boom.v4.util._

class DispatchIO(implicit p: Parameters) extends BoomBundle
{
  // incoming microops from rename2
  val ren_uops = Vec(coreWidth, Flipped(DecoupledIO(new MicroOp)))

  // outgoing microops to issue queues
  // N issues each accept up to dispatchWidth uops
  // dispatchWidth may vary between issue queues
  val dis_uops = MixedVec(issueParams.map(ip=>Vec(ip.dispatchWidth, DecoupledIO(new MicroOp))))

  // Global control signal to enable/disable the SSI logic 
  // when true, the uops tagged tainted are moved into the inorder lane 
  // TODO: Doesn't this requre a specific logic or something for the Register File port contention
  val veto_enable = Input(Bool())
}

abstract class Dispatcher(implicit p: Parameters) extends BoomModule
{
  val io = IO(new DispatchIO)
}

/**
 * This Dispatcher assumes worst case, all dispatched uops go to 1 issue queue
 * This is equivalent to BOOMv2 behavior
 */
class BasicDispatcher(implicit p: Parameters) extends Dispatcher
{
  issueParams.map(ip=>require(ip.dispatchWidth == coreWidth))
  // TODO: Make ren_readys aware of the vetoed status
  val ren_readys = io.dis_uops.map(d=>VecInit(d.map(_.ready)).asUInt).reduce(_&_)

  for (w <- 0 until coreWidth) {
    io.ren_uops(w).ready := ren_readys(w)
  }

  for {i <- 0 until issueParams.size
       w <- 0 until coreWidth} {
    val issueParam = issueParams(i)
    val dis        = io.dis_uops(i)
    val uop = io.ren_uops(w).bits
    val is_vetoed = uop.is_tainted && io.veto_enable


    val target_iq_type = Mux(is_vetoed, (1 << IQ_VETO()).U, uop.iq_type)

  
    dis(w).valid := io.ren_uops(w).valid && io.ren_uops(w).bits.iq_type(issueParam.iqType)
    dis(w).bits  := io.ren_uops(w).bits
  }
}

/**
 *  Tries to dispatch as many uops as it can to issue queues,
 *  which may accept fewer than coreWidth per cycle.
 *  When dispatchWidth == coreWidth, its behavior differs
 *  from the BasicDispatcher in that it will only stall dispatch when
 *  an issue queue required by a uop is full.
 */
class CompactingDispatcher(implicit p: Parameters) extends Dispatcher
{
  issueParams.map(ip => require(ip.dispatchWidth >= ip.issueWidth))

  val ren_readys = Wire(Vec(issueParams.size, Vec(coreWidth, Bool())))

  for (((ip, dis), rdy) <- issueParams zip io.dis_uops zip ren_readys) {
    val ren = Wire(Vec(coreWidth, Decoupled(new MicroOp)))
    //TODO: Manual wiring instead of 'ren <> io.ren_uops' to apply the mask override
    for (w <- 0 until coreWidth) {
      ren(w).bits := io.ren_uops(w).bits
    }

    val uses_iq = io.ren_uops map { u =>
      val is_vetoed = u.bits.is_tainted && io.veto_enable
      // If vetoed, mask is ONLY bit 4 (IQ_VETO). Otherwise, use original mask.
      val effective_mask = Mux(is_vetoed, (1 << IQ_VETO).U, u.bits.iq_type)
      effective_mask(ip.iqType)
    }

    // Only request an issue slot if the uop needs to enter THIS specific queue.
    (ren zip io.ren_uops zip uses_iq) foreach {case ((u,v),q) =>
      u.valid := v.valid && q}

    val compactor = Module(new Compactor(coreWidth, ip.dispatchWidth, new MicroOp))
    compactor.io.in  <> ren
    dis <> compactor.io.out

    //The queue is considered ready if the uop doesn't use it.
    rdy := ren zip uses_iq map {case (u,q) => u.ready || !q}
  }

  // Combine readys: Rename only stalls if the instruction's SPECIFIC target is full.
  (ren_readys.reduce((r,i) =>
      VecInit(r zip i map {case (r,i) =>
        r && i})) zip io.ren_uops) foreach {case (r,u) =>
          u.ready := r}
}
