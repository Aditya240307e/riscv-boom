package boom.v4.common

import chisel3._
import chisel3.util._

object VetoInstructions {
  // VETO rd, rs1, rs2
  // Format [ funct7 | rs2 | rs1 | funct3 | rd | opcode]
  def VETO = BitPat("b0000000__________000_____0001011")

}
