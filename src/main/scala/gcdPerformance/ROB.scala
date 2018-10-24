package gcdPerformance

import chisel3._
import chisel3.util._
import common.Constants._

//class ROBElement extends Bundle{
//    val data = UInt(OPERAND_WIDTH.W)
//    val valid = Bool()
//}
//
//class ROB extends Bundle{
//    val commitPointer = UInt(log2Ceil(OPERAND_WIDTH).W)
//    val elements = Vec(PIPELINE_STAGE, ROBElement)
//}
