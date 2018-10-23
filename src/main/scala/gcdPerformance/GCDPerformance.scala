package gcdPerformance
import chisel3._
import chisel3.util._
import common.Constants._

class GCDPerformance extends Module{
    val io = IO(new Bundle {
        val opa = Input(UInt(OPERAND_WIDTH.W))
        val opb = Input(UInt(OPERAND_WIDTH.W))
        val valid = Input(Bool())
        val ready = Output(Bool())
        val done = Output(Bool())
        val result = Output(UInt(OPERAND_WIDTH.W))
    })
    val processElements = new Array[GCDProcessElement](PIPELINE_STAGE)
    processElements(0).io.last.opa := io.opa
    processElements(0).io.last.opb := io.opb
    processElements(0).io.last.valid := io.valid && processElements(0).io.last.ready
    processElements(0).io.last.record := 0.U(RECORD_WIDTH.W)

    for (i <- 0 until (PIPELINE_STAGE - 2) by 1){
        processElements(i).io.next <> processElements(i+1).io.last
    }

}
