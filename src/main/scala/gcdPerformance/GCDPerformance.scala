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
    // Output Registers
    val done_r = RegInit(0.U(Bool()))
    val result_r = RegInit(0.U(OPERAND_WIDTH.W))

    // Reorder Buffer registers
    val vecROBData_r = RegInit(Vec(Seq.fill(ROB_DEPTH)(0.U(OPERAND_WIDTH.W))))
    val vecROBValid_r = RegInit(Vec(Seq.fill(ROB_DEPTH)(0.U(Bool()))))
    val ROBCommitPointer_r = RegInit(0.U(log2Ceil(ROB_DEPTH).W))
    val ROBWritePointer_r = RegInit(0.U(log2Ceil(ROB_DEPTH).W))

    io.done := done_r
    io.result := result_r


    val processElements = new Array[GCDProcessElement](PIPELINE_STAGE)
    processElements(0).io.last.opa := io.opa
    processElements(0).io.last.opb := io.opb
    processElements(0).io.last.valid := io.valid && processElements(0).io.last.ready
    processElements(0).io.last.record := 0.U(RECORD_WIDTH.W)
    processElements(0).io.last.ROBIndex := ROBWritePointer_r
    io.ready := processElements(0).io.last.ready

    when(processElements(0).io.last.valid){
        ROBWritePointer_r := ROBWritePointer_r + 1.U(log2Ceil(ROB_DEPTH).W)
    }



    for (i <- 0 until (PIPELINE_STAGE - 2) by 1){
        processElements(i).io.next <> processElements(i+1).io.last
    }
    processElements(PIPELINE_STAGE-1).io.next.ready := 1.U(Bool())

    for(i <- 0 until (PIPELINE_STAGE - 1) by 1){
        when(processElements(i).io.done) {
            vecROBData_r(processElements(i).io.next.ROBIndex) := processElements(i).io.result
            vecROBValid_r(processElements(i).io.next.ROBIndex) := true.B
        }
    }
    when(vecROBValid_r(ROBCommitPointer_r) === true.B){
        result_r := vecROBData_r(ROBCommitPointer_r)
        done_r := true.B
        vecROBValid_r(ROBCommitPointer_r) := false.B
        ROBCommitPointer_r := ROBCommitPointer_r + 1.U
    }

}
