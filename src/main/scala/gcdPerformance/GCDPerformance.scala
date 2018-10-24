
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
    val done_r = RegInit(0.U(1.W))
    val result_r = RegInit(0.U(OPERAND_WIDTH.W))

    // Reorder Buffer registers
    val vecROBData_r = RegInit(VecInit(Seq.fill(ROB_DEPTH)(0.U(OPERAND_WIDTH.W))))
    val vecROBValid_r = RegInit(VecInit(Seq.fill(ROB_DEPTH)(0.U(1.W))))
    val ROBCommitPointer_r = RegInit(0.U(log2Ceil(ROB_DEPTH).W))
    val ROBWritePointer_r = RegInit(0.U(log2Ceil(ROB_DEPTH).W))

    io.done := done_r
    io.result := result_r


//    val processElements = Vec.fill(PIPELINE_STAGE) {Module(new GCDProcessElement()).io}
    val processElements = VecInit(Seq.fill(PIPELINE_STAGE)(Module(new GCDProcessElement()).io))
    processElements(0).last.opa := io.opa
    processElements(0).last.opb := io.opb
    processElements(0).last.valid := io.valid && processElements(0).ready_out
    processElements(0).last.record := 0.U(RECORD_WIDTH.W)
    processElements(0).last.ROBIndex := ROBWritePointer_r
    processElements(0).last.done := 0.U(1.W)
    io.ready := processElements(0).ready_out

    when(processElements(0).last.valid){
        ROBWritePointer_r := ROBWritePointer_r + 1.U(log2Ceil(ROB_DEPTH).W)
    }

    printf("in:  a_r = %d, b_r = %d\n", processElements(0).last.opa, processElements(0).last.opb)
    printf("PE0: a_r = %d, b_r = %d\n", processElements(0).next.opa, processElements(0).next.opb)
    printf("PE1: a_r = %d, b_r = %d\n", processElements(1).next.opa, processElements(1).next.opb)
    printf("BOR: BOR[0] = %d\n", vecROBData_r(0))
    //printf("BOR: BOR[1] = %d\n", vecROBData_r(1))
    //printf("BOR: BOR[2] = %d\n", vecROBData_r(2))




    for (i <- 0 until (PIPELINE_STAGE - 1) by 1){
        processElements(i + 1).last := processElements(i).next
        processElements(i).ready_in := processElements(i + 1).ready_out
    }


//    // Debug
//    val pe1 = Module(new GCDProcessElement())
//    val pe2 = Module(new GCDProcessElement())
//    pe1.io.next <> pe2.io.last
//
//    // Debug end

    processElements(PIPELINE_STAGE-1).ready_in := 1.U(1.W)

    for(i <- 0 until (PIPELINE_STAGE - 1) by 1){
        when(processElements(i).done) {
            vecROBData_r(processElements(i).next.ROBIndex) := processElements(i).result
            vecROBValid_r(processElements(i).next.ROBIndex) := true.B
        }
    }
    when(vecROBValid_r(ROBCommitPointer_r) === true.B){
        result_r := vecROBData_r(ROBCommitPointer_r)
        done_r := true.B
        vecROBValid_r(ROBCommitPointer_r) := false.B
        ROBCommitPointer_r := ROBCommitPointer_r + 1.U
    }.otherwise{
        done_r := false.B
    }

}


object ANDDriver extends App {
    chisel3.Driver.execute(args, () => new GCDPerformance)
}