
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

    io.done := done_r
    io.result := result_r


//    val processElements = Vec.fill(PIPELINE_STAGE) {Module(new GCDProcessElement()).io}
    val processElements = VecInit(Seq.fill(PIPELINE_STAGE-1)(Module(new GCDProcessElement()).io))
    processElements(0).last.opa := io.opa
    processElements(0).last.opb := io.opb
    processElements(0).last.valid := io.valid && processElements(0).ready_out
    processElements(0).last.record := 0.U(RECORD_WIDTH.W)
    processElements(0).last.done := 0.U(1.W)
    io.ready := processElements(0).ready_out







    for (i <- 0 until (PIPELINE_STAGE - 2) by 1){
        processElements(i + 1).last := processElements(i).next
        processElements(i).ready_in := processElements(i + 1).ready_out
    }
    val lastProcessElement = Module(new GCDLastProcessElement())
    lastProcessElement.io.last := processElements(PIPELINE_STAGE-2).next
    lastProcessElement.io.ready_in := true.B



//    // Debug
//    val pe1 = Module(new GCDProcessElement())
//    val pe2 = Module(new GCDProcessElement())
//    pe1.io.next <> pe2.io.last
//
//    // Debug end

    processElements(PIPELINE_STAGE-2).ready_in := lastProcessElement.io.ready_out
    done_r := lastProcessElement.io.done
    result_r := lastProcessElement.io.result



    printf("in:  a_r = %d, b_r = %d\n", processElements(0).last.opa, processElements(0).last.opb)

    for(i <- 0 until PIPELINE_STAGE-1){
        printf("PE%d: a_r = %d, b_r = %d, done = %d, valid = %d, record = %d, done_r = %d, result_r = %d\n", i.asUInt(8.W), processElements(i).next.opa,
            processElements(i).next.opb, processElements(i).next.done, processElements(i).next.valid,
            processElements(i).next.record, processElements(i).done, processElements(i).result)
    }
    printf("PE%d: done_r = %d, result_r = %d\n", (PIPELINE_STAGE-1).U, lastProcessElement.io.done, lastProcessElement.io.result)

    printf("done_r = %d, result_r = %d\n", done_r, result_r)

}


object ANDDriver extends App {
    chisel3.Driver.execute(args, () => new GCDPerformance)
}