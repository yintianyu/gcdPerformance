package gcdPerformance
import chisel3._
import chisel3.util._
import common.Constants._

class GCDPerformance extends Module{
    val io = IO(new Bundle {
        val opa = Input(UInt(OPERAND_WIDTH.W))
        val opb = Input(UInt(OPERAND_WIDTH.W))
        val loadingValues = Input(Bool())
        val done = Output(Bool())
        val result = Output(UInt(OPERAND_WIDTH.W))
    })
    val a_r = RegInit(0.U(OPERAND_WIDTH.W))
    val b_r = RegInit(0.U(OPERAND_WIDTH.W))
    val record = RegInit(0.U(RECORD_WIDTH.W)) // Suppose it can be executed in 127 cycles
    when(io.loadingValues === true.B){
        a_r := io.opa
        b_r := io.opb
        record := 0.U
    }
    val bothEven = !(a_r(0) || b_r(0))
    val onlyAEven = !a_r(0) && b_r(0)
    val onlyBEven =  a_r(0) && !b_r(0)
    val bothOdd = a_r(0) && b_r(0)
    val difference = a_r - b_r
    val bLarger = difference(OPERAND_WIDTH-1)
    val minusDifference = b_r - a_r
    val aZero = a_r === 0.U(OPERAND_WIDTH.W)
    val bZero = b_r === 0.U(OPERAND_WIDTH.W)
    when(bothEven && !io.done){
        a_r := a_r >> 1
        b_r := b_r >> 1
        record := record + 1.U
    }
    when(onlyAEven && !io.done){
        a_r := a_r >> 1
    }
    when(onlyBEven && !io.done){
        b_r := b_r >> 1
    }
    when(bothOdd && !io.done){
        a_r := Mux(bLarger, a_r, difference)
        b_r := Mux(bLarger, minusDifference, b_r)
    }
    io.done := aZero || bZero
    val result = RegInit(0.U(OPERAND_WIDTH.W))
    when(io.done === true.B){
        result := Mux(aZero, b_r, a_r) << record
    }
    io.result := result

}
