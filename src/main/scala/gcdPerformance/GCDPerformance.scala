package gcdPerformance
import chisel3._
import chisel3.util._

class GCDPerformance extends Module{
    val io = IO(new Bundle {
        val opa = Input(UInt(32.W))
        val opb = Input(UInt(32.W))
        val loadingValues = Input(Bool())
        val done = Output(Bool())
        val result = Output(UInt(32.W))
    })
    val a_r = RegInit(0.U(32.W))
    val b_r = RegInit(0.U(32.W))
    val record = RegInit(0.U(6.W)) // Suppose it can be executed in 63 cycles
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
    val bLarger = difference(31)
    val minusDifference = b_r - a_r
    val aZero = a_r === 0.U(32.W)
    val bZero = b_r === 0.U(32.W)
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
    val result = RegInit(0.U(32.W))
    when(io.done === true.B){
        result := Mux(aZero, b_r, a_r) << record
    }
    io.result := result

}
