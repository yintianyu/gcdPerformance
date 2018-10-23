package gcdPerformance

import chisel3._
import chisel3.util._
import common.Constants._

class ioBetweenUints extends Bundle{
    val opa = Input(UInt(OPERAND_WIDTH.W))
    val opb = Input(UInt(OPERAND_WIDTH.W))
    val record = Input(UInt(RECORD_WIDTH.W))
    val valid = Input(Bool())
    val ready = Output(Bool())
}



class GCDProcessElement extends Module{
    val io = IO(new Bundle{
        val last = new ioBetweenUints()
        val next = Flipped(new ioBetweenUints())
        val done = Output(Bool())
        val result = Output(UInt(OPERAND_WIDTH.W))
    })

    val a_r = RegInit(0.U(OPERAND_WIDTH.W))
    val b_r = RegInit(0.U(OPERAND_WIDTH.W))
    val record_r = RegInit(0.U(RECORD_WIDTH.W))
    val nextValid_r = RegInit(0.U(Bool()))
    val result_r = RegInit(0.U(OPERAND_WIDTH.W))
    val done_r = RegInit(0.U(Bool()))

    val bothEven = !(io.last.opa(0) || io.last.opb(0))
    val onlyAEven = !io.last.opa(0) && io.last.opb(0)
    val onlyBEven =  io.last.opa(0) && !io.last.opb(0)
    val bothOdd = io.last.opa(0) && io.last.opb(0)
    val difference = io.last.opa - io.last.opb
    val bLarger = difference(OPERAND_WIDTH-1)
    val minusDifference = io.last.opb - io.last.opa
    val aZero = a_r === 0.U(OPERAND_WIDTH.W)
    val bZero = b_r === 0.U(OPERAND_WIDTH.W)
    val done = aZero ^ bZero
    when(bothEven && io.last.valid){
        a_r := io.last.opa >> 1
        b_r := io.last.opb >> 1
        record_r := io.last.record + 1.U
    }
    when(onlyAEven && io.last.valid){
        a_r := io.last.opa >> 1
        record_r := io.last.record
    }
    when(onlyBEven && io.last.valid){
        b_r := io.last.opb >> 1
        record_r := io.last.record
    }
    when(bothOdd && io.last.valid){
        a_r := Mux(bLarger, io.last.opa, difference)
        b_r := Mux(bLarger, minusDifference, io.last.opb)
        record_r := io.last.record
    }
    nextValid_r := io.last.valid && io.next.ready
    io.next.valid := nextValid_r
    io.next.opa := a_r
    io.next.opb := b_r
    io.next.record := record_r
    io.last.ready := io.next.valid

    done_r := done
    when(done)
    {
        result_r := Mux(aZero, b_r, a_r) << record_r
    }
    io.done := done_r
    io.result := result_r
}
