
package gcdPerformance

import chisel3._
import chisel3.util._
import common.Constants._

class ioBetweenUints extends Bundle{
    val opa = Input(UInt(OPERAND_WIDTH.W))
    val opb = Input(UInt(OPERAND_WIDTH.W))
    val record = Input(UInt(RECORD_WIDTH.W))
    val valid = Input(Bool())
    val done = Input(Bool())
}



class GCDProcessElement extends Module{
    val io = IO(new Bundle{
        val last = new ioBetweenUints()
        val next = Flipped(new ioBetweenUints())
        val ready_in = Input(Bool())
        val ready_out = Output(Bool())
        val done = Output(Bool())
        val result = Output(UInt(OPERAND_WIDTH.W))
    })
    val a_r = RegInit(0.U(OPERAND_WIDTH.W))
    val b_r = RegInit(0.U(OPERAND_WIDTH.W))
    val record_r = RegInit(0.U(RECORD_WIDTH.W))
    val nextValid_r = RegInit(0.U(1.W))
    val result_r = RegInit(0.U(OPERAND_WIDTH.W))
    val done_r = RegInit(0.U(1.W))


    val aZero = a_r === 0.U(OPERAND_WIDTH.W)
    val bZero = b_r === 0.U(OPERAND_WIDTH.W)
    val bothZero = (io.last.opa === 0.U(OPERAND_WIDTH.W)) && (io.last.opb === 0.U(OPERAND_WIDTH.W))
    val done = aZero ^ bZero
    val bothEven = (!(io.last.opa(0) || io.last.opb(0))) && !bothZero
    val onlyAEven = !io.last.opa(0) && io.last.opb(0)
    val onlyBEven =  io.last.opa(0) && !io.last.opb(0)
    val bothOdd = io.last.opa(0) && io.last.opb(0)
    val difference = io.last.opa - io.last.opb
    val bLarger = difference(OPERAND_WIDTH-1)
    val minusDifference = io.last.opb - io.last.opa

    val busy = !io.ready_in
    when(bothEven && io.last.valid){
        a_r := io.last.opa >> 1
        b_r := io.last.opb >> 1
        record_r := io.last.record + 1.U
    }
    when(onlyAEven && io.last.valid){
        a_r := io.last.opa >> 1
        b_r := io.last.opb
        record_r := io.last.record
    }
    when(onlyBEven && io.last.valid){
        a_r := io.last.opa
        b_r := io.last.opb >> 1
        record_r := io.last.record
    }
    when(bothOdd && io.last.valid){
        a_r := Mux(bLarger, io.last.opa, difference >> 1)
        b_r := Mux(bLarger, minusDifference >> 1, io.last.opb)
        record_r := io.last.record
    }
//    when(io.last.valid){
//        ROBIndex_r := io.last.ROBIndex
//    }/*.otherwise{
//        a_r := 0.U(OPERAND_WIDTH.W)
//        b_r := 0.U(OPERAND_WIDTH.W)
//    }*/
    nextValid_r := /*io.last.valid &&*/ io.ready_in
    io.next.valid := io.ready_in//nextValid_r
    io.next.opa := a_r
    io.next.opb := b_r
    io.next.record := record_r
    io.next.done := done
    io.ready_out := !busy

    done_r := done
    when(done)
    {
        result_r := Mux(aZero, b_r, a_r) << record_r
    }
    io.done := done_r
    io.result := result_r
}


class GCDLastProcessElement extends Module{
    val io = IO(new Bundle{
        val last = new ioBetweenUints()
        val ready_in = Input(Bool())
        val ready_out = Output(Bool())
        val done = Output(Bool())
        val result = Output(UInt(OPERAND_WIDTH.W))
    })
    val a_r = RegInit(0.U(OPERAND_WIDTH.W))
    val b_r = RegInit(0.U(OPERAND_WIDTH.W))
    val record_r = RegInit(0.U(RECORD_WIDTH.W))
    val nextValid_r = RegInit(0.U(1.W))
    val result_r = RegInit(0.U(OPERAND_WIDTH.W))
    val done_r = RegInit(0.U(1.W))


    val aZero = a_r === 0.U(OPERAND_WIDTH.W)
    val bZero = b_r === 0.U(OPERAND_WIDTH.W)
    val bothZero = aZero && bZero
    val busy = !io.ready_in || (!aZero && !bZero)
    val opa = Mux(busy, a_r, io.last.opa)
    val opb = Mux(busy, b_r, io.last.opb)
    val opRecord = Mux(busy, record_r, io.last.record)
    val bothEven = (!(io.last.opa(0) || io.last.opb(0))) && !bothZero
    val onlyAEven = !opa(0) && opb(0)
    val onlyBEven =  opa(0) && !opb(0)
    val bothOdd = opa(0) && opb(0)
    val difference = opa - opb
    val bLarger = difference(OPERAND_WIDTH-1)
    val minusDifference = opb - opa
    val done = aZero ^ bZero
    when(bothEven && io.last.valid){
        a_r := opa >> 1
        b_r := opb >> 1
        record_r := opRecord + 1.U
    }
    when(onlyAEven && io.last.valid){
        a_r := opa >> 1
        b_r := opb
        record_r := opRecord
    }
    when(onlyBEven && io.last.valid){
        a_r := io.last.opa
        b_r := io.last.opb >> 1
        record_r := opRecord
    }
    when(bothOdd && io.last.valid){
        a_r := Mux(bLarger, opa, difference >> 1)
        b_r := Mux(bLarger, minusDifference >> 1, opb)
        record_r := opRecord
    }
//    when(io.last.valid){
//        ROBIndex_r := io.last.ROBIndex
//    }/*.otherwise{
//        a_r := 0.U(OPERAND_WIDTH.W)
//        b_r := 0.U(OPERAND_WIDTH.W)
//    }*/
    nextValid_r := io.last.valid && io.ready_in && !io.last.done
    io.ready_out := !busy
//    io.next.ROBIndex := ROBIndex_r

    done_r := done
    when(done)
    {
        result_r := Mux(aZero, b_r, a_r) << record_r
    }
    io.done := done_r
    io.result := result_r
    printf("LastPE: a_r = %d, b_r = %d, record_r = %d, busy = %d, done = %d, valid = %d, opa = %d, opb = %d, " +
            " io.last.opa = %d, io.last.opb = %d,result = %d\n", a_r,
        b_r, record_r, busy, done_r, io.last.valid, opa, opb, io.last.opa, io.last.opb, result_r)
}
