package gcdPerformance

import chisel3._
import chisel3.util._


class GCDProcessUnit extends Module{
    val io = IO(new Bundle{
        val opa = Input(UInt(32.W))
        val opb = Input(UInt(32.W))
        val valid = Input(Bool())
        val ready = Output(Bool())
        val done = Output(Bool())
        val result = Output(UInt(32.W))
        val nxta = Output(UInt(32.W))
        val nxtb = Output(UInt(32.W))
    })

}
