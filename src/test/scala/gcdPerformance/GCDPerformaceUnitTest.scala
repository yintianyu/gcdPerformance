// See README.md for license details.

package gcdPerformance

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class GCDPerformanceUnitTester(c: GCDPerformance) extends PeekPokeTester(c) {
    /**
      * compute the gcd and the number of steps it should take to do it
      *
      * @param a positive integer
      * @param b positive integer
      * @return the GCD of a and b
      */
    def computeGcd(a: Int, b: Int): (Int, Int) = {
        var x = a
        var y = b
        var depth = 1
        while(y > 0 ) {
            if (x > y) {
                x -= y
            }
            else {
                y -= x
            }
            depth += 1
        }
        (x, depth)
    }
    def computeGcdWithStein(a: Int, b: Int): (Int, Int) ={
        var x = 0
        var y = 0
        if (a > b){
            x = a
            y = b
        }
        else{
            x = b
            y = a
        }
        var depth = 1
        var record = 0
        while(y > 0){
            if((x % 2 == 0) && (y % 2 == 0)){
                x /= 2
                y /= 2
                record += 1
            }
            else if((x % 2 == 0) && (y % 2 != 0)){
                x /= 2
            }
            else if((x % 2 != 0) && (y % 2 == 0)){
                y /= 2
            }
            else{
                x = x - y
            }
            if (x < y){
                val temp = x
                x = y
                y = temp
            }
            depth += 1
        }
        (x << record, depth)
    }

    private val gcd = c

    for(i <- 1 to 400 by 7) {
        for (j <- 1 to 400 by 3) {
            poke(gcd.io.opa, i)
            poke(gcd.io.opb, j)
            poke(gcd.io.loadingValues, value=1)
            step(1)
            poke(gcd.io.loadingValues, value=0)

            val (expected_gcd, steps) = computeGcdWithStein(i, j)
            val (absolute_true, _) = computeGcd(i, j)

            step(steps) // -1 is because we step(1) already to toggle the enable
            assert(expected_gcd == absolute_true)
            expect(gcd.io.result, expected_gcd)
            expect(gcd.io.done, expected=1)
        }
    }
//    val i = 100
//    val j = 70
//    poke(gcd.io.opa, i)
//    poke(gcd.io.opb, j)
//    poke(gcd.io.loadingValues, value=1)
//    step(1)
//    poke(gcd.io.loadingValues, value=0)
//
//    val (expected_gcd, steps) = computeGcdWithStein(i, j)
//    val (absolute_true, _) = computeGcd(i, j)
//
//    step(steps) // -1 is because we step(1) already to toggle the enable
//    expect(gcd.io.result, expected_gcd)
//    expect(gcd.io.done, expected=1)
}

/**
  * This is a trivial example of how to run this Specification
  * From within sbt use:
  * {{{
  * testOnly example.test.GCDTester
  * }}}
  * From a terminal shell use:
  * {{{
  * sbt 'testOnly example.test.GCDTester'
  * }}}
  */
class GCDPerformanceTester extends ChiselFlatSpec {
    // Disable this until we fix isCommandAvailable to swallow stderr along with stdout
    private val backendNames = if(firrtl.FileUtils.isCommandAvailable(Seq("verilator", "--version"))) {
        Array("firrtl", "verilator")
    }
    else {
        Array("firrtl")
    }
    for ( backendName <- backendNames ) {
        "GCD" should s"calculate proper greatest common denominator (with $backendName)" in {
            Driver(() => new GCDPerformance, backendName) {
                c => new GCDPerformanceUnitTester(c)
            } should be (true)
        }
    }

    "Basic test using Driver.execute" should "be used as an alternative way to run specification" in {
        iotesters.Driver.execute(Array(), () => new GCDPerformance) {
            c => new GCDPerformanceUnitTester(c)
        } should be (true)
    }

    "using --backend-name verilator" should "be an alternative way to run using verilator" in {
        if(backendNames.contains("verilator")) {
            iotesters.Driver.execute(Array("--backend-name", "verilator"), () => new GCDPerformance) {
                c => new GCDPerformanceUnitTester(c)
            } should be(true)
        }
    }

    "running with --is-verbose" should "show more about what's going on in your tester" in {
        iotesters.Driver.execute(Array("--is-verbose"), () => new GCDPerformance) {
            c => new GCDPerformanceUnitTester(c)
        } should be(true)
    }

    "running with --fint-write-vcd" should "create a vcd file from your test" in {
        iotesters.Driver.execute(Array("--fint-write-vcd"), () => new GCDPerformance) {
            c => new GCDPerformanceUnitTester(c)
        } should be(true)
    }
}

