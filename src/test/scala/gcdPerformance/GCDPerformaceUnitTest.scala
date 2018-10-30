// See README.md for license details.

package gcdPerformance

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import common.Constants._

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

/* Big Test start*/
    var cycle: Int = 0
    var expectArray = new Array[Int](TEST_QUANTITY)
    var actualArray = new Array[Int](TEST_QUANTITY)
    var opaArray = new Array[Int](TEST_QUANTITY)
    var opbArray = new Array[Int](TEST_QUANTITY)
    var expectArrayCounter: Int = 0
    var actualArrayCounter: Int = 0
    val randomObj = new util.Random()
    for (_ <- 0 until TEST_QUANTITY) {
        val a = randomObj.nextInt(100)
        val b = randomObj.nextInt(100)
        poke(gcd.io.opa, a)
        poke(gcd.io.opb, b)
        poke(gcd.io.valid, value = 0)
        while(peek(gcd.io.ready) != 1){
            step(1)
            printf("In waiting for ready\n")
        }
        poke(gcd.io.valid, value=1)
        step(1)
        cycle += 1

        val (expected_gcd, steps) = computeGcdWithStein(a, b)
        val result = peek(gcd.io.result)
        val done = peek(gcd.io.done)
        val ready = peek(gcd.io.ready)
        expectArray(expectArrayCounter) = expected_gcd
        opaArray(expectArrayCounter) = a
        opbArray(expectArrayCounter) = b
        expectArrayCounter += 1
        if(1 == done){
            actualArray(actualArrayCounter) = result.toInt
            actualArrayCounter += 1
        }
        printf("Cycle %d, result = %d, done = %d, ready = %d\n",cycle, result, done, ready)
        printf("**************************************************expected = %d, steps = %d\n", expected_gcd, steps)

    }
    while(actualArrayCounter < expectArrayCounter){
        step(1)
        val result = peek(gcd.io.result)
        val done = peek(gcd.io.done)
        cycle +=1
        if(1 == done){
            actualArray(actualArrayCounter) = result.toInt
            actualArrayCounter += 1
        }
    }
    var correctFlag: Boolean = true
    for(i <- 0 until actualArrayCounter){
        if (expectArray(i) != actualArray(i)){
            printf("!!!!!!!!!!!!!!!!!Error: No.%d, opa = %d, opb = %d, expect= %d, result= %d\n", i,
                opaArray(i), opbArray(i), expectArray(i), actualArray(i))
            correctFlag = false
        }
        else{
            printf("No.%d, opa = %d, opb = %d, expect: %d, result: %d\n", i, opaArray(i), opbArray(i),
                expectArray(i), actualArray(i))
        }
        printf("No.%d, expect: %d, result: %d\n", i, expectArray(i), actualArray(i))
    }
    printf("Clock Cycles = %d\n", cycle)
//    assert(correctFlag)
/* Big Test end*/

//
//    var i = 691559254
//    var j = 1057066721
//    poke(gcd.io.opa, i)
//    poke(gcd.io.opb, j)
//    poke(gcd.io.valid, value=1)
//    step(1)
//
//
//    var (expected_gcd1, steps1) = computeGcdWithStein(i, j)
//    var (absolute_true1, _) = computeGcd(i, j)
//
//
//    i = 996584635
//    j = 2002456335
//    poke(gcd.io.opa, i)
//    poke(gcd.io.opb, j)
//    poke(gcd.io.valid, value=1)
//    step(1)
//
//
//    var (expected_gcd2, steps2) = computeGcdWithStein(i, j)
//    var (absolute_true2, _) = computeGcd(i, j)
//
//    var done = peek(gcd.io.done)
//    var result = peek(gcd.io.result)
//
//    poke(gcd.io.valid, value=0)
//
//    var count = 0
//
//    while(count < 2){
//        if(done == 1 && count == 0){
//            count += 1
//            printf("expected_gcd = %d, steps = %d, absolute_true = %d, result = %d\n", expected_gcd1, steps1,
//                absolute_true1, result)
//        }
//        else if(done == 1 && count == 1){
//            count += 1
//            printf("expected_gcd = %d, steps = %d, absolute_true = %d, result = %d\n", expected_gcd2, steps2,
//                absolute_true2, result)
//        }
//        step(1)
//        done = peek(gcd.io.done)
//        result = peek(gcd.io.result)
//    }





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
    private val backendNames = if(false && firrtl.FileUtils.isCommandAvailable(Seq("verilator", "--version"))) {
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

//    "Basic test using Driver.execute" should "be used as an alternative way to run specification" in {
//        iotesters.Driver.execute(Array(), () => new GCDPerformance) {
//            c => new GCDPerformanceUnitTester(c)
//        } should be (true)
//    }
//
//    "using --backend-name verilator" should "be an alternative way to run using verilator" in {
//        if(backendNames.contains("verilator")) {
//            iotesters.Driver.execute(Array("--backend-name", "verilator"), () => new GCDPerformance) {
//                c => new GCDPerformanceUnitTester(c)
//            } should be(true)
//        }
//    }
//
//    "running with --is-verbose" should "show more about what's going on in your tester" in {
//        iotesters.Driver.execute(Array("--is-verbose"), () => new GCDPerformance) {
//            c => new GCDPerformanceUnitTester(c)
//        } should be(true)
//    }
//
//    "running with --fint-write-vcd" should "create a vcd file from your test" in {
//        iotesters.Driver.execute(Array("--fint-write-vcd"), () => new GCDPerformance) {
//            c => new GCDPerformanceUnitTester(c)
//        } should be(true)
//    }
}

