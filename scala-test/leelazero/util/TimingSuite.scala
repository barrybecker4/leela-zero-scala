package leelazero.util

import org.scalatest.FunSuite

class TimingSuite extends FunSuite {

  test("getTime") {
    val t = Timing.getTime.toString
    assertResult(12) { t.length }
  }

  test("timeDiff") {
    val diff = Timing.timeDiff(12345, 23456)
    assertResult(1111) {diff}
  }
}
