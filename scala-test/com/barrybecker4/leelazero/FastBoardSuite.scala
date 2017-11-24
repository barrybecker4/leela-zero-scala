package com.barrybecker4.leelazero

import org.scalatest.FunSuite
import TestUtil._

class FastBoardSuite extends FunSuite {


  test("Create an empty 3x3 board") {
    val b = new FastBoard()
    b.resetBoard(3)
    assertResult(clean("""
        |   a b c
        | 3 . . .  3
        | 2 . . .  2
        | 1 . . .  1
        |   a b c
        |
        |""")) { b.toString }
  }

  test("Create an empty 5x5 board") {
    val b = new FastBoard()
    b.resetBoard(5)
    assertResult(clean("""
        |   a b c d e
        | 5 . . . . .  5
        | 4 . . . . .  4
        | 3 . . . . .  3
        | 2 . . . . .  2
        | 1 . . . . .  1
        |   a b c d e
        |
        |""")) {b.toString}
  }
}
