package com.barrybecker4.leelazero

import org.scalatest.FunSuite


class FastBoardSuite extends FunSuite {


  test("Create an empty 3x3 board") {
    val b = new FastBoard()
    b.resetBoard(3)
    assertResult("\n   a b c \n 3 . . .  3\n 2 . . .  2\n 1 . . .  1\n   a b c \n\n") { b.toString }
  }

  test("Create an empty 5x5 board") {
    val b = new FastBoard()
    b.resetBoard(5)
    assertResult("\n   a b c d e \n 5 . . . . .  5\n 4 . . . . .  4\n 3 . . . . .  3\n 2 . . . . .  2\n 1 . . . . .  1\n   a b c d e \n\n") {
      b.toString
    }
  }
}
