package com.barrybecker4.leelazero

import org.scalatest.FunSuite
import TestUtil._

class FastBoardSuite extends FunSuite {


  test("Create an empty 3x3 board") {
    val b = new FastBoard(3)
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

  test("Create an empty 19x19 board") {
    val b = new FastBoard(19)
    assertResult(clean("""
       |   a b c d e f g h j k l m n o p q r s t
       |19 . . . . . . . . . . . . . . . . . . . 19
       |18 . . . . . . . . . . . . . . . . . . . 18
       |17 . . . . . . . . . . . . . . . . . . . 17
       |16 . . . + . . . . . + . . . . . + . . . 16
       |15 . . . . . . . . . . . . . . . . . . . 15
       |14 . . . . . . . . . . . . . . . . . . . 14
       |13 . . . . . . . . . . . . . . . . . . . 13
       |12 . . . . . . . . . . . . . . . . . . . 12
       |11 . . . . . . . . . . . . . . . . . . . 11
       |10 . . . + . . . . . + . . . . . + . . . 10
       | 9 . . . . . . . . . . . . . . . . . . .  9
       | 8 . . . . . . . . . . . . . . . . . . .  8
       | 7 . . . . . . . . . . . . . . . . . . .  7
       | 6 . . . . . . . . . . . . . . . . . . .  6
       | 5 . . . . . . . . . . . . . . . . . . .  5
       | 4 . . . + . . . . . + . . . . . + . . .  4
       | 3 . . . . . . . . . . . . . . . . . . .  3
       | 2 . . . . . . . . . . . . . . . . . . .  2
       | 1 . . . . . . . . . . . . . . . . . . .  1
       |   a b c d e f g h j k l m n o p q r s t
       |
       |""")) {b.toString}
  }

  test("get vertex on 19x19") {
    val b = new FastBoard()
    assertResult(22) {b.getVertex(0, 0)}
    assertResult(43) {b.getVertex(0, 1)}
    assertResult(44) {b.getVertex(1, 1)}
    assertResult(87) {b.getVertex(2, 3)}
    assertResult(418) {b.getVertex(18, 18)}
  }

  test("getXY from vertex") {
    val b = new FastBoard()
    assertResult((0, 0)) {b.getXY(22)}
    assertResult((0, 1)) {b.getXY(43)}
    assertResult((1, 1)) {b.getXY(44)}
    assertResult((2, 3)) {b.getXY(87)}
    assertResult((18, 18)) {b.getXY(418)}
    assertThrows[AssertionError] {
      b.getXY(7)
    }
  }
}
