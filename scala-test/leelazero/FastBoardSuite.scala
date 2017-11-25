package leelazero

import org.scalatest.FunSuite
import FastBoard._
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
    b.setSquare(b.getVertex(2, 1), BLACK)
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
       | 2 . . X . . . . . . . . . . . . . . . .  2
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
    assertResult((2, 1)) {b.getXY(45)}
    assertResult((2, 3)) {b.getXY(87)}
    assertResult((18, 18)) {b.getXY(418)}
    assertThrows[AssertionError] {
      b.getXY(7)
    }
  }

  test("set/getSquare contents") {
    val b = new FastBoard()
    assertResult(EMPTY) { b.getSquare(43)}
    assertResult(EMPTY) { b.getSquare(0, 1)}
    b.setSquare(43, BLACK)
    assertResult(BLACK) { b.getSquare(43)}
    b.setSquare(43, WHITE)
    assertResult(WHITE) { b.getSquare(43)}
  }

  test("rotateVertex (2, 1)") {
    val b = new FastBoard()
    for (i <- 0  to 7)
      b.setSquare(b.rotateVertex(b.getVertex(2, 1), i.toShort), BLACK)
    assertResult(clean("""
                         |   a b c d e f g h j k l m n o p q r s t
                         |19 . . . . . . . . . . . . . . . . . . . 19
                         |18 . . X . . . . . . . . . . . . . X . . 18
                         |17 . X . . . . . . . . . . . . . . . X . 17
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
                         | 3 . X . . . . . . . . . . . . . . . X .  3
                         | 2 . . X . . . . . . . . . . . . . X . .  2
                         | 1 . . . . . . . . . . . . . . . . . . .  1
                         |   a b c d e f g h j k l m n o p q r s t
                         |
                         |""")) {b.toString}
  }

  test("rotateVertex (1, 0)") {
    val b = new FastBoard()
    for (i <- 0  to 7)
      b.setSquare(b.rotateVertex(b.getVertex(1, 0), i.toShort), BLACK)
    assertResult(clean("""
                         |   a b c d e f g h j k l m n o p q r s t
                         |19 . X . . . . . . . . . . . . . . . X . 19
                         |18 X . . . . . . . . . . . . . . . . . X 18
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
                         | 2 X . . . . . . . . . . . . . . . . . X  2
                         | 1 . X . . . . . . . . . . . . . . . X .  1
                         |   a b c d e f g h j k l m n o p q r s t
                         |
                         |""")) {b.toString}
  }

  test("isSuicide when not suicide (for black)") {
      val b = new FastBoard(5)
      b.setSquare(2, 2, WHITE)
      assertResult(false) { b.isSuicide(b.getVertex(1, 1), BLACK) }
      assertResult(false) { b.isSuicide(b.getVertex(2, 1), BLACK) }
  }

  /*
  test("isSuicide when suicide (for black in all white field)") {
    val b = new FastBoard(5)
    b.setSquare(1, 2, WHITE)
    b.setSquare(2, 1, WHITE)
    b.setSquare(2, 2, WHITE)
    b.setSquare(2, 3, WHITE)
    b.setSquare(2, 4, WHITE)
    b.setSquare(3, 2, WHITE)
    b.setSquare(3, 4, WHITE)
    b.setSquare(4, 3, WHITE)
    b.setSquare(0, 2, WHITE)
    b.setSquare(2, 0, WHITE)
    println(b.toString())

    assertResult(false) { b.isSuicide(b.getVertex(1, 1), BLACK) }
    assertResult(true) {
      val v = b.getVertex(3, 4)
      b.isSuicide(v, BLACK)
    }
    assertResult(true) { b.isSuicide(b.getVertex(4, 4), BLACK) }
  }*/
}
