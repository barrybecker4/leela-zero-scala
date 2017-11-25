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


  test("isSuicide when suicide (for black in all white field)") {
    val b = create5x5AllWhiteField()
    println(b.toString())

    assertResult(false) { b.isSuicide(b.getVertex(1, 1), BLACK) }
    assertResult(true) {
      b.isSuicide(b.getVertex(3, 3), BLACK)
    }
    assertResult(true) {
      b.isSuicide(b.getVertex(4, 4), BLACK)
    }
    assertResult(false) {
      b.isSuicide(b.getVertex(4, 2), BLACK)
    }
    assertResult(true) { b.isSuicide(b.getVertex(4, 4), BLACK) }
  }

  test("Count real liberties on 5x5") {
    val b = createFilled5x5Board()
    val vertices = Array(
      b.getVertex(1, 1), b.getVertex(2, 1), b.getVertex(3, 1), b.getVertex(4, 1), b.getVertex(2, 2), b.getVertex(3, 2), b.getVertex(0, 3)
    )
    assertResult("4, 4, 2, 16376, 5, 2, 3") {   // 16376 used for black spaces?
      vertices.map(v => b.countRealLiberties(v)).mkString(", ")
    }
  }

  test("Count empty neighbors on 5x5") {
    val b = createFilled5x5Board()
    val vertices = Array(
      b.getVertex(1, 1), b.getVertex(2, 1), b.getVertex(3, 1), b.getVertex(4, 1), b.getVertex(2, 2), b.getVertex(3, 2), b.getVertex(0, 3)
    )
    assertResult("3, 1, 2, 2, 1, 2, 3") {  // correct!
      vertices.map(v => b.countNeighbors(EMPTY, v)).mkString(", ")
    }
  }

  private def create5x5AllWhiteField(): FastBoard = {
    val b = new FastBoard(5)
    b.updateBoardFast(WHITE, b.getVertex(1, 2))
    b.updateBoardFast(WHITE, b.getVertex(2, 1))
    b.updateBoardFast(WHITE, b.getVertex(2, 2))
    b.updateBoardFast(WHITE, b.getVertex(2, 3))
    b.updateBoardFast(WHITE, b.getVertex(2, 4))
    b.updateBoardFast(WHITE, b.getVertex(3, 2))
    b.updateBoardFast(WHITE, b.getVertex(3, 4))
    b.updateBoardFast(WHITE, b.getVertex(4, 3))
    b.updateBoardFast(WHITE, b.getVertex(0, 2))
    b.updateBoardFast(WHITE, b.getVertex(2, 0))
    b
  }

  /**
    *   a b c d e
      5 . . O . .  5
      4 X . O . .  4
      3 . . O X .  3
      2 . X X O .  2
      1 . . . . .  1
        a b c d e
    * @return
    */
  private def createFilled5x5Board(): FastBoard = {
    val b = new FastBoard(5)
    b.updateBoardFast(BLACK, b.getVertex(1, 1))
    b.updateBoardFast(BLACK, b.getVertex(2, 1))
    b.updateBoardFast(WHITE, b.getVertex(3, 1))
    b.updateBoardFast(WHITE, b.getVertex(2, 2))
    b.updateBoardFast(BLACK, b.getVertex(3, 2))
    b.updateBoardFast(BLACK, b.getVertex(0, 3))
    b.updateBoardFast(WHITE, b.getVertex(2, 3))
    b.updateBoardFast(WHITE, b.getVertex(2, 4))
    b
  }
}
