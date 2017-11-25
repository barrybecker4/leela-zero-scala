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
    val b = createSemiFilled5x5Board()
    val vertices = Array(
      b.getVertex(1, 1), b.getVertex(2, 1), b.getVertex(3, 1), b.getVertex(4, 1), b.getVertex(2, 2), b.getVertex(3, 2), b.getVertex(0, 3)
    )
    assertResult("4, 4, 2, 16376, 5, 2, 3") {   // 16376 used for black spaces?
      vertices.map(v => b.countStringLiberties(v)).mkString(", ")
    }
  }

  test("Count empty neighbors on 5x5") {
    val b = createSemiFilled5x5Board()
    val vertices = Array(
      b.getVertex(1, 1), b.getVertex(2, 1), b.getVertex(3, 1), b.getVertex(4, 1), b.getVertex(2, 2), b.getVertex(3, 2), b.getVertex(0, 3)
    )
    b.displayLiberties(-1)
    assertResult("3, 1, 2, 2, 1, 2, 3") {  // correct!
      vertices.map(v => b.countNeighbors(EMPTY, v)).mkString(", ")
    }
  }

  test("getStoneCount on 5x5") {
    val b = createSemiFilled5x5Board()
    assertResult(8) { b.getStoneCount }
  }

  test("calcAreaScore on 5x5") {
    val b = createSemiFilled5x5Board()
    assertResult(-3.5) { b.areaScore(6.5F) }
    assertResult(2.5) { b.areaScore(0.5F) }
  }

  test("calcAreaScore on whiteField 5x5") {
    val b = create5x5AllWhiteField()
    assertResult(-30.5) { b.areaScore(6.5F) }
    assertResult(-24.5) { b.areaScore(0.5F) }
  }

  test("estimate MC score 5x5") {
    val b = createSemiFilled5x5Board()
    assertResult(1) { b.estimateMcScore(0.5F) }
  }

  test("final MC score 5x5") {
    val b = createSemiFilled5x5Board()
    assertResult(0.5) { b.finalMcScore(0.5F) }
  }

  test("estimate MC score whiteField 5x5") {
    val b = create5x5AllWhiteField()
    assertResult(-9) { b.estimateMcScore(0.5F) }
  }

  test("final MC score whiteField 5x5") {
    val b = create5x5AllWhiteField()
    assertResult(-11.5) { b.finalMcScore(0.5F) }
  }

  test("isEye on white field 5x5") {
    val b = create5x5AllWhiteField()
    assertResult(true) { b.isEye(WHITE, b.getVertex(4, 4)) }
    assertResult(true) { b.isEye(WHITE, b.getVertex(3, 3)) }
    assertResult(true) { b.isEye(WHITE, b.getVertex(2, 2)) } // not really because its filled
    assertResult(false) { b.isEye(WHITE, b.getVertex(1, 1)) } // not a single point eye

    assertResult(false) { b.isEye(BLACK, b.getVertex(3, 3)) }
    assertResult(false) { b.isEye(BLACK, b.getVertex(2, 2)) }
  }

  test("isEye on filled 5x5") {
    val b = createFilled5x5Board()
    assertResult(false) { b.isEye(WHITE, b.getVertex(3, 4)) } // false eye
    assertResult(false) { b.isEye(BLACK, b.getVertex(4, 2)) }  // false eye
    assertResult(false) { b.isEye(BLACK, b.getVertex(2, 0)) }
    assertResult(false) { b.isEye(WHITE, b.getVertex(0, 2)) }
  }

  test("isEye on black field 5x5") {
    val b = create5x5AllBlackField()
    assertResult(true) { b.isEye(BLACK, b.getVertex(4, 4)) }
    assertResult(true) { b.isEye(BLACK, b.getVertex(3, 3)) }
    assertResult(false) { b.isEye(BLACK, b.getVertex(2, 2)) } // white stone on diagonals make it a false eye
  }

  test("getPrisoners when single capture 5x5") {
    val b = createFilled5x5Board()
    b.updateBoardFast(3, 4, BLACK)
    assertResult(0) {b.getPrisoners(WHITE)}
    assertResult(1) {b.getPrisoners(BLACK)}
  }

  test("toMove") {
    val b = createFilled5x5Board()
    assertResult(BLACK) { b.getToMove }
    assertResult(true)  { b.blackToMove() }
    b.setToMove(WHITE)
    assertResult(false) { b.blackToMove() }
    assertResult(WHITE) { b.getToMove }
  }

  test("getParentString") {
    val b = createFilled5x5Board()
    assertResult(17) {b.getParentString(b.getVertex(1, 1))}
    assertResult(31) {b.getParentString(b.getVertex(2, 2))}
    assertResult(31) {b.getParentString(b.getVertex(3, 3))}
    assertResult(40) {b.getParentString(b.getVertex(4, 4))}
    assertThrows[AssertionError] {
      b.getParentString(b.getVertex(0, 1)) // no string here
    }
  }


  /**
         a b c d e
       5 . . O O .  5
       4 . . O . O  4
       3 O O O O .  3
       2 . . O . .  2
       1 . . O . .  1
         a b c d e
    */
  private def create5x5AllWhiteField(): FastBoard = {
    val b = new FastBoard(5)
    b.updateBoardFast(1, 2, WHITE)
    b.updateBoardFast(2, 1, WHITE)
    b.updateBoardFast(2, 2, WHITE)
    b.updateBoardFast(2, 3, WHITE)
    b.updateBoardFast(2, 4, WHITE)
    b.updateBoardFast(3, 2, WHITE)
    b.updateBoardFast(3, 4, WHITE)
    b.updateBoardFast(4, 3, WHITE)
    b.updateBoardFast(0, 2, WHITE)
    b.updateBoardFast(2, 0, WHITE)
    b
  }

  /**
         a b c d e
       5 X . X X .  5
       4 . . X . X  4
       3 X X . X .  3
       2 . O X O .  2
       1 . . . . .  1
         a b c d e
    */
  private def create5x5AllBlackField(): FastBoard = {
    val b = new FastBoard(5)
    b.updateBoardFast(1, 2, BLACK)
    b.updateBoardFast(2, 1, BLACK)
    b.updateBoardFast(0, 4, BLACK)
    b.updateBoardFast(2, 3, BLACK)
    b.updateBoardFast(2, 4, BLACK)
    b.updateBoardFast(3, 2, BLACK)
    b.updateBoardFast(3, 4, BLACK)
    b.updateBoardFast(4, 3, BLACK)
    b.updateBoardFast(0, 2, BLACK)
    b.updateBoardFast(1, 1, WHITE)
    b.updateBoardFast(3, 1, WHITE)
    b
  }

  /**
        a b c d e
      5 . . O . .  5
      4 X . O . .  4
      3 . . O X .  3
      2 . X X O .  2
      1 . . . . .  1
        a b c d e
    */
  private def createSemiFilled5x5Board(): FastBoard = {
    val b = new FastBoard(5)
    b.updateBoardFast(1, 1, BLACK)
    b.updateBoardFast(2, 1, BLACK)
    b.updateBoardFast(3, 1, WHITE)
    b.updateBoardFast(2, 2, WHITE)
    b.updateBoardFast(3, 2, BLACK)
    b.updateBoardFast(0, 3, BLACK)
    b.updateBoardFast(2, 3, WHITE)
    b.updateBoardFast(2, 4, WHITE)
    b
  }

  /**
        a b c d e
      5 . . O . O  5
      4 X . O O X  4
      3 X O O X .  3
      2 . X X O .  2
      1 O X . X .  1
        a b c d e
    */
  private def createFilled5x5Board(): FastBoard = {
    val b = new FastBoard(5)
    b.updateBoardFast(1, 1, BLACK)
    b.updateBoardFast(2, 1, BLACK)
    b.updateBoardFast(3, 1, WHITE)
    b.updateBoardFast(2, 2, WHITE)
    b.updateBoardFast(3, 2, BLACK)
    b.updateBoardFast(0, 3, BLACK)
    b.updateBoardFast(2, 3, WHITE)
    b.updateBoardFast(2, 4, WHITE)
    b.updateBoardFast(3, 3, WHITE)
    b.updateBoardFast(4, 4, WHITE)
    b.updateBoardFast(4, 3, BLACK)
    b.updateBoardFast(1, 2, WHITE)
    b.updateBoardFast(0, 2, BLACK)
    b.updateBoardFast(0, 0, WHITE)
    b.updateBoardFast(1, 0, BLACK)
    b.updateBoardFast(3, 0, BLACK)
    b
  }
}
