package leelazero.sgf

import leelazero.TestUtil.clean
import org.scalatest.FunSuite
import leelazero.board.FastBoard._


class SgfParserSuite extends FunSuite {

  val parser = new SgfParser()

  test("chopStream invalid file") {
    assertThrows[IllegalArgumentException] {
      parser.countGamesInFile("games/XXXX.sgf")
    }
  }

  test("chopStream 1") {
    assertResult(1) {parser.countGamesInFile("games/2002-01-10-7.sgf")}
  }

  test("count games some_matches") {
    assertResult(3) {parser.countGamesInFile("games/some_matches.sgf")}
  }

  test("Parse sgf file") {
    verify("2002-01-10-7.sgf", SgfResult(157, 1, BLACK))
  }

  test("Parse 'simple_match' sgf file") {
    verify("simple_match.sgf", SgfResult(50, 1, EMPTY))
  }

  test("Parse 'some_matches' sgf file") {
    verify("some_matches.sgf", SgfResult(320, 1, WHITE))
  }

  test("Parse 'variation_matches' sgf file") {
    verify("variation_matches.sgf", SgfResult(320, 1, WHITE))
  }

  test("follow main line state for simple_match") {
    val tree: SgfTree = SgfParser.loadFromFile("games/simple_match.sgf")
    val koState = tree.followMainlineState(40)

    assertResult(clean("""
       |Passes: 0
       |Black (X) Prisoners: 0
       |White (O) Prisoners: 0
       |Black (X) to move
       |   a b c d e f g h j k l m n o p q r s t
       |19 . . . . . . . . . X O . . . . . . . . 19
       |18 . . . . . . . . . X O . . . . . . . . 18
       |17 . . . . . . . . . X O . . . . . . . . 17
       |16 . . . X . . . . . X O . . . . O . . . 16
       |15 . . . . . . . . . X O . . . . . . . . 15
       |14 . . . . . . . . . X O . . . . . . . . 14
       |13 . . . . . . . . . X O . . . . . . . . 13
       |12 . . . . . . . . . X O . . . . . . . . 12
       |11 . . . . . . . . . X O . . . . . . . . 11
       |10 . . . + . . . . . X O . . . . + . . . 10
       | 9 . . . . . . . . . X O . . . . . . . .  9
       | 8 . . . . . . . . . X O . . . . . . . .  8
       | 7 . . . . . . . . . X O . . . . . . . .  7
       | 6 . . . . . . . . . X O . . . . . . . .  6
       | 5 . . . . . . . . . X O . . . . . . . .  5
       | 4 . . . X . . . . X O . . . . . O . . .  4
       | 3 . . . . . . . . X O . . . . . . . . .  3
       | 2 . . . . . . . . X(O). . . . . . . . .  2
       | 1 . . . . . . . . . . . . . . . . . . .  1
       |   a b c d e f g h j k l m n o p q r s t
       |
       |Hash: 27b7989b9c68c861 Ko-Hash: c1d7c0aced5b9cf6""")) {
      koState.toString
    }
  }

  test("follow main line state for 2002-01-10-7 (120)") {
    val tree: SgfTree = SgfParser.loadFromFile("games/2002-01-10-7.sgf")
    val koState = tree.followMainlineState(120)

    assertResult(clean("""
       |Passes: 0
       |Black (X) Prisoners: 4
       |White (O) Prisoners: 2
       |Black (X) to move
       |   a b c d e f g h j k l m n o p q r s t
       |19 . . . . . . . . . . . . . . . . . . . 19
       |18 . . X X O O . . . . . . . O X . . . . 18
       |17 . . X O X X O . . . . . . O X . . . . 17
       |16 . X X O O . . . . O . . O X X X . . . 16
       |15 . X O . . . . . . . . . O O . . . . . 15
       |14 O O O O . . . . . . . . . . . . X . . 14
       |13 X X X O O . . . . . . . . . . . . . . 13
       |12 . X X X O X . . . . O . . . . . . . . 12
       |11 X . X O X O O . . . O X . X . . . . . 11
       |10 . X O O X . O X . + O X . . . + . . . 10
       | 9 . O O . X . O . . O X X . O . . . . .  9
       | 8 . . . . . . . . O . . . . . . . . . .  8
       | 7 . . . . . . . X X X . X(O)O . O O . .  7
       | 6 . . . X . . O O X . . . . . X X X . .  6
       | 5 . O O O O O O X O O . . O O . . . . .  5
       | 4 . X X O X O X X . + O . . X O X . . .  4
       | 3 . . . X X X . . . X O . X . X . . . .  3
       | 2 . . . . . . . . . X . . . X . . . . .  2
       | 1 . . . . . . . . . . . . . . . . . . .  1
       |   a b c d e f g h j k l m n o p q r s t
       |
       |Hash: c6375ccb39364391 Ko-Hash: 666c1aa2df3f028""")) {
      koState.toString
    }
  }

  test("state from main line for 2002-01-10-7 (20)") {
    val tree: SgfTree = SgfParser.loadFromFile("games/2002-01-10-7.sgf")
    val koState = tree.getStateFromMainline(20)

    assertResult(clean("""
       |Passes: 0
       |Black (X) Prisoners: 0
       |White (O) Prisoners: 0
       |White (O) to move
       |   a b c d e f g h j k l m n o p q r s t
       |19 . . . . . . . . . . . . . . . . . . . 19
       |18 . . . . . . . . . . . . . . . . . . . 18
       |17 . . . . . . . . . . . . . O . . . . . 17
       |16 . . . O . . . . . O . . . . . X . . . 16
       |15 . . . . . . . . . . . . . . . . . . . 15
       |14 . . . . . . . . . . . . . . . . X . . 14
       |13 . . . . . . . . . . . . . . . . . . . 13
       |12 . . . . . . . . . . . . . . . . . . . 12
       |11 . . . . . . . . . . . . . . . . . . . 11
       |10 . . . O . . . . . + . . . . . + . . . 10
       | 9 . . . . . . . . . . . . . . . . . . .  9
       | 8 . . . . . . . . . . . . . . . . . . .  8
       | 7 . . . . . . . . . . . . . . . . . . .  7
       | 6 . . . X . . . . . . . . . . . . . . .  6
       | 5 . O . O . O O . . . . . . . . . . . .  5
       | 4 . X(X)O . O X X . + . . . . . X . . .  4
       | 3 . . . X X X . . . . . . . . . . . . .  3
       | 2 . . . . . . . . . . . . . . . . . . .  2
       | 1 . . . . . . . . . . . . . . . . . . .  1
       |   a b c d e f g h j k l m n o p q r s t
       |
       |Hash: 1947dd9ba124d9bc Ko-Hash: 54ea2e617bda26e6""")) {
      koState.toString
    }
  }

  test("state from main line for 2002-01-10-7 (120)") {
    val tree: SgfTree = SgfParser.loadFromFile("games/2002-01-10-7.sgf")
    val koState = tree.getStateFromMainline(120)

    assertResult(clean("""
       |Passes: 0
       |Black (X) Prisoners: 4
       |White (O) Prisoners: 2
       |White (O) to move
       |   a b c d e f g h j k l m n o p q r s t
       |19 . . . . . . . . . . . . . . . . . . . 19
       |18 . . X X O O . . . . . . . O X . . . . 18
       |17 . . X O X X O . . . . . . O X . . . . 17
       |16 . X X O O . . . . O . . O X X X . . . 16
       |15 . X O . . . . . . . . . O O . . . . . 15
       |14 O O O O . . . . . . . . . . . . X . . 14
       |13 X X X O O . . . . . . . . . . . . . . 13
       |12 . X X X O X . . . . O . . . . . . . . 12
       |11 X . X O X O O . . . O X . X . . . . . 11
       |10 . X O O X . O X . + O X . . . + . . . 10
       | 9 . O O . X . O . . O X X . O .(X). . .  9
       | 8 . . . . . . . . O . . . . . . . . . .  8
       | 7 . . . . . . . X X X . X O O . O O . .  7
       | 6 . . . X . . O O X . . . . . X X X . .  6
       | 5 . O O O O O O X O O . . O O . . . . .  5
       | 4 . X X O X O X X . + O . . X O X . . .  4
       | 3 . . . X X X . . . X O . X . X . . . .  3
       | 2 . . . . . . . . . X . . . X . . . . .  2
       | 1 . . . . . . . . . . . . . . . . . . .  1
       |   a b c d e f g h j k l m n o p q r s t
       |
       |Hash: 8c5381e6233890c5 Ko-Hash: e7cfb74a9c3088b1""")) {
      koState.toString
    }
  }

  test("main line for 2002-01-10-7") {
    val tree: SgfTree = SgfParser.loadFromFile("games/2002-01-10-7.sgf")
    val moves = tree.getMainline

    assertResult("352, 88, 100, 340, 69, 371, 311, 346, 130, 90, 91, 111, 68, 109, 67, 112, 92, 214, 86, 107, 87, 108, 360, 361, 339, 318, 317, 296, 382, 383, 362, 341, 381, 298, 363, 384, 338, 364, 194, 196, 215, 235, 236, 238, 276, 297, 256, 257, 234, 213, 212, 233, 254, 255, 89, 110, 234, 277, 232, 255, 218, 217, 256, 192, 255, 191, 274, 295, 258, 278, 275, 237, 393, 392, 372, 329, 350, 349, 351, 328, 143, 115, 73, 95, 113, 134, 135, 114, 155, 133, 157, 177, 156, 199, 159, 74, 52, 99, 78, 77, 56, 161, 98, 119, 76, 118, 200, 221, 201, 203, 222, 242, 243, 263, 245, 164, 142, 163, 141, 160, 205, 225, 246, 226, 247, 206, 248, 144, 123, 165, 264, 285, 286, 307, 176, 197, 174, 175, 154, 153, 152, 132, 128, 129, 150, 149, 170, 127, 171, 169, 193, 190, 211, 148, 85, 172, 151") {
      moves.mkString(", ")
    }
  }

  test("main line for simple_match") {
    val tree: SgfTree = SgfParser.loadFromFile("games/simple_match.sgf")
    val moves = tree.getMainline

    assertResult("340, 100, 88, 352, 346, 94, 220, 368, 367, 347, 388, 389, 409, 410, 325, 326, 304, 305, 283, 284, 262, 263, 241, 242, 199, 221, 178, 200, 157, 179, 136, 158, 115, 137, 93, 116, 72, 73, 51, 52, 30, 31, 213, 95, 215, 227, 360, 374, 87, 101") {
      moves.mkString(", ")
    }
  }


  case class SgfResult(mainLineMoves: Int, numChildren: Int, winner: Byte = EMPTY, isInitialized: Boolean = true)

  private def verify(fileName: String, expectation: SgfResult): Unit = {
    val tree: SgfTree = SgfParser.loadFromFile("games/" + fileName)

    assertResult(expectation.mainLineMoves) {tree.countMainlineMoves()}
    assertResult(expectation.isInitialized) {tree.isInitialized}
    assertResult(expectation.numChildren) {tree.getNumChildren }
    assertResult(expectation.winner) {tree.getWinner}
  }
}