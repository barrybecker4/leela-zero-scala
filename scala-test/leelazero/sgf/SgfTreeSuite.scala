package leelazero.sgf

import org.scalatest.FunSuite
import leelazero.board.FastBoard._
import leelazero.TestUtil._


class SgfTreeSuite extends FunSuite {

  test("constructor without init") {
    val tree: SgfTree = new SgfTree()

    assertResult(INVALID) {tree.getWinner}
    assertThrows[AssertionError] {
      tree.getState
    }
  }

  test("constructor with init") {
    val tree: SgfTree = new SgfTree()
    tree.initState()

    assertResult(INVALID) {tree.getWinner}
    assertResult(true) {tree.isInitialized}
    assertResult(
      clean("""
       |Passes: 0
       |Black (X) Prisoners: 0
       |White (O) Prisoners: 0
       |Black (X) to move
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
       |Hash: 26f03c7022b1f5c5 Ko-Hash: c09064475382a152""")) {tree.getState.toString}
  }

  test("add child") {
    val tree: SgfTree = new SgfTree()
    tree.initState()

    assertResult(0) {tree.countMainlineMoves()}
    val child = new SgfTree()
    tree.addChild(child)

    assertResult(child) {tree.getChild(0)}
    assertResult(null) {tree.getChild(1)}
  }

}
