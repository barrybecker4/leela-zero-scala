package leelazero.board

import org.scalatest.FunSuite
import FastBoard._
import FastStateSuite._
import leelazero.TestUtil._


object FastStateSuite {
  def createFilled5x5State(): FastState = {
    val b = new FullBoard(5)
    val state = new FastState(5, 0.5f, b)

    state.playMove((1, 1), BLACK)
    state.playMove((2, 1), BLACK)
    state.playMove((3, 1), WHITE)
    state.playMove((2, 2), WHITE)
    state.playMove((3, 2), BLACK)
    state.playMove((0, 3), BLACK)
    state.playMove((2, 3), WHITE)
    state.playMove((2, 4), WHITE)
    state.playMove((3, 3), WHITE)
    state.playMove((4, 4), WHITE)
    state.playMove((4, 3), BLACK)
    state.playMove((1, 2), WHITE)
    state.playMove((0, 2), BLACK)
    state.playMove((0, 0), WHITE)
    state.playMove((1, 0), BLACK)
    state.playMove((3, 0), BLACK)
    state
  }
}


class FastStateSuite extends FunSuite {

  val state = new FastState(5, 0.5f)

  test("default construction") {
    assertResult(5) { state.size }
    assertResult(0.5) { state.komi }
    assertResult(1) { state.estimateMcScore }
    assertResult(-0.5) { state.finalScore }
    assertResult(0) { state.getHandicap }
    assertResult(0) { state.getKoMove }
    assertResult(-1) { state.getLastMove }
    assertResult(-1) { state.getPrevLastMove }
    assertResult(0) { state.getMoveNum }
    assertResult(BLACK) { state.getToMove }
  }

  test("setters/getters") {
    state.setHandicap(8)
    assertResult(8) { state.getHandicap }
    state.setToMove(WHITE)
    assertResult(WHITE) { state.getToMove }
    state.playMove(23)

    assertResult(23) { state.getLastMove }
    assertResult(1) { state.getMoveNum }
    assertResult(-1) { state.getPrevLastMove }
    assertResult(BLACK) { state.getToMove }

    assertResult(-8) { state.estimateMcScore }
    assertResult(-33.5) { state.finalScore }
  }

  test("state after play (BLACK winning)") {
    val state = new FastState(9, 6.5f)
    state.setHandicap(4)
    state.setToMove(WHITE)
    state.playMove(state.getVertex(5, 4))
    state.playMove(state.getVertex(5, 3))
    state.playMove(state.getVertex(4, 5))

    assertResult(71) { state.getLastMove }
    assertResult(3) { state.getMoveNum }
    assertResult(50) { state.getPrevLastMove }
    assertResult(BLACK) { state.getToMove }

    assertResult(-10) { state.estimateMcScore }
    assertResult(-11.5) { state.finalScore }

    assertResult(
      "12, 23, 34, 45, 56, 67, 78, 89, 100, 13, 24, 35, 46, 57, 68, 79, 90, 101, 14, 25, 36, 47, 58, " +
      "69, 80, 91, 102, 15, 26, 37, 48, 59, 70, 81, 92, 103, 16, 27, 38, 49, 60, 86, 82, 93, 104, 17, 28, 39, 97, " +
      "108, 72, 83, 94, 105, 18, 29, 40, 51, 62, 73, 84, 95, 106, 19, 30, 41, 52, 63, 74, 85, 96, 107, 20, 31, " +
      "42, 53, 64, 75") {
      state.generateMoves(WHITE).mkString(", ")
    }
  }

  test("state after play (WHITE winning)") {
    val state = new FastState(9, 6.5f)
    state.setHandicap(4)
    state.setToMove(WHITE)
    state.playMove(state.getVertex(5, 4))
    state.playMove(state.getVertex(5, 3))
    state.playMove(state.getVertex(4, 5))
    state.playMove(state.getVertex(2, 2))
    state.playMove(state.getVertex(4, 3))
    state.playMove(state.getVertex(1, 2))
    state.playMove(state.getVertex(6, 3))
    state.playMove(state.getVertex(2, 3))
    state.playMove(state.getVertex(5, 2))
    state.playMove(state.getVertex(0, 0))
    state.playMove(state.getVertex(6, 6))

    state.displayState()

    assertResult(84) { state.getLastMove }
    assertResult(11) { state.getMoveNum }
    assertResult(12) { state.getPrevLastMove }
    assertResult(BLACK) { state.getToMove }

    assertResult(-11) { state.estimateMcScore }
    assertResult(-13.5) { state.finalScore }

    assertResult(
      "20, 23, 34, 45, 56, 67, 78, 89, 100, 13, 24, 53, 46, 57, 68, 79, 90, 101, 14, 25, 75, 31, 58, " +
      "69, 80, 91, 102, 15, 26, 37, 48, 59, 70, 81, 92, 103, 16, 27, 38, 64, 60, 86, 82, 93, 104, 17, 28, 97, " +
      "108, 72, 83, 94, 105, 18, 29, 40, 42, 62, 73, 107, 95, 106, 19, 30, 41, 52, 63, 74, 85, 96"
    ) {
      state.generateMoves(BLACK).mkString(", ")
    }
  }

  test("reset") {
    val fstate = createFilled5x5State()
    assertResult(16) {fstate.getMoveNum}
    assertResult(11) {fstate.getLastMove}
    assertResult(WHITE) {fstate.getToMove}
    fstate.resetGame()
    assertResult(BLACK) {fstate.getToMove}
    assertResult(0.5) {fstate.komi}
    assertResult(0) {fstate.getMoveNum}
    assertResult(0) {fstate.getLastMove}
  }

  test("toString") {
    val fstate = createFilled5x5State()
    fstate.displayState()
    assertResult(clean("""
       |Passes: 0
       |Black (X) Prisoners: 0
       |White (O) Prisoners: 0
       |White (O) to move
       |   a b c d e
       | 5 . . O . O  5
       | 4 X . O O X  4
       | 3 X O O X .  3
       | 2 . X X O .  2
       | 1 O X .(X).  1
       |   a b c d e
       |
       |Hash: d45d73afd91565f9 Ko-Hash: 97a9823d509053a3""")) {
      fstate.toString
    }
  }

}
