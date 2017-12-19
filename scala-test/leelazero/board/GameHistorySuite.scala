package leelazero.board

import org.scalatest.FunSuite
import leelazero.TestUtil._
import leelazero.board.FastStateSuite._
import KoStateSuite._
import FastBoard._

class GameHistorySuite extends FunSuite {

  val hist = new GameHistory()

  test("init without koState") {
    hist.initHistory(5, 6.5f)


    assertResult(clean(
      """
        |Passes: 0
        |Black (X) Prisoners: 0
        |White (O) Prisoners: 0
        |Black (X) to move
        |   a b c d e
        | 5 . . . . .  5
        | 4 . . . . .  4
        | 3 . . . . .  3
        | 2 . . . . .  2
        | 1 . . . . .  1
        |   a b c d e
        |
        |Hash: 747dfc7043e29403 Ko-Hash: f00f674adb142abb
        |Black time: 01:00:00
        |White time: 01:00:00""")) {hist.toString}
  }

  test("init with 3x3 koState") {
    val state: KoState = createSuperKoSetup3x3()
    hist.initHistory(state)
    hist.adjustTime(BLACK, time=1, stones=2)

    assertResult(clean(
      """
        |Passes: 0
        |Black (X) Prisoners: 0
        |White (O) Prisoners: 0
        |White (O) to move
        |   a b c
        | 3 O X .  3
        | 2 . O X  2
        | 1 O(X).  1
        |   a b c
        |
        |Hash: 1e8f126e0ab31829 Ko-Hash: add368aa011fa2af
        |Black time: 00:00:00, 2 stones left
        |White time: 01:00:00""")) {hist.toString}
  }

  test("test playMove") {
    val hist = create3x3History()

    assertResult(clean(
      """
        |Passes: 0
        |Black (X) Prisoners: 0
        |White (O) Prisoners: 0
        |White (O) to move
        |   a b c
        | 3 O X .  3
        | 2 . O X  2
        | 1 O(X).  1
        |   a b c
        |
        |Hash: 1e8f126e0ab31829 Ko-Hash: add368aa011fa2af""")) {hist.getCurrentState.toString}
  }

  test("undo") {
    val hist = create3x3History()
    hist.undoMove()
    hist.undoMove()

    assertResult(clean(
      """
        |Passes: 0
        |Black (X) Prisoners: 0
        |White (O) Prisoners: 0
        |White (O) to move
        |   a b c
        | 3 .(X).  3
        | 2 . O X  2
        | 1 O . .  1
        |   a b c
        |
        |Hash: 5dd1a99718428bf3 Ko-Hash: ee8dd35313ee3175""")) {hist.getCurrentState.toString}
  }

  test("forward") {
    val hist = create3x3History()
    hist.undoMove()
    hist.undoMove()
    hist.undoMove()
    hist.forwardMove()

    assertResult(clean(
      """
        |Passes: 0
        |Black (X) Prisoners: 0
        |White (O) Prisoners: 0
        |White (O) to move
        |   a b c
        | 3 .(X).  3
        | 2 . O X  2
        | 1 O . .  1
        |   a b c
        |
        |Hash: 5dd1a99718428bf3 Ko-Hash: ee8dd35313ee3175""")) {hist.getCurrentState.toString}
  }

  test("rewind, then forward 2") {
    val hist = create3x3History()
    hist.rewind()
    hist.forwardMove()
    hist.forwardMove()

    assertResult(clean(
      """
        |Passes: 0
        |Black (X) Prisoners: 0
        |White (O) Prisoners: 0
        |White (O) to move
        |   a b c
        | 3 . . .  3
        | 2 . .(X) 2
        | 1 O . .  1
        |   a b c
        |
        |Hash: a2ccde58da94f64d Ko-Hash: 1190a49cd1384ccb""")) {hist.getCurrentState.toString}
  }

  private def create3x3History(): GameHistory = {
    val hist = new GameHistory()
    hist.initHistory(3, 6.5f)
    val b = hist.getCurrentState.getBoard

    hist.playMove(WHITE, b.getVertex(0, 0))
    hist.playMove(BLACK, b.getVertex(2, 1))
    hist.playMove(WHITE, b.getVertex(1, 1))
    hist.playMove(BLACK, b.getVertex(1, 2))
    hist.playMove(WHITE, b.getVertex(0, 2))
    hist.playMove(BLACK, b.getVertex(1, 0))
    hist
  }
}
