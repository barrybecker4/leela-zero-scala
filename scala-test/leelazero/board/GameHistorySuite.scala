package leelazero.board

import org.scalatest.FunSuite
import leelazero.TestUtil._
import leelazero.board.FastStateSuite._
import KoStateSuite._
import FastBoard._
import leelazero.util.TimeControl

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

  test("resetGame") {
    val hist = create3x3History()
    hist.resetGame()

    assertResult(clean(
      """
        |Passes: 0
        |Black (X) Prisoners: 0
        |White (O) Prisoners: 0
        |Black (X) to move
        |   a b c
        | 3 . . .  3
        | 2 . . .  2
        | 1 . . .  1
        |   a b c
        |
        |Hash: d7ddc46eacc89f24 Ko-Hash: cf4c15670ca98e6f""")) {
      hist.getCurrentState.toString
    }
  }

  test("play black textMove") {
    val hist = create3x3History()
    assertResult(true) {hist.playTextMove("black", "c3")}

    assertResult(clean(
      """
        |Passes: 0
        |Black (X) Prisoners: 0
        |White (O) Prisoners: 0
        |White (O) to move
        |   a b c
        | 3 O X(X) 3
        | 2 . O X  2
        | 1 O X .  1
        |   a b c
        |
        |Hash: c320387b8499c033 Ko-Hash: 707c42bf8f357ab5""")) {
      hist.getCurrentState.toString
    }
  }

  test("play white textMove") {
    val hist = create3x3History()
    assertResult(true) {hist.playTextMove("w", "c1")}

    assertResult(clean(
      """
        |Passes: 0
        |Black (X) Prisoners: 0
        |White (O) Prisoners: 1
        |Black (X) to move
        |   a b c
        | 3 O X .  3
        | 2 . O X  2
        | 1 O .(O) 1
        |   a b c
        |
        |Hash: 18a3487d67f824cb Ko-Hash: e144d893d378e944""")) {
      hist.getCurrentState.toString
    }
  }

  test("play out of bounds black textMove") {
    val hist = create3x3History()
    assertResult(false) {hist.playTextMove("black", "d6")}
  }

  test("trim game history") {
    val hist = create3x3History()
    assertResult(true) {hist.playTextMove("w", "c1")}

    assertResult(8) {hist.getHistoryLength}
    assertResult(7) {hist.getMoveNum}

    hist.trimGameHistory(3)

    assertResult(3) {hist.getHistoryLength}
    assertResult(2) {hist.getMoveNum}
  }

  test("anchor game history") {
    val hist = createEmpty(9)
    hist.setFixedHandicap(2)
    assertResult(true) {hist.playTextMove("w", "c1")}
    assertResult(true) {hist.playTextMove("b", "d2")}

    assertResult(3) {hist.getHistoryLength}
    assertResult(2) {hist.getMoveNum}

    hist.anchorGameHistory()

    assertResult(1) {hist.getHistoryLength}
    assertResult(0) {hist.getMoveNum}
  }

  test("copy") {
    val hist = create3x3History()
    assertResult(true) {hist.playTextMove("w", "c1")}

    val hist2 = hist.copy()
    assertResult(hist.getMoveNum){hist2.getMoveNum}
    assertResult(hist.getHistoryLength){hist2.getHistoryLength}
  }

  test("start stop timer") {
    val hist = createEmpty(9)
    assertResult(360000) { hist.getTimeControl.getRemainingTime(BLACK)}
    hist.startClock(BLACK)
    Thread.sleep(1500)
    hist.stopClock(BLACK)
    assertResult(359985) { hist.getTimeControl.getRemainingTime(BLACK)}
  }

  test("start stop timer with custom timer") {
    val hist = createEmpty(9)
    hist.setTimeControl(new TimeControl(9, 6000, 30, 0, 3))
    assertResult(6000) { hist.getTimeControl.getRemainingTime(BLACK)}
    hist.startClock(BLACK)
    Thread.sleep(1600)
    hist.stopClock(BLACK)
    assertResult(5984) { hist.getTimeControl.getRemainingTime(BLACK)}
  }

  // cannot have handcap on board less than 7 in size
  test("set fixed handicap 1 on 5x5") {
    val hist = createEmpty(5)
    //assertResult(true) {hist.playTextMove("w", "c1")}
    hist.setFixedHandicap(1)
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
        |Hash: 747dfc7043e29403 Ko-Hash: f00f674adb142abb""")) {
      hist.getCurrentState.toString
    }
  }

  test("set fixed handicap 3 on 9x9") {
    val hist = createEmpty(9)
    hist.setFixedHandicap(3)
    assertResult(clean(
      """
        |Passes: 0
        |Black (X) Prisoners: 0
        |White (O) Prisoners: 0
        |White (O) to move
        |   a b c d e f g h j
        | 9 . . . . . . . . .  9
        | 8 . . . . . . . . .  8
        | 7 . .(X). + . X . .  7
        | 6 . . . . . . . . .  6
        | 5 . . + . + . + . .  5
        | 4 . . . . . . . . .  4
        | 3 . . X . + . + . .  3
        | 2 . . . . . . . . .  2
        | 1 . . . . . . . . .  1
        |   a b c d e f g h j
        |
        |Hash: 759ebef2dc390cc7 Ko-Hash: 334746a6f701fd74""")) {
      hist.getCurrentState.toString
    }
  }

  // The computer figures out where to play the handicap stones.
  test("set fix free handicap 3 on 9x9") {
    val hist = createEmpty(9)
    hist.placeFreeHandicap(3)
    assertResult(clean(
      """
        |Passes: 0
        |Black (X) Prisoners: 0
        |White (O) Prisoners: 0
        |White (O) to move
        |   a b c d e f g h j
        | 9 . . . . . . . . .  9
        | 8 . . . . . . . . .  8
        | 7 . .(X). + . X . .  7
        | 6 . . . . . . . . .  6
        | 5 . . + . + . + . .  5
        | 4 . . . . . . . . .  4
        | 3 . . X . + . + . .  3
        | 2 . . . . . . . . .  2
        | 1 . . . . . . . . .  1
        |   a b c d e f g h j
        |
        |Hash: 759ebef2dc390cc7 Ko-Hash: 334746a6f701fd74""")) {
      hist.getCurrentState.toString
    }
  }

  private def createEmpty(size: Short): GameHistory = {
    val hist = new GameHistory()
    hist.initHistory(size, 6.5f)
    hist
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
