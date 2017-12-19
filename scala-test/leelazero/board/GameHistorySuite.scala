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

  test("init with koState") {
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

}
