package leelazero.util

import org.scalatest.FunSuite
import leelazero.board.FastBoard._
import  TimeControl._

class TimeControlSuite extends FunSuite {

  test("basic time control construction with defaults") {
    val tc = new TimeControl(boardSize = 9, mainTime = HOUR_IN_CENTISECONDS)

    assertResult(HOUR_IN_CENTISECONDS) {tc.getRemainingTime(BLACK)}
    assertResult(HOUR_IN_CENTISECONDS) {tc.getRemainingTime(WHITE)}
    assertResult(22493) {tc.maxTimeForMove(BLACK)}
    assertResult(22493) {tc.maxTimeForMove(WHITE)}
    assertResult("Black time: 01:00:00\nWhite time: 01:00:00") { tc.toString }
  }

  test("time control construction with custom values") {
    val tc = new TimeControl(boardSize = 17, mainTime = 100000, byoPeriods = 3, byoStones = 1, byoTime = 3000)

    assertResult(100000) {tc.getRemainingTime(BLACK)}
    assertResult(100000) {tc.getRemainingTime(WHITE)}
    assertResult(4705) {tc.maxTimeForMove(BLACK)}
    assertResult(4705) {tc.maxTimeForMove(WHITE)}
  }

  test("time control after updating") {
    val tc = new TimeControl(boardSize = 9, mainTime = HOUR_IN_CENTISECONDS)

    tc.adjustTime(BLACK, 500, 1)
    tc.adjustTime(WHITE, 400, 1)

    assertResult(500) {tc.getRemainingTime(BLACK)}
    assertResult(400) {tc.getRemainingTime(WHITE)}
    assertResult(25) {tc.maxTimeForMove(BLACK)}
    assertResult(18) {tc.maxTimeForMove(WHITE)}

    assertResult("Black time: 00:00:05, 1 stones left\nWhite time: 00:00:04, 1 stones left") { tc.toString }
  }

  test("time control start/stop") {
    val tc = new TimeControl(boardSize = 9, mainTime = HOUR_IN_CENTISECONDS)

    tc.start(BLACK)
    Thread.sleep(200)
    tc.start(WHITE)
    Thread.sleep(500)
    tc.stop(WHITE)
    tc.stop(BLACK)

    assertResult(359993) {tc.getRemainingTime(BLACK)}
    assertResult(359995) {tc.getRemainingTime(WHITE)}
    assertResult("Black time: 00:59:59\nWhite time: 00:59:59") { tc.toString }
  }

  test("time control start/stop with byo periods") {
    val tc = new TimeControl(boardSize = 9, mainTime = 60 * 60 * 10, byoPeriods = 3, byoStones = 0, byoTime = 100)

    tc.adjustTime(BLACK, 0, 0)
    tc.adjustTime(WHITE, 400, 0)

    tc.start(BLACK)
    Thread.sleep(150)
    tc.start(WHITE)
    Thread.sleep(1100)
    tc.stop(WHITE)
    tc.stop(BLACK)

    assertResult(100) {tc.getRemainingTime(BLACK)}
    assertResult(389) {tc.getRemainingTime(WHITE)}
    assertResult("Black time: 00:00:01, 0 period(s) of 1 seconds left\nWhite time: 00:00:03") { tc.toString }
  }
}
