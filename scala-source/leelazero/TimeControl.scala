package leelazero

import TimeControl._
import Timing._
import Utils._

object TimeControl {
  val HOUR: Int = 60 * 60 * 100
  val MONTH: Int = 31 * 24 * 60 * 60 * 100

  /** Keep a 1 second margin for net hiccups */
  val BUFFER_CENTISECS: Int = 100 //cfg_lagbuffer_cs
}

/**  Timing info is per GTP and in centiseconds */
case class TimeControl(boardSize: Short,
                       mainTime: Int = HOUR, byoTime: Int = 0, byoStones: Int = 25, byoPeriods: Int = 0) {
  private var movesExpected: Int = 0

  private val remainingTime = Array.ofDim[Int](2) // main time per player
  private val stonesLeft = Array.ofDim[Int](2) // stones to play in byo period
  private val periodsLeft = Array.ofDim[Int](2) //  byo periods remaining
  private val inByo = Array.ofDim[Boolean](2) // player is in byo yomi
  private val times = Array.ofDim[Long](2) // storage for player times
  resetClocks()
  setBoardSize(boardSize)

  def this(tc: TimeControl) {
    this(tc.boardSize, tc.mainTime, tc.byoTime, tc.byoStones, tc.byoPeriods)
  }

  def resetClocks(): Unit = {
    for (i <- 0 to 1) {
      remainingTime(i) = mainTime
      stonesLeft(i) = byoStones
      periodsLeft(i) = byoPeriods
      inByo(i) = mainTime <= 0
      // Now that byo-yomi status is set, add time back to the clocks
      if (inByo(0))
        remainingTime(0) = byoTime
    }
  }

  def start(color: Byte): Unit = times(color) = getTime

  def stop(color: Byte): Unit = {
    val stop = getTime
    val elapsed: Int = timeDiff(times(color), stop)
    assert(elapsed >= 0)
    remainingTime(color) -= elapsed
    if (inByo(color)) {
      if (byoStones > 0)
        stonesLeft(color) -= 1
      else if (byoPeriods > 0) {
        if (elapsed > byoTime)
          periodsLeft(color) -= 1
      }
    }

    // times up, entering byo yomi
    if (!inByo(color) && remainingTime(color) <= 0) {
      remainingTime(color) = byoTime
      stonesLeft(color) = byoStones
      periodsLeft(color) = byoPeriods
      inByo(color) = true
    } else if (inByo(color) && byoStones > 0 && stonesLeft(color) <= 0) {
      // reset byoyomi time and stones
      remainingTime(color) = byoTime
      stonesLeft(color) = byoStones
    } else if (inByo(color) && byoPeriods > 0) {
      remainingTime(color) = byoTime
    }
  }

  /** Note this is constant as we play, so it's fair to underestimate quite a bit. */
  def setBoardSize(size: Short): Unit = movesExpected = (size * size) / 5
  def getRemainingTime(color: Int): Int = remainingTime(color)
  override def toString: String = Array[Byte](0, 1).map(serializeForPlayer).mkString("\n")
  def displayTimes(): Unit = for (i <- 0 to 1) myPrint(serializeForPlayer(i.toByte))

  private def serializeForPlayer(color: Byte): String = {
    var remaining: Int = remainingTime(color) / 100 // centiseconds to seconds
    val hours: Int = remaining / (60 * 60)
    remaining %= (60 * 60)
    val minutes: Int = remaining / 60
    val seconds = remaining % 60

    val colorStr = if (color == 0) "Black" else "White"
    var s = f"$colorStr time: $hours%02d:$minutes%02d:$seconds%02d"
    if (inByo(color)) {
      if (byoStones > 0) {
        s += f", ${stonesLeft(color)}%d stones left"
      } else if (byoPeriods > 0) {
        s += f", ${periodsLeft(color)}%d period(s) of ${byoTime / 100}%d seconds left"
      }
    }
    s
  }

  /** @return time allowed for move in centiseconds */
  def maxTimeForMove(color: Byte): Int = {
    var timeAlloc: Int = 0

    // no byo-yomi (absolute), easiest case
    if (byoTime == 0) {
      timeAlloc = (remainingTime(color) - BUFFER_CENTISECS) / movesExpected
    } else if (byoTime != 0) {
      if (byoStones == 0 && byoPeriods == 0)
        return MONTH // No periods or stones set means infinite time = 1 month

      // byo-yomi and in byo-yomi
      if (inByo(color)) {
        if (byoStones > 0) {
          timeAlloc = (remainingTime(color) - BUFFER_CENTISECS) / Math.max(stonesLeft(color), 1)
        } else {
          assert(byoPeriods > 0)
          timeAlloc = byoTime - BUFFER_CENTISECS  // Just use the byo yomi period
        }
      } else {
        // byo yomi time but not in byo yomi yet
        if (byoStones > 0) {
          val byoExtra: Int = byoTime / byoStones
          val totalTime: Int = remainingTime(color) + byoExtra
          timeAlloc = (totalTime - BUFFER_CENTISECS) / movesExpected
          // Add back the guaranteed extra seconds
          timeAlloc += Math.max(byoExtra - BUFFER_CENTISECS, 0)
        } else {
          assert(byoPeriods > 0)
          val byoExtra = byoTime * (periodsLeft(color) - 1)
          val totalTime = remainingTime(color) + byoExtra
          timeAlloc = (totalTime - BUFFER_CENTISECS) / movesExpected
          // Add back the guaranteed extra seconds
          timeAlloc += Math.max(byoTime - BUFFER_CENTISECS, 0)
        }
      }
    }
    timeAlloc = Math.max(timeAlloc, 0)
    timeAlloc
  }

  def adjustTime(color: Byte, time: Int, stones: Int): Unit = {
    remainingTime(color) = time
    // From pachi: some GTP things send 0 0 at the end of main time
    if (time == 0 && stones == 0) {
      inByo(color) = true
      remainingTime(color) = byoTime
      stonesLeft(color) = byoStones
      periodsLeft(color) = byoPeriods
    }
    if (stones > 0) {
      // stones are only given in byo-yomi
      inByo(color) = true
    }
    // We must be in byo-yomi before interpreting stones. The previous condition guarantees we do this if != 0
    if (inByo(color)) {
      if (byoStones > 0) {
        stonesLeft(color) = stones
      } else if (byoPeriods > 0) {
        // KGS extension
        periodsLeft(color) = stones
      }
    }
  }
}