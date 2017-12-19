package leelazero.board

import leelazero.board.FastBoard._
import leelazero.uct.UctSearch
import leelazero.util.TimeControl
import leelazero.util.Utils._


/** Keeps track of all the moves played for this game */
class GameHistory() {

  private var currentState: KoState = _
  private var gameHistory: Seq[KoState] = Seq()
  private var timeControl: TimeControl = _
  private var moveNum = 0

  def initHistory(size: Short, komi: Float) {
    currentState = new KoState(size, komi)
    gameHistory = Seq(currentState)
    timeControl = new TimeControl(size)
    moveNum = 0
  }

  def initHistory(state: KoState) {
    currentState = state.copy()
    gameHistory = Seq(currentState)
    timeControl = new TimeControl(state.size)
    moveNum = 0
  }

  def copy(): GameHistory = {
    val newGhist = new GameHistory
    newGhist.gameHistory = gameHistory.map(_.copy())
    newGhist.currentState = gameHistory(moveNum)
    newGhist.timeControl = new TimeControl(timeControl)
    newGhist.moveNum = moveNum
    newGhist
  }

  def getCurrentState: KoState = currentState

  def resetGame(): Unit = {
    currentState.resetGame()
    gameHistory = Seq()
    gameHistory :+= currentState.copy()
    timeControl.resetClocks()
    moveNum = 0
  }

  /** Move the state foward one move.
    * @return true if moved forward, false if already most current
    */
  def forwardMove(): Boolean = {
    if (gameHistory.length > moveNum + 1) {
      moveNum += 1
      currentState = gameHistory(moveNum)
      true
    } else false
  }

  /* @return true if moved back, false if already at start. */
  def undoMove(): Boolean = {
    if (moveNum > 0) {
      moveNum -= 1
      // This also restores hashes as they're part of state
      currentState = gameHistory(moveNum)
      true
    } else false
  }

  /** Go all the way back to the start of the game */
  def rewind(): Unit = {
    currentState = gameHistory.head
    moveNum = 0
  }

  def playMove(vertex: Short): Unit = currentState.playMove(vertex)
  def playPass(): Unit = currentState.playPass()

  def playMove(color: Byte, vertex: Short): Unit = {
    if (vertex != PASS && vertex != RESIGN) {
      currentState.playMove(color, vertex)
    } else {
      playPass()
      if (vertex == RESIGN) {
        currentState.resign()
      }
    }
    moveNum += 1

    // cut off any leftover moves from navigating
    gameHistory = gameHistory.take(moveNum)
    gameHistory :+= currentState.copy()
  }

  def playTextMove(color: String, vertex: String): Boolean = {

    val boardSize = currentState.size

    val who = if (color == "w" || color == "white") {
       WHITE
    } else if (color == "b" || color == "black") {
       BLACK
    } else return false

    if (vertex.length < 2 || !vertex(0).isLetter || !vertex(1).isDigit || vertex(0) == 'i')
      return false

    val column = if (vertex(0) >= 'A' && vertex(0) <= 'Z') {
      if (vertex(0) < 'I') 25 + vertex(0) - 'A'
      else 25 + (vertex(0) - 'A') - 1
    } else {
      if (vertex(0) < 'i') vertex(0) - 'a'
      else (vertex(0) - 'a') - 1
    }

    val row = vertex.charAt(1).toInt - 1
    if (row >= boardSize || column >= boardSize)
      return false

    val move = currentState.getBoard.getVertex(column, row)
    playMove(who, move)
    true
  }

  def stopClock(color: Byte): Unit = timeControl.stop(color)
  def startClock(color: Byte): Unit = timeControl.start(color)
  def displayState(): Unit = {
    myPrint(toString)
    currentState.displayState()
    timeControl.displayTimes()
  }
  override def toString: String = {
    currentState.toString + "\n" + timeControl.toString
  }

  def getTimeControl: TimeControl = timeControl
  def setTimeControl(mainTime: Int, byoTime: Int, byoStones: Int, byoPeriods: Int): Unit =
    timeControl = new TimeControl(currentState.size, mainTime, byoTime, byoStones, byoPeriods)
  def setTimeControl(tmc: TimeControl) { timeControl = tmc }
  def adjustTime(color: Byte, time: Int, stones: Int) { timeControl.adjustTime(color, time, stones) }

  def anchorGameHistory(): Unit = {
    // handicap moves don't count in game history
    moveNum = 0
    gameHistory = Seq()
    gameHistory :+= currentState.copy()
  }

  def trimGameHistory(lastMove: Short): Unit = {
    moveNum = lastMove - 1
    gameHistory = gameHistory.take(lastMove)
  }

  def setFixedHandicap(handi: Short): Boolean = {
    if (!validHandicap(handi)) {
      return false
    }
    val board = currentState.getBoard
    val size = currentState.size
    val high = if (size >= 13) 3 else 2
    val mid = size / 2
    val low = size - 1 - high

    if (handi >= 2) {
      playMove(BLACK, board.getVertex(low, low))
      playMove(BLACK, board.getVertex(high, high))
    }
    if (handi >= 3) {
      playMove(BLACK, board.getVertex(high, low))
    }
    if (handi >= 4) {
      playMove(BLACK, board.getVertex(low, high))
    }
    if (handi >= 5 && handi % 2 == 1) {
      playMove(BLACK, board.getVertex(mid, mid))
    }
    if (handi >= 6) {
      playMove(BLACK, board.getVertex(low, mid))
      playMove(BLACK, board.getVertex(high, mid))
    }
    if (handi >= 8) {
      playMove(BLACK, board.getVertex(mid, low))
      playMove(BLACK, board.getVertex(mid, high))
    }

    board.setToMove(WHITE)
    anchorGameHistory()
    currentState.setHandicap(handi)
    true
  }

  /** @return number of placed handicap stones - which might be less than the number specified */
  private def setFixedHandicap2(handicap: Short): Int = {
    val size = currentState.size
    val board = currentState.getBoard
    val low = if (size >= 13)  3 else 2
    val mid = size / 2
    val high = size - 1 - low

    var interval = (high - mid) / 2
    var placed = 0

    while (interval >= 3) {
      for ( i <- low to high by interval) {
        for (j <- low to high by interval) {
          if (placed >= handicap) return placed
          if ((board.getSquare(i-1, j-1) == EMPTY) && (board.getSquare(i-1, j) == EMPTY) &&
            (board.getSquare(i-1, j+1) == EMPTY) && (board.getSquare(i, j-1) == EMPTY) &&
            (board.getSquare(i, j) == EMPTY) && (board.getSquare(i, j+1) == EMPTY) &&
            (board.getSquare(i+1, j-1) == EMPTY) && (board.getSquare(i+1, j) == EMPTY) &&
            (board.getSquare(i+1, j+1) == EMPTY)) {
            playMove(BLACK, board.getVertex(i, j))
            placed += 1
          }
        }
      }
      interval /= 2
    }
    placed
  }

  def validHandicap(handicap: Short): Boolean = {
    val size = currentState.size

    !((handicap < 2 || handicap > 9) ||
      (size % 2 == 0 && handicap > 4) ||
      (size == 7 && handicap > 4) ||
      (size < 7 && handicap > 0))
  }

  def placeFreeHandicap(origStones: Short): Unit = {
    val board = currentState.getBoard
    val limit = currentState.size * currentState.size
    var stones: Short = if (origStones > limit / 2) (limit / 2).toShort else origStones

    val fixPlace: Short = Math.min(9, stones).toShort
    setFixedHandicap(fixPlace)
    stones = (stones - fixPlace).toShort
    stones = (stones - setFixedHandicap2(stones)).toShort

    for (i <- 0 until stones) {
      val search = new UctSearch(this.getCurrentState.copy()) //std::make_unique<UCTSearch>(*this);

      val move = search.think(BLACK, UctSearch.NO_PASS.toShort)
      playMove(BLACK, move)
    }

    if (origStones > 0)  board.setToMove(WHITE)
    else board.setToMove(BLACK)

    anchorGameHistory()
    currentState.setHandicap(origStones)
  }
}