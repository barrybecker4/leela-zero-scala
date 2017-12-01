package leelazero

import Utils._
import FastBoardSerializer.{PASS, RESIGN}
import FastBoard.{BLACK, WHITE}


class FastState(val size: Short, val komi: Float) {

  private val board: FullBoard = new FullBoard(size)
  private var moveNum: Int = 0
  private var handicap: Short = 0
  private var passes: Int = 0
  private var koMove: Short = 0
  private var lastWasCapture = false
  private var lastMove: Array[Short] = Array[Short]()

  def resetGame(): Unit = {
    board.resetBoard(size)
    moveNum = 0
    handicap = 0
    passes = 0
    koMove = 0
    for (i <- lastMove.indices) lastMove(i) = 0
    lastWasCapture = false
  }

  /** @return a list of valid moves */
  def generateMoves(color: Short): Array[Short] =
    for {
      vertex <- board.getEmpties
      if vertex != koMove && !board.isSuicide(vertex, color)
    } yield vertex

  def playPass(): Unit = {
    moveNum += 1

    pushLastMove(PASS)
    lastWasCapture = false

    board.playPass(passes)
    incrementPasses()
  }

  def playMove(vertex: Short): Unit = {
    playMove(board.getToMove, vertex)
  }

  def playMove(color: Byte, vertex: Short): Unit = {
    if (vertex != PASS && vertex != RESIGN) {
      val (km, capture) = board.updateBoard(color, vertex)
      koMove = km
      lastWasCapture = capture

      pushLastMove(vertex)
      moveNum += 1

      board.playMove(color, passes)
      passes = 0
    } else {
      playPass()
    }
  }

  def getMoveNum: Int = moveNum
  def esitmateMsScore: Int = board.estimateMcScore(komi + handicap)
  def getToMove: Byte = board.getToMove
  def setToMove(tomove: Byte) { board.setToMove(tomove) }
  def getHandicap: Short = handicap
  def setHandicap(h: Short) { handicap = h }
  def getLastMove: Short = lastMove.head
  def getPrevLastMove: Short = lastMove(1)
  def finalScore: Float = board.areaScore(komi + handicap)  // workstate?
  def getKomi: Float = komi
  def getKoMove: Short = koMove

  def displayState(): Unit = {
    myPrint(f"\nPasses: $passes%d            Black (X) Prisoners: ${board.getPrisoners(BLACK)}%d\n")
    if (board.blackToMove()) {
      myPrint("Black (X) to move")
    } else {
      myPrint("White (O) to move")
    }
    myPrint(s"    White (O) Prisoners: ${board.getPrisoners(WHITE)}%d\n")

    board.displayBoard(getLastMove)
  }

  private def pushLastMove(vertex: Short): Unit = {
    // just keep the last 15 moves
    lastMove = lastMove.take(15)
    lastMove = vertex +: lastMove
  }

  private def incrementPasses(): Unit = {
    passes += 1
    if (passes > 4) passes = 4
  }
}