package leelazero.board

import leelazero.board.FastBoard._
import leelazero.board.FastState.NUM_RECENT_MOVES
import leelazero.util.Utils._

object FastState {
  /** Number of recent moves to remember */
  val NUM_RECENT_MOVES = 16
}

/**
  * Maintains the state of the board
  */
class FastState(val size: Short, val komi: Float, val fboard: FullBoard = null) {

  private val board: FullBoard = if (fboard == null) new FullBoard(size) else fboard
  protected var moveNum: Int = 0
  protected var handicap: Short = 0
  protected var passes: Int = 0
  protected var koMove: Short = 0
  protected var lastWasCapture = false
  protected var lastMoves: Array[Short] = Array[Short]()

  def resetGame(): Unit = {
    board.resetBoard(size)
    moveNum = 0
    handicap = 0
    passes = 0
    koMove = 0
    for (i <- lastMoves.indices) lastMoves(i) = 0
    lastWasCapture = false
  }

  /** @return a list of valid moves */
  def generateMoves(color: Byte): Seq[Short] =
    for {
      i <- 0 until board.getEmptyCount
      vertex = board.getEmpties(i)
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
  def estimateMcScore: Int = board.estimateMcScore(komi + handicap)
  def getToMove: Byte = board.getToMove
  def setToMove(tomove: Byte) { board.setToMove(tomove) }
  def getHandicap: Short = handicap
  def setHandicap(h: Short) { handicap = h }
  def getLastMove: Short = if (lastMoves.isEmpty) -1 else lastMoves.head
  def getPrevLastMove: Short = if (lastMoves.length > 1) lastMoves(1) else -1
  def finalScore: Float = board.areaScore(komi + handicap)  // workstate?
  def getKoMove: Short = koMove
  def getVertex(x: Short, y: Short): Short = board.getVertex(x, y)
  def getBoard: FullBoard = board
  def getPasses: Int = passes

  def getHash: Long = board.getHash
  def getKoHash: Long = board.getKoHash
  def calcHash: Long = board.calcHash()
  def calcKoHash: Long = board.calcKoHash()

  def displayState(): Unit = {
    myPrint(f"\nPasses: $passes%d")
    myPrint(f"Black (X) Prisoners: ${board.getPrisoners(BLACK)}%d")
    myPrint(f"White (O) Prisoners: ${board.getPrisoners(WHITE)}%d")
    if (board.blackToMove()) {
      myPrint("Black (X) to move")
    } else {
      myPrint("White (O) to move")
    }
    board.displayBoard(getLastMove)
  }

  def resign(): Unit = {
    pushLastMove(RESIGN)
    lastWasCapture = false
  }

  private def pushLastMove(vertex: Short): Unit = {
    // just keep the last 15 moves
    lastMoves = lastMoves.take(NUM_RECENT_MOVES - 1)
    lastMoves = vertex +: lastMoves
  }

  private def incrementPasses(): Unit = {
    passes += 1
    if (passes > 4) passes = 4
  }
}