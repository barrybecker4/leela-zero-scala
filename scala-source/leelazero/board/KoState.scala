package leelazero.board

import leelazero.board.FastBoard.{PASS, RESIGN}

/** maintains Zobrist hash history in order to detect super kos */
class KoState(size: Short, komi: Float, fboard: FullBoard = null) extends FastState(size, komi, fboard) {
  assert(size <= FastBoard.MAX_BOARD_SIZE)
  private var koHashHistory: Seq[Long] = Seq(calcKoHash)
  private var hashHistory: Seq[Long] = Seq(calcHash)

  /** @return true if there is a superKo. IOW if the koHash repeats in the history */
  def superKo(): Boolean = koHashHistory.tail.dropRight(1).contains(getKoHash)
  def superKo(newHash: Long): Boolean = koHashHistory.dropRight(1).contains(newHash)

  override def resetGame(): Unit = {
    super.resetGame()
    koHashHistory = Seq(calcKoHash)
    hashHistory = Seq(calcHash)
  }

  def playMove(color: Byte, x: Short, y: Short): Unit = playMove(color, super.getVertex(x, y))

  override def playMove(color: Byte, vertex: Short): Unit = {
    if (vertex != PASS && vertex != RESIGN) {
      super.playMove(color, vertex)
      updateHistory()
    } else {
      playPass()
    }
  }

  override def playPass(): Unit = {
    super.playPass()
    updateHistory()
  }

  def copy(): KoState = {
    val cp = new KoState(size, komi, getBoard.copy())
    cp.setToMove(this.getToMove)
    cp.setHandicap(this.getHandicap)
    cp.passes = this.passes
    cp.moveNum = this.moveNum
    cp.koMove = this.koMove
    cp.lastMoves = this.lastMoves
    cp.lastWasCapture = this.lastWasCapture
    cp.hashHistory = this.hashHistory
    cp.koHashHistory = this.koHashHistory
    cp
  }

  private def updateHistory(): Unit = {
    koHashHistory :+= getKoHash
    hashHistory :+= getHash
  }
}