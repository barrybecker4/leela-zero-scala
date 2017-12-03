package leelazero

import FastBoardSerializer.{PASS, RESIGN}

/** maintains Zobrist hash history in order to detect supoer kos */
class KoState(val size: Short, val komi: Float) {
  assert(size < FastBoard.MAX_BOARD_SIZE)
  private val state = new FastState(size, komi)
  private var koHashHistory: Seq[Long] = Seq(state.calcKoHash)
  private var hashHistory: Seq[Long] = Seq(state.calcHash)

  /** @return true if there is a superKo. IOW if the koHash repeats in the history */
  def superKo(): Boolean = koHashHistory.tail.dropRight(1).contains(state.getKoHash)
  def superKo(newHash: Long): Boolean = koHashHistory.dropRight(1).contains(newHash)
  def getKoHash: Long = state.getKoHash
  def getToMove: Byte = state.getToMove
  def getBoard: FullBoard = state.getBoard

  def resetGame(): Unit = {
    state.resetGame()
    koHashHistory = Seq(state.calcKoHash)
    hashHistory = Seq(state.calcHash)
  }

  def playPass(): Unit = {
    state.playPass()
    updateHistory()
  }

  def playMove(vertex: Short): Unit = state.playMove(vertex)

  def playMove(color: Byte, x: Short, y: Short): Unit = playMove(color, state.getVertex(x, y))
  def playMove(color: Byte, vertex: Short): Unit = {
    if (vertex != PASS && vertex != RESIGN) {
      state.playMove(color, vertex)
      updateHistory()
    } else {
      playPass()
    }
  }

  private def updateHistory(): Unit = {
    koHashHistory :+= state.getKoHash
    hashHistory :+= state.getHash
  }
}