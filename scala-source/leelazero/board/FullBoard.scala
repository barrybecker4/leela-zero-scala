package leelazero.board

import leelazero.board.FastBoard._
import leelazero.board.FullBoard._
import leelazero.util.Utils.myPrint

object FullBoard {

  /** Initial Zobrist hash when computing hashes for board positions.
    * Largest scala Long is 9223372036854775807. The 0x prefix indicates hexadecimal.
    */
  val INITIAL_HASH = 0x1234567887654321L
  /** Hash applied after each move or pass */
  val MOVE_HASH = 0xABCDABCDABCDABCDL
}

class FullBoard(size: Short = MAX_BOARD_SIZE) extends FastBoard(size) {

  private var hash: Long = _
  private var koHash: Long = _
  private var zobrist: Zobrist = _

  def getHash: Long = hash
  def getKoHash: Long = koHash

  /**
    * Remove the string from the board that the specified position is part of.
    * @return the number of stones in the string that was removed from the board
    */
  def removeString(position: Short): Int = {
    var pos: Short = position
    var removed: Int = 0
    val color: Short = square(position)

    do {
      hash ^= zobrist.zobrist(square(pos))(pos)
      koHash ^= zobrist.zobrist(square(pos))(pos)

      square(pos) = EMPTY
      parentString(pos) = maxSq
      totalStones(color) -= 1

      removeNeighbor(pos, color)

      emptyIdices(pos) = emptyCount
      emptySquares(emptyCount) = pos
      emptyCount += 1

      hash ^= zobrist.zobrist(square(pos))(pos)
      koHash ^= zobrist.zobrist(square(pos))(pos)

      removed += 1
      pos = next(pos)
    } while (pos != position)
    removed
  }

  def copy(): FullBoard = {
    val cp = new FullBoard(size)
    cp.square = this.square.map(identity)
    cp.emptyIdices = this.emptyIdices.map(identity)
    cp.emptyCount = this.emptyCount
    cp.parentString = this.parentString
    cp.stringLiberties = this.stringLiberties.map(identity)
    cp.stonesInString = this.stonesInString.map(identity)
    cp.neighbors = this.neighbors.map(identity)
    cp.next = this.next
    cp.prisoners = this.prisoners.map(identity)
    cp.totalStones = this.totalStones.map(identity)
    cp.toMove = this.toMove
    cp.hash = this.getHash
    cp.koHash = this.getKoHash
    cp.zobrist = this.zobrist
    cp
  }

  /** @return Tromp-Taylor has positional superko value */
  def calcKoHash(): Long = {
    koHash = calcBaseHash()
    koHash
  }

  /** @return zobrish hash for current board state */
  def calcHash(): Long = {
    hash = incorporatePrisoners(calcBaseHash())
    hash
  }

  def playPass(passes: Int): Unit = {
    hash  ^= MOVE_HASH
    toMove = otherColor(toMove)
    hash ^= zobrist.zobristPass(passes)
    hash ^= zobrist.zobristPass(passes)
  }

  def playMove(color: Byte, passes: Int): Unit = {
    if (toMove == color) {
      hash  ^= MOVE_HASH
    }
    toMove = otherColor(color)

    if (passes > 0) {
      hash ^= zobrist.zobristPass(passes)
      hash ^= zobrist.zobristPass(0)
    }
  }

  private def calcBaseHash(): Long = {
    var res = INITIAL_HASH

    for (i <- 0 until maxSq) {
      if (square(i) != INVALID)
        res ^= zobrist.zobrist(square(i))(i)
    }
    res
  }

  /** @return hash modified by current prisoners. Prisoner hashing is rule set dependent */
  private def incorporatePrisoners(hash: Long): Long = {
    var res = hash
    res ^= zobrist.zobristPristine(BLACK)(prisoners(BLACK))
    res ^= zobrist.zobristPristine(WHITE)(prisoners(WHITE))
    if (toMove == BLACK)
      res ^= MOVE_HASH
    res
  }

  /**
    * Returns ko square (if there is one, else -1) and whether there were captures made. Does not update side to move.
    * If in the middle of a ko, then returns (position of ko, 1)
    * If i is a suicide move, then returns (-1, true)
    * If move captures 4 stones, then returns (-1, true)
    * @param color player who placed a stone
    * @param i position
    * @return (ko square, capture) If no capture then return (-1, false)
    */
  def updateBoard(color: Byte, i: Short): (Short, Boolean) = {
    assert(square(i) == EMPTY)

    hash ^= zobrist.zobrist(square(i))(i)
    koHash ^= zobrist.zobrist(square(i))(i)

    square(i) = color.toByte
    next(i) = i
    parentString(i) = i
    stringLiberties(i) = countPseudoLiberties(i)
    stonesInString(i) = 1
    totalStones(color) += 1

    hash ^= zobrist.zobrist(square(i))(i)
    koHash ^= zobrist.zobrist(square(i))(i)

    /* update neighbor liberties (they all lose 1) */
    addNeighbor(i, color)

    /* did we play into an opponent eye? */
    val eyePlay = neighbors(i) & EYE_MASK(otherColor(color))

    var capturedSq: Short = 0
    var capturedStones = 0

    for (k <- 0 until 4) {
      val ai = (i + directions(k)).toShort

      if (square(ai) == otherColor(color)) {
        if (stringLiberties(parentString(ai)) <= 0) {
          val thisCaptured = removeString(ai)
          capturedSq = ai
          capturedStones += thisCaptured
        }
      } else if (square(ai) == color) {
        val ip = parentString(i)
        val aip = parentString(ai)

        if (ip != aip) {
          if (stonesInString(ip) >= stonesInString(aip)) mergeStrings(ip, aip)
          else mergeStrings(aip, ip)
        }
      }
    }

    hash ^= zobrist.zobristPristine(color)(prisoners(color))
    prisoners(color) += capturedStones
    hash ^= zobrist.zobristPristine(color)(prisoners(color))

    /* move last vertex in list to our position */
    emptyCount -= 1
    val lastvertex = emptySquares(emptyCount)
    emptyIdices(lastvertex) = emptyIdices(i)
    emptySquares(emptyIdices(i)) = lastvertex

    /* check whether we still live (i.e. detect suicide) */
    if (stringLiberties(parentString(i)) == 0) {
      assert(capturedStones == 0)
      removeStringFast(i)
    }

    var capture = false
    if (capturedStones > 0) {
      capture = true
      /* check for possible simple ko */
      if (capturedStones == 1 && eyePlay > 0) {
        return (capturedSq, capture)
      }
    }

    (-1, capture)
  }

  override def resetBoard(size: Short = MAX_BOARD_SIZE): Unit = {
    super.resetBoard(size)
    zobrist = new Zobrist(boardSize)
    calcHash()
    calcKoHash()
  }

  override def displayBoard(lastMove: Short = -1): Unit = {
    myPrint(toString(lastMove))
  }

  override def toString: String = toString(-1)
  override def toString(lastMove: Short): String = {
    var s = super.toString(lastMove)
    s + f"Hash: ${hash.toHexString} Ko-Hash: ${koHash.toHexString}"
  }
}