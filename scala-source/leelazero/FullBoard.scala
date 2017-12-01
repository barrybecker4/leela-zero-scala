package leelazero

import FastBoard._
import FullBoard._

object FullBoard {

  /** Initial Zobrist hash when computing hashes for board positions.
    * Largest scala Long is 9223372036854775807. The 0x prefix indicates hexadecimal.
    */
  val INITIAL_HASH = 0x1234567887654321L
}

class FullBoard(size: Short = MAX_BOARD_SIZE) extends FastBoard(size) {

  private var hash: Long = _
  private var koHash: Long = _
  private var zobrist: Zobrist = _

  def getHash: Long = hash

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

      emptyIdx(pos) = emptyCount
      emptySquare(emptyCount) = pos
      emptyCount += 1

      hash ^= zobrist.zobrist(square(pos))(pos)
      koHash ^= zobrist.zobrist(square(pos))(pos)

      removed += 1
      pos = next(pos)
    } while (pos != position)
    removed
  }

  /** @return Tromp-Taylor has positional superko value */
  def calcKoHash(): Long = {
    koHash = calcBaseHash()
    koHash
  }

  /** @return zobrish has for current board state */
  def calcHash(): Long = {
    hash = incorporatePrisoners(calcBaseHash())
    hash
  }

  def playPass(passes: Int): Unit = {
    hash  ^= 0xABCDABCDABCDABCDL
    toMove = otherColor(toMove)
    hash ^= zobrist.zobristPass(passes)
    hash ^= zobrist.zobristPass(passes)
  }

  def playMove(color: Byte, passes: Int): Unit = {
    if (toMove == color) {
      hash  ^= 0xABCDABCDABCDABCDL
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
    res ^= zobrist.zobristPristine(0)(prisoners(0))
    res ^= zobrist.zobristPristine(1)(prisoners(1))
    if (toMove == BLACK)
      res ^= 0xABCDABCDABCDABCDL
    res
  }

  /**
    * Returns ko square or suicide tag. Does not update side to move.
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
    val lastvertex = emptySquare(emptyCount)
    emptyIdx(lastvertex) = emptyIdx(i)
    emptySquare(emptyIdx(i)) = lastvertex

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
    super.displayBoard(lastMove)
    println(f"Hash: ${hash.toHexString} Ko-Hash: ${koHash.toHexString}\n")
  }
}