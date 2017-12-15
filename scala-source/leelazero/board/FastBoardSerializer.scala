package leelazero.board

import leelazero.board.FastBoard._


/** Used to serialize a FastBoard in different ways */
class FastBoardSerializer(board: FastBoard) {

  def serialize(lastMove: Short = -1): String = {
    val colLabels = createColumnLabels()
    var result = "\n" + colLabels
    val size = board.getBoardSize

    for (j <- size - 1 to 0 by -1) {
      result += f"${j + 1}%2d"
      if (lastMove == board.getVertex(0, j))
        result += "("
      else
        result += " "
      for (i <- 0 until size) {
        result += (board.getSquare(i, j) match {
          case WHITE => "O"
          case BLACK => "X"
          case _ => if (starPoint(size, i, j)) "+" else "."
        })
        if (lastMove == board.getVertex(i, j)) result += ")"
        else if (i != size - 1 && lastMove == board.getVertex(i, j) + 1) result += "("
        else result += " "
      }
      result += f"${j + 1}%2d\n"
    }
    result += colLabels
    result + "\n"
  }

  /** @return a string that displays the board with the number of string liberties at every played position */
  def toLibertiesString(lastMove: Short): String = {
    var res = "\n" + createColumnLabels()
    val size = board.getBoardSize
    for (j <- size - 1 to 0 by -1) {
      res += f"${j + 1}%2d"
      if (lastMove == board.getVertex(0,j)) res += "("
      else res += " "
      for (i <- 0 until size) {
        val vertex = board.getVertex(i, j)
        if (board.getSquare(i, j) == WHITE) {
          var libs = board.countStringLiberties(vertex)
          if (libs > 9) { libs = 9 }
          res += f"$libs%1d"
        } else if (board.getSquare(i, j) == BLACK) {
          var libs = board.countStringLiberties(vertex)
          if (libs > 9)
            libs = 9
          res += f"$libs%1d"
        } else if (starPoint(size, i, j)) {
          res += "+"
        } else res += "."

        if (lastMove == vertex) res += ")"
        else if (i != size - 1 && lastMove == vertex + 1)
          res += "("
        else res += " "
      }
      res += f"${j + 1}%2d\n"
    }
    res
  }

  /** @return a string that displays the board with the id of the string at each string point */
  def toStringIdString(lastMove: Short): String = {
    var res = "\n" + createColumnLabels("  ")
    val size = board.getBoardSize
    for (j <- size - 1 to 0 by -1) {
      res += f"${j + 1}%2d"
      if (lastMove == board.getVertex(0, j)) res += "("
      else res += " "
      for (i <- 0 until size) {
        val color = board.getSquare(i, j)
        val vertex =  board.getVertex(i, j)
        if (color == WHITE || color == BLACK) {
          val stringId = board.getParentString(vertex)
          res += f"$stringId%2d"
        } else if (starPoint(size, i, j)) {
          res += "+ "
        } else res += ". "
        if (lastMove == vertex) res += ")"
        else if (i != size-1 && lastMove == vertex + 1) res += "("
        else res += " "
      }
      res += f"${j + 1}%2d\n"
    }
    res
  }

  def textToMove(move: String): Int = {
    if (move.length == 0 || move == "pass") {
      return PASS
    }
    if (move == "resign") {
      return RESIGN
    }

    val c1 = move(0).toLower
    var x: Int = c1 - 'a'
    // There is no i in ...
    assert(x != 8)
    if (x > 8) x -= 1
    val remainder = move.substring(1)
    val y: Int = remainder.toInt - 1
    board.getVertex(x, y)
  }

  /** @return the string coordinate (like A2) corresponding to the integer vertex position. */
  def moveToText(move: Short): String = {
    val (row, column) = getCoord(move)
    var result = ""

    if (board.validVertex(move)) {
      result += (if (column < 8) ('A' + column).toChar else ('A' + column + 1).toChar)
      result += (row + 1)
    } else if (move == PASS) {
      result += "pass"
    } else if (move == RESIGN) {
      result += "resign"
    } else {
      result += "error"
    }
    result
  }

  def moveToTextSgf(move: Short): String = {
    var (row, column) = getCoord(move)

    // SGF inverts rows
    row = board.getBoardSize - row - 1
    var result = ""

    if (board.validVertex(move)) {
      if (column <= 25) {
        result += ('a' + column).toChar
      } else {
        result  += ('A' + column - 26).toChar
      }
      if (row <= 25) {
        result += ('a' + row).toChar
      } else {
        result += ('A' + row - 26).toChar
      }
    }
    else if (move == PASS || move == RESIGN) { result += "tt" }
    else { result += "error" }
    result
  }

  private def getCoord(move: Int): (Int, Int) = {
    val size = board.getBoardSize
    val column = move % (size + 2) - 1
    val row = move / (size + 2) - 1
    assert(move == PASS || move == RESIGN || (row >= 0 && row < size))
    assert(move == PASS || move == RESIGN || (column >= 0 && column < size))
    (row , column)
  }

  private def createColumnLabels(padding: String = " "): String = {
    var result = ""
    for (i <- 0 until board.getBoardSize) {
      if (i < 25) {
        result += (if ('a' + i < 'i') 'a' + i else 'a' + i + 1).toChar + padding
      } else {
        result += (if ('A' + (i - 25) < 'I') 'A' + (i - 25) else 'A' + (i - 25) + 1).toChar + padding
      }
    }
    "   " + result.trim + "\n"
  }

  /** @return true if the specified position is a star point */
  private def starPoint(size: Short, x: Int, y: Int): Boolean = starPoint(size, y * size + x)
  private def starPoint(size: Short, point: Int): Boolean = {
    val stars = Array.ofDim[Int](3)
    val points = Array.ofDim[Int](2)
    var hits = 0

    if (size % 2 == 0 || size < 9) {
      return false
    }

    stars(0) = if (size >= 13) 3 else 2
    stars(1) = size / 2
    stars(2) = size - 1 - stars(0)

    points(0) = point / size
    points(1) = point % size

    for (i <- 0 until 2) {
      for (j <- 0 until 3) {
        if (points(i) == stars(j)) {
          hits += 1
        }
      }
    }
    hits >= 2
  }
}
