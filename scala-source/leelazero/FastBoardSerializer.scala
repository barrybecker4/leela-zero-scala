package leelazero

import leelazero.FastBoard.{BLACK, WHITE}

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
          case _ => if (board.starpoint(size, i, j)) "+" else "."
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
          var libs = board.countRealLiberties(vertex)
          if (libs > 9) { libs = 9 }
          res += f"$libs%1d"
        } else if (board.getSquare(i, j) == BLACK) {
          var libs = board.countRealLiberties(vertex)
          if (libs > 9)
            libs = 9
          res += f"$libs%1d"
        } else if (board.starpoint(size, i, j)) {
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
        } else if (board.starpoint(size, i, j)) {
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

}
