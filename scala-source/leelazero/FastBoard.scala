package leelazero

import FastBoard._


object FastBoard {

  /** Neighbor counts are up to 4, so 3 bits is ok, but a power of 2 makes things a bit faster */
  val NBR_SHIFT: Short = 4

  /** largest board supported */
  val MAX_BOARD_SIZE: Short = 19

  /** highest existing square */
  val MAX_LIBS: Short =  16384  // 2^14

  /** infinite score */
  val BIG = 10000000

  /** vertex of a pass */
  val PASS: Short   = -1

  /**  vertex of a "resign move" */
  val RESIGN: Short = -2

  /** possible contents of a square */    // square_t
  val BLACK: Byte = 0
  val WHITE: Byte = 1
  val EMPTY: Byte = 2
  val INVALID: Byte = 3

  type Point = Tuple2[Short, Short]
  type MoveScore = Tuple2[Short, Float]    // movescore_t

  /**  bit masks to detect eyes on neighbors */
  val EYE_MASK: Array[Short] = Array(        // s_eyemask
    (4 * (1 << (NBR_SHIFT * BLACK))).toShort,
    (4 * (1 << (NBR_SHIFT * WHITE))).toShort
  )

  val CINVERT = Array(WHITE, BLACK, EMPTY, INVALID) // s_cinvert
}

/**
  * Manages moves on the go board.
  */
class FastBoard(size: Short = MAX_BOARD_SIZE) {

  private var boardSize: Short = size
  private var scoreMoves: Seq[MoveScore] = _

  private var square: Array[Byte] = _    // Board contents
  private var next: Array[Short] = _     // next stone in string
  private var parentString: Array[Short] = _  // parent string of vertex
  private var stringLiberties: Array[Int] = _  // liberties per string parent
  private var stonesInString: Array[Int] = _   // stones per string parent
  private var neighbors: Array[Int] = _  // counts of neighboring stones
  private var directions: Array[Int] = _      // movement in 4 directions
  private var extraDirections: Array[Int] = _ // movement in 8 directions
  private var prisoners: Array[Int] = _     // prisoners per color
  private var totalStones: Array[Int] = _   // total stones per color
  private var critical: Seq[Short] = Seq()  // queue of critical points  (use dropRight to pop)
  private var emptySquare: Array[Short] = _ // empty squares
  private var emptyIdx: Array[Int] = _  // indices of empty squares
  private var emptyCnt: Int = _
  private var toMove: Byte = _
  private var maxSq: Short = _
  resetBoard(boardSize)

  def getBoardSize: Short = boardSize

  /** @return index into 1D board arrays from the x, y coordinate */
  def getVertex(x: Short, y: Short): Short = {
    assert(x >= 0 && x < boardSize, "x out of range: " + x)
    assert(y >= 0 && y < boardSize, "y out of range: " + y)
    val vertex: Short = (((y + 1) * (boardSize + 2)) + (x + 1)).toShort
    assert(vertex >= 0 && vertex < maxSq)
    vertex
  }

  def getVertex(x: Int, y: Int): Short = getVertex(x.toShort, y.toShort)

  /** @return the x,y coordinate from the 1D index */
  def getXY(vertex: Short): Point = {
    val x: Short = ((vertex % (boardSize + 2)) - 1).toShort
    val y: Short = ((vertex / (boardSize + 2)) - 1).toShort

    assert(x >= 0 && x < boardSize)
    assert(y >= 0 && y < boardSize)
    assert(getVertex(x, y) == vertex)
    (x, y)
  }

  def getSquare(vertex: Int): Byte = {
    assert(vertex >= 0 && vertex < maxSq)
    square(vertex)
  }

  def setSquare(vertex: Int, content: Byte): Unit = {
    assert(vertex >= 0 && vertex < maxSq)
    assert(content >= BLACK && content <= INVALID)
    square(vertex) = content
  }

  def getSquare(x: Int, y: Int): Byte = getSquare(getVertex(x, y))
  def setSquare(x: Int, y: Int, content: Byte): Unit = setSquare(getVertex(x, y), content)

  /** Take advantage of the 8 fold board symmetry to rotate a given position */
  def rotateVertex(vertex: Short, symmetry: Short): Short = {
    assert(symmetry >= 0 && symmetry <= 7)
    val xy: Point = getXY(vertex)
    val x: Short = xy._1
    val y: Short = xy._2
    val reverseX: Short = (boardSize - x - 1).toShort
    val reverseY: Short = (boardSize - y - 1).toShort

    val newxy: Tuple2[Short, Short] = symmetry match {
      case 0 => (x, y)
      case 1 => (reverseX, y)
      case 2 => (x, reverseY)
      case 3 => (reverseX, reverseY)
      case 4 => (y, x)
      case 5 => (reverseY, x)
      case 6 => (y, reverseX)
      case 7 => (reverseY, reverseX)
      case _ => throw new IllegalArgumentException("Unexpected symmetry value: " + symmetry)
    }

    getVertex(newxy._1, newxy._2)
  }

  def resetBoard(size: Short): Unit = {
    assert(size <= MAX_BOARD_SIZE)
    boardSize = size
    maxSq = ((size + 2) * (size + 2)).toShort

    square = Array.ofDim[Byte](maxSq)
    next = Array.ofDim[Short](maxSq + 1)
    parentString = Array.ofDim[Short](maxSq + 1)
    stringLiberties = Array.ofDim[Int](maxSq + 1)
    stonesInString = Array.ofDim[Int](maxSq + 1)
    neighbors = Array.ofDim[Int](maxSq )
    directions = Array.ofDim[Int](4)
    extraDirections = Array.ofDim[Int](8)
    prisoners = Array.ofDim[Int](2)
    totalStones = Array.ofDim[Int](2)
    critical = Seq()
    emptySquare = Array.ofDim[Short](maxSq)
    emptyIdx = Array.ofDim[Int](maxSq)
    initializeAfterReset()
  }

  private def initializeAfterReset(): Unit = {
    toMove = BLACK
    prisoners(BLACK) = 0
    prisoners(WHITE) = 0
    totalStones(BLACK) = 0
    totalStones(WHITE) = 0
    emptyCnt = 0

    directions(0) = -boardSize - 2
    directions(1) = +1
    directions(2) = +boardSize + 2
    directions(3) = -1

    extraDirections(0) = -boardSize - 2 - 1
    extraDirections(1) = -boardSize - 2
    extraDirections(2) = -boardSize - 2 + 1
    extraDirections(3) = -1
    extraDirections(4) = +1
    extraDirections(5) = +boardSize + 2 - 1
    extraDirections(6) = +boardSize + 2
    extraDirections(7) = +boardSize + 2 + 1

    for (i <- 0 until maxSq) {
      square(i) = INVALID
      neighbors(i) = 0
      parentString(i) = maxSq
    }

    initializeNeighbors()

    parentString(maxSq ) = maxSq
    stringLiberties(maxSq) = MAX_LIBS   /* subtract from this */
    next(maxSq) = maxSq
  }

  private def initializeNeighbors(): Unit = {
    for (i <- 0 until boardSize) {
      for (j <- 0 until boardSize) {
        val vertex: Short = getVertex(i, j)

        square(vertex) = EMPTY
        emptyIdx(vertex) = emptyCnt
        emptySquare(emptyCnt) = vertex
        emptyCnt = (emptyCnt + 1).toShort

        if (i == 0 || i == boardSize - 1) initializeOnBorder(vertex) else initializeCenterPoint(vertex)
        if (j == 0 || j == boardSize - 1) initializeOnBorder(vertex) else initializeCenterPoint(vertex)
      }
    }
  }

  private def initializeOnBorder(vertex: Short): Unit = {
    neighbors(vertex) += (1 << (NBR_SHIFT * BLACK)) | (1 << (NBR_SHIFT * WHITE))
    neighbors(vertex) += 1 << (NBR_SHIFT * EMPTY)
  }
  private def initializeCenterPoint(vertex: Short): Unit = { neighbors(vertex) += 2 << (NBR_SHIFT * EMPTY) }

  /** @return true if placing the specified colored stone at the specified position would be suicide */
  def isSuicide(vertex: Short, color: Short): Boolean = {
    if (countPointLiberties(vertex) > 0) return false

    var connecting = false

    for (k <- 0 until 4) {
      val ai = vertex + directions(k)

      val libs = countStringLiberties(ai)
      if (getSquare(ai) == color) {
        if (libs > 1) {
          return false // connecting to live group = never suicide
        }
        connecting = true
      } else {
        if (libs <= 1) {
          return false // killing a neighbor is never suicide
        }
      }
    }

    addNeighbor(vertex, color)
    var opps_live = true
    var ours_die = true

    for (k <- 0 until 4) {
      val ai = vertex + directions(k)
      val libs = countStringLiberties(ai)

      if (libs == 0 && getSquare(ai) != color) {
        opps_live = false
      } else if (libs != 0 && getSquare(ai) == color) {
        ours_die = false
      }
    }

    removeNeighbor(vertex, color)
    if (!connecting) opps_live else opps_live && ours_die
  }

  /**
    * Count all neighboring empty cells to a group as a liberty.
    * When placing stones adjacent to that group, add one pseudo liberty for neighboring empty cells,
    * and remove one pseudo liberty for each neighboring group.
    * There may be duplicate pseudo liberties with certain "bent" shapes.
    * However, the test for 0 liberties remains correct, and there is also a simple "atari" check.
    * @return number of pseudo liberties
    */
  private def countPointLiberties(vertex: Short): Short = {
    countNeighbors(EMPTY, vertex)
  }

  /**
    * Actual liberties of the string the specified vertex position belongs to.
    * Possibly implemented using union-find (need to verify).
    * The difference is speed, pseudo liberties have O(1) adding and removal,
    * and real liberties have the inverse Ackermann function for that,
    * but in practice there is a significant speed difference.
    * @return number of real liberties
    */
  def countStringLiberties(vertex: Int): Int = stringLiberties(parentString(vertex))

  /**
    * @return Count of neighbors of color c at vertex v.
    *   The border of the board has fake neighbors of both colors.
    */
  def countNeighbors(c: Short, vertex: Short): Short = {
    assert(c == WHITE || c == BLACK || c == EMPTY)
    ((neighbors(vertex) >> (NBR_SHIFT * c)) & 7).toShort
  }

  def getStoneCount: Int = totalStones.sum

  /**
    * The score is from the point of view of the black player. A negative score means white is leading.
    * @return score needed for scoring passed out games not in Monte-Carlo play-outs
    */
  def areaScore(komi: Float): Float = {
    val white = calcReachColor(WHITE)
    val black = calcReachColor(BLACK)
    var score = -komi

    for (i <- 0 until boardSize) {
      for (j <- 0 until boardSize) {
        val vertex = getVertex(i, j)
        val isWhite = white(vertex)
        val isBlack = black(vertex)
        if (isWhite && !isBlack) {
          score -= 1.0F
        } else if (isBlack && !isWhite) {
          score += 1.0F
        }
      }
    }
    score
  }

  def estimateMcScore(komi: Float): Int = {
    val wsc = totalStones(BLACK)
    val bsc = totalStones(WHITE)
    bsc - wsc - komi.toShort + 1
  }

  def finalMcScore(komi: Float): Float = {
    val maxempty = emptyCnt
    var bsc = totalStones(BLACK)
    var wsc = totalStones(WHITE)

    for (v <- 0 until maxempty) {
      val i = emptySquare(v)
      assert(square(i) == EMPTY)

      val allblack = ((neighbors(i) >> (NBR_SHIFT * BLACK)) & 7) == 4
      val allwhite = ((neighbors(i) >> (NBR_SHIFT * WHITE)) & 7) == 4

      if (allwhite) { wsc += 1 }
      else if (allblack) { bsc += 1 }
    }
    bsc - wsc + komi
  }

  /** Print the board as text */
  def displayBoard(lastMove: Short = -1): Unit = {
    print(toString(lastMove))
  }

  override def toString: String = toString(-1)
  def toString(lastMove: Short): String = new FastBoardSerializer(this).serialize(lastMove)


  def displayLiberties(lastMove: Short):  Unit = {
    val fbs = new FastBoardSerializer(this)
    println(fbs.toLibertiesString(lastMove))
    println(fbs.toStringIdString(lastMove))
  }

  private def addNeighbor(vertex: Short, color: Short): Unit = {
    assert(color == WHITE || color == BLACK || color == EMPTY)

    val nbrParents = Array.ofDim[Short](4)
    var nbrParentCount = 0

    for (k <- 0 until 4) {
      val ai = vertex + directions(k)
      neighbors(ai) += (1 << (NBR_SHIFT * color)) - (1 << (NBR_SHIFT * EMPTY))

      var found = false
      var i = 0
      while (i < nbrParentCount && !found) {
        if (nbrParents(i) == parentString(ai)) {
          found = true
        }
        i += 1
      }
      if (!found) {
        stringLiberties(parentString(ai)) -= 1
        nbrParents(nbrParentCount) = parentString(ai)
        nbrParentCount += 1
      }
    }
  }

  private def removeNeighbor(vertex: Short, color: Short): Unit = {
    assert(color == WHITE || color == BLACK || color == EMPTY)
    val nbrParents = Array.ofDim[Short](4)
    var nbrParentCount = 0

    for (k <- 0 until 4) {
      val ai = vertex + directions(k)

      neighbors(ai) += (1 << (NBR_SHIFT * EMPTY)) - (1 << (NBR_SHIFT * color))

      var found = false
      var i = 0
      while (i < nbrParentCount && !found) {
        if (nbrParents(i) == parentString(ai)) {
          found = true
        }
        i += 1
      }
      if (!found) {
        stringLiberties(parentString(ai)) += 1
        nbrParents(nbrParentCount) = parentString(ai)
        nbrParentCount += 1
      }
    }
  }

  private def otherColor(color: Short): Short =
    if (color == BLACK) WHITE
    else if (color == WHITE) BLACK
    else throw new IllegalStateException("Unexpected color: " + color)

  private def fastSsSuicide(color: Short, vertex: Short): Boolean = {
    val eyePlay = neighbors(vertex) & EYE_MASK(otherColor(color))

    if (eyePlay > 0) return false

    !((countStringLiberties(vertex - 1) <= 1) ||
      (countStringLiberties(vertex + 1) <= 1) ||
      (countStringLiberties(vertex + boardSize + 2) <= 1) ||
      (countStringLiberties(vertex - boardSize - 2) <= 1))
  }

  /** @return the number of stones in the string that was removed */
  private def removeStringFast(vertex: Short): Int = {
    var pos: Short = vertex
    var removed = 0
    val color = square(vertex)
    assert(color == WHITE || color == BLACK || color == EMPTY)

    do {
      assert(square(pos) == color)
      square(pos) = EMPTY
      parentString(pos) = maxSq
      totalStones(color) -= 1

      removeNeighbor(pos, color)

      emptyIdx(pos) = emptyCnt
      emptySquare(emptyCnt) = pos
      emptyCnt += 1

      removed += 1
      pos = next(pos)
    } while (pos != vertex)

    removed
  }

  private def calcReachColor(col: Short): Array[Boolean] = {
    val bd = Array.fill[Boolean](maxSq)(false)
    var last = Array.fill[Boolean](maxSq)(false)

    do {  //  needs multi-pass propagation, slow
      last = bd
      for (i <- 0 until boardSize) {
        for (j <- 0 until boardSize) {
          val vertex = getVertex(i, j)
          // colored field, spread
          if (square(vertex) == col) {
            bd(vertex) = true
            for (k <- 0 until 4) {
              if (square(vertex + directions(k)) == EMPTY) {
                bd(vertex + directions(k)) = true
              }
            }
          } else if (square(vertex) == EMPTY && bd(vertex)) {
            for (k <- 0 until 4) {
              if (square(vertex + directions(k)) == EMPTY) {
                bd(vertex + directions(k)) = true
              }
            }
          }
        }
      }
    } while (last != bd)
    bd
  }

  private def mergeStrings(ip: Short, aip: Short): Unit = {
    assert(ip != maxSq && aip != maxSq)
    stonesInString(ip) += stonesInString(aip) // merge stones
    var newpos = aip              // loop over stones, update parents

    do {
      // check if this stone has a liberty
      for (k <- 0 until 4) {
        val ai = newpos + directions(k)
        // for each liberty, check if it is not shared
        if (square(ai) == EMPTY) {
          // find liberty neighbors
          var found = false
          var kk = 0
          while (kk < 4 && !found) {
            val aai = ai + directions(kk)
            // Friendly string shouldn't be ip. ip can also be an aip that has been marked.
            if (parentString(aai) == ip) {
              found = true
            }
            kk += 1
          }

          if (!found) stringLiberties(ip) += 1
        }
      }

      parentString(newpos) = ip
      newpos = next(newpos)
    } while (newpos != aip)

    // merge strings
    val tmp = next(aip)
    next(aip) = next(ip)
    next(ip) = tmp
  }

  /** @return the captured square, if any. Else -1 */
  private def updateBoardEye(color: Short, i: Short): Short = {
    square(i)  = color.toByte
    next(i) = i
    parentString(i) = i
    stringLiberties(i) = 0
    stonesInString(i)  = 1
    totalStones(color) += 1

    addNeighbor(i, color)

    var captured_sq: Int = 0
    var captured_stones = 0

    for (k <- 0 until 4) {
      val ai = i + directions(k)
      assert(ai >= 0 && ai <= maxSq)

      if (countStringLiberties(ai) <= 0) {
        val this_captured = removeStringFast(ai.toShort)
        captured_sq = ai
        captured_stones += this_captured
      }
    }

    // move last vertex in list to our position
    emptyCnt -= 1
    val lastvertex = emptySquare(emptyCnt)
    emptyIdx(lastvertex) = emptyIdx(i)
    emptySquare(emptyIdx(i)) = lastvertex
    prisoners(color) += captured_stones

    // possibility of ko
    if (captured_stones == 1) {
       captured_sq
    }
    -1
  }

  /**
    * Returns ko square or suicide tag. Does not update side to move.
    * @param color player who placed a stone
    * @param vertex position
    * @return (ko square, capture) If no capture then return (-1, false)
    */
  def updateBoardFast(color: Short, vertex: Short): (Int, Boolean) = {
    assert(square(vertex) == EMPTY)
    assert(color == WHITE || color == BLACK)

    val eyePlay: Boolean = (neighbors(vertex) & EYE_MASK(otherColor(color))) > 0   // did we play into an opponent eye?

    // because we check for single stone suicide, we know it's a capture, and it might be a ko capture
    var capture = false
    if (eyePlay) {
      return (updateBoardEye(color, vertex), true)
    }

    square(vertex) = color.toByte
    next(vertex) = vertex
    parentString(vertex) = vertex
    stringLiberties(vertex) = countPointLiberties(vertex)
    stonesInString(vertex) = 1
    totalStones(color) += 1

    addNeighbor(vertex, color)

    for (k <- 0 until 4) {
      val ai = vertex + directions(k)

      if (square(ai) <= WHITE) {
        assert(ai >= 0 && ai <= maxSq)

        if (square(ai) == otherColor(color)) {
          if (countStringLiberties(ai) <= 0) {
            capture = true
            prisoners(color) += removeStringFast(ai.toShort)
          }
        } else if (square(ai) == color) {
          val ip  = parentString(vertex)
          val aip = parentString(ai)

          if (ip != aip) {
            if (stonesInString(ip) >= stonesInString(aip)) {
              mergeStrings(ip, aip)
            } else {
              mergeStrings(aip, ip)
            }
          }
        }
      }
    }

    /* move last vertex in list to our position */
    emptyCnt -= 1
    val lastvertex = emptySquare(emptyCnt)
    emptyIdx(lastvertex) = emptyIdx(vertex)
    emptySquare(emptyIdx(vertex)) = lastvertex
    assert(countStringLiberties(vertex) < boardSize * boardSize)

    /* check whether we still live (i.e. detect suicide) */
    if (countStringLiberties(vertex) == 0) removeStringFast(vertex)
    (-1, capture)
  }

  /** Check for 4 neighbors of the same color */
  def isEye(color: Short, vertex: Short): Boolean = {
    val ownSurrounded = (neighbors(vertex) & EYE_MASK(color)) > 0

    // If not, it can't be an eye.
    // This takes advantage of borders being colored both ways.
    if (!ownSurrounded) {
      return false
    }

    // 2 or more diagonals taken; 1 for side groups
    val colorcount = Array.fill[Int](4)(0)

    colorcount(square(vertex - 1 - boardSize - 2)) += 1
    colorcount(square(vertex + 1 - boardSize - 2)) += 1
    colorcount(square(vertex - 1 + boardSize + 2)) += 1
    colorcount(square(vertex + 1 + boardSize + 2)) += 1

    if (colorcount(INVALID) == 0) {
      if (colorcount(otherColor(color)) > 1) return false
    }
    else if (colorcount(otherColor(color)) > 0) return false
    true
  }

  def moveToText(move: Int): String = {
    val (row, column) = getCoord(move)
    var result = ""

    if (move >= 0 && move <= maxSq) {
      result += (if (column < 8) 'A' + column else 'A' + column + 1)
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

  def moveToTextSgf(move: Int): String = {
    var (row, column) = getCoord(move)

    // SGF inverts rows
    row = boardSize - row - 1
    var result = ""

    if (move >= 0 && move <= maxSq) {
      if (column <= 25) {
        result += ('a' + column)
      } else {
        result  += ('A' + column - 26)
      }
      if (row <= 25) {
        result += ('a' + row)
      } else {
        result += ('A' + row - 26)
      }
    }
    else if (move == PASS) { result += "tt" }
    else if (move == RESIGN) { result += "tt" }
    else { result += "error" }
    result
  }

  private def getCoord(move: Int): (Int, Int) = {
    var column = move % (boardSize + 2) - 1
    var row = move / (boardSize + 2) - 1
    assert(move == PASS || move == RESIGN || (row >= 0 && row < boardSize))
    assert(move == PASS || move == RESIGN || (column >= 0 && column < boardSize))
    (row , column)
  }

  def testToMove(move: String): Int = {
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
    getVertex(x, y)
  }

  def getPrisoners(side: Short): Int = {
    assert(side == WHITE || side == BLACK)
    prisoners(side)
  }

  def blackToMove(): Boolean = toMove == BLACK
  def getToMove: Byte = toMove
  def setToMove(tomove: Byte): Unit = { toMove = tomove }
  def getParentString(vertex: Short): Short = parentString(vertex)

  def getGroupId(vertex: Int): Int = {
    assert(square(vertex) == WHITE || square(vertex) == BLACK)
    assert(parentString(vertex) == parentString(parentString(vertex)))
    parentString(vertex)
  }

  def getStringStones(vertex: Int): Seq[Int] = {
    val start = parentString(vertex)
    var res: Seq[Int] = Seq() //Array.ofDim(stones(start))
    var newpos = start

    do {
      assert(square(newpos) == square(vertex))
      res :+= newpos.toInt
      newpos = next(newpos)
    } while (newpos != start)
    res
  }

  def getString(vertex: Int): String = {
    var result: String = ""
    val start = parentString(vertex)
    var newpos = start

    do {
      result += moveToText(newpos) + " "
      newpos = next(newpos)
    } while (newpos != start)

    result.substring(0, result.length - 1) // remove last space
  }

  def fastInAtari(vertex: Int): Boolean = {
    assert((square(vertex) < EMPTY) || (countStringLiberties(vertex) > maxSq))
    val theParent = parentString(vertex)
    stringLiberties(theParent) == 1
  }

  /**
    * @param vertex the vertex to check if in atari
    * @return 0 if not in atari, position of single liberty if it is
    */
  def inAtari(vertex: Short): Int = {
    assert(square(vertex) < EMPTY)

    if (countStringLiberties(vertex) > 1) {
      return 0
    }

    assert(countStringLiberties(vertex) == 1)
    var pos = vertex

    do {
      if (countPointLiberties(pos) > 0) {
        for (k <- 0 until 4) {
          val ai = pos + directions(k)
          if (square(ai) == EMPTY) {
            return ai
          }
        }
      }

      pos = next(pos)
    } while (pos != vertex)
    assert(false)  // should be unreachable
    0
  }

  def getDir(vertex: Int): Int = directions(vertex)
  def getExtraDir(vertex: Int): Int = extraDirections(vertex)

  def getStoneList: String = {
    var res: String = ""

    for (i <- 0 until boardSize) {
      for (j <- 0 until boardSize) {
        val vertex: Int = getVertex(i, j)

        if (getSquare(vertex) != EMPTY) {
          res += moveToText(vertex) + " "
        }
      }
    }
    res.substring(0, res.length - 1) // remove final space
  }

  def stringSize(vertex: Int): Int = {
    assert(vertex > 0 && vertex < maxSq)
    assert(square(vertex) == WHITE || square(vertex) == BLACK)
    stonesInString(parentString(vertex))
  }

  def mergedStringSize(color: Short, vertex: Int): Int = {
    var totalSize = 0
    val nbrParent = Array.ofDim[Int](4)
    var nbrCount = 0

    for (k <- 0 until 4) {
      val ai = vertex + directions(k)

      if (getSquare(ai) == color) {
        val theParent = parentString(ai)

        var found = false
        var i = 0
        while (i < nbrCount && !found) {
          if (nbrParent(i) == theParent) {
            found = true
          }
          i += 1
        }

        if (!found) {
          totalSize += stringSize(ai)
          nbrParent(nbrCount) = theParent
          nbrCount += 1
        }
      }
    }
    totalSize
  }
}