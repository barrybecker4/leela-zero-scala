package leelazero.sgf

import java.text.SimpleDateFormat

import leelazero.Config.{PROGRAM_NAME, cfg_weightsfile}
import leelazero.board.FastBoard._
import leelazero.Config._
import leelazero.board.{FastBoard, FastBoardSerializer, GameHistory, KoState}
import leelazero.sgf.SgfTree._


object SgfTree {
  /** End-Of-Tree marker */
  val EOT: Short = 0

  /** For printing dates in ISO format */
  val ISO_DATE_FORMAT: SimpleDateFormat = new SimpleDateFormat("yyyy-mm-dd")
}

class SgfTree {

  private var initialized = false
  private var gameHistory: GameHistory = _
  private var winner: Byte = INVALID // no winner initially
  private var children: Seq[SgfTree] = Seq()
  private var properties = Map[String, String]()

  /** Initialize defaults. The SGF might be missing boardsize or komi which means we'll never initialize properly */
  def initState(): Unit = {
    gameHistory = new GameHistory()
    gameHistory.initHistory(MAX_BOARD_SIZE, 7.5f)
    initialized = true
  }

  def getState: KoState = {
    assert(initialized)
    gameHistory.getCurrentState
  }

  def getNumChildren: Int = children.length

  /** @return the child based on specified index, or null if none at that index */
  def getChild(count: Short): SgfTree = {
    if (count < children.length) {
      assert(initialized)
      children(count)
    } else null
  }

  /**
    * This follows the entire line, and doesn't really need the intermediate states, just the moves.
    * As a consequence, states that contain more than just moves won't have any effect.
    */
  def followMainlineState(movenum: Int): KoState = {
    var link: SgfTree = this
    // This initializes a starting state from a KoState and sets up the game history.
    val result: KoState = getState

    var i = 0
    while (i <= movenum && link != null) {
      // root position has no associated move
      if (i != 0) {
        val move: Short = link.getMove(result.getToMove)
        if (move != EOT) {
          if (move != PASS && move != EMPTY
            && result.getBoard.getSquare(move) != EMPTY) {
            // Fail loading
            return result
          }
          result.playMove(move)
        }
      }
      link = link.getChild(0)
      i += 1
    }
    result
  }

  /** @return the state at the specified move or final move if move number not specified */
  def getStateFromMainline(movenum: Int = 999): KoState = {
    var link = this
    var last = this
    var i = 0
    while (i <= movenum && link != null) {
      link = link.getChild(0)
      if (link == null) {
        return last.getState
      } else {
        last = link
      }
      i += 1
    }
    link.getState
  }

  /** @return the number of states is one more than the number of moves */
  def countMainlineMoves(): Int = {
    var link = this
    var count = -1
    while (link != null) {
      link = link.getChild(0)
      count += 1
    }
    count
  }


  /** called once on the root node to initialize global game properties */
  def populateStates(): Unit = {
    var validSize = false
    var hasHandicap = false

    // first check for go game setup in properties
    if (properties.contains("GM")) {
      if (properties("GM") != "1") {
        throw new IllegalStateException("SGF Game is not a Go game")
      } else {
        if (!properties.contains("SZ")) {
          // No size, but SGF spec defines default size for Go
          properties += "SZ" -> "19"
          validSize = true
        }
      }
    }

    if (properties.contains("SZ")) {         // board size
      val bsize = properties("SZ").toShort

      if (bsize <= MAX_BOARD_SIZE) {
        // Assume 6.5 komi if not specified
        gameHistory.initHistory(bsize, 6.5f)
        validSize = true
      } else {
        throw new IllegalStateException("Board size not supported.")
      }
    }

    if (properties.contains("KM")) {         // komi
      val komi = properties("KM").toFloat
      val handicap = gameHistory.getCurrentState.getHandicap
      // last ditch effort: if no GM or SZ, assume 19x19 Go here
      var bsize: Short = 19
      if (validSize) {
        bsize = gameHistory.getCurrentState.size
      }
      gameHistory.initHistory(bsize, komi)
      gameHistory.getCurrentState.setHandicap(handicap)
    }

    if (properties.contains("HA")) {        // handicap
      val handicap = properties("HA").toFloat
      hasHandicap = handicap > 0.0f
      gameHistory.getCurrentState.setHandicap(handicap.toShort)
    }

    if (properties.contains("RE")) {        // result
      val result = properties("RE")
      if (result.contains("Time")) {
        winner = EMPTY
      } else {
        if (result.startsWith("W+")) {
          winner = WHITE
        } else if (result.startsWith("B+")) {
          winner = BLACK
        } else {
          winner = INVALID
          println("Could not parse game result: " + result)
        }
      }
    } else {
      winner = EMPTY
    }

    // initial handicap stones
    val addBlacksValue: Option[String] = properties.get("AB")

    // Do we have a handicap specified but no handicap stones placed in
    // the same node? Then the SGF file is corrupt. Let's see if we can find
    // them in the next node, which is a common bug in some Go apps.
    var addBlacksList: Seq[String] = Seq()
    if (hasHandicap && addBlacksValue.nonEmpty) {
      var addBlacks = addBlacksValue.get
      if (children.nonEmpty) {
        val successor = children.head
        addBlacks = successor.properties("AB")
      }
      addBlacksList = addBlacks.substring(1, addBlacks.length - 1).split("][")
      println("adding blacks: " + addBlacksList.mkString(", "))
    }

    // Loop through the stone list and apply
    for (move <- addBlacksList) {
      val vtx: Short = stringToVertex(move)
      applyMove(BLACK, vtx)
    }

    // XXX: count handicap stones
    val addWhitesValue = properties.get("AW")
    if (addWhitesValue.nonEmpty) {
      var addWhites = addWhitesValue.get
      val addWhitesList = addWhites.substring(1, addWhites.length - 1).split("][")
      println("adding whites: " + addWhitesList.mkString(", "))
      for (move <- addWhitesList) {
        val vtx: Short = stringToVertex(move)
        applyMove(WHITE, vtx)
      }
    }

    if (properties.contains("PL")) {
      val startPlayer = properties("PL")
      if (startPlayer == "W") {
        gameHistory.getCurrentState.setToMove(WHITE)
      } else if (startPlayer == "B") {
        gameHistory.getCurrentState.setToMove(BLACK)
      }
    }

    // now for all children, play out the moves
    for (childState <- children) {
      // propagate state
      childState.copyState(this)

      // XXX: maybe move this to the recursive call. get move for side to move
      val move: Short = childState.getMove(gameHistory.getCurrentState.getToMove)
      if (move != EOT) {
        childState.applyMove(move)
      }
      childState.populateStates()
    }
  }

  private def stringToVertex(moveString: String): Short = {
    if (moveString.length == 0) {
      return PASS
    }
    val bsize = gameHistory.getCurrentState.size
    if (bsize <= 19 && moveString == "tt") {
      return PASS
    }
    if (bsize == 0) {
      throw new IllegalStateException("Node has 0 sized board")
    }

    val c1 = moveString(0)
    val c2 = moveString(1)
    val cc1 = if (c1 >= 'A' && c1 <= 'Z') 26 + c1 - 'A'
              else c1 - 'a'
    val cc2 = if (c2 >= 'A' && c2 <= 'Z') bsize - 26 - (c2 - 'A') - 1
              else bsize - (c2 - 'a') - 1

    // catch illegal SGF
    if (cc1 < 0 || cc1 >= bsize
      || cc2 < 0 || cc2 >= bsize) {
      throw new IllegalStateException("Illegal SGF move")
    }
    gameHistory.getCurrentState.getBoard.getVertex(cc1, cc2)
  }

  /** copy the state of the specified tree into this one */
  private def copyState(tree: SgfTree): Unit = {
    initialized = tree.initialized
    gameHistory = tree.gameHistory.copy()
  }

  private def applyMove(color: Byte, move: Short): Unit = {
    if (move != PASS && move != RESIGN) {
      val currSquare = gameHistory.getCurrentState.getBoard.getSquare(move)
      if (currSquare == otherColor(color) || currSquare ==INVALID) {
        throw new IllegalStateException("Illegal move: " + move)
      }
      // Playing on an occupied square is legal in SGF setup, but we can't really handle it.
      // So just ignore and hope that works.
      if (currSquare == color) {
        return
      }
      assert(currSquare == EMPTY)
    }
    gameHistory.playMove(color, move)
  }

  private def applyMove(move: Short): Unit = applyMove(gameHistory.getCurrentState.getToMove, move)
  def addProperty(property: String, value: String): Unit = properties += property -> value

  def addChild(child: SgfTree): Unit = children :+= child

  private def getMove(toMove: Byte): Short = {
    val moveString = if (toMove == BLACK) "B" else "W"
    if (properties.contains(moveString)) stringToVertex(properties(moveString)) else EOT
  }

  def getWinner: Byte = winner
  def isInitialized: Boolean =  initialized

  /** @return sequence of moves in the main line of play recorded in the sgf */
  def getMainline: Seq[Short] = {
    var moves = Seq[Short]()
    var link: SgfTree = this
    var toMove = FastBoard.otherColor(gameHistory.getCurrentState.getToMove)  // ? why is other color needed here?
    link = link.getChild(0)

    while (link != null && link.isInitialized) {
      val move = link.getMove(toMove)
      println("move for " + toMove + " is " + move)
      if (move != EOT) moves :+= move    // append the move
      toMove = otherColor(toMove)
      link = link.getChild(0)
    }
    moves
  }

  /** Serialize the entire game from its history */
  def stateToString(pstate: GameHistory, compColor: Byte): String = {
    val state = pstate.copy()
    //*state = pstate; // make a working copy ???

    var header = ""
    var moves = ""

    val komi = state.getCurrentState.komi
    val size = state.getCurrentState.size
    val now = ISO_DATE_FORMAT.format( java.time.LocalDate.now)
    println("now = " + now)

    header += "(;GM[1]FF[4]RU[Chinese]"
    header += "DT[" + now + "]"
    header += "SZ[" + size + "]"
    header += f"KM[$komi%.1f]"

    var leelaName = PROGRAM_NAME
    leelaName += " " + PROGRAM_VERSION
    if (cfg_weightsfile.isDefined) {
      leelaName += " " + cfg_weightsfile.get.substring(0, 8)
    }

    if (compColor == WHITE) {
      header += "PW[" + leelaName + "]PB[Human]"
    } else {
      header += "PB[" + leelaName + "]PW[Human]"
    }
    state.rewind()

    // check handicap here (anchor point)
    var handicap: Int = 0
    var handicapStr = ""
    val board = gameHistory.getCurrentState.getBoard
    val bs = new FastBoardSerializer(board)
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        val vertex = board.getVertex(i, j)
        val square = board.getSquare(vertex)
        if (square == BLACK) {
          handicap += 1
          handicapStr += "[" + bs.moveToTextSgf(vertex) + "]"
        }
      }
    }

    if (handicap > 0) {
      header += "HA[" + handicap + "]"
      moves += "AB" + handicapStr
    }

    moves += "\n"
    var counter: Int = 0

    while (gameHistory.forwardMove()) {
      val move = gameHistory.getCurrentState.getLastMove
      if (move != RESIGN) {
        var movestr = bs.moveToTextSgf(move)
        if (gameHistory.getCurrentState.getToMove == BLACK) {
          moves += ";W[" + movestr + "]"
        } else {
          moves += ";B[" + movestr + "]"
        }
        counter += 1
        if (counter % 10 == 0) moves += "\n"
      }
    }

    if (gameHistory.getCurrentState.getLastMove != RESIGN) {
      var score = gameHistory.getCurrentState.finalScore
      if (score > 0.0f) header += f"RE[B+$score%.1f]"
      else header += s"RE[W+${-score}}%.1f]"
    } else {
      // Last move was resign, so side to move won
      if (gameHistory.getCurrentState.getToMove == BLACK) header += "RE[B+Resign]"
      else header += "RE[W+Resign]"

    }
    header + "\n" + moves + ")\n"
  }
}