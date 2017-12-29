package leelazero.sgf

import java.io._
import java.net.URL
import SgfParser._

object SgfParser {

  /** @return game tree loaded from specified string */
  def loadFromString(gameBuff: String): SgfTree = {
    val parser: SgfParser = new SgfParser()

    val stream = new ByteArrayInputStream(gameBuff.getBytes)
    val sgfTree = parser.parse(new PushbackInputStream(stream)) // loads properties with moves
    sgfTree.initState() // Set up the root state to defaults

    // populates the states from the moves; split this up in root node, anchor (handicap), other nodes
    sgfTree.populateStates()
    sgfTree
  }

  /** load an SGF tree from the specified file */
  def loadFromFile(fileName: String, index: Short = Short.MaxValue): SgfTree = {
    val parser: SgfParser = new SgfParser()
    val gameBuff: String = parser.chopFromFile(fileName, index)
    loadFromString(gameBuff)
  }

  def is7bit(c: Int): Boolean = c >= 0 && c <= 127
}

/** Parse an SFG file */
class SgfParser {

  /** @return the parse SGF file as a string */
  def chopStream(ins: InputStream, stopAt: Short): Seq[String] = {
    var result = Seq[String]()
    var gameBuff = ""

    var nesting: Int = 0      // parentheses nesting
    var intag = false         // brackets
    var line = 0

    var nextChar: Int = ins.read()
    while (nextChar != -1 && result.length <= stopAt) {
      var c = nextChar.toChar
      if (c == '\n') line += 1

      gameBuff += c
      if (c == '\\') {
        // read literal char
        c = ins.read().toChar
        gameBuff += c
        // Skip special char parsing
      } else if (c == '(' && !intag) {
        if (nesting == 0) {
          // eat ; too
          do {
            c = ins.read().toChar
          } while(c.isWhitespace && c != ';')
          gameBuff = ""
        }
        nesting += 1
      } else if (c == ')' && !intag) {
        nesting -= 1

        if (nesting == 0) {
          result :+= gameBuff
        }
      } else if (c == '[' && !intag) {
        intag = true
      } else if (c == ']') {
        if (!intag) {
          println(f"Tag error on line $line%d")
        }
        intag = false
      }
      nextChar = ins.read()
    }

    // No game found? Assume closing tag was missing (OGS)
    if (result.isEmpty)
      result :+= gameBuff
    result
  }

  private def chopAll(fileName: String, stopAt: Short = Short.MaxValue): Seq[String] = {
    val ins: InputStream = getInputStreamFromFile(fileName)
    val result = chopStream(ins, stopAt)
    ins.close()
    result
  }

  private def chopFromFile(fileName: String, index: Short): String = {
    val vec = chopAll(fileName, index)
    vec(Math.min(vec.length -1 , index))
  }

  // took StringReader, now PushbackReader
  private def parsePropertyName(strm: PushbackInputStream): String = {
    var done = false
    var result = ""

    do {
      val i = strm.read()
      if (i != -1) {
        val c = i.toChar
        if (!c.isUpper && !c.isLower) {
          strm.unread(i)
          done = true
        } else {
          result += c
        }
      } else {
        done = true
      }
    } while (!done)
    result
  }

  private def parsePropertyValue(strm: PushbackInputStream): (Boolean, String) = {
    var c: Char = ' '
    var result = ""
    var done = false
    do {
      val i = strm.read()
      if (i == -1 ) done = true
      else {
        c = i.toChar
        if (!c.isSpaceChar) {
          strm.unread(i)
          done = true
        }
      }
    } while (!done)

    c = strm.read().toChar

    if (c != '[') {
      strm.unread(c)
      return (false, "")
    }

    done = false
    do {
      val i = strm.read()
      if (i == -1) done = true
      else {
        c = i.toChar
        if (c == ']') {
          done = true
        } else if (c == '\\') {
          c = strm.read().toChar
        }
        if (!done) result += c
      }
    } while (!done)

    (true, result)
  }

  private def parse(strm: PushbackInputStream): SgfTree = {
    var splitPoint: Boolean = false
    var c: Char = ' '
    var node: SgfTree = new SgfTree()
    val rootNode = node
    var done = false

    do {
      val i = strm.read()
      if (i == -1) done = true
      else {
        c = i.toChar
        if (!c.isWhitespace) {
          if (c.isLetter && c.isUpper) {
            strm.unread(c)
            val propName = parsePropertyName(strm)
            var result: (Boolean, String) = (true, "")
            do {
              result = parsePropertyValue(strm)
              if (result._1) {
                //println("adding prop " + propName + " : " + result._2)
                node.addProperty(propName, result._2)
              }
            } while (result._1)
          } else if (c == '(') {
            var cc: Char = ' '
            do {
              cc = strm.read().toChar
            } while (cc.isSpaceChar)
            if (cc != ';') { // eat first ;
              strm.unread(cc)
            }
            // start a variation here
            splitPoint = true
            // new node
            val childTree = parse(strm)
            node.addChild(childTree)
          } else if (c == ')') {
            // variation ends, go back.
            // If the variation didn't start here, then push the "variation ends" mark back
            // and try again one level up the tree
            if (!splitPoint) {
              strm.unread(c)
              //println("tree read. returning. Numkids = " + node.getNumChildren)
              return rootNode
            } else {
              splitPoint = false
            }
          } else if (c == ';') {
            // new node
            val newNode = new SgfTree()
            //println("Encountered ; adding new child node")
            node.addChild(newNode)
            node = newNode
          }
        }
      }
    } while (!done)

    rootNode
  }

  /** @return the number of games in the specified file  (i.e. the different branches) */
  def countGamesInFile(fileName: String): Int = {
    val ins: InputStream = getInputStreamFromFile(fileName)
    var count = 0
    var nesting = 0
    var done = false
    var c: Char = ' '

    do {
      var i = ins.read()
      if (i == -1) done = true
      else {
        c = i.toChar
        if (!is7bit(i)){
          do {
            i = ins.read()
            c = i.toChar
          } while (!is7bit(i))
        } else if (c == '\\') {
          c = ins.read().toChar // read literal char
        } else {
          if (c == '(') {
            nesting += 1
          } else if (c == ')') {
            nesting -= 1
            assert (nesting >= 0)
            if (nesting == 0) {
              count += 1 // one game processed
            }
          }
        }
      }
    } while (!done)

    ins.close()
    count
  }

  private def getInputStreamFromFile(fileName: String): InputStream = {
    val url: URL = this.getClass.getResource(fileName)
    if (url == null) throw new IllegalArgumentException("Could not find file: " + fileName)
    val file: File = new File(url.getPath)
    new FileInputStream(file)
  }
}