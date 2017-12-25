package leelazero.sgf

import org.scalatest.FunSuite
import leelazero.board.FastBoard._


class SgfParserSuite extends FunSuite {

  val parser = new SgfParser()

  test("chopStream 1") {
    assertResult(1) {parser.countGamesInFile("games/2002-01-10-7.sgf")}
  }

  test("chopStream 2") {
    assertResult(1) {parser.countGamesInFile("games/2002-01-10-8.sgf")}
  }

  test("Parse sgf file") {
    val tree: SgfTree = SgfParser.loadFromFile("games/2002-01-10-7.sgf")
    assertResult(1) {tree.countMainlineMoves()}
    assertResult(true) {tree.isInitialized}
    assertResult(BLACK) {tree.getWinner}
  }

}
