package leelazero.sgf

import org.scalatest.FunSuite
import leelazero.board.FastBoard._


class SgfParserSuite extends FunSuite {

  val parser = new SgfParser()

  test("chopStream 1") {
    assertResult(1) {parser.countGamesInFile("games/2002-01-10-7.sgf")}
  }

  test("count games some_matches") {
    assertResult(3) {parser.countGamesInFile("games/some_matches.sgf")}
  }

  test("Parse sgf file") {
    verify("2002-01-10-7.sgf", SgfResult(157, 1, BLACK))
  }

  test("Parse 'simple_match' sgf file") {
    verify("simple_match.sgf", SgfResult(50, 1, EMPTY))
  }

  test("Parse 'some_matches' sgf file") {
    verify("some_matches.sgf", SgfResult(320, 1, WHITE))
  }

  test("Parse 'variation_matches' sgf file") {
    verify("variation_matches.sgf", SgfResult(320, 1, WHITE))
  }


  case class SgfResult(mainLineMoves: Int, numChildren: Int, winner: Byte = EMPTY, isInitialized: Boolean = true)

  private def verify(fileName: String, expectation: SgfResult): Unit = {
    val tree: SgfTree = SgfParser.loadFromFile("games/" + fileName)

    assertResult(expectation.mainLineMoves) {tree.countMainlineMoves()}
    assertResult(expectation.isInitialized) {tree.isInitialized}
    assertResult(expectation.numChildren) {tree.getNumChildren }
    assertResult(expectation.winner) {tree.getWinner}

  }
}