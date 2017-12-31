package leelazero.network

import leelazero.Config
import leelazero.sgf.{SgfParser, SgfTree}
import org.scalatest.FunSuite


class NetworkSuite extends FunSuite {

  Config.cfg_weightsfile = Some("scala-test/leelazero/sgf/weights/leelaz-model-256000.txt")
  // check only one initialized singleton instance
  val instance: Network = Network.getInstance()

  test("get instance") {
    assertResult(instance) {Network.getInstance()}
  }

  test("get benchmark") {
    val tree: SgfTree = SgfParser.loadFromFile("../sgf/games/simple_match.sgf")
    val gameHistory = tree.gameHistory
    assertResult("--") {Network.getInstance().getBenchmarkResult(gameHistory)}
  }
}
