package leelazero.network

import leelazero.Config
import org.scalatest.FunSuite


class NetworkSuite extends FunSuite {

  test("get instance") {
    Config.cfg_weightsfile = Some("scala-test/leelazero/sgf/weights/leelaz-model-256000.txt")
    // check only one initialized singleton instance
    val instance = Network.getInstance()
    assertResult(instance) {Network.getInstance()}
  }
}
