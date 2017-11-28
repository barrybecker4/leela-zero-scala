package leelazero

import org.scalatest.FunSuite


class ZobristSuite extends FunSuite {

  test("typical zobrist instance") {
    val z = new Zobrist(5)

    assertResult(3896605098068159738L) {z.zobrist(1)(1) }
    assertResult(-4651716432629916536L) {z.zobristPristine(1)(1) }
    assertResult(
      "7430493277149599813, 7815809079660525551, 4085467786997160691, -1457205381421357414, -5404242953009338273"
    ) { z.zobristPass.mkString(", ") }

  }
}
