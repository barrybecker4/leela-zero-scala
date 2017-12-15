package leelazero.board

import leelazero.board.ZobristSuite._
import leelazero.util.Randomizer
import org.scalatest.FunSuite


object ZobristSuite {
  val RND = new Randomizer(5489)
}


class ZobristSuite extends FunSuite {

  test("typical zobrist instance") {
    val z = new Zobrist(5, RND)

    assertResult(-992086816489951536L) {z.zobrist(1)(1) }
    assertResult(7203540468310131802L) {z.zobristPristine(1)(1) }
    assertResult(
      "-7788122022957466439, 8345156559435542935, 6813009757636519123, 5688798515523735197, 2029477349424068609"
    ) { z.zobristPass.mkString(", ") }

  }
}
