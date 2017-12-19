package leelazero.board

import leelazero.board.ZobristSuite._
import leelazero.util.Randomizer
import org.scalatest.FunSuite


object ZobristSuite {
  val RND = new Randomizer(5489)
  val SEED = 1L // don't use 0
}


class ZobristSuite extends FunSuite {

  test("typical zobrist instance") {
    val z = new Zobrist(5, 1) // used to pass in RND

    assertResult(-3106017016242315815L) {z.zobrist(1)(1) }
    assertResult(4955861658172173498L) {z.zobristPristine(1)(1) }
    assertResult(
      "-5164626664978726138, 2630326410385389633, -3628232988553148864, 428099000082003612, -2140497579567675424"
    ) { z.zobristPass.mkString(", ") }
  }

}
