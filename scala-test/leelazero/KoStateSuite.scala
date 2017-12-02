package leelazero

import leelazero.FastBoard.{BLACK, WHITE}
import org.scalatest.FunSuite


class KoStateSuite extends FunSuite {


  test("default construction") {
    val state = new KoState(5, 0.5f)

    assertResult(5) { state.size }
    assertResult(0.5) { state.komi }
    assertResult(false) { state.superKo() }
    assertResult(false) { state.superKo(234234L) }
  }

  test("detect pinwheel superKo when present") {
    val state = createSuperKoSetup3x3()

    state.playMove(WHITE, 2, 2) // now the captures start
    state.playMove(BLACK, 0, 1)
    state.playMove(WHITE, 2, 0)
    state.playMove(BLACK, 1, 2)
    state.playMove(WHITE, 0, 0)
    state.playMove(BLACK, 2, 1)
    state.playMove(WHITE, 0, 2)
    state.playMove(BLACK, 1, 0)

    assertResult(0.5) { state.komi }
    assertResult(true) { state.superKo() }
    assertResult(false) { state.superKo(234234L) }
    assertResult(true) { state.superKo(state.getKoHash) }
  }

  test("detect superKo when close") {
    val state = createSuperKoSetup3x3()

    state.playMove(WHITE, 2, 2) // now the captures start
    state.playMove(BLACK, 0, 1)

    assertResult(0.5) { state.komi }
    assertResult(false) { state.superKo() }
    assertResult(false) { state.superKo(234234L) }
    assertResult(false) { state.superKo(state.getKoHash) }
  }

  test("detect superKo when closer") {
    val state = createSuperKoSetup3x3()

    state.playMove(WHITE, 2, 2) // now the captures start
    state.playMove(BLACK, 0, 1)
    state.playMove(WHITE, 2, 0)
    state.playMove(BLACK, 1, 2)
    state.playMove(WHITE, 0, 0)
    state.playMove(BLACK, 2, 1)

    assertResult(0.5) { state.komi }
    assertResult(false) { state.superKo() }
    assertResult(false) { state.superKo(234234L) }
    assertResult(false) { state.superKo(state.getKoHash) }
  }

  private def createSuperKoSetup3x3(): KoState = {
    val state = new KoState(3, 0.5f)
    state.playMove(WHITE, 0, 0)
    state.playMove(BLACK, 2, 1)
    state.playMove(WHITE, 1, 1)
    state.playMove(BLACK, 1, 2)
    state.playMove(WHITE, 0, 2)
    state.playMove(BLACK, 1, 0)
    state
  }
}
