package leelazero.board

import FastBoard._


class FullBoardSuite extends FastBoardSuite {

  override def createBoard(size: Short): FullBoard = new FullBoard(size)

  test("removeString") {
    val b: FullBoard = createFilled5x5Board().asInstanceOf[FullBoard]
    assertResult(3) { b.removeString(b.getVertex(1, 1))}
  }

  test("updateBoard when corner ko") {
    val b = createFilled5x5Board().asInstanceOf[FullBoard]
    assertResult((b.getVertex(4, 4), true)) {b.updateBoard(BLACK, b.getVertex(3, 4))}
  }

  test("updateBoard when black group captured on edge") {
    val b = createFilled5x5Board().asInstanceOf[FullBoard]
    assertResult((-1, true)) {b.updateBoard(WHITE, b.getVertex(4, 2))}
  }

  test("updateBoard when white suicide") {
    val b = createFilled5x5Board().asInstanceOf[FullBoard]
    assertResult((-1, false)) {b.updateBoard(WHITE, b.getVertex(2, 0))}
  }
}
