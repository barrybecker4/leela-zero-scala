package leelazero



class FullBoardSuite extends FastBoardSuite {

  override def createBoard(size: Short): FullBoard = new FullBoard(size)

  test("calcHash") {
    val b = createBoard()
    assertResult(839790810239240290L) { b.calcHash() }
    assertResult(839790810239240290L) { b.calcHash() }
  }

  test("calcKoHash") {
    val b = createBoard()
    assertResult(-4220881996681454477L) { b.calcKoHash()}
    assertResult(-4220881996681454477L) { b.calcKoHash()}
  }

  test("removeString") {
    val b: FullBoard = createFilled5x5Board().asInstanceOf[FullBoard]
    assertResult(3) { b.removeString(b.getVertex(1, 1))}
  }

}
