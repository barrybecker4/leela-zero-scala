package leelazero



class FullBoardSuite extends FastBoardSuite {

  override def createBoard(size: Short): FullBoard = new FullBoard(size)



  test("removeString") {
    val b: FullBoard = createFilled5x5Board().asInstanceOf[FullBoard]
    assertResult(3) { b.removeString(b.getVertex(1, 1))}
  }

}
