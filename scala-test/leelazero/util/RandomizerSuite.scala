package leelazero.util

import org.scalatest.FunSuite

class RandomizerSuite extends FunSuite {

  test("nextInt seed = 1") {
    val r = new Randomizer(1L)
    assertResult(127878631) { r.nextInt()}
    assertResult(1490101574) { r.nextInt()}
  }

  test("nextInt seed = 2") {
    val r = new Randomizer(2L)
    assertResult(255757263) { r.nextInt()}
    assertResult(-1314764147) { r.nextInt()}
  }

  test("nextInt N = 10") {
    val r = new Randomizer(1L)
    assertResult(0) { r.nextInt(10)}
    assertResult(3) { r.nextInt(10)}
    assertResult(-3) { r.nextInt(10)}
  }

  test("nextInt N = 5") {
    val r = new Randomizer(1L)
    val seq = Range(0, 10).map(i => r.nextInt(10))
    assertResult("0, 3, -3, 3, 2, 2, -2, 0, 0, 1") {seq.mkString(", ")}
  }

  test("nextLong") {
    val r = new Randomizer(1L)
    assertResult(549234542227442006L) { r.nextLong()}
    assertResult(6399937531338952493L) { r.nextLong()}
  }
}
