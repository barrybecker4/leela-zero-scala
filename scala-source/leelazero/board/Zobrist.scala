package leelazero.board

import leelazero.board.FastBoard._
import leelazero.board.Zobrist._
import leelazero.util.Randomizer
//import scala.util.Random can we use this instead?


object Zobrist {
  val RND = new Randomizer(5489)

  /** size of zobrist pass array */
  val NUM_PASSES = 5
  val NUM_REGULAR = 4
}

/**
  * Zobrist Hash.
  * See https://en.wikipedia.org/wiki/Zobrist_hashing
  * See https://stackoverflow.com/questions/21212993/unsigned-variables-in-scala
  */
class Zobrist(size: Short, rand: Randomizer = RND) {
  private val maxSq: Short = calcMaxSquare(size)

  val zobrist: Array[Array[Long]] = Array.ofDim(NUM_REGULAR, maxSq)
  val zobristPristine: Array[Array[Long]] = Array.ofDim(2, 2 * maxSq)
  val zobristPass: Array[Long] = Array.ofDim(NUM_PASSES)
  init()

  private def init(): Unit = {
    def nextR() = rand.nextInt().toLong

    for (i <- 0 until NUM_REGULAR) {
      for (j <- 0 until maxSq) {
        zobrist(i)(j)  = nextR << 32 //((uint64)rng.randuint32()) << 32
        zobrist(i)(j) ^= nextR       //(uint64)rng.randuint32()
      }
    }

    for (i <- 0 until 2) {
      for (j <- 0 until 2 * maxSq) {
        zobristPristine(i)(j)  = nextR << 32
        zobristPristine(i)(j) ^= nextR
      }
    }

    for (i <- 0 until NUM_PASSES) {
      zobristPass(i) = nextR << 32
      zobristPass(i) ^= nextR
    }
  }
}