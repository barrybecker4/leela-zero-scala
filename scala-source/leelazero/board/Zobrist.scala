package leelazero.board

import leelazero.board.FastBoard._
import leelazero.board.Zobrist._
import leelazero.util.Randomizer
//import scala.util.Random can we use this instead?


object Zobrist {
  //val RND = new Randomizer(5489)
  val SEED = 5489

  /** size of zobrist pass array */
  val NUM_PASSES = 5
  val NUM_REGULAR = 4
}

/**
  * Zobrist Hash.
  * See https://en.wikipedia.org/wiki/Zobrist_hashing
  * See https://stackoverflow.com/questions/21212993/unsigned-variables-in-scala
  * @param size size of the go board
  * @param seed any positive long. Do not use 0.
  */
class Zobrist(size: Short, seed: Long = SEED) {
  private val maxSq: Short = calcMaxSquare(size)
  private val rand = new Randomizer(seed)

  val zobrist: Array[Array[Long]] = Array.ofDim(NUM_REGULAR, maxSq)
  val zobristPristine: Array[Array[Long]] = Array.ofDim(2, 2 * maxSq)
  val zobristPass: Array[Long] = Array.ofDim(NUM_PASSES)
  init()
  /*
  def copy(): Zobrist = {
    val z = new Zobrist(size, seed)
    for (i <- 0 until NUM_REGULAR)
      for (j <- 0 until maxSq)
        z.zobrist(i)(j) = this.zobrist(i)(j)

    for (i <- 0 until 2) {
      for (j <- 0 until 2 * maxSq) {
        z.zobristPristine(i)(j) = this.zobristPristine(i)(j)
      }
    }
    for (i <- 0 until NUM_PASSES) {
      z.zobristPass(i) = this.zobristPass(i)
    }
    z
  }*/

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