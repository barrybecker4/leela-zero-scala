package leelazero

import FastBoard._
import Zobrist._
import scala.util.Random


object Zobrist {
  val RND = new Random()
  RND.setSeed(0)
}

/** Zobrist Hash. See https://en.wikipedia.org/wiki/Zobrist_hashing */
class Zobrist(size: Short) {
  private val maxSq: Short = calcMaxSquare(size)

  val zobrist: Array[Array[Long]] = Array.ofDim(4, maxSq)
  val zobristPrisine: Array[Array[Long]] = Array.ofDim(2, 2 * maxSq)
  val zobristPass: Array[Long] = Array.ofDim(5)
  init()

  private def init(rand: Random = RND): Unit = {
    def nextR() = rand.nextInt().toLong

    for (i <- 0 until 4) {
      for (j <- 0 until maxSq) {
        zobrist(i)(j)  = nextR << 32 //((uint64)rng.randuint32()) << 32
        zobrist(i)(j) ^= nextR       //(uint64)rng.randuint32()
      }
    }

    for (i <- 0 until 2) {
      for (j <- 0 until 2 * maxSq) {
        zobristPrisine(i)(j)  = nextR << 32
        zobristPrisine(i)(j) ^= nextR
      }
    }

    for (i <- 0 until 5) {
      zobristPass(i) = nextR << 32
      zobristPass(i) ^= nextR
    }
  }
}