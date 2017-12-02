package leelazero


class Randomizer(theSeed: Long) {
  val seed: Long = if (theSeed == -1) System.currentTimeMillis() ^ Thread.currentThread().getId else theSeed
  val s: Array[Long] = Array.ofDim[Long](2)
  seedRandom(seed)

  /** This is xoroshiro128+. Note that the last bit isn't entirely random, so don't use it, if possible. */
  def nextLong(): Long = {
    val s0 = s(0)
    var s1 = s(1)
    val result = s0 + s1
    s1 ^= s0
    s(0) = Utils.rotateLeft(s0, 55) ^ s1 ^ (s1 << 14)
    s(1) = Utils.rotateLeft(s1, 36)
    result
  }

  def nextShort(max: Short): Short = (((nextLong() >> 48) * max) >> 16).toShort
  def nextInt(max: Int): Int = (((nextLong() >> 32) * max.toLong) >> 32).toInt
  def nextInt(): Int = (nextLong() >> 32).toInt

  /**
    * Magic values from Pierre L'Ecuyer.
    * See "Tables of Linear Congruental Generators of different sizes and good lattice structure"
    */
  def seedRandom(sd: Long): Unit = {
    s(0) = 741103597 * sd
    s(1) = 741103597 * s(0)
  }
  /** Need a 23 bit mantissa + implicit 1 bit = 24 bit number starting from a 64 bit random */
  def randFloat(): Float = {
    val umax: Float = 1.0f / (1 << 24)
    val rnd = nextLong() >> 40
    rnd.toFloat * umax
  }
}