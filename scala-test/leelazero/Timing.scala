package leelazero

object Timing {
  /** @return time delta in hundredths of a second */
  def diff(start: Long, stop: Long): Int = (stop - start).toInt / 10
}
