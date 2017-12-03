package leelazero


object Timing {

  /** @return current time in hundredsth of a second */
  def getTime: Long = System.currentTimeMillis() / 10

  /** @return time delta in hundredths of a second */
  def timeDiff(start: Long, stop: Long): Int = (stop - start).toInt / 10
}
