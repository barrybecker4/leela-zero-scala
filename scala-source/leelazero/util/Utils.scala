package leelazero.util

import java.io.OutputStream


object Utils {

  val cfgQuiet = false // Move to GTP once that class is ported
  var cfgLogFileHandle: Option[OutputStream] = None // Move to GTP once that class is ported

  def myPrint(string: String): Unit = {
    if (!cfgQuiet) {
      System.out.println(string)
      if (cfgLogFileHandle.nonEmpty) {
        this.synchronized {
          //std::lock_guard<std::mutex> lock(IOmutex)  ??
          cfgLogFileHandle.get.write(string.getBytes())
        }
      }
    }
  }

  def gtpPrint(id: Int, string: String): Unit = {
    val idText = if (id != -1) f"=$id%d " else "= "
    print(idText)
    println(string + "\n")

    if (cfgLogFileHandle.nonEmpty) {
      this.synchronized {
        val handle = cfgLogFileHandle.get
        //std::lock_guard<std::mutex> lock(IOmutex)
        handle.write(idText.getBytes)
        handle.write((string + "\n").getBytes)
      }
    }
  }

  def gtpFailPrint(id: Int, string: String): Unit = {
    val idText = if (id != -1 ) f"?$id%d " else "? "
    print(idText)
    println(string + "\n")

    if (cfgLogFileHandle.nonEmpty) {
      this.synchronized {
        val handle = cfgLogFileHandle.get
        //std::lock_guard<std::mutex> lock(IOmutex)
        handle.write(idText.getBytes)
        handle.write((string + "\n").getBytes)
      }
    }
  }

  def logInput(input: String): Unit = {
    if (cfgLogFileHandle.nonEmpty) {
      this.synchronized {
        //std::lock_guard<std::mutex> lock(IOmutex) ??
        cfgLogFileHandle.get.write(s">>$input".getBytes())
      }
    }
  }


  def rotateLeft(a: Long, b: Int): Long = (a << b) | (a >> (64 - b))
  def rotateLeft(x: Int, k: Int): Int = (x << k) | (x >> (32 - k))
  def is7bit(c: Int): Boolean = c >= 0 && c <= 127
}