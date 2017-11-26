package leelazero

import java.io.OutputStream


object Utils {

  val cfgQuiet = false // Move to GTP once that class is ported
  val cfgLogFileHandle: Option[OutputStream] = None // Move to GTP once that class is ported

  def myPrint(string: String): Unit = {
    if (!cfgQuiet) {
      System.err.println(string)
      if (cfgLogFileHandle.nonEmpty) {
        //std::lock_guard<std::mutex> lock(IOmutex)  ??
        println(cfgLogFileHandle.get, string)
      }
    }
  }

  def gtpPrint(id: Int, string: String): Unit = {
    val idText = if (id != -1) f"=$id%d " else "= "
    print(idText)
    println(string + "\n")

    if (cfgLogFileHandle.nonEmpty) {
      val handle = cfgLogFileHandle.get
      //std::lock_guard<std::mutex> lock(IOmutex)
      print(handle, idText)
      print(handle, string + "\n")
    }
  }

  def gtpFailPrint(id: Int, string: String): Unit = {
    val idText = if (id != -1 ) f"?$id%d " else "? "
    print(idText)
    println(string + "\n")

    if (cfgLogFileHandle.nonEmpty) {
      val handle = cfgLogFileHandle.get
      //std::lock_guard<std::mutex> lock(IOmutex)
      print(handle, idText)
      print(handle, string + "\n")
    }
  }

  def logInput(input: String): Unit = {
    if (cfgLogFileHandle.nonEmpty) {
      //std::lock_guard<std::mutex> lock(IOmutex) ??
      println(cfgLogFileHandle.get, s">>$input")
    }
  }
}