package leelazero

import java.io.File

object Config {
  val PROGRAM_NAME = "Leela-Zero-Scala"
  val PROGRAM_VERSION = "0.6"  // from the corresponding Leela-zero version
  val MAX_CPUS: Int = 64  // consider raising to 128

  val cfg_weightsfile: Option[String] = None // FIXME
}
