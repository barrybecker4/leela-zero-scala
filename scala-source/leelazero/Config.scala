package leelazero

// Progression in copmputer go strength https://imgur.com/a/gJSaM
object Config {
  val DEBUG = true
  val PROGRAM_NAME = "Leela-Zero-Scala"
  val PROGRAM_VERSION = "0.6"  // from the corresponding Leela-zero version
  val MAX_CPUS: Int = 64  // consider raising to 128

  // These were ifdefs in c++
  val USE_BLAS = true
  val USE_OPENBLAS = true
  var USE_MKL = false
  val USE_OPENCL = true
  val USE_HALF = false

  // these will probably be read from file or come from GTP
  val cfg_max_playouts: Int = 1000    // should come from config
  val cfg_weightsfile: Option[String] = None // FIXME
  val cfg_num_threads: Int = Runtime.getRuntime.availableProcessors()  // Should be configured
  val cfg_preffered_gpus: Seq[Int] = Seq()

  val cfg_softmax_temp = 1.0f  // from GTP
  val cfg_rowtiles = 5  // from GTP
}
