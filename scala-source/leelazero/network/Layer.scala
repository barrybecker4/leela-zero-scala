package leelazero.network

import org.jocl.cl_mem


class Layer {
  var channels: Int = 0
  var outputs: Int = 0
  var filterSize: Int = 0
  var isBatchNorm = false
  var isInnerProduct = false
  var isResidualBlock = false
  var weights: Seq[cl_mem] = Seq()
}
