package leelazero.network

import org.jocl.{cl_command_queue, cl_kernel, cl_mem}


class ThreadData {
  var isInitialized: Boolean = false
  var buffersAllocated: Boolean = false

  //var d: CUdevice = new CUdevice()
  var commandQueue: cl_command_queue = _
  var convolve1Kernel: cl_kernel = _
  var convolve3Kernel: cl_kernel = _
  var mergeKernel: cl_kernel = _
  var batchNormKernel: cl_kernel = _
  var inBuffer: cl_mem = _
  var tmpBuffer: cl_mem = _
  var mergeBuffer: cl_mem = _
  var outBuffer: cl_mem = _
  var residualBuffer: cl_mem = _
}
