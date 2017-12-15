package leelazero.network

import leelazero.Config
import leelazero.network.Network.{BOARD_SIZE, _}
import org.jocl.CL._
import org.jocl.{Pointer, Sizeof, _}


class OpenCLNetwork {

  private var layers: Seq[Layer] = Seq()

  def pushBatchNorm(spatialSize: Int, means: Seq[Float], variances: Seq[Float]): Unit = {
    val layer = getLayerCount
    pushWeights(layer, means)
    pushWeights(layer, variances)
    layers(layer).isBatchNorm = true
    layers(layer).channels = means.length
    layers(layer).outputs = means.length
    layers(layer).filterSize = spatialSize
  }

  def pushConvolve(filterSize: Int, weights: Seq[Float], biases: Seq[Float]): Unit = {
    val layer = getLayerCount
    pushWeights(layer, weights)
    pushWeights(layer, biases)
    layers(layer).outputs = biases.length
    layers(layer).filterSize = filterSize
    layers(layer).channels = weights.length / (biases.length * filterSize * filterSize)
  }

  def pushResidual(filterSize: Int,
                   weights1: Seq[Float], biases1: Seq[Float], means1: Seq[Float], variances1: Seq[Float],
                   weights2: Seq[Float], biases2: Seq[Float], means2: Seq[Float], variances2: Seq[Float]): Unit = {
    val layer = getLayerCount
    pushWeights(layer, weights1)
    pushWeights(layer, biases1)
    pushWeights(layer, means1)
    pushWeights(layer, variances1)
    pushWeights(layer, weights2)
    pushWeights(layer, biases2)
    pushWeights(layer, means2)
    pushWeights(layer, variances2)
    layers(layer).isResidualBlock = true
    layers(layer).outputs = biases1.length
    layers(layer).filterSize = filterSize
    layers(layer).channels = weights1.length / (biases1.length * filterSize * filterSize)
  }

  def getLayerCount: Int= layers.length

  private def pushWeights(layer: Int, weights: Seq[Float]): Unit = {
    addWeights(layer, weights)
  }

  private def addWeights(layer: Int, weights: Seq[Float]): Unit = {
    if (layer >= layers.length) {
      layers :+= new Layer()
    }

    val convertedWeights: Array[Float] = weights.map(identity).toArray
    val weightSize = weights.length * FLOAT_SIZE /*sizeof(decltype(convertedWeights)::value_type)*/
    val context = OPEN_CL.context
    val bufferWeights: cl_mem =
      clCreateBuffer(context, CL_MEM_COPY_HOST_PTR | CL_MEM_READ_ONLY, weightSize, Pointer.to(convertedWeights), null)
      ////new Buffer(CL_MEM_COPY_HOST_PTR | CL_MEM_READ_ONLY, weightSize, convertedWeights)

    layers.last.weights :+= bufferWeights
  }

  def forward(input: Array[NetWeight], output: Array[NetWeight]): Unit = {
    val onePlane = BOARD_SQ * FLOAT_SIZE /*sizeof(NetWeight)*/

    OPEN_CL.ensureThreadInitialized()
    val threadData = OPEN_CL_THREAD_DATA.get

    if (!threadData.buffersAllocated) {
      var maxInBufferSize = 0
      var maxMergeSize = 0
      for (layer <- layers) {
        val channelGroups = layer.channels / (if (layer.channels % 8 != 0)  2 else 8)
        maxMergeSize = Math.max(maxMergeSize, layer.outputs * channelGroups)
        maxInBufferSize = Math.max(maxInBufferSize, layer.channels)
      }
      val allocInSize = onePlane * maxInBufferSize
      val allocMergeSize = onePlane * maxMergeSize

      val context = OPEN_CL.context
      threadData.inBuffer =
        clCreateBuffer(context, CL_MEM_READ_WRITE, allocInSize, Pointer.to(input), null)
      threadData.tmpBuffer =
        clCreateBuffer(context, CL_MEM_READ_WRITE, allocInSize, Pointer.to(input), null)
      threadData.residualBuffer =
        clCreateBuffer(context, CL_MEM_READ_WRITE, allocInSize, Pointer.to(input), null)
      threadData.mergeBuffer =
        clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_HOST_NO_ACCESS, allocMergeSize, Pointer.to(input), null)
      threadData.buffersAllocated = true
    }

    val inBuffer: cl_mem = threadData.inBuffer
    val tmpBuffer: cl_mem = threadData.tmpBuffer
    val mergeBuffer: cl_mem = threadData.mergeBuffer
    val residualBuffer: cl_mem = threadData.residualBuffer
    val queue: cl_command_queue = threadData.commandQueue

    val inSize = FLOAT_SIZE * input.length /*sizeof(float)*/
    clEnqueueWriteBuffer(queue, inBuffer, CL_FALSE, 0, inSize, Pointer.to(input), 0, null, null) // ?
    var bufs = (inBuffer, tmpBuffer)
    def swap(b: (cl_mem, cl_mem)): (cl_mem, cl_mem) = (b._2, b._1)

    for (layer <- layers) {
      if (layer.isBatchNorm) {
        batchNorm(layer.outputs, layer.filterSize, bufs._1, bufs._2, null, layer.weights)
        bufs = swap(bufs)
      } else if (layer.isResidualBlock) {
        assert(layer.channels == layer.outputs)
        val conv1Weights = layer.weights.take(2)
          //std::vector<cl::Buffer>(begin(layer.weights), begin(layer.weights) + 2)
        val bn1Weights  = layer.weights.slice(2, 4)
          //std::vector<cl::Buffer>(begin(layer.weights) + 2, begin(layer.weights) + 4)
        val conv2Weights = layer.weights.slice(4, 6)
          //std::vector<cl::Buffer>(begin(layer.weights) + 4, begin(layer.weights) + 6);
        val bn2Weights = layer.weights.slice(6, 8)
          // std::vector<cl::Buffer>(begin(layer.weights) + 6, begin(layer.weights) + 8);
        val inBufferSize = layer.channels * onePlane
        clEnqueueCopyBuffer(queue, bufs._1, residualBuffer, 0, 0, inBufferSize, 0, null, null) // ?
        convolve(layer.filterSize, layer.channels, layer.outputs,
          bufs._1, bufs._2, mergeBuffer, conv1Weights)
        bufs = swap(bufs)
        batchNorm(layer.outputs, BOARD_SQ, bufs._1, bufs._2, null, bn1Weights)  // BOARD_SQ = 361
        bufs = swap(bufs)
        convolve(layer.filterSize, layer.channels, layer.outputs, bufs._1, bufs._2, mergeBuffer, conv2Weights)
        bufs = swap(bufs)
        batchNorm(layer.outputs, BOARD_SQ, bufs._1, bufs._2, residualBuffer, bn2Weights)
        bufs = swap(bufs)
      } else  {
        // plain convolution
        convolve(layer.filterSize, layer.channels, layer.outputs, bufs._1, bufs._2, mergeBuffer, layer.weights)
        bufs = swap(bufs)
      }
    }

    val finalSize = layers.last.outputs * onePlane
    clEnqueueReadBuffer(queue, bufs._1, CL_FALSE, 0, finalSize, Pointer.to(output), 0, null, null) // ?

    clFinish(queue)
  }

  def convolve(filterSize: Int, channels: Int, outputs: Int,
               bufferInput: cl_mem, bufferOutput: cl_mem, bufferMerge: cl_mem, weights: Seq[cl_mem]): Unit = {
    // fixed for 19x19
    val boardSize = BOARD_SQ

    var convolveKernel: cl_kernel = null
    val threadData = OPEN_CL_THREAD_DATA.get
    if (filterSize == 3) {
      convolveKernel = threadData.convolve3Kernel
    } else {
      assert(filterSize == 1)
      convolveKernel = threadData.convolve1Kernel
    }

    // Input channel grouping in multiples of 8
    var channelGroup = 8
    var channelShift = 3

    // Input layer is not a multiple of 8
    if (channels % 8 != 0) {
      assert(channels % 2 == 0)
      channelGroup = 2
      channelShift = 1
    }

    val rowGroup: Int = 1
    val outputGroup: Int = Math.min(outputs, 32)

    if (Config.DEBUG) {
      // Total output size after reducing
      val outSize = BOARD_SQ * outputs * FLOAT_SIZE /* sizeof(net_t) */

      // Produce channel * output planes and merge them at the end
      val mergeSize = (channels >> channelShift) * outSize
      assert(mergeSize <= clGetMemObjectInfo(bufferMerge, CL_MEM_SIZE, 4, null, null))
    }

    // Copy the rows locally
    var stripSize: Int = 0
    var rowTileSize: Int = 0
    var rowTiles: Int = 0
    if (filterSize == 3) {
      stripSize = filterSize * (BOARD_SIZE + (filterSize - 1)) * FLOAT_SIZE /*sizeof(float)*/
      rowTiles  = Config.cfg_rowtiles
      rowTileSize =  (19 + rowTiles - 1) / rowTiles
    } else {
      assert(filterSize == 1)
      stripSize = BOARD_SIZE * FLOAT_SIZE  /* sizeof(float);*/
      rowTiles = BOARD_SIZE
      rowTileSize =  1
      assert(channelGroup == 8); // hardcoded in kernel
    }

    val rowBuffer: Int = Math.min(channelGroup, 7)
    val rowSize = channelGroup * outputGroup * rowBuffer * FLOAT_SIZE /*sizeof(float)*/

    val  queue: cl_command_queue = threadData.commandQueue

    { // try
      clSetKernelArg(convolveKernel, 0, Sizeof.cl_mem, Pointer.to(bufferInput))
      clSetKernelArg(convolveKernel, 1, Sizeof.cl_mem, Pointer.to(bufferMerge))
      clSetKernelArg(convolveKernel, 2, Sizeof.cl_mem, Pointer.to(weights.head))
      clSetKernelArg(convolveKernel, 3, Sizeof.cl_int,
        Pointer.to(Array.ofDim[Int](stripSize * channelGroup * rowGroup)))
          // cl_local(stripSize * channelGroup * rowGroup)?
      clSetKernelArg(convolveKernel, 4, Sizeof.cl_int, Pointer.to(Array.ofDim[Int](rowSize)))  // cl::Local(rowSize)?
      if (filterSize == 3) {
        clSetKernelArg(convolveKernel, 5, Sizeof.cl_int, Pointer.to(Array.ofDim[Int](rowTileSize))) // ? need pointer?
        clSetKernelArg(convolveKernel, 6, Sizeof.cl_int, Pointer.to(Array.ofDim[Int](rowBuffer)))
        clSetKernelArg(convolveKernel, 7, Sizeof.cl_int, Pointer.to(Array.ofDim[Int](channelGroup)))
        clSetKernelArg(convolveKernel, 8, Sizeof.cl_int, Pointer.to(Array.ofDim[Int](channelShift)))
      }

      val globalWorkSize = Array[Long](channelGroup, outputGroup, rowGroup)
                          // cl::NDRange(channels, outputs, rowTiles)
      val localWorkSize = Array[Long](channelGroup, outputGroup, rowGroup)
                          // cl::NDRange(channelGroup, outputGroup, rowGroup)
      clEnqueueNDRangeKernel(queue, convolveKernel, 1, null/*cl::NullRange*/,
                             globalWorkSize, localWorkSize, 0, null, null);
    } // throw error

    val mergeKernel: cl_kernel = threadData.mergeKernel
    assert(channels % (1 << channelShift) == 0)

    { // try
      clSetKernelArg(mergeKernel, 0, Sizeof.cl_mem, Pointer.to(bufferMerge))
      clSetKernelArg(mergeKernel, 1, Sizeof.cl_mem, Pointer.to(bufferOutput))
      clSetKernelArg(mergeKernel, 2, Sizeof.cl_mem, Pointer.to(weights(1)))
      clSetKernelArg(mergeKernel, 3, Sizeof.cl_int, Pointer.to(Array.ofDim[Int](channels >> channelShift)))

      val globalWorkSize = Array[Long](outputs, boardSize)
      // cl::NDRange(outputs, boardsize)
      val localWorkSize = Array[Long](Math.min(8, outputs), BOARD_SIZE)
      // cl::NDRange(std::min(8, outputs), 19)
      clEnqueueNDRangeKernel(queue, mergeKernel, 1, null/*cl::NullRange*/,
        globalWorkSize, localWorkSize, 0, null, null)
    } // throw error
  }

  def batchNorm(outputs: Int, channelSize: Int,
                bufferInput: cl_mem, bufferOutput: cl_mem, bufferResidual: cl_mem,
                weights: Seq[cl_mem]): Unit = {
    val threadData = OPEN_CL_THREAD_DATA.get
    val queue: cl_command_queue = threadData.commandQueue

    val batchNormKernel = threadData.batchNormKernel

    var channelGroup = 1
    if (channelSize == 361) {
      channelGroup = BOARD_SIZE
    }

    { // try
      clSetKernelArg(batchNormKernel, 0, Sizeof.cl_mem, Pointer.to(bufferInput))
      clSetKernelArg(batchNormKernel, 1, Sizeof.cl_mem, Pointer.to(bufferOutput))
      if (bufferResidual != null) { // ?
        clSetKernelArg(batchNormKernel, 2, Sizeof.cl_mem, Pointer.to(bufferResidual)) // *bufferResidual
      } else {
        clSetKernelArg(batchNormKernel, 2, Sizeof.cl_mem, null)
      }
      clSetKernelArg(batchNormKernel, 3, Sizeof.cl_mem, Pointer.to(weights(0)))
      clSetKernelArg(batchNormKernel, 4, Sizeof.cl_mem, Pointer.to(weights(1)))

      val globalWorkSize = Array[Long](outputs, channelSize)
      val localWorkSize = Array[Long](Math.min(8, outputs), channelGroup)
      clEnqueueNDRangeKernel(queue, batchNormKernel, 1, null,
        globalWorkSize, localWorkSize, 0, null, null)
    } // throw error
  }
}