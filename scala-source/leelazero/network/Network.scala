package leelazero.network

import leelazero.board.FastBoard._
import leelazero.util.Utils.myPrint
import leelazero._
import leelazero.board.{FastBoard, FastBoardSerializer, GameHistory, KoState}
import leelazero.util.Timing
import org.bytedeco.javacpp.mkl_rt._

import scala.collection.mutable
import scala.io.Source
import scala.util.Random
import Network._


/**
  * The neural network - evaluated using the Open Cuda Library (OCL) and OpenBLAS
  * All static methods - should they be?
  */
object Network {
  val ENSEMBLE_DIRECT: Ensemble = 1
  val ENSEMBLE_RANDOM_ROTATION: Ensemble = 2

  // singleton instance. Access with getInstance
  private var instance: Option[Network] = None

  // File format version
  private val FORMAT_VERSION = 1
  private val INPUT_CHANNELS = 18
  private var RND = new Random()
  RND.setSeed(1)

  val OPEN_CL: OpenCL = new OpenCL()
  val OPEN_CL_NET: OpenCLNetwork = new OpenCLNetwork()
  val OPEN_CL_THREAD_DATA: ThreadLocal[ThreadData] = new ThreadLocal[ThreadData]

  val IP_POLICY_W_LEN = 261364
  val IP_POLICY_B_LEN = 362
  val IP1_VAL_W_LEN = 92416
  val IP1_VAL_B_LEN = 256
  val IP2_VAL_W_LEN = 256
  val BOARD_SIZE: Short = FastBoard.MAX_BOARD_SIZE
  val BOARD_SQ: Int = BOARD_SIZE * BOARD_SIZE // 361
  //val ALL_SET = mutable.BitSet(1 to BOARD_SQ: _*)
  val FLOAT_SIZE = 4 // number of bytes in a float

  type NetWeight = Float
  type Ensemble = Int
  type ScoredNode = (Float, Short)
  type NetResult = (Seq[ScoredNode], Float)
  type BoardPlane = mutable.BitSet
  type NNPlanes = Seq[BoardPlane]

  /** @return singleton instance. Initialized lazily */
  def getInstance(): Network = {
    if (instance.isEmpty) {
      instance = Some(new Network())
    }
    instance.get
  }

  private def createNNPlanes(): NNPlanes = {
    for (i <- 0 to 18) yield new mutable.BitSet() // 19*19
  }

  private def lambdaReLU(value: Float): Float = if (value > 0.0f) value else 0.0f
}


class Network {

  // Input + residual block tower
  private var convWeights: Seq[Seq[NetWeight]] = Seq()
  private var convBiases: Seq[Seq[NetWeight]] = Seq()
  private var batchNormMeans: Seq[Seq[NetWeight]] = Seq()
  private var batchNormVariances: Seq[Seq[NetWeight]] = Seq()

  // Policy head
  private var convPolicyW: Seq[NetWeight] = _
  private var convPolicyB: Seq[NetWeight] = _
  private val bnPolicyW1: Array[NetWeight] = Array.ofDim[NetWeight](2)
  private val bnPolicyW2: Array[NetWeight] = Array.ofDim[NetWeight](2)

  private val ipPolicyW: Array[NetWeight] = Array.ofDim[NetWeight](IP_POLICY_W_LEN)
  private val ipPolicyB: Array[NetWeight] = Array.ofDim[NetWeight](IP_POLICY_B_LEN)

  // Value head
  private var convValW: Seq[NetWeight] = _
  private var convValB: Seq[NetWeight] = _
  private var bnValW1: Array[NetWeight] = Array.ofDim[NetWeight](1)
  private var bnValW2: Array[NetWeight] = Array.ofDim[NetWeight](1)

  private val ip1ValW: Array[NetWeight] = Array.ofDim[NetWeight](IP1_VAL_W_LEN)
  private val ip1ValB: Array[NetWeight] = Array.ofDim[NetWeight](IP1_VAL_B_LEN)

  private val ip2ValW: Array[NetWeight] = Array.ofDim[NetWeight](IP2_VAL_W_LEN)
  private val ip2ValB: Array[NetWeight] = Array.ofDim[NetWeight](1)

  initialize()

  def benchmark(history: GameHistory): Unit = {
    val BENCH_AMOUNT = 1600
    val cpus = Config.cfg_num_threads
    val itersPerThread: Int = (BENCH_AMOUNT + (cpus - 1)) / cpus
    val start = System.currentTimeMillis()

    class Worker() extends Runnable {
      def run(): Unit = {
        var myHistory: GameHistory = history
        for (loop <- 0 until itersPerThread) {
          val newHistory = myHistory.copy()
          val vec = getScoredMoves(history, ENSEMBLE_RANDOM_ROTATION)
          myHistory = newHistory
        }
      }
    }
    val tasks: Seq[Worker] = for (i <- 0 until cpus) yield new Worker()
    tasks.par.foreach(x => x.run())

    val end = System.currentTimeMillis()
    val timeDiffSecs = Timing.timeDiff(start, end)/100.0
    val numEvals = (BENCH_AMOUNT / timeDiffSecs.toFloat).toInt
    myPrint(f"$BENCH_AMOUNT%5d evaluations in $timeDiffSecs%5.2f seconds -> $numEvals%d n/s\n")
  }

  // ifdef USE_OPENCL
  private def initialize(): Unit = {
    myPrint("Initializing OpenCL")
    OPEN_CL.initialize()

    // Count size of the network
    myPrint("Detecting residual layers...")

    var lineCount: Int = 0
    if (Config.cfg_weightsfile.isEmpty) {
      myPrint("No Weights file to open.")
      System.exit(0)
    }

    var lines = Source.fromFile(Config.cfg_weightsfile.get).getLines.toList
    for (line <- lines) {
      // First line is the file format version id
      if (lineCount == 0) {
        val formatVersion = line.toInt
        if (formatVersion != FORMAT_VERSION) {
          myPrint(s"Weights file is the wrong version: $line")
          System.exit(0)
        }
        else myPrint(f"v$formatVersion%d")
      }

      // Third line of parameters are the convolution layer biases, so this tells the number of channels
      // in the residual layers (Provided they're all equally large - that's not actually required!)
      if (lineCount == 2) {
        val count = line.split(" ").length
          //std::distance(std::istream_iterator<std::string>(iss), std::istream_iterator<std::string>())
        myPrint(f"$count%d channels...")
      }
      lineCount += 1
    }

    // 1 format id, 1 input layer (4 x weights), 14 ending weights.
    // The rest are residuals, every residual has 8 x weight lines
    var residualBlocks = lineCount - (1 + 4 + 14)
    if (residualBlocks % 8 != 0) {
      myPrint("\nInconsistent number of weights in the file. Residual blocks = " + residualBlocks)
      System.exit(0)
    }
    residualBlocks /= 8
    myPrint(f"$residualBlocks%d blocks\nTransferring weights to GPU...")

    // Re-read file and process
    val plainConvLayers = 1 + (residualBlocks * 2)
    val plainConvWts = plainConvLayers * 4
    myPrint("plainConvWts = $plainConvWts")
    lineCount = 0
    lines = lines.drop(1) // skip the file format id on first line

    for (line <- lines) {
      val weights: Seq[NetWeight] = line.split(" ").map(_.toFloat)
      println("num weights in line is " + weights.length)

      if (lineCount < plainConvWts) {
        if (lineCount % 4 == 0) {
          convWeights :+= weights
        } else if (lineCount % 4 == 1) {
          convBiases :+= weights
        } else if (lineCount % 4 == 2) {
          batchNormMeans :+= weights
        } else if (lineCount % 4 == 3) {
          batchNormVariances :+= weights
        }
      } else if (lineCount == plainConvWts) {
        convPolicyW = weights      // std::move(weights)
      } else if (lineCount == plainConvWts + 1) {
        convPolicyB = weights
      } else if (lineCount == plainConvWts + 2) {
        assert(weights.length == 2)
        for (i <- bnPolicyW1.indices) bnPolicyW1(i) = weights(i) // std::copy(begin(weights), end(weights), begin(bn_pol_w1))
      } else if (lineCount == plainConvWts + 3) {
        for (i <- bnPolicyW2.indices) bnPolicyW2(i) = weights(i) // std::copy(begin(weights), end(weights), begin(bn_pol_w2))
      } else if (lineCount == plainConvWts + 4) {
        assert(weights.length == IP_POLICY_W_LEN)
        for (i <- weights.indices) ipPolicyW(i) = weights(i) // std::copy(weights, begin(ip_pol_w))
      } else if (lineCount == plainConvWts + 5) {
        assert(weights.length == IP_POLICY_B_LEN)
        for (i <- weights.indices) ipPolicyB(i) = weights(i) // std::copy(weights), begin(ip_pol_b))
      } else if (lineCount == plainConvWts + 6) {
        convValW = weights     // std::move(weights)
      } else if (lineCount == plainConvWts + 7) {
        convValB = weights     // std::move(weights)
      } else if (lineCount == plainConvWts + 8) {
        assert(weights.length == 1)
        for (i <- weights.indices) bnValW1(i) = weights(i)  //std::copy(weights, begin(bn_val_w1))
      } else if (lineCount == plainConvWts + 9) {
        assert(weights.length == 1)
        for (i <- weights.indices) bnValW2(i) = weights(i) // std::copy(weights, begin(bn_val_w2))
      } else if (lineCount == plainConvWts + 10) {
        assert(weights.length == IP1_VAL_W_LEN)
        for (i <- weights.indices) ip1ValW(i) = weights(i) // std::copy(weights, begin(ip1_val_w))
      } else if (lineCount == plainConvWts + 11) {
        assert(weights.length == IP1_VAL_B_LEN)
        for (i <- weights.indices) ip1ValB(i) = weights(i) // std::copy(weights, begin(ip1_val_b))
      } else if (lineCount == plainConvWts + 12) {
        assert(weights.length == IP2_VAL_W_LEN)
        for (i <- weights.indices) ip2ValW(i) = weights(i) //std::copy(weights), begin(ip2_val_w))
      } else if (lineCount == plainConvWts + 13) {
        assert(weights.length == 1)
        for (i <- weights.indices) ip2ValB(i) = weights(i)   // std::copy(weights, begin(ip2_val_b))
      }
      lineCount += 1
    }

    // input
    var weightIndex: Int = 0
    OPEN_CL_NET.pushConvolve(3, convWeights(weightIndex), convBiases(weightIndex))
    OPEN_CL_NET.pushBatchNorm(361, batchNormMeans(weightIndex), batchNormVariances(weightIndex))
    weightIndex += 1

    // residual blocks
    for (i <- 0 until residualBlocks) {
      OPEN_CL_NET.pushResidual(3,
        convWeights(weightIndex),
        convBiases(weightIndex),
        batchNormMeans(weightIndex),
        batchNormVariances(weightIndex),
        convWeights(weightIndex + 1),
        convBiases(weightIndex + 1),
        batchNormMeans(weightIndex + 1),
        batchNormVariances(weightIndex + 1))
      weightIndex += 2
    }
    myPrint("done")
  }

  /** @param output this gets populated */
  //#ifdef USE_BLAS
  def convolve(filterSize: Int, outputs: Int,  // template params?
               input: Seq[NetWeight], weights: Seq[NetWeight], biases: Seq[NetWeight],
               output: Array[NetWeight]): Unit = {
    // fixed for 19x19
    val width = BOARD_SIZE
    val height = BOARD_SIZE
    val boardSquares = width * height
    val filterLen = filterSize * filterSize
    val inputChannels = weights.length / (biases.length * filterLen)
    val filterDim = filterLen * inputChannels
    assert(outputs * boardSquares == output.length)

    val col = Array.ofDim[NetWeight](filterDim * width * height)
    Im2Col.im2Col(filterSize, inputChannels, input, col)

    // Weight shape (output, input, filterSize, filterSize)
    // 96 22 3 3
    // outputs[96, 19 x 19] = weights[96, 22 x 3 x 3] x col[22 x 3 x 3, 19 x 19]
    // C←αAB + βC
    // M Number of rows in matrices A and C.
    // N Number of columns in matrices B and C.
    // K Number of columns in matrix A; number of rows in matrix B.
    // lda The size of the first dimension of matrix A; if you are
    // passing a matrix A[m][n], the value should be m.
    //    cblas_sgemm(CblasRowMajor, TransA, TransB, M, N, K, alpha, A, lda, B,
    //                ldb, beta, C, N);
    cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans,
      // M        N            K
      outputs, boardSquares, filterDim,
      1.0f, weights.toArray, filterDim,
      col, boardSquares,
      0.0f, output.toArray, boardSquares)

    for (o <- 0 until outputs) {
      for (b <- 0 until boardSquares) {
        output((o * boardSquares) + b) = biases(o) + output((o * boardSquares) + b)
      }
    }
  }

  def innerProduct(inputs: Int, outputs: Int,
                   input: Seq[NetWeight], weights: Seq[NetWeight], biases: Seq[NetWeight],
                   output: Array[NetWeight]): Unit = {
    cblas_sgemv(CblasRowMajor, CblasNoTrans,
      // M     K
      outputs, inputs,
      1.0f, weights.toArray, inputs,
      input.toArray, 1,
      0.0f, output.toArray, 1)

    for (o <- 0 until outputs) {
      var value: Float = biases(o) + output(o)
      if (outputs == 256) {
        value = lambdaReLU(value)
      }
      output(o) = value
    }
  }

  def batchNorm(channels: Int, spatialSize: Int, input: Seq[NetWeight],
                means: Seq[NetWeight], variances: Seq[NetWeight], output: Array[NetWeight]): Unit = {
    val epsilon = 1e-5f

    for (c <- 0 until channels) {
      var mean = means(c)
      var variance = variances(c) + epsilon
      var scaleStddiv = 1.0f / Math.sqrt(variance)

      //float * out = &output[c * spatial_size]
      //float const * in  = &input[c * spatial_size]
      for (b <- 0 until spatialSize) {
        val value = (scaleStddiv * (input(b) - mean)).toFloat
        output(b) = lambdaReLU(value)
      }
    }
  } // endif #USE_BLAS

  private def softMax(input: Array[NetWeight], output: Array[NetWeight], temperature: Float): Unit = {
    assert(!(input sameElements output))

    var alpha = input.take(output.length).max      //*std::max_element(input.begin(), input.begin() + output.size())
    alpha /= temperature

    var denom = 0.0f
    val helper: Array[Float] = Array.ofDim[Float](output.length)

    for (i <- output.indices) {
      val value = Math.exp((input(i) / temperature) - alpha).toFloat
      helper(i) = value
      denom += value
    }
    for (i <- output.indices) {
      output(i) = helper(i) / denom
    }
  }

  def getScoredMoves(history: GameHistory, ensemble: Ensemble, rotation: Int = -1): NetResult = {
    var result: NetResult = null
    val state: KoState = history.getCurrentState
    if (state.size != BOARD_SIZE) {
      return result
    }

    var planes: NNPlanes = gatherFeatures(history)

    if (ensemble == ENSEMBLE_DIRECT) {
      assert(rotation >= 0 && rotation <= 7)
      result = getScoredMovesInternal(history, planes, rotation)
    } else {
      assert(ensemble == ENSEMBLE_RANDOM_ROTATION)
      assert(rotation == -1)
      val randRot = RND.nextInt(8) //eRandom::gt_Rng()->randfix<8>()
      result = getScoredMovesInternal(history, planes, randRot)
    }

    result
  }

  private def getScoredMovesInternal(history: GameHistory, planes: NNPlanes, rotation: Int): NetResult = {

    assert(rotation >= 0 && rotation <= 7)
    assert(INPUT_CHANNELS == planes.length)
    val width = BOARD_SIZE
    val height = BOARD_SIZE
    val convolveChannels = convPolicyW.length / convPolicyB.length

    // Data layout is input_data[(c * height + h) * width + w]
    val inputData = Array.ofDim[NetWeight](INPUT_CHANNELS * width * height) // Array.ofDim[Int](INPUT_CHANNELS * width * height)
    val outputData = Array.ofDim[NetWeight](convolveChannels * width * height) //std::vector<net_t> output_data(convolve_channels * width * height)
    val policyData1 = Array.ofDim[NetWeight](2 * width * height)
    val policyData2 = Array.ofDim[NetWeight](2 * width * height)
    val valueData1 = Array.ofDim[NetWeight](1 * width * height)
    val valueData2 = Array.ofDim[NetWeight](1 * width * height)
    val policyOut = Array.ofDim[NetWeight]((width * height) + 1)
    val softmaxData = Array.ofDim[NetWeight]((width * height) + 1)
    val winrateData = Array.ofDim[NetWeight](256)
    val winrateOut = Array.ofDim[NetWeight](1)

    var ct = 0
    for (c <- 0 until INPUT_CHANNELS) {
        for (h <- 0 until height) {
            for (w <- 0 until width) {
              val rotIdx = rotateNnIdx(h * BOARD_SIZE + w, rotation)
              inputData(ct) = if (planes(c)(rotIdx)) 1 else 0
              ct +=1
            }
        }
    }
    var outputs: Seq[NetWeight] = null

    if (Config.USE_OPENCL) {
      OPEN_CL_NET.forward(inputData, outputData)
      // Get the moves
      convolve(1, 2, outputData, convPolicyW, convPolicyB, policyData1)
      batchNorm(2, 361, policyData1, bnPolicyW1, bnPolicyW2, policyData2)
      innerProduct(2 * 361, 362, policyData2, ipPolicyW, ipPolicyB, policyOut)
      softMax(policyOut, softmaxData, Config.cfg_softmax_temp)
      outputs = softmaxData // std::vector<float>&

      // Now get the score
      convolve(1, 1, outputData, convValW, convValB, valueData1)
      batchNorm(1, 361, valueData1, bnValW1, bnValW2, valueData2)
      innerProduct(361, 256, valueData2, ip1ValW, ip1ValB, winrateData)
      innerProduct(256, 1, winrateData, ip2ValW, ip2ValB, winrateOut)
    } else if (Config.USE_BLAS && !Config.USE_OPENCL) {
      throw new UnsupportedOperationException(
        "Not yet implemented. Not very useful unless you have some sort of Xeon Phi")
      //softmax(output_data, softmax_data, cfg_softmax_temp)
      // Move scores
      //std::vector<float>& outputs = softmax_data
    }
    // Sigmoid
    val winrateSig = ((1.0f + Math.tanh(winrateOut(0))) / 2.0f).toFloat

    val state = history.getCurrentState
    var result: Seq[ScoredNode] = Seq()
    for (idx <- outputs.indices) {
        if (idx < BOARD_SQ) {
            val value = outputs(idx)
            val rotIdx = rotateNnIdx(idx, rotation)
            val x = rotIdx % BOARD_SIZE
            val y = rotIdx / BOARD_SIZE
            val rotVtx = state.getBoard.getVertex(x, y)
            if (state.getBoard.getSquare(rotVtx) == EMPTY) {
                result :+= (value, rotVtx)
            }
        } else {
            result :+= (outputs(idx), PASS)
        }
    }

    (result, winrateSig)
  }

  def showHeatmap(state: KoState, result: NetResult, topMoves: Boolean): Unit = {

    var moves = result._1
    var displayMap: Seq[String] = Seq()
    var line: String = ""

    for (y <- 0 until BOARD_SIZE) {
      for (x <- 0 until BOARD_SIZE) {
        val vtx = state.getBoard.getVertex(x, y)
        val item = moves.find(item => item._2 == vtx).get
        var score = 0.0f
        // Non-empty squares won't be scored
        val last = moves.last
        if (item != last) {
            score = item._1
            assert(vtx == item._2)
        }

        val s = (score * 1000).toInt
        line += f"$s%3d "
        if (x == 18) {
            displayMap :+= line
            line = ""
        }
      }
    }

    for (i <- displayMap.length - 1 to 0 by -1) {
        myPrint(f"${displayMap(i)}%s\n")
    }
    assert(result._1.last._2 == PASS)
    val passScore = (result._1.last._1 * 1000).toInt
    myPrint(f"pass: $passScore%d")
    myPrint(f"winrate: ${result._2}%f")
    val bs = new FastBoardSerializer(state.getBoard)

    if (topMoves) {
      moves = moves.sorted  // std::stable_sort(moves.rbegin(), moves.rend())
      var cum = 0.0f
      var tried: Int = 0
      var done = false
      while (cum < 0.85f && tried < moves.length && !done) {
        if (moves(tried)._1 < 0.01f) done == true
        else {
          myPrint(f"${moves(tried)._1}%1.3f (${bs.moveToText(moves(tried)._2.toShort)}%s)\n")
          cum += moves(tried)._1
          tried += 1
        }
      }
    }
  }

  def gatherFeatures(history: GameHistory): NNPlanes = {
    val planes = createNNPlanes()
    var ourOffset: Int = 0
    var theirOffset: Int = 8
    var blackToMove: BoardPlane = planes(16)
    var whiteToMove: BoardPlane = planes(17)

    var toMove: Int = history.getCurrentState.getBoard.getToMove
    var whitesMove = toMove == WHITE
    if (whitesMove) {
        whiteToMove ++= mutable.BitSet(1 to BOARD_SQ: _*)
    } else {
        blackToMove ++= mutable.BitSet(1 to BOARD_SQ: _*)
    }

    // Go back in time, fill history boards
    var backtracks: Int = 0
    var h = 0
    var done = false
    while (h < 8 && !done) {
      // collect white, black occupation planes
      for (j <- 0 until BOARD_SIZE) {
          for (i <- 0 until BOARD_SIZE) {
              val vtx: Short = history.getCurrentState.getBoard.getVertex(i, j)
              val color: Byte = history.getCurrentState.getBoard.getSquare(vtx)
              val idx: Int = j * BOARD_SIZE + i
              if (color != EMPTY) {
                  if (color == toMove) {
                      planes(ourOffset + h)(idx) = true
                  } else {
                      planes(theirOffset + h)(idx) = true
                  }
              }
          }
      }
      if (!history.undoMove()) {
          done = true
      } else {
          backtracks += 1
      }
      h += 1
    }

    // Now go back to present day
    for (h <- 0 until backtracks) {
      history.forwardMove()
    }
    planes
  }

  /** @return a new index in the range [0, 19*19) */
  private def rotateNnIdx(vertex: Int, symmetry: Int): Int = {
    assert(vertex >= 0 && vertex < BOARD_SQ)
    assert(symmetry >= 0 && symmetry < 8)
    var symm = symmetry
    var x = vertex % 19
    var y = vertex / 19
    var newx = 0
    var newy = 0

    if (symm >= 4) {
      val tmp = x
      y = x
      x = tmp
      symm -= 4
    }

    if (symm == 0) {
      newx = x
      newy = y
    } else if (symm == 1) {
      newx = x
      newy = 19 - y - 1
    } else if (symm == 2) {
      newx = 19 - x - 1
      newy = y
    } else {
      assert(symmetry == 3)
      newx = 19 - x - 1
      newy = 19 - y - 1
    }

    val newVtx = (newy * 19) + newx
    assert(newVtx >= 0 && newVtx < BOARD_SQ)
    newVtx
  }
}
