package leelazero.network

import java.nio.ByteBuffer

import leelazero.Config
import leelazero.network.Network.NetWeight
import leelazero.sgf.{SgfParser, SgfTree}
import org.scalatest.FunSuite
import Network.FLOAT_SIZE

import scala.util.Random


class NetworkSuite extends FunSuite {

  Config.cfg_weightsfile = Some("scala-test/leelazero/sgf/weights/leelaz-model-256000.txt")
  // check only one initialized singleton instance
  val instance: Network = Network.getInstance()

  test("get instance") {
    assertResult(instance) {Network.getInstance()}
  }

  test("get benchmark") {
    val tree: SgfTree = SgfParser.loadFromFile("../sgf/games/simple_match.sgf")
    val gameHistory = tree.gameHistory
    val result = Network.getInstance().getBenchmarkResult(gameHistory)

    println("becnchmark result = " + result)
    assertResult(1600) { result.numEvaluations }
    assert(result.timeDiffSeconds < 10 )
    assert(result.evalsPerSecond > 600 )
  }

  test("trivial convolve") {
    val convPolicyW: Seq[NetWeight] = Seq(1, 1, 1, 1)
    val convPolicyB: Seq[NetWeight] = Seq(1, 1)
    val width = 19
    val height = 19
    val policyData1 = Array.ofDim[NetWeight](2 * width * height)
    val convolveChannels = convPolicyW.length / convPolicyB.length

    val outputData =
      ByteBuffer.allocateDirect(convolveChannels * width * height * FLOAT_SIZE).asFloatBuffer()

    Network.getInstance().convolve(1, 2, outputData, convPolicyW, convPolicyB, policyData1)

    assertResult("java.nio.DirectFloatBufferS[pos=0 lim=722 cap=722]") {outputData.toString}
    assert(outputData.isDirect)
    assertResult("0.0, 0.0, 0.0, 0.0") {
      var dst = Array.ofDim[Float](4)
      outputData.get(dst)
      dst.mkString(", ")
    }
  }

  test("reg convolve") {
    val rnd = new Random(0)
    val convPolicyW: Seq[NetWeight] = Seq(1, 2, 3, 4, 5, 6, 7, 8)
    val convPolicyB: Seq[NetWeight] = Seq(11, 12, 13, 14, 15, 16, 17, 18)
    val width = 19
    val height = 19
    val outputData = Array.ofDim[NetWeight](2 * width * height)
    val convolveChannels = convPolicyW.length / convPolicyB.length

    val rawdata = Array.fill[Float](convolveChannels * width * height)(rnd.nextFloat())
    val policyData1 =
      ByteBuffer.allocateDirect(convolveChannels * width * height * FLOAT_SIZE).asFloatBuffer()
    policyData1.put(rawdata)

    Network.getInstance().convolve(1, 2, policyData1, convPolicyW, convPolicyB, outputData)

    //assertResult("java.nio.DirectFloatBufferS[pos=0 lim=361 cap=361]") {outputData.toString}
    assertResult(
      "11.7309675, 11.831441, 11.240537, 11.606345, 11.637418, 11.309051, 11.550437, 11.117006, 11.597546, 11.781534") {
      outputData.take(10).mkString(", ")
    }
  }

  test("gather features") {
    val tree: SgfTree = SgfParser.loadFromFile("../sgf/games/simple_match.sgf")
    tree.getState.displayState()
    val gameHistory = tree.gameHistory
    val featues = Network.getInstance().gatherFeatures(gameHistory)

    assertResult(18) {featues.size}
    assertResult("") { featues.head.mkString(", ") }
    assertResult(0) { featues(10).size }
    assertResult("") { featues(10).mkString(", ") }
    assertResult(361) {featues(16).size}
    assertResult("1, 2, 3, 4, 5, 6, 7, 8, 9, 10") { featues(16).take(10).mkString(", ") }
    assertResult("") { featues(17).mkString(", ") }
    assertResult("BitSet()") { featues.head.toString }
  }
}
