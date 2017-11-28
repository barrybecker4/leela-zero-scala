package leelazero

import java.io.{ByteArrayOutputStream, OutputStream}

import org.scalatest.FunSuite

class UtilsSuite extends FunSuite {

  val out = new ByteArrayOutputStream()
  Utils.cfgLogFileHandle = Some(out)

  test("myPrint") {
    val foo = 4
    Utils.myPrint(f"$foo%2d result")

    assertResult(" 4 result") { out.toString }
  }

  test("Longest hex long") {
    val big = 9223372036854775807L
    println("largest long in hex = " + big.toHexString)
    assertResult("7FFFFFFFFFFFFFFF") { big.toHexString.toUpperCase() }

    val a1 = 0x1234567887654321L
    assertResult(1311768467139281697L) { a1 }
    val a2 = 0xABCDABCDABCDABCDL
    assertResult(-6067004223159161907L) { a2 }
  }
}
