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
}
