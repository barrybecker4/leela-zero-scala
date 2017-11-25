package leelazero

object TestUtil {
  def clean(str: String): String = str.stripMargin.replaceAll("\r\n", "\n")
}
