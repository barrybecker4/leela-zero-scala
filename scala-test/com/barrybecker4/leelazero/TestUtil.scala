package com.barrybecker4.leelazero

object TestUtil {
  def clean(str: String): String = str.stripMargin.replaceAll("\r\n", "\n")
}
