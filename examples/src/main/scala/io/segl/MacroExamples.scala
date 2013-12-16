package io.segl

import SeglMacros.hello
import SeglMacros.trace
import SeglMacros.info
import org.slf4j.LoggerFactory

object MacroExamples {

  private val logger = LoggerFactory.getLogger("macros")

  def main(args: Array[String]) {
    hello("Macros")

    val a = 2
    val b = 3
    trace(a + b)
    trace(1 + 2)
    trace("a" + 2)
    trace("a" + "b")
    trace("a" + new Object)

    info(a + b)
  }
}
