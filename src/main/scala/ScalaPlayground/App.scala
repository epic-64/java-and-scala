package ScalaPlayground

import JavaPlayground.JavaRomans
import ScalaPlayground.RomanNumerals.ScalaRomansPipeline

object App {
  def main(args: Array[String]): Unit = {
    val javaRomans = new JavaRomans()
    println(javaRomans.toNumeral(1996))

    val scalaRomans = ScalaRomansPipeline()
    println(scalaRomans.toNumeral(1996))
  }
}
