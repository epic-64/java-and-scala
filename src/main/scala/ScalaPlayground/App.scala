package ScalaPlayground

import JavaPlayground.RomanNumerals.JavaRomans
import ScalaPlayground.RomanNumerals.{ScalaRomansImperative, ScalaRomansPipeline, toRomanNumeral, toInt}

object App {
  def main(args: Array[String]): Unit = {
    val javaRomans = new JavaRomans()
    val scalaRomans = ScalaRomansPipeline()
    
    println(javaRomans.toNumeral(1996))
    println(scalaRomans.toNumeral(1996))
    
    println(147.toRomanNumeral)
    println("MMXXIV".toInt)
  }
}
