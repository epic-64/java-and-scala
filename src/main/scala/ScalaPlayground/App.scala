package ScalaPlayground

import JavaPlayground.RomanNumerals.JavaRomans
import ScalaPlayground.RomanNumerals.{ScalaRomansImperative, ScalaRomansPipeline}

object App {
  extension (n: Int)
    def toRoman: String = (new JavaRomans).toNumeral(n)

  extension (s: String)
    def fromRoman: Int = (new ScalaRomansImperative).toInt(s)
  
  def main(args: Array[String]): Unit = {
    val javaRomans = new JavaRomans()
    val scalaRomans = ScalaRomansPipeline()
    
    println(javaRomans.toNumeral(1996))
    println(scalaRomans.toNumeral(1996))

    val numeral147 = 147.toRoman
    val numberMMXXIV = "MMXXIV".fromRoman
    
    println(s"147 is $numeral147")
    println(s"MMXXIV is $numberMMXXIV")
  }
}
