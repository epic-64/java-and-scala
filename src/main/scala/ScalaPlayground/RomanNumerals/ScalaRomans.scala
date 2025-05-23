package ScalaPlayground.RomanNumerals

import JavaPlayground.RomanNumerals.JavaRomans

import scala.annotation.tailrec

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

extension (n: Int) def toRomanNumeral: String = ScalaRomansImperative().toNumeral(n)
extension (numeral: String) def toInt: Int    = ScalaRomansImperative().toInt(numeral)

trait Romans:
  def toNumeral(number: Int): String
  def toInt(roman: String): Int

private val romanNumerals: Vector[(Int, String)] = Vector(
  1000 -> "M",
  900  -> "CM",
  500  -> "D",
  400  -> "CD",
  100  -> "C",
  90   -> "XC",
  50   -> "L",
  40   -> "XL",
  10   -> "X",
  9    -> "IX",
  5    -> "V",
  4    -> "IV",
  1    -> "I"
)

class ScalaRomansImperative extends Romans:
  override def toNumeral(number: Int): String = {
    val sb        = new StringBuilder
    var remaining = number

    for ((value, numeral) <- romanNumerals)
      while (remaining >= value) {
        sb.append(numeral)
        remaining -= value
      }

    sb.toString()
  }

  override def toInt(roman: String): Int = {
    var result    = 0
    var remaining = roman

    for ((value, numeral) <- romanNumerals)
      while (remaining.startsWith(numeral)) {
        result += value
        remaining = remaining.substring(numeral.length)
      }

    result
  }
end ScalaRomansImperative

class ScalaRomansIdiomatic extends Romans {
  def toNumeral(number: Int): String = {
    @tailrec
    def convert(remaining: Int, pairs: Vector[(Int, String)], result: String = ""): String =
      pairs match
        case (value, numeral) +: _ if remaining >= value => convert(remaining - value, pairs, result + numeral)
        case _ +: tail                                   => convert(remaining, tail, result)
        case _                                           => result

    convert(number, romanNumerals)
  }

  override def toInt(roman: String): Int = {
    @tailrec
    def convert(remaining: String, pairs: Vector[(Int, String)], result: Int = 0): Int =
      pairs match
        case (value, numeral) +: _ if remaining.startsWith(numeral) =>
          convert(remaining.substring(numeral.length), pairs, result + value)
        case _ +: tail                                              => convert(remaining, tail, result)
        case _                                                      => result

    convert(roman, romanNumerals)
  }
}

class ScalaRomansPipeline extends Romans:
  override def toNumeral(number: Int): String =
    ("I" * number)
      .replace("IIIII", "V")
      .replace("IIII", "IV")
      .replace("VV", "X")
      .replace("VIV", "IX")
      .replace("XXXXX", "L")
      .replace("XXXX", "XL")
      .replace("LL", "C")
      .replace("LXL", "XC")
      .replace("CCCCC", "D")
      .replace("CCCC", "CD")
      .replace("DD", "M")
      .replace("DCD", "CM")

  override def toInt(roman: String): Int =
    roman
      .replace("CM", "DCD")
      .replace("M", "DD")
      .replace("CD", "CCCC")
      .replace("D", "CCCCC")
      .replace("XC", "LXL")
      .replace("C", "LL")
      .replace("XL", "XXXX")
      .replace("L", "XXXXX")
      .replace("IX", "VIV")
      .replace("X", "VV")
      .replace("IV", "IIII")
      .replace("V", "IIIII")
      .length
end ScalaRomansPipeline
