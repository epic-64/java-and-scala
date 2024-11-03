package ScalaPlayground.RomanNumerals

trait Romans:
  def toNumeral(number: Int): String
  def toInt(roman: String): Int

class ScalaRomansImperative extends Romans:
  private val values: Vector[(Int, String)] = Vector(
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

  override def toNumeral(number: Int): String = {
    val sb        = new StringBuilder
    var remaining = number

    for ((value, numeral) <- values)
      while (remaining >= value) {
        sb.append(numeral)
        remaining -= value
      }

    sb.toString()
  }

  override def toInt(roman: String): Int = {
    var result    = 0
    var remaining = roman

    for ((value, numeral) <- values)
      while (remaining.startsWith(numeral)) {
        result += value
        remaining = remaining.substring(numeral.length)
      }

    result
  }
end ScalaRomansImperative

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
      // custom edge cases
      // .replace("CMXCIX", "IM") // 999
      // .replace("XCIX", "IC")   // 99

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
