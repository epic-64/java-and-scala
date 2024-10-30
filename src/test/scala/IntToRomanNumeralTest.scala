import JavaPlayground.JavaRomans
import ScalaPlayground.RomanNumerals.{ScalaRomansImperative, ScalaRomansPipeline}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.*
import org.scalatest.prop.TableFor2

class IntToRomanNumeralTest extends AnyFunSuite {
  val testCases: TableFor2[Int, String] = Table(
    ("input", "expected"),
    (0, ""),
    (1, "I"),
    (4, "IV"),
    (9, "IX"),
    (58, "LVIII"),
    (1999, "MCMXCIX"),
    (2023, "MMXXIII"),
    (1987, "MCMLXXXVII"),
    (3999, "MMMCMXCIX"),
  )

  val javaRomans            = new JavaRomans
  val scalaRomansPipeline   = new ScalaRomansPipeline
  val scalaRomansImperative = new ScalaRomansImperative

  testCases.foreach { case (input, expected) =>
    test(s"Java and Scala implementations return $expected for input $input") {
      assert(javaRomans.toNumeral(input) == expected)
      assert(scalaRomansPipeline.toNumeral(input) == expected)
      assert(scalaRomansImperative.toNumeral(input) == expected)
    }
  }
}
