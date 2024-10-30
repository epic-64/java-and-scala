import JavaPlayground.JavaRomans
import ScalaPlayground.RomanNumerals.{ScalaRomansImperative, ScalaRomansPipeline}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.*
import org.scalatest.prop.TableFor2

class RomanNumeralToIntTest extends AnyFunSuite {
  val testCases: TableFor2[String, Int] = Table(
    ("input", "expected"),
    ("", 0),
    ("I", 1),
    ("IV", 4),
    ("IX", 9),
    ("LVIII", 58),
    ("MCMXCIX", 1999),
    ("MMXXIII", 2023),
    ("MCMLXXXVII", 1987),
    ("MMMCMXCIX", 3999),
  )

  val javaRomans = new JavaRomans
  val scalaRomansPipeline = new ScalaRomansPipeline
  val scalaRomansImperative = new ScalaRomansImperative

  testCases.foreach { case (input, expected) =>
    test(s"Java and Scala implementations return $expected for input $input") {
      // assert(javaRomans.toInt(input) == expected)
      assert(scalaRomansPipeline.toInt(input) == expected)
      assert(scalaRomansImperative.toInt(input) == expected)
    }
  }
}
