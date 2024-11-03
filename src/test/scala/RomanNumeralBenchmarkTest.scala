import JavaPlayground.RomanNumerals.JavaRomans
import ScalaPlayground.RomanNumerals.*
import org.scalatest.funsuite.AnyFunSuite

class RomanNumeralBenchmarkTest extends AnyFunSuite {
  test("benchmark the performance of Roman Numerals implementations") {
    def checkDuration(action: Int => String, input: Int, iterations: Int, message: String): Unit = {
      val now = System.nanoTime()
      for (_ <- 1 to iterations) action(input)
      val elapsed = System.nanoTime() - now

      info(f"$message: ${elapsed / 1_000_000f}%1.0f ms")
    }

    val input = 3999
    val iterations = 10_000

    val javaRomans = new JavaRomans()
    val scalaRomansImperative = ScalaRomansImperative()
    val scalaRomansIdiomatic = ScalaRomansIdiomatic()
    val scalaRomansPipeline = ScalaRomansPipeline()

    checkDuration(javaRomans.toNumeral, input, iterations, "Java Romans Imperative")
    checkDuration(scalaRomansImperative.toNumeral, input, iterations, "Scala Romans Imperative")
    checkDuration(scalaRomansIdiomatic.toNumeral, input, iterations, "Scala Romans Idiomatic")
    checkDuration(scalaRomansPipeline.toNumeral, input, iterations, "Scala Romans Pipeline")
  }
}
