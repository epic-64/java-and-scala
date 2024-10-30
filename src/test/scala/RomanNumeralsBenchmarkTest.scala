import ScalaPlayground.RomanNumerals.ScalaRomansImperative
import org.scalatest.funsuite.AnyFunSuite

class RomanNumeralsBenchmarkTest extends AnyFunSuite {
  test("benchmark the performance of Roman Numerals implementations") {
    def checkDuration(action: Int => String, input: Int, iterations: Int, message: String): Unit = {
      val now = System.nanoTime()
      for (_ <- 1 to iterations) action(input)
      val elapsed = System.nanoTime() - now

      println(message)
      println(elapsed / 1_000_000 + " ms")
    }

    val input = 3999
    val iterations = 1_000_000

    val javaRomans = new JavaPlayground.JavaRomans()
    val scalaRomans = ScalaRomansImperative()

    checkDuration(javaRomans.toNumeral, input, iterations, "Java Romans Imperative")
    checkDuration(scalaRomans.toNumeral, input, iterations, "Scala Romans Imperative")
  }
}
