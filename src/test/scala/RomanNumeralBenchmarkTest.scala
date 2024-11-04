import JavaPlayground.RomanNumerals.JavaRomans
import ScalaPlayground.RomanNumerals.*
import org.scalatest.funsuite.AnyFunSuite

class RomanNumeralBenchmarkTest extends AnyFunSuite {
  test("benchmark the performance of Roman Numerals implementations") {
    def benchmark(action: Int => String, input: Int, iterations: Int, message: String): Unit = {
      val now = System.nanoTime()
      val freeMemory = collection.mutable.ArrayBuffer.empty[Float]
      
      for (i <- 1 to iterations) {
        action(input)
        if (i % 10_000 == 0) freeMemory += Runtime.getRuntime.freeMemory() / 1024f / 1024f
      }
      
      val elapsed = System.nanoTime() - now
      
      info(f"$message: ${elapsed / 1_000_000f}%1.0f ms, ${freeMemory.mkString(", ")}")
    }

    val input = 3999
    val iterations = 100_000

    val javaRomans = new JavaRomans()
    val scalaRomansImperative = ScalaRomansImperative()
    val scalaRomansIdiomatic = ScalaRomansIdiomatic()
    val scalaRomansPipeline = ScalaRomansPipeline()

    benchmark(javaRomans.toNumeral, input, iterations, "Java Romans Imperative")
    benchmark(scalaRomansImperative.toNumeral, input, iterations, "Scala Romans Imperative")
    benchmark(scalaRomansIdiomatic.toNumeral, input, iterations, "Scala Romans Idiomatic")
    benchmark(scalaRomansPipeline.toNumeral, input, iterations, "Scala Romans Pipeline")
  }
}
