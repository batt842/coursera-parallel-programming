package scalashop

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import scalashop.HorizontalBoxBlur.blur

@RunWith(classOf[JUnitRunner])
class MiscSuite extends FunSuite {
  test("cutting test") {
    {
      val h = 32
      val numChunk = 1
      println(for (x <- 0 until h by numChunk) yield (x, clamp(x + numChunk, x, h)))
    }

    {
      val h = 23
      val numChunk = 7
      println(for (x <- 0 until h by numChunk) yield (x, clamp(x + numChunk, x, h)))
    }

    {
      val h = 22
      val numChunk = 7
      println(for (x <- 0 until h by numChunk) yield (x, clamp(x + numChunk, x, h)))
    }

    {
      val h = 21
      val numChunk = 7
      println(for (x <- 0 until h by numChunk) yield (x, clamp(x + numChunk, x, h)))
    }

    {
      val h = 20
      val numChunk = 7
      println(for (x <- 0 until h by numChunk) yield (x, clamp(x + numChunk, x, h)))
    }
  }

  test("to or until") {
    println(for (x <- 0 to 10) yield x)
    println(for (x <- 0 until 10) yield x)
  }
}
