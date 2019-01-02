package scalashop

import org.scalameter._
import common._
import scalashop.HorizontalBoxBlur.blur

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    for (x <- from until end; y <- 0 until src.height)
      dst.update(x, y, boxBlurKernel(src, x, y, radius))
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    /*
    def _parBlur(ranges: List[(Int, Int)]): Unit = ranges match {
      case x :: rem => parallel(blur(src, dst, x._1, x._2, radius), _parBlur(rem))
      case Nil => return
    }
    val numChunk = caculateChunkNumber(src.width, numTasks)
    _parBlur((for (x <- 0 until src.width by numChunk) yield  (x, clamp(x + numChunk, x, src.width))).toList)
    */
    val numChunk = caculateChunkNumber(src.width, numTasks)
    val tasks = for (x <- 0 until src.width by numChunk) yield task {
      blur(src, dst, x, clamp(x + numChunk, x, src.width), radius)
    }
    for (t <- tasks) t.join
  }

}
