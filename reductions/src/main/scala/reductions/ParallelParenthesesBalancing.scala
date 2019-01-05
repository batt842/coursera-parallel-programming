package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var counter = 0
    chars.forall(c => {
      if (c == '(') counter += 1
      else if (c == ')') counter -= 1

      counter >= 0
    })
    counter == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, delta: Int, minDepth: Int): (Int, Int) = {
      if (idx >= until) (delta, minDepth)
      else {
        val v = if (chars(idx) == '(') 1 else if (chars(idx) == ')') -1 else 0
        val newDelta = delta + v
        traverse(idx + 1, until, newDelta, Math.min(newDelta, minDepth))
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        val (x, y) = traverse(from, until, 0, 0)
//        println(chars.subSequence(from, until).toString +  " => (" + x + "," + y + ")")
        (x, y)
      } else {
        val m = (from + until) / 2
        val ((lx, ly), (rx, ry)) = parallel(reduce(from, m), reduce(m, until))
        val x = lx + rx
        val y = if (ly < ry) ly else lx + ry
//        println(chars.subSequence(from, until).toString +  " => (" + x + "," + y + ")")
        (x, y)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
