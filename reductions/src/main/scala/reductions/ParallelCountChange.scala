package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
//    def _countChange(m: Int, cs: List[Int], memo: scala.collection.mutable.Map[String, Int]): Int = {
//      val memoKey = m + ":" + (cs mkString ",")
//      if (memo contains memoKey) memo(memoKey)
//      else {
//        if (m == 0) 1
//        else if (m < 0) 0
//        else if (cs.isEmpty) 0
//        else {
//          var currCount = 0
//          currCount += _countChange(m - cs.head, cs, memo) // take a coin
//          currCount += _countChange(m, cs.tail, memo) // drop a coin
//
//          memo(memoKey) = currCount
//          currCount
//        }
//      }
//    }
//    _countChange(money, coins, scala.collection.mutable.Map());

    def _countChange(m: Int, cs: List[Int]): Int = {
      if (m == 0) 1
      else if (m < 0) 0
      else if (cs.isEmpty) 0
      else {
        var currCount = 0
        currCount += _countChange(m - cs.head, cs) // take a coin
        currCount += _countChange(m, cs.tail) // drop a coin
        currCount
      }
    }

    _countChange(money, coins)
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else {
      if (threshold(money, coins))
        countChange(money, coins)
      else {
        val (count0, count1) = parallel(
          parCountChange(money - coins.head, coins, threshold), // take a coin
          parCountChange(money, coins.tail, threshold) // drop a coin
        )
        count0 + count1
      }
    }
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold = {
    val threshold = startingMoney * 2 / 3
    (money: Int, coins: List[Int]) => money <= threshold
  }

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold = {
    val threshold = totalCoins * 2 / 3
    (money: Int, coins: List[Int]) => coins.length <= threshold
  }


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    val threshold = startingMoney * allCoins.length / 2
    (money: Int, coins: List[Int]) => money * coins.length <= threshold
  }
}
