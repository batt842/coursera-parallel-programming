package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import ParallelCountChange._

@RunWith(classOf[JUnitRunner])
class ParallelCountChangeSuite extends FunSuite {
  def parCountChangeSimple(money: Int, coins: List[Int]): Int = {
//    parCountChange(money, coins, combinedThreshold(money, coins))
    parCountChange(money, coins, moneyThreshold(money))
  }

  test("parCountChange should return 0 for money < 0") {
    def check(money: Int, coins: List[Int]) =
      assert(parCountChangeSimple(money, coins) == 0,
        s"parCountChange($money, _) should be 0")

    check(-1, List())
    check(-1, List(1, 2, 3))
    check(-Int.MinValue, List())
    check(-Int.MinValue, List(1, 2, 3))
  }

  test("parCountChange should return 1 when money == 0") {
    def check(coins: List[Int]) =
      assert(parCountChangeSimple(0, coins) == 1,
        s"parCountChange(0, _) should be 1")

    check(List())
    check(List(1, 2, 3))
    check(List.range(1, 100))
  }

  test("parCountChange should return 0 for money > 0 and coins = List()") {
    def check(money: Int) =
      assert(parCountChangeSimple(money, List()) == 0,
        s"parCountChange($money, List()) should be 0")

    check(1)
    check(Int.MaxValue)
  }

  test("parCountChange should work when there is only one coin") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChangeSimple(money, coins) == expected,
        s"parCountChange($money, $coins) should be $expected")

    check(1, List(1), 1)
    check(2, List(1), 1)
    check(1, List(2), 0)
    check(Int.MaxValue, List(Int.MaxValue), 1)
    check(Int.MaxValue - 1, List(Int.MaxValue), 0)
  }

//  test("parCountChange should invoke the parallel construct 6 times for money == 16, coins == List(1) and moneyThreshold(16)") {
//    def check(money: Int, coins: List[Int], expected: Int) = {
//      parCountChange(money, coins, moneyThreshold(money))
//      assert(??? == expected,
//        s"The number of parallel calls should be $expected")
//    }
//
//    check(16, List(1), 6)
//  }

  test("parCountChange should work for multi-coins") {
    def check(money: Int, coins: List[Int], expected: Int) =
      assert(parCountChangeSimple(money, coins) == expected,
        s"parCountChange($money, $coins) should be $expected")

    check(50, List(1, 2, 5, 10), 341)
    check(250, List(1, 2, 5, 10, 20, 50), 177863)
  }
}
