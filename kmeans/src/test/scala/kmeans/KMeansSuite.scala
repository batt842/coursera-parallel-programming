package kmeans

import java.util.concurrent._

import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._

import scala.collection.mutable.ArrayBuffer
import scala.math._

object KM extends KMeans
import KM._

@RunWith(classOf[JUnitRunner])
class KMeansSuite extends FunSuite {

  def checkClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points, means) == expected,
      s"classify($points, $means) should equal to $expected")
  }

  test("'classify should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected = GenMap[Point, GenSeq[Point]]()
    checkClassify(points, means, expected)
  }

  test("'classify' should work for empty 'points' and 'means' == GenSeq(Point(1,1,1))") {
    val points: GenSeq[Point] = IndexedSeq()
    val mean = new Point(1, 1, 1)
    val means: GenSeq[Point] = IndexedSeq(mean)
    val expected = GenMap[Point, GenSeq[Point]]((mean, GenSeq()))
    checkClassify(points, means, expected)
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((0, 0, 0))") {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean = new Point(0, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean)
    val expected = GenMap((mean, GenSeq(p1, p2, p3, p4)))
    checkClassify(points, means, expected)
  }

  test("'classify' should work for 'points' == GenSeq((1, 1, 0), (1, -1, 0), (-1, 1, 0), (-1, -1, 0)) and 'means' == GenSeq((1, 0, 0), (-1, 0, 0))") {
    val p1 = new Point(1, 1, 0)
    val p2 = new Point(1, -1, 0)
    val p3 = new Point(-1, 1, 0)
    val p4 = new Point(-1, -1, 0)
    val points: GenSeq[Point] = IndexedSeq(p1, p2, p3, p4)
    val mean1 = new Point(1, 0, 0)
    val mean2 = new Point(-1, 0, 0)
    val means: GenSeq[Point] = IndexedSeq(mean1, mean2)
    val expected = GenMap((mean1, GenSeq(p1, p2)), (mean2, GenSeq(p3, p4)))
    checkClassify(points, means, expected)
  }

  def checkParClassify(points: GenSeq[Point], means: GenSeq[Point], expected: GenMap[Point, GenSeq[Point]]) {
    assert(classify(points.par, means.par) == expected,
      s"classify($points par, $means par) should equal to $expected")
  }

  test("'classify with data parallelism should work for empty 'points' and empty 'means'") {
    val points: GenSeq[Point] = IndexedSeq()
    val means: GenSeq[Point] = IndexedSeq()
    val expected = GenMap[Point,GenSeq[Point]]()
    checkParClassify(points, means, expected)
  }

  test("for update") {
    val oldMeans = Vector(new Point(-9.0, 0.0, 0.0), new Point(9.0, 0.0, 0.0))
    val newMeans = update(
      Map(
        oldMeans(0) -> Vector(new Point(-10.0, 1.0, 0.0), new Point(-10.0, -1.0, 0.0)),
        oldMeans(1) -> Vector(new Point(10.0, 1.0, 0.0), new Point(10.0, -1.0, 0.0))),
      oldMeans)

    equalPointSeq(
      Vector(new Point(-10.0, 0.0, 0.0), new Point(10.0, 0.0, 0.0)),
      newMeans)
  }


  test("'kMeans' should work for 'points' == GenSeq((0, 0, 1), (0,0, -1), (0,1,0), (0,10,0)) and 'oldMeans' == GenSeq((0, -1, 0), (0, 2, 0)) and 'eta' == 12.25") {
    val means = kMeans(
      Vector(
        new Point(0.0, 0.0, 1.0),
        new Point(0.0, 0.0, -1.0),
        new Point(0.0, 1.0, 0.0),
        new Point(0.0, 10.0, 0.0)),
      Vector(
        new Point(0.0, -1.0, 0.0),
        new Point(0.0, 2.0, 0.0)),
      12.25)
    equalPointSeq(means, Vector(new Point(0.0, 0.0, 0.0), new Point(0.0, 5.5, 0.0)))
  }

  test("'converged' should work for 'oldMeans' == GenSeq((1,1,1), ..., (99,99,99)) and 'newMeans' == GenSeq((1,1,1), ..., (99, 99, 100))") {
    assert(converged(1)(
      ArrayBuffer(new Point(1.0, 1.0, 1.0), new Point(2.0, 2.0, 2.0), new Point(3.0, 3.0, 3.0), new Point(4.0, 4.0, 4.0), new Point(5.0, 5.0, 5.0), new Point(6.0, 6.0, 6.0), new Point(7.0, 7.0, 7.0), new Point(8.0, 8.0, 8.0), new Point(9.0, 9.0, 9.0), new Point(10.0, 10.0, 10.0), new Point(11.0, 11.0, 11.0), new Point(12.0, 12.0, 12.0), new Point(13.0, 13.0, 13.0), new Point(14.0, 14.0, 14.0), new Point(15.0, 15.0, 15.0), new Point(16.0, 16.0, 16.0), new Point(17.0, 17.0, 17.0), new Point(18.0, 18.0, 18.0), new Point(19.0, 19.0, 19.0), new Point(20.0, 20.0, 20.0), new Point(21.0, 21.0, 21.0), new Point(22.0, 22.0, 22.0), new Point(23.0, 23.0, 23.0), new Point(24.0, 24.0, 24.0), new Point(25.0, 25.0, 25.0), new Point(26.0, 26.0, 26.0), new Point(27.0, 27.0, 27.0), new Point(28.0, 28.0, 28.0), new Point(29.0, 29.0, 29.0), new Point(30.0, 30.0, 30.0), new Point(31.0, 31.0, 31.0), new Point(32.0, 32.0, 32.0), new Point(33.0, 33.0, 33.0), new Point(34.0, 34.0, 34.0), new Point(35.0, 35.0, 35.0), new Point(36.0, 36.0, 36.0), new Point(37.0, 37.0, 37.0), new Point(38.0, 38.0, 38.0), new Point(39.0, 39.0, 39.0), new Point(40.0, 40.0, 40.0), new Point(41.0, 41.0, 41.0), new Point(42.0, 42.0, 42.0), new Point(43.0, 43.0, 43.0), new Point(44.0, 44.0, 44.0), new Point(45.0, 45.0, 45.0), new Point(46.0, 46.0, 46.0), new Point(47.0, 47.0, 47.0), new Point(48.0, 48.0, 48.0), new Point(49.0, 49.0, 49.0), new Point(50.0, 50.0, 50.0), new Point(51.0, 51.0, 51.0), new Point(52.0, 52.0, 52.0), new Point(53.0, 53.0, 53.0), new Point(54.0, 54.0, 54.0), new Point(55.0, 55.0, 55.0), new Point(56.0, 56.0, 56.0), new Point(57.0, 57.0, 57.0), new Point(58.0, 58.0, 58.0), new Point(59.0, 59.0, 59.0), new Point(60.0, 60.0, 60.0), new Point(61.0, 61.0, 61.0), new Point(62.0, 62.0, 62.0), new Point(63.0, 63.0, 63.0), new Point(64.0, 64.0, 64.0), new Point(65.0, 65.0, 65.0), new Point(66.0, 66.0, 66.0), new Point(67.0, 67.0, 67.0), new Point(68.0, 68.0, 68.0), new Point(69.0, 69.0, 69.0), new Point(70.0, 70.0, 70.0), new Point(71.0, 71.0, 71.0), new Point(72.0, 72.0, 72.0), new Point(73.0, 73.0, 73.0), new Point(74.0, 74.0, 74.0), new Point(75.0, 75.0, 75.0), new Point(76.0, 76.0, 76.0), new Point(77.0, 77.0, 77.0), new Point(78.0, 78.0, 78.0), new Point(79.0, 79.0, 79.0), new Point(80.0, 80.0, 80.0), new Point(81.0, 81.0, 81.0), new Point(82.0, 82.0, 82.0), new Point(83.0, 83.0, 83.0), new Point(84.0, 84.0, 84.0), new Point(85.0, 85.0, 85.0), new Point(86.0, 86.0, 86.0), new Point(87.0, 87.0, 87.0), new Point(88.0, 88.0, 88.0), new Point(89.0, 89.0, 89.0), new Point(90.0, 90.0, 90.0), new Point(91.0, 91.0, 91.0), new Point(92.0, 92.0, 92.0), new Point(93.0, 93.0, 93.0), new Point(94.0, 94.0, 94.0), new Point(95.0, 95.0, 95.0), new Point(96.0, 96.0, 96.0), new Point(97.0, 97.0, 97.0), new Point(98.0, 98.0, 98.0), new Point(99.0, 99.0, 99.0)),
      ArrayBuffer(new Point(1.0, 1.0, 1.0), new Point(2.0, 2.0, 2.0), new Point(3.0, 3.0, 3.0), new Point(4.0, 4.0, 4.0), new Point(5.0, 5.0, 5.0), new Point(6.0, 6.0, 6.0), new Point(7.0, 7.0, 7.0), new Point(8.0, 8.0, 8.0), new Point(9.0, 9.0, 9.0), new Point(10.0, 10.0, 10.0), new Point(11.0, 11.0, 11.0), new Point(12.0, 12.0, 12.0), new Point(13.0, 13.0, 13.0), new Point(14.0, 14.0, 14.0), new Point(15.0, 15.0, 15.0), new Point(16.0, 16.0, 16.0), new Point(17.0, 17.0, 17.0), new Point(18.0, 18.0, 18.0), new Point(19.0, 19.0, 19.0), new Point(20.0, 20.0, 20.0), new Point(21.0, 21.0, 21.0), new Point(22.0, 22.0, 22.0), new Point(23.0, 23.0, 23.0), new Point(24.0, 24.0, 24.0), new Point(25.0, 25.0, 25.0), new Point(26.0, 26.0, 26.0), new Point(27.0, 27.0, 27.0), new Point(28.0, 28.0, 28.0), new Point(29.0, 29.0, 29.0), new Point(30.0, 30.0, 30.0), new Point(31.0, 31.0, 31.0), new Point(32.0, 32.0, 32.0), new Point(33.0, 33.0, 33.0), new Point(34.0, 34.0, 34.0), new Point(35.0, 35.0, 35.0), new Point(36.0, 36.0, 36.0), new Point(37.0, 37.0, 37.0), new Point(38.0, 38.0, 38.0), new Point(39.0, 39.0, 39.0), new Point(40.0, 40.0, 40.0), new Point(41.0, 41.0, 41.0), new Point(42.0, 42.0, 42.0), new Point(43.0, 43.0, 43.0), new Point(44.0, 44.0, 44.0), new Point(45.0, 45.0, 45.0), new Point(46.0, 46.0, 46.0), new Point(47.0, 47.0, 47.0), new Point(48.0, 48.0, 48.0), new Point(49.0, 49.0, 49.0), new Point(50.0, 50.0, 50.0), new Point(51.0, 51.0, 51.0), new Point(52.0, 52.0, 52.0), new Point(53.0, 53.0, 53.0), new Point(54.0, 54.0, 54.0), new Point(55.0, 55.0, 55.0), new Point(56.0, 56.0, 56.0), new Point(57.0, 57.0, 57.0), new Point(58.0, 58.0, 58.0), new Point(59.0, 59.0, 59.0), new Point(60.0, 60.0, 60.0), new Point(61.0, 61.0, 61.0), new Point(62.0, 62.0, 62.0), new Point(63.0, 63.0, 63.0), new Point(64.0, 64.0, 64.0), new Point(65.0, 65.0, 65.0), new Point(66.0, 66.0, 66.0), new Point(67.0, 67.0, 67.0), new Point(68.0, 68.0, 68.0), new Point(69.0, 69.0, 69.0), new Point(70.0, 70.0, 70.0), new Point(71.0, 71.0, 71.0), new Point(72.0, 72.0, 72.0), new Point(73.0, 73.0, 73.0), new Point(74.0, 74.0, 74.0), new Point(75.0, 75.0, 75.0), new Point(76.0, 76.0, 76.0), new Point(77.0, 77.0, 77.0), new Point(78.0, 78.0, 78.0), new Point(79.0, 79.0, 79.0), new Point(80.0, 80.0, 80.0), new Point(81.0, 81.0, 81.0), new Point(82.0, 82.0, 82.0), new Point(83.0, 83.0, 83.0), new Point(84.0, 84.0, 84.0), new Point(85.0, 85.0, 85.0), new Point(86.0, 86.0, 86.0), new Point(87.0, 87.0, 87.0), new Point(88.0, 88.0, 88.0), new Point(89.0, 89.0, 89.0), new Point(90.0, 90.0, 90.0), new Point(91.0, 91.0, 91.0), new Point(92.0, 92.0, 92.0), new Point(93.0, 93.0, 93.0), new Point(94.0, 94.0, 94.0), new Point(95.0, 95.0, 95.0), new Point(96.0, 96.0, 96.0), new Point(97.0, 97.0, 97.0), new Point(98.0, 98.0, 98.0), new Point(99.0, 99.0, 100.0))),
      "wrong")
  }

  def equalPointSeq(newMeans: GenSeq[Point], oldMeans: GenSeq[Point]): Unit = {
    var i = 0
    while (i < newMeans.length) {
      assert(newMeans(i).x === oldMeans(i).x, "wrong")
      assert(newMeans(i).y === oldMeans(i).y, "wrong")
      assert(newMeans(i).z === oldMeans(i).z, "wrong")
      i += 1
    }
  }
}


  
