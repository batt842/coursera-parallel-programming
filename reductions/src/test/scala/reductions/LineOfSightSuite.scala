package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }

  test("upsweep should correctly handle the chunk 1 until 12 of an array of 12 elements") {
    val res = upsweep(Array[Float](0f, 1f, 2f, 3f, 5f, 3f, 9f, 5f, 10f, 4f, 6f, 5f), 1, 12, 2)
    assert(res.maxPrevious == 1.5f)
  }

  test("upsweep should correctly compute the tree on the indices 1 until 5 of a 5 element array for threshold 1") {
    val res = upsweep(Array[Float](0f, 1f, 8f, 9f, 10f), 1, 5, 1)
    assert(res.maxPrevious == 4f)
  }

  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweep should correctly handle a 12 element array when the starting angle is zero") {
    val input = Array[Float](0f, 1f, 2f, 3f, 5f, 3f, 9f, 5f, 10f, 4f, 6f, 5f)
    val inputTree = upsweep(input, 1, 12, 2)
    val output = new Array[Float](12)
    downsweep(input, output, 0f, inputTree)
    assert(output.toList == List(0.0f, 1.0f, 1.0f, 1.0f, 1.25f, 1.25f, 1.5f, 1.5f, 1.5f, 1.5f, 1.5f, 1.5f))
  }

  test("downsweep should correctly handle trees with a single leaf") {
    val input = Array[Float](0f, 1f, 2f, 3f, 5f, 3f, 9f, 5f, 10f, 4f, 6f, 5f)
    val inputTree = upsweep(input, 1, 12, 11)
    val output = new Array[Float](12)
    downsweep(input, output, 0f, inputTree)
    assert(output.toList == List(0.0f, 1.0f, 1.0f, 1.0f, 1.25f, 1.25f, 1.5f, 1.5f, 1.5f, 1.5f, 1.5f, 1.5f))
  }

  test("parLineOfSight should correctly handle a 12 element array") {
    val input = Array[Float](0f, 1f, 2f, 3f, 5f, 3f, 9f, 5f, 10f, 4f, 6f, 5f)
    val output = new Array[Float](12)
    parLineOfSight(input, output, 2)
    assert(output.toList == List(0.0f, 1.0f, 1.0f, 1.0f, 1.25f, 1.25f, 1.5f, 1.5f, 1.5f, 1.5f, 1.5f, 1.5f))
  }
}

