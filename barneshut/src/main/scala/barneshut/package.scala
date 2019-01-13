import common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad

    def bounded(b: Body): Boolean = {
      isBounded(b.x, b.y, size, centerX, centerY)
    }
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX

    def massY: Float = centerY

    def mass: Float = 0f

    def total: Int = 0

    def insert(b: Body): Quad = {
      //      if (!bounded(b))
      //        throw new IllegalArgumentException
      // 이거 없는게 맞나...

      Leaf(centerX, centerY, size, Seq(b))
    }
  }

  case class Fork(
                   nw: Quad, ne: Quad, sw: Quad, se: Quad
                 ) extends Quad {
    val quads = Vector(nw, ne, sw, se)
    val sums = quads
      .map(q => (q.mass, q.mass * q.massX, q.mass * q.massY, q.total))
      .foldLeft((0f, 0f, 0f, 0))((acc, v) => (
        acc._1 + v._1,
        acc._2 + v._2,
        acc._3 + v._3,
        acc._4 + v._4))

    val centerX: Float = nw.centerX + (nw.size / 2)
    val centerY: Float = nw.centerY + (nw.size / 2)
    val size: Float = nw.size * 2
    val mass: Float = sums._1
    val massX: Float = if (mass == 0) centerX else sums._2 / mass
    val massY: Float = if (mass == 0) centerY else sums._3 / mass
    val total: Int = sums._4

    def insert(b: Body): Fork = {
      val west = b.x < centerX
      val north = b.y < centerY
      val oldQuad = (north, west) match {
        case (true, true) => nw
        case (true, false) => ne
        case (false, true) => sw
        case (false, false) => se
      }
      val newQuad = oldQuad.insert(b)
      val newQuads = quads.map(q => if (q == oldQuad) newQuad else q)
      Fork(newQuads(0), newQuads(1), newQuads(2), newQuads(3))
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
    extends Quad {
    val sums = bodies
      .map(b => (b.mass, b.mass * b.x, b.mass * b.y))
      .foldLeft((0f, 0f, 0f))((acc, v) => (
        acc._1 + v._1,
        acc._2 + v._2,
        acc._3 + v._3))

    val (mass, massX, massY) = (sums._1, sums._2 / sums._1, sums._3 / sums._1)
    val total: Int = bodies.length

    def insert(b: Body): Quad = {
      val newBodies = bodies :+ b
      if (size > minimumSize) {
        val half = size / 2
        val quarter = size / 4
        val centers = Vector(
          (centerX - quarter, centerY - quarter), // nw
          (centerX + quarter, centerY - quarter), // ne
          (centerX - quarter, centerY + quarter), // sw
          (centerX + quarter, centerY + quarter)) // se
        val quads = centers.map(c => Empty(c._1, c._2, half))
        newBodies.foldLeft(Fork(quads(0), quads(1), quads(2), quads(3)))((f, b) => f insert b)
      } else {
        Leaf(centerX, centerY, size, newBodies)
      }
    }
  }

  def isBounded(x: Float, y: Float, size: Float, centerX: Float, centerY: Float): Boolean = {
    val halfSize = size / 2f
    val (left, right, top, bottom) = (
      centerX - halfSize,
      centerX + halfSize,
      centerY - halfSize,
      centerY + halfSize)
    isBounded(x, y, left, top, right, bottom)
  }

  def isBounded(x: Float, y: Float, left: Float, top: Float, right: Float, bottom: Float): Boolean = {
    x >= left && x < right && y >= top && y < bottom
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
        // no force
        case Leaf(_, _, _, bodies) =>
          // add force contribution of each body by calling addForce
          bodies.foreach(b => addForce(b.mass, b.x, b.y))
        case Fork(nw, ne, sw, se) =>
          // see if node is far enough from the body,
          // or recursion is needed
          if (quad.size / distance(x, y, quad.massX, quad.massY) < theta)
            addForce(quad.mass, quad.massX, quad.massY)
          else
            Vector(nw, ne, sw, se).foreach(traverse(_))
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      // 뭔 소리지?
      // Importantly, if the Body lies outside of the Boundaries,
      // it should be considered to be located at the closest point within the Boundaries
      // for the purpose of finding which ConcBuffer should hold the body.
      def coordToIndex(v: Float, base: Float): Int = {
        val xf = (v - base) / sectorSize
        if (xf < 0) 0 else if (xf >= sectorPrecision) sectorPrecision - 1 else xf.toInt
      }

      this (coordToIndex(b.x, boundaries.minX), coordToIndex(b.y, boundaries.minY)) += b
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      val combined = new SectorMatrix(boundaries, sectorPrecision)
      for (i <- 0 until matrix.length) combined.matrix(i) = matrix(i).combine(that.matrix(i))
      combined
      // ??????
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4

      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this (x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: => T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString ("\n")
    }
  }

}
