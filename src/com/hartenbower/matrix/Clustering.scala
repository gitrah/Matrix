package com.hartenbower.matrix
import Util.Concurrent
object Clustering {
  val rnd = new java.util.Random(System.currentTimeMillis())

  implicit def scalarOp(d: Double) = new ScalarOp(d)

  class ScalarOp(d: Double) {
    def +(a: Array[Double]): Array[Double] = {
      val l = a.length
      val o = new Array[Double](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) + d
        i -= 1
      }
      o
    }
    def -(a: Array[Double]): Array[Double] = {
      val l = a.length
      val o = new Array[Double](l)
      var i = l - 1
      while (i > -1) {
        o(i) = d - a(i)
        i -= 1
      }
      o
    }
    def /(a: Array[Double]): Array[Double] = {
      val l = a.length
      val o = new Array[Double](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) / d
        i -= 1
      }
      o
    }
    def *(a: Array[Double]): Array[Double] = {
      val l = a.length
      val o = new Array[Double](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) * d
        i -= 1
      }
      o
    }
  }

  implicit def arrayOp(a: Array[Double]) = new ArrayOp(a)

  class ArrayOp(a: Array[Double]) {
    def +(oa: Array[Double]): Array[Double] = {
      val l = a.length
      require(oa.length == l, "arrays of unequal length")
      val o = new Array[Double](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) + oa(i)
        i -= 1
      }
      o
    }
    def +(d: Double): Array[Double] = {
      val l = a.length
      val o = new Array[Double](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) + d
        i -= 1
      }
      o
    }
    def -(oa: Array[Double]): Array[Double] = {
      val l = a.length
      require(oa.length == l, "arrays of unequal length")
      val o = new Array[Double](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) - oa(i)
        i -= 1
      }
      o
    }

    def -(d: Double): Array[Double] = {
      val l = a.length
      val o = new Array[Double](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) - d
        i -= 1
      }
      o
    }

    def /(d: Double): Array[Double] = {
      val l = a.length
      val o = new Array[Double](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) / d
        i -= 1
      }
      o
    }

    def *(d: Double): Array[Double] = {
      val l = a.length
      val o = new Array[Double](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) * d
        i -= 1
      }
      o
    }

    def dot(oa: Array[Double]): Double = {
      val l = a.length
      require(oa.length == l, "arrays of unequal length")
      var s = 0d
      var i = l - 1
      while (i > -1) {
        s += a(i) * oa(i)
        i -= 1
      }
      s
    }
  }

  def lengthSquared(v: Array[Double]): Double = {
    var d = 0d
    var vi = 0d
    var i = v.length - 1
    while (i > -1) {
      vi = v(i)
      d += vi * vi
      i -= 1
    }
    d
  }

  def lengthSquared2(v: Array[Double]): Double = {
    var d = 0d
    var i = v.length - 1
    while (i > -1) {
      d += v(i) * v(i)
      i -= 1
    }
    d
  }

  def minMax(x: Array[Array[Double]]): Array[Array[Double]] = {
    val mm = new Array[Array[Double]](2)
    val innerL = x(0).length
    var min = x(0)
    var minL = 0d
    var maxL = 0d
    var curr = 0d
    var currL = 0d
    var max = x(0)
    val outerL = x.length
    var i = 0
    var j = 0
    while (i < outerL) {
      j = 0
      currL = 0
      while (j < innerL) {
        curr = x(i)(j)
        currL += curr * curr
        j += 1
      }
      if (currL > maxL) {
        max = x(i)
        maxL = currL
      } else if (currL < minL) {
        min = x(i)
        minL = currL
      }
      i += 1
    }
    Array(min, max)
  }

  def unitV(v: Array[Double]): (Double, Array[Double]) = {
    val ov = v.clone
    val l = math.sqrt(lengthSquared(v))
    var i = ov.length - 1
    while (i > -1) {
      ov(i) /= l
      i -= 1
    }
    (l, ov)
  }

  def randTerb(v: Array[Double], epsilon: Double): Array[Double] = {
    val ov = v.clone
    var i = ov.length - 1
    while (i > -1) {
      ov(i) += (2 * rnd.nextDouble - 1) * epsilon
      i -= 1
    }
    ov
  }

  def assignment(centroids: Array[Array[Double]], v: Array[Double]): (Double, Int) = {
    var i = centroids.length - 1
    var minLsqr = -5d
    var currL = 0d
    var idx = -1
    var vc: Array[Double] = null
    while (i > -1) {
      vc = centroids(i) - v
      currL = lengthSquared(centroids(i) - v)
      if (currL < minLsqr || minLsqr < 0) {
        minLsqr = currL
        idx = i
      }
      i -= 1
    }
    (minLsqr, idx)
  }

  def assignChunk(x: Array[Array[Double]], assignments: Array[(Double, Int)], cents: Array[Array[Double]])(range: (Long, Long))() = {
    // println("chunk " + range)
    var li = range._1
    var i = 0
    while (li <= range._2) {
      i = li.asInstanceOf[Int]
      assignments(i) = assignment(cents, x(i))
      li += 1
    }
    li
  }

  def newCentroids(x: Array[Array[Double]], assignments: Array[(Double, Int)], centroids: Int, cents: Array[Array[Double]]): Double = {
    val xl = x.length
    val counts = new Array[Int](centroids)
    var i = xl - 1
    var idx = 0
    var cost = 0d
    var tup: Tuple2[Double, Int] = null
    while (i > -1) {
      tup = assignments(i)
      idx = tup._2
      cost += tup._1
      cents(idx) = cents(idx) + x(i)
      counts(idx) += 1
      i -= 1
    }
    cost /= xl
    i = 0
    while (i < centroids) {
      cents(i) = cents(i) / (1d * counts(i))
      i += 1
    }
    cost // of previous centroids
  }

  def moveChunk(x: Array[Array[Double]],
    assignments: Array[(Double, Int)], cents: Array[Array[Double]],
    counts: Array[Int])(range: Tuple2[Long, Long])() = {
    // println("range " + range)
    var li = range._2
    var idx = 0
    var i = 0
    var tup: Tuple2[Double, Int] = null
    var cost = 0d
    val centL = x(0).length
    while (li >= range._1) {
      i = li.asInstanceOf[Int]
      tup = assignments(i)
      idx = tup._2
      cost += tup._1
      cents(idx) = cents(idx) + x(i.asInstanceOf[Int])
      counts(idx) += 1
      li -= 1
    }
    cost
  }

  def avgChunk(cents: Array[Array[Double]], counts: Array[Int])(range: (Long, Long))() = {
    var li = range._1
    var i = 0
    while (li <= range._2) {
      i = li.asInstanceOf[Int]
      cents(i) = cents(i) / (1d * counts(i))
      li += 1
    }
  }

  def newCentroidsDc(x: Array[Array[Double]], assignments: Array[(Double, Int)], centroids: Int, cents: Array[Array[Double]]) = {
    val xl = x.length
    val counts = new Array[Int](centroids)
    var i = xl - 1
    var idx = 0
    Concurrent.combine(Concurrent.distribute(x.length, moveChunk(x, assignments, cents, counts)))
    Concurrent.combine(Concurrent.distribute(centroids, avgChunk(cents, counts)))
  }

  def kMeans(x: Array[Array[Double]], centroids: Int, threshold: Double) = {
    val xl = x.length
    val assignments = new Array[(Double, Int)](xl)
    val cents = new Array[Array[Double]](centroids)
    val newCents = new Array[Array[Double]](centroids)
    var i = 0

    val mm = minMax(x)
    var v1 = mm(0)
    val (v2l, uv2) = unitV(mm(1) - mm(0))
    val epsilon = v2l * rnd.nextDouble()
    var delta = 0d
    while (i < cents.length) {
      cents(i) = randTerb(v1 + rnd.nextDouble() * uv2, epsilon)
      i += 1
    }

    do {
      i = 0
      while (i < xl) {
        assignments(i) = assignment(cents, x(i))
        i += 1
      }
      newCentroids(x, assignments, centroids, newCents)

      i = 0
      delta = 0
      while (i < centroids) {
        delta += lengthSquared(newCents(i) - cents(i))
        i += 1
      }
      Array.copy(newCents, 0, cents, 0, centroids)
    } while (delta < threshold)
    newCents
  }

  def zero(a: Array[Array[Double]], n: Int) {
    val xl = a.length
    var i = xl - 1
    var j = 0
    while (i > -1) {
      j = 0
      while (j < n) {
        a(i)(j) = 0d
        j += 1
      }
      i -= 1
    }
  }

  def copy(s: Array[Array[Double]], t: Array[Array[Double]], n: Int) {
    val xl = s.length
    var i = xl - 1
    var j = 0
    while (i > -1) {
      j = 0
      while (j < n) {
        t(i)(j) = s(i)(j)
        j += 1
      }
      i -= 1
    }
  }

  def distortion(centroids: Int, assignments: Array[(Double, Int)]): Double = {
    val counts = Array.fill(centroids)(0)
    val dists = Array.fill(centroids)(0d)
    var i = assignments.length - 1
    var idx = -1
    var tup: (Double, Int) = null
    while (i > -1) {
      tup = assignments(i)
      counts(tup._2) += 1
      dists(tup._2) += tup._1
      i -= 1
    }
    i = 0
    var total = 0d
    while (i < centroids) {
      total += dists(i) / counts(i)
      i += 1
    }
    total
  }

  def kMeansDc(x: Array[Array[Double]], centroids: Int) = {
    val xl = x.length
    val n = x(0).length
    val assignments = new Array[(Double, Int)](xl)
    val cents = Array.fill(centroids, n)(0d)
    val newCents = Array.fill(centroids, n)(0d)
    var i = 0
    val mm = minMax(x)
    //println("minMaxed " + mm.deep.mkString)
    var v1 = mm(0)

    val (v2l, uv2) = unitV(mm(1) - mm(0))
    val epsilon = v2l * rnd.nextDouble()

    var iter = 0

    def randInitCentroids = {
      var idx = 0
      while (idx < cents.length) {
        cents(idx) = randTerb(v1 + rnd.nextDouble() * v2l * uv2, epsilon)
        idx += 1
      }
    }

    randInitCentroids

    var lastDistortion = 0d
    var currDistortion = 0d
    do {
      lastDistortion = currDistortion
      do {
        // make assignments; check for centroids with zero assignments and re-init if so
        Concurrent.combine(Concurrent.distribute(xl, assignChunk(x, assignments, cents)))
        currDistortion = distortion(centroids, assignments)
        if (currDistortion.isNaN()) {
          println("got a bad centroid, re-randomizing")
          randInitCentroids
        }
      } while (currDistortion.isNaN())
      zero(newCents, n)
      // update centroids
      newCentroidsDc(x, assignments, centroids, newCents)
      iter += 1
      copy(newCents, cents, n)
    } while (lastDistortion != currDistortion)
    println("iter " + iter + " cost " + currDistortion)
    (currDistortion, newCents)
  }

  def kMeansTrials(x: Array[Array[Double]], centroids: Int, iterations: Int, stopAfterXrepetitions: Int = 5) = {
    var i = iterations
    var res: (Double, Array[Array[Double]]) = null
    var repeats = 0
    while (i > 0 && repeats < stopAfterXrepetitions) {
      val curr = kMeansDc(x, centroids)
      if (res == null || res._1 > curr._1) {
        res = curr
      } else if (res._1 == curr._1) {
        repeats += 1
      }
      i -= 1
    }
    if (repeats == stopAfterXrepetitions) {
      println("halted at " + i + " after same distortion repeated " + repeats + " times")
    }
    res
  }
}