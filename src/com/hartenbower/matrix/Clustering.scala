package com.hartenbower.matrix
import Util.Concurrent
import Util.Math._
import java.util.concurrent.Future
import java.util.concurrent.CompletionService
object Clustering {
  val rnd = new java.util.Random(System.currentTimeMillis())

  var useDc = true

  def assignment(centroids: Array[Array[Double]], v: Array[Double]): (Double, Int) = {
    var i = centroids.length - 1
    var minLsqr = -5d
    var currL = 0d
    var idx = -1
    while (i > -1) {
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
    var li = range._1
    var i = 0
    while (li <= range._2) {
      i = li.asInstanceOf[Int]
      assignments(i) = assignment(cents, x(i))
      li += 1
    }
    li
  }

  /**
 * @param x array of samples * features
 * @param assignments array of samples index * (length to, nearest centroid)
 * @param centroids count of centroids
 * @param cents array of centroids * features
 * @return cost (distortion) from previous centroids
 */
def newCentroids(x: Array[Array[Double]], assignments: Array[(Double, Int)], centroids: Int, cents: Array[Array[Double]]): Double = {
    val xl = x.length
    val counts = new Array[Int](centroids)
    var i = xl - 1
    var idx = 0
    var cost = 0d
    var tup: Tuple2[Double, Int] = null
    zero(cents, cents(0).length)
    while (i > -1) {
      tup = assignments(i)
      idx = tup._2
      cost += tup._1
      cents(idx) = cents(idx) + x(i)  // should implement += for more clarity?
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

  /**
 * @param x array of samples * features
 * @param assignments array of samples index * (length to, nearest centroid)
 * @param centroids count of centroids
 * @param features count of features
 * @param range range of indices into x
 * @return Tuple of ( Tuple of (Array of cenroids * features, Array of centroid * counts ), cost of previous centroids)
 */
def moveChunk(x: Array[Array[Double]],
    assignments: Array[(Double, Int)], 
    centroids: Int,
    features: Int)(range: Tuple2[Long, Long])() = {
    var li = range._2
    var idx = 0
    var i = 0
    var tup: Tuple2[Double, Int] = null
    var cost = 0d
    val centL = x(0).length
    val centCounts = (new Array[Array[Double]](centroids), new Array[Int](centroids))
    var cent : Array[Double] = null
    while (li >= range._1) {
      i = li.asInstanceOf[Int]
      tup = assignments(i)
      idx = tup._2
      cost += tup._1
      
      if(centCounts._1(idx) == null) {
        centCounts._1(idx) = new Array[Double](features)
      }
      centCounts._1(idx) = centCounts._1(idx) + x(i)
      centCounts._2(idx) += 1
      li -= 1
    }
    (centCounts,cost)
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
   def aggregateCentroidsD(cents: Array[Array[Double]], counts: Array[Int], efforts: ( Array[Future[( (Array[Array[Double]],Array[Int]), Double)]], CompletionService[( (Array[Array[Double]],Array[Int]), Double)])) : Double = {
      var i = 0
      var j = 0
      val centroids = cents.length
      var totalCost = 0d
      var curr : ( (Array[Array[Double]], Array[Int]), Double) = null
      var currCents : Array[Array[Double]] = null
      var currCounts : Array[Int] = null
      
      while (i < efforts._1.length) {
        curr = efforts._2.take.get
        currCents = curr._1._1
        currCounts = curr._1._2
        j = 0
        while(j < centroids) {
          if(currCents(j) != null) {
        	  cents(j) = cents(j) + currCents(j)
          } 
          counts(j) = counts(j) + currCounts(j)
          j += 1
        }
        totalCost += curr._2
        i += 1
      }
      j = 0
      while(j < centroids) {
        j+=1
      }
      totalCost
    }


  def newCentroidsDc(x: Array[Array[Double]], assignments: Array[(Double, Int)], centroids: Int, cents: Array[Array[Double]]) = {
    val xl = x.length
    val counts = new Array[Int](centroids)
    var i = xl - 1
    var idx = 0
    val totalCost = aggregateCentroidsD(cents, counts, Concurrent.distribute(x.length, moveChunk(x, assignments, cents.length, cents(0).length )))
    Concurrent.combine(Concurrent.distribute(centroids, avgChunk(cents, counts)))
    totalCost
  }
  
  

  def findNearestChunk(x: Array[Array[Double]], cents: Array[Array[Double]], idxs: Array[Int])(range: (Long, Long))() = {
    var i = range._1.asInstanceOf[Int]
    val end = range._2.asInstanceOf[Int]
    val xl = x.length
    val centsl = cents.length
    var j = 0
    var currDist = 0d
    var minDist = Double.MaxValue
    var currCent: Array[Double] = null
    while (i <= end) {
      j = 0
      minDist = Double.MaxValue
      idxs(i) = -1
      while (j < centsl) {
        currDist = Util.Math.sumSquaredDiffs(x(i), cents(j))
        if (currDist < minDist) {
          idxs(i) = j
          minDist = currDist
        }
        j += 1
      }
      i += 1
    }
    i
  }

  /**
   * @param x array of samples, m by n
   * @param cents array of centroids, k by n
   * @return array of indices, m by 1, for each sample, the index of the nearest centroid
   */
  def findNearestIdxDc(x: Array[Array[Double]], cents: Array[Array[Double]]): Array[Int] = {
    val ret = new Array[Int](x.length)
    Concurrent.combine(Concurrent.distribute(x.length, findNearestChunk(x, cents, ret)))
    ret
  }

  def randomCentroids(x: Array[Array[Double]], centroids: Int): Array[Array[Double]] = {
    val cents = new Array[Array[Double]](centroids)
    val mm = minMaxRow(x)
    var v1 = mm(0)
    val (v2l, uv2) = unitV(mm(1) - mm(0))
    val epsilon = v2l * rnd.nextDouble()
    var i = 0
    while (i < cents.length) {
      cents(i) = randTerb(v1 + rnd.nextDouble() * uv2, epsilon)
      i += 1
    }
    cents
  }

  def kMeans(x: Array[Array[Double]], cents: Array[Array[Double]], threshold: Double) = {
    val xl = x.length
    val centroidCount = cents.length
    val n = cents(0).length
    val assignments = new Array[(Double, Int)](xl)
    val newCents = new Array[Array[Double]](centroidCount)
    val centsCopy = cents.clone
    var i = 0
    while (i < cents.length) {
      newCents(i) = new Array[Double](n)
      i += 1
    }
    var delta = 0d

    do {
      i = 0
      while (i < xl) {
        assignments(i) = assignment(centsCopy, x(i))
        i += 1
      }
      newCentroids(x, assignments, centroidCount, newCents)

      i = 0
      delta = 0
      while (i < centroidCount) {
        delta += lengthSquared(newCents(i) - centsCopy(i))
        i += 1
      }
      Array.copy(newCents, 0, centsCopy, 0, centroidCount)
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

  def distortionChunk(centroids: Int, assignments: Array[(Double, Int)])(range: (Long, Long))() = {
    val counts = Array.fill(centroids)(0)
    val dists = Array.fill(centroids)(0d)
    var i = range._1.asInstanceOf[Int]
    val end = range._2.asInstanceOf[Int]

    var idx = -1
    var tup: (Double, Int) = null
    while (i <= end) {
      tup = assignments(i)
      counts(tup._2) += 1
      dists(tup._2) += tup._1
      i += 1
    }
    (counts, dists)
  }

  def distortionDc(centroids: Int, assignments: Array[(Double, Int)]): Double = {
    import Util.Math._
    var counts = Array.fill(centroids)(0)
    var dists = Array.fill(centroids)(0d)
    val futs = Concurrent.distribute(assignments.length, distortionChunk(centroids, assignments))
    var i = 0
    val l = futs._1.length
    var f: Future[(Array[Int], Array[Double])] = null
    while (i < l) {
      f = futs._2.take
      counts = counts + f.get._1
      dists = dists + f.get._2
      i += 1
    }
    i = 0
    var total = 0d
    while (i < centroids) {
      total += dists(i) / counts(i)
      i += 1
    }
    total
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

  def kMeansDc(x: Array[Array[Double]], _cents: Array[Array[Double]]) = {
    val xl = x.length
    val centroids = _cents.length
    val n = x(0).length
    val assignments = new Array[(Double, Int)](xl)
    val cents = _cents.clone()
    val newCents = Array.fill(centroids, n)(0d)
    var i = 0
    val mm = minMaxRowDc(x)
    println("minMaxed " + mm.deep.mkString)
    println("cents " + cents.deep.mkString)
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
        if (iter % 100 == 0) print(".")
        if (iter % 500 == 0) new Thread {
          override def run {print("*")}
          
        }.start
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
    var cents = randomCentroids(x, centroids)
    println("cents\n" + cents.deep.mkString)
    while (i > 0 && repeats < stopAfterXrepetitions) {
      val curr = kMeansDc(x, cents)
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