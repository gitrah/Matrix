package com.hartenbower.matrix
import scala.util.Random
import java.io._
import scala.io._
import java.util.concurrent._
import com.hartenbower.util.TimingUtil
import scala.reflect.runtime.universe._
import scala.reflect.{ClassTag, classTag}
import scala.math.ScalaNumericConversions

object Util {
  val rnd = new java.util.Random(System.currentTimeMillis())
  var verbose = false

  object Concurrent {

    val defaultSpanThreshold = 1
    var threadCount = 2 * Runtime.getRuntime.availableProcessors
    println("starting with a " + threadCount + "-thread pool")
    val pool = Executors.newFixedThreadPool(threadCount)

    class FutureIsNow[T](v: T) extends Future[T] {
      def cancel(b: Boolean): Boolean = false
      def isCancelled = false
      def isDone = true
      val get = v
      def get(timeout: Long, unit: TimeUnit) = v
    }

    class Exister[T](f: () => T) extends Callable[T] {
      def call(): T = f()
    }

    def effort[T](f: () => T, cs : CompletionService[T]): Future[T] = {
      val ex = new Exister(f)
      val ft = new FutureTask[T](ex)
      if (verbose) println("making an effort")
      cs.submit(ex);
      ft
    }

    def distribute[T](elementCount: Long, f: (Tuple2[Long, Long]) => () => T, oneBased: Boolean = false) = {
      var i = 0
      val spans = toSpans(elementCount, threadCount, oneBased)
      if (verbose) println("spans " + spans.mkString)
      val efforts = new Array[Future[T]](spans.length)
      val cs = new ExecutorCompletionService[T](pool);
      while (i < spans.length) {
        efforts(i) = effort(f(spans(i)), cs)
        i += 1
      }
      (efforts,cs)
    }

    def combine[T](efforts: (Array[Future[T]], CompletionService[T])) {
      var i = 0
      while (i < efforts._1.length) {
        efforts._2.take.get // blocks if none
        if (verbose) println("got " + i)
        i += 1
      }
    }

    def aggregate[T](efforts: (Array[Future[T]],CompletionService[T]))(implicit n : Numeric[T]): T = {
      import n.mkNumericOps
      var i = 0
      var s = n.zero
      while (i < efforts._1.length) {
         s = s + efforts._2.take.get
        i += 1
      }
      s
    }

    def aggregateL[X](efforts: (Array[Future[ List[X]]],CompletionService[ List[X]])): List[X] = {
      var i = 0
      var l = List[X]()
      while (i < efforts._1.length) {
         l = l ++ efforts._2.take.get
         i += 1
      }
      l
    }

    def aggregateDA(n: Int, efforts: (Array[Future[Array[Double]]], CompletionService[Array[Double]])): Array[Double] = {

      def arrayPlus(a: Array[Double], oa: Array[Double]): Array[Double] = {
        val l = a.length
        require(oa.length == l, "arrays of unequal length (" + oa.length + " != " + l)

        val o = new Array[Double](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) + oa(i)
          i -= 1
        }
        o
      }

      var i = 0
      var s = new Array[Double](n)

      // can't just say s = s + efforts(i).get because array math stuff hasn't been declared 
      // and can't move Concurrent stuff after it because of dependencies 
      while (i < efforts._1.length) {
        s = arrayPlus(s, efforts._2.take.get)
        i += 1
      }
      s
    }

    def toSpans(elementCount: Long, d: Int, oneBased: Boolean = false, threshold: Int = defaultSpanThreshold): Array[Tuple2[Long, Long]] = {
      val span = elementCount / d
      if (span >= threshold) {
        val size = math.min(elementCount, d).asInstanceOf[Int]
        val ret = new Array[Tuple2[Long, Long]](size)
        var i = 0
        while (i < size - 1) {
          if (span > 0) {
            ret(i) = if (oneBased) (i * span + 1, (i + 1) * span) else (i * span, (i + 1) * span - 1)
          } else {
            ret(i) = if (oneBased) (i + 1, i + 1) else (i, i)
          }
          i += 1
        }
        if (span > 0) {
          ret(size - 1) = if (oneBased) ((d - 1) * span + 1, elementCount) else ((d - 1) * span, elementCount - 1)
        } else {
          ret(size - 1) = if (oneBased) (elementCount, elementCount) else (elementCount - 1, elementCount - 1)
        }
        ret
      } else {
        Array((if (oneBased) 1l else 0l, if (oneBased) elementCount else elementCount - 1))
      }
    }
  }

  object Timing {
    def elapsed(msg: String, l: Long) = {
      val now = System.currentTimeMillis
      println(msg + " took " + (now - l) + " millis")
      (now, now - l)
    }

    def time(msg: String, f: => Unit, count: Int = 1, ticker: Boolean = false): Tuple2[Int, Long] = {
      val l = System.currentTimeMillis
      var idx = count
      while (idx > 0) {
        f
        if (ticker) print(".")
        idx -= 1
      }
      val delta = (System.currentTimeMillis - l)
      println("\n" + msg + " took " + TimingUtil.fromMillis(delta) + " or " + (count * 1000.0/ delta) + "evals/s")
      (count, delta)
    }
  }


  object Math {
    def nextPowerOf2(n: Int) = {
      var x = n - 1
      x |= x >> 1
      x |= x >> 2
      x |= x >> 4
      x |= x >> 8
      x |= x >> 16
      x + 1
    }
    def aboutEq(x: Double, y: Double, eps: Double = 1e-3) = math.abs(y - x) <= eps
    def aboutEqF(x: Float, y: Float, eps: Float = .001f) = math.abs(y - x) <= eps
    def aboutEqt(x: (Double, Double), y: (Double, Double), eps: Double = 1e-3) = aboutEq(x._1, y._1, eps) && aboutEq(x._2, y._2, eps)
    def aboutEqa(x: Array[Double], y: Array[Double], eps: Double = 1e-3) = {
      if (x.length == y.length) {
        !(x.toList.zip(y.toList)).exists(x => !aboutEq(x._1, x._2, eps))
      } else {
        false
      }
    }
  
   trait NumberLike[T] {
      def plus(x: T, y: T): T
      def divide(x: T, y: T): T
      def minus(x: T, y: T): T
      def sqrt(x: T): T
    }
   
    object NumberLike {
      implicit object NumberLikeDouble extends NumberLike[Double] {
        def plus(x: Double, y: Double): Double = x + y
        def divide(x: Double, y: Double): Double = x / y
        def minus(x: Double, y: Double): Double = x - y
        def sqrt(x:Double) : Double = math.sqrt(x)
      }
      implicit object NumberLikeFloat extends NumberLike[Float] {
        def plus(x: Float, y: Float): Float = x + y
        def divide(x: Float, y: Float): Float = x / y
        def minus(x: Float, y: Float): Float = x - y
        def sqrt(x:Float) : Float = math.sqrt(x).toFloat
      }
      implicit object NumberLikeInt extends NumberLike[Int] {
        def plus(x: Int, y: Int): Int = x + y
        def divide(x: Int, y: Int): Int = x / y
        def minus(x: Int, y: Int): Int = x - y
        def sqrt(x:Int) : Int = math.sqrt(x).toInt
      }
    }
    
    def chunkOp[T](a : Array[T], chop : ( T, T ) => T)( range: (Long,Long))()(implicit n: Numeric[T]) = {
      var i = range._1.asInstanceOf[Int]
      val end = range._2.asInstanceOf[Int]
      var s = n.zero
      while (i <= end) {
        s = chop( s,  a(i) ) 
        i += 1
      }
      s
    }
    
    def biChunkOp[T](a : Array[T], b : Array[T], chop : ( T, T ) => T)( range: (Long,Long))()(implicit n: Numeric[T]) = {
      import n.mkNumericOps
       var i = range._1.asInstanceOf[Int]
      val end = range._2.asInstanceOf[Int]
      var s = n.zero
      while (i <= end) {
        s =  s + chop( a(i), b(i)) 
        i += 1
      }
      s
    }
    
    def sum(a: Array[Double]): Double = {
      var i = a.length - 1
      var s = 0d
      while (i > -1) {
        s += a(i)
        i -= 1
      }
      s
    }

    def transposeDot(a: Array[Double]): Array[Double] = {
      val l = a.length
      val out = new Array[Double](l * l)
      var rows = 0
      var cols = 0
      var s = 0d
      // iterator over diag and upper tri
      while (rows < l) {
        cols = rows
        while (cols < l) {
          s = a(cols) * a(rows)
          out(rows * l + cols) = s
          // copy value to lower tri where approp
          if (rows != cols) out(cols * l + rows) = s
          cols += 1
        }
        rows += 1
      }
      out
    }

    import Concurrent._
 
    def sumDc[T](a: Array[T])(implicit n : Numeric[T]): T = {
      import n.mkNumericOps
      aggregate(
          distribute(
              a.length, chunkOp(a, (a1:T,b:T) => a1 + b)
          )
      )
    }
    
    def sumSqrDc[T](a: Array[T])(implicit n: Numeric[T]): T = {
      import n.mkNumericOps
      aggregate(distribute(a.length, chunkOp(a, (a1:T,b:T) => a1 + b*b)))
    }  
    
    def sum[T : Numeric](a: Array[T]): T = {
      val n = implicitly[Numeric[T]]
      import n.mkNumericOps
      var i = a.length - 1
      var s = n.zero
      while (i > -1) {
        s += a(i)
        i -= 1
      }
      s
    }
  
    def average[T ](a: Array[T])(implicit n: Numeric[T]): T = n match{
        case num: Fractional[_] => import num._; sum(a)/ fromInt(a.length)
        case num: Integral[_] => import num._; sum(a) / fromInt(a.length)
        case _ => sys.error("Undivisable numeric!")
    }

    def averageDc[T ](a: Array[T])(implicit n: Numeric[T]): T = n match{
        case num: Fractional[_] => import num._; sumDc(a)/ fromInt(a.length)
        case num: Integral[_] => import num._; sumDc(a) / fromInt(a.length)
        case _ => sys.error("Undivisable numeric!")
    }

    def powF(f: Float, exp: Float) = {
      math.pow(f, exp).asInstanceOf[Float]
    }
    def logF(f: Float) = {
      math.log(f).asInstanceOf[Float]
    }

    def accuracy(s: Array[_], t: Array[_]): Double = {
      var res = 0d
      var i = 0
      while (i < s.length) {
        res += (if (s(i) == t(i)) 1d else 0d)
        i += 1
      }
      res / s.length
    }

    def accuracyDc[T](s: Array[T], t: Array[T])(implicit n: Numeric[T]): Double = {
      require(s.length == t.length)
      aggregate(distribute(s.length, biChunkOp(s,t, (a1:T,b:T) => if(a1 == b) n.one else n.zero)))
        
      var res = 0d
      var i = 0
      while (i < s.length) {
        res += (if (s(i) == t(i)) 1d else 0d)
        i += 1
      }
      res / s.length
    }

    def sumSquaredDiffs(s: Array[Double], t: Array[Double]) = {
      val len = s.length
      var i = 0
      var sum = 0d
      var delta = 0d
      while (i < len) {
        delta = t(i) - s(i)
        sum += delta * delta
        i += 1
      }
      sum
    }

    def sumSquaredDiffs(s: Array[Double], soff: Int, t: Array[Double], toff: Int, n: Int) = {
      val len = s.length
      var i = 0
      var sum = 0d
      var delta = 0d
      while (i < n) {
        delta = t(toff + i) - s(soff + i)
        sum += delta * delta
        i += 1
      }
      sum
    }

    private def sumSqrDiffsChunk(s: Array[Double], t: Array[Double])(range: (Long, Long))() = {
      val end = range._2.asInstanceOf[Int]
      var i = range._1.asInstanceOf[Int]
      var sum = 0d
      var delta = 0d
      while (i <= end) {
        delta = t(i) - s(i)
        sum += delta * delta
        i += 1
      }
      sum
    }

    def sumSquaredDiffsDc0(s: Array[Double], t: Array[Double]) = {
      val len = s.length
      aggregate(distribute(len, sumSqrDiffsChunk(s, t)))
    }
   
    // (a : Array[T], b : Array[T], chop : ( T, T ) => T)
    def sumSquaredDiffsDc[T](s: Array[T], t: Array[T])(implicit n : Numeric[T]) = {
      import n.mkNumericOps
      val len = s.length
      aggregate(
          distribute(len, biChunkOp(s, t, ( a: T, b : T) => { val delta = a - b; delta* delta})
              ))
    }

    private def sumSqrFdiffsChunk(s: Array[Float], t: Array[Float])(range: (Long, Long))() = {
      val end = range._2.asInstanceOf[Int]
      var i = range._1.asInstanceOf[Int]
      var sum = 0f
      var delta = 0f
      while (i <= end) {
        delta = t(i) - s(i)
        sum += delta * delta
        i += 1
      }
      sum
    }

    def sumSquaredFdiffsDc(s: Array[Float], t: Array[Float]) = {
      val len = s.length
      aggregate(distribute(len, sumSqrFdiffsChunk(s, t)))
    }

    def meanSquaredError(s: Array[Double], t: Array[Double]) = sumSquaredDiffs(s, t) / s.length
    def meanSquaredErrorDc(s: Array[Double], t: Array[Double]) = sumSquaredDiffsDc(s, t) / s.length

    def randpermArray(m: Int): Array[Int] = {
      val rnd = new Random(System.currentTimeMillis())
      var src = new Array[Int](m)
      var res = new Array[Int](m)
      var i = 0
      while (i < m) {
        src(i) = i + 1
        i += 1
      }
      i = 0

      while (src.length > 1) {
        val idx = math.abs(rnd.nextInt) % src.length
        res(i) = src(idx)
        src = src.filterNot(_ == src(idx))
        i += 1
      }
      res
    }

    def randpermBatch(m: Int, batchSize: Int): Array[Array[Int]] = {
      val rnd = new Random(System.currentTimeMillis())
      var src = new Array[Int](m)
      var res = new Array[Array[Int]](m)
      var i = 0
      while (i < m) {
        src(i) = i + 1
        i += 1
      }
      i = 0

      while (src.length > 1) {
        if (i % batchSize == 0) {
          res(i) = new Array[Int](batchSize)
        }
        val idx = math.abs(rnd.nextInt) % src.length
        res(i / batchSize)(i % batchSize) = src(idx)
        src = src.filterNot(_ == src(idx))
        i += 1
      }
      res
    }

    def randperm(m: Int): List[Int] = randpermArray(m).toList

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

    implicit def scalarOpF(f: Float) = new ScalarOpF(f)

    class ScalarOpF(f: Float) {
      def +(a: Array[Float]): Array[Float] = {
        val l = a.length
        val o = new Array[Float](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) + f
          i -= 1
        }
        o
      }
      def -(a: Array[Float]): Array[Float] = {
        val l = a.length
        val o = new Array[Float](l)
        var i = l - 1
        while (i > -1) {
          o(i) = f - a(i)
          i -= 1
        }
        o
      }
      def /(a: Array[Float]): Array[Float] = {
        val l = a.length
        val o = new Array[Float](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) / f
          i -= 1
        }
        o
      }
      def *(a: Array[Float]): Array[Float] = {
        val l = a.length
        val o = new Array[Float](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) * f
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
      def -(oa: Array[Double], sOff: Int = 0, tOff: Int = 0): Array[Double] = {
        val l = oa.length
        //require(oa.length == l, "arrays of unequal length")
        val o = new Array[Double](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i + sOff) - oa(i + tOff)
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
      def *(oa: Array[Double]): Array[Double] = {
        val l = a.length
        require(oa.length == l, "arrays of unequal length")
        val o = new Array[Double](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) * oa(i)
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

    implicit def arrayOpI(a: Array[Int]) = new ArrayOpI(a)

    class ArrayOpI(a: Array[Int]) {
      def +(oa: Array[Int]): Array[Int] = {
        val l = a.length
        require(oa.length == l, "arrays of unequal length")
        val o = new Array[Int](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) + oa(i)
          i -= 1
        }
        o
      }
      def +(d: Int): Array[Int] = {
        val l = a.length
        val o = new Array[Int](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) + d
          i -= 1
        }
        o
      }
      def -(oa: Array[Int]): Array[Int] = {
        val l = a.length
        require(oa.length == l, "arrays of unequal length")
        val o = new Array[Int](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) - oa(i)
          i -= 1
        }
        o
      }
      def -(oa: Array[Int], sOff: Int = 0, tOff: Int = 0): Array[Int] = {
        val l = oa.length
        //require(oa.length == l, "arrays of unequal length")
        val o = new Array[Int](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i + sOff) - oa(i + tOff)
          i -= 1
        }
        o
      }

      def -(d: Int): Array[Int] = {
        val l = a.length
        val o = new Array[Int](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) - d
          i -= 1
        }
        o
      }

      def /(d: Int): Array[Int] = {
        val l = a.length
        val o = new Array[Int](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) / d
          i -= 1
        }
        o
      }

      def *(d: Int): Array[Int] = {
        val l = a.length
        val o = new Array[Int](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) * d
          i -= 1
        }
        o
      }
      def *(oa: Array[Int]): Array[Int] = {
        val l = a.length
        require(oa.length == l, "arrays of unequal length")
        val o = new Array[Int](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) * oa(i)
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

    implicit def arrayOpF(a: Array[Float]) = new ArrayOpF(a)

    class ArrayOpF(a: Array[Float]) {
      def +(oa: Array[Float]): Array[Float] = {
        val l = a.length
        require(oa.length == l, "arrays of unequal length")
        val o = new Array[Float](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) + oa(i)
          i -= 1
        }
        o
      }
      def +(f: Float): Array[Float] = {
        val l = a.length
        val o = new Array[Float](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) + f
          i -= 1
        }
        o
      }
      def -(oa: Array[Float]): Array[Float] = {
        val l = a.length
        require(oa.length == l, "arrays of unequal length")
        val o = new Array[Float](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) - oa(i)
          i -= 1
        }
        o
      }
      def -(oa: Array[Float], sOff: Int = 0, tOff: Int = 0): Array[Float] = {
        val l = oa.length
        //require(oa.length == l, "arrays of unequal length")
        val o = new Array[Float](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i + sOff) - oa(i + tOff)
          i -= 1
        }
        o
      }

      def -(d: Float): Array[Float] = {
        val l = a.length
        val o = new Array[Float](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) - d
          i -= 1
        }
        o
      }

      def /(d: Float): Array[Float] = {
        val l = a.length
        val o = new Array[Float](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) / d
          i -= 1
        }
        o
      }

      def *(d: Float): Array[Float] = {
        val l = a.length
        val o = new Array[Float](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) * d
          i -= 1
        }
        o
      }
      def *(oa: Array[Float]): Array[Float] = {
        val l = a.length
        require(oa.length == l, "arrays of unequal length")
        val o = new Array[Float](l)
        var i = l - 1
        while (i > -1) {
          o(i) = a(i) * oa(i)
          i -= 1
        }
        o
      }

      def dot(oa: Array[Float]): Float = {
        val l = a.length
        require(oa.length == l, "arrays of unequal length")
        var s = 0f
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

    def lengthSquaredChunk(v: Array[Double])(range: (Long, Long))(): Double = {
      var d = 0d
      var vi = 0d
      var i = range._1.asInstanceOf[Int]
      val end = range._2.asInstanceOf[Int]
      while (i <= end) {
        vi = v(i)
        d += vi * vi
        i += 1
      }
      d
    }
    def lengthSquaredDc(v: Array[Double]): Double = aggregate(distribute(v.length, lengthSquaredChunk(v)))

    def lengthFsquaredChunk(v: Array[Float])(range: (Long, Long))(): Float = {
      var d = 0f
      var vi = 0f
      var i = range._1.asInstanceOf[Int]
      val end = range._2.asInstanceOf[Int]
      while (i <= end) {
        vi = v(i)
        d += vi * vi
        i += 1
      }
      d
    }
    def lengthFsquaredDc(v: Array[Float]): Float = aggregate(distribute(v.length, lengthFsquaredChunk(v)))

    def lengthSquared2(v: Array[Double]): Double = {
      var d = 0d
      var i = v.length - 1
      while (i > -1) {
        d += v(i) * v(i)
        i -= 1
      }
      d
    }

    def minMaxRow(x: Array[Array[Double]]): Array[Array[Double]] = {
      val innerL = x(0).length
      var min = x(0)
      var minL = Double.MaxValue
      var maxL = Double.MinValue
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

    def minMaxRowChunk(x: Array[Array[Double]])(range: (Long, Long))(): Array[Array[Double]] = {
      val innerL = x(0).length
      var min = x(0)
      var minL = Double.MaxValue
      var maxL = Double.MinValue
      var curr = 0d
      var currL = 0d
      var max = x(0)
      val outerL = range._2.asInstanceOf[Int]
      var i = range._1.asInstanceOf[Int]
      var j = 0
      while (i <= outerL) {
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
        }
        if (currL < minL) {
          min = x(i)
          minL = currL
        }
        i += 1
      }
      Array(min, max)
    }

    def minMaxRowDc(x: Array[Array[Double]]): Array[Array[Double]] = {
      val (futs,cs) = Concurrent.distribute(x.length, minMaxRowChunk(x))
      var i = 0
      var f: Future[Array[Array[Double]]] = null
      var m: Array[Array[Double]] = null
      val innerL = x(0).length
      var min = x(0)
      var minL = Double.MaxValue
      var maxL = Double.MinValue
      var curr = 0d
      var currL = 0d
      var max = x(0)
      var j = 0
      var futmmIdx = 0
      while (i < futs.length) {
        f = cs.take()
        futmmIdx = 0
        m = f.get()
        while (futmmIdx < m.length) {
          j = 0
          currL = 0
          while (j < innerL) {
            curr = m(futmmIdx)(j)
            currL += curr * curr
            j += 1
          }
          if (currL > maxL) {
            max = m(futmmIdx)
            maxL = currL
          }
          if (currL < minL) {
            min = m(futmmIdx)
            minL = currL
          }
          futmmIdx += 1
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

    def unitFv(v: Array[Float]): (Float, Array[Float]) = {
      val ov = v.clone
      val l = math.sqrt(lengthFsquaredDc(v)).asInstanceOf[Float]
      var i = ov.length - 1
      while (i > -1) {
        ov(i) /= l
        i -= 1
      }
      (l, ov)
    }

    def divChunk(v: Array[Double], divisor: Double)(range: (Long, Long))() = {
      var i = range._1.asInstanceOf[Int]
      val end = range._2.asInstanceOf[Int]
      while (i <= end) {
        v(i) /= divisor
        i += 1
      }
      i
    }

    def divFchunk(v: Array[Float], divisor: Float)(range: (Long, Long))() = {
      var i = range._1.asInstanceOf[Int]
      val end = range._2.asInstanceOf[Int]
      while (i <= end) {
        v(i) /= divisor
        i += 1
      }
      i
    }

    def addChunk(v: Array[Double], add: Double)(range: (Long, Long))() = {
      var i = range._1.asInstanceOf[Int]
      val end = range._2.asInstanceOf[Int]
      while (i <= end) {
        v(i) += add
        i += 1
      }
      i
    }
    def addFchunk(v: Array[Float], add: Float)(range: (Long, Long))() = {
      var i = range._1.asInstanceOf[Int]
      val end = range._2.asInstanceOf[Int]
      while (i <= end) {
        v(i) += add
        i += 1
      }
      i
    }
    def negateChunk(v: Array[Double])(range: (Long, Long))() = {
      var i = range._1.asInstanceOf[Int]
      val end = range._2.asInstanceOf[Int]
      var el = 0d
      var s = 0d
      while (i <= end) {
        v(i) = -v(i)
        i += 1
      }
      i
    }
    def negateFchunk(v: Array[Float])(range: (Long, Long))() = {
      var i = range._1.asInstanceOf[Int]
      val end = range._2.asInstanceOf[Int]
      var el = 0f
      var s = 0f
      while (i <= end) {
        v(i) = -v(i)
        i += 1
      }
      i
    }

    def maxColIdxChunk(a: Array[Double], n: Int, idxs: Array[Double])(range: (Long, Long))() = {
      val end = range._2.asInstanceOf[Int]
      var i = range._1.asInstanceOf[Int]
      var j = 0
      var offset = 0
      var max = 0d
      var curr = 0d
      while (i <= end) {
        j = 0
        max = -Double.MaxValue
        offset = i * n
        while (j < n) {
          curr = a(offset + j)
          if (curr > max) {
            max = curr
            idxs(i) = j
          }
          j += 1
        }
        i += 1
      }
      i
    }

    def maxColFidxChunk(a: Array[Float], n: Int, idxs: Array[Float])(range: (Long, Long))() = {
      val end = range._2.asInstanceOf[Int]
      var i = range._1.asInstanceOf[Int]
      var j = 0
      var offset = 0
      var max = 0f
      var curr = 0f
      while (i <= end) {
        j = 0
        max = -Float.MaxValue
        offset = i * n
        while (j < n) {
          curr = a(offset + j)
          if (curr > max) {
            max = curr
            idxs(i) = j
          }
          j += 1
        }
        i += 1
      }
      i
    }

    def unitVDc(v: Array[Double]): (Double, Array[Double]) = {
      val ov = v.clone
      val l = math.sqrt(lengthSquaredDc(v))
      combine(distribute(ov.length, divChunk(ov, l)))
      (l, ov)
    }

    def unitVfDc(v: Array[Float]): (Float, Array[Float]) = {
      val ov = v.clone
      val l = math.sqrt(lengthFsquaredDc(v)).asInstanceOf[Float]
      combine(distribute(ov.length, divFchunk(ov, l)))
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

    /*  def average[T ](a: Array[T])(implicit n: Numeric[T]): T = n match{
        case num: Fractional[_] => import num._; sum(a)/ fromInt(a.length)
        case num: Integral[_] => import num._; sum(a) / fromInt(a.length)
        case _ => sys.error("Undivisable numeric!")
    }
*/

    def stdDc[T](a: Array[T])(implicit n: Numeric[T], nl : NumberLike[T]) = {
      val l = a.length
      val sumSqr = sumSqrDc(a)
      val avg = averageDc(a)
      val res = 
      n match {
        case num: Fractional[_] => {
          import num._
          val em = NumberLike
          sumSqr / fromInt(l) - (avg * avg)
        }
        case num: Integral[_] => {
          import num._
          sumSqr / fromInt(l) - (avg * avg)
        }
        case _ => sys.error("Undivisable numeric!")
      }
      nl.sqrt(res)
    }

    
    def toDouble(els: Array[Int]): Array[Double] = {
      val l = els.length
      val el = new Array[Double](l)
      var i = 0
      while (i < l) {
        el(i) = els(i)
        i += 1
      }
      el
    }
    def toFloat(els: Array[Int]): Array[Float] = {
      val l = els.length
      val el = new Array[Float](l)
      var i = 0
      while (i < l) {
        el(i) = els(i)
        i += 1
      }
      el
    }
  }
  object ArrayUtil {
    def fill(a: Array[Double], v: Double) {
      var i = a.length - 1
      while (i > -1) {
        a(i) = v
        i -= 1
      }
    }
    def fill(a: Array[Short], s: Short) {
      var i = a.length - 1
      while (i > -1) {
        a(i) = s
        i -= 1
      }
    }
    def fill(a: Array[Int], s: Int) {
      var i = a.length - 1
      while (i > -1) {
        a(i) = s
        i -= 1
      }
    }
  }
  
  
  object Io {
    private def lines(s: Array[Char]): Array[String] = {
      println("loaded file")
      var l = List[String]()
      var line = ""
      var c: Char = ' '
      var idx = 0
      val len = s.length
      val sb = new StringBuffer
      while (idx < len) {
        c = s(idx)
        if (c != '\n') {
          sb.append(c)
        } else {
          line = sb.toString()
          sb.setLength(0)
          if (!line.trim().isEmpty())
            l = l :+ line.trim
          line = ""
        }
        idx += 1
      }
      l.toArray
    }

    def parseOctaveDataFile(path: String, asDouble: Boolean = true): Map[String, _] = {
      var m = Map[String, Any]()
      val la = Source.fromFile(path).getLines
      //println("loaded " + path + " into line array of size " + la.length)
      var elementType = ""
      var idx = -1
      var rows = -1
      var cols = -1
      var currRow = 0
      var lastChunk = 0l
      var name = ""
      var elementDataD = if (asDouble) new Array[Double](0) else null
      var elementDataF = if (!asDouble) new Array[Float](0) else null
      var lastTime = 0l

      def addObject {
        if (asDouble) {
          if (!elementDataD.isEmpty) {
            println("loading " + name + " took " + (System.currentTimeMillis() - lastTime))
            elementType match {
              case "matrix" =>
                println("adding " + elementType + " '" + name + "' of dims " + rows + ", " + cols)
                m = m + new Tuple2(name, new MatrixD(elementDataD.clone(), cols, true))
              case "scalar" =>
                println("adding " + elementType + " '" + name + "' of dims " + rows + ", " + cols)
                m = m + new Tuple2(name, elementDataD(0))
            }
          }
        } else {
          if (!elementDataF.isEmpty) {
            println("loading " + name + " took " + (System.currentTimeMillis() - lastTime))
            elementType match {
              case "matrix" =>
                println("adding " + elementType + " '" + name + "' of dims " + rows + ", " + cols)
                m = m + new Tuple2(name, new MatrixF(elementDataF, cols, true))
              case "scalar" =>
                println("adding " + elementType + " '" + name + "' of dims " + rows + ", " + cols)
                m = m + new Tuple2(name, elementDataF(0))
            }
          }

        }
      }

      def parseDataLine(l: String, elementData: Array[_], startIdx: Int, cols: Int) {
        val elDatD = if (asDouble) elementData.asInstanceOf[Array[Double]] else null
        val elDatF = if (!asDouble) elementData.asInstanceOf[Array[Float]] else null
        val spl = l.split(" ")
        assert(spl.length == cols, spl.length + " != " + cols)
        val len = spl.length
        var idx = 0
        while (idx < len) {
          if (asDouble) {
            elDatD(startIdx + idx) = java.lang.Double.parseDouble(spl(idx))
          } else elDatF(startIdx + idx) = java.lang.Float.parseFloat(spl(idx))
          idx += 1
        }
      }

      for (l <- la) {
        val line = l.trim
        //println(line)
        if (line.startsWith("#")) {
          idx = line.indexOf("name:")
          if (idx > -1) {
            if (!name.isEmpty() && !elementType.isEmpty()) {
              // add previous obj
              addObject
              currRow = 0
            }
            name = line.substring(idx + "name:".length).trim
            println("found " + name)
            lastTime = System.currentTimeMillis()
            rows = -1
            cols = -1
            elementType = ""
          } else {
            idx = line.indexOf("type:")
            if (idx > -1) {
              elementType = line.substring(idx + "type:".length).trim
            } else {
              idx = line.indexOf("rows:")
              if (idx > -1) {
                rows = Integer.parseInt(line.substring(idx + "rows:".length).trim)
              } else {
                idx = line.indexOf("columns:")
                if (idx > -1) {
                  cols = Integer.parseInt(line.substring(idx + "columns:".length).trim)
                }
              }
            }
          }
        } else if (!line.isEmpty()) {
          elementType match {
            case "matrix" =>
              assert(!name.isEmpty() && !elementType.isEmpty() && rows > -1 && cols > -1)
              if (asDouble) {
                if (elementDataD.length != rows * cols) {
                  elementDataD = new Array[Double](rows * cols)
                }
                // it's a data line (row)
                parseDataLine(line, elementDataD, currRow * cols, cols)
                currRow += 1
                if (currRow % 100 == 0) {
                  val now = System.currentTimeMillis()
                  if (lastChunk != 0) {
                    val delta = ((now - lastChunk) / 1000.0).asInstanceOf[Int]
                    println("on " + currRow + "/" + rows + " at " + delta / 100.0f + " s/row")
                  } else {
                    println("on " + currRow + "/" + rows)
                  }
                  lastChunk = now
                }
              } else {
                if (elementDataF.length != rows * cols) {
                  elementDataF = new Array[Float](rows * cols)
                }
                // it's a data line (row)
                parseDataLine(line, elementDataF, currRow * cols, cols)
                currRow += 1
                if (currRow % 100 == 0) {
                  val now = System.currentTimeMillis()
                  if (lastChunk != 0) {
                    val delta = ((now - lastChunk) / 1000.0).asInstanceOf[Int]
                    println("on " + currRow + "/" + rows + " at " + delta / 100.0f + " s/row")
                  } else {
                    println("on " + currRow + "/" + rows)
                  }
                  lastChunk = now
                }
              }
            case "scalar" =>
              if (asDouble) {
                elementDataD = new Array[Double](1)
                elementDataD(0) = java.lang.Double.parseDouble(line)
              } else {
                elementDataF = new Array[Float](1)
                elementDataF(0) = java.lang.Float.parseFloat(line)
              }

            case _ =>
              println("unknown type " + elementType)
          }
        }
      }
      // add last obj
      if (!name.isEmpty() && !elementType.isEmpty()) {
        addObject
      }
      m
    }

    class RichFile(file: File) {

      def text = Source.fromFile(file).mkString
      def obj = {
        val ois = new ObjectInputStream(new FileInputStream(file))
        try { ois.readObject }
        finally { ois.close }
      }
      def text_=(s: String) {
        val out = new PrintWriter(file)
        try { out.print(s) }
        finally { out.close }
      }
      def obj_=(o: Any) {
        val oos = new ObjectOutputStream(new FileOutputStream(file))
        try { oos.writeObject(o) }
        finally { oos.close }
      }
    }

    object RichFile {

      implicit def enrichFile(file: File) = new RichFile(file)

    }

  }
  
}

