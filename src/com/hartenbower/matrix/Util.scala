package com.hartenbower.matrix
import scala.util.Random
import java.io._
import scala.io._
import java.util.concurrent._
import com.hartenbower.util.TimingUtil

object Util {
  val rnd = new java.util.Random(System.currentTimeMillis())

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

    def effort[T](f: () => T): Future[T] = {
      val ft = new FutureTask[T](new Exister(f))
      //println("making an effort")
      pool.execute(ft)
      ft
    }

    def distribute[T](indexSpace: Long, f: (Tuple2[Long, Long]) => () => T, oneBased: Boolean = false) = {
      var i = 0
      val spans = toSpans(indexSpace, threadCount, oneBased)
      //println("spans " + spans.mkString)
      val efforts = new Array[Future[T]](spans.length)
      spans.length match {
        case 1 =>
          efforts(0) = new FutureIsNow[T](f(spans(0))())
        case _ =>
          while (i < spans.length) {
            efforts(i) = effort(f(spans(i)))
            i += 1
          }
      }
      efforts
    }

    def combine[T](efforts: Array[Future[T]]) {
      var i = 0
      while (i < efforts.length) {
        efforts(i).get
        i += 1
      }
    }

    def aggregateD(efforts: Array[Future[Double]]): Double = {
      var i = 0
      var s = 0d
      while (i < efforts.length) {
        s += efforts(i).get
        i += 1
      }
      s
    }

    def toSpans(l: Long, d: Int, oneBased: Boolean = false, threshold: Int = defaultSpanThreshold): Array[Tuple2[Long, Long]] = {
      val span = l / d
      //if (span >= threshold) {
      val ret = new Array[Tuple2[Long, Long]](d)
      var i = 0
      while (i < d - 1) {
        ret(i) = if (oneBased) (i * span + 1, (i + 1) * span) else (i * span, (i + 1) * span - 1)
        i += 1
      }
      ret(d - 1) = if (oneBased) ((d - 1) * span + 1, l) else ((d - 1) * span, l - 1)
      ret
      //      } else {
      //        Array((if (oneBased) 1l else 0l, if (oneBased) l else l - 1))
      //      }
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
      println("\n" + msg + " took " + TimingUtil.fromMillis(delta) + " or " + (count * 1000. / delta) + "evals/s")
      (count, delta)
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
                    val delta = ((now - lastChunk) / 1000.).asInstanceOf[Int]
                    println("on " + currRow + "/" + rows + " at " + delta / 100.f + " s/row")
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
                    val delta = ((now - lastChunk) / 1000.).asInstanceOf[Int]
                    println("on " + currRow + "/" + rows + " at " + delta / 100.f + " s/row")
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

  object Math {
    private def sumChunk(a: Array[Double])(range: (Long, Long))(): Double = {
      var i = range._1.asInstanceOf[Int]
      val end = range._2.asInstanceOf[Int]
      var s = 0d
      while (i <= end) {
        s += a(i)
        i += 1
      }
      s
    }

    private def sumSqrChunk(a: Array[Double])(range: (Long, Long))(): Double = {
      var i = range._1.asInstanceOf[Int]
      val end = range._2.asInstanceOf[Int]
      var s = 0d
      var t = 0d
      while (i <= end) {
        t = a(i)
        s += t * t
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
    import Concurrent._
    def sumDc(a: Array[Double]): Double = {
      aggregateD(distribute(a.length, sumChunk(a)))
    }

    def sumSqrDc(a: Array[Double]): Double = {
      aggregateD(distribute(a.length, sumSqrChunk(a)))
    }

    def sumF(a: Array[Float]): Float = {
      var i = a.length - 1
      var s = 0f
      while (i > -1) {
        s += a(i)
        i -= 1
      }
      s
    }

    def average(a: Array[Double]): Double = {
      sum(a) / a.length
    }

    def averageDc(a: Array[Double]): Double = {
      sumDc(a) / a.length
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

    private def sumSquaredDiffsChunk(s: Array[Double], t: Array[Double])(range: (Long, Long))() = {
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

    def sumSquaredDiffsDc(s: Array[Double], t: Array[Double]) = {
      val len = s.length
      aggregateD(distribute(len, sumSquaredDiffsChunk(s, t)))
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

    private def lengthSquaredChunk(v: Array[Double])(range:(Long,Long))(): Double = {
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
    def lengthSquaredDc(v: Array[Double]): Double = aggregateD(distribute(v.length, lengthSquaredChunk(v)))

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
    
    def divChunk(v:Array[Double], divisor : Double)(range:(Long,Long))() = {
       var i = range._1.asInstanceOf[Int]
       val end = range._2.asInstanceOf[Int]
       while (i <= end) {
        v(i) /= divisor
        i += 1
      }
      i
    }

    def addChunk(v:Array[Double], add : Double)(range:(Long,Long))() = {
       var i = range._1.asInstanceOf[Int]
       val end = range._2.asInstanceOf[Int]
       while (i <= end) {
        v(i) += add
        i += 1
      }
      i
    }

    def unitVDc(v: Array[Double]): (Double, Array[Double]) = {
      val ov = v.clone
      val l = math.sqrt(lengthSquaredDc(v))
      combine(distribute(ov.length, divChunk(ov,l)))
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

    def stdDc(a: Array[Double]) = {
      val l = a.length
      val sumSqr = sumSqrDc(a)
      val avg = averageDc(a)
      math.sqrt( sumSqr /l - (avg * avg) )
    }
  }

  object ArrayUtil {
    def fill(a:Array[Double], v : Double) {
      var i = a.length-1
      while(i > -1) {
        a(i) = v
        i-=1
      }
    }
    def fill(a:Array[Short], s : Short) {
      var i = a.length-1
      while(i > -1) {
        a(i) = s
        i-=1
      }
    }
    def fill(a:Array[Int], s : Int) {
      var i = a.length-1
      while(i > -1) {
        a(i) = s
        i-=1
      }
    }
  }
}