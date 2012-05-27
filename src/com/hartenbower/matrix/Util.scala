package com.hartenbower.matrix
import scala.util.Random
import java.io._
import scala.io._
import java.util.concurrent._
import com.hartenbower.util.TimingUtil

object Util {

  object Concurrent {

    val defaultSpanThreshold = 3
    val threadCount = 5 * Runtime.getRuntime.availableProcessors / 2
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

    def distribute(indexSpace: Long, f: (Tuple2[Long, Long]) => () => Long, oneBased: Boolean = false) = {
      var i = 0
      val spans = toSpans(indexSpace, threadCount, oneBased)
      //println("spans " + spans.mkString)
      val efforts = new Array[Future[Long]](spans.length)
      spans.length match {
        case 1 =>
          efforts(0) = new FutureIsNow(f(spans(0))())
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

    def toSpans(l: Long, d: Int, oneBased: Boolean = false, threshold: Int = defaultSpanThreshold): Array[Tuple2[Long, Long]] = {
      val span = l / d
      if (span > threshold) {
        val ret = new Array[Tuple2[Long, Long]](d)
        var i = 0
        while (i < d - 1) {
          ret(i) = if (oneBased) (i * span + 1, (i + 1) * span) else (i * span, (i + 1) * span - 1)
          i += 1
        }
        ret(d - 1) = if (oneBased) ((d - 1) * span + 1, l) else ((d - 1) * span, l - 1)
        ret
      } else {
        Array((if (oneBased) 1l else 0l, if (oneBased) l else l - 1))
      }
    }

    def transposeChunk(src: Array[Double], len: Long, trg: Array[Double], rows: Int)(range: Tuple2[Long, Long])(): Long = {
      //println("txChunk range " + range)
      var i: Long = range._1
      while (i <= range._2) {
        trg((i * rows % len).asInstanceOf[Int]) = src(i.asInstanceOf[Int])
        i += 1
      }
      i
    }

    def transposeChunkF(src: Array[Float], len: Long, trg: Array[Float], rows: Int)(range: Tuple2[Long, Long])(): Long = {
      //println("txChunk range " + range)
      var i: Long = range._1
      while (i <= range._2) {
        trg((i * rows % len).asInstanceOf[Int]) = src(i.asInstanceOf[Int])
        i += 1
      }
      i
    }

    def multChunk(src1: Array[Double], cols1: Int, src2: Array[Double], rows2: Int, cols2: Int, trg: Array[Double])(range: Tuple2[Long, Long])(): Long = {
      @inline def rowIndices(row: Int, cols: Int) = {
        val start = (row - 1) * cols
        (start, start + cols - 1)
      }
      var row = range._1
      var rowT = 1
      var idx = (row - 1) * rows2
      while (row <= range._2) {
        rowT = 1
        //println("row " + row + " idx " + idx)
        while (rowT <= rows2) {
          trg(idx.asInstanceOf[Int]) = MatrixD.dot(src1, rowIndices(row.asInstanceOf[Int], cols1), src2, rowIndices(rowT, cols2))
          rowT += 1
          idx += 1
        }
        row += 1
      }
      row
    }

    def multChunkF(src1: Array[Float], cols1: Int, src2: Array[Float], rows2: Int, cols2: Int, trg: Array[Float])(range: Tuple2[Long, Long])(): Long = {
      @inline def rowIndices(row: Int, cols: Int) = {
        val start = (row - 1) * cols
        (start, start + cols - 1)
      }
      var row = range._1
      var rowT = 1
      var idx = (row - 1) * rows2
      while (row <= range._2) {
        rowT = 1
        //println("row " + row + " idx " + idx)
        while (rowT <= rows2) {
          trg(idx.asInstanceOf[Int]) = MatrixF.dot(src1, rowIndices(row.asInstanceOf[Int], cols1), src2, rowIndices(rowT, cols2))
          rowT += 1
          idx += 1
        }
        row += 1
      }
      row
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
      val la = lines(Source.fromFile(path).toArray)
      println("loaded " + path + " into line array of size " + la.length)
      var elementType = ""
      var idx = -1
      var rows = -1
      var cols = -1
      var currRow = 0
      var lastChunk = 0l
      var name = ""
      var elementDataD = if (asDouble) Array[Double]() else null
      var elementDataF = if (!asDouble) Array[Float]() else null
      var lastTime = 0l

      def addObject {
        if (asDouble) {
          if (!elementDataD.isEmpty) {
            println("loading " + name + " took " + (System.currentTimeMillis() - lastTime))
            elementType match {
              case "matrix" =>
                println("adding " + elementType + " '" + name + "' of dims " + rows + ", " + cols)
                m = m + (name -> new MatrixD(elementDataD, cols))
              case "scalar" =>
                println("adding " + elementType + " '" + name + "' of dims " + rows + ", " + cols)
                m = m + (name -> elementDataD(0))
            }
          }
        } else {
          if (!elementDataF.isEmpty) {
            println("loading " + name + " took " + (System.currentTimeMillis() - lastTime))
            elementType match {
              case "matrix" =>
                println("adding " + elementType + " '" + name + "' of dims " + rows + ", " + cols)
                m = m + (name -> new MatrixF(elementDataF, cols))
              case "scalar" =>
                println("adding " + elementType + " '" + name + "' of dims " + rows + ", " + cols)
                m = m + (name -> elementDataF(0))
            }
          }

        }
      }

      def parseDataLine(l: String, elementData: Array[_], startIdx: Int, cols: Int) {
        val elDatD = if (asDouble) elementData.asInstanceOf[Array[Double]] else null
        val elDatF = if (!asDouble) elementData.asInstanceOf[Array[Float]] else null
        val spl = l.split(" ")
        assert(spl.length == cols)
        val len = spl.length
        var idx = 0
        while (idx < len) {
          if (asDouble) {
            elDatD(startIdx + idx) = java.lang.Double.parseDouble(spl(idx))
          } else elDatF(startIdx + idx) = java.lang.Float.parseFloat(spl(idx))
          idx += 1
        }
      }

      for (line <- la) {
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
        } else {
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
                elementDataD = Array[Double](java.lang.Double.parseDouble(line))
              } else {
                elementDataF = Array[Float](java.lang.Float.parseFloat(line))
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
    def sum(a: Array[Double]): Double = {
      var i = a.length - 1
      var s = 0d
      while (i > -1) {
        s += a(i)
        i -= 1
      }
      s
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
    
    def powF(f :Float, exp : Float) = {
      math.pow(f,exp).asInstanceOf[Float]
    }
    def logF(f :Float) = {
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
      }
      sum
    }

    def meanSquaredError(s: Array[Double], t: Array[Double]) = sumSquaredDiffs(s, t) / s.length

    def randperm(m: Int): List[Int] = {
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
      res.toList
    }

  }

}