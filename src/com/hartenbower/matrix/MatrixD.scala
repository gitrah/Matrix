package com.hartenbower.matrix
import scala.util.Random
import java.util.concurrent._
import Util._
import Util.Math._
object MatrixD {
  var txpsCreateCount = 0l
  var txpsUseCount = 0l
  var verbose = false

  implicit def log(m: MatrixD) = m.log()
  implicit def scalarOp(d: Double) = new ScalarOp(d)

  class ScalarOp(d: Double) {
    def +(matrix: MatrixD): MatrixD = matrix + d
    def -(matrix: MatrixD): MatrixD = matrix.negateN + d
    def /(matrix: MatrixD): MatrixD = matrix / d
    def *(matrix: MatrixD): MatrixD = matrix * d
    def +(a: Array[MatrixD]): Array[MatrixD] =
      { var i = a.length; val o = new Array[MatrixD](i); i -= 1; while (i > -1) { o(i) = a(i) + d; i -= 1 }; o }
    def -(a: Array[MatrixD]): Array[MatrixD] =
      { var i = a.length; val o = new Array[MatrixD](i); i -= 1; while (i > -1) { o(i) = a(i).negateN + d; i -= 1 }; o }
    def /(a: Array[MatrixD]): Array[MatrixD] =
      { var i = a.length; val o = new Array[MatrixD](i); i -= 1; while (i > -1) { o(i) = a(i) / d; i -= 1 }; o }
    def *(a: Array[MatrixD]): Array[MatrixD] =
      { var i = a.length; val o = new Array[MatrixD](i); i -= 1; while (i > -1) { o(i) = a(i) * d; i -= 1 }; o }
  }

  implicit def arrayOp(a: Array[MatrixD]) = new ArrayOp(a)

  class ArrayOp(a: Array[MatrixD]) {
    def +(m: MatrixD): Array[MatrixD] = {
      val l = a.length
      val o = new Array[MatrixD](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) + m
        i -= 1
      }
      o
    }
    def +(oa: Array[MatrixD]): Array[MatrixD] = {
      val l = a.length
      require(l == oa.length, "arrays of unequal length")
      val o = new Array[MatrixD](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) + oa(i)
        i -= 1
      }
      o
    }
    def -(m: MatrixD): Array[MatrixD] = {
      val l = a.length
      val o = new Array[MatrixD](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) - m
        i -= 1
      }
      o
    }
    def -(oa: Array[MatrixD]): Array[MatrixD] = {
      val l = a.length
      require(l == oa.length, "arrays of unequal length")
      val o = new Array[MatrixD](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) - oa(i)
        i -= 1
      }
      o
    }
    def *(m: MatrixD): Array[MatrixD] = {
      val l = a.length
      val o = new Array[MatrixD](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) * m
        i -= 1
      }
      o
    }
    def *(oa: Array[MatrixD]): Array[MatrixD] = {
      val l = a.length
      require(l == oa.length, "arrays of unequal length")
      val o = new Array[MatrixD](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) * oa(i)
        i -= 1
      }
      o
    }
    def /(m: MatrixD): Array[MatrixD] = {
      val l = a.length
      val o = new Array[MatrixD](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) / m
        i -= 1
      }
      o
    }
    def /(oa: Array[MatrixD]): Array[MatrixD] = {
      val l = a.length
      require(l == oa.length, "arrays of unequal length")
      val o = new Array[MatrixD](l)
      var i = l - 1
      while (i > -1) {
        o(i) = a(i) / oa(i)
        i -= 1
      }
      o
    }
  }

  def zeros(nRows: Int, nCols: Int): MatrixD = new MatrixD(Array.fill(nRows * nCols)(0d), nCols)
  def zeros(dims: (Int, Int)): MatrixD = zeros(dims._1, dims._2)
  def ones(nRows: Int, nCols: Int): MatrixD = new MatrixD(Array.fill(nRows * nCols)(1d), nCols)
  def ones(dims: (Int, Int)): MatrixD = ones(dims._1, dims._2)

  /**
   * Creates a repeatable psuedo-random matrix based on math.sin function
   * @param m
   * @param n
   * @return
   */
  def sin(m: Int, n: Int): MatrixD = {
    val l = m * n
    val mat = MatrixD.zeros(m, n)
    var i = 0
    while (i < l) {
      mat.elements(i) = math.sin(i + 1) / 10
      i += 1
    }
    mat
  }

  def dot(v1: Array[Double], range1: Tuple2[Int, Int], v2: Array[Double], range2: Tuple2[Int, Int]): Double = {
    //require(range1._1 >= 0 && range1._2 < v1.length, "range1 outside v1")
    //require(range2._1 >= 0 && range2._2 < v2.length, "range2 outside v2")
    var l = range1._2 - range1._1
    //require(l == range2._2 - range2._1, "vectors of unequal length")
    var sum = 0.
    while (l >= 0) {
      sum += v1(range1._1 + l) * v2(range2._1 + l)
      l -= 1
    }
    sum
  }

  def dotlv(v1: Array[Double], range1: Tuple2[Int, Int], v2: Array[Double], range2: Tuple2[Int, Int]): Double = {
    //require(range1._1 >= 0 && range1._2 < v1.length, "range1 outside v1")
    //require(range2._1 >= 0 && range2._2 < v2.length, "range2 outside v2")
    var l = range1._2 - range1._1
    val s1 = range1._1
    val s2 = range2._1
    //require(l == range2._2 - range2._1, "vectors of unequal length")
    var sum = 0.
    while (l >= 0) {
      sum += v1(s1 + l) * v2(s2 + l)
      l -= 1
    }
    sum
  }

  val MaxDim = math.sqrt(Int.MaxValue)
  def diagonalM(dim: Int, d: Double = 1.0): MatrixD = {
    require(dim <= MaxDim, "dim exceeds sqrt(Int.MaxValue) or " + MaxDim)
    val l = dim * dim
    val c = new Array[Double](l)
    var i = 0
    while (i < l) {
      c(i) = if (i / dim == i % dim) d else 0
      i += 1
    }
    new MatrixD(c, dim)
  }

  def identityM(dim: Int) = diagonalM(dim)

  def diag(a : Array[Double]) : MatrixD = {
    val l = a.length
    val m = MatrixD.zeros(l,l)
    var i = 0
    val len = l * l
    while(i < len) {
      if( i % l == i / l) {
        m.elements(i) = a(i % l)
      }
      i+=1
    }
    m
  }
  /* could also define one as transpose of other... */
  def rowMatrix(a: Array[Double]) = new MatrixD(a, a.length)

  def columnMatrix(a: Array[Double]) = new MatrixD(a, 1)
  def randn(dims: Tuple2[Int, Int], epsilon: Double): MatrixD = randn(dims._1, dims._2, epsilon)
  def randn(dims: Tuple2[Int, Int]): MatrixD = randn(dims._1, dims._2)
  def randn(nRows: Int, nCols: Int, epsilon: Double = 1d): MatrixD = {
    val rnd = new Random(System.currentTimeMillis)
    val l = nRows * nCols
    val c = new Array[Double](l)
    var i = 0
    while (i < l) {
      c(i) = (2 * rnd.nextDouble - 1) * epsilon
      i += 1
    }
    new MatrixD(c, nCols)
  }

  def randSpace(nRows: Int, nCols: Int, nElements: Int): Array[Double] = {
    val rnd = new Random(System.currentTimeMillis)
    Array.fill(nRows * nCols * nElements, 1)(rnd.nextDouble).flatten
  }

  def mult4by4(mat1: Array[Double], mat2: Array[Double], resMat: Array[Double]) = {
    resMat(0) = mat1(0) * mat2(0) + mat1(4) * mat2(1) + mat1(8) * mat2(2) + mat1(12) * mat2(3)
    resMat(4) = mat1(0) * mat2(4) + mat1(4) * mat2(5) + mat1(8) * mat2(6) + mat1(12) * mat2(7)
    resMat(8) = mat1(0) * mat2(8) + mat1(4) * mat2(9) + mat1(8) * mat2(10) + mat1(12) * mat2(11)
    resMat(12) = mat1(0) * mat2(12) + mat1(4) * mat2(13) + mat1(8) * mat2(14) + mat1(12) * mat2(15)
    resMat(1) = mat1(1) * mat2(0) + mat1(5) * mat2(1) + mat1(9) * mat2(2) + mat1(13) * mat2(3)
    resMat(5) = mat1(1) * mat2(4) + mat1(5) * mat2(5) + mat1(9) * mat2(6) + mat1(13) * mat2(7)
    resMat(9) = mat1(1) * mat2(8) + mat1(5) * mat2(9) + mat1(9) * mat2(10) + mat1(13) * mat2(11)
    resMat(13) = mat1(1) * mat2(12) + mat1(5) * mat2(13) + mat1(9) * mat2(14) + mat1(13) * mat2(15)
    resMat(2) = mat1(2) * mat2(0) + mat1(6) * mat2(1) + mat1(10) * mat2(2) + mat1(14) * mat2(3)
    resMat(6) = mat1(2) * mat2(4) + mat1(6) * mat2(5) + mat1(10) * mat2(6) + mat1(14) * mat2(7)
    resMat(10) = mat1(2) * mat2(8) + mat1(6) * mat2(9) + mat1(10) * mat2(10) + mat1(14) * mat2(11)
    resMat(14) = mat1(2) * mat2(1) + mat1(6) * mat2(13) + mat1(10) * mat2(14) + mat1(14) * mat2(15)
    resMat(3) = mat1(3) * mat2(0) + mat1(7) * mat2(1) + mat1(11) * mat2(2) + mat1(15) * mat2(3)
    resMat(7) = mat1(3) * mat2(4) + mat1(7) * mat2(5) + mat1(11) * mat2(6) + mat1(15) * mat2(7)
    resMat(11) = mat1(3) * mat2(8) + mat1(7) * mat2(9) + mat1(11) * mat2(10) + mat1(15) * mat2(11)
    resMat(15) = mat1(3) * mat2(12) + mat1(7) * mat2(13) + mat1(11) * mat2(14) + mat1(15) * mat2(15)
  }

  def mult4by4Range(
    count: Int,
    mat1: Array[Double], offset1: Int,
    mat2: Array[Double], offset2: Int,
    resMat: Array[Double], offset3: Int) {
    var ctr = count - 1
    val base1 = offset1
    while (ctr >= 0) {
      val base0 = ctr << 4
      val base2 = base0 + offset2
      val base3 = base0 + offset3
      resMat(base3) = mat1(base1) * mat2(base2) + mat1(base1 + 4) * mat2(base2 + 1) + mat1(base1 + 8) * mat2(base2 + 2) + mat1(base1 + 12) * mat2(base2 + 3)
      resMat(base3 + 4) = mat1(base1) * mat2(base2 + 4) + mat1(base1 + 4) * mat2(base2 + 5) + mat1(base1 + 8) * mat2(base2 + 6) + mat1(base1 + 12) * mat2(base2 + 7)
      resMat(base3 + 8) = mat1(base1) * mat2(base2 + 8) + mat1(base1 + 4) * mat2(base2 + 9) + mat1(base1 + 8) * mat2(base2 + 10) + mat1(base1 + 12) * mat2(base2 + 11)
      resMat(base3 + 12) = mat1(base1) * mat2(base2 + 12) + mat1(base1 + 4) * mat2(base2 + 13) + mat1(base1 + 8) * mat2(base2 + 14) + mat1(base1 + 12) * mat2(base2 + 15)
      resMat(base3 + 1) = mat1(base1 + 1) * mat2(base2) + mat1(base1 + 5) * mat2(base2 + 1) + mat1(base1 + 9) * mat2(base2 + 2) + mat1(base1 + 13) * mat2(base2 + 3)
      resMat(base3 + 5) = mat1(base1 + 1) * mat2(base2 + 4) + mat1(base1 + 5) * mat2(base2 + 5) + mat1(base1 + 9) * mat2(base2 + 6) + mat1(base1 + 13) * mat2(base2 + 7)
      resMat(base3 + 9) = mat1(base1 + 1) * mat2(base2 + 8) + mat1(base1 + 5) * mat2(base2 + 9) + mat1(base1 + 9) * mat2(base2 + 10) + mat1(base1 + 13) * mat2(base2 + 11)
      resMat(base3 + 13) = mat1(base1 + 1) * mat2(base2 + 12) + mat1(base1 + 5) * mat2(base2 + 13) + mat1(base1 + 9) * mat2(base2 + 14) + mat1(base1 + 13) * mat2(base2 + 15)
      resMat(base3 + 2) = mat1(base1 + 2) * mat2(base2) + mat1(base1 + 6) * mat2(base2 + 1) + mat1(base1 + 10) * mat2(base2 + 2) + mat1(base1 + 14) * mat2(base2 + 3)
      resMat(base3 + 6) = mat1(base1 + 2) * mat2(base2 + 4) + mat1(base1 + 6) * mat2(base2 + 5) + mat1(base1 + 10) * mat2(base2 + 6) + mat1(base1 + 14) * mat2(base2 + 7)
      resMat(base3 + 10) = mat1(base1 + 2) * mat2(base2 + 8) + mat1(base1 + 6) * mat2(base2 + 9) + mat1(base1 + 10) * mat2(base2 + 10) + mat1(base1 + 14) * mat2(base2 + 11)
      resMat(base3 + 14) = mat1(base1 + 2) * mat2(base2 + 1) + mat1(base1 + 6) * mat2(base2 + 13) + mat1(base1 + 10) * mat2(base2 + 14) + mat1(base1 + 14) * mat2(base2 + 15)
      resMat(base3 + 3) = mat1(base1 + 3) * mat2(base2) + mat1(base1 + 7) * mat2(base2 + 1) + mat1(base1 + 11) * mat2(base2 + 2) + mat1(base1 + 15) * mat2(base2 + 3)
      resMat(base3 + 7) = mat1(base1 + 3) * mat2(base2 + 4) + mat1(base1 + 7) * mat2(base2 + 5) + mat1(base1 + 11) * mat2(base2 + 6) + mat1(base1 + 15) * mat2(base2 + 7)
      resMat(base3 + 11) = mat1(base1 + 3) * mat2(base2 + 8) + mat1(base1 + 7) * mat2(base2 + 9) + mat1(base1 + 11) * mat2(base2 + 10) + mat1(base1 + 15) * mat2(base2 + 11)
      resMat(base3 + 15) = mat1(base1 + 3) * mat2(base2 + 12) + mat1(base1 + 7) * mat2(base2 + 13) + mat1(base1 + 11) * mat2(base2 + 14) + mat1(base1 + 15) * mat2(base2 + 15)
      ctr -= 1
    }
  }

  def mult4by4VecRange(
    count: Int,
    mat: Array[Double], offset1: Int,
    vec: Array[Double], offset2: Int,
    resVec: Array[Double], offset3: Int) {
    var ctr = count - 1
    while (ctr >= 0) {
      val base0 = ctr << 2
      val base1 = offset1
      val base2 = base0 + offset2
      val base3 = base0 + offset3
      resVec(base3) = mat(base1) * vec(base2) + mat(base1 + 4) * vec(base2 + 1) + mat(base1 + 8) * vec(base2 + 2) + mat(base1 + 12) * vec(base2 + 3)
      resVec(base3 + 1) = mat(base1 + 1) * vec(base2) + mat(base1 + 5) * vec(base2 + 1) + mat(base1 + 9) * vec(base2 + 2) + mat(base1 + 13) * vec(base2 + 3)
      resVec(base3 + 2) = mat(base1 + 2) * vec(base2) + mat(base1 + 6) * vec(base2 + 1) + mat(base1 + 10) * vec(base2 + 2) + mat(base1 + 14) * vec(base2 + 3)
      resVec(base3 + 3) = mat(base1 + 3) * vec(base2) + mat(base1 + 7) * vec(base2 + 1) + mat(base1 + 11) * vec(base2 + 2) + mat(base1 + 15) * vec(base2 + 3)
      ctr -= 1
    }
  }

  def mult4by4Worker(m: Array[Double], targ: Array[Double], res: Array[Double], procs: Int = -1) {
    // println("execing mult4by4Worker")
    val scheduler = procs match {
      case -1 => new Scheduler[Double](targ, 16)
      case x => new Scheduler[Double](targ, 16, x)
    }
    scheduler.start
    scheduler ! Init((idx: Tuple2[Int, Int]) => mult4by4Range(idx._2 - idx._1, m, 0, targ, idx._1, res, idx._1))
    // simulate an actor that waits for the search to complete
    while (!scheduler.finished) {
      Thread.sleep(50)
    }
  }

  def spanIndicies(total: Int, pieces: Int): Array[Tuple2[Int, Int]] = {
    val span = total / pieces
    val arry = new Array[Tuple2[Int, Int]](total / span)
    var offset = 0
    var tupleIdx = 0
    while (offset < pieces) {
      arry(tupleIdx) = (offset * span, if (offset < pieces - 1) (offset + 1) * span else total)
      offset += 1
      tupleIdx += 1
    }
    arry
  }

  def mult4by4Threaded(m: Array[Double], targ: Array[Double], res: Array[Double], procs: Int = 1)(implicit threading: ThreadStrategy = SameThreadStrategy) {
    val l = targ.length / m.length
    //  println("execing mult4by4Threaded over " + spanIndicies(l, procs).mkString("\n"))

    spanIndicies(l, procs).map((idx) =>
      () => mult4by4Range(
        idx._2 - idx._1,
        m, 0,
        targ, idx._1,
        res, idx._1)).map(threading.execute(_)).map(_())
  }

  def mult4by4VecThreaded(m: Array[Double], vec: Array[Double], res: Array[Double], procs: Int = 1)(implicit threading: ThreadStrategy = SameThreadStrategy) {
    val l = vec.length / java.lang.Double.SIZE / 8

    spanIndicies(l, procs).map((idx) =>
      () => mult4by4VecRange(
        idx._2 - idx._1,
        m, 0,
        vec, idx._1,
        res, idx._1)).map(threading.execute(_)).map(_())
  }

  def fill(src: MatrixD, dest: Array[Double]) = {
    val l = src.elements.length
    require(dest.length % l == 0)
    var offset = dest.length / l - 1
    while (offset >= 0) {
      src.elements.copyToArray(dest, offset * l)
      offset -= 1
    }
  }

  // TODO: finish / generalize this
  def mapFeature(x1: MatrixD, x2: MatrixD, degree: Int = 6): MatrixD = {
    require(x1.dims() == x2.dims(), x1.dims() + "!=" + x2.dims())
    require(x1.nCols == 1, "must be column vecs")

    var res = MatrixD.ones(x1.nRows, 1)
    var i = 1
    var j = 0
    while (i <= degree) {
      j = 0
      while (j <= i) {
        res = res ++ x1.elementOp(math.pow(_, i - j)) ** x2.elementOp(math.pow(_, j))
        j += 1
      }
      i += 1
    }
    res
  }

  def copy(a: Array[MatrixD]) = {
    val res = new Array[MatrixD](a.length)
    var i = a.length - 1
    while (i > -1) {
      res(i) = a(i).clone
      i -= 1
    }
    res
  }

  def aggregate(m: Int, n: Int, efforts: Array[Future[MatrixD]]): MatrixD = {
    var i = 0
    var s = MatrixD.zeros(m, n)
    while (i < efforts.length) {
      s = s + efforts(i).get
      i += 1
    }
    s
  }

}

/*
 * The array backed,  double precision version
 */
import MatrixD.verbose
@SerialVersionUID(1l) case class MatrixD(val elements: Array[Double], var nCols: Int, val txpM: MatrixD, transpose: Boolean) {
  if (nCols != 0) require(elements.length % nCols == 0, "length " + elements.length + " not integrally divisible by col spec " + nCols)
  var nRows: Int = if (elements.isEmpty) 0 else elements.length / nCols

  @transient val txp = if (txpM != null) new Concurrent.FutureIsNow(txpM) else if (transpose) Concurrent.effort(transposeDc) else null
  @transient lazy val inv = Concurrent.effort(_inverseDc)
  var oldCols = -1
  var oldRows = -1
  def this(els: Array[Double], cols: Int, transpose: Boolean = true) {
    this(els, cols, null, transpose)
  }

  def same(o: MatrixD, maxError: Double = 0): Boolean = {
    if (nCols == o.nCols) {
      val l = elements.length
      if (l == o.elements.length) {
        var i = 0
        var exceededError = false
        while (!exceededError && i < l) {
          if (math.abs(elements(i) - o.elements(i)) > maxError) {
            exceededError = true
          }
          i += 1
        }
        !exceededError
      } else
        false
    } else
      false
  }

  def sameSqrDiff(o: MatrixD, maxError: Double = 0): Boolean = {
    if (nCols == o.nCols) {
      val l = elements.length
      l == o.elements.length && sumSquaredDiffs(o) / l <= maxError
    } else
      false
  }

  @inline def validIndicesQ(row: Int, col: Int) {
    require(col > 0 && col <= nCols && row <= nRows && row > 0, "index (" + row + ", " + col + ") out of bounds [1," + nRows + "],[1," + nCols + "]")
  }
  @inline def validColQ(col: Int) = require(col > 0 && col <= nCols, "column " + col + " must be 1 to " + nCols)
  @inline def validRowQ(row: Int) = require(row > 0 && row <= nRows, "row " + row + " must be 1 to " + nRows)

  @inline def deref(row: Int, col: Int) = (row - 1) * nCols + col - 1
  @inline def enref(idx: Int): (Int, Int) = {
    if (idx >= nCols) {
      (idx / nCols + 1, idx % nCols + 1)
    } else {
      (1, idx + 1)
    }
  }

  override def clone = {
    new MatrixD(elements.clone(), nCols, if (txp != null) txp.get else null, true)
  }

  //     Concurrent.combine(Concurrent.distribute(l, matrixOpChunk(f, elements, o.elements, l, c, nRows)))
  def averageChunk(te: Array[Double], mus: Array[Double])(range: (Long, Long))() = {
    var i = range._1.asInstanceOf[Int]
    val end = range._2.asInstanceOf[Int]
    var j = 0
    var rowOff = 0
    while (i <= end) {
      j = 0
      rowOff = i * nRows
      while (j < nRows) {
        mus(i) += te(rowOff + j)
        j += 1
      }
      mus(i) /= nRows
      i += 1
    }
    i
  }

  def featureAveragesDc: Array[Double] = {
    val te = tN.elements // each row holds all samples of ith feature
    val l = nCols
    var mus = Array.fill(nCols)(0d) // will hold avg of each feature
    Concurrent.combine(Concurrent.distribute(nCols, averageChunk(te, mus)))
    mus
  }

  def featureAverages: Array[Double] = {
    val te = tN.elements // each row holds all samples of ith feature
    val l = te.length
    var mus = Array.fill(nCols)(0d) // will hold avg of each feature
    var i = 0
    while (i < l) {
      mus(i / nRows) += te(i)
      i += 1
    }
    mus / nRows
  }

  def varianceChunk(te: Array[Double], sigmas: Array[Double], mus: Array[Double])(range: (Long, Long))() = {
    var i = range._1.asInstanceOf[Int]
    val end = range._2.asInstanceOf[Int]
    var p = 0d
    var rowOff = 0
    var j = 0
    while (i <= end) {
      j = 0
      rowOff = i * nRows
      while (j < nRows) {
        p = te(rowOff + j) - mus(i)
        sigmas(i) += p * p
        j += 1
      }
      sigmas(i) = math.sqrt(sigmas(i) / nRows)
      i += 1
    }
    i
  }

  def varianceDc(mus: Array[Double]): Array[Double] = {
    var sigmas = Array.fill(nCols)(0d)
    val te = tN.elements
    Concurrent.combine(Concurrent.distribute(nCols, varianceChunk(te, sigmas, mus)))
    sigmas
  }

  def variance(mus: Array[Double]): Array[Double] = {
    var sigmas = Array.fill(nCols)(0d)
    val l = nRows * nCols
    var i = l - 1
    var t = 0d
    while (i > -1) {
      t = elements(i) - mus(i % nCols)
      sigmas(i % nCols) += t * t
      i -= 1
    }
    i = 0
    while (i < nCols) {
      sigmas(i) = math.sqrt(sigmas(i) / nRows)
      i += 1
    }
    sigmas
  }

  def range(): (Double, Double) = {
    var min = Double.MaxValue
    var max = Double.MinValue
    var s = 0d
    var i = 0
    while (i < elements.length) {
      s = elements(i)
      if (s < min) {
        min = s
      }
      if (s > max) {
        max = s
      }
      i += 1
    }
    (min, max)
  }

  def featureMinMax(): Array[(Double, Double)] = {
    val res = Array.fill(nCols)((Double.MaxValue, Double.MinValue))
    var i = 0
    var j = 0
    var s = 0d
    var offset = 0
    var tup: (Double, Double) = null
    while (i < nRows) {
      j = 0
      offset = i * nCols
      while (j < nCols) {
        s = elements(offset + j)
        tup = res(j)
        if (s < tup._1) {
          res(j) = (s, tup._2)
        }
        if (s > tup._2) {
          res(j) = (tup._1, s)
        }
        j += 1
      }
      i += 1
    }
    res
  }

  //  Xi - μi
  //  -------
  //     σ
  def normalize: MatrixD = {
    val mus = featureAveragesDc
    var i = 0
    val l = elements.length
    var e = elements.clone()
    // for each feature of each sample, subtract feature mean and divide by standard deviation of feature  
    i = 0
    while (i < l) {
      e(i) -= mus(i % nCols)
      i += 1
    }
    e = e / Math.stdDc(e)
    new MatrixD(e, nCols)
  }

  private def normalizeChunk(e: Array[Double], mus: Array[Double])(range: (Long, Long))() = {
    var i = range._1.asInstanceOf[Int]
    val end = range._2.asInstanceOf[Int]
    while (i <= end) {
      e(i) -= mus(i % nCols)
      i += 1
    }
    i
  }
  
  private def normalizeSqrChunk(e: Array[Double], mus: Array[Double])(range: (Long, Long))() = {
    var i = range._1.asInstanceOf[Int]
    val end = range._2.asInstanceOf[Int]
    var v = 0d
    while (i <= end) {
      v = e(i) - mus(i % nCols)
      e(i) = v*v
      i += 1
    }
    i
  }

  def normalizeDc: MatrixD = {
    val mus = featureAveragesDc
    var i = 0
    val l = elements.length
    var e = elements.clone()
    Concurrent.combine(Concurrent.distribute(l, normalizeChunk(e, mus)))
    val stdDev = Math.stdDc(e)
    Concurrent.combine(Concurrent.distribute(l, Math.divChunk(e, stdDev)))
    new MatrixD(e, nCols)
  }
  
  def normalizeWithDc(mus : Array[Double]): MatrixD = {
    var i = 0
    val l = elements.length
    var e = elements.clone()
    Concurrent.combine(Concurrent.distribute(l, normalizeChunk(e, mus)))
    new MatrixD(e, nCols)
  }

  def normalizeSqrWithDc(mus : Array[Double]): MatrixD = {
    var i = 0
    val l = elements.length
    var e = elements.clone()
    Concurrent.combine(Concurrent.distribute(l, normalizeSqrChunk(e, mus)))
    new MatrixD(e, nCols)
  }

  @inline def negateIp: MatrixD = {
    var i = elements.length - 1
    while (i >= 0) {
      elements(i) = -elements(i)
      i -= 1
    }
    if (txp != null) txp.get.negateIp
    this
  }

  @inline def negateNSlow = {
    val cl = elements.clone
    var txcl = if (txp != null) txp.get.elements else null
    var i = elements.length - 1
    while (i >= 0) {
      cl(i) = -elements(i)
      if (txcl != null) txcl(i) = -txp.get.elements(i)
      i -= 1
    }
    new MatrixD(cl, nCols, new MatrixD(txcl, nRows, false), txcl == null)
  }

  def negateN = {
    val cl = elements.clone
    Concurrent.combine(Concurrent.distribute(cl.length, Math.negateChunk(cl)))
    var txcl = if (txp != null) txp.get.elements.clone else null
    if (txcl != null) {
      Concurrent.combine(Concurrent.distribute(cl.length, Math.negateChunk(txcl)))
    }
    new MatrixD(cl, nCols, if (txcl != null) new MatrixD(txcl, nRows, false) else null, txcl == null)
  }

  @inline def sumDc() = Math.sumDc(elements)
  @inline def sum() = { var s = 0d; var i = 0; while (i < elements.length) { s += elements(i); i += 1 }; s }

  def lengthDc = math.sqrt(Concurrent.aggregateD(Concurrent.distribute(elements.length, Math.lengthSquaredChunk(elements))))

  def length = {
    var s = 0d
    var v = 0d
    var i = elements.length - 1
    while (i > -1) {
      v = elements(i)
      s += v * v
      i -= 1
    }
    math.sqrt(s)
  }

  def unitV = {
    val s = lengthDc
    val el = elements.clone
    var v = 0d
    var i = elements.length - 1
    while (i > -1) {
      el(i) /= s
    }
    new MatrixD(el, nCols, txp != null)
  }

  def unitVdc() = {
    val s = lengthDc
    val el = elements.clone
    Concurrent.combine(Concurrent.distribute(el.length, Math.divChunk(el, s)))
    new MatrixD(el, nCols, txp != null)
  }

  def autoDot() = Concurrent.aggregateD(Concurrent.distribute(elements.length, Math.sumSqrChunk(elements)))

  // returns a column matrix where each row contains the index of the largest column 
  def maxColIdxs(): MatrixD = {
    val a = MatrixD.zeros(nRows, 1)
    Concurrent.combine(Concurrent.distribute(nRows, Math.maxColIdxChunk(elements, nCols, a.elements)))
    a
  }

  @inline def apply(row: Int, col: Int): Double = {
    validIndicesQ(row, col)
    elements(deref(row, col))
  }

  def update(row: Int, col: Int, v: Double) { elements(deref(row, col)) = v }

  def addBiasCol() = MatrixD.ones(nRows, 1) ++ this

  def hasBiasCol() = nCols > 1 && !columnVector(1).elements.exists(_ != 1)

  def toBinaryCategoryMatrix(): MatrixD = {
    val s = elements.toSet
    val max = s.max
    val newCols = s.size
    val bmat = MatrixD.zeros(nRows, newCols)
    var i = 0
    var off = 0
    while (i < nRows) {
      off = apply(i + 1, 1).asInstanceOf[Int] - 1
      // println("changing " + (i * newCols + off ))
      bmat.elements(i * newCols + off) = 1
      i += 1
    }
    bmat
  }
  
  def toColumnSumVector() : MatrixD = {
    val res = MatrixD.zeros(nRows,1)
    var i = 0
    var j = 0
    var offset = 0
    while(i<nRows) {
      j= 0
      offset =  i * nCols
      while(j < nCols) {
        res.elements(i) += elements(offset + j)
        j+=1
      }
      i+=1
    }
    res
  }

  def isBinaryCategoryMatrix = !elements.exists(x => x != 0 && x != 1)

  def poseAsRow() = {
    oldRows = nRows
    nRows = 1
    nCols *= oldRows
    this
  }

  def poseAsCol() = {
    oldCols = nCols
    nCols = 1
    nRows *= oldCols
    this
  }

  def unPose() = {
    if (oldRows != -1 && oldCols == -1) {
      nRows = oldRows
      nCols /= oldRows
      oldRows = -1
    } else if (oldRows == -1 && oldCols != -1) {
      nCols = oldCols
      nRows /= oldCols
      oldCols = -1
    }
    this
  }

  def reshape(rows: Int, cols: Int, offset: Long = 0): MatrixD = {
    val l = rows * cols
    val nelems = new Array[Double](l)
    Array.copy(elements, offset.asInstanceOf[Int], nelems, 0, l)
    new MatrixD(nelems, cols)
  }
  def redimension(dims: (Int, Int), offset: Long = 0): MatrixD = reshape(dims._1, dims._2, offset)

  def dropFirst() = {
    val nelems = new Array[Double](nCols * nRows - nRows)
    var i = 0
    while (i < nRows) {
      Array.copy(elements, i * nCols + 1, nelems, i * (nCols - 1), nCols - 1)
      i += 1
    }
    new MatrixD(nelems, nCols - 1)
  }

  private def matrixOpIp(o: MatrixD, f: (Double, Double) => Double): MatrixD = {
    var l = elements.length
    require(l == o.elements.length && nCols == o.nCols, "sizes don't match")
    l -= 1
    while (l >= 0) {
      elements(l) = f(elements(l), o.elements(l))
      l -= 1
    }
    this
  }

  private def matrixOpN(o: MatrixD, f: (Double, Double) => Double): MatrixD = {
    var l = elements.length
    require(l == o.elements.length && nCols == o.nCols, "this: elements " + l + ", cols " + nCols + " vs o: elements " + o.elements.length + ", cols " + o.nCols + " sizes or dims don't match")
    val c = new Array[Double](l)
    l -= 1
    while (l >= 0) {
      c(l) = f(elements(l), o.elements(l))
      l -= 1
    }
    new MatrixD(c, nCols)
  }

  def matrixOpChunk(f: (Double, Double) => Double, src1: Array[Double], src2: Array[Double],
    len: Long, trg: Array[Double], rows: Int)(range: Tuple2[Long, Long])(): Long = {
    //println("txChunk range " + range)
    var i: Long = range._1
    while (i <= range._2) {
      trg(i.asInstanceOf[Int]) = f(src1(i.asInstanceOf[Int]), src2(i.asInstanceOf[Int]))
      i += 1
    }
    i
  }

  private def matrixOpDc(o: MatrixD, f: (Double, Double) => Double): MatrixD = {
    var l = elements.length
    require(l == o.elements.length && nCols == o.nCols, "this: elements " + l + ", cols " + nCols + " vs o: elements " + o.elements.length + ", cols " + o.nCols + " sizes or dims don't match")
    val c = new Array[Double](l)

    Concurrent.combine(Concurrent.distribute(l, matrixOpChunk(f, elements, o.elements, l, c, nRows)))
    new MatrixD(c, nCols)
  }

  def boolOpChunk(f: (Double, Double) => Boolean, oe: Array[Double])(range: (Long, Long))() = {
    var i = range._1.asInstanceOf[Int]
    val end = range._2.asInstanceOf[Int]
    var test = true
    while (test && i <= end) {
      if (!f(elements(i), oe(i))) {
        test = false
      }
      i += 1
    }
    test
  }

  def binBoolOpDc(f: (Double, Double) => Boolean, other: MatrixD): Boolean = {
    val futs = Concurrent.distribute(elements.length, boolOpChunk(f, other.elements))
    var i = 0
    var greater = true
    while (i < futs.length) {
      greater &= futs(i).get()
      i += 1
    }
    greater
  }

  def countBoolOpChunk(f: (Double) => Boolean)(range: (Long, Long))() = {
    var i = range._1.asInstanceOf[Int]
    val end = range._2.asInstanceOf[Int]
    var count = 0
    while (i <= end) {
      if (f(elements(i))) {
        count +=1
      }
      i += 1
    }
    count
  }

  
  def countBoolOpDc(f: (Double) => Boolean) = Concurrent.aggregateD(Concurrent.distribute(elements.length, countBoolOpChunk(f)))
  

  def rowsWhereChunk(f: (Array[Double] ) => Boolean)(range : (Long,Long))() = {
    var i = range._1.asInstanceOf[Int]
    val end = range._2.asInstanceOf[Int]
    val args = new Array[Double](nCols)
    var l = List[Int]()
    while(i <= end) {
      Array.copy(elements, i * nCols, args,0, nCols)
      if(f(args)) {
        l = l :+ i
      }
      i+=1
    }
    l
  }
  
  def rowsWhere( f: (Array[Double] ) => Boolean) : Array[Int] =
  		Concurrent.aggregateL(Concurrent.distribute(nRows, rowsWhereChunk(f))).asInstanceOf[List[Int]].toArray
  
  def >(o: MatrixD) = binBoolOpDc((a, b) => a > b, o)
  def <(o: MatrixD) = binBoolOpDc((a, b) => a < b, o)
  def `>=`(o: MatrixD) = binBoolOpDc((a, b) => a >= b, o)
  def `<=`(o: MatrixD) = binBoolOpDc((a, b) => a <= b, o)
  def equals(o: MatrixD) = if (o != null) binBoolOpDc((a, b) => a == b, o) else false
  def almostEquals(o: MatrixD, epsilon: Double) = binBoolOpDc((a, b) => math.abs(b - a) <= epsilon, o)
  def ==(o: MatrixD) = equals(o)

  def +(other: MatrixD): MatrixD = matrixOpDc(other, _ + _)
  def -(other: MatrixD): MatrixD = matrixOpDc(other, _ - _)
  def &&(other: MatrixD): MatrixD = matrixOpDc(other, (x,y)=> if(x==1d && y==1d) 1d else 0)
  def hadamardProduct(other: MatrixD) = matrixOpDc(other, _ * _)
  def **(other: MatrixD) = hadamardProduct(other)
  def hadamardQuotient(other: MatrixD) = matrixOpDc(other, _ / _)
  def /(other: MatrixD) = hadamardQuotient(other)

  def squareQ() = nCols == nRows

  /*
   * structural ops
   */
  def dims(): (Int, Int) = {
    Tuple2(nRows, nCols)
  }

  @inline
  def rowIndices(row: Int) = {
    validRowQ(row)
    val start = (row - 1) * nCols
    (start, start + nCols - 1)
  }

  @inline def columnIndices(col: Int): Array[Int] = {
    validColQ(col)
    val c = new Array[Int](nRows)
    var i = 0
    while (i < nRows) {
      c(i) = i * nRows + col - 1
      i += 1
    }
    c
  }

  def copyOfRow(row: Int) = {
    validRowQ(row)
    val c = new Array[Double](nCols)
    var l = nCols - 1
    while (l >= 0) {
      c(l) = elements((row - 1) * nCols + l)
      l -= 1
    }
    c
  }

  def copyOfCol(col: Int) = {
    validColQ(col)
    val c = new Array[Double](nRows)
    var l = nRows - 1
    while (l >= 0) {
      c(l) = elements(l * nCols + col - 1)
      l -= 1
    }
    c
  }

  def columnVector(col: Int) = {
    new MatrixD(copyOfCol(col), 1, false)
  }

  def rowVector(row: Int) = {
    new MatrixD(copyOfRow(row), nCols, false)
  }

  def rowVectorQ = nRows == 1

  def columnVectorQ = nCols == 1

  def toRowVector = new MatrixD(elements, elements.length)

  def toColumnVector = new MatrixD(elements, 1)

  def toScalar() = {
    require(nCols == nRows && nRows == 1, "ill-formed scalar with dims " + dims)
    elements(0)
  }
  
  def diagonals() : Array[Double] = {
    require(squareQ)
    val ret = new Array[Double](nCols)
    var i = 0
    while(i < nCols) {
      ret(i) = elements(i * nCols + i)
      i+=1
    }
    ret
  }
  
  def setRows(rows: Array[Int], d : Double) = {
    var i = 0
    var j = 0
    var offset = 0
    while(i < rows.length) {
      offset = rows(i) * nCols
      j = 0
      while(j < nCols) {
        elements(offset + j) = d
        j+=1
      }
      i+=1
    }
    this
  }

  def columnSubset(indices: Array[Int]) = {
    // delay precomputing tx until matrix is complete
    var i = 0
    var res: MatrixD = MatrixD.zeros(0, 0)
    while (i < indices.size) {
      val cVec = columnVector(indices(i))
      if (res.dims == (0, 0)) {
        res = cVec
      } else {
        res = res ++ cVec
      }
      i += 1
    }
    new MatrixD(res.elements, res.nCols, true)
  }

  def clippedRowSubset(r: Array[Int], colRange: (Int, Int)) = {
    require(colRange._1 > -1 && colRange._2 < nCols, "bad column range")
    val newM = r.length
    val res = MatrixD.zeros(newM, colRange._2 - colRange._1 + 1)
    val (m, n) = res.dims()
    val b = res.elements
    var i = 0
    var j = 0
    var boff = 0
    var eoff = 0
    while (i < newM) {
      j = colRange._1
      boff = i * n - colRange._1
      eoff = r(i) * nCols
      while (j <= colRange._2) {
        b(boff + j) = elements(eoff + j)
        j += 1
      }
      i += 1
    }
    res
  }

  def rowSubset(indices: Array[Int]) = {
    var i = 0
    var res: MatrixD = MatrixD.zeros(0, 0)
    while (i < indices.size) {
      val rVec = rowVector(indices(i))
      if (res.dims() == (0, 0)) {
        res = rVec
      } else {
        res = res +/ rVec
      }
      i += 1
    }
    res
  }

  def copyRowChunk(e: Array[Double], indices: Array[Int])(range: (Long, Long))() = {
    val end = range._2.asInstanceOf[Int]
    var i = range._1.asInstanceOf[Int]
    var j = 0
    var soff = 0
    var toff = 0
    while (i <= end) {
      j = 0
      soff = indices(i) * nCols
      toff = i * nCols
      while (j < nCols) {
        e(toff + j) = elements(soff + j)
        j += 1
      }
      i += 1
    }
  }

  def rowSubsetDc(indices: Array[Int]) = {
    var i = 0
    val e = new Array[Double](indices.length * nCols)
    Concurrent.combine(Concurrent.distribute(indices.length, copyRowChunk(e, indices)))
    new MatrixD(e, nCols)
  }

  def copyRange(src: Array[Double], targ: Array[Double], range: Tuple2[Int, Int], start: Int) {
    require(range._1 > -1 && range._2 < src.length, "range " + range + " bad for source")
    require(start + range._2 - range._1 < targ.length, "range (" + (range._2 - range._1) + ") + start (" + start + ") bad for target length " + targ.length)
    var l = range._2 - range._1
    while (l >= 0) {
      targ(start + l) = src(range._1 + l)
      l -= 1
    }
  }

  @inline def rowCopy(out: Array[Double], row: Int, startIdx: Int) = {
    copyRange(elements, out, rowIndices(row), startIdx)
  }

  def toArrayArray(): Array[Array[Double]] = {
    var ir = 0
    val out = new Array[Array[Double]](nRows)
    while (ir < nRows) {
      val line = new Array[Double](nCols)
      Array.copy(elements, ir * nCols, line, 0, nCols)
      out(ir) = line
      ir += 1
    }
    out
  }

  val df = new java.text.DecimalFormat("0.0000E00")

  def octStr(): String = {
    if (verbose || (nRows < 11 && nCols < 11)) {
      val sb = new java.lang.StringBuilder
      var l = List[String]()
      var row = 0
      var col = 0
      var off = 0
      while (row < nRows) {
        col = 0
        sb.setLength(0)
        off = row * nCols
        while (col < nCols) {
          sb.append(df.format(elements(off + col)))
          if (col < nCols - 1) {
            sb.append("  ")
          }
          col += 1
        }
        l = l :+ sb.toString()
        row += 1
      }
      l.mkString("", "\n", "\n")
    } else {
      "MatrixD[" + nRows + "," + nCols + "]"
    }
  }

  override def toString(): String = {
    if (verbose || (nRows < 11 && nCols < 11)) {
      val rowA = new Array[Double](nCols)
      var row = nRows
      var l = List[String]()
      while (row > 0) {
        rowCopy(rowA, row, 0)
        l = l :+ rowA.mkString("[", ", ", "]")
        row -= 1
      }
      l.reverse.mkString("", "\n", "\n")
    } else {
      "MatrixD[" + nRows + "," + nCols + "]"
    }
  }

  def rightConcatenate(other: MatrixD): MatrixD = {
    require(other.nRows == nRows, "can only right-concatenate matrices of equal row count")
    val newCols = nCols + other.nCols
    val c = new Array[Double](nRows * newCols)
    var row = nRows
    while (row > 0) {
      rowCopy(c, row, (row - 1) * newCols)
      other.rowCopy(c, row, (row - 1) * newCols + nCols)
      row -= 1
    }
    new MatrixD(c, newCols)
  }

  def ++(other: MatrixD) = rightConcatenate(other)

  def bottomConcatenate(other: MatrixD): MatrixD = {
    require(other.nCols == nCols, "can only bottom-concatenate matrices of equal column count")
    val newRows = nRows + other.nRows
    val c = new Array[Double](nCols * newRows)
    elements.copyToArray(c)
    other.elements.copyToArray(c, elements.length)
    new MatrixD(c, nCols)
  }

  def +/(other: MatrixD) = bottomConcatenate(other)

  def prependColumnNew(col: Array[Double]): MatrixD = {
    require(col.length == nRows, "new column doesn't fit matrix")
    new MatrixD(col, 1) ++ this
  }

  def appendColumnNew(col: Array[Double]): MatrixD = {
    require(col.length == nRows, "new column doesn't fit matrix")
    this ++ new MatrixD(col, 1)
  }

  def prependRowNew(row: Array[Double]): MatrixD = {
    require(row.length == nCols, "new row doesn't fit matrix")
    new MatrixD(row, nCols) +/ this
  }

  def appendRowNew(row: Array[Double]): MatrixD = {
    require(row.length == nCols, "new row doesn't fit matrix")
    this +/ new MatrixD(row, nCols)
  }

  def flipud = {
    val c = new Array[Double](nCols)
    var row = 1
    while (row <= nRows / 2) {
      // swap row, nRows-row
      rowCopy(c, row, 0)
      rowCopy(elements, nRows - row + 1, (row - 1) * nCols)
      c.copyToArray(elements, (nRows - row) * nCols)
      row += 1
    }
  }

  def tN() = transposeN()
  def transposeN(): MatrixD = {
    MatrixD.txpsUseCount += 1
    if (txp != null) {
      // println(dims + " cached !!! txps")
      txp.get
    } else {
      //  println(dims + " had no cached txps")
      transposeDc
    }
  }

  //  def transposeChunky() : MatrixD = {
  //    
  //  }

  def transposeEl(): MatrixD = {
    MatrixD.txpsCreateCount += 1
    val l: Long = elements.length - 1
    val b = new Array[Double]((l + 1).asInstanceOf[Int])

    // first and last elems unchanged
    b(0) = elements(0)
    b(l.asInstanceOf[Int]) = elements(l.asInstanceOf[Int])
    var i: Long = 1
    while (i < l) {
      b((i * nRows % l).asInstanceOf[Int]) = elements(i.asInstanceOf[Int])
      i += 1
    }
    val ret = new MatrixD(b, nRows, this, false)
    //println("transposeEl b.length " + b.length + " cols " + nRows)
    ret
  }

  def transposeChunk(src: Array[Double], len: Long, trg: Array[Double], rows: Int)(range: Tuple2[Long, Long])(): Long = {
    var i: Long = range._1
    while (i <= range._2) {
      trg((i * rows % len).asInstanceOf[Int]) = src(i.asInstanceOf[Int])
      i += 1
    }
    i
  }

  def transposeDc(): MatrixD = {
    MatrixD.txpsCreateCount += 1
    val l: Long = elements.length - 1
    val b = new Array[Double]((l + 1).asInstanceOf[Int])
    b(l.asInstanceOf[Int]) = elements(l.asInstanceOf[Int])
    Concurrent.combine(Concurrent.distribute(l, transposeChunk(elements, l, b, nRows)))
    new MatrixD(b, nRows, this, false)
  }

  def transposeSlow(): MatrixD = {
    val res = MatrixD.zeros(nCols, nRows)
    var i = 1
    var j = 1
    while (i < nRows) {
      j = 1
      while (j < nCols) {
        res.elements(res.deref(j, i)) = elements(deref(i, j))
        j += 1
      }
      i += 1
    }
    res
  }

  def transposeIp(m: java.util.Map[Int, Double] = null): MatrixD = if (nCols == nRows) transposeSquareIp else transposeNsIp(m)

  // about 37% faster than txNew
  def transposeSquareIp(): MatrixD = {
    val l = elements.length - 1
    var idx = 1 // can skip first and last elements
    var temp = 0.
    var imod = 0
    var idiv = 1
    var oppIdx = 0
    while (idx < l) {
      idiv = idx / nCols
      imod = idx % nCols
      if (imod > idiv) { // stay on top triangle and skip diagonals
        oppIdx = imod * nCols + idiv
        temp = elements(idx)
        elements(idx) = elements(oppIdx)
        elements(oppIdx) = temp
      }
      idx += 1
    }
    this
  }

  def transposeNsIp(m: java.util.Map[Int, Double] = null): MatrixD = {
    val l = elements.length - 1
    var i = 1
    var idx = i
    val map: java.util.Map[Int, Double] = if (m == null) new java.util.HashMap else { m.clear; m }
    var cachedElem: Any = null

    while (i < l) {
      idx = i * nRows % l
      if (idx > i) {
        // store original content
        map.put(idx, elements(idx))
      }
      cachedElem = map.get(i)
      elements(idx) = if (cachedElem != null) cachedElem.asInstanceOf[Double] else elements(i)
      i += 1
    }
    if (nCols != nRows) {
      val temp = nCols
      nCols = nRows
      nRows = temp
    }
    this
  }

  @inline def *(o: MatrixD): MatrixD = multDc(o)

  def multSequential(o: MatrixD): MatrixD = {
    require(nCols == o.nRows, "matrices " + dims() + " and " + o.dims() + " of incompatible shape for multiplication")
    val c = new Array[Double](nRows * o.nCols)
    val oT = o.transposeN
    var row = 1
    var rowT = 1
    var idx = 0
    while (row <= nRows) {
      rowT = 1
      //println("row " + row + " idx " + idx)
      while (rowT <= oT.nRows) {
        c(idx) = MatrixD.dot(elements, rowIndices(row), oT.elements, oT.rowIndices(rowT))
        rowT += 1
        idx += 1
      }
      row += 1
    }
    new MatrixD(c, o.nCols)
  }

  
  def multSeqNoTn(o: MatrixD): MatrixD = {
    require(nCols == o.nRows, "matrices " + dims() + " and " + o.dims() + " of incompatible shape for multiplication")
    val c = new Array[Double](nRows * o.nCols)
    var i = 0
    var j = 0
    var k = 0
    var sum = 0d
    var a = 0d
    var b = 0d
    var ioff = 0
    var oioff = 0
    while (i < nRows) {
      j = 0
      ioff = i * nCols
      oioff = i * o.nCols
      while (j < o.nCols) {
        k = 0
        sum = 0
        while(k < nCols) {
        	a = elements(ioff + k)
        	b = o.elements(k * o.nCols + j)
        	sum += a *b
        	k +=1
        }
        c(oioff + j ) = sum
        j +=1
      }
      i += 1
    }
    new MatrixD(c, o.nCols)
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

  def multDc(o: MatrixD): MatrixD = {
    require(nCols == o.nRows, "matrices " + dims() + " and " + o.dims() + " of incompatible shape for multiplication")
    val oT = o.transposeN
    val c = new Array[Double](nRows * o.nCols)
    Concurrent.combine(Concurrent.distribute(nRows, multChunk(elements, nCols, oT.elements, oT.nRows, oT.nCols, c), true))
    new MatrixD(c, o.nCols)
  }

  def slowMult(o: MatrixD): MatrixD = {
    val rRows = nRows
    val rCols = o.nCols
    val res = MatrixD.zeros(rRows, rCols)
    var i = 0
    var j = 0
    var k = 0
    while (i < rRows) {
      j = 0
      while (j < rCols) {
        k = 0
        while (k < nCols) {
          //res.elements( res.deref(i, j)) += elements( deref(i, k)) * o.elements(o.deref(k, j))
          //println( (i * rCols + j) + " " + ( i * nCols + k) + " " +  (k * o.nCols + j))
          res.elements(i * rCols + j) += elements(i * nCols + k) * o.elements(k * o.nCols + j)
          k += 1
        }
        j += 1
      }
      i += 1
    }
    res
  }

  def elOp(f: (Double) => Double) = elementOp(f)
  def elementOp(f: (Double) => Double): MatrixD = {
    var el = elements.clone
    var l = elements.length - 1
    while (l >= 0) {
      el(l) = f(elements(l))
      l -= 1
    }
    new MatrixD(el, nCols, txp != null)
  }

  def elementOpChunk(el: Array[Double], f: (Double) => Double)(range: (Long, Long))() = {
    var l = range._1.asInstanceOf[Int]
    val end = range._2.asInstanceOf[Int]
    while (l <= end) {
      el(l) = f(elements(l))
      l += 1
    }
    l
  }
  def elementOpDc(f: (Double) => Double): MatrixD = {
    var el = elements.clone
    var l = elements.length
    Concurrent.combine(Concurrent.distribute(l, elementOpChunk(el, f)))
    new MatrixD(el, nCols, txp != null)
  }

  def elementScalarOp(s: Double, f: (Double, Double) => Double): MatrixD = {
    val el = elements.clone
    var l = elements.length - 1
    while (l >= 0) {
      el(l) = f(elements(l), s)
      l -= 1
    }
    new MatrixD(el, nCols, txp != null)
  }

  def elementScalarOpChunk(f: (Double, Double) => Double, src: Array[Double], s: Double, trg: Array[Double])(range: Tuple2[Long, Long])(): Long = {
    //println("txChunk range " + range)
    var i: Long = range._1
    while (i <= range._2) {
      trg(i.asInstanceOf[Int]) = f(src(i.asInstanceOf[Int]), s)
      i += 1
    }
    i
  }

  def elementScalarOpDc(s: Double, f: (Double, Double) => Double): MatrixD = {
    val el = elements.clone
    Concurrent.combine(Concurrent.distribute(el.length, elementScalarOpChunk(f, elements, s, el)))
    new MatrixD(el, nCols, txp != null)
  }

  def +(s: Double) = elementScalarOpDc(s, _ + _)
  def -(s: Double) = elementScalarOpDc(s, _ - _)
  def *(s: Double) = elementScalarOpDc(s, _ * _)
  def /(s: Double) = elementScalarOpDc(s, _ / _)

  def ^(exp: Double) = elementScalarOpDc(exp, (x, y) => scala.math.pow(x, y))
  def log() = elementOpDc(math.log)
  def clean(σ: Double = .0001) = elementScalarOpDc(σ, (x, y) => if (x * x < y * y) 0. else x)

  def filterElements(f: Double => Double): MatrixD = {
    val c = elements.clone
    var i = elements.length - 1
    while (i > -1) {
      c(i) = f(c(i))
      i -= 1
    }
    new MatrixD(c, nCols)
  }

  def sumSquaredDiffs(other: MatrixD): Double = {
    require(dims() == other.dims(), "this " + dims + " dimly incompat w other: " + other.dims)
    var sum = 0d
    val l = elements.length
    var i = 0
    var delta = 0d
    while (i < l) {
      delta = elements(i) - other.elements(i)
      sum += delta * delta
      i += 1
    }
    sum
  }

  def sgn(row: Int, col: Int): Int = {
    validIndicesQ(row, col)
    var l = -1
    var k = 0
    val total = row + col
    while (k <= total) {
      l *= -1
      k += 1
    }
    l
  }

  def minorM(row: Int, col: Int): MatrixD = {
    validIndicesQ(row, col)
    val c = new Array[Double]((nCols - 1) * (nRows - 1))
    var i = 0
    var j = 0
    var rowRange = rowIndices(row)
    while (j < elements.length) {
      if ((j < rowRange._1 || j > rowRange._2) && j % nCols + 1 != col) {
        c(i) = elements(j)
        i += 1
      }
      j += 1
    }
    new MatrixD(c, nCols - 1)
  }

  def minorChunk(c: Array[Double])(row: Int, col: Int): MatrixD = {
    validIndicesQ(row, col)
    val c = new Array[Double]((nCols - 1) * (nRows - 1))
    var i = 0
    var j = 0
    var rowRange = rowIndices(row)
    while (j < elements.length) {
      if ((j < rowRange._1 || j > rowRange._2) && j % nCols + 1 != col) {
        c(i) = elements(j)
        i += 1
      }
      j += 1
    }
    new MatrixD(c, nCols - 1)
  }

  def minorMdc(row: Int, col: Int): MatrixD = {
    validIndicesQ(row, col)
    val c = new Array[Double]((nCols - 1) * (nRows - 1))
    var i = 0
    var j = 0
    var rowRange = rowIndices(row)
    while (j < elements.length) {
      if ((j < rowRange._1 || j > rowRange._2) && j % nCols + 1 != col) {
        c(i) = elements(j)
        i += 1
      }
      j += 1
    }
    new MatrixD(c, nCols - 1)
  }

  def minor(row: Int, col: Int): Double = minorM(row, col).determinant

  def determinant: Double = {
    require(nCols == nRows, "not square")
    nCols match {
      case 1 =>
        elements(0)
      case 2 =>
        elements(0) * elements(3) - elements(1) * elements(2)
      case _ =>
        // cofactor expansion along the first column
        var row = 1
        var sum = 0.
        while (row <= nRows) {
          sum += elements((row - 1) * nCols) * cofactor(row, 1)
          row += 1
        }
        sum
    }
  }

  def determinantChunk(range: (Long, Long))(): Double = {
    var row = range._1.asInstanceOf[Int]
    val end = range._2.asInstanceOf[Int]
    var sum = 0.
    while (row <= end) {
      sum += elements((row - 1) * nCols) * cofactor(row, 1)
      row += 1
    }
    sum
  }

  def determinantDc: Double = {
    require(nCols == nRows, "not square")
    nCols match {
      case 1 =>
        elements(0)
      case 2 =>
        elements(0) * elements(3) - elements(1) * elements(2)
      case _ =>
        // cofactor expansion along the first column
        Concurrent.aggregateD(Concurrent.distribute(nRows, determinantChunk))
    }
  }

  def cofactor(row: Int, col: Int) = minor(row, col) * sgn(row, col)

  def cofactorM() = {
    val c = new Array[Double](elements.length)
    var row = 1
    var col = 1
    var i = 0
    while (row <= nRows) {
      col = 1
      while (col <= nCols) {
        c(i) = cofactor(row, col)
        col += 1
        i += 1
      }
      row += 1
    }
    new MatrixD(c, nCols)
  }

  def cofactorChunk(c: Array[Double])(range: (Long, Long))() = {
    var row = range._1.asInstanceOf[Int]
    val end = range._2.asInstanceOf[Int]
    var col = 1
    var i = (row-1) * nCols
    while (row <= end) {
      col = 1
      while (col <= nCols) {
        c(i) = cofactor(row, col)
        col += 1
        i += 1
      }
      row += 1
    }
    row
  }

  def cofactorMdc() = {
    val c = new Array[Double](elements.length)
    Concurrent.combine(Concurrent.distribute(nRows, cofactorChunk(c),true))
    new MatrixD(c, nCols)
  }

  def cofactorM2() = { // about 3% slower than nested whiles
    val l = elements.length
    val c = new Array[Double](l)
    var i = 0
    while (i < l) {
      c(i) = cofactor(i / nCols + 1, i % nCols + 1)
      i += 1
    }
    new MatrixD(c, nCols)
  }

  def inverse() = inv.get
  def _inverse(): MatrixD = {
    val d = determinant
    require(d != 0, "not linearly independent")
    val mT = cofactorM.transposeN()
    mT / d
  }

  def _inverseDc(): MatrixD = {
    val d = determinantDc
    require(d != 0, "not linearly independent")
    val mT = cofactorMdc.transposeN()
    mT / d
  }

  implicit val matDims = (nRows, nCols)
  implicit def matrixToScalar(): Double = {
    require(nCols == 1 && nRows == 1)
    elements(0)
  }

}
