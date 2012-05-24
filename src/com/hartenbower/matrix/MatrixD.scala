package com.hartenbower.matrix
import scala.util.Random

object MatrixD {
  val verbose = false
  implicit def scalarOp(d: Double) = new ScalarOp(d)

  class ScalarOp(d: Double) {
    def +(matrix: MatrixD): MatrixD = matrix + d
    def -(matrix: MatrixD): MatrixD = matrix.negateN + d
    def /(matrix: MatrixD): MatrixD = matrix / d
    def *(matrix: MatrixD): MatrixD = matrix * d
  }

  def zeros(nRows: Int, nCols: Int): MatrixD = new MatrixD(Array.fill(nRows * nCols)(0d), nCols)
  def zeros(dims: (Int, Int)): MatrixD = zeros(dims._1, dims._2)
  def ones(nRows: Int, nCols: Int): MatrixD = new MatrixD(Array.fill(nRows * nCols)(1d), nCols)
  def ones(dims: (Int, Int)): MatrixD = ones(dims._1, dims._2)

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

  def diagonalM(dim: Int, d: Double = 1.0): MatrixD = {
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

  /* could also define one as transpose of other... */
  def rowMatrix(a: Array[Double]) = new MatrixD(a, a.length)

  def columnMatrix(a: Array[Double]) = new MatrixD(a, 1)
  def randn( dims : Tuple2[Int,Int], epsilon: Double ): MatrixD = randn(dims._1,dims._2,epsilon) 
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

  def mapFeature(x1: MatrixD, x2: MatrixD, degree: Int = 6): MatrixD = {
    require(x1.dims() == x2.dims(), x1.dims() + "!=" + x2.dims())
    require(x1.nCols == 1, "must be column vecs")

    var res = MatrixD.ones(x1.nRows, 1)
    var i = 1
    var j = 0
    while (i <= degree) {
      j = 0
      while(j <= i) {
        res = res ++ x1.elementOp(math.pow(_, i - j)) ** x2.elementOp(math.pow(_, j))
        j += 1
      }
      i += 1
    }
    res
  }
  
}

import MatrixD.verbose
/*
 * The array backed,  double precision version
 */
case class MatrixD(val elements: Array[Double], var nCols: Int) {
  if (nCols != 0) require(elements.length % nCols == 0)
  var nRows: Int = if (elements.isEmpty) 0 else elements.length / nCols

  def validIndicesQ(row: Int, col: Int) {
    require(col > 0 && col <= nCols && row <= nRows && row > 0, "index (" + row + ", " + col + ") out of bounds [1," + nRows + "],[1," + nCols + "]")
  }
  def validColQ(col: Int) = require(col > 0 && col <= nCols, "column " + col + " must be 1 to " + nCols)
  def validRowQ(row: Int) = require(row > 0 && row <= nRows, "row " + row + " must be 1 to " + nRows)

  def deref(row: Int, col: Int) = (row - 1) * nCols + col - 1
  def enref(idx: Int): (Int, Int) = {
    if (idx >= nCols) {
      (idx / nCols + 1, idx % nCols + 1)
    } else {
      (1, idx + 1)
    }
  }

  override def clone = {
    new MatrixD(elements.clone(), nCols)
  }

  def negateIp = {
    var i = elements.length - 1
    while (i >= 0) {
      elements(i) = -elements(i)
      i -= 1
    }
    this
  }
  
  def negateN = {
    val cl = clone
    var i = elements.length - 1
    while (i >= 0) {
      cl.elements(i) = -elements(i)
      i -= 1
    }
    cl
  }
  
  @inline def sum() = {var s = 0d; var i = 0; while (i < elements.length){ s += elements(i); i += 1 }; s }
  
  

  def apply(row: Int, col: Int): Double = {
    validIndicesQ(row, col)
    elements(deref(row, col))
  }

  def addBiasCol() = MatrixD.ones(nRows, 1) rightConcatenate this
  
  def hasBiasCol() = nCols > 1 && !columnVector(1).elements.exists(_!=1)

  def toBinaryCategoryMatrix() : MatrixD = {
    val s = elements.toSet
    val max = s.max
    val newCols = s.size
    val bmat = MatrixD.zeros(nRows, newCols)
    var i = 0
    var off = 0
    while(i < nRows ) {
      off = apply(i+1,1).asInstanceOf[Int] -1
     // println("changing " + (i * newCols + off ))
      bmat.elements(i * newCols + off ) = 1
      i += 1
    }
    bmat
  }
  
  def isBinaryCategoryMatrix = ! elements.exists( x => x != 0 && x != 1)
  
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

  def +(other: MatrixD): MatrixD = matrixOpN(other, _ + _)
  def -(other: MatrixD): MatrixD = matrixOpN(other, _ - _)
  def hadamardProduct(other: MatrixD) = matrixOpN(other, _ * _)
  def **(other: MatrixD) = hadamardProduct(other)

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

  def columnIndices(col: Int): Array[Int] = {
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

  def columnVector(col:Int) = {
    new MatrixD(copyOfCol(col), 1)
  }
  
  def rowVector(row:Int) = {
    new MatrixD(copyOfRow(row), nCols)
  }
  
  def rowVectorQ = nRows == 1
  
  def columnVectorQ = nCols == 1
  
  def toRowVector = if(columnVectorQ) tN else this
  
  def toColumnVector = if(rowVectorQ) tN else this
  
  def columnSubset(indices : List[Int]) = {
    var i = 0
    var res : MatrixD = null
    while(i < indices.size) {
      val cVec = columnVector(indices(i))
      if(res == null) {
        res = cVec
      } else {
        res = res ++ cVec
      }
      i += 1
    }
    res
  }

  def rowSubset(indices : List[Int]) = {
    var i = 0
    var res : MatrixD = null
    while(i < indices.size) {
      val rVec = rowVector(indices(i))
      if(res == null) {
        res = rVec
      } else {
        res = res +/ rVec
      }
      i += 1
    }
    res
  }
  
  def toRowMaxIndices() = {
    val l = new Array[Int](nRows)
    var idx =0
    var jdx = 0
    var rowMax = 0d
    var maxIdx = 0
    var currEl = 0d
    while(idx < nRows) {
      jdx = 0
      rowMax = elements(idx * nCols)
      maxIdx = 0
      while(jdx < nCols) {
        currEl = elements(idx * nCols + jdx)
        if(currEl > rowMax) {
          rowMax = currEl
          maxIdx = jdx
        }
        jdx += 1
      }
      l(idx) = maxIdx + 1
      idx += 1
    }
    l
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

  def rowCopy(out: Array[Double], row: Int, startIdx: Int) = {
    copyRange(elements, out, rowIndices(row), startIdx)
  }

  override def toString(): String = {
    if(verbose || (nRows < 11 && nCols < 11)) {
	    val rowA = new Array[Double](nCols)
	    var row = nRows
	    var l = List[String]()
	    while (row > 0) {
	      rowCopy(rowA, row, 0)
	      l = l :+ rowA.mkString("[", ", ", "]")
	      row -= 1
	    }
	    l.reverse.mkString("", "\n", "\n")
    }else {
      "MatrixD["+nRows +","+nCols+"]"
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
    new MatrixD(b, nRows)
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

  def *(o: MatrixD): MatrixD = {
    require(nCols == o.nRows, "matrices " + dims() + " and " + o.dims() + " of incompatible shape for mulitplication")
    val c = new Array[Double](nRows * o.nCols)
    val oT = o.transposeN
    var row = 1
    var rowT = 1
    var idx = 0
    while (row <= nRows) {
      rowT = 1
      while (rowT <= oT.nRows) {
        c(idx) = MatrixD.dot(elements, rowIndices(row), oT.elements, oT.rowIndices(rowT))
        rowT += 1
        idx += 1
      }
      row += 1
    }
    new MatrixD(c, o.nCols)
  }

  def elOp(f: (Double) => Double) = elementOp(f)
  def elementOp(f: (Double) => Double): MatrixD = {
    var m = clone
    var l = elements.length - 1
    while (l >= 0) {
      m.elements(l) = f(elements(l))
      l -= 1
    }
    m
  }

  def elementScalarOp(s: Double, f: (Double, Double) => Double): MatrixD = {
    val m = clone
    var l = elements.length - 1
    while (l >= 0) {
      m.elements(l) = f(elements(l), s)
      l -= 1
    }
    m
  }

  def +(s: Double) = elementScalarOp(s, _ + _)
  def -(s: Double) = elementScalarOp(s, _ - _)
  def *(s: Double) = elementScalarOp(s, _ * _)
  def /(s: Double) = elementScalarOp(s, _ / _)

  def ^(exp: Double) = elementScalarOp(exp, (x, y) => scala.math.pow(x, y))
  def clean(σ: Double = .0001) = elementScalarOp(σ, (x, y) => if (x * x < y * y) 0. else x)
  
  def sumSquaredDiffs(other : MatrixD) : Double = {
    require(dims() == other.dims(), "this " + dims + " dimly incompat w other: " + other.dims)
    var sum = 0d
    val l = elements.length
    var i = 0
    var delta = 0d
    while(i < l) {
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

  def inverse(): MatrixD = {
    val d = determinant
    require(d != 0, "not linearly independent")
    val mT = cofactorM.transposeN()
    mT / d
    mT
  }

  implicit val matDims = (nRows, nCols)
  implicit def matrixToScalar(): Double = {
    require(nCols == 1 && nRows == 1)
    elements(0)
  }

  import MatrixD.scalarOp
  def test() = {
    2 + this
  }
}
