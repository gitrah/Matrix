package com.hartenbower.matrix
import scala.util.Random

object MatrixA {
  def elapsed(msg: String, l: Long) = {
    val now = System.currentTimeMillis
    println(msg + " took " + (now - l) + " millis")
    (now, now - l)
  }

  def dot(v1: Array[Double], range1: Tuple2[Int, Int], v2: Array[Double], range2: Tuple2[Int, Int]): Double = {
    require(range1._1 >= 0 && range1._2 < v1.length, "range1 outside v1")
    require(range2._1 >= 0 && range2._2 < v2.length, "range2 outside v2")
    var l = range1._2 - range1._1
    require(l == range2._2 - range2._1, "vectors of unequal length")
    var sum = 0.
    while (l >= 0) {
      sum += v1(range1._1 + l) * v2(range2._1 + l)
      l -= 1
    }
    sum
  }

  def diagonalM(dim: Int, d: Double = 1.0): MatrixA = {
    val l = dim * dim
    val c = new Array[Double](l)
    var i = 0
    while (i < l) {
      c(i) = if (i / dim == i % dim) d else 0
      i += 1
    }
    new MatrixA(c, dim)
  }

  def identityM(dim: Int) = diagonalM(dim)

  /* could also define one as transpose of other... */
  def rowMatrix(a: Array[Double]) = new MatrixA(a, a.length)

  def columnMatrix(a: Array[Double]) = new MatrixA(a, 1)

  def randn(nRows: Int, nCols: Int): MatrixA = {
    val rnd = new Random(System.currentTimeMillis)
    val l = nRows * nCols
    val c = new Array[Double](l)
    var i = 0
    while (i < l) {
      c(i) = rnd.nextDouble
      i += 1
    }
    new MatrixA(c, nCols)
  }
}

case class MatrixA(val elements: Array[Double], var nCols: Int) {
  require(elements.length % nCols == 0)
  var nRows: Int = if (elements.isEmpty) 0 else elements.length / nCols

  def validIndicesQ(row: Int, col: Int) {
    require(col > 0 && col <= nCols && row <= nRows && row > 0, "index (" + row + ", " + col + ") out of bounds [1," + nRows + "],[1," + nCols + "]")
  }

  def deref(row: Int, col: Int) = (row - 1) * nCols + col - 1
  def enref(idx: Int): (Int, Int) = {
    if (idx >= nCols) {
      (idx / nCols + 1, idx % nCols + 1)
    } else {
      (1, idx + 1)
    }
  }

  def apply(row: Int, col: Int): Double = {
    validIndicesQ(row, col)
    elements(deref(row, col))
  }

  private def matrixOp(o: MatrixA, f: (Double, Double) => Double): MatrixA = {
    var l = elements.length
    require(l == o.elements.length && nCols == o.nCols, "sizes don't match")
    l -= 1
    while (l >= 0) {
      elements(l) = f(elements(l), o.elements(l))
      l -= 1
    }
    this
  }

  private def matrixOpNew(o: MatrixA, f: (Double, Double) => Double): MatrixA = {
    var l = elements.length
    require(l == o.elements.length && nCols == o.nCols, "sizes don't match")
    val c = new Array[Double](l)
    l -= 1
    while (l >= 0) {
      c(l) = f(elements(l), o.elements(l))
      l -= 1
    }
    new MatrixA(c, nCols)
  }

  def +(other: MatrixA): MatrixA = matrixOpNew(other, _ + _)
  def -(other: MatrixA): MatrixA = matrixOpNew(other, _ - _)
  def hadamardProduct(other: MatrixA) = matrixOpNew(other, _ * _)
  def **(other: MatrixA) = hadamardProduct(other)

  def squareQ() = nCols == nRows

  /*
   * structural ops
   */
  def dims(): (Int, Int) = {
    Tuple2(nRows, nCols)
  }

  @inline
  def rowIndices(row: Int) = {
    require(row > 0 && row <= nRows, "row " + row + " must be 1 to " + nRows)
    val start = (row - 1) * nCols
    (start, start + nCols - 1)
  }

  def columnIndices(col: Int): Array[Int] = {
    require(col > 0 && col <= nCols, "column " + col + " must be 1 to " + nCols)
    val c = new Array[Int](nRows)
    var i = 0
    while (i < nRows) {
      c(i) = i * nRows + col - 1
      i += 1
    }
    c
  }

  def rowCopy(out: Array[Double], row: Int, startIdx: Int) = {
    Matrix.copyRange(elements, out, rowIndices(row), startIdx)
  }

  override def toString(): String = {
    val rowA = new Array[Double](nCols)
    var row = nRows
    var l = List[String]()
    while (row > 0) {
      rowCopy(rowA, row, 0)
      l = l :+ rowA.mkString("[", ", ", "]")
      row -= 1
    }
    l.reverse.mkString("", "\n", "\n")
  }

  def rightConcatenate(other: MatrixA): MatrixA = {
    require(other.nRows == nRows, "can only right-concatenate matrices of equal row count")
    val newCols = nCols + other.nCols
    val c = new Array[Double](nRows * newCols)
    var row = nRows
    while (row > 0) {
      rowCopy(c, row, (row - 1) * newCols)
      other.rowCopy(c, row, (row - 1) * newCols + nCols)
      row -= 1
    }
    new MatrixA(c, newCols)
  }

  def ++(other: MatrixA) = rightConcatenate(other)

  def bottomConcatenate(other: MatrixA): MatrixA = {
    require(other.nCols == nCols, "can only bottom-concatenate matrices of equal column count")
    val newRows = nRows + other.nRows
    val c = new Array[Double](nCols * newRows)
    elements.copyToArray(c)
    other.elements.copyToArray(c, elements.length)
    new MatrixA(c, nCols)
  }

  def +/(other: MatrixA) = bottomConcatenate(other)

  def prependColumnNew(col: Array[Double]): MatrixA = {
    require(col.length == nRows, "new column doesn't fit matrix")
    new MatrixA(col, 1) ++ this
  }

  def appendColumnNew(col: Array[Double]): MatrixA = {
    require(col.length == nRows, "new column doesn't fit matrix")
    this ++ new MatrixA(col, 1)
  }

  def prependRowNew(row: Array[Double]): MatrixA = {
    require(row.length == nCols, "new row doesn't fit matrix")
    new MatrixA(row, nCols) +/ this
  }

  def appendRowNew(row: Array[Double]): MatrixA = {
    require(row.length == nCols, "new row doesn't fit matrix")
    this +/ new MatrixA(row, nCols)
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

  def transposeNew(): MatrixA = {
    val l = elements.length
    val b = new Array[Double](elements.length)
    // first and last elems unchanged
    b(0) = elements(0)
    b(l - 1) = elements(l - 1)
    var i = 1
    while (i < l - 1) {
      b(i * nRows % (nRows * nCols - 1)) = elements(i)
      i += 1
    }
    new MatrixA(b, nRows)
  }

  def transpose(m: java.util.Map[Int, Double] = null) = if (nCols == nRows) transposeSquare else transposeNS(m)

  def transposeSquare() {
    val l = elements.length

    var i = 1
    var idx = i
    var temp = 0.
    var mod = 0
    var div = 1
    var imod = 0
    var idiv = 1
    var counter = nCols - 1
    var numToCache = 0
    while (counter > 0) {
      numToCache += counter
      counter -= 1
    }
    var cache = new Array[Double](numToCache)

    while (i < l - 1) {
      idx = i * nRows % (nRows * nCols - 1)

      div = idx / nCols
      mod = idx % nCols
      idiv = i / nCols
      imod = i % nCols
      if (div > 0 && mod >= 0 && mod < div) {
        //println("cacheing element " + idx + " = " + elements(idx))
        cache(div + mod - 1) = elements(idx)
      }
      elements(idx) = if (idiv > 0 && imod >= 0 && imod < idiv) {
        //println("reusing cached element " + (idiv + imod - 1))
        cache(idiv + imod - 1)
      } else {
        elements(i)
      }

      i += 1
    }
    if (nCols != nRows) {
      val temp = nCols
      nCols = nRows
      nRows = temp
    }
  }

  def transposeNS(m: java.util.Map[Int, Double] = null) = {
    val l = elements.length
    val square = nCols == nRows
    val size = if (nRows > nCols) nRows else nCols

    var i = 1
    var idx = i
    val map: java.util.Map[Int, Double] = if (m == null) new java.util.HashMap else { m.clear; m }

    while (i < l - 1) {
      idx = i * nRows % (nRows * nCols - 1)

      if (idx > i) {
        // store original content
        map.put(idx, elements(idx))
      }
      elements(idx) =
        if (map.containsKey(i)) {
          // retrieve cell's original content
          map.get(i)
        } else elements(i)

      i += 1
    }
    if (nCols != nRows) {
      val temp = nCols
      nCols = nRows
      nRows = temp
    }
  }

  def *(o: MatrixA): MatrixA = {
    require(nCols == o.nRows, "matrices of incompatible shape for mulitplication")
    val c = new Array[Double](nRows * o.nCols)
    val oT = o.transposeNew
    var row = 1
    var rowT = 1
    var idx = 0
    while (row <= nRows) {
      rowT = 1
      while (rowT <= oT.nRows) {
        c(idx) = MatrixA.dot(elements, rowIndices(row), oT.elements, oT.rowIndices(rowT))
        rowT += 1
        idx += 1
      }
      row += 1
    }
    new MatrixA(c, o.nCols)
  }

  def elementScalarOp(s: Double, f: (Double, Double) => Double) = {
    var l = elements.length - 1
    while (l >= 0) {
      elements(l) = f(elements(l), s)
      l -= 1
    }
  }

  def +(s: Double) = elementScalarOp(s, _ + _)
  def -(s: Double) = elementScalarOp(s, _ - _)
  def *(s: Double) = elementScalarOp(s, _ * _)
  def /(s: Double) = elementScalarOp(s, _ / _)

  def ^(exp: Double) = elementScalarOp(exp, (x, y) => Math.pow(x, y))
  def clean(σ: Double = .0001) = elementScalarOp(σ, (x, y) => if (x * x < y * y) 0. else x)

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

  def minorM(row: Int, col: Int): MatrixA = {
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
    new MatrixA(c, nCols - 1)
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
    new MatrixA(c, nCols)
  }

  def cofactorM2() = { // about 3% slower than nested whiles
    val l = elements.length
    val c = new Array[Double](l)
    var i = 0
    while (i < l) {
      c(i) = cofactor(i / nCols + 1, i % nCols + 1)
      i += 1
    }
    new MatrixA(c, nCols)
  }

  def inverse(): MatrixA = {
    val d = determinant
    require(d != 0, "not linearly independent")
    val mT = cofactorM.transposeNew()
    mT / d
    mT
  }

  implicit val dim = nCols
  implicit def scalarToMatrix(i: Int)(implicit dim: Int): MatrixA = {
    MatrixA.diagonalM(dim, i)
  }
  implicit def scalarToMatrix(s: Double)(implicit dim: Int): Matrix = {
    Matrix.diagonalM(dim, s)
  }

}

/*
val ma = new MatrixA(Array(1.,2,3,1,4,8,3,9,.5),3)
val m1 = new MatrixA( Array(1.,2.,3.,4.), 2)
val m2 = new MatrixA( Array(3.,4,5,6),2)
val m2b = new MatrixA(Array(1.,2,3,4,5,6), 3)
val m2c = new MatrixA(Array(1.,2,3,4,5,6), 2)
val m3 = new MatrixA(Array(1.,2.,3,4,5,6,7,8,9),3)
val m3b = new MatrixA(Array(1.,2.,3,4,5,6,7,8,9,10,11,12),3)
val m3c = new MatrixA(Array(1.,2.,3,4,5,6,7,8,9,10,11,12),4)
val m4 = new MatrixA(Array(1.,2.,3,4,5,6,7,8,9,10,11,12,13,14,15,16),4)
val m5 = new MatrixA(Array(1.,2.,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25),5)

def time( msg : String, count : Int, f : => Unit) {
  val l  = System.currentTimeMillis
  var idx = count
  while(idx > 0) {
	f
	idx -= 1
  } 
  println(msg + " took " + (System.currentTimeMillis - l))
}


def time(total: Long) = {
  val m3b = new MatrixA(Array(1., 2., 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 3)
  val m3c = new MatrixA(Array(1., 2., 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 4)
  var count = total
  var l = System.currentTimeMillis
  var res = 0.
  while (count > 0) {
    m3b.transpose()
    count -= 1
  }
  var r = elapsed("MatrixA.transpose(" + total + ")", l)
  l = r._1
  var delta1 = r._2
  count = total
  val map = new java.util.HashMap[Int, Double]()
  while (count > 0) {
    m3b.transpose(map)
    count -= 1
  }
  r = elapsed("Matrix.transpose(map)(" + total + ")", l)
  l = r._1
  var delta2 = r._2
  println("ratio is " + (1.0 * delta1 / delta2))
}
*/
