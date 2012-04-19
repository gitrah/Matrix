package com.hartenbower.matrix

import scala.util.Random

object MatrixF {
  def elapsed(msg: String, l: Long) = {
    val now = System.currentTimeMillis
    println(msg + " took " + (now - l) + " millis")
    (now, now - l)
  }

  def dot(v1: Array[Float], range1: Tuple2[Int, Int], v2: Array[Float], range2: Tuple2[Int, Int]): Float = {
    //require(range1._1 >= 0 && range1._2 < v1.length, "range1 outside v1")
    //require(range2._1 >= 0 && range2._2 < v2.length, "range2 outside v2")
    var l = range1._2 - range1._1
    //require(l == range2._2 - range2._1, "vectors of unequal length")
    var sum = 0.f
    while (l >= 0) {
      sum += v1(range1._1 + l) * v2(range2._1 + l)
      l -= 1
    }
    sum
  }

  def dotlv(v1: Array[Float], range1: Tuple2[Int, Int], v2: Array[Float], range2: Tuple2[Int, Int]): Float = {
    //require(range1._1 >= 0 && range1._2 < v1.length, "range1 outside v1")
    //require(range2._1 >= 0 && range2._2 < v2.length, "range2 outside v2")
    var l = range1._2 - range1._1
    val s1 = range1._1
    val s2 = range2._1
    //require(l == range2._2 - range2._1, "vectors of unequal length")
    var sum = 0.f
    while (l >= 0) {
      sum += v1(s1 + l) * v2(s2 + l)
      l -= 1
    }
    sum
  }

  def diagonalM(dim: Int, d: Float = 1.0f): MatrixF = {
    val l = dim * dim
    val c = new Array[Float](l)
    var i = 0
    while (i < l) {
      c(i) = if (i / dim == i % dim) d else 0
      i += 1
    }
    new MatrixF(c, dim)
  }

  def identityM(dim: Int) = diagonalM(dim)

  /* could also define one as transpose of other... */
  def rowMatrix(a: Array[Float]) = new MatrixF(a, a.length)

  def columnMatrix(a: Array[Float]) = new MatrixF(a, 1)

  def randn(nRows: Int, nCols: Int): MatrixF = {
    val rnd = new Random(System.currentTimeMillis)
    val l = nRows * nCols
    val c = new Array[Float](l)
    var i = 0
    while (i < l) {
      c(i) = rnd.nextFloat
      i += 1
    }
    new MatrixF(c, nCols)
  }
  
  def randSpace(nRows : Int, nCols: Int, nElements:Int):Array[Float] = {
    val rnd = new Random(System.currentTimeMillis)
    Array.fill(nRows*nCols*nElements,1)(rnd.nextFloat).flatten
  }

  def arrTest = {
    val a1 = Array(5, 4, 3, 2, 1)
    val a2 = a1
    a2(3) = 9
    println("a1 " + a1.mkString(","))

    println("a2 " + a2.mkString(","))
  }

  def mult4by4(mat1: Array[Float], mat2: Array[Float], resMat: Array[Float]) = {
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
		  mat1: Array[Float], offset1: Int,
		  mat2: Array[Float], offset2: Int,
		  resMat: Array[Float], offset3:Int) : Option[List[(Int, Int)]] = {
    var ctr = count-1
    while(ctr >= 0) {
    	val base1 = offset1
    	val base2 = ctr * 16 + offset2
    	val base3 = ctr * 16 + offset3
	    resMat(base3) = mat1(base1) * mat2(base2) + mat1(base1 +4) * mat2(base2 +1) + mat1(base1 +8) * mat2(base2 +2) + mat1(base1 +12) * mat2(base2 +3)
	    resMat(base3 +4) = mat1(base1 ) * mat2(base2 +4) + mat1(base1 +4) * mat2(base2 +5) + mat1(base1 +8) * mat2(base2 +6) + mat1(base1 +12) * mat2(base2 +7)
	    resMat(base3 +8) = mat1(base1) * mat2(base2 +8) + mat1(base1 +4) * mat2(base2 +9) + mat1(base1 +8) * mat2(base2 +10) + mat1(base1 +12) * mat2(base2 +11)
	    resMat(base3 +12) = mat1(base1) * mat2(base2 +12) + mat1(base1 +4) * mat2(base2 +13) + mat1(base1 +8) * mat2(base2 +14) + mat1(base1 +12) * mat2(base2 +15)
	    resMat(base3 +1) = mat1(base1 +1) * mat2(base2) + mat1(base1 +5) * mat2(base2 +1) + mat1(base1 +9) * mat2(base2 +2) + mat1(base1 +13) * mat2(base2 +3)
	    resMat(base3 +5) = mat1(base1 +1) * mat2(base2 +4) + mat1(base1 +5) * mat2(base2 +5) + mat1(base1 +9) * mat2(base2 +6) + mat1(base1 +13) * mat2(base2 +7)
	    resMat(base3 +9) = mat1(base1 +1) * mat2(base2 +8) + mat1(base1 +5) * mat2(base2 +9) + mat1(base1 +9) * mat2(base2 +10) + mat1(base1 +13) * mat2(base2 +11)
	    resMat(base3 +13) = mat1(base1 +1) * mat2(base2 +12) + mat1(base1 +5) * mat2(base2 +13) + mat1(base1 +9) * mat2(base2 +14) + mat1(base1 +13) * mat2(base2 +15)
	    resMat(base3 +2) = mat1(base1 +2) * mat2(base2) + mat1(base1 +6) * mat2(base2 +1) + mat1(base1 +10) * mat2(base2 +2) + mat1(base1 +14) * mat2(base2 +3)
	    resMat(base3 +6) = mat1(base1 +2) * mat2(base2 +4) + mat1(base1 +6) * mat2(base2 +5) + mat1(base1 +10) * mat2(base2 +6) + mat1(base1 +14) * mat2(base2 +7)
	    resMat(base3 +10) = mat1(base1 +2) * mat2(base2 +8) + mat1(base1 +6) * mat2(base2 +9) + mat1(base1 +10) * mat2(base2 +10) + mat1(base1 +14) * mat2(base2 +11)
	    resMat(base3 +14) = mat1(base1 +2) * mat2(base2 +1) + mat1(base1 +6) * mat2(base2 +13) + mat1(base1 +10) * mat2(base2 +14) + mat1(base1 +14) * mat2(base2 +15)
	    resMat(base3 +3) = mat1(base1 +3) * mat2(base2) + mat1(base1 +7) * mat2(base2 +1) + mat1(base1 +11) * mat2(base2 +2) + mat1(base1 +15) * mat2(base2 +3)
	    resMat(base3 +7) = mat1(base1 +3) * mat2(base2 +4) + mat1(base1 +7) * mat2(base2 +5) + mat1(base1 +11) * mat2(base2 +6) + mat1(base1 +15) * mat2(base2 +7)
	    resMat(base3 +11) = mat1(base1 +3) * mat2(base2 +8) + mat1(base1 +7) * mat2(base2 +9) + mat1(base1 +11) * mat2(base2 +10) + mat1(base1 +15) * mat2(base2 +11)
	    resMat(base3 +15) = mat1(base1 +3) * mat2(base2 +12) + mat1(base1 +7) * mat2(base2 +13) + mat1(base1 +11) * mat2(base2 +14) + mat1(base1 +15) * mat2(base2 +15)
	    ctr -= 1
    }
    None
  }
  
  def mult4by4Worker(m : Array[Float], targ: Array[Float], res: Array[Float], procs : Int = -1) {
    val scheduler = procs match {
      case -1 => new Scheduler[Float](targ, 16)
      case x => new Scheduler[Float](targ, 16,x)
    }
    scheduler.start
    scheduler ! Init( ( idx : Tuple2[Int,Int]) => mult4by4Range(idx._2-idx._1, m, 0, targ, idx._1, res, idx._1 ))
    // simulate an actor that waits for the search to complete
    while(!scheduler.finished) {
      Thread.sleep(50)
    }
  }
  
  def fill(src: MatrixF, dest : Array[Float]) = {
    val l = src.elements.length
    require(dest.length % l == 0)
    var offset = dest.length / l - 1
    while(offset >= 0) {
      src.elements.copyToArray(dest, offset* l)
      offset -= 1
    }
  }
}

/*
 * The array backed,  single-precision version
 */
case class MatrixF(val elements: Array[Float], var nCols: Int) {
  require(elements.length % nCols == 0)
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

  def apply(row: Int, col: Int): Float = {
    validIndicesQ(row, col)
    elements(deref(row, col))
  }

  private def matrixOp(o: MatrixF, f: (Float, Float) => Float): MatrixF = {
    var l = elements.length
    require(l == o.elements.length && nCols == o.nCols, "sizes don't match")
    l -= 1
    while (l >= 0) {
      elements(l) = f(elements(l), o.elements(l))
      l -= 1
    }
    this
  }

  private def matrixOpNew(o: MatrixF, f: (Float, Float) => Float): MatrixF = {
    var l = elements.length
    require(l == o.elements.length && nCols == o.nCols, "sizes don't match")
    val c = new Array[Float](l)
    l -= 1
    while (l >= 0) {
      c(l) = f(elements(l), o.elements(l))
      l -= 1
    }
    new MatrixF(c, nCols)
  }

  def +(other: MatrixF): MatrixF = matrixOpNew(other, _ + _)
  def -(other: MatrixF): MatrixF = matrixOpNew(other, _ - _)
  def hadamardProduct(other: MatrixF) = matrixOpNew(other, _ * _)
  def **(other: MatrixF) = hadamardProduct(other)

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
    val c = new Array[Float](nCols)
    var l = nCols - 1
    while (l >= 0) {
      c(l) = elements((row - 1) * nCols + l)
      l -= 1
    }
    c
  }

  def copyOfCol(col: Int) = {
    validColQ(col)
    val c = new Array[Float](nRows)
    var l = nRows - 1
    while (l >= 0) {
      c(l) = elements(l * nCols + col - 1)
      l -= 1
    }
    c
  }

  def copyRange(src: Array[Float], targ: Array[Float], range: Tuple2[Int, Int], start: Int) {
    require(range._1 > -1 && range._2 < src.length, "range " + range + " bad for source")
    require(start + range._2 - range._1 < targ.length, "range (" + (range._2 - range._1) + ") + start (" + start + ") bad for target length " + targ.length)
    var l = range._2 - range._1
    while (l >= 0) {
      targ(start + l) = src(range._1 + l)
      l -= 1
    }
  }

  def rowCopy(out: Array[Float], row: Int, startIdx: Int) = {
    copyRange(elements, out, rowIndices(row), startIdx)
  }

  override def toString(): String = {
    val rowA = new Array[Float](nCols)
    var row = nRows
    var l = List[String]()
    while (row > 0) {
      rowCopy(rowA, row, 0)
      l = l :+ rowA.mkString("[", ", ", "]")
      row -= 1
    }
    l.reverse.mkString("", "\n", "\n")
  }

  def rightConcatenate(other: MatrixF): MatrixF = {
    require(other.nRows == nRows, "can only right-concatenate matrices of equal row count")
    val newCols = nCols + other.nCols
    val c = new Array[Float](nRows * newCols)
    var row = nRows
    while (row > 0) {
      rowCopy(c, row, (row - 1) * newCols)
      other.rowCopy(c, row, (row - 1) * newCols + nCols)
      row -= 1
    }
    new MatrixF(c, newCols)
  }

  def ++(other: MatrixF) = rightConcatenate(other)

  def bottomConcatenate(other: MatrixF): MatrixF = {
    require(other.nCols == nCols, "can only bottom-concatenate matrices of equal column count")
    val newRows = nRows + other.nRows
    val c = new Array[Float](nCols * newRows)
    elements.copyToArray(c)
    other.elements.copyToArray(c, elements.length)
    new MatrixF(c, nCols)
  }

  def +/(other: MatrixF) = bottomConcatenate(other)

  def prependColumnNew(col: Array[Float]): MatrixF = {
    require(col.length == nRows, "new column doesn't fit matrix")
    new MatrixF(col, 1) ++ this
  }

  def appendColumnNew(col: Array[Float]): MatrixF = {
    require(col.length == nRows, "new column doesn't fit matrix")
    this ++ new MatrixF(col, 1)
  }

  def prependRowNew(row: Array[Float]): MatrixF = {
    require(row.length == nCols, "new row doesn't fit matrix")
    new MatrixF(row, nCols) +/ this
  }

  def appendRowNew(row: Array[Float]): MatrixF = {
    require(row.length == nCols, "new row doesn't fit matrix")
    this +/ new MatrixF(row, nCols)
  }

  def flipud = {
    val c = new Array[Float](nCols)
    var row = 1
    while (row <= nRows / 2) {
      // swap row, nRows-row
      rowCopy(c, row, 0)
      rowCopy(elements, nRows - row + 1, (row - 1) * nCols)
      c.copyToArray(elements, (nRows - row) * nCols)
      row += 1
    }
  }

  def transposeNew(): MatrixF = {
    val l = elements.length - 1
    val b = new Array[Float](l + 1)
    // first and last elems unchanged
    b(0) = elements(0)
    b(l) = elements(l)
    var i = 1
    while (i < l) {
      b(i * nRows % l) = elements(i)
      i += 1
    }
    new MatrixF(b, nRows)
  }

  def transpose(m: java.util.Map[Int, Float] = null) = if (nCols == nRows) transposeSquare else transposeNS(m)

  // about 37% faster than txNew
  def transposeSquare() {
    val l = elements.length - 1
    var idx = 1 // can skip first and last elements
    var temp = 0.f
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
  }

  def transposeNS(m: java.util.Map[Int, Float] = null) = {
    val l = elements.length - 1
    var i = 1
    var idx = i
    val map: java.util.Map[Int, Float] = if (m == null) new java.util.HashMap else { m.clear; m }
    var cachedElem: Any = null

    while (i < l) {
      idx = i * nRows % l
      if (idx > i) {
        // store original content
        map.put(idx, elements(idx))
      }
      cachedElem = map.get(i)
      elements(idx) = if (cachedElem != null) cachedElem.asInstanceOf[Float] else elements(i)
      i += 1
    }
    if (nCols != nRows) {
      val temp = nCols
      nCols = nRows
      nRows = temp
    }
  }

  def *(o: MatrixF): MatrixF = {
    require(nCols == o.nRows, "matrices of incompatible shape for mulitplication")
    val c = new Array[Float](nRows * o.nCols)
    val oT = o.transposeNew
    var row = 1
    var rowT = 1
    var idx = 0
    while (row <= nRows) {
      rowT = 1
      while (rowT <= oT.nRows) {
        c(idx) = MatrixF.dot(elements, rowIndices(row), oT.elements, oT.rowIndices(rowT))
        rowT += 1
        idx += 1
      }
      row += 1
    }
    new MatrixF(c, o.nCols)
  }

  def elementScalarOp(s: Float, f: (Float, Float) => Float) = {
    var l = elements.length - 1
    while (l >= 0) {
      elements(l) = f(elements(l), s)
      l -= 1
    }
  }

  def +(s: Float) = elementScalarOp(s, _ + _)
  def -(s: Float) = elementScalarOp(s, _ - _)
  def *(s: Float) = elementScalarOp(s, _ * _)
  def /(s: Float) = elementScalarOp(s, _ / _)

  def ^(exp: Float) = elementScalarOp(exp, (x, y) => scala.math.pow(x, y).toFloat)

  def clean(σ: Float = .0001f) = elementScalarOp(σ, (x, y) => if (x * x < y * y) 0.f else x)

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

  def minorM(row: Int, col: Int): MatrixF = {
    validIndicesQ(row, col)
    val c = new Array[Float]((nCols - 1) * (nRows - 1))
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
    new MatrixF(c, nCols - 1)
  }

  def minor(row: Int, col: Int): Float = minorM(row, col).determinant

  def determinant: Float = {
    require(nCols == nRows, "not square")
    nCols match {
      case 1 =>
        elements(0)
      case 2 =>
        elements(0) * elements(3) - elements(1) * elements(2)
      case _ =>
        // cofactor expansion along the first column
        var row = 1
        var sum = 0f
        while (row <= nRows) {
          sum += elements((row - 1) * nCols) * cofactor(row, 1)
          row += 1
        }
        sum
    }
  }

  def cofactor(row: Int, col: Int) = minor(row, col) * sgn(row, col)

  def cofactorM() = {
    val c = new Array[Float](elements.length)
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
    new MatrixF(c, nCols)
  }

  def cofactorM2() = { // about 3% slower than nested whiles
    val l = elements.length
    val c = new Array[Float](l)
    var i = 0
    while (i < l) {
      c(i) = cofactor(i / nCols + 1, i % nCols + 1)
      i += 1
    }
    new MatrixF(c, nCols)
  }

  def inverse(): MatrixF = {
    val d = determinant
    require(d != 0, "not linearly independent")
    val mT = cofactorM.transposeNew()
    mT / d
    mT
  }

  implicit val dim = nCols
  implicit def scalarToMatrix(i: Int)(implicit dim: Int): MatrixF = {
    MatrixF.diagonalM(dim, i)
  }
  implicit def scalarToMatrix(s: Float)(implicit dim: Int): MatrixF = {
    MatrixF.diagonalM(dim, s)
  }

}

