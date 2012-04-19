package com.hartenbower.matrix
import scala.util.Random

object MatrixP {
  def elapsed(msg: String, l: Long) = {
    val now = System.currentTimeMillis
    println(msg + " took " + (now - l) + " millis")
    (now, now - l)
  }

  def dot[N](v1: Array[N], range1: Tuple2[Int, Int],
      v2: Array[N], range2: Tuple2[Int, Int])(implicit numeric: Numeric[N]): N = {
    
    //require(range1._1 >= 0 && range1._2 < v1.length, "range1 outside v1")
    //require(range2._1 >= 0 && range2._2 < v2.length, "range2 outside v2")
    var l = range1._2 - range1._1
    val s1 = range1._1
    val s2 = range2._1
    //require(l == range2._2 - range2._1, "vectors of unequal length")
    var sum : N = numeric.zero
    while (l >= 0) {
       sum = numeric.plus(sum, numeric.times( v1(s1 + l), v2(s2 + l) ))
      l -= 1
    }
    sum
  }
  
  //def array[N](dim : Int)(implicit manifest : Manifest[N])  = new Array[N](dim)

  def diagonalM[N](dim: Int, d: N)(implicit numeric: Numeric[N], manifest : Manifest[N]): MatrixP[N] = {
    val l = dim * dim
    val c = manifest.newArray(l)
    var i = 0
    while (i < l) {
      c(i) = if (i / dim == i % dim) d else numeric.zero
      i += 1
    }
    new MatrixP(c, dim)(numeric,manifest)
  }

  def identityM[N](dim: Int)(implicit numeric: Numeric[N], manifest : Manifest[N]) = diagonalM[N](dim,numeric.one)(numeric,manifest)

  /* could also define one as transpose of other... */
  def rowMatrix[N](a: Array[N])(implicit numeric: Numeric[N], manifest : Manifest[N]) = new MatrixP[N](a, a.length)

  def columnMatrix[N](a: Array[N])(implicit numeric: Numeric[N], manifest : Manifest[N]) = new MatrixP[N](a, 1)

  def randn[N](nRows: Int, nCols: Int)(implicit numeric: Numeric[N], manifest : Manifest[N]): MatrixP[N] = {
    val rnd = new Random(System.currentTimeMillis)
    val l = nRows * nCols
    val c = manifest.newArray(l)
    var idx = 0
    while (idx < l) {
      c(idx) =  numeric match {
        case i : Integral[N]=> rnd.nextInt.asInstanceOf[N]
        case fr : Fractional[N]=>
          if(java.lang.Double.TYPE.isAssignableFrom(manifest.erasure)) 
            rnd.nextDouble.asInstanceOf[N]
          else
            rnd.nextFloat.asInstanceOf[N]
      }
      idx += 1
    }
    new MatrixP(c, nCols)(numeric, manifest)
  }
}

/**
 * This is the parameterized implementation.  
 * @return
 */

case class MatrixP[N](val elements: Array[N], var nCols: Int)(implicit num: Numeric[N], manifest : Manifest[N]) {
  require(elements.length % nCols == 0)
  var nRows: Int = if (elements.isEmpty) 0 else elements.length / nCols

  def validIndicesQ(row: Int, col: Int) {
    require(col > 0 && col <= nCols && row <= nRows && row > 0, "index (" + row + ", " + col + ") out of bounds [1," + nRows + "],[1," + nCols + "]")
  }
  def validColQ(col : Int) = require(col > 0 && col <= nCols, "column " + col + " must be 1 to " + nCols)
  def validRowQ(row : Int) = require(row > 0 && row <= nRows,"row " + row + " must be 1 to " + nRows)

  def deref(row: Int, col: Int) = (row - 1) * nCols + col - 1
  def enref(idx: Int): (Int, Int) = {
    if (idx >= nCols) {
      (idx / nCols + 1, idx % nCols + 1)
    } else {
      (1, idx + 1)
    }
  }

  def apply(row: Int, col: Int): N = {
    validIndicesQ(row, col)
    elements(deref(row, col))
  }

  private def matrixOp(o: MatrixP[N], f: (N, N) => N): MatrixP[N] = {
    var l = elements.length
    require(l == o.elements.length && nCols == o.nCols, "sizes don't match")
    l -= 1
    while (l >= 0) {
      elements(l) = f(elements(l), o.elements(l))
      l -= 1
    }
    this
  }

  private def matrixOpNew(o: MatrixP[N], f: (N, N) => N)(implicit num: Numeric[N], manifest : Manifest[N]): MatrixP[N] = {
    var l = elements.length
    require(l == o.elements.length && nCols == o.nCols, "sizes don't match")
    val c = manifest.newArray(l)
    
    l -= 1
    while (l >= 0) {
      c(l) = f(elements(l), o.elements(l))
      l -= 1
    }
    new MatrixP[N](c.asInstanceOf[Array[N]], nCols)
  }

  def +(other: MatrixP[N])(implicit num: Numeric[N], manifest : Manifest[N]): MatrixP[N] = matrixOpNew(other, num.plus(_, _))(num, manifest)
  def -(other: MatrixP[N])(implicit num: Numeric[N], manifest : Manifest[N]): MatrixP[N] = matrixOpNew(other, num.minus(_ , _))(num, manifest)
  def hadamardProduct(other: MatrixP[N])(implicit num: Numeric[N], manifest : Manifest[N]) = matrixOpNew(other, num.times(_ , _))(num, manifest)
  def **(other: MatrixP[N])(implicit num: Numeric[N], manifest : Manifest[N]) = hadamardProduct(other)(num,manifest)

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
  
  def copyOfRow(row : Int) = {
    validRowQ(row)
    val c = new Array[N](nCols)
    var l = nCols -1
    while(l >= 0) {
      c(l) = elements( (row -1) * nCols + l)
      l-=1
    }
    c
  }
  
  def copyOfCol(col : Int) = {
    validColQ(col)
    val c = manifest.newArray(nRows)
    var l = nRows -1
    while(l >= 0) {
      c(l) = elements( l * nCols + col-1)
      l-=1
    }
    c
  }
  
  def copyRange(src: Array[N], targ: Array[N], range: Tuple2[Int, Int], start: Int) {
    require(range._1 > -1 && range._2 < src.length, "range " + range + " bad for source")
    require(start + range._2 - range._1 < targ.length, "range (" + (range._2 - range._1) + ") + start (" + start + ") bad for target length " + targ.length)
    var l = range._2 - range._1
    while (l >= 0) {
      targ(start + l) = src(range._1 + l)
      l -= 1
    }
  }


  def rowCopy(out: Array[N], row: Int, startIdx: Int) = {
    copyRange(elements, out, rowIndices(row), startIdx)
  }

  override def toString(): String = {
    val rowA = manifest.newArray(nCols)
    var row = nRows
    var l = List[String]()
    while (row > 0) {
      rowCopy(rowA, row, 0)
      l = l :+ rowA.mkString("[", ", ", "]")
      row -= 1
    }
    l.reverse.mkString("", "\n", "\n")
  }

  def rightConcatenate(other: MatrixP[N]): MatrixP[N] = {
    require(other.nRows == nRows, "can only right-concatenate matrices of equal row count")
    val newCols = nCols + other.nCols
    val c = manifest.newArray(nRows * newCols)
    var row = nRows
    while (row > 0) {
      rowCopy(c, row, (row - 1) * newCols)
      other.rowCopy(c, row, (row - 1) * newCols + nCols)
      row -= 1
    }
    new MatrixP(c, newCols)
  }

  def ++(other: MatrixP[N]) = rightConcatenate(other)

  def bottomConcatenate(other: MatrixP[N]): MatrixP[N] = {
    require(other.nCols == nCols, "can only bottom-concatenate matrices of equal column count")
    val newRows = nRows + other.nRows
    val c = manifest.newArray(nCols * newRows)
    elements.copyToArray(c)
    other.elements.copyToArray(c, elements.length)
    new MatrixP(c, nCols)
  }

  def +/(other: MatrixP[N]) = bottomConcatenate(other)

  def prependColumnNew(col: Array[N]): MatrixP[N] = {
    require(col.length == nRows, "new column doesn't fit matrix")
    new MatrixP(col, 1) ++ this
  }

  def appendColumnNew(col: Array[N]): MatrixP[N] = {
    require(col.length == nRows, "new column doesn't fit matrix")
    this ++ new MatrixP(col, 1)
  }

  def prependRowNew(row: Array[N]): MatrixP[N] = {
    require(row.length == nCols, "new row doesn't fit matrix")
    new MatrixP(row, nCols) +/ this
  }

  def appendRowNew(row: Array[N]): MatrixP[N] = {
    require(row.length == nCols, "new row doesn't fit matrix")
    this +/ new MatrixP(row, nCols)
  }

  def flipud = {
    val c = manifest.newArray(nCols)
    var row = 1
    while (row <= nRows / 2) {
      // swap row, nRows-row
      rowCopy(c, row, 0)
      rowCopy(elements, nRows - row + 1, (row - 1) * nCols)
      c.copyToArray(elements, (nRows - row) * nCols)
      row += 1
    }
  }
  
  def transposeNew(): MatrixP[N] = {
    val l = elements.length - 1
    val b = manifest.newArray(l+1)
    // first and last elems unchanged
    b(0) = elements(0)
    b(l) = elements(l)
    var i = 1
    while (i < l) {
      b(i * nRows % l) = elements(i)
      i += 1
    }
    new MatrixP(b, nRows)
  }

  def transpose(m: java.util.Map[Int, N] = null) = if (nCols == nRows) transposeSquare else transposeNS(m)

  // about 37% faster than txNew
  def transposeSquare() {
    val l = elements.length - 1
    var idx = 1 // can skip first and last elements
    var temp = num.zero
    var imod = 0
    var idiv = 1
    var oppIdx = 0
    while (idx < l ) {
      idiv = idx / nCols
      imod = idx % nCols
      if( imod > idiv) { // stay on top triangle and skip diagonals
    	  oppIdx = imod * nCols + idiv
    	  temp = elements(idx)
    	  elements(idx) = elements(oppIdx)
    	  elements(oppIdx) = temp
      } 
      idx += 1
    }
  }

  def transposeNS(m: java.util.Map[Int, N] = null) = {
    val l = elements.length - 1
    var i = 1
    var idx = i
    val map: java.util.Map[Int, N] = if (m == null) new java.util.HashMap else { m.clear; m }
    var cachedElem : Any = null

    while (i < l ) {
      idx = i * nRows % l
      if (idx > i) {
        // store original content
        map.put(idx, elements(idx))
      }
      cachedElem = map.get(i) 
      elements(idx) = if (cachedElem != null) cachedElem.asInstanceOf[N] else elements(i)
      i += 1
    }
    if (nCols != nRows) {
      val temp = nCols
      nCols = nRows
      nRows = temp
    }
  }

  def *(o: MatrixP[N]): MatrixP[N] = {
    require(nCols == o.nRows, "matrices of incompatible shape for mulitplication")
    val c = manifest.newArray(nRows * o.nCols)
    val oT = o.transposeNew
    var row = 1
    var rowT = 1
    var idx = 0
    while (row <= nRows) {
      rowT = 1
      while (rowT <= oT.nRows) {
        c(idx) = MatrixP.dot(elements, rowIndices(row), oT.elements, oT.rowIndices(rowT))
        rowT += 1
        idx += 1
      }
      row += 1
    }
    new MatrixP(c, o.nCols)
  }

  def elementScalarOp(s: N, f: (N, N) => N) = {
    var l = elements.length - 1
    while (l >= 0) {
      elements(l) = f(elements(l), s)
      l -= 1
    }
  }

  def +(s: N) = elementScalarOp(s, num.plus(_, _))
  def -(s: N) = elementScalarOp(s, num.minus(_, _))
  def *(s: N) = elementScalarOp(s, num.times(_, _))
  def /(s: N) =  num match{
     case i: Integral[_] => elementScalarOp(s,i.quot(_, _))
     case fr: Fractional[_] => elementScalarOp(s,fr.div(_, _))}

  def ^(exp: N) = elementScalarOp(exp, (x, y) => scala.math.pow(num.toDouble(x), num.toDouble(y)).asInstanceOf[N])
  def clean(σ: N) = elementScalarOp(σ, (x, y) => if (num.lt(num.times(x,x),  num.times(y, y))) num.zero else x)

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

  def minorM(row: Int, col: Int): MatrixP[N] = {
    validIndicesQ(row, col)
    val c = manifest.newArray((nCols - 1) * (nRows - 1))
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
    new MatrixP(c, nCols - 1)
  }

  def minor(row: Int, col: Int): N = minorM(row, col).determinant

  def determinant: N = {
    require(nCols == nRows, "not square")
    nCols match {
      case 1 =>
        elements(0)
      case 2 =>
        num.minus(num.times(elements(0) , elements(3)), num.times( elements(1) ,elements(2)))
      case _ =>
        // cofactor expansion along the first column
        var row = 1
        var sum = num.zero
        while (row <= nRows) {
          sum = num.plus(sum, num.times(elements((row - 1) * nCols) , cofactor(row, 1)))
          row += 1
        }
        sum
    }
  }

  def cofactor(row: Int, col: Int) = num.times(minor(row, col),num.fromInt(sgn(row, col)))

  def cofactorM() = {
    val c = manifest.newArray(elements.length)
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
    new MatrixP[N](c, nCols)
  }

  def cofactorM2() = { // about 3% slower than nested whiles
    val l = elements.length
    val c = manifest.newArray(l)
    var i = 0
    while (i < l) {
      c(i) = cofactor(i / nCols + 1, i % nCols + 1)
      i += 1
    }
    new MatrixP(c, nCols)
  }

  def inverse(): MatrixP[N] = {
    val d = determinant
    require(d != 0, "not linearly independent")
    val mT = cofactorM.transposeNew()
    mT / d
    mT
  }

  implicit val dim = nCols
  implicit def scalarToMatrix(i: Int)(implicit dim: Int): MatrixP[N]= {
    MatrixP.diagonalM(dim, num.fromInt(i))
  }
  implicit def scalarToMatrix(s: N)(implicit dim: Int): MatrixP[N] = {
    MatrixP.diagonalM(dim, s)
  }

}
