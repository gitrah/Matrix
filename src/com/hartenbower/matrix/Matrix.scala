package com.hartenbower.matrix
import Util._
import scala.util.Random

object Matrix {
  def apply(nCols: Int, els: Double*): Matrix = {
    def splitRowsWorker(inList: List[Double], working: List[List[Double]]): List[List[Double]] = {
      if (inList.isEmpty)
        working
      else {
        val (a, b) = inList.splitAt(nCols)
        splitRowsWorker(b, working :+ a)
      }
    }
    def splitRows(inList: List[Double]) =
      splitRowsWorker(inList, List[List[Double]]())
    val rows: List[List[Double]] =
      splitRows(els.toList)
    new Matrix(rows)
  }

  def diagonalM(dim: Int, d: Double = 1.0): Matrix =
    new Matrix(
      (1 to dim).map(i =>
        (1 to dim).map(j =>
          if (i == j) d else 0.0)))

  def identityM(dim: Int) = diagonalM(dim)

  /* could also define one as transpose of other... */
  def rowMatrix(l: List[Double]): Matrix = new Matrix(List(l))

  def columnMatrix(l: List[Double]): Matrix = new Matrix(l map (List(_)))

  def randn(nRows: Int, nCols: Int): Matrix = {
    val rnd = new Random(System.currentTimeMillis)
    new Matrix(
      (1 to nRows).map(_ =>
        (1 to nCols).map(_ =>
          rnd.nextDouble())))
  }

  def randnw(nRows: Int, nCols: Int): Matrix = {
    val rnd = new Random(System.currentTimeMillis)
    new Matrix(
      (1 to nRows).map(_ =>
        (1 to nCols).map(_ =>
          rnd.nextDouble())))
  }

  def dotVectors(a: List[Double],
    b: List[Double]): Double = {
    (0.0 /: (a, b).zipped.map(_ * _))(_ + _)
  }

  def dotVectorsW(a: List[Double],
    b: List[Double]): Double = {
    var l = a.length
    require(l == b.length)
    l -= 1
    var sum = 0.0
    while (l >= 0) {
      sum += a(l) * b(l)
      l -= 1
    }
    sum
  }

  def dotVectorsA(a: Array[Double],
    b: Array[Double]): Double = {
    var l = a.length
    require(l == b.length)
    l -= 1
    var sum = 0.0
    while (l >= 0) {
      sum += a(l) * b(l)
      l -= 1
    }
    sum
  }

  // warning: invoking this method will make you sad
  def time(total: Long) = {
    val v1 = List(0.1, 0.5, 2, 5, 6)
    val a1 = v1.toArray
    val v2 = List(0.5, 8, 81, 5, 0.7)
    val a2 = v2.toArray
    var count = total
    var l = System.currentTimeMillis
    var res = 0.0
    while (count > 0) {
      res = Matrix.dotVectors(v1, v2)
      count -= 1
    }
    var r = Timing.elapsed("Matrix.dotVectors(" + total + ")", l)
    l = r._1
    var delta1 = r._2
    count = total
    while (count > 0) {
      res = Matrix.dotVectorsW(v1, v2)
      count -= 1
    }
    r = Timing.elapsed("Matrix.dotVectorsW(" + total + ")", l)
    l = r._1
    var delta2 = r._2
    println("ratio is " + (1.0 * delta1 / delta2))
    count = total
    while (count > 0) {
      res = Matrix.dotVectorsA(a1, a2)
      count -= 1
    }
    r = Timing.elapsed("Matrix.dotVectorsA(" + total + ")", l)
    l = r._1
    var delta3 = r._2
    println("ratio is " + (1.0 * delta1 / delta3))
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

  implicit def seqSeqToListList(ss: Seq[Seq[Double]]): List[List[Double]] = {
    (ss map (_.toList)).toList
  }

  implicit def tupleToList(t: Tuple2[Double, Double]) = List(t._1, t._2)

}

/**
 * This is the most idiomatic implementation.  As so often happens,
 * it is as slow as it is pretty; some operations are several hundred times slower
 * than the corresponding Array-backed, non-for-comprehending implementation
 *
 */
case class Matrix(val elements: List[List[Double]]) {

  def nRows: Int = elements.length
  def nCols: Int = if (elements.isEmpty) 0 else elements.head.length

  /**
   * all rows of the matrix must have the same
   * number of columns
   */
  require(elements.forall(_.length == nCols), "data not of matrix form (column count varies across rows)")

  //def this(ss : Seq[Seq[Double]]) = this((ss map ( _.toList)).toList) 

  //  def this(tups: Product*) = {
  //    this((for (t <- tups) yield (t.productIterator.toList).asInstanceOf[List[Double]]).toList)
  //  }

  def validIndicesQ(row: Int, col: Int) {
    require(col > 0 && col <= nCols && row <= nRows && row > 0, "index (" + row + ", " + col + ") out of bounds [1," + nRows + "],[1," + nCols + "]")
  }

  def apply(row: Int, col: Int): Double = {
    validIndicesQ(row, col)
    elements(row - 1)(col - 1)
  }

  private def addRows(a: List[Double], b: List[Double]): List[Double] =
    (a, b).zipped.map(_ + _).toList

  private def subRows(a: List[Double], b: List[Double]): List[Double] =
    (a, b).zipped.map(_ - _).toList

  def +(other: Matrix): Matrix = {
    require(other.nRows == nRows && other.nCols == nCols, "matrices of different dimensions")
    new Matrix(
      (elements, other.elements).zipped.map(addRows(_, _)).toList)
  }

  /*
   * structural ops
   */
  def dims(): (Int, Int) = {
    Tuple2(nRows, nCols)
  }

  def ++(other: Matrix): Matrix = {
    require(other.nRows == nRows, "can only right-concatenate matrices of equal row count")
    new Matrix((elements, other.elements).zipped.map(_ ++ _))
  }
  def rightConcatenate(other: Matrix): Matrix = ++(other)

  def +/(other: Matrix): Matrix = {
    require(other.nCols == nCols, "can only bottom-concatenate matrices of equal column count")
    new Matrix(elements ++ other.elements)
  }
  def bottomConcatenate(other: Matrix): Matrix = +/(other)

  def -(other: Matrix): Matrix = {
    require(other.nRows == nRows && other.nCols == nCols, "matrices of different dimensions")
    new Matrix((elements, other.elements).zipped.map(subRows(_, _)))
  }

  def transpose(): Matrix =
    new Matrix(elements.transpose)

  def prependColumn(col: List[Double]): Matrix = {
    require(col.length == nRows, "new column doesn't fit matrix")
    new Matrix((col, elements).zipped.map(_ :: _))
  }

  def appendColumn(col: List[Double]): Matrix = {
    require(col.length == nRows, "new column doesn't fit matrix")
    new Matrix((elements, col).zipped.map(_ :+ _))
  }

  def appendRow(row: List[Double]): Matrix = {
    require(row.length == nCols, "new row doesn't fit matrix")
    new Matrix(elements :+ row)
  }

  def prependRow(row: List[Double]): Matrix = {
    require(row.length == nCols, "new row doesn't fit matrix")
    new Matrix(row :: elements)
  }
  // ala Octave
  def flipud = new Matrix(elements.reverse)

  def *(other: Matrix): Matrix = {
    require(nCols == other.nRows, "matrices incompatible for multiplication (column count of this != row count of other)")
    val t = other.transpose()
    new Matrix(
      (for (row <- elements) yield {
        for (otherCol <- t.elements)
          yield Matrix.dotVectors(row, otherCol)
      }))
  }

  def elementScalarOp(s: Double, f: (Double, Double) => Double): Matrix = new Matrix(elements map (_ map (f(_, s))))

  def +(s: Double) = elementScalarOp(s, _ + _)
  def -(s: Double) = elementScalarOp(s, _ - _)
  def *(s: Double) = elementScalarOp(s, _ * _)
  def /(s: Double) = elementScalarOp(s, _ / _)

  def ^(exp: Double) = elementScalarOp(exp, (x, y) => scala.math.pow(x, y))
  def clean(σ: Double = 0.0001) = elementScalarOp(σ, (x, y) => if (x * x < y * y) 0.0 else x)

  // elementwise operations
  //  def elementElementOp( other : Matrix, f : (Tuple2[Double,Double])=>Double ) : Matrix = {
  //    new Matrix((elements,other.elements).zipped.map( (row, rowo) => row.zip(rowo).map( f )))
  //  }
  //   
  def elementElementOp(other: Matrix, f: (Double, Double) => Double): Matrix = {
    new Matrix((elements, other.elements).zipped.map((row, rowo) => (row, rowo).zipped.map(f)))
  }

  def hadamardProduct(other: Matrix) = elementElementOp(other, _ * _)

  def **(other: Matrix) = hadamardProduct(other)

  /**
   * other versions of +, *, /
   * left in for eventual profiling
   */
  def plus(s: Double) = new Matrix(elements map (_ map (_ + s)))
  def mult(s: Double) = new Matrix(for (row <- elements) yield (row.map(_ * s)))
  def div(s: Double) = new Matrix(for (row <- elements) yield (row.map(_ / s)))

  override def toString(): String = {
    val rowStrings =
      for (row <- elements)
        yield row.mkString("[", ", ", "]")
    rowStrings.mkString("", "\n", "\n")
  }

  def sgn(row: Int, col: Int): Int = {
    validIndicesQ(row, col)
    var l = -1
    for (k <- 0 to row + col)
      l *= -1
    l
  }

  import Matrix.seqSeqToListList

  // yields Matrix with row,col omitted
  def minorM(row: Int, col: Int): Matrix = {
    validIndicesQ(row, col)
    new Matrix(
      for (y <- 0 to nRows - 1 if row - 1 != y) yield {
        for (x <- 0 to nCols - 1 if col - 1 != x && row - 1 != y)
          yield elements(y)(x)
      })
  }

  def minor(row: Int, col: Int): Double = minorM(row, col).determinant

  def determinant: Double = {
    require(nCols == nRows, "not square")
    nCols match {
      case 1 =>
        elements(0)(0)
      case 2 =>
        elements(0)(0) * elements(1)(1) - elements(0)(1) * elements(1)(0)
      case _ =>
        // cofactor expansion along the first column
        (0.0 /: (for (i <- 1 to nRows) yield elements(i - 1)(0) * cofactor(i, 1)))(_ + _)
    }
  }

  def cofactor(row: Int, col: Int) = minor(row, col) * sgn(row, col)

  def cofactorM1() =
    new Matrix(
      for (row <- 1 to nRows) yield {
        for (col <- 1 to nCols) yield cofactor(row, col)
      })

  def cofactorM() = new Matrix(
    (1 to nRows).map(i =>
      (1 to nCols).map(j =>
        cofactor(i, j))))

  def inverse(): Matrix = {
    val d = determinant
    require(d != 0, "not linearly independent")
    cofactorM.transpose / d
  }

  implicit val dim = nCols
  implicit def scalarToMatrix(i: Int)(implicit dim: Int): Matrix = {
    Matrix.diagonalM(dim, i)
  }
  implicit def scalarToMatrix(s: Double)(implicit dim: Int): Matrix = {
    Matrix.diagonalM(dim, s)
  }

}



/*
 * 
val m = new Matrix(List(List(1.0,2,3),List(1,4,8),List(3,9,.5)))
val m = new Matrix(List(List(1.0,2,3),List(4,5,6),List(7,8,9)))

val a = new Matrix(List(List(1,2),List(3,4),List(5.0,6)))
*/
