package com.hartenbower.matrix

object Matrix {
  def apply(nCols: Int, els: Double*): Matrix = {
    def splitRowsWorker(inList: List[Double],working: List[List[Double]]): List[List[Double]] = {
      if (inList.isEmpty)
        working
      else {
        val (a, b) = inList.splitAt(nCols)
        splitRowsWorker(b, working :+ a )
      }
    }
    def splitRows(inList: List[Double]) =
      splitRowsWorker(inList, List[List[Double]]())
    val rows: List[List[Double]] =
      splitRows(els.toList)
    new Matrix(rows)
  }

  def identityM(dim: Int, d: Double = 1.0): Matrix = {
    new Matrix(
      (for (i <- 1 to dim) yield {
        (for (j <- 1 to dim)
          yield if (i == j) d else 0.).toList
      }).toList)
  }

  // could also define one as transpose of other... 
  def rowMatrix(l : List[Double]) : Matrix = {
    new Matrix(List(l))
  }
  
  def columnMatrix(l : List[Double]) : Matrix = {
    new Matrix(for( i <- l) yield List(i))
  }
 
}

//import Matrix._

class Matrix(els: List[List[Double]]) {
  /**
   * elements of the matrix, stored as a list of
   * its rows
   */
  val elements: List[List[Double]] = els

  def nRows: Int = elements.length
  def nCols: Int = if (elements.isEmpty) 0
  else elements.head.length
  
  /**
   * all rows of the matrix must have the same
   * number of columns
   */
  require(elements.forall(_.length == nCols))

  def apply(row : Int, col : Int) : Double = {
    require(col <= nCols && row <= nRows)
    elements(row-1)(col-1)
  }

  private def addRows(a: List[Double],
    b: List[Double]): List[Double] =
    List.map2(a, b)(_ + _)

  private def subRows(a: List[Double],
    b: List[Double]): List[Double] =
    List.map2(a, b)(_ - _)

  def +(other: Matrix): Matrix = {
    require((other.nRows == nRows) &&
      (other.nCols == nCols))
    new Matrix(
      List.map2(elements, other.elements)(addRows(_, _)))
  }
  def +(s : Double) : Matrix = {
    new Matrix(for(row <- elements) yield(row.map(_+s)))
  }
  def -(s : Double) : Matrix = {
    new Matrix(for(row <- elements) yield(row.map(_-s)))
  }
  def *(s : Double) : Matrix = {
    new Matrix(for(row <- elements) yield(row.map(_*s)))
  }
  def /(s : Double) : Matrix = {
    new Matrix(for(row <- elements) yield(row.map(_/s)))
  }

  def -(other: Matrix): Matrix = {
    require((other.nRows == nRows) &&
      (other.nCols == nCols))
    new Matrix(
      List.map2(elements, other.elements)(subRows(_, _)))
  }

  def transpose(): Matrix =
    new Matrix(List.transpose(elements))

  private def dotVectors(a: List[Double],
    b: List[Double]): Double = {
    val multipliedElements =
      List.map2(a, b)(_ * _)
    (0.0 /: multipliedElements)(_ + _)
  }

  def *(other: Matrix): Matrix = {
    require(nCols == other.nRows)
    val t = other.transpose()
    new Matrix(
      for (row <- elements) yield {
        for (otherCol <- t.elements)
          yield dotVectors(row, otherCol)
      })
  }
  
  def prependColumn(col : List[Double]) : Matrix = {
    require(col.length == nRows)
    val i = col.iterator
    new Matrix(
    	for(row <- elements) yield i.next :: row
    )
  }

  def appendColumn(col : List[Double]) : Matrix = {
    require(col.length == nRows)
    val i = col.iterator
    new Matrix(
    	for(row <- elements) yield row :+ i.next 
    )
  }

  
  override def toString(): String = {
    val rowStrings =
      for (row <- elements)
        yield row.mkString("[", ", ", "]")
    rowStrings.mkString("", "\n", "\n")
  }

  def sgn(row : Int, col : Int) : Int = {
    require(row <= nRows && col <= nCols)
    var l = -1
    for( k <- 0 to row + col)
      l *= -1
    l
  }
  
  def minorM(row : Int, col: Int) : Matrix = {
    new Matrix(
	(for(y <- 0 to nRows-1 if row-1 != y) yield {
		(for(x <- 0 to nCols -1 if col-1 != x && row-1 != y) 
		   yield elements(y)(x)).toList
	}).toList)
  }
  
  def minor(row : Int, col: Int) : Double = {
    require(col <= nCols, row <= nRows)
    new Matrix(
    	(for(y <- 0 to nRows-1 if row-1 != y) yield {
    		(for(x <- 0 to nCols -1 if col-1 != x && row-1 != y) 
    		   yield elements(y)(x)).toList
    	}).toList
    ).determinant
  }

  def determinant : Double = {
    require(nCols == nRows, "not square")
    nCols match {
      case 1 =>
          elements(0)(0)
      case 2 =>
         elements(0)(0) * elements(1)(1) - elements(0)(1)* elements(1)(0)
      case _ =>
	      // cofactor expansion
	      (0.0  /: (for(i <- 1 to nRows) yield apply(i,1) * cofactor(i,1))) (_+_)
    }
  }
  
  def cofactor(row : Int, col: Int) : Double = {
	 minor(row,col) * sgn(row,col) 
  }

  def cofactorM() : Matrix = {
    new Matrix(
        (for(row <- 1 to nRows) yield {
        	(for(col <- 1 to nCols) yield cofactor(row,col)).toList
        }).toList)
  }
  
  def inverse() : Matrix = {
    val d = determinant
    require(d != 0, "not linearly independent")
    cofactorM.transpose / d
  }
  
  implicit val dim = nCols
  implicit def scalarToMatrix(i: Int)(implicit dim: Int): Matrix = {
    Matrix.identityM(dim, i)
  }
  implicit def scalarToMatrix(s: Double)(implicit dim: Int): Matrix = {
    Matrix.identityM(dim, s)
  }
  

}
/*
 * 
val m = new Matrix(List(List(1.,2,3),List(1,4,8),List(3,9,.5)))

val a = new Matrix(List(List(1,2),List(3,4)))
*/

