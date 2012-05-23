package com.hartenbower.matrix

import MatrixD._
import Util._

object TestMatrixD {
  def testScalarToMatrix {
    val m = MatrixD.randn(4, 4)
    println(m)
    val m2 = 2 + m
    val m3 = m + 2

  }
  
  def sumVsFold(count: Int) {
    val m = MatrixD.randn(500, 500)
    var sFold: Double = 0
    var sSum: Double = 0
    var aSum: Double = 0
    time("fold ", sFold = (0d /: m.elements)(_ + _), count)
    time("sum ", sSum = sum(m.elements), count)
    time("arry.sum ", aSum = m.elements.sum, count)
    println("sanity " + sFold + " == " + sSum + " " + (sFold == sSum))
  }
  
  
}