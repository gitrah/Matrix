package com.hartenbower.matrix

import MatrixD._
import Util._
import Util.Timing._

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
    time("sum ", sSum = Math.sum(m.elements), count)
    time("arry.sum ", aSum = m.elements.sum, count)
    println("sanity " + sFold + " == " + sSum + " " + (sFold == sSum))
  }

  def tm() = System.currentTimeMillis

  def compareTranspose = {
    var m = MatrixD.randn(500, 5000)
    var i = 0
    var lim = 100

    time("randn ", m = MatrixD.randn(500, 5000), lim)
    time("tranposeN ", { m = MatrixD.randn(500, 5000); m = m.transposeN }, lim)
    time("transposeSlow ", { m = MatrixD.randn(500, 5000); m = m.transposeSlow }, lim)
    time("transposeChunky ", { m = MatrixD.randn(500, 5000); m = m.transposeDc }, lim)
  }

  // import com.hartenbower.matrix._; import Util._ ; import MatrixD._; import Util.Timing._

  def compareMult0 = {
    val m1 = new MatrixD(Array(1, 2d, 5, 10), 2)
    val m2 = new MatrixD(Array(10d, 20, 2, 1), 2)
  }
  def compareMult(lim: Int) = {
    var mats = Array(
      MatrixD.randn(5, 5, 5),
      MatrixD.randn(25, 25, 5),
      MatrixD.randn(50, 50, 5),
      MatrixD.randn(100, 100, 5),
      MatrixD.randn(500, 500, 50),
      MatrixD.randn(1000, 1000, 50),
      MatrixD.randn(5000, 5000, 50))
    var m: MatrixD = null
    var i = 0
    while (i < mats.length-1) {
      m = mats(i)
      time("i: " + i + ", slowMult ", m slowMult m, lim)
      time("i: " + i + " * ", m multSequential m, lim)
      time("i: " + i + ", multChunkty ", m multDc m, lim)
      i += 1
    }
  }
}