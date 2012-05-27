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
    var m = MatrixD.randn(500,5000)
    var i = 0
    var lim = 100
    
    time("randn ",  m = MatrixD.randn(500,5000), lim)
    time("tranposeN ",  {m = MatrixD.randn(500,5000); m = m.transposeN}, lim)
    time("transposeSlow ", { m = MatrixD.randn(500,5000); m = m.transposeSlow}, lim)
    time("transposeChunky ", { m = MatrixD.randn(500,5000); m = m.transposeChunky}, lim)
  }
  
    // import com.hartenbower.matrix._; import Util._ ; import MatrixD._
  
  def compareMult0= {
    val m1 = new MatrixD(Array(1,2d,5,10),2)
    val m2 = new MatrixD(Array(10d,20,2,1),2)
  }
  def compareMult= {
    var m1 = MatrixD.randn(50,5000)
    var m2 = MatrixD.randn(5000,50)
    var i = 0
    var lim = 1000
    time("slowMult ", m1 slowMult m2, lim)
    time("* ", m1 multSequential m2, lim)
    time("multChunkty ", m1 multChunky m2, lim)
    
  }
  
  
  
  
}