package com.hartenbower.matrix
import LogisticRegression.{ Limit, sigmoidNs, sigmoid }
import Util._

class TestLogisticRegression {
  def testSigmoidNs = {
    var i = 0
    var z = 0
    while (i < Limit) {
      z += sigmoidNs(i)
      i += 1
    }
    i = 0
    var zf = 0f
    while (i < Limit) {
      zf += sigmoidNs(i * 1f)
      i += 1
    }
    var zd = 0d
    i = 0
    while (i < Limit) {
      zd += sigmoidNs(i * 1d)
      i += 1
    }
    println("sanity z " + z + "; zf " + zf + "; zd " + zd)
  }

  def testSigmoid = {
    var i = 0
    var z = 0
    while (i < Limit) {
      z += sigmoid(i)
      i += 1
    }
    var zf = 0f
    i = 0
    while (i < Limit) {
      zf += sigmoid(i * 1.f)
      i += 1
    }
    var zd = 0d
    i = 0
    while (i < Limit) {
      zd += sigmoid(i * 1.d)
      i += 1
    }
    println("sanity z " + z + "; zf " + zf + "; zd " + zd)
  }

  def tests() {
    Util.Timing.time("nonspec sigmoid", testSigmoidNs)
    Util.Timing.time("spec sigmoid", testSigmoid)
  }

  def testCostFn() {
    // import com.hartenbower.matrix._

    val f = Io.parseOctaveDataFile("ex2data2m.txt")
    val x = f.get("X").get.asInstanceOf[MatrixD]
    val y = f.get("y").get.asInstanceOf[MatrixD]
    val x1 = x.columnVector(1)
    val x2 = x.columnVector(2)
    val xm = MatrixD.mapFeature(x1,x2,6)
    val init_theta = MatrixD.randn(xm.nCols, 1, .25)
    val lambda = 1
    val (j: Double, grad : MatrixD) = LogisticRegression.costGradFunction(xm,y,init_theta, lambda)
    val alpha = .001
    val iters = 1000
    val (j2: Double, theta : MatrixD) = LogisticRegression.iterativeDescent(xm,y,alpha,iters,lambda,.0001,.00001,0)
  }

  def testCostHandwriting() = {
    // import com.hartenbower.matrix._

    val f = Io.parseOctaveDataFile("ex3data1.txt")
    val x = f.get("X").get.asInstanceOf[MatrixD]
    val m = x.nRows
    val y = f.get("y").get.asInstanceOf[MatrixD]
    val yb = y.toBinaryCategoryMatrix
    val xm = x.addBiasCol
    val init_theta = MatrixD.randn(xm.nCols, 1, .25)
    val lambda = 1
    val alpha = 100
    val iters =5000
    val epsilon = 2
    val maxErr = 1e-11
    val gradCheckCount = 0
    var res = List[Double]();
    var thetaRows : MatrixD = null
    for(i <- 1 to yb.nCols) {
      println("feature " + i + "\n\n\n")
      val yact = yb.columnVector(i)
      //val (j2: Double, theta : MatrixD) 
      val tup = LogisticRegression.iterativeDescent(xm,yact,alpha,iters,lambda,epsilon,maxErr,gradCheckCount)
      thetaRows = if(thetaRows == null) tup._2.toRowVector else thetaRows +/ tup._2.toRowVector
      res = res :+ tup._1
    }
    
    val pred  = NeuralNet.predict(Array(thetaRows), xm)._1.transposeIp().toRowMaxIndices
    
    val acc = Math.accuracy(y.elements, pred)
    
    //val test100 = Util.randperm(m).take(100)
    //val xsubset = xm.rowSubset(test100)
    res
  }

}