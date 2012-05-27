package com.hartenbower.matrix
import Util._
import LogisticRegressionF._

object TestIo {
  def testFloat {
    val f = Io.parseOctaveDataFile("ex3data1.txt", false)
    val x = f.get("X").get.asInstanceOf[MatrixF]
    val y = f.get("y").get.asInstanceOf[MatrixF]
    val m = x.nRows
    val f2 = Io.parseOctaveDataFile("ex3weights.txt", false)
    val theta1 = f2.get("Theta1").get.asInstanceOf[MatrixF]
    val theta2 = f2.get("Theta2").get.asInstanceOf[MatrixF]
    val thetas = Array(theta1,theta2)
    val (hTheta, zs, as) = NeuralNetF.predict(thetas,x)
    val preds = hTheta.toRowMaxIndices
    val acc = Math.accuracy(y.elements, preds)
    
    val ff = Io.parseOctaveDataFile("all_theta.txt")
    val all_theta : MatrixD = ff.get("all_theta").get.asInstanceOf[MatrixD]
    
    val yb = y.toBinaryCategoryMatrix
    val a1 = x.addBiasCol()
    val z2 = theta1 * a1.tN
    val a2 = z2.elementOp(sigmoidF).transposeIp().addBiasCol
    val z3 = (theta2 * a2.tN).transposeIp()
    val a3 = z3.elementOp(sigmoidF)
    val lambda =1 
    val jNoReg = costFunctionNoReg(a3, yb, m)
       
    NeuralNetF.forwardAndBack(x,y,thetas,1)
    val epsilon = .25f
    val delta = 1e-11f
    val alpha = 1.5f
    var tup :  Tuple2[Float, Array[com.hartenbower.matrix.MatrixF]]= null
    Util.Timing.time("desc", tup = NeuralNetF.descend(x, y, Array((25,400),(10,25)), 1000, epsilon, lambda, alpha,delta),1)
    var descThetas = tup._2
    Util.Timing.time("desc", tup = NeuralNetF.descend(x, y, descThetas, 1000, epsilon, lambda, alpha,delta),1)
    descThetas = tup._2
    val (hTheta2, zs2, as2) = NeuralNetF.predict(descThetas,x)
    val preds2 = hTheta2.toRowMaxIndices
    val acc2 = Math.accuracy(y.elements, preds2)

    
  }
}