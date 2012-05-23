package com.hartenbower.matrix

import LogisticRegression._

class TestNeuralNet {
  def testCostHandwriting() = {
    // import com.hartenbower.matrix._; import LogisticRegression._ ; import NeuralNet._


    val f = Util.parseOctaveDataFile("ex3data1.txt")
    val x = f.get("X").get.asInstanceOf[MatrixD]
    val y = f.get("y").get.asInstanceOf[MatrixD]
    val m = x.nRows
    val f2 = Util.parseOctaveDataFile("ex3weights.txt")
    val theta1 = f2.get("Theta1").get.asInstanceOf[MatrixD]
    val theta2 = f2.get("Theta2").get.asInstanceOf[MatrixD]
    val thetas = Array(theta1,theta2)
    val (hTheta, zs, as) = NeuralNet.predict(thetas,x)
    val preds = hTheta.toRowMaxIndices
    val acc = Util.accuracy(y.elements, preds)
    
    val ff = Util.parseOctaveDataFile("all_theta.txt")
    val all_theta : MatrixD = ff.get("all_theta").get.asInstanceOf[MatrixD]
    
    val yb = y.toBinaryCategoryMatrix
    val a1 = x.addBiasCol()
    val z2 = theta1 * a1.tN
    val a2 = z2.elementOp(sigmoidD).transposeIp().addBiasCol
    val z3 = (theta2 * a2.tN).transposeIp()
    val a3 = z3.elementOp(sigmoidD)
    val lambda =1 
    val jNoReg = costFunctionNoReg(a3, yb, m)

    NeuralNet.forwardAndback(x,y,thetas,1)

  }
}