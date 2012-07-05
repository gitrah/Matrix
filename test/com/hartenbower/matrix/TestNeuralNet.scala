package com.hartenbower.matrix
import java.io._
import LogisticRegression._
import Util._
import Util.Io.RichFile.enrichFile
import MatrixD._
class TestNeuralNet {
  def testCostHandwriting() = {
    // import com.hartenbower.matrix._; import LogisticRegression._ ; import NeuralNet._; import Util._;import java.io._; import Util.Io.RichFile.enrichFile

    var descThetas: Array[MatrixD] = null

    val thetaFile = new File("thetas.bin")
    if (thetaFile.exists()) {
      descThetas = thetaFile.obj.asInstanceOf[Array[MatrixD]]
    } else {
      val f2 = Io.parseOctaveDataFile("ex3weights.txt")
      val theta1 = f2.get("Theta1").get.asInstanceOf[MatrixD]
      val theta2 = f2.get("Theta2").get.asInstanceOf[MatrixD]
      descThetas = Array(theta1, theta2)
    }
    val f = Io.parseOctaveDataFile("ex3data1.txt")
    val x = f.get("X").get.asInstanceOf[MatrixD]
    val y = f.get("y").get.asInstanceOf[MatrixD]
    val m = x.nRows
    val (hTheta, zs, as) = NeuralNet.predict(descThetas, x)
    val preds = hTheta.maxColIdxs()
    val acc = Math.accuracy(y.elements, preds.elements)

    val ff = Io.parseOctaveDataFile("all_theta.txt")

    val yb = y.toBinaryCategoryMatrix
    val a1 = x.addBiasCol()
    val z2 = descThetas(0) * a1.tN
    val a2 = z2.elementOp(sigmoidD).transposeIp().addBiasCol
    val z3 = (descThetas(1) * a2.tN).transposeIp()
    val a3 = z3.elementOp(sigmoidD)
    val lambda = 1
    val jNoReg = costFunctionNoReg(a3, yb, m)

    var tup: Tuple2[Double, Array[com.hartenbower.matrix.MatrixD]] = null
    val epsilon = .25
    val delta = 1e-11
    val alpha = 1.5
    tup = NeuralNet.forwardAndBack(x, y, descThetas, 1)
    tup = (tup._1, descThetas - (alpha * tup._2))
    Util.Timing.time("desc", tup = NeuralNet.descend(x, y, tup._2, 1000, epsilon, lambda, alpha, delta), 1)
    descThetas = tup._2
    Util.Timing.time("desc", tup = NeuralNet.descend(x, y, descThetas, 1000, epsilon, lambda, alpha, delta), 1)
    descThetas = tup._2
    val (hTheta2, zs2, as2) = NeuralNet.predict(descThetas, x)
    val preds2 = hTheta2.maxColIdxs
    val acc2 = Math.accuracy(y.elements, preds2.elements)

    thetaFile.obj = descThetas
  }
}