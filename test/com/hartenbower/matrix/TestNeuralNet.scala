package com.hartenbower.matrix
import java.io._
import LogisticRegression._
import Util._
import Util.Io.RichFile.enrichFile
import MatrixD._
class TestNeuralNet {
  def testCostHandwriting() = {
    // import com.hartenbower.matrix._; import LogisticRegression._ ; import NeuralNet._; import Util._


    val f = Io.parseOctaveDataFile("ex3data1.txt")
    val x = f.get("X").get.asInstanceOf[MatrixD]
    val y = f.get("y").get.asInstanceOf[MatrixD]
    val m = x.nRows
    val f2 = Io.parseOctaveDataFile("ex3weights.txt")
    val theta1 = f2.get("Theta1").get.asInstanceOf[MatrixD]
    val theta2 = f2.get("Theta2").get.asInstanceOf[MatrixD]
    val thetas = Array(theta1,theta2)
    val (hTheta, zs, as) = NeuralNet.predict(thetas,x)
    val preds = hTheta.toRowMaxIndices
    val acc = Math.accuracy(y.elements, preds)
    
    val ff = Io.parseOctaveDataFile("all_theta.txt")
    val all_theta : MatrixD = ff.get("all_theta").get.asInstanceOf[MatrixD]
    
    val yb = y.toBinaryCategoryMatrix
    val a1 = x.addBiasCol()
    val z2 = theta1 * a1.tN
    val a2 = z2.elementOp(sigmoidD).transposeIp().addBiasCol
    val z3 = (theta2 * a2.tN).transposeIp()
    val a3 = z3.elementOp(sigmoidD)
    val lambda =1 
    val jNoReg = costFunctionNoReg(a3, yb, m)

    var tup :  Tuple2[Double, Array[com.hartenbower.matrix.MatrixD]]= null
    val epsilon = .25
    val delta = 1e-11
    val alpha = 1.5
    tup=  NeuralNet.forwardAndBack(x,y,thetas,1)
    tup = (tup._1, thetas  - (alpha * tup._2 ))
    Util.Timing.time("desc", tup = NeuralNet.descend(x, y, tup._2, 1000, epsilon, lambda, alpha,delta),1)
    var descThetas = tup._2
    Util.Timing.time("desc", tup = NeuralNet.descend(x, y, descThetas, 1000, epsilon, lambda, alpha,delta),1)
    val (hTheta2, zs2, as2) = NeuralNet.predict(descThetas,x)
    val preds2 = hTheta2.toRowMaxIndices
    val acc2 = Math.accuracy(y.elements, preds2)
 
    val writer = new File("thetas.bin")
    writer.obj = descThetas
    
    val reader = new File("thetas.bin")
    descThetas = reader.obj.asInstanceOf[Array[MatrixD]]
  }
}