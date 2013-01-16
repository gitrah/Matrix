package com.hartenbower.matrix
import Util._
import Util.Timing._
import NeuralNet._
import LogisticRegression._
import ConjugateGradient._
import Util.Math._
import org.junit.Test
class TestConjGrad {

  @Test
  def testCG = {
    // import com.hartenbower.matrix._; import LogisticRegression._ ; import ConjugateGradient._; import NeuralNet._; import Util._;import Util.Timing._;import java.io._; import Util.Io.RichFile.enrichFile
    val f = Io.parseOctaveDataFile("ex4data1.txt")
    val fw = Io.parseOctaveDataFile("ex4weights.txt")
    val x = f.get("X").get.asInstanceOf[MatrixD]
    val y = f.get("y").get.asInstanceOf[MatrixD]
    val theta1 = fw.get("Theta1").get.asInstanceOf[MatrixD]
    val theta2 = fw.get("Theta2").get.asInstanceOf[MatrixD]
    val (m, n) = x.dims
    val mus = x.featureAveragesDc

    val input_layer_size = 400 // 20x20 Input Images of Digits
    val hidden_layer_size = 25 //   25 hidden units
    val num_labels = 10 // 10 labels, from 1 to 10   
    var nfeatures = theta1.nCols
   // assert(num_labels == nfeatures, "feature mismatch")

    val thetas = theta1.poseAsRow() ++ theta2.poseAsRow()
    theta1.unPose()
    theta2.unPose()
    var lambda = 0d

    var (j, grad) = nnCostFunction(thetas, input_layer_size, hidden_layer_size, num_labels, x, y, lambda)

    println("cost " + j)
    lambda = 1d
    val tup = nnCostFunction(thetas, input_layer_size, hidden_layer_size, num_labels, x, y, lambda)
    println("cost (lambada = 1) " + tup._1)

    val sigTest1 = new MatrixD(Array(1, -.5, 0, .5, 1), 5)
    val sigTest2 = MatrixD.randn(5000, 500, 25)
    var g1: MatrixD = null
    var g2: MatrixD = null
    var gdc1: MatrixD = null
    var gdc2: MatrixD = null
    time("sigmoidGradient1", g1 = sigmoidGradient(sigTest1), 5000)
    // time("sigmoidGradient2", g2= sigmoidGradient(sigTest2),100)

    time("sigmoidGradientDc1", gdc1 = sigmoidGradientDc(sigTest1), 5000)
    //time("sigmoidGradientDc2", gdc2= sigmoidGradientDc(sigTest2),100)

   // val initial_Theta1 = MatrixD.randn(hidden_layer_size, input_layer_size).addBiasCol()
    //val initial_Theta2 = MatrixD.randn(num_labels, hidden_layer_size).addBiasCol()

    val initial_Theta1 = MatrixD.sin(hidden_layer_size, input_layer_size).addBiasCol()
    println("initial_Theta1 " + initial_Theta1)
    val initial_Theta2 = MatrixD.cos(num_labels, hidden_layer_size).addBiasCol()
    println("initial_Theta2 " + initial_Theta2)

    // Unroll parameters
    val initial_nn_params = initial_Theta1.poseAsCol +/ initial_Theta2.poseAsCol
    initial_Theta1.unPose
    initial_Theta2.unPose

    checkNnGradients()

    lambda = 3
    checkNnGradients(lambda)

    // def fmincg(f: (MatrixD) => (Double, MatrixD), xin: MatrixD, length: Int = 100, red: Int = 1) {
    var tupcg: (MatrixD, MatrixD, Int) = null
    time("fmincg 50", tupcg = fmincg(nnCostFunction(_, input_layer_size, hidden_layer_size, num_labels, x, y, lambda), initial_nn_params))
    val nn_parms = tupcg._1

    val nn_j = tupcg._2

    val nTheta1 = nn_parms.reshape(hidden_layer_size, (input_layer_size + 1))
    val nTheta2 = nn_parms.reshape(num_labels, (hidden_layer_size + 1), hidden_layer_size * (input_layer_size + 1))
    val p1 = predictCg(nTheta1, nTheta2, x)
    val h1 = p1.maxColIdxs + 1
    val res = (y - h1).elementOpDc(x => if (x == 0d) 1 else 0).featureAverages
    println("accuracy : " + res(0) * 100)
    val p2 = predict(Array(nTheta1, nTheta2), x)
  }
}
