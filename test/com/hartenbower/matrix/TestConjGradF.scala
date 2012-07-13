package com.hartenbower.matrix
import Util._
import Util.Timing._
import NeuralNetF._
import LogisticRegressionF._
import ConjugateGradientF._
import Util.Math._
import org.junit.Test
class TestConjGradF {

  @Test
  def testCG = {
    // import com.hartenbower.matrix._; import LogisticRegressionF._ ; import ConjugateGradientF._; import NeuralNetF._; import Util._;import Util.Timing._;import java.io._; import Util.Io.RichFile.enrichFile
    val f = Io.parseOctaveDataFile("ex4data1.txt",false)
    val fw = Io.parseOctaveDataFile("ex4weights.txt",false)
    val x = f.get("X").get.asInstanceOf[MatrixF]
    val y = f.get("y").get.asInstanceOf[MatrixF]
    val theta1 = fw.get("Theta1").get.asInstanceOf[MatrixF]
    val theta2 = fw.get("Theta2").get.asInstanceOf[MatrixF]
    val (m, n) = x.dims
    val mus = x.featureAveragesDc

    val input_layer_size = 400 // 20x20 Input Images of Digits
    val hidden_layer_size = 25 //   25 hidden units
    val num_labels = 10 // 10 labels, from 1 to 10   
    var nfeatures = theta1.nCols
    assert(num_labels == nfeatures, "feature mismatch")

    val thetas = theta1.poseAsRow() ++ theta2.poseAsRow()
    theta1.unPose()
    theta2.unPose()
    var lambda = 0f

    var (j, grad) = nnCostFunction(thetas, input_layer_size, hidden_layer_size, num_labels, x, y, lambda)

    println("cost " + j)
    lambda = 1f
    val tup = nnCostFunction(thetas, input_layer_size, hidden_layer_size, num_labels, x, y, lambda)
    println("cost (lambada = 1) " + tup._1)

    val sigTest1 = new MatrixF(Array(1, -.5f, 0, .5f, 1), 5)
    val sigTest2 = MatrixF.randn(5000, 500, 25)
    var g1: MatrixF = null
    var g2: MatrixF = null
    var gdc1: MatrixF = null
    var gdc2: MatrixF = null
    time("sigmoidGradient1", g1 = sigmoidGradient(sigTest1), 5000)
    // time("sigmoidGradient2", g2= sigmoidGradient(sigTest2),100)

    time("sigmoidGradientDc1", gdc1 = sigmoidGradientDc(sigTest1), 5000)
    //time("sigmoidGradientDc2", gdc2= sigmoidGradientDc(sigTest2),100)

    val initial_Theta1 = MatrixF.randn(hidden_layer_size, input_layer_size).addBiasCol()
    val initial_Theta2 = MatrixF.randn(num_labels, hidden_layer_size).addBiasCol()

    // Unroll parameters
    val initial_nn_params = initial_Theta1.poseAsCol +/ initial_Theta2.poseAsCol
    initial_Theta1.unPose
    initial_Theta2.unPose

    checkNnGradients()

    lambda = 3
    checkNnGradients(lambda)

    // def fmincg(f: (MatrixF) => (Float, MatrixF), xin: MatrixF, length: Int = 100, red: Int = 1) {
    var tupcg: (MatrixF, MatrixF, Int) = null
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
