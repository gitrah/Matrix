package com.hartenbower.matrix
import Util._
import NeuralNet._
object TestConjGrad {
 // import com.hartenbower.matrix._; import LogisticRegression._ ; import NeuralNet._; import Util._;import java.io._; import Util.Io.RichFile.enrichFile
  val f = Io.parseOctaveDataFile("ex4data1.txt")
  val fw = Io.parseOctaveDataFile("ex4weights.txt")
  val x = f.get("X").get.asInstanceOf[MatrixD]
  val y = f.get("y").get.asInstanceOf[MatrixD]
  val theta1 = fw.get("Theta1").get.asInstanceOf[MatrixD]
  val theta2 = fw.get("Theta2").get.asInstanceOf[MatrixD]
  val (m, n) = x.dims
  val mus = x.featureAveragesDc
  var sigma : MatrixD = null
  var sigma2 : MatrixD = null
  Util.Timing.time("sigma", sigma = AnomalyDetection.sigma(x, mus) )
  Util.Timing.time("sigma2", sigma2 = AnomalyDetection.sigmaDc(x, mus) )
  
  val input_layer_size = 400 // 20x20 Input Images of Digits
  val hidden_layer_size = 25  //   25 hidden units
  val num_labels = 10 // 10 labels, from 1 to 10   
  var nfeatures = theta1.nCols
  assert(num_labels == nfeatures, "feature mismatch")
  
  val thetas = theta1.makeRowVector() ++ theta2.makeRowVector()
  
  var lambda = 0d

  var (j,grad) = nnCostFunction(thetas,input_layer_size, hidden_layer_size, num_labels, x, y, lambda)
  
  println("cost " + j)
  lambda = 1d
  val tup = nnCostFunction(thetas,input_layer_size, hidden_layer_size, num_labels, x, y, lambda)
  println("cost " + tup._1)
}