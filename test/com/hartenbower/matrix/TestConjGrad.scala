package com.hartenbower.matrix
import Util._
object TestConjGrad {
 // import com.hartenbower.matrix._; import LogisticRegression._ ; import NeuralNet._; import Util._;import java.io._; import Util.Io.RichFile.enrichFile
  val f = Io.parseOctaveDataFile("ex4data1.txt")
  val x = f.get("X").get.asInstanceOf[MatrixD]
  val (m, n) = x.dims
  val mus = x.featureAveragesDc
  val sigma = AnomalyDetection.sigma(x, mus)

}