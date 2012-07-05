package com.hartenbower.matrix
import java.io._
import LogisticRegression._
import Util._
import Util.Io.RichFile.enrichFile
import MatrixD._
import org.junit.Test
object TestCollabFilter {

  @Test
  def testFilter {
    // import com.hartenbower.matrix._; import LogisticRegression._ ; import NeuralNet._; import Util._;import java.io._; import Util.Io.RichFile.enrichFile
    val f = Io.parseOctaveDataFile("ex8_movies.txt")
    val ff = Io.parseOctaveDataFile("ex8_movieParams.txt")
    val x = ff.get("X").get.asInstanceOf[MatrixD]
    val y = f.get("Y").get.asInstanceOf[MatrixD]
    val theta = ff.get("Theta").get.asInstanceOf[MatrixD]
    val (m, n) = x.dims()
    val r = f.get("R").get.asInstanceOf[MatrixD]
    var lambda = 0d
    var j = CollaborativeFiltering.cost(x, theta, y, r, lambda)
    lambda = 1.5
    j = CollaborativeFiltering.cost(x, theta, y, r, lambda)
  }
}