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
    val x = ff.get("X").get.asInstanceOf[MatrixD] //  sampls x features
    val y = f.get("Y").get.asInstanceOf[MatrixD] // sampls x users
    val theta = ff.get("Theta").get.asInstanceOf[MatrixD]  // users x features
    val num_users = 4; val num_movies = 5; val num_features = 3;
    val (m, n) = x.dims()
    val smallX = x.rowSubset( Range(1,num_movies+1).toArray).columnSubset(Range(1,num_features+1).toArray)
    val smallY = y.rowSubset( Range(1,num_movies+1).toArray).columnSubset(Range(1,num_users+1).toArray)
    val smallTh = theta.rowSubset( Range(1,num_users+1).toArray).columnSubset(Range(1,num_features+1).toArray)
    val r = f.get("R").get.asInstanceOf[MatrixD] // sampls x users
    val smallR =  r.rowSubset( Range(1,num_movies+1).toArray).columnSubset(Range(1,num_users+1).toArray)
    var lambda = 0d
    var j = CollaborativeFiltering.cost(x, theta, y, r, lambda)
    var smallJ = CollaborativeFiltering.cost(smallX, smallTh, smallY, smallR, lambda)
    assert(Util.Math.aboutEq(smallJ, 22.22,.005))
    lambda = 1.5
    j = CollaborativeFiltering.cost(x, theta, y, r, lambda)
    smallJ = CollaborativeFiltering.cost(smallX, smallTh, smallY, smallR, lambda)
    assert(Util.Math.aboutEq(smallJ, 31.34,.005))
   }
}