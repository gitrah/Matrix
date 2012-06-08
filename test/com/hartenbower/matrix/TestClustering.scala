package com.hartenbower.matrix
import java.io._
import LogisticRegression._
import Util._
import Util.Io.RichFile.enrichFile
import Clustering._

class TestClustering {

  def testClustering {
    
    // import com.hartenbower.matrix._; import LogisticRegression._ ; import NeuralNet._; import Util._;import java.io._; import Util.Io.RichFile.enrichFile
    val f = Io.parseOctaveDataFile("ex7data2.txt")
  	val x = f.get("X").get.asInstanceOf[MatrixD]
    val xs = x.toArrayArray
    
    val cents = Clustering.kMeansDc(xs, 2, 0)

  
  }
}