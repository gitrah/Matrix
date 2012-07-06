package com.hartenbower.matrix
import java.io._
import LogisticRegression._
import Util._
import Util.Io.RichFile.enrichFile
import Clustering._
import Util.Math._
import org.junit.Test
class TestClustering {

  @Test
  def testClustering {

    // import com.hartenbower.matrix._; import LogisticRegression._ ; import NeuralNet._; import Util._;import java.io._; import Util.Io.RichFile.enrichFile;import Util.Math._
    val f = Io.parseOctaveDataFile("ex7data2.txt")
    val x = f.get("X").get.asInstanceOf[MatrixD]
    
    val initial_centroids = Array(Array(3d, 3),Array(6d, 2),Array(8d, 5))
    
    val xs = x.toArrayArray
    val idxs = Clustering.findNearestIdxDc(xs, initial_centroids)
    assert(idxs(0)==0, "bad first nearest")
    
    assert(idxs(1)==2, "bad 2nd nearest")
    assert(idxs(2)==1, "bad 3rd nearest")
    
    var cents = Clustering.kMeans(xs, initial_centroids,1)
    assert( aboutEqa(cents(0), Array(2.428301,3.157924)))
    assert( aboutEqa(cents(1), Array(5.813503,2.633656)))
    assert( aboutEqa(cents(2), Array(7.119387,3.616684)))
    val mus = x.featureAverages
    
    val tup = Clustering.kMeansTrials(xs,3,10)
    val kmCents = tup._2
    var i =0
    var count =0
    while(i < kmCents.length) {
      val cent = kmCents(i)
      if( aboutEqa(cent, Array(1.954,5.0256))) count +=1
      if( aboutEqa(cent, Array(3.0437 ,1.0154 ))) count +=1
      if( aboutEqa(cent, Array( 6.0337, 3.0005))) count +=1
      i+=1
    }
    println("found " + count + " expected centroids")
    assert(count >=2, "didn't find expected centroids")
    
  }
}