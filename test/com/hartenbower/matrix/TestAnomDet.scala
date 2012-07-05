package com.hartenbower.matrix
import Util._
import org.junit.Test
class TestAnomDet {

  @Test
  def testAnomalyDetection {
    //import com.hartenbower.matrix._; import LogisticRegression._ ; import NeuralNet._; import Util._;import java.io._; import Util.Io.RichFile.enrichFile
    val f = Io.parseOctaveDataFile("ex8data1.txt")
    val x = f.get("X").get.asInstanceOf[MatrixD]
    val (m, n) = x.dims
    val mus = x.featureAverages
    val sigma = AnomalyDetection.sigma(x, mus)
    val sigmaTm = AnomalyDetection.sigmaTm(x, mus)
    val p1 = AnomalyDetection.multiVariateProbDensity(x, (mus, sigma))
    println("p1 " + p1)

    //import com.hartenbower.matrix._; import LogisticRegression._ ; import NeuralNet._; import Util._;import java.io._; import Util.Io.RichFile.enrichFile
    //val ff = Io.parseOctaveDataFile("ms.txt")
    val m100 = MatrixD.sin(100, 100)

    val m1000 = MatrixD.sin(1000, 1000)
    val lu100 = new LUDecomposition(m100)
    val lu1000 = new LUDecomposition(m1000)

    val inv100 = lu100.solve(MatrixD.identityM(100))

    lu100.det

    val m3 = new MatrixD(Array(1, 2, 3, 0, 5, 6, 7, 8, 0), 3)
    val id3 = MatrixD.identityM(3)
    val lu3 = new LUDecomposition(m3)
    val inv3 = lu3.solve(id3)
    val idm3 = m3 * inv3

    val m7 = MatrixD.randn(7, 7)
    m3.clippedRowSubset(Array(1), (0, 2))
  }
}