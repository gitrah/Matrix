package com.hartenbower.matrix
import Util._
import Util.Math.aboutEq
import AnomalyDetection._
import org.junit.Test
import com.hartenbower.matrix.view.SgtMViewer
class TestAnomDet {

  @Test
  def testAnomalyDetection {
    //import com.hartenbower.matrix._; import LogisticRegression._ ; import NeuralNet._; import Util._;import java.io._; import Util.Io.RichFile.enrichFile;import com.hartenbower.matrix.view._;import AnomalyDetection._
    val f = Io.parseOctaveDataFile("ex8data1.txt")
    val x = f.get("X").get.asInstanceOf[MatrixD]
    val xval = f.get("Xval").get.asInstanceOf[MatrixD]
    var yval = f.get("yval").get.asInstanceOf[MatrixD]
    SgtMViewer.graph(xval, yval, "val")

    val (m, n) = x.dims
    val mus = x.featureAverages
    val sigma = AnomalyDetection.sigmaVector(x, mus)
    val sigmaTm = AnomalyDetection.sigmaTm(x, mus)
    val p = AnomalyDetection.multiVariateProbDensity(x, (mus, sigma))
    val pval = AnomalyDetection.multiVariateProbDensity(xval, (mus, sigma))
    println("pval " + pval)
    val(epsilon, f1Score) = AnomalyDetection.selectThreshold(yval, pval )
    assert(aboutEq(epsilon,8.99085E-5), "bad epsilon")
    assert(aboutEq(f1Score,.875), "bad f1")
    println("epsilon " +epsilon)
    println("f1Score " +f1Score)
    val inliers = p.rowsWhere( _.toList.exists( _ > epsilon))
    val outliers = p.rowsWhere( _.toList.exists( _ < epsilon))
    println("inliers " +inliers.length)
    println("outliers " +outliers.length)
    assert(outliers.length == 6, "bad outlier count")
    val y = MatrixD.ones(x.nRows,1).setRows(inliers, 0)
    SgtMViewer.graph(x, y, "actual")
    
    //import com.hartenbower.matrix._; import LogisticRegression._ ; import NeuralNet._; import Util._;import java.io._; import Util.Io.RichFile.enrichFile
    //val ff = Io.parseOctaveDataFile("ms.txt")
    val m100 = MatrixD.sin(100, 100) // adds

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
    javax.swing.JOptionPane.showMessageDialog(null, "Press Ok to Exit")
    
  }
}