package com.hartenbower.matrix.view
import com.hartenbower.matrix._
object TestMviewer {

  def testViewer {
    // import com.hartenbower.matrix._; import LogisticRegression._ ; import NeuralNet._; import Util._;import java.io._; import Util.Io.RichFile.enrichFile
    val m = MatrixD.identityM(250) * Short.MaxValue
    val mv = new MViewer(m)()
    val mv2 = new MViewer(m)((100, 100))
    val mv3 = new MViewer(m)((500, 500))
    mv.show
    mv2.show
    mv3.show

  }
}