package com.hartenbower.matrix.view
import com.hartenbower.matrix._
import Util._
object TestViewers {
  def testView {
 //	 import com.hartenbower.matrix._; import com.hartenbower.matrix.view._;import LogisticRegression._ ; import NeuralNet._; import Util._;import java.io._; import Util.Io.RichFile.enrichFile
   var x = MatrixD.randn(50,2,25)
    var y = MatrixD.randn(50,1).elementOp( x => if (x > .5) 1d else 0)
    SgtMViewer.graph(x,y,"hello")
	  val f = Io.parseOctaveDataFile("ex6data1.txt")
	   x = f.get("X").get.asInstanceOf[MatrixD]
	  y = f.get("y").get.asInstanceOf[MatrixD]
    SgtMViewer.graph(x,y,"2")

  }


}