package com.hartenbower.matrix
import org.junit.Test

class TestMatrixF {
  val ma = new MatrixF(Array(1.f, 2, 3, 1, 4, 8, 3, 9, .5f), 3)
  val m1 = new MatrixF(Array(1.f, 2, 3, 4.f), 2)
  val m2 = new MatrixF(Array(3.f, 4, 5, 6), 2)
  val m2b = new MatrixF(Array(1.f, 2, 3, 4, 5, 6), 3)
  val m2c = new MatrixF(Array(1.f, 2, 3, 4, 5, 6), 2)
  val m3 = new MatrixF(Array(1.f, 2, 3, 4, 5, 6, 7, 8, 9), 3)
  val m3b = new MatrixF(Array(1.f, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 3)
  val m3c = new MatrixF(Array(1.f, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 4)
  val m4 = new MatrixF(Array(1.f, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 4)
  val m4b = new MatrixF(Array(1.f, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20), 4)
  val m5 = new MatrixF(Array(1.f, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25), 5)
  val m5b = new MatrixF(Array(1.f, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30), 5)

  
  @Test
  def testActorPerformance() = {
      val size =2000000 
	  val i4 = MatrixF.diagonalM(4, 1f)
	
	  val res = Array.fill(size * 16)(0f)
	  val res2 = Array.fill(size * 16)(0f)
	  val m4 = MatrixF.randn(4, 4)
	  val targ = new Array[Float](size * 16)
	  MatrixF.fill(m4, targ)
	
	  time("mulreg", 100, MatrixF.mult4by4Range(res.length / 16, i4.elements, 0, targ, 0, res, 0))
	  (1 to 15).map( 
	      (i) => time("mulwork " + i, 100, MatrixF.mult4by4Worker(i4.elements, targ, res2,i)))
	  ()
  }

  //MatrixF.mult4by4Range(res.length / 16, i4.elements, 0, targ, 0, res, 0)
  //MatrixF.mult4by4Worker(i4.elements, targ, res2)

  def time(msg: String, count: Int, f: => Unit) {
    val l = System.currentTimeMillis
    var idx = count
    while (idx > 0) {
      f
      idx -= 1
    }
    val delta = (System.currentTimeMillis - l)
    println(msg + " took " + delta + "ms or " + (count * 1000. / delta) + "evals/s")
  }

  def time(total: Long) = {
    val m3b = new MatrixF(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 3)
    val m3c = new MatrixF(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 4)
    var count = total
    var l = System.currentTimeMillis
    var res = 0.
    while (count > 0) {
      m3b.transpose()
      count -= 1
    }
    var r = Matrix.elapsed("MatrixF.transpose(" + total + ")", l)
    l = r._1
    var delta1 = r._2
    count = total
    val map = new java.util.HashMap[Int, Float]()
    while (count > 0) {
      m3b.transpose(map)
      count -= 1
    }
    r = Matrix.elapsed("Matrix.transpose(map)(" + total + ")", l)
    l = r._1
    var delta2 = r._2
    println("ratio is " + (1.0 * delta1 / delta2))
  }

}