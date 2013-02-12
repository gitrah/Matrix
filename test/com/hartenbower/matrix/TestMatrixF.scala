package com.hartenbower.matrix

import org.junit.Test
import Util._
import Util.Timing._

class TestMatrixF {
  val ma = new MatrixF(Array(1.0f, 2, 3, 1, 4, 8, 3, 9,0.5f), 3)
  val m1 = new MatrixF(Array(1.0f, 2, 3, 4.0f), 2)
  val m2 = new MatrixF(Array(3.0f, 4, 5, 6), 2)
  val m2b = new MatrixF(Array(1.0f, 2, 3, 4, 5, 6), 3)
  val m2c = new MatrixF(Array(1.0f, 2, 3, 4, 5, 6), 2)
  val m3 = new MatrixF(Array(1.0f, 2, 3, 4, 5, 6, 7, 8, 9), 3)
  val m3b = new MatrixF(Array(1.0f, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 3)
  val m3c = new MatrixF(Array(1.0f, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 4)
  val m4 = new MatrixF(Array(1.0f, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 4)
  val m4b = new MatrixF(Array(1.0f, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20), 4)
  val m5 = new MatrixF(Array(1.0f, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25), 5)
  val m5b = new MatrixF(Array(1.0f, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30), 5)

  def print4by4(arry: Array[Float], off: Int) {
    val sb = new StringBuffer()
    var i = 0
    var j = 0
    while (i < 4) {
      sb.append("[");
      j = 0
      while (j < 4) {
        sb.append(arry(off + j * 4 + i));
        sb.append(",");
        j += 1
      }
      sb.setLength(sb.length() - 1);
      sb.append("]\n");
      i += 1
    }
    print(sb.toString());
  }

  @Test
  def testActorPerformance() = {
    val size = 2000000
    val i4 = MatrixF.diagonalM(4, 1f)
    val z4 = MatrixF.diagonalM(4, 0f)

    val res = Array.fill(size * 16)(0f)
    val res2 = Array.fill(size * 16)(0f)
    val m4 = MatrixF.randn(4, 4)
    println("m4")
    print4by4(m4.elements, 0)
    val v4 = MatrixF.randn(4, 1)
    val targ = new Array[Float](size * 16)
    MatrixF.fill(m4, targ)

    var l = List[Tuple2[Int, Long]]()
    println("mulregular with count " + res.length / 16);
    var start = System.currentTimeMillis()
    time("mulreg", MatrixF.mult4by4Range(res.length / 16, i4.elements, 0, targ, 0, res, 0), 100)
    (1 to 15).map(
      (i) => l = l :+ time("mulwork " + i, MatrixF.mult4by4Worker(i4.elements, targ, res2, i)))
    println("last res")
    print4by4(res, size * 16 - 16)
    MatrixF.fill(z4, res)

    (1 to (15, 5)).map(
      (i) => l = l :+ time("multhread Same " + i, MatrixF.mult4by4Threaded(i4.elements, targ, res2, i)))
    (1 to 20).map(
      (i) => l = l :+ time("multhread Pool " + i, MatrixF.mult4by4Threaded(i4.elements, targ, res2, i)(ThreadPoolStrategy)))
    println("last targ")
    print4by4(targ, size * 16 - 16)
    println("first res2")
    print4by4(res2, 0)
    println("last res2")
    print4by4(res2, size * 16 - 16)
    println("bef last res2")
    print4by4(res2, size * 16 - 32)
    MatrixF.fill(z4, res2)

    MatrixF.fill(v4, targ)

    (1 to 20).map(
      (i) => l = l :+ time("multhread vec Pool " + i, MatrixF.mult4by4VecThreaded(i4.elements, targ, res2, i)(ThreadPoolStrategy), 100))

    println("\n" + l.mkString("\n"))
    ()
  }

  //MatrixF.mult4by4Range(res.length / 16, i4.elements, 0, targ, 0, res, 0)
  //MatrixF.mult4by4Worker(i4.elements, targ, res2)

  def timeTransposition(total: Long) = {
    val m3b = new MatrixF(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 3)
    val m3c = new MatrixF(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 4)
    var count = total
    var l = System.currentTimeMillis
    var res = 0.0
    while (count > 0) {
      m3b.transposeN()
      count -= 1
    }
    var r = elapsed("MatrixF.transpose(" + total + ")", l)
    l = r._1
    var delta1 = r._2
    count = total
    val map = new java.util.HashMap[Int, Float]()
    while (count > 0) {
      m3b.transposeIp(map)
      count -= 1
    }
    r = elapsed("Matrix.transpose(map)(" + total + ")", l)
    l = r._1
    var delta2 = r._2
    println("ratio is " + (1.0 * delta1 / delta2))
  }

  def compareMult(lim: Int) = {
    var mats = Array(
      MatrixF.randn(5, 5, 5),
      MatrixF.randn(25, 25, 5),
      MatrixF.randn(50, 50, 5),
      MatrixF.randn(100, 100, 5),
      MatrixF.randn(500, 500, 50),
      MatrixF.randn(1000, 1000, 50),
      MatrixF.randn(5000, 5000, 50))
    var m: MatrixF = null
    var i = 0
    while (i < mats.length - 1) {
      m = mats(i)
      time("i: " + i + ", slowMult ", m slowMult m, lim)
      time("i: " + i + " * ", m multSequential m, lim)
      time("i: " + i + ", multChunkty ", m multDc m, lim)
      i += 1
    }
  }

  def testSumSqrDiff() { // import com.hartenbower.matrix._; import Util._;  import Util.Timing._
    val m1 = MatrixF.ones(1000, 1000)
    val m2 = MatrixF.ones(1000, 1000) * 2
    var s = 0f
    time("100000", s += m1.sumSquaredDiffs(m2), 100000)
    println("s " + s)

  }
  def testSumSqrDiffDc() { // import com.hartenbower.matrix._; import Util._;  import Util.Timing._
    val m1 = MatrixF.ones(1000, 1000)
    val m2 = MatrixF.ones(1000, 1000) * 2
    var s = 0f
    time("100000", s += m1.sumSquaredDiffsDc(m2), 100000)
    println("s " + s)

  }
  def testMultDc() { // import com.hartenbower.matrix._; import Util._;  import Util.Timing._
    val m1 = MatrixF.ones(1000, 1000)
    val m2 = MatrixF.ones(1000, 1000) * 2
    val c = new Array[Float](m1.nRows * m2.nCols);
    time("100000", m1.multDc(m2, c), 1000)
    val m3 = new MatrixF(c, m2.nCols);
    println("m3.sum " + m3.sum());

  }
  def testMult() { // import com.hartenbower.matrix._; import Util._;  import Util.Timing._
    val m1 = MatrixF.sin(400, 400)
    val m2 = MatrixF.cos(400, 400)
    val m3 = m1 * m2;
    println("m3.sum " + m3.sum());
  }
  def testCg() { // import com.hartenbower.matrix._; import Util._;  import Util.Timing._
    val m400 = MatrixD.sin(400, 400)

    val i400 = MatrixD.identityM(400);
    val lu400 = new LUDecomposition(m400);
    val inv400 = lu400.solve(i400);

  }
}