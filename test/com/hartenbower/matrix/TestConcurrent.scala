package com.hartenbower.matrix
import Util.Concurrent
class TestConcurrent {

  def chunk(arry: Array[Double])(range: (Long, Long))() = {
    var s = 0d
    var i = range._1.asInstanceOf[Int]
    while (i <= range._2.asInstanceOf[Int]) {
      s += arry(i)
      i += 1
    }
    s
  }

  def chunkDc() {
    val a = Array(1, 2d, 3, 4, 5, 6)
    val l = a.length
    assert(21d == Concurrent.aggregateD(Concurrent.distribute(l, chunk(a))))
  }

}