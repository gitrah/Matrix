package com.hartenbower.matrix
import Util._
import Util.Timing._

class TestMatrix {
  def timeMatrixDot(total: Long) = {
    val v1 = List(0.1, .5, 2, 5, 6)
    val a1 = v1.toArray
    val v2 = List(0.5, 8, 81, 5, .7)
    val a2 = v2.toArray
    var count = total
    var l = System.currentTimeMillis
    var res = 0.
    while (count > 0) {
      res = Matrix.dotVectors(v1, v2)
      count -= 1
    }
    var r = elapsed("Matrix.dotVectors(" + total + ")", l)
    l = r._1
    var delta1 = r._2
    count = total
    while (count > 0) {
      res = Matrix.dotVectorsW(v1, v2)
      count -= 1
    }
    r = elapsed("Matrix.dotVectorsW(" + total + ")", l)
    l = r._1
    var delta2 = r._2
    println("ratio is " + (1.0 * delta1 / delta2))
    count = total
    while (count > 0) {
      res = Matrix.dotVectorsA(a1, a2)
      count -= 1
    }
    r = elapsed("Matrix.dotVectorsA(" + total + ")", l)
    l = r._1
    var delta3 = r._2
    println("ratio is " + (1.0 * delta1 / delta3))
  }

}