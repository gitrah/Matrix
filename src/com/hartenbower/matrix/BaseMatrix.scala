package com.hartenbower.matrix
import scala.util.Random

object BaseMatrix {

  def negate[@specialized(Double, Float) N](elements: Array[N])(implicit numeric: Numeric[N]) = {
    var i = elements.length - 1
    while (i >= 0) {
      elements(i) = numeric.negate(elements(i))
      i -= 1
    }
    this
  }

  @specialized(Double, Float)
  def negate2[N](elements: Array[N])(implicit numeric: Numeric[N]) = {
    var i = elements.length - 1
    while (i >= 0) {
      elements(i) = numeric.negate(elements(i))
      i -= 1
    }
    this
  }

  def negateNs[N](elements: Array[N])(implicit numeric: Numeric[N]) = {
    var i = elements.length - 1
    while (i >= 0) {
      elements(i) = numeric.negate(elements(i))
      i -= 1
    }
    this
  }

}
