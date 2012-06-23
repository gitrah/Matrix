package com.hartenbower.matrix

import Util.Math._
object AnomalyDetection {
  val oneOverSqrt2Pi = 1d / math.sqrt(2 * math.Pi)
  val twoPi = 2 * math.Pi

  def fitGaussians(x: MatrixD): (Array[Double], Array[Double]) = {
    val mus = x.featureAveragesDc
    (mus, x.varianceDc(mus))
  }

  // 
  def fitMultiVariateGaussians(x: MatrixD): (Array[Double], Array[Double]) = {
    val mus = x.featureAveragesDc
    (mus, x.varianceDc(mus))
  }

  /**
   * @param x sample to be tested
   * @param params Gaussian/Normal distribution parameters, an array of feature means and the total variance
   * @return probability sample is an anomaly
   *          n     1             (xj - μj)^2
   * p(x) = 	Π   -------- exp( - ---------   )
   *         j=1   √(2Piσ)           2σj^2
   */
  def probabilityDensityFunction(x: Array[Double], params: (Array[Double], Array[Double])) = {
    val mus = params._1
    val sigmas = params._2
    val m = x.length
    var pc = 1d
    var i = 0
    var dist = 0d
    var sigma = 0d
    while (i < m) {
      sigma = sigmas(i)
      dist = (x(i) - mus(i) / sigma)
      pc *= 1 / (math.sqrt(twoPi * sigma)) * math.exp(-(dist * dist / 2))
      i += 1
    }
    pc
  }
  def p(x: Array[Double], params: (Array[Double], Array[Double])) = probabilityDensityFunction(x,params)

  def sigma(x:MatrixD, mus: Array[Double]) : MatrixD = {
    val (m,n) = x.dims
    var sigma = MatrixD.zeros(n,n)
    var i = m-1
    var mat : MatrixD = MatrixD.zeros(n,1)
    while(i > -1) {
      Array.copy(x.elements,i*n, mat.elements, 0, n )
      Array.copy(mat.elements - mus,0, mat.elements,0,n)
      sigma = sigma + mat * mat.tN
      i-=1
    }
    sigma / m
  }
  
  // multiply a 1 by N matrix by its transpose
  def transMultOneByN(a : Array[Double])  {
    var s = 0d
    var i = a.length-1
    while(i > -1) {
      s += a(i) * a(i)
      i -=1
    }
    s
  }
  
  // multiply an N by 1 matrix (as row major array) by its transpose
  def addTransMultNByOne(a : Array[Double], out: Array[Double])  {
    val n = a.length
    val l = n * n
    var i = 0
    while(i < l) {
      out(i) += a(i / n) * a(i % n)
      i +=1
    }
  }
  
  def sigmaTm(x:MatrixD, mus: Array[Double]) : MatrixD = {
    val (m,n) = x.dims
    var sigma = MatrixD.zeros(n,n)
    var i = 0
    var diff : Array[Double] = null
    while(i < m) {
      val diff = x.elements.-(mus, i*n,0)
      addTransMultNByOne(diff, sigma.elements)
      i += 1
    }
    sigma / m
  }
  
  def multiVariateProbDensity(x:Array[Double], params : (Array[Double],MatrixD)) = {
    val (mus, sigma) = params
    val n = x.length
    val xminmuArray = x - mus
    val xMinusMu = new MatrixD(xminmuArray, n)
    1/(math.pow(twoPi,n/2) * math.sqrt(params._2.determinant)) * math.exp( ( xMinusMu.tN * sigma.inverse * xMinusMu).elements(0) * (-.5)  )
  }
}