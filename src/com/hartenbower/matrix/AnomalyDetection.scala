package com.hartenbower.matrix

import Util._
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
  def p(x: Array[Double], params: (Array[Double], Array[Double])) = probabilityDensityFunction(x, params)

  def sigma(x: MatrixD, mus: Array[Double]): MatrixD = {
    val (m, n) = x.dims
    var sigma = MatrixD.zeros(n, n)
    var i = m - 1
    var mat: MatrixD = MatrixD.zeros(n, 1)
    while (i > -1) {
      Array.copy(x.elements, i * n, mat.elements, 0, n)
      Array.copy(mat.elements - mus, 0, mat.elements, 0, n)
      sigma = sigma + mat * mat.tN
      i -= 1
    }
    sigma / m
  }

  def sigmaVector(x: MatrixD, mus: Array[Double]): MatrixD = {
    val normed = x.normalizeSqrWithDc(mus)
    new MatrixD(normed.featureAveragesDc, x.nCols) 
  }

  def sigmaChunk(xElems: Array[Double], mus: Array[Double], m: Int, n: Int)(range: (Long, Long))(): Array[Double] = {
    var sigma = Array.fill(n * n)(0d)
    var i = range._1.asInstanceOf[Int]
    val end = range._2.asInstanceOf[Int]
    var mat = new Array[Double](n)
    while (i <= end) {
      Array.copy(xElems, i * n, mat, 0, n)
      Array.copy(mat - mus, 0, mat, 0, n)
      sigma = sigma + Math.transposeDot(mat)
      i += 1
    }
    sigma
  }

  def sigmaDc(x: MatrixD, mus: Array[Double]): MatrixD = {
    val (m, n) = x.dims
    val sigma = Concurrent.aggregateDA(n * n, Concurrent.distribute(m, sigmaChunk(x.elements, mus, m, n)))
    new MatrixD(sigma, n) / m
  }

  // multiply a 1 by N matrix by its transpose
  def transMultOneByN(a: Array[Double]) {
    var s = 0d
    var i = a.length - 1
    while (i > -1) {
      s += a(i) * a(i)
      i -= 1
    }
    s
  }

  // multiply an N by 1 matrix (as row major array) by its transpose
  def addTransMultNByOne(a: Array[Double], out: Array[Double]) {
    val n = a.length
    val l = n * n
    var i = 0
    while (i < l) {
      out(i) += a(i / n) * a(i % n)
      i += 1
    }
  }

  def sigmaTm(x: MatrixD, mus: Array[Double]): MatrixD = {
    val (m, n) = x.dims
    var sigma = MatrixD.zeros(n, n)
    var i = 0
    var diff: Array[Double] = null
    while (i < m) {
      val diff = x.elements.-(mus, i * n, 0)
      addTransMultNByOne(diff, sigma.elements)
      i += 1
    }
    sigma / m
  }

  def multiVariateProbDensity(x: MatrixD, params: (Array[Double], MatrixD)) = {
    val mus = params._1
    var  sigma  = params._2
    if(sigma.nCols == 1 || sigma.nRows == 1 && sigma.elements.length > 1) {
      sigma = MatrixD.diag(sigma.elements)
    }
    val n = mus.length
    val xMinusMu = x.normalizeWithDc(mus)
    val fac1 = (math.pow(twoPi, -n / 2d) / math.sqrt(sigma.determinant))
    println("fac1 " +fac1)
    (((xMinusMu * sigma.inverse) ** xMinusMu).toColumnSumVector() * (-.5)).elementOpDc(math.exp) * fac1
  }
  
  def selectThreshold(y : MatrixD, p : MatrixD) : (Double,Double) = {
    var truePos = y.sum()
    var falsePos = 0d
    var falseNeg = 0d
    val (min,max) = p.featureMinMax()(0)
    val stepsize = (max - min)/1000d
    var epsilon = min + stepsize
    var cvPredictions : MatrixD = null
    var f1Score = 0d
    var bestEpsilon = 0d
    var bestF1 = 0d
    var precision = 0d
    var recall = 0d
    while(epsilon <= max) {
      cvPredictions = p.elementOpDc( x => if( x < epsilon) 1d else 0d)
      //tp = sum((cvPredictions == 1) & (yval == 1));
      truePos = (cvPredictions && y).sumDc
      //fp = sum((cvPredictions == 1) & (yval == 0));
      falsePos = (cvPredictions && ( y.elementOpDc( x => if(x== 0) 1d else 0))).sumDc
      //fn = sum((cvPredictions == 0) & (yval == 1));
      falseNeg = (cvPredictions.elementOpDc( x => if(x== 0) 1d else 0) && y).sumDc
      //println("epsilon " + epsilon + ", tp " + truePos + ", fp " + falsePos + ", fN " + falseNeg)
	    precision = truePos / (truePos + falsePos)
	    recall = truePos/(truePos + falseNeg)
	    f1Score = 2 * precision * recall /(precision + recall)
	    if (f1Score > bestF1) {
       bestF1 = f1Score
       bestEpsilon = epsilon
	    }
      epsilon += stepsize
    }
  
    (bestEpsilon, bestF1)
  }
}