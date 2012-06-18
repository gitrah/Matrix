package com.hartenbower.matrix

object AnomalyDetection {
  val oneOverSqrt2Pi = 1d / math.sqrt(2 * math.Pi)
  val twoPi = 2 * math.Pi

  def fitGaussians(x: MatrixD): (Array[Double], Array[Double]) = {
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

}