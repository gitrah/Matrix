package com.hartenbower.matrix

import Util._

object LogisticRegressionF {
  def sigmoid[@specialized(Double, Float, Int) N](z: N)(implicit numeric: Numeric[N]): N = {
    val g: Double = 1.0 / (1.0 + math.exp(-numeric.toDouble(z)))
    numeric match {
      case i: Integral[N] =>
        (math.round(g)).asInstanceOf[N]
      case fr: Fractional[N] =>
        g.asInstanceOf[N]
    }
  }

  def sigmoidNs[N](z: N)(implicit numeric: Numeric[N]): N = {
    val g: Double = 1.0 / (1.0 + math.exp(-numeric.toDouble(z)))
    numeric match {
      case i: Integral[N] =>
        numeric.fromInt((math.round(g)).asInstanceOf[Int])
      case fr: Fractional[N] =>
        if (numeric == Numeric.DoubleIsFractional)
          g.asInstanceOf[N]
        else
          g.asInstanceOf[Float].asInstanceOf[N]
    }
  }
  val Limit = 50000000

  @inline def sigmoidF(z: Float) = 1.0f / (1.0f + math.exp(-z).asInstanceOf[Float])
  @inline def sigmoidD(z: Double) = 1.0 / (1.0 + math.exp(-z))

  def logF(z: Float) = math.log(z).asInstanceOf[Float]

  def bad_gradientApprox(
    @desc("partial funct with just theta as param") costFn: MatrixF => Float,
    @desc("parameters, weights") theta: MatrixF,
    epsilon: Float): MatrixF = {
    val n = math.max(theta.nRows, theta.nCols)
    var i = 0
    val tempThetaPlusEps = theta.clone
    val tempThetaMinusEps = theta.clone
    val gradApprox = new Array[Float](n)
    while (i < n) {
      tempThetaPlusEps.elements(i) += epsilon
      tempThetaMinusEps.elements(i) -= epsilon
      gradApprox(i) = (costFn(tempThetaPlusEps) - costFn(tempThetaMinusEps)) / (2 * epsilon)
      tempThetaPlusEps.elements(i) -= epsilon
      tempThetaMinusEps.elements(i) += epsilon
      i += 1
    }
    new MatrixF(gradApprox, 1)
  }

  def gradientApprox(
    @desc("partial funct with just theta as param") costFn: MatrixF => Float,
    @desc("parameters, weights") theta: MatrixF,
    epsilon: Float): MatrixF = {
    val l = theta.elements.length
    var i = 0
    val perturb = MatrixF.zeros(theta.dims())
    val gradApprox = MatrixF.zeros(theta.dims())
    var jMinus, jPlus = 0f
    while (i < l) {
      perturb.elements(i) = epsilon
      jMinus = costFn(theta - perturb)
      jPlus = costFn(theta + perturb)
      gradApprox.elements(i) = (jPlus - jMinus) / (2f * epsilon)
      perturb.elements(i) = 0f
      i += 1
    }
    gradApprox
  }

  import MatrixF._

  def sigmoidGradientSlow(z: MatrixF) = {
    val a = z.elementOp(sigmoidF)
    a ** (1 - a)
  }

  def sigmoidGradient(z: MatrixF) = {
    val res = z.clone
    var i = 0
    var sig = 0f
    while (i < z.elements.length) {
      sig = sigmoidF(res.elements(i))
      res.elements(i) = sig * (1 - sig)
      i += 1
    }
    res
  }

  def sigmoidGradientChunk(el: Array[Float])(range: (Long, Long))() = {
    var i = range._1.asInstanceOf[Int]
    val end = range._2.asInstanceOf[Int]
    var sig = 0f
    while (i <= end) {
      sig = sigmoidF(el(i))
      el(i) = sig * (1 - sig)
      i += 1
    }
    i
  }

  def sigmoidGradientDc(z: MatrixF) = {
    val res = z.clone
    Concurrent.combine(Concurrent.distribute(z.elements.length, sigmoidGradientChunk(res.elements)))
    res
  }

  def costFunction(
    @desc("predictions") a: MatrixF,
    @desc("outputs, targets, actual values") y: MatrixF,
    @desc("regularization factor") lambda: Float = 0)(
      @desc("parameters, weights") thetas: Array[MatrixF]): Float = {
    val m = y.dims._1
    val yb = if (y.isBinaryCategoryMatrix) y else y.toBinaryCategoryMatrix
    var jDel = 0f
    if (lambda != 0) {
      var i = 0
      while (i < thetas.length) {
        val thetaCopy = thetas(i).dropFirst()
        val jdeldel = lambda / (2.0f * m) * Math.sumDc(thetaCopy.elementOp(math.pow(_, 2).asInstanceOf[Float]).elements)
        //println(i + " jdeldel: " + jdeldel)
        jDel += jdeldel
        i += 1
      }
    }
    val jNoReg = costFunctionNoReg(a, yb, m)
    //println("jNoReg " + jNoReg)
    jNoReg + jDel
  }

  def costFunctionNoReg(hThetaT: MatrixF, yT: MatrixF, m: Int): Float = {
    (-1.0f / m) * (yT ** hThetaT.elOp(math.log(_).asInstanceOf[Float]) + (1f - yT) ** ((1f - hThetaT).elOp(math.log(_).asInstanceOf[Float]))).sum()
  }
  /*
   *  tThetaX = theta' * X';
   *  hTheta = sigmoid(tThetaX);	
   *  J = - 1/m * ( y' * log(hTheta') + (1-y') * log(1 - hTheta'));
   */

  def costGradFunction(
    @desc("inputs") x: MatrixF,
    @desc("outputs, targets, actual values") y: MatrixF,
    @desc("parameters, weights") theta: MatrixF,
    @desc("regularization factor") lambda: Float = 0) = {
    val m = y.dims._1
    val tThetaX = theta.tN() * x.tN()
    val hTheta = tThetaX.elOp(sigmoidF)
    val yT = y.tN()
    val j = costFunctionNoReg(hTheta.tN, yT, m)
    val grad = 1 / m * ((hTheta - yT) * x)

    val thetaCopy = theta.clone()
    thetaCopy.elements(0) = 0
    val gradDel = lambda * thetaCopy.tN() / m
    val jDel = lambda / (2f * m) * Math.sumDc(thetaCopy.elementOp(math.pow(_, 2).asInstanceOf[Float]).elements)
    (j + jDel, (grad + gradDel).transposeIp())
  }

  def iterativeDescent(
    @desc("inputs") x: MatrixF,
    @desc("outputs, targets, actual values") y: MatrixF,
    @desc("learning rate") alpha: Float,
    maxIters: Int,
    @desc("regularization factor") lambda: Float = 0f,
    @desc("randomization magnitude") epsilon: Float = .25f,
    @desc("max error") delta: Float = .25f,
    gradCheckCount: Int) = {
    var i = 0
    var deltaCost = -delta
    val m = y.nRows
    var theta = MatrixF.randn(x.nCols, 1, epsilon)
    var j: Float = 0
    var lastJ: Float = 0
    var gradChecks = 0
    var grad: MatrixF = MatrixF.zeros(theta.dims)
    while (i < maxIters && -1 * deltaCost >= delta) {
      val tup = costGradFunction(x, y, theta, lambda)
      j = tup._1
      grad = tup._2
      if (gradChecks < gradCheckCount) {
        //val gradApprox = gradientApprox(costFunctionNoReg(x,y,lambda), theta, epsilon)
        //println("sumsqr diff " + grad.sumSquaredDiffs(gradApprox))
        gradChecks += 1
      }

      theta -= alpha * grad
      if (lastJ != 0) {
        deltaCost = j - lastJ
      }
      lastJ = j
      println("iter: " + i + ", j " + j + " delta: " + deltaCost)
      i += 1
    }
    (j, theta)
  }
}