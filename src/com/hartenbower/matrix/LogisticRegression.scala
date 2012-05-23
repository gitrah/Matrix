package com.hartenbower.matrix

import Util._


object LogisticRegression {
  def sigmoid[@specialized(Double, Float, Int) N](z: N)(implicit numeric: Numeric[N]): N = {
    val g: Double = 1. / (1. + math.exp(-numeric.toDouble(z)))
    numeric match {
      case i: Integral[N] =>
        (math.round(g)).asInstanceOf[N]
      case fr: Fractional[N] =>
        g.asInstanceOf[N]
    }
  }

  def sigmoidNs[N](z: N)(implicit numeric: Numeric[N]): N = {
    val g: Double = 1. / (1. + math.exp(-numeric.toDouble(z)))
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

  def sigmoidF(z: Float) = 1.f / (1.f + math.exp(-z).asInstanceOf[Float])
  def sigmoidD(z: Double) = 1. / (1. + math.exp(-z))

  def logF(z: Float) = math.log(z).asInstanceOf[Float]



  def from_ex3data1_txt() =  {
    val m = parseOctaveDataFile("ex3data1.txt")
    val x: MatrixD = m.get("X").get.asInstanceOf[MatrixD]
    val y: MatrixD = m.get("y").get.asInstanceOf[MatrixD]
    val num_labels: Int =m.get("num_labels").get.asInstanceOf[Double].asInstanceOf[Int]
    val input_layer_size: Int = m.get("input_layer_size").get.asInstanceOf[Double].asInstanceOf[Int]
    (x,y,num_labels,input_layer_size)
  }
  
  
  def gradientApprox(
      @desc("partial funct with just theta as param") 
      	costFn: MatrixD => Double, 
		@desc("parameters, weights") 
      theta : MatrixD, 
      epsilon : Double) : MatrixD= {
    val n = math.max(theta.nRows,theta.nCols)
    var i = 0
    val tempThetaPlusEps = theta.clone
    val tempThetaMinusEps = theta.clone
    val gradApprox = new Array[Double](n)
    while(i < n) {
    	tempThetaPlusEps.elements(i) += epsilon
    	tempThetaMinusEps.elements(i) -= epsilon
    	gradApprox(i) = (costFn(tempThetaPlusEps) - costFn(tempThetaMinusEps) )/ (2*epsilon)
    	tempThetaPlusEps.elements(i) -= epsilon
    	tempThetaMinusEps.elements(i) += epsilon
    	i+= 1
    }
    new MatrixD(gradApprox,1)
  }
  
  import MatrixD._
  
  def sigmoidGradient( z : MatrixD ) = {
    val a = z.elementOp(sigmoidD)
    a ** (1 - a)
  }
  
 
  def costFunction(
		@desc("predictions") 
			a : MatrixD, 
		@desc("outputs, targets, actual values") 
			y: MatrixD, 
		@desc("regularization factor")
	    	lambda : Double = 0	
     )(	
        @desc("parameters, weights") 
     		thetas : Array[MatrixD]
    )	 : Double = {
    val m = y.dims._1
    val yb = if(y.isBinaryCategoryMatrix) y else y.toBinaryCategoryMatrix
    var jDel = 0d
    if(lambda != 0) {
        var i = 0
        while(i < thetas.length) {
		    val thetaCopy = thetas(i).columnSubset( (2 to thetas(i).nCols).toList)
		    val jdeldel = lambda/(2.*m) * Util.sum(thetaCopy.elementOp(math.pow(_,2)).elements)
		    println(i + " jdeldel: " + jdeldel)
		    jDel += jdeldel
		    i += 1
        }
    }
    val jNoReg = costFunctionNoReg(a,yb,m) 
    println("jNoReg " + jNoReg)
    jNoReg + jDel
  }
  
  def costFunctionNoReg(hThetaT : MatrixD, yT: MatrixD, m : Int) : Double = {
    (-1./m) * (yT ** hThetaT.elOp(math.log) + (1 - yT)**( (1 - hThetaT).elOp(math.log))).sum()
  }
  /*
   *  tThetaX = theta' * X';
   *  hTheta = sigmoid(tThetaX);	
   *  J = - 1/m * ( y' * log(hTheta') + (1-y') * log(1 - hTheta'));
   */

  def costGradFunction(
		@desc("inputs") 
			x : MatrixD, 
		@desc("outputs, targets, actual values") 
			y: MatrixD, 
		@desc("parameters, weights") 
			theta : MatrixD, 
	    @desc("regularization factor")
	    	lambda : Double = 0) = {
    val m = y.dims._1
    val tThetaX = theta.tN() * x.tN()
    val hTheta = tThetaX.elOp(sigmoidD)
    val yT = y.tN()
    val j = costFunctionNoReg(hTheta.tN, yT, m)
      //(-1./m) * (y.tN * hTheta.tN.elOp(math.log) + (1 - y.tN)*( (1 - hTheta.tN).elOp(math.log)))
    val grad = 1/m * ((hTheta - yT) * x)
    
    val thetaCopy = theta.clone()
    thetaCopy.elements(0)= 0
    val gradDel = lambda * thetaCopy.tN()/m
    val jDel = lambda/(2*m) * sum(thetaCopy.elementOp(math.pow(_,2)).elements)
    (j +jDel,(grad + gradDel).transposeIp())
  }

  def iterativeDescent(
		@desc("inputs") 
			x : MatrixD, 
		@desc("outputs, targets, actual values") 
			y: MatrixD, 
	    @desc("learning rate")
	    	alpha : Double,    
	    maxIters: Int,
	    @desc("regularization factor")
	    	lambda : Double = 0,
	    @desc("randomization magnitude")
	    	epsilon : Double = .25, 
	    @desc("max error")
	    	delta : Double = .25,
	    gradCheckCount : Int) = {
    var i = 0
    var deltaCost = -delta
    val m = y.nRows
    var theta = MatrixD.randn(x.nCols,1,epsilon)
    var j : Double = 0
    var lastJ : Double = 0
    var gradChecks = 0
    var grad : MatrixD = MatrixD.zeros(theta.dims)
    while (i < maxIters && -1*deltaCost >= delta) {
      val tup = costGradFunction(x, y, theta, lambda)
      j = tup._1
      grad = tup._2
      if(gradChecks < gradCheckCount) {
    	 // val gradApprox = gradientApprox(costFunctionNoReg(x,y,lambda), theta, epsilon)
    	 // println("sumsqr diff " + grad.sumSquaredDiffs(gradApprox))
    	  gradChecks += 1
      }
      
      theta -= alpha * grad
      if(lastJ != 0) {
        deltaCost = j - lastJ
      }
      lastJ = j
      println("iter: " + i + ", j " + j + " delta: " + deltaCost)
      i += 1
    }
    (j, theta)
  }
}