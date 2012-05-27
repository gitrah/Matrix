package com.hartenbower.matrix

import Util._


object LogisticRegressionF {

  @inline def sigmoidF(z: Float) = 1.f / (1.f + math.exp(-z).asInstanceOf[Float])

  def from_ex3data1_txt() =  {
    val m = Io.parseOctaveDataFile("ex3data1.txt",false)
    val x: MatrixF = m.get("X").get.asInstanceOf[MatrixF]
    val y: MatrixF = m.get("y").get.asInstanceOf[MatrixF]
    val num_labels: Int =m.get("num_labels").get.asInstanceOf[Float].asInstanceOf[Int]
    val input_layer_size: Int = m.get("input_layer_size").get.asInstanceOf[Float].asInstanceOf[Int]
    (x,y,num_labels,input_layer_size)
  }
  
  
  def gradientApprox(
      @desc("partial funct with just theta as param") 
      	costFn: MatrixF => Float, 
		@desc("parameters, weights") 
      theta : MatrixF, 
      epsilon : Float) : MatrixF= {
    val n = math.max(theta.nRows,theta.nCols)
    var i = 0
    val tempThetaPlusEps = theta.clone
    val tempThetaMinusEps = theta.clone
    val gradApprox = new Array[Float](n)
    while(i < n) {
    	tempThetaPlusEps.elements(i) += epsilon
    	tempThetaMinusEps.elements(i) -= epsilon
    	gradApprox(i) = (costFn(tempThetaPlusEps) - costFn(tempThetaMinusEps) )/ (2*epsilon)
    	tempThetaPlusEps.elements(i) -= epsilon
    	tempThetaMinusEps.elements(i) += epsilon
    	i+= 1
    }
    new MatrixF(gradApprox,1)
  }
  
  import MatrixF._
  
  def sigmoidGradientSlow( z : MatrixF ) = {
    val a = z.elementOp(sigmoidF)
    a ** (1 - a)
  }
  
  def sigmoidGradient(z : MatrixF ) = {
    val res  = z.clone
    var i = 0
    var sig = 0d
    while(i < z.elements.length) {
      sig = sigmoidF(res.elements(i))
      res.elements(i) = (sig * (1 - sig)).asInstanceOf[Float]
      i+=1
    }
    res
  }
 
  def costFunction(
		@desc("predictions") 
			a : MatrixF, 
		@desc("outputs, targets, actual values") 
			y: MatrixF, 
		@desc("regularization factor")
	    	lambda : Float = 0	
     )(	
        @desc("parameters, weights") 
     		thetas : Array[MatrixF]
    )	 : Float = {
    val m = y.dims._1
    val yb = if(y.isBinaryCategoryMatrix) y else y.toBinaryCategoryMatrix
    var jDel = 0f
    if(lambda != 0) {
        var i = 0
        while(i < thetas.length) {
		    val thetaCopy = thetas(i).columnSubset( (2 to thetas(i).nCols).toList)
		    val jdeldel =   (lambda/(2.f*m) * Math.sumF(thetaCopy.elementOp(Math.powF(_,2)).elements))
		    //println(i + " jdeldel: " + jdeldel)
		    jDel += jdeldel
		    i += 1
        }
    }
    val jNoReg = costFunctionNoReg(a,yb,m) 
    //println("jNoReg " + jNoReg)
    jNoReg + jDel
  }
  
  def costFunctionNoReg(hThetaT : MatrixF, yT: MatrixF, m : Int) : Float = {
    ((-1.f/m) * (yT ** hThetaT.elOp(Math.logF) + (1 - yT)**( (1 - hThetaT).elOp(Math.logF))).sum()).asInstanceOf[Float]
  }
  /*
   *  tThetaX = theta' * X';
   *  hTheta = sigmoid(tThetaX);	
   *  J = - 1/m * ( y' * log(hTheta') + (1-y') * log(1 - hTheta'));
   */

  def costGradFunction(
		@desc("inputs") 
			x : MatrixF, 
		@desc("outputs, targets, actual values") 
			y: MatrixF, 
		@desc("parameters, weights") 
			theta : MatrixF, 
	    @desc("regularization factor")
	    	lambda : Float = 0) = {
    val m = y.dims._1
    val tThetaX = theta.tN() * x.tN()
    val hTheta = tThetaX.elOp(sigmoidF)
    val yT = y.tN()
    val j = costFunctionNoReg(hTheta.tN, yT, m)
    val grad = 1/m * ((hTheta - yT) * x)
    
    val thetaCopy = theta.clone()
    thetaCopy.elements(0)= 0
    val gradDel = lambda * thetaCopy.tN()/m
    val jDel = (lambda/(2*m) * Math.sumF(thetaCopy.elementOp(Math.powF(_,2)).elements)).asInstanceOf[Float]
    (j +jDel,(grad + gradDel).transposeIp())
  }

  def iterativeDescent(
		@desc("inputs") 
			x : MatrixF, 
		@desc("outputs, targets, actual values") 
			y: MatrixF, 
	    @desc("learning rate")
	    	alpha : Float,    
	    maxIters: Int,
	    @desc("regularization factor")
	    	lambda : Float = 0,
	    @desc("randomization magnitude")
	    	epsilon : Float = .25f, 
	    @desc("max error")
	    	delta : Float = .25f,
	    gradCheckCount : Int) = {
    var i = 0
    var deltaCost = -delta
    val m = y.nRows
    var theta = MatrixF.randn(x.nCols,1,epsilon)
    var j : Float = 0
    var lastJ : Float = 0
    var gradChecks = 0
    var grad : MatrixF = MatrixF.zeros(theta.dims)
    while (i < maxIters && -1*deltaCost >= delta) {
      val tup = costGradFunction(x, y, theta, lambda)
      j = tup._1
      grad = tup._2
      if(gradChecks < gradCheckCount) {
      	//val gradApprox = gradientApprox(costFunctionNoReg(x,y,lambda), theta, epsilon)
    	  //println("sumsqr diff " + grad.sumSquaredDiffs(gradApprox))
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