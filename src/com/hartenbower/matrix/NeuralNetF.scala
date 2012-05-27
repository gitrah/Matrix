package com.hartenbower.matrix

import LogisticRegressionF._
object NeuralNetF {

  def forwardAndBack(x: MatrixF, y: MatrixF, thetas: Array[MatrixF], lambda: Float) = {
    val m = x.nRows
    val (hTheta, zs, as) = predict(thetas, x)
    val yb = if (y.isBinaryCategoryMatrix) y else y.toBinaryCategoryMatrix
    val j = costFunction(hTheta, yb, lambda)(thetas)
    //println("fnb j " + j)
    var i = thetas.length - 1
    val deltas = new Array[MatrixF](thetas.length)
    val bigDeltas = new Array[MatrixF](thetas.length)
    val grads = new Array[MatrixF](thetas.length)
    deltas(thetas.length - 1) = hTheta - yb;
    i -= 1
    while (i > -1) {
      val theta = thetas(i + 1).columnSubset((2 to thetas(i + 1).nCols).toList)
      deltas(i) = (deltas(i + 1) * theta) ** (sigmoidGradient(zs(i)).transposeIp())
      i -= 1
    }
    i = thetas.length - 1
    while (i > -1) {
      bigDeltas(i) = deltas(i).tN() * as(i)
      val theta = MatrixF.zeros(thetas(i).nRows, 1) ++ thetas(i).columnSubset((2 to thetas(i).nCols).toList)
      grads(i) = (bigDeltas(i) + theta * lambda) / m
      i -= 1
    }
    (j, grads)
  }

  def descend(
      inputs: MatrixF, 
      y: MatrixF, 
      thetasOrDims: Array[_],
      maxIters : Int,
      epsilon: Float, 
      regularizationFactor : Float, 
      learningRate : Float,
      maxError : Float) = {
    val layers = thetasOrDims.length
    val yb = if (y.isBinaryCategoryMatrix) y else y.toBinaryCategoryMatrix
    val x = if (inputs.hasBiasCol) inputs else inputs.addBiasCol
    val thetas = new Array[MatrixF](layers)
    var i = 0
    var layerIdx = 0
    var lastTxpsUseCount = MatrixF.txpsUseCount
    var lastTxpsCrCount = MatrixF.txpsCreateCount
    while (layerIdx < layers) {
      thetasOrDims match {
        case specifiedThetas : Array[MatrixF] =>
          thetas(layerIdx) = specifiedThetas(layerIdx)
        case specifiedDims : Array[Tuple2[Int, Int]] =>
          thetas(layerIdx) = MatrixF.randn(specifiedDims(layerIdx), epsilon).addBiasCol()
      }
      layerIdx += 1
    }
    println("thetas " + thetas)
    var deltaCost = -maxError
    var j = 0f
    var lastJ = 0f
    while (i < maxIters && -1*deltaCost >= maxError) {
      val tup = forwardAndBack(x,yb,thetas,regularizationFactor)
      j = tup._1
      val grads = tup._2
      layerIdx = 0
	  while (layerIdx < layers) {
	    thetas(layerIdx) = thetas(layerIdx) - grads(layerIdx) * learningRate
	    layerIdx += 1
	  }
      if(lastJ != 0) {
        deltaCost = j - lastJ
      }
      lastJ = j
      
      println("iter: " + i + ", j " + j + " delta: " + deltaCost
          + "; txCreateDelta " + (MatrixF.txpsCreateCount -lastTxpsCrCount)
         + "; txUseDelta " + (MatrixF.txpsUseCount -lastTxpsUseCount)
          )
      lastTxpsCrCount = MatrixF.txpsCreateCount
      lastTxpsUseCount = MatrixF.txpsUseCount
      i += 1
    }
    (j, thetas)
  }

  def predict(weights: Array[MatrixF], inputs: MatrixF): (MatrixF, Array[MatrixF], Array[MatrixF]) = {
    val m = inputs.nRows
    var x = if (inputs.hasBiasCol) inputs else inputs.addBiasCol
    var idx = 0
    var lastA: MatrixF = x
    val zs = new Array[MatrixF](weights.length)
    val as = new Array[MatrixF](weights.length + 1)
    as(0) = lastA
    while (idx < weights.length) {
      zs(idx) = weights(idx) * lastA.tN
      lastA = zs(idx).elementOp(sigmoidF).tN
      if (idx < weights.length - 1)
        lastA = lastA.addBiasCol
      idx += 1
      as(idx) = lastA
    }
    (lastA, zs, as)
  }
}