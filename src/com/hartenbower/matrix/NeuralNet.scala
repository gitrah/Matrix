package com.hartenbower.matrix

import LogisticRegression._
object NeuralNet {
  
  def forwardAndback(x : MatrixD, y: MatrixD, thetas : Array[MatrixD], lambda : Double) = {
    val m = x.nRows
    val (hTheta, zs, as) = predict(thetas,x)
    val yb = if(y.isBinaryCategoryMatrix) y else y.toBinaryCategoryMatrix
    val j = costFunction(hTheta, yb, lambda)(thetas)
    println("j " + j)
    var i = thetas.length -1
    val deltas = new Array[MatrixD](thetas.length)
    val bigDeltas = new Array[MatrixD](thetas.length)
    val grads = new Array[MatrixD](thetas.length)
    deltas(thetas.length - 1) = hTheta - yb;
    i -= 1
    while (i > -1) {
      val theta = thetas(i+1 ).columnSubset( (2 to thetas(i + 1).nCols).toList)
      deltas(i) = (deltas(i+1) * theta) ** (sigmoidGradient(zs(i )).transposeIp())
      i -= 1
    }
    i = thetas.length -1
    while (i > -1) {
      bigDeltas(i) = deltas(i).tN() * as(i)
      val theta = MatrixD.zeros(thetas(i).nRows,1) ++ thetas(i ).columnSubset( (2 to thetas(i ).nCols).toList)
      grads(i) = (bigDeltas(i) +  theta * lambda)/m
      i -= 1
    }
    grads
  }

  def predict(weights : Array[MatrixD], inputs : MatrixD) : (MatrixD,Array[MatrixD], Array[MatrixD]) = {
    val m = inputs.nRows
    var x = if (inputs.hasBiasCol) inputs else inputs.addBiasCol 
    var idx = 0
    var lastA : MatrixD = x 
    val zs = new Array[MatrixD](weights.length)
    val as = new Array[MatrixD](weights.length + 1)
    as(0) = lastA
    while(idx < weights.length) {
      zs(idx) = weights(idx) * lastA.tN
      lastA = zs(idx).elementOp(sigmoidD).tN
      if(idx < weights.length -1) 
        	lastA = lastA.addBiasCol 
      idx += 1
      as(idx)= lastA
    }
    (lastA,zs,as)
  }
}