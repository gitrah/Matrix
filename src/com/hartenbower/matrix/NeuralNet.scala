package com.hartenbower.matrix

import LogisticRegression._
import MatrixD._

object NeuralNet {

  def nnCostFunctionSanGradient(nn_params: MatrixD, input_layer_size: Int,
    hidden_layer_size: Int,
    num_labels: Int,
    _x: MatrixD, y: MatrixD, lambda: Double): Double =
    nnCostFunction(nn_params, input_layer_size, hidden_layer_size, num_labels, _x, y, lambda)._1

  def nnCostFunction(nn_params: MatrixD, input_layer_size: Int,
    hidden_layer_size: Int,
    num_labels: Int,
    _x: MatrixD, y: MatrixD, lambda: Double): (Double, MatrixD) = {
    val theta1 = nn_params.reshape(hidden_layer_size, input_layer_size + 1)
    val theta2 = nn_params.reshape(num_labels, hidden_layer_size + 1, (hidden_layer_size * (input_layer_size + 1)))
    val m = _x.nRows
    var x = if (!_x.hasBiasCol) _x.addBiasCol else _x
    val yb = y.toBinaryCategoryMatrix
    val z2 = (theta1 * x.tN()).tN()
    val a2 = MatrixD.ones(m, 1) ++ z2.elementOpDc(sigmoid)
    val z3 = (theta2 * a2.tN()).tN()
    val a3 = z3.elementOpDc(sigmoid)

    // 
    var j = (-1d / m * (yb ** log(a3) + (1d - yb) ** (log(1d - a3)))).sumDc()
    //println("j initial " + j)
    //temp = Theta2(:,2:end); % skip bias
    var tempTheta2 = theta2.dropFirst
    //jreg3 =sum(sum((lambda / (2 * m) * temp.^2)));
    val jreg3 = ((tempTheta2 ^ 2d) * (lambda / (2d * m))).sumDc()
    //println("jreg3 " + jreg3)
    j += jreg3
    //temp = Theta1(:,2:end); % skip bias
    val tempTheta1 = theta1.dropFirst()
    //jreg2 =sum(sum((lambda / (2 * m) * temp.^2))); 
    val jreg2 = ((tempTheta1 ^ 2d) * (lambda / (2d * m))).sumDc()
    //println("jreg2 " + jreg2)
    j += jreg2

    val delta_3 = a3 - yb
    //% Theta2_grad=  1/m* ((a3 - y') * X);
    val delta_2 = (delta_3 * tempTheta2) ** sigmoidGradient(z2)
    val bigDelta2 = delta_3.tN() * a2
    val bigDelta1 = delta_2.tN() * x

    //temp = [zeros(size(Theta2,1),1) Theta2(:,2:end)];
    var temp = MatrixD.zeros(theta2.nRows, 1) ++ tempTheta2
    val theta2_grad = (bigDelta2 + (temp * lambda)) / m
    // temp = [zeros(size(Theta1,1),1) Theta1(:,2:end)];
    temp = MatrixD.zeros(theta1.nRows, 1) ++ tempTheta1
    val theta1_grad = 1d / m * (bigDelta1 + lambda * temp)

    val grad = theta1_grad.poseAsCol +/ theta2_grad.poseAsCol
    theta1.unPose()
    theta2.unPose()
    (j, grad)
  }

  def checkNnGradients(lambda: Double = 0d): Double = {
    val input_layer_size = 3
    val hidden_layer_size = 5
    val num_labels = 3
    val m = 5

    // We generate some 'random' test data
    val theta1 = sin(hidden_layer_size, input_layer_size+1)
    val theta2 = sin(num_labels, hidden_layer_size+1)
    val nn_params = theta1.poseAsCol +/ theta2.poseAsCol
    theta1.unPose()
    theta2.unPose()
    // Reusing debugInitializeWeights to generate X
    val x = sin(m, input_layer_size )
    val y = (new MatrixD(Util.Math.toDouble((1 to m).toArray), 1).elementOp(_ % num_labels) + 1)

    val epsilon = 1e-4
    val tup = nnCostFunction(nn_params, input_layer_size, hidden_layer_size, num_labels, x, y, lambda)
    val numgrad = gradientApprox(nnCostFunctionSanGradient(_, input_layer_size, hidden_layer_size, num_labels, x, y, lambda), nn_params, epsilon)
    println("grad\n" + tup._2.octStr())
    println("numgrad\n" + numgrad.octStr())
    (numgrad - tup._2).length / (numgrad + tup._2).length
  }

  def forwardAndBack(x: MatrixD, y: MatrixD, thetas: Array[MatrixD], lambda: Double) = {
    val m = x.nRows
    val (hTheta, zs, as) = predict(thetas, x)
    val yb = if (y.isBinaryCategoryMatrix) y else y.toBinaryCategoryMatrix
    val j = costFunction(hTheta, yb, lambda)(thetas)
    //println("fnb j " + j)
    var i = thetas.length - 1
    val deltas = new Array[MatrixD](thetas.length)
    val bigDeltas = new Array[MatrixD](thetas.length)
    val grads = new Array[MatrixD](thetas.length)
    deltas(thetas.length - 1) = hTheta - yb;
    i -= 1
    while (i > -1) {
      val theta = thetas(i + 1).columnSubset((2 to thetas(i + 1).nCols).toArray)
      deltas(i) = (deltas(i + 1) * theta) ** (sigmoidGradient(zs(i)).transposeIp())
      i -= 1
    }
    i = thetas.length - 1
    while (i > -1) {
      bigDeltas(i) = deltas(i).tN() * as(i)
      val theta = MatrixD.zeros(thetas(i).nRows, 1) ++ thetas(i).columnSubset((2 to thetas(i).nCols).toArray)
      grads(i) = (bigDeltas(i) + theta * lambda) / m
      i -= 1
    }
    (j, grads)
  }

  def descend(
    inputs: MatrixD,
    y: MatrixD,
    thetasOrDims: Array[_],
    maxIters: Int,
    epsilon: Double,
    regularizationFactor: Double,
    learningRate: Double,
    maxError: Double) = {
    val layers = thetasOrDims.length
    val yb = if (y.isBinaryCategoryMatrix) y else y.toBinaryCategoryMatrix
    val x = if (inputs.hasBiasCol) inputs else inputs.addBiasCol
    val thetas = new Array[MatrixD](layers)
    var i = 0
    var layerIdx = 0
    var lastTxpsUseCount = MatrixD.txpsUseCount
    var lastTxpsCrCount = MatrixD.txpsCreateCount
    while (layerIdx < layers) {
      thetasOrDims match {
        case specifiedThetas: Array[MatrixD] =>
          thetas(layerIdx) = specifiedThetas(layerIdx)
        case specifiedDims: Array[Tuple2[Int, Int]] =>
          thetas(layerIdx) = MatrixD.randn(specifiedDims(layerIdx), epsilon).addBiasCol()
      }
      layerIdx += 1
    }
    println("thetas " + thetas)
    var deltaCost = -maxError
    var j = 0d
    var lastJ = 0d
    while (i < maxIters && -1 * deltaCost >= maxError) {
      val tup = forwardAndBack(x, yb, thetas, regularizationFactor)
      j = tup._1
      val grads = tup._2
      layerIdx = 0
      while (layerIdx < layers) {
        thetas(layerIdx) = thetas(layerIdx) - grads(layerIdx) * learningRate
        layerIdx += 1
      }
      if (lastJ != 0) {
        deltaCost = j - lastJ
      }
      lastJ = j

      println("iter: " + i + ", j " + j + " delta: " + deltaCost
        + "; txCreateDelta " + (MatrixD.txpsCreateCount - lastTxpsCrCount)
        + "; txUseDelta " + (MatrixD.txpsUseCount - lastTxpsUseCount))
      lastTxpsCrCount = MatrixD.txpsCreateCount
      lastTxpsUseCount = MatrixD.txpsUseCount
      i += 1
    }
    (j, thetas)
  }

  def descendAdaptive(
    inputs: MatrixD,
    y: MatrixD,
    thetasOrDims: Array[_],
    maxIters: Int,
    epsilon: Double,
    regularizationFactor: Double,
    learningRate: Double,
    maxError: Double) = {
    val layers = thetasOrDims.length
    val depth = 3
    val yb = if (y.isBinaryCategoryMatrix) y else y.toBinaryCategoryMatrix
    val x = if (inputs.hasBiasCol) inputs else inputs.addBiasCol
    var thetas = new Array[MatrixD](layers)
    var i = 0
    var layerIdx = 0
    while (layerIdx < layers) {
      thetasOrDims match {
        case specifiedThetas: Array[MatrixD] =>
          thetas(layerIdx) = specifiedThetas(layerIdx)
        case specifiedDims: Array[Tuple2[Int, Int]] =>
          thetas(layerIdx) = MatrixD.randn(specifiedDims(layerIdx), epsilon).addBiasCol()
      }
      layerIdx += 1
    }
    println("thetas " + thetas)
    var deltaCost = -maxError
    var lastJ = 0d
    var lastI = 0
    var alpha = learningRate
    val currThetas: Array[MatrixD] = copy(thetas)
    //var currThetas: Array[MatrixD] = thetas
    var fNbTup = forwardAndBack(x, yb, thetas, regularizationFactor)
    var j = fNbTup._1
    val grads: Array[MatrixD] = fNbTup._2
    val gradHistory = new java.util.Stack[Array[MatrixD]]();
    gradHistory.push(copy(grads));
    val thetaHistory = new java.util.Stack[Array[MatrixD]]();
    thetaHistory.push(copy(thetas));
    var modAlphaCounter = 0
    while (i < maxIters && -1 * deltaCost >= maxError) {
      lastI = i
      modAlphaCounter = 0
      do {
        val ths = thetas(0).sum
        println("\nthetas(0).sum " + ths)
        println("lastThetas(0).sum " + thetaHistory.peek()(0).sum)
        println("grads(0).sum " + grads(0).sum)
        // update thetas with grads
        layerIdx = 0
        while (layerIdx < layers) {
          currThetas(layerIdx) = thetas(layerIdx) - grads(layerIdx) * alpha
          layerIdx += 1
        }
        // calc cost and new grads
        fNbTup = forwardAndBack(x, yb, currThetas, regularizationFactor)
        j = fNbTup._1
        if (lastJ != 0) {
          deltaCost = j - lastJ
        }
        modAlphaCounter += 1
        println("iter: " + i + ", modCtr: " + modAlphaCounter + ", j " + j + " delta: " + deltaCost + " alpha: " + alpha)
        if (deltaCost > 0) {
          deltaCost = -maxError
          if (modAlphaCounter < 5) {
            alpha = alpha / 2d
            Array.copy(gradHistory.peek(), 0, grads, 0, layers)
            Array.copy(thetaHistory.peek(), 0, thetas, 0, layers)
          } else {
            Array.copy(gradHistory.pop(), 0, grads, 0, layers)
            Array.copy(thetaHistory.pop(), 0, thetas, 0, layers)
          }
        } else {
          thetaHistory.push(copy(currThetas))
          if (thetaHistory.size() > depth) {
            thetaHistory.remove(thetaHistory.firstElement())
          }
          gradHistory.push(copy(grads))
          if (gradHistory.size() > depth) {
            gradHistory.remove(gradHistory.firstElement())
          }
          Array.copy(fNbTup._2, 0, grads, 0, layers)
          alpha += alpha / 10d
          lastJ = j
          i += 1
        }

      } while (lastI == i)
    }
    (j, thetas)
  }
  def predictCg(theta1: MatrixD, theta2: MatrixD, x: MatrixD) = {
    val m = x.nRows
    val num_labels = theta2.nRows

    // You need to return the following variables correctly 
    val p = MatrixD.zeros(m, 1)
    //h1 = sigmoid([ones(m, 1) X] * Theta1');
    val h1 = (x.addBiasCol * theta1.tN()).elementOpDc(sigmoidD)
    //h2 = sigmoid([ones(m, 1) h1] * Theta2');
    val h2 = (h1.addBiasCol() * theta2.tN).elementOpDc(sigmoidD)
    h2

  }

  def predict(weights: Array[MatrixD], inputs: MatrixD): (MatrixD, Array[MatrixD], Array[MatrixD]) = {
    val m = inputs.nRows
    var x = if (inputs.hasBiasCol) inputs else inputs.addBiasCol
    var idx = 0
    var lastA: MatrixD = x
    val zs = new Array[MatrixD](weights.length)
    val as = new Array[MatrixD](weights.length + 1)
    as(0) = lastA
    while (idx < weights.length) {
      zs(idx) = weights(idx) * lastA.tN
      lastA = zs(idx).elementOp(sigmoidD).tN
      if (idx < weights.length - 1)
        lastA = lastA.addBiasCol
      idx += 1
      as(idx) = lastA
    }
    (lastA, zs, as)
  }
}