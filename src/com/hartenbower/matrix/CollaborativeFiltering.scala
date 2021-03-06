package com.hartenbower.matrix
import MatrixD._
object CollaborativeFiltering {
  /**
   * @param x : inputs (num samples x num features)
   * @param theta : params (num users x num features)
   * @param y	: outputs (num samples x num users)
   * @param r : activations (num samples x num users)
   * @param lambda : regularization
   * @return
   */
  def cost(x: MatrixD, theta: MatrixD, y: MatrixD, r: MatrixD, lambda: Double): Double = {
    .5 * ((((x * theta.tN - y) ** r) ^ 2).sum + (x.autoDot() + theta.autoDot()) * lambda)
  }
  def grad(x: MatrixD, theta: MatrixD, y: MatrixD, r: MatrixD, lambda: Double): (MatrixD, MatrixD) = {
    val x_grad = (x * theta.tN - y) ** r * theta + lambda * x
    val theta_grad = ((theta * x.tN - y.tN) ** r.tN) * x + lambda * theta
    (x_grad, theta_grad)
  }
  def thetaGradBatch(x: MatrixD, theta: MatrixD, y: MatrixD, r: MatrixD, alpha: Double, rowBatch: Array[Int]): MatrixD = {
    val l = rowBatch.length
    var i = 0
    var dTheta = MatrixD.zeros(theta.dims())
    var xi: MatrixD = null
    var yi: MatrixD = null
    while (i < l) {
      xi = x.rowVector(i)
      yi = y.rowVector(i)
      dTheta = dTheta + (theta.tN * xi - yi) * xi
    }
    dTheta = dTheta * (-alpha / l)
    theta + dTheta
  }

  def checkCost(lambda: Double) {
    val x_t = MatrixD.randn(4, 3)
    val theta_t = MatrixD.randn(5, 3)
    val y = (x_t * theta_t.tN()).filterElements(x => if (x > .5d) 0 else x)
    val r = y.filterElements(x => if (x != 0) 1d else 0)
    // TODO
  }
}
