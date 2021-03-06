package com.hartenbower.matrix
import scala.io.Source
import Util._
import MatrixD._
object GradientDescent {
  /**
   * linear regresson model
   * hypothesis hθ(x) = θ0 + θ1(x)
   * with squared error cost function J
   *             1   m      (i)  (i) 2
   * J(θ0,θ1) = ---  Σ  (hθ(x) - y  )
   *             2m  i=1
   */
  def Σ(f: ((Double, Double)) => Double, data: Array[(Double, Double)]): Double = {
    (0.0 /: data.map(f))(_ + _)
  }

  def hypothesis(θ0: Double, θ1: Double, x: Double) = θ0 + θ1 * x

  def θ0θ1(θ0: Double, θ1: Double, α: Double, data: Array[(Double, Double)]): (Double, Double) = {
    val m = data.length
    (θ0 - α / m * Σ((x) => θ0 + θ1 * x._1 - x._2, data),
      θ1 - α / m * Σ((x) => (θ0 + θ1 * x._1 - x._2) * x._1, data))
  }

  def diffSquared(a: Tuple2[Double, Double], b: Tuple2[Double, Double]): Double = {
    (b._1 - a._1) * (b._1 - a._1) + (b._2 - a._2) * (b._2 - a._2)
  }

  def iterate(α: Double, data: Array[(Double, Double)], σ: Double): (Double, Double) = {
    var θ = (0.0, 0.0)
    var oldθ = θ
    var ctr = 0
    do {
      oldθ = θ
      ctr += 1
      θ = θ0θ1(θ._1, θ._2, α, data)
      // println("iter " + ctr + " θ " + θ + " diff " + diffSquared(θ, oldθ))
    } while (diffSquared(θ, oldθ) > σ * σ)
    θ
  }

  implicit def arrayToTuple(args: Array[Double]): Product = {
    args match {
      case Array(x) => Tuple1[Double](x)
      case Array(x, y) => Tuple2[Double, Double](x, y)
      case Array(x, y, z) => Tuple3[Double, Double, Double](x, y, z)
      case Array(w, x, y, z) => Tuple4[Double, Double, Double, Double](w, x, y, z)
      case Array(v, w, x, y, z) => Tuple5[Double, Double, Double, Double, Double](v, w, x, y, z)
      case Array(u, v, w, x, y, z) => Tuple6[Double, Double, Double, Double, Double, Double](u, v, w, x, y, z)
    }
  }

  def normalEquation(features: Array[Double], nCols: Int, values: Array[Double]): MatrixD = {
    require(features.length / nCols == values.length, "counts of feature examples and value examples must agree")
    /*       T -1  T
     * θ = (X X)  X y
     */
    val designMatrix = new MatrixD(features, nCols).prependColumnNew(
      Array.fill[Double](values.length)(1.0))
    val dmTxp = designMatrix.transposeN()
    // without determinant check, just return
    //     (xTxp * x).inverse * xTxp * Matrix.columnMatrix(values)
    val prod = dmTxp * designMatrix
    require(prod.determinant != 0.0, "features matrix is singular")
    prod.inverse() * dmTxp * MatrixD.columnMatrix(values)
  }

  def hLinReg(theta: MatrixD, x: MatrixD) = theta.tN * x

  def cost(h: (MatrixD, MatrixD) => MatrixD)(x: MatrixD, theta: MatrixD, y: MatrixD): Double = {
    (0.5 * (h(x, theta) - y) ^ 2).sum
  }

  type HypothFn = (MatrixD, MatrixD) => MatrixD
  type CostFn = (HypothFn, MatrixD, MatrixD, MatrixD) => Double

  // same as stochastic gradient descent if batchSize == 1
  // store cost per batch and average over every x* 1000th sample to verify convergence.
  def batchDescent(h: HypothFn, c: CostFn, x: MatrixD, y: MatrixD, theta: MatrixD, alpha: Double, epsilon: Double, batchSize: Int = 10) {
    val (m, n) = x.dims()
    var j = 0d
    var lastJ = Double.NaN
    var i = 0
    var xiter: MatrixD = null
    var yiter: MatrixD = null
    var newTheta: MatrixD = theta.clone()
    do {
      val rows = Math.randpermBatch(m, batchSize)
      i = 0
      while (i < rows.length) {
        xiter = x.rowSubset(rows(i))
        yiter = y.rowSubset(rows(i))
        newTheta = iterativeThetaGrad(theta, xiter, h, yiter, alpha)
      }

    } while (lastJ.isNaN() || (j - lastJ) > epsilon)
  }

  def iterativeThetaGrad(theta: MatrixD, xiter: MatrixD, hypThetaX: HypothFn, yiter: MatrixD, alpha: Double): MatrixD = {
    // theta = theta - alpha (cost(xi) - yi) * xi
    theta - ((hypThetaX(theta, xiter) - yiter) * xiter) * (alpha)
  }
}



/*

val features = Array(
    2104, 5, 1, 45.0,
    1416, 3, 2, 40,
    1534, 3, 2, 30,
    852, 2, 1, 36 )
val values = Array(460.0, 232, 315, 178)

val data : Array[(Double,Double)]= Array( 
          (800.0,180.0),
          (1000,170),
          (1100,250),
          (1250,200),
          (1250,230),
          (1200,245),
          (1300,260),
          (1250,300),
          (1300,300),
          (1400,205),
          (1450,200),
          (1450,230),
          (1470,250),
          (1500,250),
          (1505,305),
          (1500,460),
          (1600,260),
          (1600,330),
          (1700,370),
          (1760,250),
          (1800,270),
          (1800,350),
          (1850,250),
          (1850,330),
          (1950,240),
          (2000,250),
          (2005,300),
          (2010,340))
     
val data : List[(Double,Double)]= List( 
          (5,5),
          (6,6),
          (7,6.5),
          (8,6.9),
          (8,7.0),
          (9,8),
          (9.5, 8.2),
          (10, 8.5))
          

GradientDescent.iterate(.000000001, data, .00000001)

time("iter", 100000,GradientDescent.iterate(.000000001, data, .00000001))


*/
