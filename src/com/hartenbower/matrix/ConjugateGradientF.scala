package com.hartenbower.matrix

import MatrixF._
import LogisticRegression._

/**
 * Port of fmincg.m (Octave function)
 *
 */
object ConjugateGradientF {
  def isReal(d: Float) = (!d.isNaN() && !d.isInfinite())
  val rho = 0.01f
  val sig = .5f
  val int0 = .1f
  val ext = 3.0f
  val max = 20
  val ratio = 100
  def fmincg(f: (MatrixF) => (Float, MatrixF), xin: MatrixF, length: Int = 50, red: Int = 1): (MatrixF, MatrixF, Int) = {
    var x: MatrixF = xin.clone()
    var a = 0f
    var b = 0f
    var x0: MatrixF = null
    var df0: MatrixF = null
    var df1: MatrixF = null
    var df2: MatrixF = null
    var f0: Float = 0f
    var f1: Float = 0f
    var f2: Float = 0f
    var f3: Float = 0f
    var s: MatrixF = null
    var d1: Float = 0f
    var d2: Float = 0f
    var d3: Float = 0f
    var z1: Float = 0f
    var z2: Float = 0f
    var z3: Float = 0f
    var tup: (Float, MatrixF) = null
    var success = false
    var outerLoop = true
    var limit = 0f
    var i = 0
    var ls_failed = false
    //var fX = new Array[Float](0)
    var fX: MatrixF = null
    tup = f(x); f1 = tup._1; df1 = tup._2
    i += (if (length > 0) 1 else 0)
    //println("df1 " + df1)
    s = df1.negateN
    d1 = -s.autoDot()
    z1 = red / (1 - d1)

    while (outerLoop && i < math.abs(length)) {
      i = i + (if (length > 0) 1 else 0)
      x0 = x.clone()
      f0 = f1
      df0 = df1.clone()
      //println("x dims " + x.dims())
      //println("s dims " + s.dims())
      x = x + z1 * s
      tup = f(x); f2 = tup._1; df2 = tup._2
      i = i + (if (length < 0) 1 else 0)
      d2 = (df2.tN() * s).toScalar()
      f3 = f1
      d3 = d1
      z3 = -z1
      var m = if (length > 0) max else math.min(max, -length - i)
      success = false
      limit = -1
      var innerLoop = true
      while (innerLoop) {
        while (((f2 > f1 + z1 * rho * d1) || (d2 > -sig * d1)) && m > 0) {
          limit = z1
          if (f2 > f1) {
            z2 = z3 - .5f * d3 * z3 * z3 / (d3 * z3 + f2 - f3)
          } else {
            a = 6f * (f2 - f3) / z3 + 3f * (d2 + d3)
            b = 3f * (f3 - f2) - z3 * (d3 + 2f * d2)
            z2 = (math.sqrt(b * b - a * d2 * z3 * z3).asInstanceOf[Float] - b) / a
          }
          if (z2.isNaN() || z2.isInfinite()) {
            z2 = z3 / 2f
          }
          z2 = math.max(math.min(z2, int0 * z3), (1f - int0) * z3)
          z1 = z1 + z2 // update the step
          x = x + z2 * s
          tup = f(x); f2 = tup._1; df2 = tup._2
          m = m - 1;
          i = i + (if (length < 0) 1 else 0)
          d2 = (df2.tN * s).toScalar()
          z3 -= z2
        }
        if ((f2 > f1 + z1 * rho * d1) || (d2 > -sig * d1)) {
          innerLoop = false
        } else if (d2 > sig * d1) {
          success = true
          innerLoop = false
        } else if (m == 0) {
          innerLoop = false
        } else {
          a = 6 * (f2 - f3) / z3 + 3 * (d2 + d3) // make cubic extrapolation
          b = 3 * (f3 - f2) - z3 * (d3 + 2 * d2)
          z2 = -d2 * z3 * z3 / (b + math.sqrt(b * b - a * d2 * z3 * z3).asInstanceOf[Float]) // num. error possible - ok!

          if (!isReal(z2) || z2 < 0) // num prob or wrong sign?
            if (limit < -0.5f) // if we have no upper limit
              z2 = z1 * (ext - 1f) // the extrapolate the maximum amount
            else
              z2 = (limit - z1) / 2f // otherwise bisect
          else if ((limit > -0.5) && (z2 + z1 > limit)) // extraplation beyond max?
            z2 = (limit - z1) / 2 // bisect
          else if ((limit < -0.5) & (z2 + z1 > z1 * ext)) // extrapolation beyond limit
            z2 = z1 * (ext - 1.0f) // set to extrapolation limit
          else if (z2 < -z3 * int0)
            z2 = -z3 * int0
          else if ((limit > -0.5) && (z2 < (limit - z1) * (1.0 - int0))) // too close to limit?
            z2 = (limit - z1) * (1.0f - int0)
          f3 = f2; d3 = d2; z3 = -z2 // set point 3 equal to point 2
          z1 = z1 + z2
          x = x + z2 * s // update current estimates
          tup = f(x); f2 = tup._1; df2 = tup._2
          m = m - 1; i = i + (if (length < 0) 1 else 0) // count epochs?!
          d2 = (df2.tN() * s).toScalar()
        }
      }
      if (success) { // if line search succeeded
        f1 = f2
        if (null == fX) {
          fX = MatrixF.ones(1, 1) * f1
        } else {
          fX = (fX.tN() ++ (MatrixF.ones(1, 1) * f1)).tN
        }

        println("%s %4d | Cost: %4.6e\r".format(s, i, f1));

        s = s * (df2.autoDot() - (df1.tN() * df2).toScalar()) / (df1.autoDot) - df2 // Polack-Ribiere direction
        val tmp = df1; df1 = df2; df2 = tmp // swap derivatives
        d2 = (df1.tN * s).toScalar()
        if (d2 > 0) { // new slope must be negative
          s = df1 * -1; // otherwise use steepest direction
          d2 = (s.tN() * s * (-1)).toScalar()
        }
        z1 = z1 * math.min(ratio, d1 / (d2 - Float.MinPositiveValue)) // slope ratio but max RATIO
        d1 = d2
        ls_failed = false // this line search did not fail
      } else {
        x = x0; f1 = f0; df1 = df0; // restore point from before failed line search
        if (ls_failed || i > math.abs(length)) { // line search failed twice in a row
          outerLoop = false; // or we ran out of time, so we give up
        } else {
          val tmp = df1; df1 = df2; df2 = tmp; // swap derivatives
          s = df1 * -1f // try steepest
          d1 = (s.tN() * s).toScalar() * -1f
          z1 = 1f / (1f - d1)
          ls_failed = true // this line search failed
        }
      }
    }
    (x, fX, i)
  }

}