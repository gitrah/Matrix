package com.hartenbower.matrix

import MatrixD._
import LogisticRegression._

/**
 * Port of fmincg.m (Octave function)
 *
 */
object ConjugateGradient {
  def isReal(d: Double) = (!d.isNaN() && !d.isInfinite())
  val rho = 0.01d
  val sig = .5d
  val int0 = .1d
  val ext = 3.0d
  val max = 20
  val ratio = 100
  def fmincg(f: (MatrixD) => (Double, MatrixD), xin: MatrixD, length: Int = 50, red: Int = 1): (MatrixD, MatrixD, Int) = {
    var x: MatrixD = xin.clone()
    var a = 0d
    var b = 0d
    var x0: MatrixD = null
    var df0: MatrixD = null
    var df1: MatrixD = null
    var df2: MatrixD = null
    var f0: Double = 0d
    var f1: Double = 0d
    var f2: Double = 0d
    var f3: Double = 0d
    var s: MatrixD = null
    var d1: Double = 0d
    var d2: Double = 0d
    var d3: Double = 0d
    var z1: Double = 0d
    var z2: Double = 0d
    var z3: Double = 0d
    var tup: (Double, MatrixD) = null
    var success = false
    var outerLoop = true
    var limit = 0d
    var i = 0
    var ls_failed = false
    //var fX = new Array[Double](0)
 	println("0 x.sum() " + x.sum())
    var fX: MatrixD = null
    tup = f(x); f1 = tup._1; df1 = tup._2
    println("f1 " + f1)
    i += (if (length > 0) 1 else 0)
    s = df1.negateN
    d1 = -s.autoDot()
    z1 = red / (1 - d1)
    println("init df1.sum " +df1.sum() + ", s.sum " + s.sum() + ", d1 " + d1)

    while (outerLoop && i < math.abs(length)) {
      i = i + (if (length > 0) 1 else 0)
      x0 = x.clone()
      f0 = f1
      df0 = df1.clone()
      print("i " + i)
      println (" z1 " + z1)
      x = x + z1 * s
	  println("0.1 x.sum " + x.sum())
      tup = f(x); f2 = tup._1; df2 = tup._2
      println("f2 " + f2 + ", df2.sum " + df2.sum());
      i = i + (if (length < 0) 1 else 0)
      d2 = (df2.tN() * s).toScalar()
      println("d2 " + d2);
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
            z2 = z3 - .5 * d3 * z3 * z3 / (d3 * z3 + f2 - f3)
            println("f2 > f1, z2 " + z2);
         } else {
            a = 6d * (f2 - f3) / z3 + 3d * (d2 + d3)
            b = 3d * (f3 - f2) - z3 * (d3 + 2d * d2)
            z2 = (math.sqrt(b * b - a * d2 * z3 * z3) - b) / a
            println("f2 <= f1, a " + a + ", b " +b + ", z2 " + z2);
          }
          if (z2.isNaN() || z2.isInfinite()) {
            println("bad z2");
            z2 = z3 / 2d
          }
          z2 = math.max(math.min(z2, int0 * z3), (1d - int0) * z3)
          z1 = z1 + z2 // update the step
          println("1 z1 = z1 + z2 " + z1)
          x = x + z2 * s
          tup = f(x); f2 = tup._1; df2 = tup._2
          m = m - 1;
          i = i + (if (length < 0) 1 else 0)
          d2 = (df2.tN * s).toScalar()
          z3 -= z2
          println("1 d2 " + d2 + ", z3 " + z3);
        }
        if ((f2 > f1 + z1 * rho * d1) || (d2 > -sig * d1)) {
          println("f2 > f1 + z1 * rho * d1) || (d2 > -sig * d1)")
          innerLoop = false
        } else if (d2 > sig * d1) {
          println("d2 > sig * d1")
          success = true
          innerLoop = false
        } else if (m == 0) {
          println("m==0")
          innerLoop = false
        } else {
          a = 6 * (f2 - f3) / z3 + 3 * (d2 + d3) // make cubic extrapolation
          b = 3 * (f3 - f2) - z3 * (d3 + 2 * d2)
          z2 = -d2 * z3 * z3 / (b + math.sqrt(b * b - a * d2 * z3 * z3)) // num. error possible - ok!
          println("a " + a + ", b " + b + ", z2 " + z2)

          if (!isReal(z2) || z2 < 0) // num prob or wrong sign?
            if (limit < -0.5) // if we have no upper limit
              z2 = z1 * (ext - 1d) // the extrapolate the maximum amount
            else
              z2 = (limit - z1) / 2d // otherwise bisect
          else if ((limit > -0.5) && (z2 + z1 > limit)) // extraplation beyond max?
            z2 = (limit - z1) / 2 // bisect
          else if ((limit < -0.5) & (z2 + z1 > z1 * ext)) // extrapolation beyond limit
            z2 = z1 * (ext - 1.0) // set to extrapolation limit
          else if (z2 < -z3 * int0)
            z2 = -z3 * int0
          else if ((limit > -0.5) && (z2 < (limit - z1) * (1.0 - int0))) // too close to limit?
            z2 = (limit - z1) * (1.0 - int0)
         
          println("z2b " + z2);
          println("f3 "   +  f3 + " f2 " + f2  +  " d3 "  + d3 );
          println(" d2 "  +  d2 + " z3 " +  z3  +  " z2 "  + z2);
          f3 = f2; d3 = d2; z3 = -z2 // set point 3 equal to point 2
          z1 = z1 + z2
          println("2 z1 = z1 + z2 " + z1);
          x = x + z2 * s // update current estimates
          println("2 x.sum " + x.sum());
          tup = f(x); f2 = tup._1; df2 = tup._2
          println("df2.sum " + df2.sum() + " df2.txp.sum " + df2.tN().sum());
          println("f2 " + f2)
          m = m - 1; i = i + (if (length < 0) 1 else 0) // count epochs?!
          d2 = (df2.tN() * s).toScalar()
          println("d2 " + d2)
        }
        println("repeat while (innerLoop)")
      }
      println("done innerLoop")
      if (success) { // if line search succeeded
       println("success f1 " + f1)
       f1 = f2
        if (null == fX) {
          fX = MatrixD.ones(1, 1) * f1
          println("simple fX " + fX)
       } else {
          fX = (fX.tN() ++ (MatrixD.ones(1, 1) * f1)).tN
          println("complex fX " + fX)
        }

        println("%4d | Cost: %4.6e\r".format(i, f1));

        s = s * (df2.autoDot() - (df1.tN() * df2).toScalar()) / (df1.autoDot) - df2 // Polack-Ribiere direction
		println("Polack-Ribiere s.sum " + s.sum());
        val tmp = df1; df1 = df2; df2 = tmp // swap derivatives
		println("df1.sum " + df1.sum() + ", df2.sum " + df2.sum() + ", tmp.sum() " + tmp.sum());
        d2 = (df1.tN * s).toScalar()
	    println("P-R d2 " + d2);
        if (d2 > 0) { // new slope must be negative
          s = df1 * -1; // otherwise use steepest direction
          d2 = (s.tN() * s * (-1)).toScalar()
        }
	    println("z1 " + z1 + ", ratio " + ratio + ", d1 " + d1 + ", d2 " + d2 + ", minPos " + MatrixD.MinPositiveValue);
        println( " d1 / (d2 - MatrixD.MinPositiveValue) " +  (d1 / (d2 - MatrixD.MinPositiveValue)))
	    z1 = z1 * math.min(ratio, d1 / (d2 - MatrixD.MinPositiveValue)) // slope ratio but max RATIO
		println("z1 * min(ratio,( d1 / (d2 - MinPositiveValue))) " + z1);
        d1 = d2
        ls_failed = false // this line search did not fail
      } else {
        x = x0; f1 = f0; df1 = df0; // restore point from before failed line search
        if (ls_failed || i > math.abs(length)) { // line search failed twice in a row
          outerLoop = false; // or we ran out of time, so we give up
        } else {
          val tmp = df1; df1 = df2; df2 = tmp; // swap derivatives
          s = df1 * -1d // try steepest
          d1 = (s.tN() * s).toScalar() * -1d
          z1 = 1 / (1 - d1)
          ls_failed = true // this line search failed
        }
      }
    }
    (x, fX, i)
  }

}