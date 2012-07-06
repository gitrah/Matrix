package com.hartenbower.matrix

class LUDecomposition(x: MatrixD) {
  val lu = x.elements
  val (m, n) = x.dims
  val pivots = (0 to m - 1).toArray

  var pivsign = 1
  var luRowi = 0
  val luColj = new Array[Double](m)

  compute

  def compute() = {
    var j = 0
    var i = 0
    var k = 0
    var p = 0
    var s = 0d
    var kMax = 0
    var t = 0d
    while (j < n) {
      // Make a copy of the j-th column to localize references.
      i = 0
      while (i < m) {
        luColj(i) = lu(i * n + j)
        i += 1
      }
      i = 0
      // Apply previous transformations.
      while (i < m) {
        luRowi = i * n
        // Most of the time is spent in the following dot product.
        kMax = math.min(i, j)
        s = 0
        k = 0
        while (k < kMax) {
          s += lu(luRowi + k) * luColj(k)
          k += 1
        }
        luColj(i) -= s
        lu(luRowi + j) = luColj(i)
        i += 1
      }
      // Find pivot and exchange if necessary.
      p = j
      i = j + 1
      while (i < m) {
        if (math.abs(luColj(i)) > math.abs(luColj(p))) {
          p = i
        }
        i += 1
      }
      if (p != j) {
        k = 0
        while (k < n) {
          t = lu(p * n + k)
          lu(p * n + k) = lu(j * n + k)
          lu(j * n + k) = t
          k += 1
        }

        k = pivots(p); pivots(p) = pivots(j); pivots(j) = k;
        pivsign = -pivsign;
      }

      // Compute multipliers.
      if (j < m && lu(j * n + j) != 0.0) {
        i = j + 1
        while (i < m) {
          lu(i * n + j) /= lu(j * n + j)
          i += 1
        }
      }
      j += 1
    }
  }

  def singularQ = {
    var j = 0
    while (j < n) {
      if (lu(j * n + j) == 0) {
        true
      }
      j += 1
    }
    false
  }

  def det() = {
    if (m != n) {
      throw new IllegalArgumentException("Matrix must be square.");
    }
    var d = pivsign.asInstanceOf[Double]
    var j = 0
    while (j < n) {
      d *= lu(j * n + j)
      j += 1
    }
    d
  }

  def solve(b: MatrixD): MatrixD = {
    if (b.nRows != m) {
      throw new IllegalArgumentException("Matrix row dimensions must agree.");
    }
    println("n " + n)
    println("m " + m)
    println("lu.length " + lu.length)
    
    if (m == n && singularQ) {
      throw new RuntimeException("Matrix is singular.");
    }

    // Copy right hand side with pivoting
    val nx = b.nCols
    val xm: MatrixD = b.clippedRowSubset(pivots, (0, nx - 1))
    val (mm, nm) = xm.dims
    val x = xm.elements

    // Solve L*Y = B(piv,:)
    var k = 0
    var i = 0
    var j = 0
    while (k < n) {
      i = k + 1
      while (i < n) {
        j = 0
        while (j < nx) {
          x(i * nm + j) -= x(k * nm + j) * lu(i * n + k)
          j += 1
        }
        i += 1
      }
      k += 1
    }
    // Solve U*X = Y;
    k = n - 1
    while (k >= 0) {
      j = 0
      while (j < nx) {
        x(k * nm + j) /= lu(k * n + k)
        j += 1
      }
      i = 0
      while (i < k) {
        j = 0
        while (j < nx) {
          x(i * nm + j) -= x(k * nm + j) * lu(i * n + k)
          j += 1
        }
        i += 1
      }
      k -= 1
    }
    xm

  }
}