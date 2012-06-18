package com.hartenbower.matrix

object PCA {
  @inline
  def hypot(a: Double, b: Double) = {
    var r = 0d
    if (math.abs(a) > math.abs(b)) {
      r = b / a
       math.abs(a) * math.sqrt(1 + r * r);
    } else if (b != 0) {
      r = a / b
       math.abs(b) * math.sqrt(1 + r * r);
    } else {
       r
    }
  }

  def assignsa(s: Array[Double], k: Int, m: Int, n: Int, a: Array[Double]) {
    s(k) = 0d
    var i = k
    while (i < m) {
      s(k) = hypot(s(k), a(i * n + k))
      i += 1
    }

    if (s(k) != 0.0) {
      if (a(k * n + k) < 0.0) {
        s(k) = -s(k)
      }
      i = k
      while (i < m) {
        a(i * n + k) /= s(k)
        i += 1
      }
      a(k * n + k) += 1.0
    }
    s(k) = -s(k)

  }

  def assignae(k: Int, n: Int, nct: Int, m: Int, a: Array[Double], s: Array[Double], e: Array[Double]) {
    // ass a,e
    var j = k + 1
    val kn = k*n
    while (j < n) {
      if ((k < nct) & (s(k) != 0.0)) {

        // Apply the transformation.
        var t = 0d
        var i = k
        var in = 0
        while (i < m) {
          in = i * n
          t += a(in + k) * a(in + j)
          i += 1
        }
        t = -t / a(kn + k)
        i = k
        while (i < m) {
          in = i * n
          a(in + j) += t * a(in + k)
          i += 1
        }
      }

      // Place the k-th row of a into e for the
      // subsequent calculation of the row transformation.
      e(j) = a(kn + j)
      j += 1
    }

  }

  def assignu(k: Int, m: Int, n: Int, nu: Int, u: Array[Double], a: Array[Double]) {
    var i = k
    while (i < m) {
      u(i * nu + k) = a(i * n + k)
      i += 1
    }
  }

  def assigneworkav(e: Array[Double], k: Int, n: Int, m: Int, work: Array[Double], a: Array[Double], wantv: Boolean, v: Array[Double]) {
    var t = 0d
    e(k) = 0d
    var i = k + 1
    while (i < n) {
      e(k) = hypot(e(k), e(i))
      i += 1
    }
    if (e(k) != 0d) {
      if (e(k + 1) < 0d) {
        e(k) = -e(k)
      }
      i = k + 1
      while (i < n) {
        e(i) /= e(k)
        i += 1
      }
      e(k + 1) += 1d
    }
    e(k) = -e(k)
    if ((k + 1 < m) && (e(k) != 0d)) {
      // Apply the transformation.
      i = k + 1
      while (i < m) {
        work(i) = 0d
        i += 1
      }
      var j = k + 1
      while (j < n) {
        i = k + 1
        while (i < m) {
          work(i) += e(j) * a(i * n + j)
          i += 1
        }
        j += 1
      }
      j = k + 1
      while (j < n) {
        t = -e(j) / e(k + 1)
        i = k + 1
        while (i < m) {
          a(i * n + j) += t * work(i)
          i += 1
        }
        j += 1
      }
    }
    if (wantv) {
      // Place the transformation in V for subsequent
      // back multiplication.
      i = k + 1
      while (i < n) {
        v(i * n + k) = e(i)
        i += 1
      }
    }

  }

  /*
   *    Copied from Jama/SingularValueDecomp
   */
  def svd(x: MatrixD): (MatrixD, MatrixD, MatrixD) = {
    val a = x.elements.clone
    val (m, n) = x.dims
    val nu = math.min(m, n)
    val uM = MatrixD.zeros(m, nu)
    val u = uM.elements
    val sM = MatrixD.zeros(math.min(m + 1, n), 1)
    val s = sM.elements
    val vM = MatrixD.zeros(n, n)
    val v = vM.elements
    val e = new Array[Double](n)
    val work = new Array[Double](n)
    var wantu = true
    var wantv = true
    val nct = math.min(m - 1, n)
    println("nct " + nct)
    var nrt = math.max(0, math.min(n - 2, m));
    println("nrt " + nrt)
    val maxNctNrt = math.max(nct, nrt)
    println("maxNctNrt " + maxNctNrt)
    var k = 0
    var i = 0
    var j = 0
    var t = 0d
    while (k < maxNctNrt) {
      //assigns s, a
      if (k < nct) {
        assignsa(s, k, m, n, a)
      }

      //assignae(k: Int, n : Int, nct: Int, m: Int, a:Array[Double], s:Array[Double], e:Array[Double] ) 
      assignae(k, n, nct, m, a, s, e)

      // ass u
      if (wantu && (k < nct)) {
        // Place the transformation in U for subsequent back
        // multiplication.
        //assignu(k : Int, m : Int, nu : Int, u: Array[Double], a: Array[Double])
        assignu(k, m, n, nu, u, a)
      }

      // ass e, work, a, v
      if (k < nrt) {
        //assigneworkav(e: Array[Double], k: Int, n: Int, m: Int, work: Array[Double], a: Array[Double], wantv: Boolean, v: Array[Double]) {
        assigneworkav(e,k,n,m,work,a,wantv,v)
      }
      k += 1
    }

    var p = math.min(n, m + 1)
    if (nct < n) {
      s(nct) = a(nct * n + nct)
    }
    if (m < p) {
      s(p - 1) = 0d
    }
    if (nrt + 1 < p) {
      e(nrt) = a(nrt * n + p - 1)
    }
    e(p - 1) = 0d

    // If required, generate U.

    if (wantu) {
      j = nct
      while (j < nu) {
        i = 0
        while (i < m) {
          u(i * nu + j) = 0d
          i += 1
        }
        u(j * nu + j) = 1d
        j += 1
      }
      k = nct - 1
      while (k >= 0) {
        if (s(k) != 0d) {
          j = k + 1
          while (j < nu) {
            t = 0d
            i = k
            while (i < m) {
              t += u(i * nu + k) * u(i * nu + j)
              i += 1
            }
            t = -t / u(k * nu + k)
            i = k
            while (i < m) {
              u(i * nu + j) += t * u(i * nu + k)
              i += 1
            }
            j += 1
          }
          i = k
          while (i < m) {
            u(i * nu + k) = -u(i * nu + k)
            i += 1
          }
          u(k * nu + k) = 1d + u(k * nu + k)
          i = 0
          while (i < k - 1) {
            u(i * nu + k) = 0d
            i += 1
          }
        } else {
          i = 0
          while (i < m) {
            u(i * nu + k) = 0d
            i += 1
          }
          u(k * nu + k) = 1d
        }
        k -= 1
      }
    }
    // If required, generate V.

    if (wantv) {
      k = n - 1
      while (k >= 0) {
        if ((k < nrt) & (e(k) != 0.0)) {
          j = k + 1
          while (j < nu) {
            t = 0d
            i = k + 1
            while (i < n) {
              t += v(i * n + k) * v(i * n + j)
              i += 1
            }
            t = -t / v((k + 1) * n + k)
            i = k + 1
            while (i < n) {
              v(i * n + j) += t * v(i * n + k)
              i += 1
            }
            j += 1
          }
        }
        i = 0
        while (i < n) {
          v(i * n + k) = 0d
          i += 1
        }
        v(k * n + k) = 1d
        k -= 1
      }
    }

    // Main iteration loop for the singular values.

    val pp = p - 1
    var iter = 0
    val eps = math.pow(2.0, -52.0);
    val tiny = math.pow(2.0, -966.0);
    while (p > 0) {
      var k = 0
      var kase = 0
      var stop = false

      // Here is where a test for too many iterations would go.

      // This section of the program inspects for
      // negligible elements in the s and e arrays.  On
      // completion the variables kase and k are set as follows.

      // kase = 1     if s(p) and e[k-1] are negligible and k<p
      // kase = 2     if s(k) is negligible and k<p
      // kase = 3     if e[k-1] is negligible, k<p, and
      //              s(k), ..., s(p) are not negligible (qr step).
      // kase = 4     if e(p-1) is negligible (convergence).
      k = p - 2
      while (k >= -1 && !stop) {
        if (k == -1) {
          stop = true
        } else if (k != -1) {
          if (math.abs(e(k)) <=
            tiny + eps * (math.abs(s(k)) + math.abs(s(k + 1)))) {
            e(k) = 0.0;
            stop = true
          }
        }
        if (!stop) k -= 1
      }
      if (k == p - 2) {
        kase = 4;
      } else {
        var ks = p - 1
        stop = false
        while (ks >= k && !stop) {
          if (ks == k) {
            stop = true;
          } else {
            t = (if (ks != p) math.abs(e(ks)) else 0d) +
              (if (ks != k + 1) math.abs(e(ks - 1)) else 0d);
            if (math.abs(s(ks)) <= tiny + eps * t) {
              s(ks) = 0d
              stop = true
            }
          }
          if (!stop) ks -= 1
        }

        if (ks == k) {
          kase = 3;
        } else if (ks == p - 1) {
          kase = 1;
        } else {
          kase = 2;
          k = ks;
        }
      }
      k += 1

      // Perform the task indicated by kase.

      kase match {

        // Deflate negligible s(p).

        case 1 =>
          var f = e(p - 2)
          e(p - 2) = 0d
          j = p - 2
          while (j >= k) {
            t = hypot(s(j), f)
            val cs = s(j) / t
            val sn = f / t
            s(j) = t
            if (j != k) {
              f = -sn * e(j - 1)
              e(j - 1) = cs * e(j - 1)
            }
            if (wantv) {
              i = 0
              while (i < n) {
                t = cs * v(i * n + j) + sn * v(i * n + p - 1)
                v(i * n + p - 1) = -sn * v(i * n + j) + cs * v(i * n + p - 1)
                v(i * n + j) = t
                i += 1
              }
            }
            j -= 1
          }

        // Split at negligible s(k).

        case 2 =>
          var f = e(k - 1)
          e(k - 1) = 0d
          j = k
          while (j < p) {
            t = hypot(s(j), f)
            val cs = s(j) / t
            val sn = f / t;
            s(j) = t
            f = -sn * e(j)
            e(j) = cs * e(j)
            if (wantu) {
              i = 0
              while (i < m) {
                t = cs * u(i * nu + j) + sn * u(i * nu + k - 1)
                u(i * nu + k - 1) = -sn * u(i * nu + j) + cs * u(i * nu + k - 1)
                u(i * nu + j) = t
                i += 1
              }
            }
            j += 1
          }

        // Perform one qr step.

        case 3 =>

          // Calculate the shift.

          val scale = math.max(math.max(math.max(math.max(
            math.abs(s(p - 1)), math.abs(s(p - 2))), math.abs(e(p - 2))),
            math.abs(s(k))), math.abs(e(k)))
          val sp = s(p - 1) / scale
          val spm1 = s(p - 2) / scale
          val epm1 = e(p - 2) / scale
          val sk = s(k) / scale
          val ek = e(k) / scale
          val b = ((spm1 + sp) * (spm1 - sp) + epm1 * epm1) / 2.0
          val c = sp * epm1 * sp * epm1
          var shift = 0d
          if ((b != 0.0) || (c != 0.0)) {
            shift = math.sqrt(b * b + c);
            if (b < 0.0) {
              shift = -shift
            }
            shift = c / (b + shift)
          }
          var f = (sk + sp) * (sk - sp) + shift;
          var g = sk * ek;

          // Chase zeros.
          j = k
          while (j < p - 1) {
            t = hypot(f, g)
            var cs = f / t
            var sn = g / t
            if (j != k) {
              e(j - 1) = t
            }
            f = cs * s(j) + sn * e(j)
            e(j) = cs * e(j) - sn * s(j)
            g = sn * s(j + 1);
            s(j + 1) = cs * s(j + 1)
            if (wantv) {
              i = 0
              while (i < n) {
                t = cs * v(i * n + j) + sn * v(i * n + j + 1)
                v(i * n + j + 1) = -sn * v(i * n + j) + cs * v(i * n + j + 1)
                v(i * n + j) = t
                i += 1
              }
            }
            t = hypot(f, g);
            cs = f / t
            sn = g / t
            s(j) = t
            f = cs * e(j) + sn * s(j + 1)
            s(j + 1) = -sn * e(j) + cs * s(j + 1)
            g = sn * e(j + 1)
            e(j + 1) = cs * e(j + 1)
            if (wantu && (j < m - 1)) {
              i = 0
              while (i < m) {
                t = cs * u(i * nu + j) + sn * u(i * nu + j + 1)
                u(i * nu + j + 1) = -sn * u(i * nu + j) + cs * u(i * nu + j + 1)
                u(i * nu + j) = t
                i += 1
              }
            }
            j += 1
          }
          e(p - 2) = f;
          iter = iter + 1;

        // Convergence.

        case 4 => {

          // Make the singular values positive.

          if (s(k) <= 0.0) {
            s(k) = if (s(k) < 0.0) -s(k) else 0d
            if (wantv) {
              i = 0
              while (i <= pp) {
                v(i * n + k) = -v(i * n + k)
                i += 1
              }
            }
          }

          // Order the singular values.
          stop = false
          while (k < pp && !stop) {
            if (s(k) >= s(k + 1)) {
              stop = true;
            } else {
              t = s(k)
              s(k) = s(k + 1)
              s(k + 1) = t
              if (wantv && (k < n - 1)) {
                i = 0
                while (i < n) {
                  t = v(i * n + k + 1); v(i * n + k + 1) = v(i * n + k); v(i * n + k) = t
                  i += 1
                }
              }
              if (wantu && (k < m - 1)) {
                i = 0
                while (i < m) {
                  t = u(i * nu + k + 1); u(i * nu + k + 1) = u(i * nu + k); u(i * nu + k) = t
                  i += 1
                }
              }
            }
            if (!stop) k += 1
          }
          iter = 0
          p -= 1
        }
      }
    }
    (uM, sM, vM)
  }
  
  
  /*
   *    Copied from Jama/SingularValueDecomp
   */
  def svdDc(x: MatrixD): (MatrixD, MatrixD, MatrixD) = {
    val a = x.elements.clone
    val (m, n) = x.dims
    val nu = math.min(m, n)
    val uM = MatrixD.zeros(m, nu)
    val u = uM.elements
    val sM = MatrixD.zeros(math.min(m + 1, n), 1)
    val s = sM.elements
    val vM = MatrixD.zeros(n, n)
    val v = vM.elements
    val e = new Array[Double](n)
    val work = new Array[Double](n)
    var wantu = true
    var wantv = true
    val nct = math.min(m - 1, n)
    println("nct " + nct)
    var nrt = math.max(0, math.min(n - 2, m));
    println("nrt " + nrt)
    val maxNctNrt = math.max(nct, nrt)
    println("maxNctNrt " + maxNctNrt)
    var k = 0
    var i = 0
    var j = 0
    var t = 0d
    while (k < maxNctNrt) {
      //assigns s, a
      if (k < nct) {
        assignsa(s, k, m, n, a)
      }

      //assignae(k: Int, n : Int, nct: Int, m: Int, a:Array[Double], s:Array[Double], e:Array[Double] ) 
      assignae(k, n, nct, m, a, s, e)

      // ass u
      if (wantu && (k < nct)) {
        // Place the transformation in U for subsequent back
        // multiplication.
        //assignu(k : Int, m : Int, nu : Int, u: Array[Double], a: Array[Double])
        assignu(k, m, n, nu, u, a)
      }

      // ass e, work, a, v
      if (k < nrt) {
        //assigneworkav(e: Array[Double], k: Int, n: Int, m: Int, work: Array[Double], a: Array[Double], wantv: Boolean, v: Array[Double]) {
        assigneworkav(e,k,n,m,work,a,wantv,v)
      }
      k += 1
    }

    var p = math.min(n, m + 1)
    if (nct < n) {
      s(nct) = a(nct * n + nct)
    }
    if (m < p) {
      s(p - 1) = 0d
    }
    if (nrt + 1 < p) {
      e(nrt) = a(nrt * n + p - 1)
    }
    e(p - 1) = 0d

    // If required, generate U.

    if (wantu) {
      j = nct
      while (j < nu) {
        i = 0
        while (i < m) {
          u(i * nu + j) = 0d
          i += 1
        }
        u(j * nu + j) = 1d
        j += 1
      }
      k = nct - 1
      while (k >= 0) {
        if (s(k) != 0d) {
          j = k + 1
          while (j < nu) {
            t = 0d
            i = k
            while (i < m) {
              t += u(i * nu + k) * u(i * nu + j)
              i += 1
            }
            t = -t / u(k * nu + k)
            i = k
            while (i < m) {
              u(i * nu + j) += t * u(i * nu + k)
              i += 1
            }
            j += 1
          }
          i = k
          while (i < m) {
            u(i * nu + k) = -u(i * nu + k)
            i += 1
          }
          u(k * nu + k) = 1d + u(k * nu + k)
          i = 0
          while (i < k - 1) {
            u(i * nu + k) = 0d
            i += 1
          }
        } else {
          i = 0
          while (i < m) {
            u(i * nu + k) = 0d
            i += 1
          }
          u(k * nu + k) = 1d
        }
        k -= 1
      }
    }
    // If required, generate V.

    if (wantv) {
      k = n - 1
      while (k >= 0) {
        if ((k < nrt) & (e(k) != 0.0)) {
          j = k + 1
          while (j < nu) {
            t = 0d
            i = k + 1
            while (i < n) {
              t += v(i * n + k) * v(i * n + j)
              i += 1
            }
            t = -t / v((k + 1) * n + k)
            i = k + 1
            while (i < n) {
              v(i * n + j) += t * v(i * n + k)
              i += 1
            }
            j += 1
          }
        }
        i = 0
        while (i < n) {
          v(i * n + k) = 0d
          i += 1
        }
        v(k * n + k) = 1d
        k -= 1
      }
    }

    // Main iteration loop for the singular values.

    val pp = p - 1
    var iter = 0
    val eps = math.pow(2.0, -52.0);
    val tiny = math.pow(2.0, -966.0);
    while (p > 0) {
      var k = 0
      var kase = 0
      var stop = false

      // Here is where a test for too many iterations would go.

      // This section of the program inspects for
      // negligible elements in the s and e arrays.  On
      // completion the variables kase and k are set as follows.

      // kase = 1     if s(p) and e[k-1] are negligible and k<p
      // kase = 2     if s(k) is negligible and k<p
      // kase = 3     if e[k-1] is negligible, k<p, and
      //              s(k), ..., s(p) are not negligible (qr step).
      // kase = 4     if e(p-1) is negligible (convergence).
      k = p - 2
      while (k >= -1 && !stop) {
        if (k == -1) {
          stop = true
        } else if (k != -1) {
          if (math.abs(e(k)) <=
            tiny + eps * (math.abs(s(k)) + math.abs(s(k + 1)))) {
            e(k) = 0.0;
            stop = true
          }
        }
        if (!stop) k -= 1
      }
      if (k == p - 2) {
        kase = 4;
      } else {
        var ks = p - 1
        stop = false
        while (ks >= k && !stop) {
          if (ks == k) {
            stop = true;
          } else {
            t = (if (ks != p) math.abs(e(ks)) else 0d) +
              (if (ks != k + 1) math.abs(e(ks - 1)) else 0d);
            if (math.abs(s(ks)) <= tiny + eps * t) {
              s(ks) = 0d
              stop = true
            }
          }
          if (!stop) ks -= 1
        }

        if (ks == k) {
          kase = 3;
        } else if (ks == p - 1) {
          kase = 1;
        } else {
          kase = 2;
          k = ks;
        }
      }
      k += 1

      // Perform the task indicated by kase.

      kase match {

        // Deflate negligible s(p).

        case 1 =>
          var f = e(p - 2)
          e(p - 2) = 0d
          j = p - 2
          while (j >= k) {
            t = hypot(s(j), f)
            val cs = s(j) / t
            val sn = f / t
            s(j) = t
            if (j != k) {
              f = -sn * e(j - 1)
              e(j - 1) = cs * e(j - 1)
            }
            if (wantv) {
              i = 0
              while (i < n) {
                t = cs * v(i * n + j) + sn * v(i * n + p - 1)
                v(i * n + p - 1) = -sn * v(i * n + j) + cs * v(i * n + p - 1)
                v(i * n + j) = t
                i += 1
              }
            }
            j -= 1
          }

        // Split at negligible s(k).

        case 2 =>
          var f = e(k - 1)
          e(k - 1) = 0d
          j = k
          while (j < p) {
            t = hypot(s(j), f)
            val cs = s(j) / t
            val sn = f / t;
            s(j) = t
            f = -sn * e(j)
            e(j) = cs * e(j)
            if (wantu) {
              i = 0
              while (i < m) {
                t = cs * u(i * nu + j) + sn * u(i * nu + k - 1)
                u(i * nu + k - 1) = -sn * u(i * nu + j) + cs * u(i * nu + k - 1)
                u(i * nu + j) = t
                i += 1
              }
            }
            j += 1
          }

        // Perform one qr step.

        case 3 =>

          // Calculate the shift.

          val scale = math.max(math.max(math.max(math.max(
            math.abs(s(p - 1)), math.abs(s(p - 2))), math.abs(e(p - 2))),
            math.abs(s(k))), math.abs(e(k)))
          val sp = s(p - 1) / scale
          val spm1 = s(p - 2) / scale
          val epm1 = e(p - 2) / scale
          val sk = s(k) / scale
          val ek = e(k) / scale
          val b = ((spm1 + sp) * (spm1 - sp) + epm1 * epm1) / 2.0
          val c = sp * epm1 * sp * epm1
          var shift = 0d
          if ((b != 0.0) || (c != 0.0)) {
            shift = math.sqrt(b * b + c);
            if (b < 0.0) {
              shift = -shift
            }
            shift = c / (b + shift)
          }
          var f = (sk + sp) * (sk - sp) + shift;
          var g = sk * ek;

          // Chase zeros.
          j = k
          while (j < p - 1) {
            t = hypot(f, g)
            var cs = f / t
            var sn = g / t
            if (j != k) {
              e(j - 1) = t
            }
            f = cs * s(j) + sn * e(j)
            e(j) = cs * e(j) - sn * s(j)
            g = sn * s(j + 1);
            s(j + 1) = cs * s(j + 1)
            if (wantv) {
              i = 0
              while (i < n) {
                t = cs * v(i * n + j) + sn * v(i * n + j + 1)
                v(i * n + j + 1) = -sn * v(i * n + j) + cs * v(i * n + j + 1)
                v(i * n + j) = t
                i += 1
              }
            }
            t = hypot(f, g);
            cs = f / t
            sn = g / t
            s(j) = t
            f = cs * e(j) + sn * s(j + 1)
            s(j + 1) = -sn * e(j) + cs * s(j + 1)
            g = sn * e(j + 1)
            e(j + 1) = cs * e(j + 1)
            if (wantu && (j < m - 1)) {
              i = 0
              while (i < m) {
                t = cs * u(i * nu + j) + sn * u(i * nu + j + 1)
                u(i * nu + j + 1) = -sn * u(i * nu + j) + cs * u(i * nu + j + 1)
                u(i * nu + j) = t
                i += 1
              }
            }
            j += 1
          }
          e(p - 2) = f;
          iter = iter + 1;

        // Convergence.

        case 4 => {

          // Make the singular values positive.

          if (s(k) <= 0.0) {
            s(k) = if (s(k) < 0.0) -s(k) else 0d
            if (wantv) {
              i = 0
              while (i <= pp) {
                v(i * n + k) = -v(i * n + k)
                i += 1
              }
            }
          }

          // Order the singular values.
          stop = false
          while (k < pp && !stop) {
            if (s(k) >= s(k + 1)) {
              stop = true;
            } else {
              t = s(k)
              s(k) = s(k + 1)
              s(k + 1) = t
              if (wantv && (k < n - 1)) {
                i = 0
                while (i < n) {
                  t = v(i * n + k + 1); v(i * n + k + 1) = v(i * n + k); v(i * n + k) = t
                  i += 1
                }
              }
              if (wantu && (k < m - 1)) {
                i = 0
                while (i < m) {
                  t = u(i * nu + k + 1); u(i * nu + k + 1) = u(i * nu + k); u(i * nu + k) = t
                  i += 1
                }
              }
            }
            if (!stop) k += 1
          }
          iter = 0
          p -= 1
        }
      }
    }
    (uM, sM, vM)
  }

  /**
   * @param x : dataset of x.nRows samples each of x.nCols dimension
   * @return the eigenvectors/values of the covariance matrix sigma by singular value decomposition
   */
  def pca(x: MatrixD): (MatrixD, MatrixD) = {

    val sigma = (x.tN * x) / x.nRows
    val tup = svd(sigma)
    (tup._1, tup._2)
  }
}