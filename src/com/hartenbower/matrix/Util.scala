package com.hartenbower.matrix
import scala.io.Source
import scala.util.Random

object Util {
  def elapsed(msg: String, l: Long) = {
    val now = System.currentTimeMillis
    println(msg + " took " + (now - l) + " millis")
    (now, now - l)
  }

  def time(msg: String, f: => Unit, count: Int = 1): Tuple2[Int, Long] = {
    val l = System.currentTimeMillis
    var idx = count
    while (idx > 0) {
      f
      idx -= 1
    }
    val delta = (System.currentTimeMillis - l)
    println(msg + " took " + delta + " ms or " + (count * 1000. / delta) + "evals/s")
    (count, delta)

  }
  
  def randperm(m : Int) : List[Int] = {
    val rnd = new Random(System.currentTimeMillis())
    var src = new Array[Int](m)
    var res = new Array[Int](m)
    var i = 0
    while(i < m) {
      src(i) = i + 1
      i+=1
    }
    i = 0
    
    while(src.length > 1) {
      val idx = math.abs(rnd.nextInt) % src.length
      res(i) = src(idx)
      src = src.filterNot( _ == src(idx))
      i += 1
    }
    res.toList
  }

  private def lines(s: Array[Char]): Array[String] = {
    println("loaded file")
    var l = List[String]()
    var line = ""
    var c: Char = ' '
    var idx = 0
    val len = s.length
    val sb = new StringBuffer
    while (idx < len) {
      c = s(idx)
      if (c != '\n') {
        sb.append(c)
      } else {
        line = sb.toString()
        sb.setLength(0)
        if (!line.trim().isEmpty())
          l = l :+ line.trim
        line = ""
      }
      idx += 1
    }
    l.toArray
  }

  def parseOctaveDataFile(path: String): Map[String, _] = {
    var m = Map[String, Any]()
    val la = lines(Source.fromFile(path).toArray)
    println("loaded " + path + " into line array of size " + la.length)
    var elementType = ""
    var idx = -1
    var rows = -1
    var cols = -1
    var currRow = 0
    var lastChunk = 0l
    var name = ""
    var elementData = Array[Double]()
    var lastTime = 0l

    def addObject {
      if (!elementData.isEmpty) {
        println("loading " + name + " took " + (System.currentTimeMillis() - lastTime))
        elementType match {
          case "matrix" =>
            println("adding " + elementType + " '" + name + "' of dims " + rows + ", " + cols)
            m = m + (name -> new MatrixD(elementData, cols))
          case "scalar" =>
            println("adding " + elementType + " '" + name + "' of dims " + rows + ", " + cols)
            m = m + (name -> elementData(0))
        }
      }
    }

    def parseDataLine(l: String, elementData: Array[Double], startIdx: Int, cols: Int) {
      val spl = l.split(" ")
      assert(spl.length == cols)
      val len = spl.length
      var idx = 0
      while (idx < len) {
        elementData(startIdx + idx) = java.lang.Double.parseDouble(spl(idx))
        idx += 1
      }
    }

    for (line <- la) {
      if (line.startsWith("#")) {
        idx = line.indexOf("name:")
        if (idx > -1) {
          if (!name.isEmpty() && !elementType.isEmpty()) {
            // add previous obj
            addObject
            currRow = 0
          }
          name = line.substring(idx + "name:".length).trim
          println("found " + name)
          lastTime = System.currentTimeMillis()
          rows = -1
          cols = -1
          elementType = ""
        } else {
          idx = line.indexOf("type:")
          if (idx > -1) {
            elementType = line.substring(idx + "type:".length).trim
          } else {
            idx = line.indexOf("rows:")
            if (idx > -1) {
              rows = Integer.parseInt(line.substring(idx + "rows:".length).trim)
            } else {
              idx = line.indexOf("columns:")
              if (idx > -1) {
                cols = Integer.parseInt(line.substring(idx + "columns:".length).trim)
              }
            }
          }
        }
      } else {
        elementType match {
          case "matrix" =>
            assert(!name.isEmpty() && !elementType.isEmpty() && rows > -1 && cols > -1)
            if (elementData.length != rows * cols) {
              elementData = new Array[Double](rows * cols)
            }
            // it's a data line (row)
            parseDataLine(line, elementData, currRow * cols, cols)
            currRow += 1
            if (currRow % 100 == 0) {
              val now = System.currentTimeMillis()
              if (lastChunk != 0) {
                val delta = ((now - lastChunk) / 1000.).asInstanceOf[Int]
                println("on " + currRow + "/" + rows + " at " + delta / 100.f + " s/row")
              } else {
                println("on " + currRow + "/" + rows)
              }
              lastChunk = now
            }

          case "scalar" =>
            elementData = Array[Double](java.lang.Double.parseDouble(line))
          case _ =>
            println("unknown type " + elementType)
        }
      }
    }
    // add last obj
    if (!name.isEmpty() && !elementType.isEmpty()) {
      addObject
    }
    m
  }
  
  def sum(a: Array[Double]): Double = {
    var i = a.length - 1
    var s = 0d
    while (i > -1) {
      s += a(i)
      i -= 1
    }
    s
  }

  def accuracy(s : Array[_], t : Array[_]) : Double = {
    var res = 0d
    var i = 0
    while (i < s.length) {
      res += ( if ( s(i) == t(i) ) 1d else 0d)
      i += 1
    }
    res / s.length
  }
  
  def sumSquaredDiffs(s : Array[Double], t : Array[Double] ) = {
    val len = s.length
    var i = 0
    var sum = 0d
    var delta = 0d
    while(i < len) {
      delta = t(i) - s(i)
      sum += delta * delta
    }
    sum
  }
   
  def meanSquaredError(s : Array[Double], t : Array[Double] ) = sumSquaredDiffs(s,t)/s.length

}