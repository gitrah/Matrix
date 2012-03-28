package com.hartenbower.matrix

object GradientDescent {
  /**
   linear regresson model 
   hypothesis hθ(x) = θ0 + θ1(x)
   with squared error cost function J
               1   m      (i)  (i) 2
   J(θ0,θ1) = ---  Σ  (hθ(x) - y  ) 
  	  	      2m  i=1
  */
  def Σ(f : ((Double,Double)) => Double, data : List[(Double,Double)]) : Double = {
    (0.0 /: (for(i <- 1 to data.length) yield f(data(i-1))))(_+_)
  }
  
  def hypothesis(θ0 : Double, θ1 : Double, x : Double) = θ0 + θ1 *x
  
  def θ0θ1(θ0 : Double, θ1 : Double, α : Double , data : List[(Double,Double)]) : (Double,Double) = {
    val m = data.length
    (θ0 - α/m * Σ((x)=>  θ0 + θ1 * x._1 - x._2, data) ,
      θ1 -  α/m * Σ((x)=>  (θ0 + θ1 * x._1 - x._2) * x._1, data))
  }
  
  def diffSquared( a : Tuple2[Double,Double], b : Tuple2[Double,Double]) : Double = {
    (b._1 - a._1)*(b._1 - a._1) + (b._2 - a._2)*(b._2 - a._2)
  }
  
  def iterate(α : Double , data : List[(Double,Double)],σ : Double ) : (Double,Double) = {
	var θ = (0.,0.)
	var oldθ = θ
	var ctr = 0
	do {
	   oldθ = θ
	   ctr += 1
	   θ = θ0θ1(θ._1,θ._2,α,data)
	   println("iter " + ctr + " θ " + θ + " diff " + diffSquared(θ,oldθ))
	}while(diffSquared(θ,oldθ) > σ )
	θ
  }
}

/*
val data : List[(Double,Double)]= List( 
		(800.,180.),
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
*/