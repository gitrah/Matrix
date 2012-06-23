package com.hartenbower.matrix

object CollaborativeFiltering {
	/**
	 * @param x : inputs (num samples x num features)
	 * @param theta : params (num users x num features)
	 * @param y	: outputs (num samples x num users)
	 * @param r : activations (num samples x num users)
	 * @param lambda : regularization
	 * @return
	 */
	def cost(x: MatrixD, theta: MatrixD, y : MatrixD, r : MatrixD, lambda : Double ) : Double = {
    .5 * (( (( x * theta.tN - y) ** r)^2 ).sum + (x.sumSqrsDc() + theta.sumSqrsDc()) * lambda)
	}
	def grad(x:MatrixD, theta: MatrixD, y: MatrixD, r : MatrixD, lambda : Double) : (MatrixD, MatrixD) = {
	  val x_grad = ( x * theta.tN - y) ** r * theta + lambda * x
	  val theta_grad = ((theta * x.tN- y.tN) ** r.tN) * x +  lambda * theta
	  (x_grad,theta_grad)
	}
	
	def checkCost(lambda : Double) {
	  val x_t = MatrixD.randn(4,3)
	  val theta_t = MatrixD.randn(5,3)
	  val y = (x_t * theta_t.tN()).filterElements( x => if (x > .5d) 0 else x)
	  val r = y.filterElements( x=> if(x != 0) 1d else 0)
	  // TODO
	}
}