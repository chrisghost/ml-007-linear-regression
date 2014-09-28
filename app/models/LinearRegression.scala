package models

import breeze.linalg._
import breeze.numerics._

case class TData[Float](in: DenseMatrix[Float], out: DenseVector[Float]) {
  def m : Int = in.rows
  def n : Int = in.cols
}

object LinearRegression {

  def resolve(ts: DenseVector[Float], data: TData[Float], alpha: Float, iterations: Int = 1000) = {
    var thetas = ts

    (1 to iterations).map { iter =>
      var cnt = 0
      val res = thetas.map { t =>
        val r = t - alpha * costFunc(thetas, data, cnt)
        cnt+=1
        r
      }
      //println("RES = == ", res)
      thetas = res
    }
    thetas
  }

  def hypothesis(tethas: DenseVector[Float], x: DenseVector[Float]) : Float = tethas.t * x

  def costFunc(tethas: DenseVector[Float], tdata: TData[Float], j: Int) = {
      val sum = (0 to (tdata.m-1)).map { i =>

        //println(s"i = $i")
        //println("call Hypothesis")
        //println(tethas.t)
        //println(tdata.in(i, 0 to tdata.n-1).inner) // i-th line

        //println(hypothesis( tethas, tdata.in(i, 0 to tdata.n-1).inner))

        val lst = (
          hypothesis(tethas, tdata.in(i, 0 to tdata.n-1).inner) - tdata.out(i)
        ) * tdata.in((i, j))
        //println("HYPOTHESIS RESULT IS ----- ", lst)
        lst
      }.reduce(_ + _)
    //println(s"${tdata.m} SUM ==========> $sum * ", (1.0f/tdata.m))

    (1.0f/tdata.m) * sum
  }
}
