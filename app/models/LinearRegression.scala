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

    val jvals = (1 to iterations).map { iter =>
      var cnt = 0
      val res = thetas.map { t =>
        val jRes = costFunc(thetas, data, cnt)
        val r = t - alpha * jRes
        cnt+=1
        (jRes, r)
      }
      thetas = res.map(_._2)
      res.map(_._1).reduce(_+_)
    }
    (thetas, jvals.map(_(0)))
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

  /*
  def drawGraph(plots: Seq[Float]) = {
    import breeze.linalg._
    import breeze.plot._

    implicit val xv: DomainFunction[Float, Int, Int] = new DomainFunction[Float, Int, Int] {
      def domain(t: Float): IndexedSeq[Int] = 0 until t.toInt
      def apply(t: Float, k: Int): Int = (t+k).toInt
    }

    implicit val xv2: DomainFunction[Int, Int, Int] = new DomainFunction[Int, Int, Int] {
      def domain(t: Int): IndexedSeq[Int] = 0 until t.toInt
      def apply(t: Int, k: Int): Int = (t+k).toInt
    }



    val f = Figure()

    val p = f.subplot(0)
    plots.zipWithIndex.map { case(e, i) =>
      p += plot(i, e, '.')
    }

    f.saveas("theta.png")

  }
    */
}
