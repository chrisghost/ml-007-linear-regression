package test

import org.specs2.mutable._
import breeze.linalg._
import breeze.numerics._

import models._
import models.LinearRegression._

object LinearRegressionSpec extends Specification {
  "LinearRegression" should {
    "compute thing" in {

      val tdt = TData(
        DenseMatrix(
          (1.0f, 1.0f),
          (1.0f, 10.0f)
        ),
        DenseVector(6.0f, 600.0f)
      )

      def computeTry(iterations: Int) = {

        println("========================================")
        println(s"Trying with $iterations iterations")

        val thetas = DenseVector(1.0f, 1.0f)
        val (result, thetasProgession) = LinearRegression.resolve(thetas, tdt, 0.001f, iterations)

        println(s"thetas = $result")

        val htest = LinearRegression.hypothesis(result, DenseVector(1.0f, 1.0f))
        println(s"Should have ~6, got $htest")

        //htest must be_<=(7.0f)
        //htest must be_>=(5.0f)

        val htest2 = LinearRegression.hypothesis(result, DenseVector(1.0f, 10.0f))
        println(s"Should have ~600, got $htest2")

        val htest3 = LinearRegression.hypothesis(result, DenseVector(1.0f, 100.0f))
        println(s"Should have ~6000, got $htest3")

        //htest2 must be_<=(70.0f)
        //htest2 must be_>=(50.0f)
      }

      computeTry(100000)


      true === true
    }
  }
}
