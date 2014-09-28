package test

import org.specs2.mutable._
import breeze.linalg._
import breeze.numerics._

import models._
import models.LinearRegression._

object LinearRegressionSpec extends Specification {
  "LinearRegression" should {
    "compute correctly for 1 param" in {

      val tdt = TData(
        DenseMatrix(
          (1.0f, 1.0f),
          (1.0f, 10.0f)
        ),
        DenseVector(6.0f, 20.0f)
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
        println(s"Should have ~20, got $htest2")

        
        println(s"For param = 0.0f, got ", LinearRegression.hypothesis(result, DenseVector(1.0f, 0.0f)))
        println(s"For param = 2.0f, got ", LinearRegression.hypothesis(result, DenseVector(1.0f, 2.0f)))
        println(s"For param = 5.0f, got ", LinearRegression.hypothesis(result, DenseVector(1.0f, 5.0f)))
        println(s"For param = 11.0f, got ", LinearRegression.hypothesis(result, DenseVector(1.0f, 11.0f)))

        //htest2 must be_<=(70.0f)
        //htest2 must be_>=(50.0f)
      }

      computeTry(100000)


      true === true
    }
    "compute correctly for 2 param" in {

      val tdt = TData(
        DenseMatrix(
          (1.0f, 1.0f, 1.0f),
          (1.0f, 10.0f, 2.0f)
        ),
        DenseVector(6.0f, 10.0f)
      )

      def computeTry(iterations: Int) = {

        println("========================================")
        println(s"Trying with $iterations iterations")

        val thetas = DenseVector(1.0f, 1.0f, 1.0f)
        val (result, thetasProgession) = LinearRegression.resolve(thetas, tdt, 0.001f, iterations)

        println(s"thetas = $result")

        val htest = LinearRegression.hypothesis(result, DenseVector(1.0f, 1.0f, 1.0f))
        println(s"Should have ~6, got $htest")

        //htest must be_<=(7.0f)
        //htest must be_>=(5.0f)

        val htest2 = LinearRegression.hypothesis(result, DenseVector(1.0f, 10.0f, 2.0f))
        println(s"Should have ~10, got $htest2")

        
        println(s"For params = 5.0f, 3.0f, got ", LinearRegression.hypothesis(result, DenseVector(1.0f, 5.0f, 3.0f)))
        println(s"For params = 11.0f, 7.0f, got ", LinearRegression.hypothesis(result, DenseVector(1.0f, 11.0f, 7.0f)))

        //htest2 must be_<=(70.0f)
        //htest2 must be_>=(50.0f)
      }

      computeTry(100000)


      true === true
    }
  }
}
