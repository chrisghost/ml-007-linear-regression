package controllers

import play.api._
import play.api.mvc._

import breeze.linalg._
import breeze.numerics._

import models._
import models.LinearRegression._

object Application extends Controller {

  def index = Action {
    Redirect("/1000")
  }

  def lr(iters: Int = 1000) = Action {

    val tdt = TData(
      DenseMatrix(
        (1.0f, 1.0f),
        (1.0f, 10.0f)
      ),
      DenseVector(6.0f, 600.0f)
    )

    val thetas = DenseVector(1.0f, 1.0f)
    val (resThetas, thetasEvolution) = LinearRegression.resolve(thetas, tdt, 0.001f, iters)


    Ok(views.html.index(thetasEvolution.toList))
  }

}
