package calculator

import scala.math._

object Polynomial {

  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {

    def delta(a1: Double, b1: Double,
              c1: Double): Double = {
      b1 * b1 - 4 * a1 * c1
    }
    Signal(delta(a.apply, b.apply, c.apply()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    def solution(a1: Double, b1: Double, c1: Double, delta: Double): Set[Double] = {
      if (delta < 0) Set()
      else if (delta == 0) Set(-b1 / (2 * a1))
      else Set((-b1 + sqrt(delta)) / (2 * a1), (-b1 - sqrt(delta)) / (2 * a1))
    }

    Signal(solution(a.apply(), b.apply(), c.apply(), delta.apply()))
  }
}
