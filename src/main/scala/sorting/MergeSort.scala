package se.ramn.sorting

import annotation.tailrec
import scala.math.Ordering.Implicits._


object MergeSort {
  def apply[T : Numeric](xs: List[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0)
      xs
    else {
      val (left, right) = xs splitAt (n)
      merge(apply(left), apply(right), Nil)
    }
  }

  @tailrec
  private def merge[T : Numeric](
    xs: List[T],
    ys: List[T],
    res: List[T]
  ): List[T] =
    (xs, ys) match {
      case (Nil, ys) => concatReverse(res, ys)
      case (xs, Nil) => concatReverse(res, xs)
      case (x :: xtail, y :: ytail) =>
        if (x < y) merge(xtail, ys, x :: res)
        else merge(xs, ytail, y :: res)
    }

  @tailrec
  private def concatReverse[T](xs: List[T], ys: List[T]): List[T] = {
    xs match {
      case Nil => ys
      case head :: tail => concatReverse(tail, head +: ys)
    }
  }
}
