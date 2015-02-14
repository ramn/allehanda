package se.ramn.sorting

import annotation.tailrec
import scala.math.Ordering.Implicits._


object MergeSort {
  def apply[T : Numeric](xs: Seq[T]): Seq[T] = {
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
    xs: Seq[T],
    ys: Seq[T],
    res: Seq[T]
  ): Seq[T] =
    (xs, ys) match {
      case (Nil, ys) => concatReverse(res, ys)
      case (xs, Nil) => concatReverse(res, xs)
      case (x +: xtail, y +: ytail) =>
        if (x < y) merge(xtail, ys, x +: res)
        else merge(xs, ytail, y +: res)
    }

  @tailrec
  private def concatReverse[T](xs: Seq[T], ys: Seq[T]): Seq[T] = {
    xs match {
      case Nil => ys
      case head +: tail => concatReverse(tail, head +: ys)
    }
  }
}
