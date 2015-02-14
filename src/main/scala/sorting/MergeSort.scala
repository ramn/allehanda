package se.ramn.sorting

import annotation.tailrec


object MergeSort {
  def apply(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0)
      xs
    else {
      val (left, right) = xs splitAt (n)
      merge(apply(left), apply(right), Nil)
    }
  }

  @tailrec
  private def merge(xs: List[Int], ys: List[Int], res: List[Int]): List[Int] =
    (xs, ys) match {
      case (Nil, ys) => concatReverse(res, ys)
      case (xs, Nil) => concatReverse(res, xs)
      case (x :: xtail, y :: ytail) =>
        if (x < y) merge(xtail, ys, x :: res)
        else merge(xs, ytail, y :: res)
    }

  @tailrec
  private def concatReverse(xs: List[Int], ys: List[Int]): List[Int] = {
    xs match {
      case Nil => ys
      case head :: tail => concatReverse(tail, head +: ys)
    }
  }
}
