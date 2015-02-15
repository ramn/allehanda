package se.ramn.sorting

import annotation.tailrec
import scala.math.Ordering.Implicits._


object QuickSort {
  def apply[T : Ordering](unsorted: Seq[T]): Seq[T] = {
    unsorted match {
      case pivot +: tail =>
        val (lessEq, greater) = tail.partition(_ <= pivot)
        apply(lessEq) ++ Seq(pivot) ++ apply(greater)
      case Seq() => unsorted
    }
  }
}
