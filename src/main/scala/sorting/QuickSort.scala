package se.ramn.sorting

import scala.annotation.tailrec
import scala.math.Ordering.Implicits._
import scala.util.Random
import scala.reflect.ClassTag


object QuickSort {
  def apply[T : Ordering](unsorted: Seq[T]): Seq[T] = {
    unsorted match {
      case pivot +: tail =>
        val (lessEq, greater) = tail.partition(_ <= pivot)
        apply(lessEq) ++ Seq(pivot) ++ apply(greater)
      case Seq() => unsorted
    }
  }

  def sortInplace[T : Ordering : ClassTag](unsorted: Array[T]): Unit = {
    InplaceQuickSort.sortInplace(unsorted)
  }
}


private object InplaceQuickSort {
  def sortInplace[T : Ordering : ClassTag](unsorted: Array[T]): Unit = {
    shuffle(unsorted)
    sort(unsorted, 0, unsorted.length - 1)
  }

  private def shuffle[T](xs: Array[T]) = {
    val len = xs.length
    for (i <- 0 until len) {
      exchange(xs, i, Random.nextInt(len))
    }
  }

  private def sort[T : Ordering](xs: Array[T], lo: Int, hi: Int): Unit = {
    if (hi > lo) {
      val pivotIx = partition(xs, lo, hi)
      require(pivotIx >= lo && pivotIx <= hi)
      sort(xs, lo, pivotIx - 1)
      sort(xs, pivotIx + 1, hi)
    }
  }

  /*
   * Partition the subarray with smaller items to the left of a pivot, larger
   * to the right. Return pivot index.
   * @param xs entire array to be sorted
   * @param lo lower end subarray delimiter
   * @param hi higher end subarray delimiter
   * @return pivot index
   */
  private def partition[T : Ordering](xs: Array[T], initLo: Int, initHi: Int): Int = {
    val pivot = xs(initLo)
    var lo = initLo
    var hi = initHi
    while (lo < hi) {
      while (xs(lo) < pivot) { lo += 1 }
      while (xs(hi) > pivot) { hi -=1 }
      exchange(xs, lo, hi)
    }
    lo
  }

  private def exchange[T](xs: Array[T], i: Int, j: Int) = {
    val tmp = xs(i)
    xs(i) = xs(j)
    xs(j) = tmp
  }
}
