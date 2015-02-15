package se.ramn.sorting

import scala.reflect.ClassTag


object CountSort {
  def apply[T : ClassTag](
    unsorted: Iterable[T],
    radix: Int,
    key: T => Int
  ): Iterable[T] = {
    require(unsorted.size > 0)
    require(radix > 0)

    // Count occurrances of each key
    val counts = Array.fill(radix + 1)(0)
    for (item <- unsorted) {
      val keyForItem = key(item)
      require(keyForItem >= 0)
      counts(keyForItem + 1) += 1
    }

    // Accumulate counts to create offsets for all keys
    for (i <- 0 until radix) {
      counts(i + 1) += counts(i)
    }

    // Put items in the sorted order, in a new array
    val sorted: Array[T] = Array.ofDim(unsorted.size)
    for (item <- unsorted) {
      val keyForItem = key(item)
      val indexWhenSorted = counts(keyForItem)
      sorted(indexWhenSorted) = item
      counts(keyForItem) += 1
    }

    sorted.toIterable
  }

  def apply(unsorted: Iterable[Int], radix: Int): Iterable[Int] = {
    apply(unsorted, radix, (identity[Int] _))
  }

  def apply(unsorted: Iterable[Int]): Iterable[Int] = {
    val radix = unsorted.max + 1
    apply(unsorted, radix, (identity[Int] _))
  }

  def apply[T : ClassTag](unsorted: Iterable[T], key: T => Int): Iterable[T] = {
    val radix = key(unsorted.maxBy(key)) + 1
    apply(unsorted, radix, key)
  }
}
