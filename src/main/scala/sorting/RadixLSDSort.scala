package se.ramn.sorting

import collection.immutable.IndexedSeq


object RadixLSDSort {
  /*
   * Sorts `unsorted` on the `stringLength` leading characters
   */
  def apply(
    unsorted: Iterable[String],
    stringLength: Int,
    radix: Int = 256
  ): Iterable[String] = {
    val toSort = unsorted.toArray.clone
    unsafeSortInplace(toSort, stringLength, radix)
    toSort.toIndexedSeq
  }

  def unsafeSortInplace(
    toSort: Array[String],
    stringLength: Int,
    radix: Int = 256
  ): Unit = {
    val n = toSort.size
    val auxiliary = Array.ofDim[String](n)
    for (d <- (stringLength - 1) to 0 by -1) {
      CountSort.unsafeSortUsingAuxiliary(
        toSort,
        radix,
        (s: String) => s.charAt(d).toInt,
        auxiliary
        )
      auxiliary.copyToArray(toSort)
    }
  }
}
