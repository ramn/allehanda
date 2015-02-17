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
    var sorted: IndexedSeq[String] = unsorted.toIndexedSeq
    val auxiliary = Array.ofDim[String](unsorted.size)

    for (d <- (stringLength - 1) to 0 by -1) {
      CountSort.unsafeSortUsingAuxiliary(
        sorted,
        radix,
        (s: String) => s.charAt(d).toInt,
        auxiliary
        )
      sorted = auxiliary.toIndexedSeq
    }
    sorted.toIterable
  }
}
