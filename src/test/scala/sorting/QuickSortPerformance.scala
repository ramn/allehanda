package se.ramn.sorting

import org.scalameter.api._


object RangeBenchmark extends PerformanceTest.Quickbenchmark {
  val sizes = Gen.range("size")(1000, 10000, 1000)

  val ranges = for {
    size <- sizes
  } yield 0 until size

  def sort(r: Range) = {
    val ary: Array[Int] = r.toArray
    val original = util.Random.shuffle(ary.toSeq).toArray
    QuickSort.sortInplace(original)
  }

  performance of "InplaceQuickSort" in {
    measure method "sort" in {
      using(ranges) in {
        r => sort(r)
      }
    }
  }
}
