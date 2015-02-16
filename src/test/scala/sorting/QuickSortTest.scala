package se.ramn.sorting

import org.scalatest.FunSuite
import org.scalatest.FunSpec


class FunctionalQuickSortTest extends FunSuite {
  test("sorting of very small list of ints") {
    assertResult(List(2)) {
      QuickSort(List(2))
    }
  }

  test("sorting of small list of ints") {
    assertResult(List(1,2,3,4)) {
      QuickSort(List(3,1,4,2))
    }
  }

  test("sort list of doubles") {
    assertResult(List(-0.1, 0.1, 1.2, 1.3, 2.0, 2.9)) {
      QuickSort(List(2.9, 1.2, -0.1, 1.3, 2.0, 0.1))
    }
  }

  test("accepts array as input") {
    assertResult(Array(-0.1, 0.1, 1.2, 1.3, 2.0, 2.9)) {
      QuickSort(Array(2.9, 1.2, -0.1, 1.3, 2.0, 0.1))
    }
  }

  test("accepts Vector as input") {
    assertResult(Vector(-0.1, 0.1, 1.2, 1.3, 2.0, 2.9)) {
      QuickSort(Vector(2.9, 1.2, -0.1, 1.3, 2.0, 0.1))
    }
  }

  test("sort strings") {
    assertResult(Seq("a", "aa", "ab", "c")) {
      QuickSort(Seq("ab", "c", "aa", "a"))
    }
  }

  //test("big list") {
    //val xs = util.Random.shuffle(0 to 1000000).toList
    //assertResult(xs.sorted) {
      //QuickSort(xs)
    //}
  //}
}


class QuickSortInplaceTest extends FunSpec {
  describe("In place Quicksort") {
    it("should sort array of doubles") {
      val original = Array(2.9, 1.2, -0.1, 1.3, 2.0, 0.1)
      QuickSort.sortInplace(original)
      assert(original === Array(-0.1, 0.1, 1.2, 1.3, 2.0, 2.9))
    }

    it("should sort tricky list") {
      val original = List(9, 0, 3, 7, 6, 10, 2, 5, 4, 1, 8).toArray
      QuickSort.sortInplace(original)
      assert(original === original.sorted)
    }

    it("should sort big list") {
      val original = util.Random.shuffle(0 to 10000).toArray
      QuickSort.sortInplace(original)
      assert(original === original.sorted)
    }
  }
}
