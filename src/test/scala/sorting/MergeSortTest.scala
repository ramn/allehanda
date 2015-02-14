package se.ramn.sorting

import org.scalatest.FunSuite


class MergeSortTest extends FunSuite {
  test("sorting of small list of ints") {
    assertResult(List(1,2,3,4)) {
      MergeSort(List(3,1,4,2))
    }
  }

  test("sort list of doubles") {
    assertResult(List(-0.1, 0.1, 1.2, 1.3, 2.0, 2.9)) {
      MergeSort(List(2.9, 1.2, -0.1, 1.3, 2.0, 0.1))
    }
  }

  test("accepts array as input") {
    assertResult(Array(-0.1, 0.1, 1.2, 1.3, 2.0, 2.9)) {
      MergeSort(Array(2.9, 1.2, -0.1, 1.3, 2.0, 0.1))
    }
  }

  test("accepts Vector as input") {
    assertResult(Vector(-0.1, 0.1, 1.2, 1.3, 2.0, 2.9)) {
      MergeSort(Vector(2.9, 1.2, -0.1, 1.3, 2.0, 0.1))
    }
  }

  //test("big list") {
    //val xs = util.Random.shuffle(0 to 1000000).toList
    //assertResult(xs.sorted) {
      //MergeSort(xs)
    //}
  //}
}
