package se.ramn.sorting

import org.scalatest.FunSuite


class MergeSortTest extends FunSuite {
  test("sorting of small shuffled list") {
    assertResult(List(1,2,3,4)) {
      MergeSort(List(3,1,4,2))
    }
  }

  //test("big list") {
    //val xs = util.Random.shuffle(0 to 1000000).toList
    //assertResult(xs.sorted) {
      //MergeSort(xs)
    //}
  //}
}
