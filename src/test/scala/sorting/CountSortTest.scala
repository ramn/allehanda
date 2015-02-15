package se.ramn.sorting

import org.scalatest.FunSuite


class CountSortTest extends FunSuite {
  test("sorting of small list of ints") {
    assertResult(List(1, 2, 3, 4)) {
      CountSort(List(3, 1, 4, 2),  radix=5)
    }
  }

  test("sorting of small list of ints, radix autocalculated") {
    assertResult(List(1, 2, 3, 4)) {
      CountSort(List(3, 1, 4, 2))
    }
  }

  test("accepts array as input") {
    assertResult(Array(1, 2, 3, 9)) {
      CountSort(Array(9, 1, 3, 2), radix=10)
    }
  }

  test("accepts Vector as input") {
    assertResult(Vector(1, 2, 3, 9)) {
      CountSort(Vector(9, 1, 3, 2), radix=10)
    }
  }

  test("accepts double as element") {
    assertResult(Seq(1, 2, 3, 9)) {
      CountSort(Seq(9, 1, 3, 2), radix=10)
    }
  }

  test("sort list of doubles") {
    // 2.9 before 2.0 since we truncate key to an integer and sort is stable
    assertResult(List(0.1, 1.2, 1.3, 2.9, 2.0)) {
      CountSort(List(2.9, 1.2, 1.3, 2.0, 0.1), radix=9, key=(x: Double) => x.toInt)
    }
  }

  test("sort list of doubles, radix autocalculated") {
    assertResult(List(0.1, 1.2, 1.3, 2.9, 2.0)) {
      CountSort(List(2.9, 1.2, 1.3, 2.0, 0.1), (_: Double).toInt)
    }
  }

  //test("big list") {
    //val xs = util.Random.shuffle(0 to 1000000).toList
    //assertResult(xs.sorted) {
      //CountSort(xs)
    //}
  //}
}
