package se.ramn.sorting

import org.scalatest.FunSuite


class RadixLSDSortTest extends FunSuite {
  test("sort strings") {
    assertResult(Seq("aa", "ab", "ba", "bb", "cb")) {
      RadixLSDSort(
        Seq("ab", "bb", "aa", "cb", "ba"),
        stringLength=2
        )
    }
  }
}
