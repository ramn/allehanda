package se.ramn.sorting

import org.scalatest.FunSuite
import org.scalatest.FunSpec
//import org.scalameter.api._


class RadixLSDSortTest extends FunSpec {
  describe("RadixLSDSort.apply") {
    it("should sort strings") {
      assertResult(Seq("aa", "ab", "ba", "bb", "cb")) {
        RadixLSDSort(
          Seq("ab", "bb", "aa", "cb", "ba"),
          stringLength=2
        )
      }
    }

    it("should not mutate arguments") {
      val orig = Array("ab", "bb", "aa", "cb", "ba")
      val expected = orig.clone.sorted
      assertResult(expected) {
        RadixLSDSort(orig, stringLength=2)
      }
      assert(
        orig === Array("ab", "bb", "aa", "cb", "ba"),
        "should not mutate arg")
    }
  }

  describe("RadixLSDSort.unsafeSortInplace") {
    it("should sort array inplace") {
      val orig = List("ab", "bb", "aa", "cb", "ba")
      val toSort = orig.toArray
      RadixLSDSort.unsafeSortInplace(
        toSort,
        stringLength=2
      )
      assert(toSort === orig.toArray.sorted)
      assert(toSort.toList === orig.toList.sorted)
      assert(toSort === List("ab", "bb", "aa", "cb", "ba").sorted)
    }
  }
}


//object RadixLSDSortBenchmark extends PerformanceTest.Quickbenchmark {
  //val sizes = Gen.range("size")(4999, 9999, 1000)
  //val ranges = for (size <- sizes) yield 1000 until size
//
  //def sort(r: Range) = {
    //val ary: Array[Int] = r.toArray
    //val original = util.Random.shuffle(ary.toSeq).toArray.map(_.toString)
    //RadixLSDSort(original, stringLength=4)
  //}
//
  //performance of "RadixLSDSort" in {
    //measure method "sort" in {
      //using(ranges) in {
        //r => sort(r)
      //}
    //}
  //}
//}
