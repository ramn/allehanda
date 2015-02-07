package se.ramn.geometry

import org.scalatest.FunSuite


class QuickhullTest extends FunSuite {
  test("100 points input") {
    val fname = "convexhull/convex_hull_input.txt"
    val points = io.Source
      .fromURL(getClass.getClassLoader.getResource(fname))
      .getLines
      .drop(1)
      .map { line =>
        val Array(a, b) = line.split(" ").map(_.toDouble)
        Point(a, b)
      }
      .toSet

    val expected = Set(
      Point(29.0,682.0),
      Point(5.0,341.0),
      Point(224.0,13.0),
      Point(950.0,991.0),
      Point(143.0,995.0),
      Point(917.0,26.0),
      Point(977.0,285.0),
      Point(8.0,555.0),
      Point(992.0,351.0),
      Point(68.0,80.0))
    val actual = (new Quickhull(points)).convexHull
    assert(actual === expected)
  }
}
