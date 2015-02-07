package se.ramn.geometry

import org.scalatest.FunSuite
import Geometry._


class GeometryTest extends FunSuite {
  test("point is left of ray going up") {
    assert(
      !pointIsLeftOfRay(
        Point(1, 1),
        Line(
          Point(0, 0),
          Point(0, 10)
          )))
    assert(
      pointIsLeftOfRay(
        Point(-1, 1),
        Line(
          Point(0, 0),
          Point(0, 10)
          )))
    assert(
      pointIsLeftOfRay(
        Point(-1, -1),
        Line(
          Point(0, 0),
          Point(0, 10)
          )))
  }

  test("point is left of ray going left") {
    assert(
      pointIsLeftOfRay(
        Point(1, -1),
        Line(
          Point(10, 0),
          Point(0, 0)
          )))
    assert(
      !pointIsLeftOfRay(
        Point(1, 1),
        Line(
          Point(10, 0),
          Point(0, 0)
          )))
  }

  test("point is left of ray going right") {
    assert(
      pointIsLeftOfRay(
        Point(1, 1),
        Line(
          Point(0, 0),
          Point(10, 0)
          )))
    assert(
      !pointIsLeftOfRay(
        Point(1, -1),
        Line(
          Point(0, 0),
          Point(10, 0)
          )))
  }

  test("point is left of ray going down") {
    assert(
      !pointIsLeftOfRay(
        Point(-1, 1),
        Line(
          Point(0, 10),
          Point(0, 0)
          )))
    assert(
      pointIsLeftOfRay(
        Point(1, 1),
        Line(
          Point(0, 10),
          Point(0, 0)
          )))
    assert(
      pointIsLeftOfRay(
        Point(1, -1),
        Line(
          Point(0, 10),
          Point(0, 0)
          )))
  }

  test("point is left of ray going at an angle") {
    assert(
      pointIsLeftOfRay(
        Point(1, 6),
        Line(
          Point(0, 0),
          Point(10, 10)
          )))
    assert(
      !pointIsLeftOfRay(
        Point(8, 3),
        Line(
          Point(0, 0),
          Point(10, 10)
          )))
  }


  test("point is in triangle") {
    assert(
      pointIsInTriangle(
        Point(x=5, y=2),
        Triangle(
          Point(x=1, y=1),
          Point(x=10, y=1),
          Point(x=5, y=10)
        )))
  }

  test("point is not in triangle") {
    assert(
      !pointIsInTriangle(
        Point(x=11, y=1),
        Triangle(
          Point(x=1, y=1),
          Point(x=10, y=1),
          Point(x=5, y=10)
        )))
  }

  test("distance from line") {
    assert(
      1.0 === distanceFromLine(
        Point(5, 1),
        Line(Point(1, 0), Point(10, 0)))
      )
  }
}
