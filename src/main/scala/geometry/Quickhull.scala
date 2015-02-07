package se.ramn.geometry

import Geometry.pointIsLeftOfRay
import Geometry.distanceFromLine
import Geometry.pointIsInTriangle


class Quickhull(points: Set[Point]) {
  require(!points.isEmpty)

  lazy val convexHull: Set[Point] = makeConvexHull

  private def makeConvexHull = {
    val minX = points.minBy(_.x)
    val maxX = points.maxBy(_.x)
    val divider = Line(minX, maxX)
    val remainder = (points - minX) - maxX
    val (leftOfDivider, rightOfDivider) =
      remainder.partition(p => pointIsLeftOfRay(p, divider))

    // Keep the remaining points to the *left* of the directed line (ray)
    Set(minX, maxX) ++
    calc(divider, leftOfDivider) ++
    calc(Line(divider.end, divider.start), rightOfDivider)
  }

  private def calc(divider: Line, points: Set[Point]): Set[Point] = {
    if (points.size <= 1) {
      points
    } else {
      val furthestPoint = points.maxBy(p => distanceFromLine(p, divider))
      val toExclude = Triangle(divider.start, divider.end, furthestPoint)
      val fringePoints = points.filterNot(p => pointIsInTriangle(p, toExclude))
      val (sideA, sideB) = fringePoints.partition { p =>
        pointIsLeftOfRay(p, Line(divider.start, furthestPoint))
      }

      // Keep the remaining points to the *left* of the directed line (ray)
      calc(Line(divider.start, furthestPoint), sideA) ++
      calc(Line(furthestPoint, divider.end), sideB) +
      furthestPoint
    }
  }
}
