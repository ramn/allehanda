package se.ramn.geometry

import math.sqrt
import math.abs
import math.pow
import java.awt.geom.Line2D


object Geometry {
  /*
   * @param p point whose location is to be determined
   * @param ray a directed line
   * @return true if p is left of ray, false otherwise
   */
  def pointIsLeftOfRay(p: Point, ray: Line): Boolean = {
    (p.y - ray.start.y) * (ray.end.x - ray.start.x) >
    (p.x - ray.start.x) * (ray.end.y - ray.start.y)
  }

  /*
   * @param p point
   * @param triangle
   * @return true if p is inside triangle, false otherwise
   */
  def pointIsInTriangle(p: Point, triangle: Triangle): Boolean = {
    val Triangle(a, b, c) = triangle
    val ABC = abs(a.x * (b.y - c.y) + b.x * (c.y - a.y) + c.x * (a.y - b.y))
    val ABP = abs(a.x * (b.y - p.y) + b.x * (p.y - a.y) + p.x * (a.y - b.y))
    val APC = abs(a.x * (p.y - c.y) + p.x * (c.y - a.y) + c.x * (a.y - p.y))
    val PBC = abs(p.x * (b.y - c.y) + b.x * (c.y - p.y) + c.x * (p.y - b.y))
    val isInTriangle = ABP + APC + PBC == ABC
    isInTriangle
  }

  /*
   * @param point
   * @param line a line
   * @return how far point is from line
   */
  def distanceFromLine(point: Point, line: Line): Double = {
    Line2D.ptLineDist(
      line.start.x, line.start.y,
      line.end.x, line.end.y,
      point.x, point.y
    )
  }

  def distance(p1: Point, p2: Point): Double = {
    val deltaX = p2.x - p1.x
    val deltaY = p2.y - p1.y
    sqrt(pow(deltaX, 2) + pow(deltaY, 2))
  }
}
