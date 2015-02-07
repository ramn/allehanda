package se.ramn.geometry


case class Point(x: Double, y: Double)

case class Line(start: Point, end: Point)

case class Triangle(a: Point, b: Point, c: Point)
