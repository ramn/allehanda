package se.ramn.graph

import org.scalatest.FunSuite


class GraphTest extends FunSuite {
  test("graph api") {
    val builder = GraphBuilder(5)
    builder.addEdge(4, 4)
    builder.addEdge(2, 3)
    builder.addEdge(2, 1)

    val target = builder.build

    assertResult(Set(4)) {
      target.adjacent(4)
    }

    assertResult(Set(3, 1)) {
      target.adjacent(2)
    }

    assertResult(Set(3, 1)) {
      target.adjacent(2)
    }

    assertResult(2) { target.degree(2) }
    assertResult(3) { target.edgeCount }
    assertResult(5) { target.vertexCount }
  }
}
