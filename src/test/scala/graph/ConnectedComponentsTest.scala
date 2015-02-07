package se.ramn.graph

import org.scalatest.FunSuite


class ConnectedComponentsTest extends FunSuite {
  test("api") {
    val builder = GraphBuilder(5)
    builder.addEdge(4, 4)
    builder.addEdge(2, 3)
    builder.addEdge(2, 1)
    val graph = builder.build

    val target = ConnectedComponents(graph)

    assertResult(false) { target.connected(2, 4) }
    assertResult(true) { target.connected(2, 3) }

    assertResult(3) { target.vertexCountInSameComponentAs(1) }
    assertResult(3) { target.vertexCountInSameComponentAs(2) }
    assertResult(3) { target.vertexCountInSameComponentAs(3) }
    assertResult(1) { target.vertexCountInSameComponentAs(4) }
    assertResult(2) { target.connectedComponentsCount }

    assertResult(false) {
      target.componentId(4) == target.componentId(2)
    }

    assertResult(false) {
      target.componentId(4) == target.componentId(1)
    }

    assertResult(false) {
      target.componentId(4) == target.componentId(3)
    }

    assertResult(true) {
      target.componentId(2) == target.componentId(3)
    }

    assertResult(true) {
      target.componentId(2) == target.componentId(1)
    }
  }

  test("connected components count") {
    val builder = GraphBuilder(5)
    builder.addEdge(4, 4)
    builder.addEdge(2, 3)
    builder.addEdge(2, 1)
    builder.addEdge(3, 1)
    val graph = builder.build

    val target = ConnectedComponents(graph)

    assertResult(2) { target.connectedComponentsCount }

    assertResult(3) { target.vertexCountInSameComponentAs(1) }
    assertResult(3) { target.vertexCountInSameComponentAs(2) }
    assertResult(3) { target.vertexCountInSameComponentAs(3) }
    assertResult(1) { target.vertexCountInSameComponentAs(4) }
  }
}
