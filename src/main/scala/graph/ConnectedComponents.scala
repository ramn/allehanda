package se.ramn.graph

import scala.annotation.tailrec


trait ConnectedComponents {
  def connected(v: Int, w: Int): Boolean
  def connectedComponentsCount: Int
  def componentId(vertex: Int): Int
  def vertexCountInSameComponentAs(vertex: Int): Int
}


object ConnectedComponents {
  def apply(graph: Graph): ConnectedComponents =
    new ConnectedComponentsImpl(graph)
}


private class ConnectedComponentsImpl(graph: Graph) extends ConnectedComponents {
  // has vertex v been marked?
  private val marked: Array[Boolean] = Array.fill(graph.vertexCount)(false)

  // id(v) = id of connected component containing v
  private val id = Array.fill(graph.vertexCount)(0)

  // componentSize(id) = number of vertices in given component
  private val componentSize = Array.fill(graph.vertexCount)(0)

  private var currentComponentId = 0

  for (vertex <- 0 until graph.vertexCount if !marked(vertex)) {
    depthFirstSearch(vertex)
    currentComponentId += 1
  }

  def connected(v: Int, w: Int): Boolean = id(v) == id(w)

  def connectedComponentsCount: Int = currentComponentId - 1

  def componentId(vertex: Int): Int = id(vertex)

  def vertexCountInSameComponentAs(vertex: Int): Int = componentSize(id(vertex))

  private def depthFirstSearch(vertex: Int): Unit = {
    @tailrec
    def recurse(remainingVertices: List[Int]): Unit = {
      if (!remainingVertices.isEmpty) {
        val vertex = remainingVertices.head
        val tail = remainingVertices.tail
        if (!marked(vertex)) {
          marked(vertex) = true
          id(vertex) = currentComponentId
          componentSize(currentComponentId) += 1
          val neighbours = graph.adjacent(vertex).filterNot(marked).toList
          recurse(neighbours ::: tail)
        } else {
          recurse(tail)
        }
      }
    }
    recurse(List(vertex))
  }
}
