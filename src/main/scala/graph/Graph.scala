package se.ramn.graph

import scala.collection.immutable.IndexedSeq


trait Graph {
  def adjacent(vertex: Int): Iterable[Int]
  def degree(vertex: Int): Int
  def edgeCount: Int
  def vertexCount: Int
}


trait GraphBuilder {
  def addEdge(fromVertex: Int, toVertex: Int): Unit
  def build: Graph
}


object GraphBuilder {
  def apply(vertexCount: Int): GraphBuilder = {
    new GraphBuilderImpl(vertexCount)
  }
}


private class GraphBuilderImpl(vertexCount: Int) extends GraphBuilder {
  require(vertexCount >= 0)
  private var edgeCount = 0
  private var adjacent: Array[Set[Int]] =
    Array.tabulate(vertexCount)(_ => Set.empty[Int])

  def addEdge(fromVertex: Int, toVertex: Int): Unit = {
    validateVertex(fromVertex)
    validateVertex(toVertex)
    edgeCount += 1
    adjacent(fromVertex) = adjacent(fromVertex) + toVertex
    adjacent(toVertex) = adjacent(toVertex) + fromVertex
  }

  def build: Graph = {
    new GraphImpl(
      edgeCount=edgeCount,
      vertexCount=vertexCount,
      adjacents=adjacent.toIndexedSeq)
  }

  private def validateVertex(vertex: Int) = {
    if (vertex < 0 || vertex >= vertexCount) {
      throw new IndexOutOfBoundsException(
        s"vertex $vertex is not between 0 and ${(vertexCount - 1)}")
    }
  }
}


private class GraphImpl(
  val edgeCount: Int,
  val vertexCount: Int,
  adjacents: IndexedSeq[Set[Int]]
) extends Graph {

  def adjacent(vertex: Int): Iterable[Int] =
    if (vertexIsWithinBoundaries(vertex)) adjacents(vertex)
    else Set.empty[Int]

  def degree(vertex: Int): Int =
    if (vertexIsWithinBoundaries(vertex)) adjacents(vertex).size
    else 0

  private def vertexIsWithinBoundaries(vertex: Int): Boolean = {
    vertex >= 0 && vertex < vertexCount
  }
}
