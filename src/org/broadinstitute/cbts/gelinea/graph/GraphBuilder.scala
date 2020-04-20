package org.broadinstitute.cbts.gelinea.graph

import scala.collection.mutable
import java.io.File

object GraphBuilder {

  def apply() = new GraphBuilder(false)
  def apply(directed: Boolean) = new GraphBuilder(directed)

  /** Load network from a file */
  def load(file: String): Graph = {
    return if (file.toLowerCase.endsWith(".sif")) loadSifFile(new File(file)) else load(new File(file))
  }

  private def findSeparator(file: File): String = {
    val source = io.Source.fromFile(file)
    val hasTab = source.filter(_ == '\t').hasNext
    source.close()
    return if (hasTab) "\t" else " "
  }

  /** Load network from a SIF file */
  private def loadSifFile(file: File): Graph = {
    val builder = new GraphBuilder()
    val separator = findSeparator(file)
    val source = io.Source.fromFile(file)

    try {
      val input = source.getLines

      for (line <- input if line.trim.length > 0) {
        val words: Array[String] = line.split(separator)
        val node1name = words(0)
        for (i <- 2 until words.length) {
          val node2name = words(i)
          builder.addEdge(node1name, node2name)
        }
      }
    } finally {
      source.close()
    }
    return builder.getGraph
  }

  /** Load network from a tab-separated text file */
  private def load(file: File): Graph = {
    val builder = new GraphBuilder()

    val input = io.Source.fromFile(file).getLines
    val firstLine: Option[String] = if (input.hasNext) Some(input.next()) else None

    for (line <- input) {
      val words: Array[String] = line.split("\t")
      val node1name = words(0)
      val node2name = words(1)
      builder.addEdge(node1name, node2name)
    }

    return builder.getGraph
  }

  /** Create a subgraph */
  def subGraph(graph: Graph, geneList: Set[String]): Graph = {
    val builder = new GraphBuilder(true)
    for (node <- graph; if geneList contains node.name; neighborIndex <- node) {
      val neighbor = graph.node(neighborIndex)
      if (geneList contains neighbor.name) {
        builder.addEdge(node.name, neighbor.name)
      }
    }
    return builder.getGraph
  }
}


/**
 * Utility class to build graphs
 */
class GraphBuilder(val directed: Boolean) {

  def this() = this(false)

  private val nodes = mutable.Buffer[NodeBuilder]()
  private val nodeIndex = mutable.Map[String, Int]()

  private def getOrCreateNode(name: String): NodeBuilder = {
    if (nodeIndex contains name) {
      return nodes(nodeIndex(name))
    } else {
      val index = nodes.size
      val node = new NodeBuilder(index, name)
      nodes.append(node)
      nodeIndex += (name -> index)
      return node
    }
  }

  /**
   * Add a node to a graph being built
   */
  def addNode(nodeName: String): Unit = {
    getOrCreateNode(nodeName)
  }

  /**
   * Add an edge to a graph being built
   */
  def addEdge(name1: String, name2: String) {
    val node1 = getOrCreateNode(name1)
    val node2 = getOrCreateNode(name2)
    node1 += node2
    if (!directed) node2 += node1
  }

  /**
   * Return a graph created by this builder
   */
  def getGraph: Graph = {
    val nodeArray: Array[Node] = new Array[Node](nodes.size)
    for (i <- 0 to nodes.size - 1) {
      nodeArray(i) = nodes(i).toNode
    }
    return new Graph(nodeArray)
  }

  
  /**
   * Utility class to build nodes
   */
  private class NodeBuilder(val index: Int, val name: String) {

    private val edges = mutable.Set[Int]()

    /**
     * Add an edge to a node being build
     */
    def +=(node: NodeBuilder) {
      edges.add(node.index)
    }

    /**
     * Return a node created by this builder
     */
    def toNode: Node = {
      val edgeArray = edges.toArray[Int].sorted
      return new Node(index, name, edgeArray)
    }

    override def toString = name + "@" + index + ":" + edges
  }
}
