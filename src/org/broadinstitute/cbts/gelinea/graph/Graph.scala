package org.broadinstitute.cbts.gelinea.graph

import java.lang.Math.max


/** Immutable graph */
class Graph(private val nodes: Array[Node]) {
  
  private val nodeMap: Map[String, Node] = createNodeMap(this.nodes)

  
  /** The number of nodes in the graph*/
  lazy val order = nodes.length
  
  
  /** The degree of the most-connected node */
  lazy val maxDegree = findMaxDegree
  
  
  /** The number of edges in the graph */
  lazy val size = countEdges
  
  
  /** Loop through all nodes */
  def foreach(f: Node => Unit) {
    nodes.foreach(f)
  } 
  
  /** Map all nodes */
  def map[T](f: Node => T) = nodes.map(f)
  
  /** Filter nodes */
  def withFilter(f: Node => Boolean) = {
    nodes.withFilter(f)
  }
  
  /** Access node by name */
  def node(nodeName: String): Option[Node] = nodeMap.get(nodeName)
  
  
  /** Access node by index */
  def node(nodeIndex: Int) = nodes(nodeIndex)
  

  /** Find an edge between two nodes */
  def hasEdge(nodeName1: String, nodeName2: String): Boolean = {
    node(nodeName1) match {
      case Some(node1) => node(nodeName2) match {
        case Some(node2) => node1 hasEdgeTo node2
        case None => false
      }
      case None => false
    }
  } 
  
  
  /** Degree distribution - number of nodes with degree 0, 1, 2, ... */
  def degreeDistribution: Array[Int] = findDegreeDistribution.clone
  

  /** Find degree distribution - number of nodes with degree 0, 1, 2, ... */
  private lazy val findDegreeDistribution: Array[Int] = {
    val counts = new Array[Int](this.maxDegree+1)
    for (node <- this) {
      val degree = node.degree
      counts(degree) = counts(degree) + 1 
    }
    counts
  }
  
  
  /** Find the degree of the most-connected node */
  private def findMaxDegree: Int = {
    var maxDeg = 0;
    for (node <- nodes) {
        maxDeg = max(maxDeg, node.degree)
    }
    return maxDeg
  }
  
  
  private def createNodeMap(nodes: Array[Node]): Map[String, Node] = {
     var map = Map[String,Node]()
     
     for (node <- nodes) {
       map = map + (node.name -> node)
     }
     
     return map
  }
  
  
  private def countEdges: Int = {
    var count = 0
    for (node <- nodes) {
      count += node.degree
    }
    assert(count % 2 == 0) 
    return count/2
  }
}
