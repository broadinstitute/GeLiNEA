package org.broadinstitute.cbts.gelinea.graph

class Node(val index: Int, val name: String, private val edges: Array[Int]) {

  /** Returns the number of connections of this node */
  def degree = edges.length


  /** Support for looping through edges */
  def foreach(f: Int => Unit) {
    edges.foreach(f)
  }


  /** Return true if this node is connected to otherNode */
  def hasEdgeTo(otherNode: Node): Boolean = hasEdge(otherNode.index)


  /** Return true if this node is connected to otherNode */
  def hasEdge(otherNode: Int): Boolean = hasEdge(otherNode, 0, degree - 1)


  /** Find connection via binary search */
  private def hasEdge(node: Int, from: Int, to: Int): Boolean = {
    //binary search
    if (from < to - 1) {
      val mid = (from + to) / 2
      if (node <= edges(mid))
        return hasEdge(node, from, mid)
      else
        return hasEdge(node, mid + 1, to)
    } else {
      return (node == edges(from) || node == edges(to))
    }
  }


  /** String representation of this node */
  override def toString = name + "@" + index + ":" + degree
}
