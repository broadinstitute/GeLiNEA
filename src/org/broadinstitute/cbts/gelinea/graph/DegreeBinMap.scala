package org.broadinstitute.cbts.gelinea.graph

import org.broadinstitute.cbts.gelinea.IndexedGeneList

object DegreeBinMap {

  def apply(graph: Graph, minBinSize: Int) = new DegreeBinMap(graph.degreeDistribution, minBinSize)
  def apply(graph: Graph) = new DegreeBinMap(graph.degreeDistribution, 1)

}

/** Map for binning node degrees to node degree bins containing at least minBinSize nodes*/
class DegreeBinMap(degreeDistribution: Array[Int], val minBinSize: Int) {


  private val degreeMap: Array[Int] = {
    val maxDegree = degreeDistribution.length - 1
    val binMap = new Array[Int](maxDegree + 1)
    var collected = 0
    var last = maxDegree
    for (i <- maxDegree to 1 by -1) {
      collected += degreeDistribution(i)
      if (collected >= minBinSize) {
        for (j <- i to last) {
          if (binMap(j) == 0) binMap(j) = i
        }
        last = i - 1
        collected = 0
      }
    }
    for (i <- 1 to last) {
      binMap(i) = last + 1
    }
    binMap
  }

  def apply(degree: Int) = degreeMap(degree)

  def size = degreeMap.length


  /**
   * The number of nodes in degree bins
   */
  def apply(geneList: IndexedGeneList): Array[Int] = {

    val counts = new Array[Int](geneList.maxDegree + 1)
    for (node <- geneList) {
      val degreeBin = degreeMap(node.degree)
      counts(degreeBin) = counts(degreeBin) + 1
    }
    return counts
  }


  /**
   *  An ordered list of (unique) degrees.
   */
  def degreeList(geneList: IndexedGeneList): Array[Int] = {
    val degreeDist = this(geneList)
    var list = List.empty[Int]
    for (k <- (degreeDist.length - 1) to 0 by -1; if degreeDist(k) > 0) {
      list = k :: list
    }
    return list.toArray
  }


  /**
   *  All nodes of the network arranged by node-degree bins
   */
  def nodesByDegreeBin(graph: Graph): Array[Array[Node]] = {
    val collect = Array.fill(graph.maxDegree + 1) { List[Node]() }
    for (node <- graph) {
      val degreeBin = degreeMap(node.degree)
      collect(degreeBin) = node :: collect(degreeBin)
    }
    val array = new Array[Array[Node]](collect.length)
    for (i <- 0 to array.length - 1) {
      array(i) = collect(i).reverse.toArray
    }
    array
  }

  override def toString = "DegreeBinMap" + degreeMap.toList.toString.substring(4)
}
