package org.broadinstitute.cbts.gelinea

import org.broadinstitute.cbts.gelinea.graph.Graph
import org.broadinstitute.cbts.gelinea.graph.Node
import java.io.File

object GeneList {

    /** Create a gene list from a sequence of strings */
    def apply(genes: String*) = new GeneList(genes.toList)

    /**
     * load gene list from a file
     */
    def load(file: String): GeneList = load(new File(file))
    
    /**
     * load gene list from a file
     */
    def load(file: File): GeneList = {
      var list: List[String] = io.Source.fromFile(file).getLines.toList
      return new GeneList(list)
    }
}


class GeneList(private val geneList: List[String]) {

  def indexBy(graph: Graph): IndexedGeneList = {
    val indexedListBuffer: Array[Node] = new Array[Node](geneList.size)
    var n = 0;
    for (gene <- geneList){
      graph.node(gene) match {
        case Some(node) => indexedListBuffer(n) = node; n += 1
        case None => 
      }  
    }
    return new IndexedGeneList(indexedListBuffer.slice(0,n))
  }
  
  def asSet: Set[String] = geneList.toSet
}



class IndexedGeneList (private val indexedList: Array[Node]){
  
  
  // mimic sequence interface
  
  def length = indexedList.length

  def apply(index: Int):Node = indexedList(index)
  
  def foreach(f: Node => Unit) {
    indexedList.foreach(f)
  } 

  //def map[T](f: Node => T) = indexedList.map(f)
  
  //def take(newLength: Int): IndexedGeneList = new IndexedGeneList(indexedList take newLength)
  

  /** node at a position */
  def node(index: Int):Node = indexedList(index)
  

  /** Largest degree of a node in this list */
  lazy val maxDegree: Int = {
    var max = 0
      for (node <- indexedList) { 
        max = math.max(max, node.degree) 
      }
    max
  }


  /** Average degree of a node in this list */
//  lazy val aveDegree: Double = {
//    1.0 * (for (node <- this) yield node.degree).sum / indexedList.length
//  }
//  
  
  /** Degree distribution - number of nodes with degree 0, 1, 2, ... */
//  def degreeDistribution: Array[Int] = {
//
//    val counts = new Array[Int](maxDegree+1)
//    for (node <- this) {
//      val degree = node.degree
//      counts(degree) = counts(degree) + 1 
//    }
//    return counts
//  }


  /**
   *  An ordered list of (unique) degrees.
   */
//  def degreeList: Array[Int] = {
//    val degreeDist = this.degreeDistribution
//    var list = List.empty[Int]
//    for (k <- (degreeDist.length - 1) to 0 by -1; if degreeDist(k) > 0){
//      list = k :: list
//    }
//    return list.toArray
//  }


}
