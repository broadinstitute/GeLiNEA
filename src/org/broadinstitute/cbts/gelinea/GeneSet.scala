package org.broadinstitute.cbts.gelinea

import org.broadinstitute.cbts.gelinea.graph.Node
import org.broadinstitute.cbts.gelinea.graph.Graph
import scala.collection.mutable
import java.io.File

object GeneSet {

  def load(file: String): List[GeneSet] = load(new File(file))

  /** Load gene set from a file*/
  def load(file: File): List[GeneSet] = {
    var list = List[GeneSet]()
    val input = io.Source.fromFile(file).getLines
    for (line <- input) {
      val words: Array[String] = line.split("\t")
      val name = words(0)
      val geneSet: Set[String] = words.slice(2, words.length).toSet
      list = new GeneSet(name, geneSet) :: list
    }
    return list.reverse
  }

  def apply(genes: String*) = new GeneSet(genes.toSet)
}


class GeneSet(val name: String, val geneSet: Set[String]) {

  def this(geneSet: Set[String]) = this("", geneSet)

  def indexBy(graph: Graph): IndexedGeneSet = {
    val set = mutable.Set[Node]()
    for (gene <- geneSet)
      graph.node(gene) match {
        case Some(node) => set += node
        case None       =>
      }
    return new IndexedGeneSet(name, set.toSet)
  }

  override def toString = name + ":" + geneSet
}


class IndexedGeneSet(val name: String, private val geneSet: Set[Node]) {

  val indexSet: Set[Int] = geneSet.map(node => node.index)

  def size = geneSet.size

  def contains(node: Node) = geneSet contains node
  def contains(index: Int) = indexSet contains index
  def ?:(node: Node) = this contains node
  def ?:(index: Int) = this contains index

  def foreach(f: Node => Unit) {
    geneSet.foreach(f)
  }

  def map[B](f: Node => B) = {
    geneSet.map(f)
  }

  /**
   * Return a set of indices of nodes belonging to both this gene set and a gene list
   */
  def intersection(geneList: IndexedGeneList): Set[Int] = {
    var set = Set[Int]()
    for (node <- geneList) if (node ?: this) {
      set += node.index
    }
    return set
  }

  /**
   * Alternative syntax for intersection wit a gene list
   */
  //def /\(geneList: IndexedGeneList) = intersection(geneList)

  /**
   * Average node degree in the gene set
   */
  //def averageDegree: Double = ((0.0 /: geneSet)(_ + _.degree)) / geneSet.size

  override def toString = "IndexedGeneSet[" + name + ":" + size + " genes]"

}
