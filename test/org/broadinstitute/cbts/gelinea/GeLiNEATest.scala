package org.broadinstitute.cbts.gelinea

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.broadinstitute.cbts.gelinea.graph.GraphBuilderTest


class GeLiNATest extends FlatSpec with Matchers {


  "GeneList" should "be indexed" in {
    val geneList = new GeneList(List("X", "A", "B", "C"))
    val graph = GraphBuilderTest.testGraph
    val indexedList = geneList.indexBy(graph)
    indexedList.length should be(3)
    indexedList.node(0).index should be(4)
    indexedList.node(1).index should be(0)
    indexedList.node(2).index should be(1)
    info("OK")
  }


  "connectionCount" should "count connection" in {
    val graph = GraphBuilderTest.testGraph
    val gelina = new GeLiNEA(graph)
    val geneList = (new GeneList(List("X", "A", "B", "C"))).indexBy(graph)
    val geneSet = GeneSet("B", "F", "E") indexBy graph
    gelina.connectionCount(geneList, geneSet) should be(4)

    val geneList1 = GeneList("B", "F", "E") indexBy graph
    gelina.connectionCount(geneList1, geneSet) should be(4)

    val geneList2 = GeneList("D", "H") indexBy graph
    gelina.connectionCount(geneList2, geneSet) should be(0)

    val geneList3 = GeneList("B", "F", "G") indexBy graph
    gelina.connectionCount(geneList3, geneSet) should be(4)
  }


  "degree distribution" should "be computed" in {
    val graph = GraphBuilderTest.testGraph
    graph.degreeDistribution should be(Array[Int](0, 3, 2, 3))
    info("OK")
  }


  "GeLiNA" should "compute distribution" in {
    val geneList = new GeneList(List("X", "A", "B", "C", "D"))
    val graph = GraphBuilderTest.testGraph
    val indexedList = geneList.indexBy(graph)
    val geneSet = new GeneSet(Set("B", "F", "E"))
    val indexedSet = geneSet indexBy graph

    val gelina = new GeLiNEA(graph)
    val genFun = gelina.connectionCountDistribution(indexedList, indexedSet)
    info(genFun.toString)
    val res = Array[Double](0, 0, 1.0 / 18, 4.0 / 18, 5.0 / 18, 4.0 / 18, 4.0 / 18)
    for (i <- 0 to genFun.dim) {
      genFun(i) should be(res(i))
    }
    info("OK")
  }


  "degree binning" should "not change with bin size 2" in {
    val graph = GraphBuilderTest.testGraph
    val gelina = new GeLiNEA(graph,2)
    gelina.degreeBinMap(1) should be (1)
    gelina.degreeBinMap(2) should be (2)
    gelina.degreeBinMap(3) should be (3)
    
  }

  it should "change with bin size 3" in {
    val graph = GraphBuilderTest.testGraph
    val gelina = new GeLiNEA(graph,3)
    gelina.degreeBinMap(1) should be (1)
    gelina.degreeBinMap(2) should be (1)
    gelina.degreeBinMap(3) should be (3)
    
  }

}