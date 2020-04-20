package org.broadinstitute.cbts.gelinea.graph

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.broadinstitute.cbts.gelinea.GeneList
import java.io.File

object GraphBuilderTest {

  def testGraph: Graph = {
    val builder = new GraphBuilder()

    builder.addEdge("B", "C")
    builder.addEdge("C", "D")
    builder.addEdge("E", "C")
    builder.addEdge("E", "A")
    builder.addEdge("E", "F")
    builder.addEdge("B", "F")
    builder.addEdge("B", "G")
    builder.addEdge("D", "H")

    return builder.getGraph
  }
}


class GraphBuilderTest extends FlatSpec with Matchers {
  
  import GraphBuilderTest.testGraph

  "GraphBuilder" should "create graph" in {
    val builder = new GraphBuilder()
    builder.addEdge("B", "C")
    val graph = builder.getGraph
    graph should not be (null)
    graph.order should be(2)
    graph.maxDegree should be(1)
    graph.node("A") should be(None)
    graph.node("B") should not be (None)
    graph.node("B").get.hasEdge(graph.node("C").get.index) should be(true)
    graph.node("C").get.hasEdge(graph.node("B").get.index) should be(true)
    graph.node("B").get.hasEdge(graph.node("B").get.index) should be(false)
    info("OK")
  }

  it should "create edge once" in {
    val builder = new GraphBuilder()
    builder.addEdge("B", "C")
    builder.addEdge("B", "C")
    val graph = builder.getGraph
    graph.node("B").get.degree should be(1)
    info("OK")
  }

  it should "read graph from file" in {
    val graph = GraphBuilder.load("test/data/testNetwork.txt")
    info("# nodes = " + graph.order)
    graph.hasEdge("ENSP00000000233", "ENSP00000325002") should be(true)
    graph.hasEdge("ENSP00000000233", "ENSP00000263181") should be(true)
    graph.hasEdge("ENSP00000000233", "ENSP00000322791") should be(true)
    info("OK")
  }

  it should "load graph from sif file" in {
    val graph = GraphBuilder.load("test/data/testNetwork.sif")
    graph.size should be(testGraph.size)
    //graph.order should be(testGraph.order)
    for (node <- testGraph; edge <- node){
      graph.hasEdge(node.name, testGraph.node(edge).name) should be(true)
    }
    info("OK")
  }

  it should "create subgraph" in {
    val graphSrc = testGraph
    val geneList = Set("A", "B", "C", "D")
    val graph = GraphBuilder.subGraph(graphSrc, geneList)
    graph.size should be(2)
    graph.order should be(3)
    graph.hasEdge("D", "C") should be(true)
    graph.hasEdge("B", "C") should be(true)
    graph.hasEdge("A", "E") should be(false)
    graph.hasEdge("D", "B") should be(false)
    graph.hasEdge("E", "F") should be(false)
    info("OK")
  }
}