package org.broadinstitute.cbts.gelinea

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.broadinstitute.cbts.gelinea.graph.GraphBuilderTest
import org.broadinstitute.cbts.gelinea.graph.GraphBuilder


class GeneListTest extends FlatSpec with Matchers {

    "GeneList" should "be indexed" in {
    	val geneList = new GeneList(List("X","A","B","C"))
    	val graph = GraphBuilderTest.testGraph
    	val indexedList = geneList.indexBy(graph)
    	indexedList.length should be (3)
    	indexedList.node(0).index should be (4)
    	indexedList.node(1).index should be (0)
    	indexedList.node(2).index should be (1)
    	info("OK")
    }
   

    it should "be loaded from a file" in {
    	val inputList = GeneList.load("test/data/testGeneList.txt")
    	val graph = GraphBuilder.load("test/data/testNetwork.txt")
    	val geneList = inputList indexBy graph
    	info(geneList.toString())
    	geneList.length should be (4)
    	geneList(0).name should be ("ENSP00000000233")
    	geneList(3).name should be ("ENSP00000006101")
    	info("OK")
    }


    "max degree" should "be 3" in {
    	val geneList = new GeneList(List("X","A","B","C"))
    	val graph = GraphBuilderTest.testGraph
    	val indexedList = geneList.indexBy(graph)
    	indexedList.maxDegree should be (3)
    	info("OK")
    }
    
   
}