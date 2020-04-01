package org.broadinstitute.cbts.gelinea.graph

import org.scalatest.FlatSpec
import org.scalatest.Matchers

object NodeTest {


}

// to run use:
//  org.scalatest.tools.Runner -oWD -s org.broadinstitute.CSofT.ccbr.gelinea.graph.NodeTest
class NodeTest extends FlatSpec with Matchers {

    "Degree 1 node" should "have connection" in {
    	val edges = Array[Int](1)
    	val node:Node = new Node(0, "test", edges)
    	node.hasEdge(0) should be (false)
    	node.hasEdge(1) should be (true)
    	node.hasEdge(2) should be (false)
    	node.hasEdge(3) should be (false)
    	info("OK")
    }
    
    "Degree 2 node" should "have connection" in {
    	val edges = Array[Int](1,2)
    	val node:Node = new Node(0, "test", edges)
    	node.hasEdge(0) should be (false)
    	node.hasEdge(1) should be (true)
    	node.hasEdge(2) should be (true)
    	node.hasEdge(3) should be (false)
    	info("OK")
    }
    
    "Degree 3 node" should "have connection" in {
    	val edges = Array[Int](1,2,4)
    	val node:Node = new Node(0, "test", edges)
    	node.hasEdge(0) should be (false)
    	node.hasEdge(1) should be (true)
    	node.hasEdge(2) should be (true)
    	node.hasEdge(3) should be (false)
    	node.hasEdge(4) should be (true)
    	node.hasEdge(5) should be (false)
    	info("OK")
    }
    
    "Degree 4 node" should "have connection" in {
    	val edges = Array[Int](1,2,4,7)
    	val node:Node = new Node(0, "test", edges)
    	node.hasEdge(0) should be (false)
    	node.hasEdge(1) should be (true)
    	node.hasEdge(2) should be (true)
    	node.hasEdge(3) should be (false)
    	node.hasEdge(4) should be (true)
    	node.hasEdge(5) should be (false)
    	node.hasEdge(6) should be (false)
    	node.hasEdge(7) should be (true)
    	node.hasEdge(8) should be (false)
    	info("OK")
    }
    
}