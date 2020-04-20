package org.broadinstitute.cbts.gelinea.graph

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.broadinstitute.cbts.gelinea.GeLiNEA

class DegreeBinMapTest extends FlatSpec with Matchers {

  "degree binning" should "not change with bin size 2" in {
    val graph = GraphBuilderTest.testGraph
    println(graph.degreeDistribution.toList)
    val gelina = new GeLiNEA(graph, 2)
    gelina.degreeBinMap(1) should be(1)
    gelina.degreeBinMap(2) should be(2)
    gelina.degreeBinMap(3) should be(3)
    info("OK")
  }


  it should "change with bin size 3" in {
    val graph = GraphBuilderTest.testGraph
    val gelina = new GeLiNEA(graph, 3)
    gelina.degreeBinMap(1) should be(1)
    gelina.degreeBinMap(2) should be(1)
    gelina.degreeBinMap(3) should be(3)
    info("OK")
  }
  

  "DegreeBinMap" should "map degrees bin size 1" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 1)
    degreeMap(3) should be(3)
    degreeMap(9) should be(8)
    degreeMap(15) should be(15)
    info("OK")
  }

  it should "map degrees bin size 2" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 2)
    degreeMap(3) should be(3)
    degreeMap(7) should be(7)
    degreeMap(8) should be(7)
    degreeMap(9) should be(7)
    degreeMap(10) should be(10)
    degreeMap(15) should be(12)
    info(degreeMap.toString)
    info("OK")
  }

  it should "map degrees bin size 3" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 3)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 11, 11, 11, 11, 11)")
    info("OK")
  }

  it should "map degrees bin size 4" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 4)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 1, 2, 2, 4, 5, 6, 7, 7, 7, 7, 11, 11, 11, 11, 11)")
    info("OK")
  }

  it should "map degrees bin size 5" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 5)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 1, 2, 3, 3, 5, 6, 7, 7, 7, 7, 11, 11, 11, 11, 11)")
    info("OK")
  }

  it should "map degrees bin size 6" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 6)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 1, 2, 3, 3, 5, 6, 7, 7, 7, 7, 11, 11, 11, 11, 11)")
    info("OK")
  }

  it should "map degrees bin size 7" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 7)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 1, 2, 3, 3, 5, 6, 6, 6, 6, 10, 10, 10, 10, 10, 10)")
    info("OK")
  }

  it should "map degrees bin size 8" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 8)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 1, 2, 2, 2, 5, 6, 6, 6, 6, 10, 10, 10, 10, 10, 10)")
    info("OK")
  }

  it should "map degrees bin size 9" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 9)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 1, 2, 2, 2, 5, 6, 6, 8, 8, 8, 8, 8, 8, 8, 8)")
    info("OK")
  }

  it should "map degrees bin size 10" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 10)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 1, 2, 2, 2, 5, 5, 7, 7, 7, 7, 7, 7, 7, 7, 7)")
    info("OK")
  }

  it should "map degrees bin size 11" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 11)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 1, 2, 2, 2, 5, 5, 7, 7, 7, 7, 7, 7, 7, 7, 7)")
    info("OK")
  }

  it should "map degrees bin size 12" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 12)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 1, 2, 2, 2, 5, 5, 7, 7, 7, 7, 7, 7, 7, 7, 7)")
    info("OK")
  }

  it should "map degrees bin size 13" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 13)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 2, 2, 2, 4, 4, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6)")
    info("OK")
  }

  it should "map degrees bin size 14" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 14)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 1, 1, 1, 4, 4, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6)")
    info("OK")
  }

  it should "map degrees bin size 16" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 16)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 1, 1, 1, 4, 4, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6)")
    info("OK")
  }

  it should "map degrees bin size 17" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 17)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 1, 1, 3, 3, 3, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6)")
    info("OK")
  }

  it should "map degrees bin size 19" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 19)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 1, 1, 3, 3, 3, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6)")
    info("OK")
  }

  it should "map degrees bin size 20" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 20)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 1, 1, 1, 1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)")
    info("OK")
  }

  it should "map degrees bin size 29" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 29)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 1, 1, 1, 1, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)")
    info("OK")
  }

  it should "map degrees bin size 30" in {
    val degrees = Array(0, 12, 10, 3, 4, 12, 7, 3, 1, 0, 2, 4, 1, 0, 0, 1)
    val degreeMap = new DegreeBinMap(degrees, 30)
    info(degreeMap.toString)
    degreeMap.toString should be("DegreeBinMap(0, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)")
    info("OK")
  }

}