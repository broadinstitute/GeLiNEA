package org.broadinstitute.cbts.gelinea.genfun

import org.scalatest.FlatSpec
import org.scalatest.Matchers


object GeneratingFunction2DTest {
  
  def findMaxVal(counts: Array[Int], nSel: Int): Int = {
    val sorted = counts.sorted
    var maxVal = 0
    for (i <- counts.length - nSel to counts.length - 1){
      maxVal += sorted(i)
    }
    return maxVal
  }
  
   def dist(counts: Array[Int], nSel: Int): Array[Int] = {
     val maxVal = findMaxVal(counts, nSel)
     val distribution = new Array[Int](maxVal+1)
     computeDist(counts, nSel, distribution, counts.length - 1, 0)
     return distribution
   } 
   
   def computeDist(counts: Array[Int], nSel: Int, distribution: Array[Int], level: Int, sum: Int) {
     if (nSel == 0) {
       distribution(sum) += 1
     }
     else if (0 <= level) {
    	 computeDist(counts, nSel, distribution, level-1, sum)
    	 computeDist(counts, nSel-1, distribution, level-1, sum+counts(level))
     }
   }
   
}


class GeneratingFunction2DTest extends FlatSpec with Matchers {

import GeneratingFunction2D.X
import GeneratingFunction2D.Y
import GeneratingFunction2D.xPower
  
  "maxVal" should "be 17" in {
    val nSel = 5
    val counts = Array[Int](3,0,1,0,4,2,1,3,0,3,1,4,2,1,2)
    GeneratingFunction2DTest.findMaxVal(counts, nSel) should be (17)
    GeneratingFunction2DTest.dist(counts, nSel).toList.sum should be (3003)
    info("OK")
  }
  
  "GeneratingFunction2D" should "match exhaustive search" in {
    val nSel = 5
    var fun = GeneratingFunction2D.one setMaxYDim nSel
    val counts = Array[Int](3,0,1,0,4,2,1,3,0,3,1,4,2,1,2)
    val dist = GeneratingFunction2DTest.dist(counts, nSel)
    
    for (n <- counts){
      fun = fun * (1 + Y * (X^n))
    }
    val res1 = fun % nSel
    info(res1.toString)
    for (i <- 0 to dist.length - 1) {
      res1(i) should be (dist(i))
    }
    info("OK")
  }
  
  
  
  it should "match exhaustive search again" in {
    val nSel = 6
    var fun = GeneratingFunction2D.one setMaxYDim nSel
    val counts = Array[Int](3,0,1,0,4,2,1,3,0,3,1,4,2,1,2,0,3,5,1,2,2,1,0,2,3,4)
    val dist = GeneratingFunction2DTest.dist(counts, nSel)
    
    for (n <- counts){
      fun = fun * (1 + Y * (X^n))
    }
    val res = fun % nSel
    info(res.toString)
    for (i <- 0 to dist.length - 1) {
      res(i) should be (dist(i))
    }
    info("OK")
  }
  
  it should "match exhaustive search and again" in {
    val nSel = 2
    var fun = GeneratingFunction2D.one setMaxYDim nSel
    val counts = Array[Int](1,2,1)
    val dist = GeneratingFunction2DTest.dist(counts, nSel)
    
    for (n <- counts){
      fun = fun * (1 + Y * (X^n))
    }
    val res = fun % nSel
    info(res.toString)
    for (i <- 0 to dist.length - 1) {
      res(i) should be (dist(i))
    }
    info("OK")
  }
  
  it should "have correct max exponent" in {
    val nSel = 5
    var fun = GeneratingFunction2D.one setMaxYDim nSel
    val counts = Array[Int](3,0,1,0,4,2,1,3,0,3,1,4,2,1,2)
    val dist = GeneratingFunction2DTest.dist(counts, nSel)
    
    for (i <- 0 to counts.length - 1){
      fun = fun * (1 + Y * xPower(counts(i)))      
      for (j <- 0 to fun.dimY){
        fun(j, fun.dimX(j)) should not be (0.0)
      }
    }
    val res = fun % nSel
    res.dim should be (dist.length - 1)
    info("OK")
  }


  "1+x^3y" should "have dimX=1" in {
    val fun = 1 + Y * (X^3)
    fun.dimY should be (1)
    fun.dimX(0) should be (0)
    fun.dimX(1) should be (3)
    fun.dimX(2) should be (-1)
    info("OK")
  }
  

  "one" should "be one" in {
    val one = GeneratingFunction2D.one
    one(0,0) should be (1)
    one(1,0) should be (0)
    one(0,1) should be (0)
    one(1,1) should be (0)
    one.dimY should be (0)
    one.toString should be ("1.0")
    info("OK")
  }
  
  "one plus one" should "be two" in {
    val one = GeneratingFunction2D.one
    val two = one + one
    two(0,0) should be (2)
    two(1,0) should be (0)
    two(0,1) should be (0)
    two(1,1) should be (0)
    two.dimY should be (0)
    two.toString should be ("2.0")
    info("OK")
  }

  "one plus 2" should "be three" in {
    val one = GeneratingFunction2D.one
    val three = one + 2
    three(0,0) should be (3)
    three(1,0) should be (0)
    three(0,1) should be (0)
    three(1,1) should be (0)
    three.dimY should be (0)
    one(0,0) should be (1)
    three.toString should be ("3.0")
    info("OK")
  }

  "1 plus two" should "be three" in {
    val one = GeneratingFunction2D.one
    val two = one + one
    val three = 1 + two
    three(0,0) should be (3)
    three(1,0) should be (0)
    three(0,1) should be (0)
    three(1,1) should be (0)
    three.dimY should be (0)
    two(0,0) should be (2)
    three.toString should be ("3.0")
    info("OK")
  }

  
  "X" should "be x" in {
    val x = GeneratingFunction2D.X
    x(0,0) should be (0)
    x(1,0) should be (0)
    x(0,1) should be (1)
    x(1,1) should be (0)
    x.dimY should be (0)
    x.dimX(0) should be (1)
    x.toString should be ("x")
    info("OK")
  }
  
  "Y" should "be y" in {
    val y = GeneratingFunction2D.Y
    y(0,0) should be (0)
    y(1,0) should be (1)
    y(0,1) should be (0)
    y(1,1) should be (0)
    y.dimY should be (1)
    y.dimX(0) should be (-1)
    y.dimX(1) should be (0)
    y.toString should be ("y")
    info("OK")
  }
  
  "X*Y" should "be y*x" in {
    val xy = X * Y
    val yx = Y * X
    xy(0,0) should be (0)
    xy(1,0) should be (0)
    xy(0,1) should be (0)
    xy(1,1) should be (1)
    xy.dimY should be (1)
    xy.dimX(0) should be (-1)
    xy.dimX(1) should be (1)
    xy.toString should be ("xy")
    
    yx(0,0) should be (0)
    yx(1,0) should be (0)
    yx(0,1) should be (0)
    yx(1,1) should be (1)
    yx.dimY should be (1)
    yx.dimX(0) should be (-1)
    yx.dimX(1) should be (1)
    yx.toString should be ("xy")
    info("OK")
  }
  
  "X + 1" should "be 1 + x" in {
    val x = GeneratingFunction2D.X
    val onePlusX = 1 + x
    val xPlus1 = x + 1
    onePlusX(0,0) should be (1)
    onePlusX(1,0) should be (0)
    onePlusX(0,1) should be (1)
    onePlusX(1,1) should be (0)
    onePlusX.dimY should be (0)
    onePlusX.dimX(0) should be (1)
    onePlusX.dimX(1) should be (-1)
    onePlusX.toString should be ("1.0 + x")
    xPlus1(0,0) should be (1)
    xPlus1(1,0) should be (0)
    xPlus1(0,1) should be (1)
    xPlus1(1,1) should be (0)
    xPlus1.dimY should be (0)
    xPlus1.dimX(0) should be (1)
    xPlus1.dimX(1) should be (-1)
    xPlus1.toString should be ("1.0 + x")
    info("OK")
  }

  
  "2*X" should "be 2x" in {
    val x = GeneratingFunction2D.X
    val twoX = 2 * x
    twoX(0,0) should be (0)
    twoX(0,1) should be (2)
    twoX(1,0) should be (0)
    twoX(1,1) should be (0)
    twoX.dimY should be (0)
    twoX.dimX(0) should be (1)
    twoX.toString should be ("2.0x")
    info("OK")
  }
 
  "X * 2" should "be 2x" in {
    val x = GeneratingFunction2D.X
    val twoX = x * 2
    twoX(0,0) should be (0)
    twoX(0,1) should be (2)
    twoX(1,0) should be (0)
    twoX(1,1) should be (0)
    twoX.dimY should be (0)
    twoX.dimX(0) should be (1)
    twoX.toString should be ("2.0x")
    info("OK")
  }
  
  "x*x" should "be xPower(2)" in {
    val x = GeneratingFunction2D.X
    val xx = x * x
    val x2 = GeneratingFunction2D.xPower(2)
    xx.toString should be ("x^2")
    x2.toString should be ("x^2")
    info("OK")
  }
 
  it should "be x^2" in {
    val xx = X * X
    val x2 = X^2
    xx.toString should be ("x^2")
    x2.toString should be ("x^2")
    info("OK")
  }

  
  "(1 + x)(1 + y)" should "be 1 + x + y + xy" in {
    val prod = (1 + X)*(1 + Y)
    prod(0,0) should be (1)
    prod(0,1) should be (1)
    prod(1,0) should be (1)
    prod(1,1) should be (1)
    prod.dimY should be (1)
    prod.dimX(0) should be (1)
    prod.dimX(1) should be (1)
    prod.toString should be ("1.0 + x + y + xy")
    info("OK")
  }
  
    "(1 + x)(1 + x)" should "be 1 + 2x + x^2" in {
    val prod = (1 + X)*(1 + X)
    prod(0,0) should be (1)
    prod(0,1) should be (2)
    prod(0,2) should be (1)
    prod(0,3) should be (0)
    prod.dimY should be (0)
    prod.dimX(0) should be (2)
    prod.dimX(1) should be (-1)
    prod.toString should be ("1.0 + 2.0x + x^2")
    info("OK")
  }
  
  it should "be x^2" in {
    val xx = X * X
    val prod = (1+X)^2
    println(prod)
    xx.toString should be ("x^2")
    prod(0,0) should be (1)
    prod(0,1) should be (2)
    prod(0,2) should be (1)
    prod(0,3) should be (0)
    prod.dimY should be (0)
    prod.dimX(0) should be (2)
    prod.dimX(1) should be (-1)
    prod.toString should be ("1.0 + 2.0x + x^2")
    info("OK")
  }

  
  "(1 + y)(1 + y)" should "be 1 + 2y + y^2" in {
    val prod = (1 + Y)*(1 + Y)
    prod(0,0) should be (1)
    prod(1,0) should be (2)
    prod(2,0) should be (1)
    prod(1,1) should be (0)
    prod.dimY should be (2)
    prod.dimX(0) should be (0)
    prod.dimX(1) should be (0)
    prod.dimX(2) should be (0)
    prod.toString should be ("1.0 + 2.0y + y^2")
    info("OK")
  }
  
  "(1 + y)(1 + y) \\\\ 1" should "be 1 + 2y" in {
    val prod = (1 + Y)*(1 + Y)
    val rest = prod setMaxYDim 1
    rest(0,0) should be (1)
    rest(1,0) should be (2)
    rest(2,0) should be (0)
    rest(1,1) should be (0)
    rest.dimY should be (1)
    rest.dimX(0) should be (0)
    rest.dimX(1) should be (0)
    rest.dimX(2) should be (-1)
    rest.toString should be ("1.0 + 2.0y")
    info("OK")
  }

  
    "setMaxYDim -1" should "remove max dim restriction" in {
    val y = Y setMaxYDim 1
    val gf = (1 + y)^2
    gf(0,0) should be (1.0)
    gf(1,0) should be (2.0)
    gf(2,0) should be (0.0)
    gf.dimY should be (1)
    gf.toString should be ("1.0 + 2.0y")
    
    val prod = (1 + Y) * gf.setMaxYDim(-1)
    prod(0,0) should be (1.0)
    prod(1,0) should be (3.0)
    prod(2,0) should be (2.0)
    prod(3,0) should be (0.0)
    prod.dimY should be (2)
    prod.toString should be ("1.0 + 3.0y + 2.0y^2")
    info("OK")     
  }
  

    "maxCoeff" should "find the largest coefficient" in {
      val polynomial = 1 + X + Y + 2*X*Y + (X^2)
      polynomial.maxCoeff should be (2.0)
      val polynomial1 = 1 + X + Y + 2*X*Y + 5*(X^2)
      polynomial1.maxCoeff should be (5.0)
      val polynomial2 = 1 + X + 7*Y + 2*X*Y + (X^2)
      polynomial2.maxCoeff should be (7.0)
      
      info("OK")
    }
}