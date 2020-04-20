package org.broadinstitute.cbts.gelinea.genfun

import org.scalatest.FlatSpec
import org.scalatest.Matchers


class GeneratingFunctionTest extends FlatSpec with Matchers {

  import GeneratingFunction.X
  
  def infog(g: GeneratingFunction) { info(g.toString)}
 
  "one" should "be one" in {
    val one = GeneratingFunction.one
    one(0) should be (1)
    one(1) should be (0)
    one.dim should be (0)
    one.toString should be ("1.0")
    info("OK")
  }
  
  "one plus one" should "be two" in {
    val one = GeneratingFunction.one
    val two = one + one
    two(0) should be (2.0)
    two(1) should be (0)
    two.dim should be (0)
    two.toString should be ("2.0")
    info("OK")
  }

  "one plus 2" should "be three" in {
    val one = GeneratingFunction.one
    val three = one + 2
    three(0) should be (3)
    three(1) should be (0)
    three.dim should be (0)
    one(0) should be (1)
    three.toString should be ("3.0")
    info("OK")
  }

  "1 plus two" should "be three" in {
    val one = GeneratingFunction.one
    val two = one + one
    val three = 1 + two
    three(0) should be (3)
    three(1) should be (0)
    three.dim should be (0)
    one(0) should be (1)
    three.toString should be ("3.0")
    info("OK")
  }

  
  "X" should "be x" in {
    val x = GeneratingFunction.X
    x(0) should be (0)
    x(1) should be (1.0)
    x.dim should be (1)
    x.toString should be ("x")
    info("OK")
  }
  
  "X + 1" should "be 1 + x" in {
    val x = GeneratingFunction.X
    val onePlusX = 1 + x
    val xPlus1 = x + 1
    onePlusX(0) should be (1.0)
    onePlusX(1) should be (1.0)
    onePlusX(2) should be (0)
    onePlusX.dim should be (1)
    onePlusX.toString should be ("1.0 + x")
    xPlus1(0) should be (1.0)
    xPlus1(1) should be (1.0)
    xPlus1(2) should be (0)
    xPlus1.dim should be (1)
    xPlus1.toString should be ("1.0 + x")
    info("OK")
  }

  
  "2*X" should "be 2x" in {
    val twoX = 2 * X
    twoX(0) should be (0)
    twoX(1) should be (2.0)
    twoX(2) should be (0)
    twoX.dim should be (1)
    twoX.toString should be ("2.0x")
    info("OK")
  }
 
  "X * 2" should "be 2x" in {
    val twoX = X * 2
    twoX(0) should be (0)
    twoX(1) should be (2.0)
    twoX(2) should be (0)
    twoX.dim should be (1)
    twoX.toString should be ("2.0x")
    info("OK")
  }


  "x*x" should "be xPower(2)" in {
    val xx = X * X
    val x2 = GeneratingFunction.xPower(2)
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


  "(1 + x)(1 + x)" should "be 1 + 2x + x^2" in {
    val prod = (1 + X)*(1 + X)
    prod(0) should be (1)
    prod(1) should be (2)
    prod(2) should be (1)
    prod(3) should be (0)
    prod.dim should be (2)
    prod.toString should be ("1.0 + 2.0x + x^2")
    info("OK")
  }


  it should "be (1 + X)^2" in {
    val prod = (1 + X)^2
    prod(0) should be (1)
    prod(1) should be (2)
    prod(2) should be (1)
    prod(3) should be (0)
    prod.dim should be (2)
    prod.toString should be ("1.0 + 2.0x + x^2")
    info("OK")
  }

  "(5x^2 + 3*x +1) - (4x^2 + 2x)" should "be x^2 + x + 1" in {
    val a = 5*(X^2) + 3*X + 1
    val b = 4*(X^2) + 2*X
    (a-b).toString should be ("1.0 + x + x^2")
    info((a-b).toString)
    info("OK")
  } 

  "(1+2x)^3" should "be 1.0 + 6.0*x + 12.0*x^2 + 8.0*x^3" in {
    val prod = (1+2*X)*(1+2*X)*(1+2*X)
    prod(0) should be (1)
    prod(1) should be (6)
    prod(2) should be (12)
    prod(3) should be (8)
    prod.dim should be (3)
    prod.toString should be ("1.0 + 6.0x + 12.0x^2 + 8.0x^3")
    info("OK")    
  }
  
  
  "sum of product" should "be product of sums" in {
    val x1 = 1 + X + 2*(X^2) + (X^3)
    val x2 = 1 + 4*X + 2*(X^2)
    val x3 = 1 + 2*(X^2)
    info((x1*x2*x3).toString)
    (x1*x2*x3).sum should be (x1.sum * x2.sum * x3.sum)
    info("OK")
  }
  
}

