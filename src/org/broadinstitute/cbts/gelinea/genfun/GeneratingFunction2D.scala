package org.broadinstitute.cbts.gelinea.genfun

import scala.math.min
import scala.math.max

object GeneratingFunction2D {

  import scala.language.implicitConversions


  def dim(dim: Int, maxDim: Int): Int = if (maxDim < 0) dim else min(maxDim, dim)


  val one = new GeneratingFunction2D(Array(Array(1)), -1, -1)


  val Y = new GeneratingFunction2D(Array(new Array(0), Array(1)), -1, -1)


  val X = new GeneratingFunction2D(Array(Array(0, 1)), -1, -1)


  def xPower(power: Int): GeneratingFunction2D = {
    if (power == 0) return one
    if (power == 1) return X
    val array = new Array[Double](power + 1)
    array(power) = 1
    return new GeneratingFunction2D(Array(array), -1, -1)
  }

  
  /** Implicit conversion from a number to a constant generating function */
  implicit def constant(value: Double): GeneratingFunction2D = new GeneratingFunction2D(Array(Array(value)), -1, -1)
}



/**
 *  Class representing generating functions with two variables
 */
class GeneratingFunction2D private (private val matrix: Array[Array[Double]], private val maxDimY: Int, private val maxDimX: Int) {


  def apply(dimY: Int, dimX: Int): Double = {
    if (dimY >= matrix.length) 0 else {
      val row = matrix(dimY)
      if (dimX >= row.length) 0 else row(dimX)
    }
  }

  
//  def maxDimYopt: Option[Int] = if (maxDimY < 0) None else Some(maxDimY)
//  def maxDimXopt: Option[Int] = if (maxDimX < 0) None else Some(maxDimX)

  
  val dimY: Int = matrix.length - 1
  def dimX(dim: Int): Int = if (dim < matrix.length) matrix(dim).length - 1 else -1

  
//  private def dim1(dim: Int): Int = if (maxDimY < 0) dim else min(maxDimY, dim)
//  private def dim2(dim: Int): Int = if (maxDimX < 0) dim else min(maxDimX, dim)

  
  private def newMaxDimY(other: GeneratingFunction2D): Int = if (this.maxDimY < 0) other.maxDimY else if (other.maxDimY < 0) this.maxDimY else min(this.maxDimY, other.maxDimY)
  private def newMaxDimX(other: GeneratingFunction2D): Int = if (this.maxDimX < 0) other.maxDimX else if (other.maxDimX < 0) this.maxDimX else min(this.maxDimX, other.maxDimX)


  /**
   *  Add two generating functions
   */
  def +(other: GeneratingFunction2D): GeneratingFunction2D = {
    val newMaxDimY: Int = this.newMaxDimY(other)
    val newMaxDimX: Int = this.newMaxDimX(other)

    val newMatrix = new Array[Array[Double]](GeneratingFunction2D.dim(max(this.dimY, other.dimY), newMaxDimY) + 1)
    for (i <- 0 to newMatrix.length - 1) {
      val newRow = new Array[Double](GeneratingFunction2D.dim(max(this.dimX(i), other.dimX(i)), newMaxDimX) + 1)
      for (j <- 0 to newRow.length - 1) {
        newRow(j) = this(i, j) + other(i, j)
      }
      newMatrix(i) = newRow
    }
    return new GeneratingFunction2D(newMatrix, newMaxDimY, newMaxDimX)
  }


  /**
   *  Add a constant to a generating function
   */
  def +(value: Int): GeneratingFunction2D = {
    val newMatrix = new Array[Array[Double]](this.dimY + 1)
    val newRow = this.matrix(0).clone
    newRow(0) = newRow(0) + value
    newMatrix(0) = newRow
    for (i <- 1 to newMatrix.length - 1) {
      newMatrix(i) = this.matrix(i)
    }
    return new GeneratingFunction2D(newMatrix, this.maxDimY, this.maxDimX)
  }


  /**
   *  Find array dimensions for product of generating functions
   */
  private def findLengths(other: GeneratingFunction2D): Array[Int] = {
    val lengths: Array[Int] = Array.fill(this.dimY + other.dimY + 1) { 0 }
    for (i <- 0 to other.dimY; if (other.dimX(i) >= 0)) {
      val shift = other.dimX(i)
      for (j <- 0 to this.dimY; if (this.dimX(j) >= 0)) {
        lengths(i + j) = max(lengths(i + j), this.dimX(j) + shift + 1)
      }
    }
    return lengths
  }


  /**
   *  Multiply two generating functions
   */
  def *(other: GeneratingFunction2D): GeneratingFunction2D = {
    val newMaxDimY: Int = this.newMaxDimY(other)
    val newMaxDimX: Int = this.newMaxDimX(other)

    val lengths: Array[Int] = findLengths(other)
    val newMatrix = new Array[Array[Double]](GeneratingFunction2D.dim(lengths.length - 1, newMaxDimY) + 1)
    for (i <- 0 to newMatrix.length - 1) {
      newMatrix(i) = new Array[Double](GeneratingFunction2D.dim(lengths(i) - 1, newMaxDimX) + 1)
    }
    for (oi <- 0 to other.dimY)
      for (oj <- 0 to other.dimX(oi))
        if (other(oi, oj) != 0) {
          val value = other(oi, oj)
          for (ti <- 0 to this.dimY)
            if (oi + ti < newMatrix.length)
              for (tj <- 0 to this.dimX(ti))
                if (oj + tj < newMatrix(oi + ti).length && this(ti, tj) != 0) {
                  newMatrix(oi + ti)(oj + tj) += value * this(ti, tj)
                }

        }
    return new GeneratingFunction2D(newMatrix, newMaxDimY, newMaxDimX)
  }

  
  /**
   * Generating function to the n-th power
   * Note: use <code>1 + y*(x^3)</code> instead of <code>1 + y*x^3</code> as + and * have higher precedence then ^!
   */
  def ^(n: Int): GeneratingFunction2D = {
    assert(n >= 0)
    if (n == 0) return GeneratingFunction2D.one
    if (n == 1) return this
    if (this == GeneratingFunction2D.X) return GeneratingFunction2D.xPower(n)
    var res = this
    for (i <- 1 to n - 1) {
      res = res * this
    }
    return res
  }

  
  /** The value of the largest coefficient */
  lazy val maxCoeff: Double = computeMaxCoeff

  
  /** Compute the value of the largest coefficient */
  private def computeMaxCoeff: Double = {
    var maxValue = this(0, 0)
    for (array <- this.matrix; if array.length > 0) {
      maxValue = max(maxValue, maxCoeff(array))
    }
    return maxValue
  }


  /** Compute the value of the largest coefficient of an array */
  private def maxCoeff(array: Array[Double]): Double = {
    var maxValue = Double.MinValue
    for (value <- array) {
      maxValue = max(maxValue, value)
    }
    return maxValue
  }

  
  /** restrict max dimY*/
  def setMaxYDim(maxDim: Int): GeneratingFunction2D = {
    if (maxDim < 0 || (0 <= this.maxDimY && this.maxDimY <= maxDim))
      return new GeneratingFunction2D(this.matrix, maxDim, this.maxDimX)
    if (this.dimY <= maxDim)
      return new GeneratingFunction2D(this.matrix, maxDim, this.maxDimX)
    else {
      val newMatrix = new Array[Array[Double]](GeneratingFunction2D.dim(this.matrix.length - 1, maxDim) + 1)
      for (i <- 0 to newMatrix.length - 1) {
        newMatrix(i) = this.matrix(i)
      }
      return new GeneratingFunction2D(newMatrix, maxDim, this.maxDimX)
    }
  }

  
  /**
   * Return coefficient at y^n as one dimensional generating function
   */
  def %(yExp: Int): GeneratingFunction = {
    return new GeneratingFunction(this.matrix(yExp), if (maxDimX < 0) None else Some(maxDimX))
  }

  
  /**
   * Return coefficient at y^n as normalized one dimensional generating function
   */
  def |%|(yExp: Int): GeneratingFunction = {
    val normalizedVector = normVector(yExp)
    return new GeneratingFunction(normalizedVector, if (maxDimX < 0) None else Some(maxDimX))
  }

  
  /**
   *   The normalized (sum to 1.0) vector of coefficients at y^n
   */  
  private def normVector(yExp: Int): Array[Double] = {
    val source: Array[Double] = this.matrix(yExp)
    val sum: Double = source.sum;
    val vector = new Array[Double](source.length)
    for (i <- 0 to vector.length - 1) {
      vector(i) = source(i) / sum
    }
    return vector
  }

  
  /**
   * String representation of generating function
   */
  override def toString: String = {
    val sb = new StringBuilder
    var empty = true
    for (i <- 0 to this.dimY)
      for (j <- 0 to this.dimX(i)) {
        val value = this(i, j)
        if (value != 0) {
          sb.append(if (empty && value > 0) "" else if (value < 0) " - " else " + ")
          sb.append(if ((i != 0 || j != 0) && math.abs(value) == 1) "" else math.abs(value))
          sb.append(if (j > 1) "x^" + j else if (j == 1) "x" else "")
          sb.append(if (i > 1) "y^" + i else if (i == 1) "y" else "")
          empty = false
        }

      }

    return if (empty) "0" else sb.toString
  }
}
