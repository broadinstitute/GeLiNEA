package org.broadinstitute.cbts.gelinea.genfun

object GeneratingFunction {

  import scala.language.implicitConversions

  val one = new GeneratingFunction(Array(1.0))
  val zero = new GeneratingFunction(new Array[Double](0))
  val X = new GeneratingFunction(Array(0.0, 1.0))


  /**
   *  Generating function for x ^ n
   */
  def xPower(power: Int): GeneratingFunction = {
    val array = new Array[Double](power + 1)
    array(power) = 1
    return new GeneratingFunction(array)
  }


  /**
   *  Implicit conversion from a number to a constant generating function
   */
  implicit def convert(value: Double): GeneratingFunction = new GeneratingFunction(Array[Double](value))

  
  private def min(maxDim1: Option[Int], maxDim2: Option[Int]): Option[Int] = maxDim1 match {
    case None => maxDim2
    case Some(maxDimVal1) => maxDim2 match {
      case None => maxDim1
      case Some(maxDimVal2) => Some(math.min(maxDimVal1, maxDimVal2))
    }
  }

  
  private def newDim(dim: Int, maxDim: Option[Int]) = maxDim match {
    case None => dim
    case Some(maxDimVal) => math.min(dim, maxDimVal)
  }
}

class GeneratingFunction protected[genfun] (private val vector: Array[Double], val maxDim: Option[Int]) {

  import GeneratingFunction.newDim
  import GeneratingFunction.min

  private def this(vector: Array[Double]) = this(vector, None)


  /**
   *  The largest exponent of the generating function
   */
  val dim = vector.length - 1


  /**
   *  The coefficient at a specified term
   */
  def apply(index: Int): Double = if (0 <= index && index < vector.length) vector(index) else 0.0


  /**
   *  Add two generating functions
   */
  def +(other: GeneratingFunction): GeneratingFunction = {
    if (this.dim == 0) return other + this(0)
    val newMaxDim = min(this.maxDim, other.maxDim)
    val newLength = 1 + newDim(math.max(this.dim, other.dim), newMaxDim)
    val newVector = new Array[Double](newLength)
    for (i <- 0 to newLength - 1) {
      newVector(i) = this(i) + other(i)
    }
    return new GeneratingFunction(newVector, newMaxDim)
  }


  /**
   *  Add a constant to a generating function
   */
  def +(value: Double): GeneratingFunction = {
    val newVector: Array[Double] = this.vector.clone
    newVector(0) += value
    return new GeneratingFunction(newVector, this.maxDim)
  }


  /**
   *  Subtract two generating functions
   */
  def -(other: GeneratingFunction): GeneratingFunction = other * (-1) + this


  /**
   *  Multiply two generating functions
   */
  def *(other: GeneratingFunction): GeneratingFunction = {
    if (this.dim == 0) return other * this(0)
    if (other.dim == 0) return this * other(0)
    val newMaxDim = min(this.maxDim, other.maxDim)
    val newLength = 1 + newDim(this.dim + other.dim, newMaxDim)
    val newVector = new Array[Double](newLength)
    for (i <- 0 to math.min(newLength, this.vector.length) - 1) {
      for (j <- 0 to math.min(newLength - i, other.vector.length) - 1) {
        newVector(i + j) += this(i) * other(j)
      }
    }
    return new GeneratingFunction(newVector, newMaxDim)
  }


  /**
   * Divide two generating functions, return also reminder
   */
//  def /(other: GeneratingFunction): GeneratingFunction = (this divide other)._1


  /**
   * Divide two generating functions, return also reminder.
   * This algorithm is numerically unstable!
   */
//  def divide(other: GeneratingFunction): (GeneratingFunction, GeneratingFunction) = {
//    if (other.dim == 0) return (this / other(0), GeneratingFunction.zero)
//    val newLength = this.dim - other.dim + 1
//    val newVector = new Array[Double](newLength)
//    val remainder = this.vector.clone
//    for (i <- (newLength - 1) to 0 by -1) {
//      newVector(i) = remainder(i + other.dim) / other(other.dim)
//      for (j <- 0 to other.dim) {
//        remainder(j + i) = remainder(j + i) - newVector(i) * other(j)
//      }
//    }
//    val res = new GeneratingFunction(newVector, this.maxDim)
//    val rem = new GeneratingFunction(remainder.slice(0, other.dim), this.maxDim)
//    return (res, rem)
//  }


//  def inverse(moduloX: Int): GeneratingFunction = {
//    var g = GeneratingFunction.one * (1 / this(0))
//    var i = 1
//    while (i < moduloX - 1) {
//      i = 2 * i
//      g = g setMaxDim math.min(i, moduloX - 1)
//      val a = 2 * g
//      val b = this * g * g
//      g = g * (2 - this * g)
//    }
//    return g
//  }


  /**
   *  Multiply generating function by a constant
   */
  def *(value: Double): GeneratingFunction = {
    val newLength = 1 + this.dim
    val newVector = new Array[Double](newLength)
    for (i <- 0 to newLength - 1) {
      newVector(i) = value * this(i)
    }
    return new GeneratingFunction(newVector, this.maxDim)
  }


  /**
   * Divide generating function by a constant
   */
  def /(value: Double) = this * (1.0 / value)


  /**
   * Sum of all coefficients. Same as value of the generating function for x=1
   */
  lazy val sum = vector.sum


  /**
   * Mean value of the probability distribution encoded by this generating function
   */
//  lazy val mean = {
//    var sum = 0.0
//    for (i <- 1 to dim) {
//      sum += i * this(i)
//    }
//    sum
//  }


  /**
   *  Normalize generating function so that coefficients sum to 1.0
   */
  def normalized: GeneratingFunction = this / this.sum


  /**
   * Generating function to the n-th power.
   * Note: use <code>1 + (x^3)</code> instead of <code>1 + x^3</code> as + has higher precedence then ^!
   */
  def ^(n: Int): GeneratingFunction = {
    assert(n >= 0)
    if (n == 0) return GeneratingFunction.one
    if (n == 1) return this
    if (this == GeneratingFunction.X) return GeneratingFunction.xPower(n)
    var res = this
    for (i <- 1 to n - 1) {
      res = res * this
    }
    return res
  }


  /**
   *  Restrict max exponent, use negative value for no restriction.
   */
//  def setMaxDim(maxDim: Int): GeneratingFunction = {
//    val trim = maxDim >= 0 && this.dim >= maxDim
//    val newMaxDim = if (maxDim < 0) None else Some(maxDim)
//    val newVector = if (trim) this.vector.slice(0, newMaxDim.get + 1) else this.vector
//    return new GeneratingFunction(newVector, newMaxDim)
//  }


  /**
   *  p-value = sum of all coefficients >= x, assuming this generating function was is normalized.
   */
  def pValue(x: Int): Double = {
    var sum = 0.0
    for (i <- x to this.dim) {
      sum += this(i)
    }
    return sum
  }

  /**
   *  String representation of a generating function.
   */
  override def toString: String = {
    val sb = new StringBuilder
    var first = true
    for (i <- 0 to this.dim) {
      val value = this(i)
      if (value != 0) {
        sb.append(if (value < 0) " - " else if (first) "" else " + ")
        sb.append(if (i > 0 && math.abs(value) == 1) "" else math.abs(value))
        sb.append(if (i > 1) "x^" + i else if (i == 1) "x" else "")
        first = false
      }
    }
    return if (first) "0.0" else sb.toString
  }

}
