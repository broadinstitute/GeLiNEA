package org.broadinstitute.cbts.gelinea

/**
 *  Class representing a result of GeLiNEA analysis
 */
case class Result(val geneSetName: String, val overlap: Int, val connectionCount: Int, val pValue: Double) extends Ordered[Result] {

  def compare(other: Result): Int = {
    if (this.pValue < other.pValue) -1
    else if (this.pValue > other.pValue) 1
    else if (this.geneSetName < other.geneSetName) -1
    else if (this.geneSetName > other.geneSetName) 1
    else 0
  }
  override def equals(other: Any): Boolean = other match {
    case that: Result => this.geneSetName == that.geneSetName && this.pValue == that.pValue
    case _            => false
  }

  override def hashCode = this.geneSetName.hashCode + (Int.MaxValue * pValue).toInt

  override def toString = this.geneSetName + "\t" + overlap + "\t" + connectionCount + "\t" + pValue

}
