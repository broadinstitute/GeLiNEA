package org.broadinstitute.cbts.gelinea

import java.io.File
import java.io.FileWriter
import java.io.PrintWriter

import org.broadinstitute.cbts.gelinea.genfun.GeneratingFunction
import org.broadinstitute.cbts.gelinea.genfun.GeneratingFunction2D
import org.broadinstitute.cbts.gelinea.genfun.GeneratingFunction2D.X
import org.broadinstitute.cbts.gelinea.genfun.GeneratingFunction2D.Y
import org.broadinstitute.cbts.gelinea.graph.DegreeBinMap
import org.broadinstitute.cbts.gelinea.graph.Graph
import org.broadinstitute.cbts.gelinea.graph.GraphBuilder
import org.broadinstitute.cbts.gelinea.graph.Node

object GeLiNEA extends AnyRef with CommandLineParser {

  protected val LOG = java.util.logging.Logger.getLogger(GeLiNEA.getClass().getName())
  
  /**
   *  Parse command line arguments and perform GeLiNA analysis
   */
  def main(args: Array[String]): Unit = {
    val commandLine = parseCommandLine(args.toList)
    val networkFile = commandLine.getRequired('network)
    val geneSetsFile = commandLine.getRequired('geneSets)
    val geneListFile = commandLine.getRequired('geneList)
    val outputFile = commandLine.getRequired('output)
    val overwrite = commandLine.getOptionalBoolean('overwrite,false)
    val minBinSize = commandLine.getOptionalInt('binSize, 1)

    GeLiNEA(networkFile, geneSetsFile, geneListFile, outputFile, overwrite, minBinSize)
  }


  /**
   * Perform GeLiNA analysis
   */
  def apply(networkFile: String, geneSetsFile: String, geneListFile: String, outputFile: String, overwriteOutput: Boolean, minBinSize: Int) {
    if ((new File(outputFile)).exists && !overwriteOutput) {
      LOG severe "ERROR: File exists: " + outputFile
      sys.exit(1)
    }
    // load network
    val graph = GraphBuilder.load(networkFile)
    LOG info "network loaded: " + graph.order + " nodes, " + graph.size + " edges"

    // load gene sets
    val inputSets: List[GeneSet] = GeneSet.load(geneSetsFile)
    val geneSets = for (geneSet <- inputSets) yield geneSet.indexBy(graph)
    LOG info inputSets.length + " gene sets loaded"

    // load gene list
    val inputList = GeneList.load(geneListFile)
    val geneList = inputList indexBy graph
    LOG info "gene list loaded, size = " + geneList.length

    // gene list network analysis
    val gelina = new GeLiNEA(graph, minBinSize)
    val results = gelina.evaluate(geneList, geneSets)
    LOG info "gene list network analysis done"

    // save results
    val out = new PrintWriter(new FileWriter(outputFile))
    out.println("geneSet\toverlap\tnConnections\tpValue")
    for (result <- results) {
      out.println(result)
    }
    out.close()
    LOG info "results saved"
  }
}


class GeLiNEA(val graph: Graph, val minBinSize: Int) {

  /**
   * Constructor for default minimal degree bin size
   */
  def this(graph: Graph) = this(graph, 1)


  /** Map for binning node degrees to node degree bins containing at least minBinSize nodes*/
  val degreeBinMap = DegreeBinMap(graph, minBinSize)
  

  /**
   *  All nodes of the network arranged by node-degree bins
   */
  val nodesByDegreeBins = degreeBinMap.nodesByDegreeBin(graph)


  /**
   * The number of connections between a node and a gene set
   */
  def connectionCount(node: Node, geneSet: IndexedGeneSet): Int = {
    var count = 0
    for (neighbor <- node) {
      if (geneSet contains neighbor) {
        count += 1
      }
    }
    return count
  }


  /**
   * The number of connections between a gene list and a gene set
   */
  def connectionCount(geneList: IndexedGeneList, geneSet: IndexedGeneSet): Int = {
    var count = 0
    for (node <- geneList) {
      count += connectionCount(node, geneSet)
    }
    return count
  }


  /** 
   *  Generating function representing the distribution of connections between an nSel randomly-selected
   *  nodes and the geneSet 
   */
  def connectionCountDistribution(nodes: Array[Node], nSel: Int, geneSet: IndexedGeneSet): GeneratingFunction = {
    var fun: GeneratingFunction2D = 1
    fun = fun setMaxYDim nSel
    for (node <- nodes; c = connectionCount(node, geneSet)) {
      fun = fun * (1 + Y * (X ^ c))
      if (fun.maxCoeff > 1E300) fun = (1.0/256) * fun //to avoid overflow
    }
    return fun |%| nSel
  }


  /** 
   *  Generating function representing the distribution of connections between the geneList and the geneSet 
   */
  def connectionCountDistribution(geneList: IndexedGeneList, geneSet: IndexedGeneSet): GeneratingFunction = {
    var fun: GeneratingFunction = GeneratingFunction.one
    val listDegreeDist = degreeBinMap(geneList)
    for (i <- 0 to listDegreeDist.length - 1) {
      val dist = connectionCountDistribution(nodesByDegreeBins(i), listDegreeDist(i), geneSet)
      fun = fun * dist
    }
    return fun
  }
  
  
  /**
   * p-value for the number of connections between a gene list and a gene set under a null model of
   * degree-preserving random node permutation.
   */
//  def connectionCountPValue(geneList: IndexedGeneList, geneSet: IndexedGeneSet): Double = evaluate(geneList, geneSet).pValue

  
  /**
   * Compute the number of connections between a gene list and a gene set and the significance
   * under a null model of degree-preserving random node permutation.
   */
  def evaluate(geneList: IndexedGeneList, geneSet: IndexedGeneSet): Result = {
    val conCount = connectionCount(geneList, geneSet)
    val overlap = (geneSet intersection geneList).size
    val dist = connectionCountDistribution(geneList, geneSet).normalized
    var pValue = 0.0;
    for (i <- conCount to dist.dim) {
      pValue += dist(i)
    }
    return new Result(geneSet.name, overlap, conCount, pValue)
  }


  /**
   * Compute the number of connections between a gene list and gene sets and the significance
   * under a null model of degree-preserving random node permutation.
   */
  def evaluate(geneList: IndexedGeneList, geneSets: Seq[IndexedGeneSet]): Array[Result] = {
    val res = for (geneSet <- geneSets) yield evaluate(geneList, geneSet)
    return res.toArray.sorted
  }
  
  /*
  /**
   * The expected number of connections between nodes in a degreeBin and a gene set.
   */
  def expectedConnectionCount(degreeBinIndex: Int, geneSet: IndexedGeneSet): Double = {
    val nodes = nodesByDegreeBins(degreeBinIndex)
    var sum = 0.0
    for (node <- nodes) {
      sum += connectionCount(node, geneSet)
    }
    return sum / nodes.length
  }

  /**
   * The variance of the number of connections between nodes in a degreeBin and a gene set.
   */
  def connectionCountVariance(degreeBinIndex: Int, geneSet: IndexedGeneSet): Double = {
    val nodes = nodesByDegreeBins(degreeBinIndex)
    var sum = 0.0
    var sum2 = 0.0
    for (node <- nodes) {
      val nConnections = connectionCount(node, geneSet)
      sum += nConnections
      sum2 += nConnections * nConnections
    }
    return math.max(0.0, (sum2 - sum * sum / nodes.length) / nodes.length)
  }


  /**
   * Compute statistics for p-value approximation
   */
  def stats(geneList: IndexedGeneList, geneSet: IndexedGeneSet): (Double, Double) = {
    var mean = 0.0
    var variance = 0.0
    val listDegreeDist = degreeBinMap(geneList)

    for ((selectCount, degreeBinIndex) <- listDegreeDist.zipWithIndex if selectCount > 0) {
      mean = mean + selectCount * expectedConnectionCount(degreeBinIndex, geneSet)
      val nodeCount = nodesByDegreeBins(degreeBinIndex).length
      // variance correction factor due to non-zero covariance
      val varFactor = if (nodeCount > 1) (1 - (selectCount - 1.0) / (nodeCount - 1)) else 1
      variance = variance + selectCount * connectionCountVariance(degreeBinIndex, geneSet) * varFactor
    }
//    println("G:  "+mean+", var = "+variance)
    return (mean, variance)
  }
  
  
  /**
   * The expected number of connections between nodes in a degreeBin and a gene set.
   */
  def expectedConnectionCountLoop(degreeBinIndex: Int, geneSet: IndexedGeneSet): Double = {
    val nodes = nodesByDegreeBins(degreeBinIndex)
    var sum = 0.0
    var i = 0
    while (i < nodes.length) {
//    for (node <- nodes) {
      sum += connectionCount(nodes(i), geneSet)
      i = i+1
    }
    return sum / nodes.length
  }

  /**
   * The variance of the number of connections between nodes in a degreeBin and a gene set.
   */
  def connectionCountVarianceLoop(degreeBinIndex: Int, geneSet: IndexedGeneSet): Double = {
    val nodes = nodesByDegreeBins(degreeBinIndex)
    var sum = 0.0
    var sum2 = 0.0
    var i = 0
    while (i < nodes.length) {
//    for (node <- nodes) {
      val nConnections = connectionCount(nodes(i), geneSet)
      sum += nConnections
      sum2 += nConnections * nConnections
      i = i+1
    }
    return math.max(0.0, (sum2 - sum * sum / nodes.length) / nodes.length)
  }


  /**
   * Compute statistics for p-value approximation
   */
  def statsLoop(geneList: IndexedGeneList, geneSet: IndexedGeneSet): (Double, Double) = {
    var mean = 0.0
    var variance = 0.0
    val listDegreeDist = degreeBinMap(geneList)

    var degreeBinIndex = 0
    while (degreeBinIndex < listDegreeDist.length) {
      val selectCount = listDegreeDist(degreeBinIndex)
      if (selectCount > 0) {
        mean = mean + selectCount * expectedConnectionCountLoop(degreeBinIndex, geneSet)
        val nodeCount = nodesByDegreeBins(degreeBinIndex).length
        // variance correction factor due to non-zero covariance
        val varFactor = if (nodeCount > 1) (1 - (selectCount - 1.0) / (nodeCount - 1)) else 1
        variance = variance + selectCount * connectionCountVarianceLoop(degreeBinIndex, geneSet) * varFactor
      }
      degreeBinIndex = degreeBinIndex + 1
    }
//    println("G:  " + mean + ", var = " + variance)
    return (mean, variance)
  }
  
  
  /**
   * Compute the number of connections between a gene list and a gene set and the approximated 
   * significance under a null model of degree-preserving random node permutation. 
   */
  def approximate(geneList: IndexedGeneList, geneSet: IndexedGeneSet): GeLiNA.Result = {
    val conCount = connectionCount(geneList, geneSet)
    val overlap = (geneSet /\ geneList).size
    val (mean, variance) = stats(geneList, geneSet)
    val z = if (variance > 0) (conCount - mean) / math.sqrt(variance) else Double.NaN
    val approxPvalue = if (z.isNaN()) 1.0 else normDist.cumulativeProbability(-z)
    return new GeLiNA.Result(geneSet, overlap, conCount, approxPvalue)
  }
  
  
    /**
   * Compute the number of connections between a gene list and gene sets and the approximated significance
   * under a null model of degree-preserving random node permutation.
   */
  def approximate(geneList: IndexedGeneList, geneSets: Seq[IndexedGeneSet]): Array[GeLiNA.Result] = {
    val res = for (geneSet <- geneSets) yield approximate(geneList, geneSet)
    return res.toArray.sorted
  }
  
  */
}
