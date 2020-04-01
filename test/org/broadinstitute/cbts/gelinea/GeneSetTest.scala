package org.broadinstitute.cbts.gelinea

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class GeneSetTest extends FlatSpec with Matchers {

    "GeneSet" should "be loaded" in {
    	val genesets: List[GeneSet] = GeneSet.load("test/data/testGeneSets.txt") 
    	var found = false
    	for (geneSet <- genesets){
    	  if (geneSet.name == "TRNA_PROCESSING"){
    	    geneSet.geneSet should be (Set("ENSP00000261772","ENSP00000234677","ENSP00000260956","ENSP00000251607","ENSP00000264670","ENSP00000310015","ENSP00000314441","ENSP00000274680","ENSP00000221770"))
    	    found = true
    	  }
    	}
    	found should be (true)
    	info("OK")
    }
}

									