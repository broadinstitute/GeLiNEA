
# Gene-List Network Enrichment Analysis

Gene-List Network Enrichment Analysis (GeLiNEA) takes a gene list (e.g., from a screening experiment), a gene set (from a corpus of curated gene sets, e.g., MSigDB), and a protein-protein association network (e.g., STRING) and evaluates the significance of connections between the gene list and the gene set in the network under a null model of degree-preserving random gene lists.

## Running GeLiNEA

Use [sbt (https://www.scala-sbt.org/)](https://www.scala-sbt.org/) to run GeLiNEA:

`sbt> run -n <network_file> -s <gene_sets_file> -l <gene_list_file> -o <results_file> -b <bin_size>`

**File formats**:
- network file: tab-separated text file with a header row, and containing node ids in the first two columns
- gene-sets file: [GMT file format](https://software.broadinstitute.org/cancer/software/gsea/wiki/index.php/Data_formats#GMT:_Gene_Matrix_Transposed_file_format_.28.2A.gmt.29), ids must match ids in the network file
- gene-list file: text file with a header row containing one node id per row
- results file: tab-separated text file with the following columns:
  - geneSet - name of a gene set
  - overlap - number of genes common to the gene list and the gene set.
  - nConnections - number of commections between the gene list and the gene set in the network
  - pValue - significance of connections between the gene list and the gene set in the network under a null model of degree-preserving random gene lists
  
**Notes**:
- you can use `-O <results_file>` to overwite an existing results file
- sugested bin size: 50

## Citing GeLiNEA

To cite your use of the GeLiNEA please reference **Yilong Zou _et al_.: Plasticity of polyunsaturated ether phospholipids promotes ferroptosis susceptibility and evasion.**
