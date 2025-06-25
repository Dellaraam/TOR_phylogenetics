# TOR Phylogenetics

### Introduction

To understand the evolution of the TOR complex manifests in other species our goal is to 
computationally confirm the proteomic sequences of the RICTOR, RAPTOR, LST8, and SIN1 components.
We seek to answer questions such as when and where have TOR components have been gained and lost
and what contexts do these occur as well as confirm or disprove previous conclusions.

We started with 96 unique genomes form various clades such as Alveolata, Stramenoplies, Rhizaria, 
Metzoa, and Fungi clades. Then added an additional, although not unique, 193 Alveolata, 176 Stramenopiles, 
8 Rhizaria genomes to our analysis from the NCBI database in protein form. If the genome did not have 
a proteome then using TBLASTN the genome would be converted to protein format. 

### Data Analysis

Using the Eddy Labs HMMER program we searched for sequences that match the stated TOR components 
either in part or in whole and then the proteins found are concatenated into large files 
corresponding to the species clade. The protein HMMs were obtained through the corresponding InterPro
page's link to Panther. The result is a .txt white space delimited table. The HMMR output files are 
converted into comma separated values using the script `hmmer2csv.py`. 

```
hmmsearch <file.hmm> 
``` 

All proteomes were pulled from the [NCBI datasets](https://github.com/ncbi/datasets) tool and [JGI Phycocosm](https://phycocosm.jgi.doe.gov/phycocosm/home) for further analysis, including:
 
BUSCO for quality assurance to sense the general completeness of the assembly and how much fragmentation 
is expected. Allowing us to be more confident in the accuracy of our results as well as identify organisms
that should be further investigated in the alignement step. 

Multiple sequence alignments to establish homology for sequences barely exceeding the threshold for the 
HMMR results. Sequences for each TOR component was aligned with a number of references including H.Sapiens,
S.cerevisiae, and S.pombe using ClustalO with the ClustalO coloring scheme on Jalview for visualization.  

### Data Compilation 

HMMER and Blast data will be merged in a larger CSV file with species names and TAXIDs. To obtain the TAXIDs, species names, 
class, phylum, order, family, genus please see the NCBI database documentation page for [taxonomy](https://www.ncbi.nlm.nih.gov/datasets/docs/v2/reference-docs/command-line/datasets/download/taxonomy/).


