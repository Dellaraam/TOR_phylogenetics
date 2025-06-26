# TOR Phylogenetics

### Introduction

To understand the evolution of the TOR complex manifests across the Tree of Life our goal is to 
computationally confirm the proteomic presence of the RICTOR, RAPTOR, LST8, and SIN1 components.
We seek to answer questions such as when and where TOR components have been gained and lost
and what contexts do these occur as well as confirm or disprove previous conclusions.

We started with 96 unique genomes form various clades such as Alveolata, Stramenoplies, Rhizaria, 
Metzoa, and Fungi clades. Then added an additional 142 Alveolata, 120 Stramenopiles, 
9 Rhizaria, 365 Streptophyta, 87 Chlorophyta, 14 Rhodophyta, 49 Discoba, and 22 Metamonada genomes 
to our analysis from the NCBI database in protein form.


### GitHub_CSV

+ `Raw_Data_Archived` contains all raw dat is for the HMMR data and taxonomic information for specific clades.
	+ Sub-directories starting with `Updated`(from NCBI) or ending in `JGI`(from JGI) 
 
+ `Jsons_JGI` and `Jsons_NCBI` contain the Json files of all the BUSCO results for specific clades.

+ `BUSCO` contains all BUSCO results for specific clades 

+ `Cleaned_JGI_csv` contains the filtered data for the HMMR data and taxonomic information for specific clades downloaded from JGI

+ `Cleaned_NCBI_csv` contains the filtered data for the HMMR data and taxonomic information for specific clades downloaded from NCBI



### Data Analysis

Using the Eddy Labs HMMER program search for sequences that match the stated TOR components 
either in part or in whole and then the proteins found are concatenated into large files 
corresponding to the species clade. The protein HMMs were obtained through the corresponding InterPro
page's link to Panther. The result is a .txt white space delimited table. The HMMR output files are 
converted into comma separated values using the script `hmmer2csv.py`. 

```
hmmsearch --tblout <file.hmm> <proteome.fa> 
``` 
Move all HMMER outputs into one directory for easy transfer
```
hmmer2csv.py <output-directory>
awk ‘NR==1 || FNR>1’ *.csv > (name of output file here)
```

All proteomes were pulled from the [NCBI datasets](https://github.com/ncbi/datasets) tool and [JGI Phycocosm](https://phycocosm.jgi.doe.gov/phycocosm/home) for further analysis, including:
 
BUSCO for quality assurance to sense the general completeness of the assembly and how much fragmentation 
is expected.Allowing us to be more confident in the accuracy of our results as well as identify organisms
that should be further investigated in the alignement step. To compile all the results into one file 
run `Json_to_csv.R`.  
 
Multiple sequence alignments to establish homology for sequences barely exceeding the threshold for the 
HMMR results. Sequences for each TOR component was aligned with a number of references including H.Sapiens,
S.cerevisiae, and S.pombe using ClustalO with the ClustalO coloring scheme on Jalview for visualization. 
To use Jalview to to "File>Input Alignment>From File" and select your sequences. To start an alignment 
select "Web Service" and choose an alignment program. 

### Data Compilation 

HMMER and Blast data will be merged in a larger CSV file with species names and TAXIDs. To obtain the TAXIDs, species names, 
class, phylum, order, family, genus please see the NCBI database documentation page for [taxonomy](https://www.ncbi.nlm.nih.gov/datasets/docs/v2/reference-docs/command-line/datasets/download/taxonomy/).


