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

### Archived_CSVs 

+ `HMMER_Results`: All old HMMER outputs before conversion to csv format 

+ HMMER outputs in csv format

+ Old assembly information for each clade

### Archived_Scripts 

+ Old scripts used to test ideas 

### BLAST_HITS

+ `RICTOR`: RICTOR searches for species with suspicious HMMER scores

+ `SIN1`: SIN1 searches for species with suspicious HMMER scores

### Comparative_MSA

+ MSA files for comparing Low to High storing components found in each clade

### FASTA_Files

+ Sub-Directories contain fasta files for the TOR components original 90 species found in Tatabe et al

+ HMMER results for TOR components original 90 species found in Tatabe et al

### GitHub_CSV

+ `Raw_Data_Archived`: all raw dat is for the HMMR data and taxonomic information for specific clades.
	+ Sub-directories starting with `Updated`(from NCBI) or ending in `JGI`(from JGI) 
 
+ `Jsons_JGI` and `Jsons_NCBI`: the Json files of all the BUSCO results for specific clades.

+ `BUSCO`: all BUSCO results for specific clades 

+ `Cleaned_JGI_csv`: the filtered data for the HMMR data and taxonomic information for specific clades downloaded from JGI

+ `Cleaned_NCBI_csv`: the filtered data for the HMMR data and taxonomic information for specific clades downloaded from NCBI

### IDs 

+ `MSA_IDs`: IDs for Multiple Sequence Alignments 

+ Text files of unique identifiers for proteomes from NCBI or JGI

### Images 

+ `Figures_TO_Review`: 

+ `MSA_Images`: Contains screenshots of MSAs 

+ `SVGs`: Figures turned to SVGs to manipulate presentation

+ `Updated_Figure_Images`: Old figures 

+ `Updated_Tree_Images`: Old trees with heatmaps

+ Original trees with diamond points for each TOR component 

### Metabolic_Information_Species

+ Lists of species categorized by trophic strategy and presence and absense of RICTOR. 

#### SRA 

+ `TSVs`: sample diamond outputs

+ `sample_xml_taxid`: practice sample for pulling taxids 

+ `species_list`: List of species that fit requirements with runids found 

+ `species_list_cheatsheet`: what components were found with diamond after using sra-tools to pull runids and grab the sequences with prefetch 

+ `sraxml.py`: script looks though `xml` files in a given directory to find sequences that are RNA-seq, single stranded, and transcriptomic. 

+ `srx_runid_len`: text file with species taxid, species-ids, runids, and average sequence length 

+ `srx_runid_len.300`: text file with species taxid, species-ids, runids, and average sequence length>=300

+ `xml_downloader.py`: Using the taxids in `species_list` the script creates urls to find the associated `xml`files from SRA

+ `xml_reader.py`: Prints the output `srx_runid_len` using `sraxml.py` with an input directory

### Trees 

+ `nwk` and `phy` trees made for importing into R for visualization 

# Might not be in final post

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


