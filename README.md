# TOR Phylogenetics

To understand the evolution of the TOR complex manifests in other species our goal is to 
computationally confirm the proteomic sequences of the RICTOR, RAPTOR, LST8, and SIN1 components.
We seek to answer questions such as when and where have TOR components have been gained and lost
and what contexts do these occur as well as confirm or disprove previous conclusions.

We started with 96 unique genomes form various clades such as Alveolata, Stramenoplies, Rhizaria, 
Metzoa, and Fungi clades. Then added an additional, although not unique, 193 Alveolata, 176 Stramenopiles, 
8 Rhizaria genomes to our analysis from the NCBI database in protein form. If the genome did not have 
a proteome then using TBLASTN the genome would be converted to protein format. 

Using the Eddy Labs HMMER program we searched for sequences that match the stated TOR components 
either in part or in whole and then the proteins found are concatenated into large files 
corresponding to the species clade. The files are convert the output into comma separated values 
using the script hmmer2csv.py. 

```
 insert example command line input here 
```
All genomes are also run on BUSCO for genome quality assurance to sense the general completeness of 
the assembly and how much fragmentation is expected. Allowing us to be more confident in the accuracy
of our results as well as identify organisms that should be further investigated in the alignement step.



