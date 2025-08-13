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

+ `HMMER_Results`: All HMMER outputs before conversion to csv format for original 90 species found in Tatabe at al.
+ HMMER outputs in csv format
+ Old assembly information for each clade for the original 90 species

### Archived_Scripts 

+ Miscellaneous scripts used to test ideas 

### BLAST_HITS

+ `RICTOR`: Only RICTOR searches for species with suspicious HMMER scores
+ `SIN1`: Only SIN1 searches for species with suspicious HMMER scores

### Comparative_MSA

+ MSA files for comparing Low to High storing components found in each clade

### FASTA_Files

+ Sub-Directories contain fasta files for the TOR components original 90 species found in Tatabe et al
+ HMMER results for TOR components original 90 species found in Tatabe et al

### GitHub_CSV

+ `Alveolata_Groups_Information`: Assembly information for taxonomic groups in the Alveolata clade
+ `BUSCO`: all BUSCO results for specific clades 
+ `Cleaned_JGI_csv` and `Cleaned_NCBI_csv`: the filtered data for the HMMR data and taxonomic information for specific clades downloaded from JGI and NCBI
+ `Combined_CSVs`: 
	+ Combined information from assembly, taxonomy, organism name, and presence of component with Low, Medium, or High score for each clade
	+ `ComeleteTable.csv`, `Project.csv` and `New_Combined_Table_218.csv`: all organisms and clades information and data combined in one table 
	+ `Numeric_Table`: Same composition as `ComeleteTable.csv` but with numeric scores instead of Low, Medium, and High	
	+ `Parasite_Test.csv`: Combined table of only information and data for parasites	
	+ `SAR_Combined.csv`: Combined table of information and data for SAR clade 
+ `Finalized_CSVs`: 
	+ Final component CSVs 
	+ Final Master Table and table for publication
	+ Finals taxonomy CSVs for JGI and NCBI
	+ Final BUSCO CSVs 
	+ Finals Metabolic/Trophic strategies
	+ `trophic_strategy.csv`: List of species and their trophic strategies
+ `Jsons_JGI` and `Jsons_NCBI`: the Json files of all the BUSCO results for specific clades.
+ `Raw_Data_Archived`: all raw dat is for the HMMR data and taxonomic information for specific clades.
	+ Sub-directories starting with `Updated`(from NCBI) or ending in `JGI`(from JGI)
+ Assembly information for various clades and sub-groups

### HMMER_Results

+ Original HMMER results for the 90 species found in Tatebe et al

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

### Phylogenetics 

+ `Copies`: Copies of Markdown files seen in `HMMER_Post_Analysis`
+ `Figures`: Use data from the Master Table and NCBI taxonomy into create phylogenetic trees that show where component loss occurs, quality of genome, and trophic strategy 
+ `HMMER_Post_Analysis`:  
	+ Markdown files for different clades to merge highest raw HMMER hits, BUSCO, assembly and taxonomic information and check for photosynthesis, parasitism and ciliates. Organism's HMMER scores for are plotted for each component individual domain score vs total sequence score. Plots are used to interoperate what is Hight, Medium, or Low in the scoring scheme.  
	+ `Complete_Phylogenetics.R`: Converging HMMER scores, BUSCO, taxonomic information, assembly information of all clades from NCBI and JGI into one file. CSVs go to `GitHub_CSV/Finalized_CSVs`
	+ Adding and merging species from JGI to the larger document 
+ `Maintenance_Scripts`
	+ `CombinedJGI_Table_Script.R`: Combining NCBI and JGI taxonomic information into one table. See table in `GitHub_CSV/Finalized_CSVs`.  
	+ `HTML_Additions.R`: Attaches `Combined_Taxonomy.csv` to `CompleteTable.csv`. Output is `HTML.tsv` in `GitHub_CSV/Finalized_CSVs`. 
	+ `Final_Busco_Script.R`: Combine BUSCO completeness score and fragmentation score from all clades and species into one BUSCO table in `GitHub_CSV/Finalized_CSVs`
	+ `Json_to_csv.R`: Converts Json files to csvs. Used for converting BUSCO Jsons. 
	+ `Library_Script.R`: libraries needed to run some R programs. 
	+ `MSA_Experimental_Script.R`: Test import Multiple Sequence Alignments to R
	+ `MSA_Protein_ID_Script.R`: Importing Multiple Sequence Alignments to R
	+ `Merging_Combined_CSV_Files.R`: Combining taxonomy, BUSCO scores, and HMMER data into one table. See table in `GitHub_CSV/Finalized_CSVs` as `HTML.tsv`.
	+ `Metabolic_Strategies_Breakdown.R`: Making tables with species organized into trophic strategies. Outputs in `Metabolic_Information_Species`.
	+ `Numeric_Table_Script.R`: Turning letter labels for components back to numeric values 
+ `MasterTable`:  
	+ `Final_Metabolic_Tree_Table_Creation.R`: Create full table with corrected trophic strategies
	+ `Master_Table_Creation.R`: Create a master table with the table adding in domain scores for TOR components and trophic strategies. 
	+ `Publication_Tree.R`:  Correct trophic strategies and combine with Master Table for creating figures in publication. See `GitHub_CSV/Finalized_CSVs` for tables.
+ `Pie_Charts`: Creating pie charts to show predominant trophic 
+ `TreeScripts`: Use data from the `HTML.tsv` table and NCBI taxonomy into create phylogenetic trees that show where component loss occurs and quality of genome 


### SRA 

+ `TSVs`: sample diamond outputs
+ `sample_xml_taxid`: practice sample for pulling taxids 
+ `species_list`: List of species that fit requirements with runids found 
+ `species_list_cheatsheet`: what components were found with diamond after using sra-tools to pull runids and grab the sequences with prefetch 
+ `sraxml.py`: script looks though `xml` files in a given directory to find sequences that are RNA-seq, single stranded, and transcriptomic. 
+ `srx_runid_len`: text file with species taxid, species-ids, runids, and average sequence length 
+ `srx_runid_len.300`: text file with species taxid, species-ids, runids, and average sequence length>=300
+ `xml_downloader.py`: Using the taxids in `species_list` the script creates urls to find the associated `xml`files from SRA
+ `xml_reader.py`: Prints the output `srx_runid_len` using `sraxml.py` with an input directory

### Supplementary_Information 

+ `HMMs`: 
	+ `Domains`: Protein domains of sequence 
	+ `Full`: Full protein component sequences
+ `Scripts`: Bash scripts for bulk HMMER 

### Test_Grounds 

+ Testing ideas for presenting Phylogenetic results

### Trees 

+ `nwk` and `phy` trees made for importing into R for visualization 



+ `hmmer2csv.py`: turns hmmr outputs into machine readable format(csv)
+ `automate_csv.py`: takes directory of hmmr outputs and puts the contents into `hmmer2csv.py`

