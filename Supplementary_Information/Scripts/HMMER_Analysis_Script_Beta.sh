#!/bin/sh


#Folders to be used along with query HMM
#To be used in the staging folder for the clade being analyzed
#Declare Input folder
Input=E:/HMMER_Searches/Shiozaki_Reference/Alveolata/Proteins/
#Declare query for HMMSearch
Query=E:/HMMER_Searches/Shiozaki_Reference/Alveolata/SIN1_FULL.hmm
#Declare Output Folder
Output=E:/HMMER_Searches/Shiozaki_Reference/Alveolata/Full_Output/

#Assign the file types to be identified
File_Types="*.faa *.fasta"

for file in $Input; do
	file_name=$(basename -- "$file")
	file_name_no_extension="${file_name%.*}"
	echo hmmsearch $Query $file > $Output
	
done






			
				
			

 