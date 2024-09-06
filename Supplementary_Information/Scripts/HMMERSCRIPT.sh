#!/bin/bash

# Note to self, should I change this to have variables for the clade I'm doing?
# Might make this overall easier to execute
# Also, would it be a good idea to make this fully automated?

# Declare Which Clade
clade=Streptophyta
# Declare the domain or protein being used
used=TOR_FAT




#Folders to be used along with query HMM
#To be used in the staging folder for the clade being analyzed
#Declare Input folder
Input=/home/kyle/Desktop/HMMER_WORK/$clade/Proteins
#Declare query for HMMSearch
Query=/home/kyle/Desktop/HMMER_WORK/HMMs/Full/$used
#Declare Output Folder
Output=/home/kyle/Desktop/HMMER_WORK/$clade/Full_Output

for file in $Input/*; do
	file_name=$(basename -- "$file")
	file_name_no_extension="${file_name%.*}"
	hmmsearch $Query.hmm $file > $Output/"${file_name_no_extension}"_${used}.out
	
	
done

