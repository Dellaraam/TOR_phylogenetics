library(tidyverse)
library(jsonlite)
library(purrr)



Probable <- read_csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Test_Ground/Probable_Table.csv")






N2951 <- read_tsv(file="C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/SRAs/TSVs/RAPTOR_smicro_matches1.tsv")
view(N2951)
colnames(N2951) <-c("Query", "Subject","Percent_ID","prot_len","x","y","Sub_start","Sub_end","Query_start","Query_end","E-val","Bit_score")
mean(N2951$Bit_score)
#Not there based upon the mean score

N5886 <- read_tsv(file="C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/SRAs/TSVs/RICTOR_pimeralia_matches1.tsv")
colnames(N5886) <- c("Query", "Subject","Percent_ID","prot_len","x","y","Sub_start","Sub_end","Query_start","Query_end","E-val","Bit_score")
summary(N5886$Bit_score)
# Confirmed based upon the mean score

N89957 <- read_tsv(file="C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/SRAs/TSVs/SIN1_polarella_matches1.tsv", col_names = FALSE)
colnames(N89957) <- c("Query", "Subject","Percent_ID","prot_len","x","y","Sub_start","Sub_end","Query_start","Query_end","E-val","Bit_score")
mean(N89957$Bit_score)
# Not there based upon the mean score

N33653R <- read_tsv(file="C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/SRAs/TSVs/RICTOR_croen_matches54_1.tsv", col_names = FALSE)
colnames(N33653R) <-  c("Query", "Subject","Percent_ID","prot_len","x","y","Sub_start","Sub_end","Query_start","Query_end","E-val","Bit_score")
mean(N33653R$Bit_score)
summary(N33653R)
#Not there
N33653S <- read_tsv(file="C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/SRAs/TSVs/SIN1_croen_matches54_1.tsv", col_names = FALSE)
colnames(N33653S) <-  c("Query", "Subject","Percent_ID","prot_len","x","y","Sub_start","Sub_end","Query_start","Query_end","E-val","Bit_score")
summary(N33653S)
#Not there

