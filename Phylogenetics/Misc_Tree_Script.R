#NCBI Loading Script

library(tidyverse)
library(ggplot2)





CompleteNCBIInfo <- read_tsv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/NCBI_Filtered_All_Information.tsv")
write.table(CompleteNCBIInfo$`Organism Taxonomic ID`, file = "~/GitHub/TOR_phylogenetics/IDs/NCBILargeInfoID.txt", sep = "\t", row.names = F, col.names = F)
