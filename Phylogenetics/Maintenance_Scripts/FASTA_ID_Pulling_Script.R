
## Script for pulling H/M/L scoring IDs from the master table for retrieving FASTA
## Kyle Johnson
## 4/30/25








#source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Library_Script.R")




MasterTable <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Master_Table.csv")
MasterTable <- select(MasterTable, -X)



# Stramenopiles First
Stram <- MasterTable %>% filter(Super.Group == "Stramenopiles")

StramH <- Stram %>% filter(RICTOR == "H")





write.table(StramH$tar, file = "~/GitHub/TOR_phylogenetics/IDs/MSA_IDs/StramH.txt", sep = "\t", row.names = F, col.names = F)