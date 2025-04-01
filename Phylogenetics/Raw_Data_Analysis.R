
# RICTOR RAW DATA Analysis
# Using ggplot2 to determine the cutoff values
# Kyle Johnson
# 4/1/2025



source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Library_Script.R")

#Import the Rictor Raw Data (NCBI)
TaxonInfo <- read_tsv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/NCBI_Filtered_All_Information.tsv")
TaxonInfo <- rename(TaxonInfo, Accn = "Assembly Accession")

AlveolataRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/RICTORAlveolata.csv")
StramenopileRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/RICTORStramenopiles.csv")
RhizariaRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/RICTORRhizaria.csv")

Chlorophyta <- read.csv(file = "")
Streptophyta <-
Rhodophyta <-

Discoba <-
Metamonada <-
  
  
  
  


# SARS RICTOR ------------------------------------------------------------------
AlveolataRictor <- left_join(AlveolataRictor, TaxonInfo[c("Organism Name", "Accn", "Organism Taxonomic ID")], by = "Accn")
AlveolataRictor %>% ggplot(aes(x = `scd`, y = `Organism Name`))+
                             geom_jitter()

StramenopileRictor <- left_join(StramenopileRictor,TaxonInfo[c("Organism Name", "Accn", "Organism Taxonomic ID")], by = "Accn")
StramenopileRictor %>% ggplot(aes(x = `scd`, y = `Organism Name`))+
  geom_jitter()

RhizariaRictor <- left_join(RhizariaRictor,TaxonInfo[c("Organism Name", "Accn", "Organism Taxonomic ID")], by = "Accn")
RhizariaRictor %>% ggplot(aes(x = `scd`, y = `Organism Name`))+
  geom_jitter()

                           