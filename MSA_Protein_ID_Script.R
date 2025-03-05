#MSA Protein Identification Script
#Kyle Johnson
# 3/4/2025


# Going to read in the csvs for the Raw Data
# Then we are going to filter the data the exact same way that was used for the Complete Tables

library(tidyverse)
library(jsonlite)
library(purrr)


#Start with RICTOR
# Remember this is only for NCBI. Have to do something special for the JGI version
# Will Work on that tomorrow

Taxon <- read.csv(file = "~/GitHub/TOR_phylogenetics/Combined_Taxonomy.csv")
Taxon <- rename(Taxon, Organism.Name = "Tax.name")

AlveolataNames <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Alveolata_Information_2.csv")
AlveolataNames <- rename(AlveolataNames, Accn = "Assembly.Accession")
AlveolataRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/RICTORAlveolata.csv")

StramenopileNames <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Stramenopiles_Information_2.csv")
StramenopileNames <- rename(StramenopileNames, Accn = "Assembly.Accession")
StramenopilesRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/RICTORStramenopiles.csv" )
Temp <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_RICTOR.csv")
StramenopilesRictor <- rbind(StramenopilesRictor,Temp)

RhizariaNames <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_Information_2.csv")
RhizariaNames <- rename(RhizariaNames, Accn = "Assembly.Accession")
RhizariaRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/RICTORRhizaria.csv")
# Will Have to add the fasta files for rhizaria manually I believe


#Do the filtering
AlveolataRictor <- merge(AlveolataRictor,AlveolataNames[c("Organism_Taxonomic_ID", "Organism.Name", "Accn")], by = "Accn")
AlveolataRictor <- AlveolataRictor %>% filter(sca >= 100 & scd >= 100)%>%
  group_by(Organism_Taxonomic_ID) %>%
  slice_max(scd) %>%
  slice_max(evd)%>%
  ungroup
AlveolataRictor <- distinct(AlveolataRictor, Organism_Taxonomic_ID, .keep_all = TRUE)

#StramenopilesRictor <- merge(StramenopilesRictor, StramenopileNames[c("Organism_Taxonomic_ID", "Organism.Name", "Accn")], by = "Accn")
StramenopilesRictor <- StramenopilesRictor %>% filter(sca >= 100 & scd >= 100)%>%
  group_by(Organism_Taxonomic_ID) %>%
  slice_max(scd) %>%
  slice_max(evd)%>%
  ungroup
StramenopilesRictor <- distinct(StramenopilesRictor, Organism_Taxonomic_ID, .keep_all = TRUE)

RhizariaRictor <- merge(RhizariaRictor, RhizariaNames[c("Organism_Taxonomic_ID", "Organism.Name", "Accn")], by = "Accn")
RhizariaRictor <- RhizariaRictor %>% filter(sca >= 100 & scd >= 100)%>%
  group_by(Organism_Taxonomic_ID) %>%
  slice_max(scd) %>%
  slice_max(evd)%>%
  ungroup
RhizariaRictor <- distinct(RhizariaRictor, Organism_Taxonomic_ID, .keep_all = TRUE)




write.table(AlveolataRictor$tar, file = "~/GitHub/TOR_phylogenetics/IDs/AlveolataRictor.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
write.table(StramenopilesRictor$tar, file = "~/GitHub/TOR_phylogenetics/IDs/StramenopileRictor.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
