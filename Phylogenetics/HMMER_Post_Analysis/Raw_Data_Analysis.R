
# RICTOR RAW DATA Analysis
# Using ggplot2 to determine the cutoff values
# Kyle Johnson
# 4/1/2025



source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Library_Script.R")

#Import the Rictor Raw Data (NCBI)
#Merge the JGI Information together into one table




JGIInfo <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/JGICombinedInfoTable.csv")
JGIInfo <- rename(JGIInfo, `Organism Name` = "Organism.Name", `Organism Taxonomic ID` = "Organism_Taxonomic_ID")

TaxonInfo <- read_tsv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/NCBI_Filtered_All_Information.tsv")
TaxonInfo <- rename(TaxonInfo, Accn = "Assembly Accession")

AlveolataRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/RICTORAlveolata.csv")
StramenopileRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/RICTORStramenopiles.csv")
RhizariaRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/RICTORRhizaria.csv")

AlvJGIRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Alveolata_JGI/AlveolataJGI_RICTOR.csv")
AlvJGIRictor$Accn <- sub("\\_.*", "", AlvJGIRictor$Accn)
StramJGIRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_RICTOR.csv")
StramJGIRictor$Accn <- sub("\\_.*", "", StramJGIRictor$Accn)
RhizariaJGIRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_JGI/RhizariaJGI_RICTOR.csv")
RhizariaJGIRictor$Accn <- sub("\\_.*", "", RhizariaJGIRictor$Accn)





Chlorophyta <- read.csv(file = "")
Streptophyta <-
Rhodophyta <-

DiscobaRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Discoba/RICTOR_Discoba.csv")
MetamonadaRictor <-read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/RICTOR_Metamonada.csv")
  
  
  
  


# SARS RICTOR ------------------------------------------------------------------
AlveolataRictor <- left_join(AlveolataRictor, TaxonInfo[c("Organism Name", "Accn", "Organism Taxonomic ID")], by = "Accn")
AlvJGIRictor <- left_join(AlvJGIRictor, JGIInfo[c("Organism Name", "Accn", "Organism Taxonomic ID")], by = "Accn")
AlveolataRictor <- rbind(AlveolataRictor, AlvJGIRictor)
AlveolataRictor %>% ggplot(aes(x = `scd`, y = `Organism Name`))+
                             geom_jitter()

StramenopileRictor <- left_join(StramenopileRictor,TaxonInfo[c("Organism Name", "Accn", "Organism Taxonomic ID")], by = "Accn")
StramJGIRictor <- left_join(StramJGIRictor, JGIInfo[c("Organism Name", "Accn", "Organism Taxonomic ID")], by = "Accn")
StramenopileRictor <- rbind(StramenopileRictor, StramJGIRictor)
StramenopileRictor %>% ggplot(aes(x = `scd`, y = `Organism Name`))+
  geom_jitter()+
  geom_vline(xintercept=100)

RhizariaRictor <- left_join(RhizariaRictor,TaxonInfo[c("Organism Name", "Accn", "Organism Taxonomic ID")], by = "Accn")
RhizariaRictor %>% ggplot(aes(x = `scd`, y = `Organism Name`))+
  geom_jitter()


# Excavates RICTOR -------------------------------------------------------------
DiscobaRictor <- left_join(DiscobaRictor,TaxonInfo[c("Organism Name", "Accn", "Organism Taxonomic ID")], by = "Accn")
DiscobaRictor %>% ggplot(aes(x = `scd`, y = `Organism Name`))+
  geom_jitter()

MetamonadaRictor <- left_join(MetamonadaRictor,TaxonInfo[c("Organism Name", "Accn", "Organism Taxonomic ID")], by = "Accn")
MetamonadaRictor %>% ggplot(aes(x = `scd`, y = `Organism Name`))+
  geom_jitter()

                           