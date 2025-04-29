#Creation of the master table
library(tidyverse)
#load in the HTML Table

FinalBusco <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/FinalBusco.csv")
FinalBusco <- FinalBusco %>% select(-X)
Taxon <- read.csv("~/Github/TOR_phylogenetics/Combined_Taxonomy.csv")
Taxon <- rename(Taxon, Organism.Name = "Tax.name")
HTML <- read_tsv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/HTML.tsv")
HTML <- HTML%>% rename(Super.Group = "Group")
HTML <- left_join(HTML,FinalBusco, by = "Accn")
# Need to update the numeric table next
Ndf <- read.csv("~/Github/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/NumericTable.csv")
Ndf <- select(Ndf, -X, -Organism.Name)
Ndf <- left_join(Ndf, Taxon[c("Organism.Name", "Organism_Taxonomic_ID")], by = "Organism_Taxonomic_ID")
Ndf <- relocate(Ndf, Organism.Name, .after = Organism_Taxonomic_ID)
Ndf <- left_join(Ndf,FinalBusco, by = "Accn")
Ndf <- distinct(Ndf, Organism_Taxonomic_ID, .keep_all = TRUE)


#Load in the Metabolic table
metabolicTable <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Final_Metabolic_Table.csv")
par = which(metabolicTable$Organism.Name == "Galdieria partita")
yel = which(metabolicTable$Organism.Name == "Galdieria yellowstonensis")
metabolicTable[par, "M.Strategy"] <- "Mixotroph"
metabolicTable[yel, "M.Strategy"] <- "Mixotroph"


MassiveTable <- left_join(HTML,Ndf[c("SIN1All","SIN1Domain","RICTORAll","RICTORDomain","RAPTORAll","RAPTORDomain","LST8All","LST8Domain","TORAll","TORDomain","Organism_Taxonomic_ID")], by = "Organism_Taxonomic_ID")
MassiveTable <- left_join(MassiveTable, metabolicTable[c("M.Strategy","Organism_Taxonomic_ID")], by = "Organism_Taxonomic_ID")

#Corrections to the metabolic table
#Alveolates



MassiveTable <- distinct(MassiveTable, Organism_Taxonomic_ID, .keep_all = TRUE)
MassiveTable <- MassiveTable %>%mutate(M.Strategy = if_else(Super.Group == "Streptophyta", "Autotrophic", M.Strategy))


MassiveTable <- MassiveTable %>% mutate(M.Strategy = if_else(Organism.Name == "Amoebophrya sp. AT5.2","Parasite",M.Strategy))%>%
  mutate(M.Strategy = if_else(Organism.Name == "Chromera velia CCMP2878","Autotrophic",M.Strategy))%>%
  mutate(M.Strategy = if_else(Organism.Name == "Fugacium kawagutii","Endosymbiotic",M.Strategy))%>%
  mutate(M.Strategy = if_else(Organism.Name == "Breviolum minutum","Endosymbiotic",M.Strategy))%>%
  mutate(M.Strategy = if_else(Organism.Name == "Paramecium tetraurelia strain d4-2","Heterotroph",M.Strategy))%>%
  mutate(M.Strategy = if_else(Organism.Name == "Galdieria partita", "Mixotroph", M.Strategy))%>%
  mutate(M.Strategy = if_else(Organism.Name == "Galdieria yellowstonensis", "Mixotroph", M.Strategy))%>%
  mutate(M.Strategy = if_else(Organism.Name == "Rehmannia glutinosa", "Parasite", M.Strategy))%>%
  mutate(M.Strategy = if_else(Genus.name == "Symbiodinium","Endosymbiotic", M.Strategy, missing = M.Strategy))




write.csv(MassiveTable, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Master_Table.csv")


