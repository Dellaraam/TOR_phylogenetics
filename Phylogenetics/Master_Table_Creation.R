#Creation of the master table

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


MassiveTable <- left_join(HTML,Ndf[c("SIN1All","SIN1Domain","RICTORAll","RICTORDomain","RAPTORAll","RAPTORDomain","LST8All","LST8Domain","TORAll","TORDomain","Organism_Taxonomic_ID")], by = "Organism_Taxonomic_ID")
MassiveTable <- left_join(MassiveTable, metabolicTable[c("M.Strategy","Organism_Taxonomic_ID")], by = "Organism_Taxonomic_ID")

MassiveTable <- distinct(MassiveTable, Organism_Taxonomic_ID, .keep_all = TRUE)


write.csv(MassiveTable, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Master_Table.csv")


