
# Updated Metabolic Strategy Table Creation
# Kyle Johnson
# 4/1/2025

source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Library_Script.R")

metabolicStrategies <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/trophic_strategy.csv")
metabolicStrategies <- distinct(metabolicStrategies, Organism_Taxonomic_ID, .keep_all = TRUE)
FinalBusco <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/FinalBusco.csv")
FinalBusco <- FinalBusco %>% select(-X)
HTML <- read_tsv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/HTML.tsv")
HTML <- HTML%>% rename(Super.Group = "Group")
HTML <- left_join(HTML,FinalBusco, by = "Accn")
num <- HTML %>% filter(Super.Group != "Streptophyta")


NoRICTOR <- HTML %>%
  filter(`Super.Group` != "Streptophyta") %>%
  filter(is.na(RICTOR))
NoRICTOR <- left_join(NoRICTOR, metabolicStrategies[c("M.Strategy","Organism_Taxonomic_ID")], by = "Organism_Taxonomic_ID")

NoRICTOR <- mutate(NoRICTOR, HasRICTOR = "NO")

YesRICTOR <- HTML %>%
  filter(`Super.Group` != "Streptophyta") %>%
  filter(!is.na(RICTOR))

YesRICTOR <- left_join(YesRICTOR, metabolicStrategies[c("M.Strategy","Organism_Taxonomic_ID")], by = "Organism_Taxonomic_ID")
YesRICTOR <- distinct(YesRICTOR, Organism_Taxonomic_ID, .keep_all = TRUE)
YesRICTOR <- mutate(YesRICTOR, HasRICTOR = "YES")


FinalMetabolicTable <- rbind(YesRICTOR,NoRICTOR)
write.csv(FinalMetabolicTable, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Final_Metabolic_Table.csv")


