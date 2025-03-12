library(tidyverse)


# Goal is to combine all of the combined data sets into one
# Cleanup anything that may be off as well

OriginalCompleteTable <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Project.csv")
OriginalCompleteTable <- distinct(OriginalCompleteTable, Organism_Taxonomic_ID, .keep_all = TRUE)
OriginalCompleteTable <- OriginalCompleteTable %>%
  rename(Organism.Name = "Organism_Name") %>%
  filter(Super.Group != "Chlorophyta") %>%
  filter(Super.Group != "Stramenopiles") %>%
  filter(Super.Group != "Discoba") %>%
  filter(Super.Group != "Metamonada") %>%
  filter(Super.Group != "Rhizaria") %>%
  mutate(Source = "NCBI")




# Read in the new data to add
UpdatedStramenopiles <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Updated_Stramenopiles_Combined.csv")
UpdatedStramenopiles <- rename(UpdatedStramenopiles, Super.Group = "Group") %>% select(-X)
UpdatedExcavata <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Updated_Excavata_Combined.csv")
UpdatedExcavata <- rename(UpdatedExcavata, Super.Group = "Group") %>% select(-X)
UpdatedChlorophyta <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Updated_Chlorophyta_Combined.csv")
UpdatedChlorophyta <- rename(UpdatedChlorophyta, Super.Group = "Group") %>% select(-X)
UpdatedRhizaria <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Updated_Rhizaria_Combined.csv")
UpdatedRhizaria <- rename(UpdatedRhizaria, Super.Group = "Group") %>% select(-X)



#Bind
NewTable <- rbind(OriginalCompleteTable, UpdatedStramenopiles, UpdatedExcavata, UpdatedChlorophyta, UpdatedRhizaria)
write.csv(NewTable, file ="C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/New_Combined_Table_218.csv")



