if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ggtree")
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Biostrings")
library("Biostrings")
library(tidyverse)
library(ggplot2)
library(ggtree) #Have citation requirements for their usage. See attached message
# LG Wang, TTY Lam, S Xu, Z Dai, L Zhou, T Feng, P Guo, CW Dunn, BR Jones, T Bradley, H Zhu, Y Guan, Y Jiang, G Yu. treeio: an R package for
# phylogenetic tree input and output with richly annotated and associated data. Molecular Biology and Evolution. 2020, 37(2):599-603. doi:
#  10.1093/molbev/msz240
library(treeio)
library(ggrepel)
library(gridExtra)
library(plotly)
library(kableExtra)
library(knitr)
library(patchwork)
library(xtable)

install.packages("tableHTML")
library(tableHTML)

install.packages("useful")
library(useful)


# Read in all of the Combined Data
# Add in a source column that denotes where it is from (if not there already)
CombinedStrep <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Combined_Streptophyta.csv")
CombinedStrep <- CombinedStrep %>% select(-X) %>% mutate(Source = "NCBI") %>% rename(Organism.Name = "Organism_Name", Group = "Phylum", Organism.Taxonomic.ID = "Organism_Taxonomic_ID") %>% distinct(Organism.Taxonomic.ID, .keep_all = TRUE)

CombinedExc <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Updated_Excavata_Combined.csv")
CombinedExc <- CombinedExc %>% select(-X) %>% rename(Organism.Taxonomic.ID = "Organism_Taxonomic_ID") %>% distinct(Organism.Taxonomic.ID, .keep_all = TRUE)

CombinedAlv <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Combined_Alveolata.csv")
CombinedAlv <- CombinedAlv %>% select(-X) %>% mutate(Source = "NCBI") %>% rename(Organism.Taxonomic.ID = "Organism_Taxonomic_ID") %>% distinct(Organism.Taxonomic.ID, .keep_all = TRUE)

CombinedChlor <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Updated_Chlorophyta_Combined.csv")
CombinedChlor <- CombinedChlor %>% select(-X) %>% rename(Organism.Taxonomic.ID = "Organism_Taxonomic_ID") %>% distinct(Organism.Taxonomic.ID, .keep_all = TRUE)

CombinedRhiz <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Updated_Rhizaria_Combined.csv")
CombinedRhiz <- CombinedRhiz %>% select(-X)%>% rename(Organism.Taxonomic.ID = "Organism_Taxonomic_ID") %>% distinct(Organism.Taxonomic.ID, .keep_all = TRUE)

CombinedStram <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Updated_Stramenopiles_Combined.csv")
CombinedStram <- CombinedStram %>% select(-X)%>% rename(Organism.Taxonomic.ID = "Organism_Taxonomic_ID") %>% distinct(Organism.Taxonomic.ID, .keep_all = TRUE)

CombinedRhodo <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Combined_Rhodophyta.csv") 
CombinedRhodo <- CombinedRhodo %>% select(-X, -C.score.y, -Frag.score.y) %>%
  rename(C.score = "C.score.x", Frag.score = "Frag.score.x", Group = "Phylum") %>%
  mutate(Source = "NCBI") %>% rename(Organism.Taxonomic.ID = "Organism_Taxonomic_ID", Organism.Name = "Organism_Name") %>%
  distinct(Organism.Taxonomic.ID, .keep_all = TRUE)
  
#Check the names
names(CombinedAlv) %in% names(CombinedChlor)
names(CombinedAlv) %in% names(CombinedExc)
names(CombinedAlv) %in% names(CombinedRhiz)
names(CombinedAlv) %in% names(CombinedRhodo)
names(CombinedAlv) %in% names(CombinedStram)
names(CombinedAlv) %in% names(CombinedStrep)
# All of the combined data is cleaned and ready to go and be merged together
CompleteTable <- rbind(CombinedAlv, CombinedChlor, CombinedExc, CombinedRhiz, CombinedRhodo, CombinedStram, CombinedStrep)
CompleteTable <- CompleteTable %>% distinct(Organism.Taxonomic.ID, .keep_all = TRUE)

write.csv(CompleteTable, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/CompleteTable.csv", row.names = FALSE)
