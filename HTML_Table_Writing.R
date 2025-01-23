
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

library(xtable)

install.packages("tableHTML")
library(tableHTML)

HTML <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Project.csv")




HTML %>%rename("Super Group" = Super.Group) %>% filter(`Super Group` == "Streptophyta") %>% view()

write_tableHTML(tableHTML(HTML), file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Table.html")



CleanedHTML<- HTML %>%
  rename("Number" = X, "Accession" = Accn, "Super Group"=Super.Group, "Tax ID" = Organism_Taxonomic_ID, "Organism Name" = Organism_Name) %>%
  rename("Completeness Score"=C.score, "Fragmentation Score" = Frag.score, "Class" = Class.name, "Phylum" = Phylum.name, "Order" = Order.name)%>%
  rename("Family"=Family.name, "Genus" = Genus.name) %>%
  relocate(`Super Group`, .before = Class)%>%
  relocate(Accession, .after = Genus)%>%
  kbl(caption = "Complete Table of Searched Species with Relevant Information") %>%
  kable_classic(full_width = F, html_font = "Cambria")

readr::write_file(CleanedHTML, "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/kable_out.html")
