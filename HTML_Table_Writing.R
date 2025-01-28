
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


Taxon <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Combined_Taxonomy.csv")
HTML <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Project.csv")




HTML %>%rename("Super Group" = Super.Group) %>% filter(`Super Group` == "Alveolata") %>% filter(!is.na(RAPTOR)) %>% relocate(Group.name) %>% view()
Stramenopiles <- HTML %>% filter(Super.Group == "Stramenopiles")
Alveolata <- HTML %>% filter(Super.Group == "Alveolata")
Rhizaria <- HTML %>% filter(Super.Group == "Rhizaria")

Chlorophyta <- HTML %>% filter(Super.Group == "Chlorophyta")
Rhodophyta <- HTML %>% filter(Super.Group == "Rhodophyta")
Streptophyta <- HTML %>% filter(Super.Group == "Streptophyta")










write.table(HTML$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/All.txt", sep = "\t", row.names = F, col.names = F)
write.table(Alveolata$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Alveolata.txt", sep = "\t", row.names = F, col.names = F)
write.table(Stramenopiles$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Stramenopiles.txt", sep = "\t", row.names = F, col.names = F)
write.table(Rhizaria$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Rhizaria.txt", sep = "\t", row.names = F, col.names = F)
write.table(Chlorophyta$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Chlorophyta.txt", sep = "\t", row.names = F, col.names = F)
write.table(Streptophyta$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Streptophyta.txt", sep = "\t", row.names = F, col.names = F)
write.table(Rhodophyta$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Rhodophyta.txt", sep = "\t", row.names = F, col.names = F)


write_tableHTML(tableHTML(HTML), file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Table.html")





tree <- read.tree(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Trees/AllTreeP.phy")
tree$tip.label <- gsub("'","", tree$tip.label)
tree$tip.label
#Relocate the Organism name to the beginning before creating the tree
HTML <- HTML %>% relocate(Organism_Name)
AllTree <- ggtree(tree, layout = "circular", branch.length = "none")
AllTree <- AllTree %<+% HTML
AllTree+aes(color = Super.Group)+ geom_tiplab(aes(color = RICTOR))+geom_text(aes(label=node))


# ------------------------------------------------------------------------------
StramenopileTree <- read.tree(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Trees/StramenopileTreeP.phy")
StramenopileTree$tip.label
StramenopileTree$tip.label <- gsub("'","", StramenopileTree$tip.label)
StramenopileTree$tip.label

Stramenopiles <- Stramenopiles %>% relocate(Organism_Name)
STP <- ggtree(StramenopileTree, branch.length = "none")
STP <- STP  %<+% Stramenopiles
write.jtree(Output, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Trees/TestTree.tree")
STP + geom_tiplab(aes(color = RAPTOR)) + geom_text(aes(label = node))




# We may need a dataframe that has all of the specific nodes that we are interested in. How to do this however





CleanedHTML<- HTML %>%
  rename("Number" = X, "Accession" = Accn, "Super Group"=Super.Group, "Tax ID" = Organism_Taxonomic_ID, "Organism Name" = Organism_Name) %>%
  rename("Completeness Score"=C.score, "Fragmentation Score" = Frag.score, "Class" = Class.name, "Phylum" = Phylum.name, "Order" = Order.name)%>%
  rename("Family"=Family.name, "Genus" = Genus.name) %>%
  relocate(`Super Group`, .before = Class)%>%
  relocate(Accession, .after = Genus)%>%
  kbl(caption = "Complete Table of Searched Species with Relevant Information") %>%
  kable_classic(full_width = F, html_font = "Cambria")

readr::write_file(CleanedHTML, "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/kable_out.html")
