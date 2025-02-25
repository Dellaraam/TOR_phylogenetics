install.packages("tableHTML")
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







#Load in the data
Yes <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Test_Ground/Yes_RICTOR .csv")
Yes %>% count(Yes$M.Strategy)
Yes %>% count(Yes$Super.Group)
Yes <- Yes %>% mutate(Has_Rictor = "Yes")


No <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Test_Ground/No_RICTOR .csv")
No %>%count(No$M.Strategy)
No %>% count(No$Super.Group)
No <- No %>% mutate(Has_Rictor = "No")

# Definitely a strong correlation between autotrophy and not having RICTOR
# Going to make a tree to examine this further

#Make a bound table of Y/N
YesNO <- rbind(Yes, No)



write.table(Yes$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Yes.txt", sep = "\t", row.names = F, col.names = F)
write.table(No$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/No.txt", sep = "\t", row.names = F, col.names = F)
write.table(YesNO$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/YN.txt", sep = "\t", row.names = F, col.names = F)


ggplot(Yes)+
  geom_bar(aes(x = M.Strategy, fill = M.Strategy))

ggplot(No)+
  geom_bar(aes(x = M.Strategy, fill = M.Strategy))
























tree1 <- read.tree(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Trees/YesTreeP.phy")
tree1$tip.label <- gsub("'","", tree1$tip.label)

dataset <- Yes %>% relocate(Organism.Name)
treeplot <- ggtree(tree1, branch.length = "none") + xlim(NA,+16)
treeplot <- treeplot %<+% dataset


Ytreeplot <- treeplot + geom_tiplab( aes(color = M.Strategy), size = 3, show.legend = FALSE)+geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_text(aes(label = node))+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_rootedge()+
  geom_polygon(aes(color = `M.Strategy`, fill = `M.Strategy`, x = 0, y = 0))
Ytreeplot

tree2 <- read.tree(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Trees/NoTreeP.phy")
tree2$tip.label <- gsub("'","", tree2$tip.label)
dataset2 <- No %>% relocate(Organism.Name)
treeplot2 <- ggtree(tree2, branch.length = "none") + xlim(NA, +16)
treeplot2 <- treeplot2 %<+% dataset2

Ntreeplot <- treeplot2 + geom_tiplab( aes(color = M.Strategy), size = 3, show.legend = FALSE)+geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_text(aes(label = node))+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_rootedge()+
  geom_polygon(aes(color = `M.Strategy`, fill = `M.Strategy`, x = 0, y = 0))
Ntreeplot

