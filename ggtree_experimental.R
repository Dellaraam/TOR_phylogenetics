# Phylogenetic Tree Experimentation
# CTRL+SHIFT+ENTER to run all


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

#Read in a fasta file for an experiment
#Remove anything beyond the specific target code for the protein
RictorMSA <- read.fasta('C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/SAR_MSA/Combined_RICTOR_SAR_CLUSTALO_MSA.fa', type = "AA")
names(RictorMSA) <- trimws(names(RictorMSA), whitespace = "/.*")
names(RictorMSA)


CombinedTaxonomy <- rbind(TaxonomyAlveolata,TaxonomyStramenopiles, TaxonomyRhizaria, TaxonomyDiscoba, TaxonomyMetamonada, FullTaxonomyChlorophyta, FullTaxonomyRhodophyta, FullTaxonomyStreptophyta)


tree <- read.tree('C:/Users/kajoh/Desktop/Verification_Proteins/TOR_File.nwk')
str(tree)
tree
tree$tip.label <- trimws(tree$tip.label, whitespace = "/.*")
 # tree$tip.label[i] <- trimws(tree$tip.label[i], whitespace = "/.*")
tempdata <- full_join(TORStramenopiles, TORAlveolata)
tempdata <- full_join(tempdata, TORRhizaria)
tempdata <- relocate(tempdata, tar)
# Taxonomy Information
TaxonomyData <- rbind(TaxonomyStramenopiles, TaxonomyAlveolata,TaxonomyRhizaria)
tempdata <- tempdata %>% left_join(select(TaxonomyStramenopiles, `Class name`,`Phylum name`,`Order name`, `Family name`, `Genus name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
# Important rule: the first column of the dataframe "MUST" be the same values as the tip labels

#Generate Test tree
p1 <- ggtree(tree, layout = "circular", branch.length = "none")
p2 <- p1 %<+% tempdata +geom_tiplab(aes(label = Organism.Name, color = `Family name`), size=2)
p2


# RICTOR Phylogenetics Tree
RictorTree <- read.tree('~/GitHub/TOR_phylogenetics/Newick_Files/Combined_SAR_RICTOR.nwk')
RictorTree$tip.label <- trimws(RictorTree$tip.label, whitespace = "/.*")
RictorData <- full_join(RICTORStramenopiles,RICTORAlveolata)
RictorData <- full_join(RictorData, RICTORRhizaria)
RictorData <- RictorData %>% left_join(select(TaxonomyData, `Class name`,`Phylum name`,`Order name`, `Family name`, `Genus name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
RictorData <- relocate(RictorData, tar)
# Generate RICTOR tree SAR
RTree <- ggtree(RictorTree,branch.length = "none")
RTree <- RTree %<+% RictorData + geom_tiplab(aes(label = Organism.Name, color = `Order name`), size=2, offset=3, align = TRUE) + labs(title = "Phylogenetics of RICTOR Protein in SAR Superclade") + geom_nodepoint()
RTree
# CombinedPlot <- msaplot(RTree, RictorMSA)
# CombinedPlot


# TOR Phylogenetics Tree SAR
TORTree <- read.tree('~/GitHub/TOR_phylogenetics/Newick_Files/TOR_SAR.nwk')
TORTree$tip.label <- trimws(TORTree$tip.label, whitespace = "/.*")
TORData <- full_join(cleanedTORStramenopiles, cleanedTORAlveolata)
TORData <- full_join(TORData, cleanedTORRhizaria)
TORData <- TORData %>% left_join(select(TaxonomyData, `Class name`,`Phylum name`,`Order name`, `Family name`, `Genus name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
TORData <- relocate(TORData, tar)

TTree <- ggtree(TORTree,branch.length = "none")
TTree <- TTree %<+% TORData + geom_tiplab(aes(label = Organism.Name, color = `Order name`), size=2, offset=3, align = TRUE) + labs(title = "Phylogenetics of TOR Protein in SAR Superclade") + geom_nodepoint()
TTree

# Raptor Phylogenetics SAR
RAPTORTree <- read.tree("~/GitHub/TOR_phylogenetics/Newick_Files/RAPTOR_SAR.nwk")
RAPTORTree$tip.label <- trimws(RAPTORTree$tip.label, whitespace = "/.*")
RAPTORData <- full_join(cleanedRAPTORAlveolata, cleanedRAPTORStramenopiles)
RAPTORData <- full_join(RAPTORData, cleanedRAPTORRhizaria)
RAPTORData <- RAPTORData %>% left_join(select(TaxonomyData, `Class name`,`Phylum name`,`Order name`, `Family name`, `Genus name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
RAPTORData <- relocate(RAPTORData, tar)

RPTree <- ggtree(RAPTORTree, branch.length = "none")
RPTree <- RPTree %<+% RAPTORData + geom_tiplab(aes(label = Organism.Name, color = `Order name`), size=2, offset=3, align = TRUE)+
  labs(title = "Phylogenetics of RAPTOR Protein in SAR Superclade") + geom_nodepoint()+
  geom_polygon(aes(color = `Order name`, fill = `Order name`, x = 0, y = 0))+
  geom_nodepoint()+
  geom_text(aes(label=node, size = 5, vjust = 1))
RPTree

# SIN1 Phylogenetics SAR
SINTree <- read.tree('~/GitHub/TOR_phylogenetics/Newick_Files/SIN1_SAR.nwk')
SINTree$tip.label <- trimws(SINTree$tip.label, whitespace = "/.*")
SINData <- full_join(cleanedSIN1Alveolata,cleanedSIN1Stramenopiles)
SINData <- SINData %>% left_join(select(TaxonomyData, `Class name`,`Phylum name`,`Order name`, `Family name`, `Genus name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
SINData <- relocate(SINData,tar)

STree <- ggtree(SINTree, branch.length = "none")
STree <- STree %<+% SINData + geom_tiplab(aes(label = Organism.Name, color = `Order name`), size=3, offset=.5, align = TRUE) + labs(title = "Phylogenetics of SIN1 Protein in SAR Superclade") + geom_nodepoint()+geom_text(aes(label=node, size = 5, vjust = 1))
STree


# ggsave("C:/Users/kajoh/Desktop/RictorGraph.pdf", CombinedPlot, width = 50, height = 50, units = "cm", limitsize = FALSE)
# Combined Tree for TOR proteins



CombinedTree <- read.tree("~/GitHub/TOR_phylogenetics/Newick_Files/Project_TOR.nwk")
CombinedTree$tip.label<- trimws(CombinedTree$tip.label, whitespace = "/.*")
CData <- full_join(cleanedTORAlveolata,cleanedTORStramenopiles)
CData <- full_join(CData, cleanedTORRhizaria)
CData <- full_join(CData, TORStreptophyta)
CData <- full_join(CData, TORChlorophyta)
CData <- full_join(CData, TORRhizaria)
CData <- full_join(CData, TORDiscoba)
CData <- full_join(CData, TORMetamonada)
CData <- CData %>% left_join(select(CombinedTaxonomy, `Group name`, `Class name`,`Phylum name`,`Order name`, `Family name`, `Genus name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
CData <- relocate(CData, tar)


CTree <- ggtree(CombinedTree, layout = "circular", branch.length = "none") + xlim(-20,NA)
CTree <- CTree %<+% CData+
  geom_tiplab(aes(label = Organism_Name, color = `Phylum name`), size=2,show.legend = FALSE)+
  geom_polygon(aes(color = `Phylum name`, fill = `Phylum name`, x = 0, y = 0))+
  labs(title = "Phylogenetics of TOR Protein")
CTree


# Combined Tree for RICTOR Proteins

RictorCombined <- read.tree("~/GitHub/TOR_phylogenetics/Newick_Files/Rictor_All.nwk")
RictorCombined$tip.label <- trimws(RictorCombined$tip.label, whitespace = "/.*")
RictorData <- full_join(cleanedRICTORAlveolata, cleanedRICTORStramenopiles)
RictorData <- RictorData %>% full_join(cleanedRICTORRhizaria)%>%
  full_join(RICTORRhodophyta)%>%
  full_join(RICTORStreptophyta)%>%
  full_join(RICTORChlorophyta)%>%
  full_join(RICTORRhodophyta)%>%
  full_join(RICTORMetamonada)%>%
  full_join(RICTORDiscoba)

RictorData <- RictorData %>% left_join(select(CombinedTaxonomy, `Group name`, `Class name`,`Phylum name`,`Order name`, `Family name`, `Genus name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
RictorData <- relocate(RictorData, tar)

RCtree <- ggtree(RictorCombined, layout = "circular", branch.length = "none")
RCtree <- RCtree <- RCtree %<+% RictorData+
  geom_tiplab(aes(label = Organism_Name, color = `Phylum name`), size=2,show.legend = FALSE)+
  geom_polygon(aes(color = `Phylum name`, fill = `Phylum name`, x = 0, y = 0))+
  labs(title = "Phylogenetics of RICTOR Protein")+
  #geom_text(aes(label=node, size = 1, vjust = 1))+
  geom_highlight(node = 312, fill = 'steelblue', color = "white", type = "roundrect", alpha=0.2, extend = 15)+
  geom_highlight(node = 232, fill = 'green', color = "white", type = "roundrect", alpha=0.2, extend = 15)
RCtree






