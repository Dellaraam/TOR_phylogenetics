# Created by:
# Kyle Johnson
# Dellaraam Pourkeramati
#







# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("ggtree")
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("Biostrings")
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


proteinPossible <- function(df,OrganismName,Protein){
  
  temp <- df %>% filter(Organism.Name == OrganismName)
  df <- subset(df, Organism.Name != OrganismName)
  temp[,Protein] <-"P"
  df <- rbind(df,temp)
  return(df)
}

proteinPossibleSwap <- function(dfNo,dfYes,OrganismName, Protein){
  
  temp<- dfNo %>% filter(Organism.Name == OrganismName)
  temp[,Protein] <-"P"
  dfYes <- rbind(dfYes, temp)
  return(dfYes)
}








Taxon <- read.csv("~/Code/TOR_phylogenetics/Combined_Taxonomy.csv")
Taxon <- rename(Taxon, Organism.Name = "Tax.name")
HTML <- read.csv("~/Code/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/New_Combined_Table_218.csv")
# Need to update the numeric table next
Ndf <- read.csv("~/Code/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/NumericTable.csv")
Ndf <- select(Ndf, -X, -Organism.Name)
Ndf <- left_join(Ndf, Taxon[c("Organism.Name", "Organism_Taxonomic_ID")], by = "Organism_Taxonomic_ID")
Ndf <- relocate(Ndf, Organism.Name, .after = Organism_Taxonomic_ID)


# Replace the names in the HTML file with the Taxon as they are more correct
HTML <- select(HTML, -Organism.Name, -X)
HTML <- left_join(HTML, Taxon[c("Organism.Name", "Organism_Taxonomic_ID")], by = "Organism_Taxonomic_ID")
HTML <- relocate(HTML, Organism.Name, .after = Organism_Taxonomic_ID)

Ndf <- left_join(Ndf, HTML[c("C.score","Frag.score","Organism_Taxonomic_ID")], by = "Organism_Taxonomic_ID")

# ------------------------------------------------------------------------------
# Lets remove some of the RICTORS
# Doing further data cleanup

# HTML$RICTOR[625] <- NA
# HTML$RICTOR[625]
# HTML$RICTOR[506] <- NA
# HTML$RICTOR[506]
# HTML$RICTOR[629] <- NA
# HTML$RICTOR[629]
# HTML$RICTOR[411] <- NA
# HTML$RICTOR[411]
# HTML$RICTOR[834] <- NA
# HTML$RICTOR[834]
# HTML$RICTOR[836] <- NA
# HTML$RICTOR[836]
# 
# # now for SIN1
# HTML$SIN1[625] <- NA
# HTML$SIN1[625]
# HTML$SIN1[629] <- NA
# HTML$SIN1[625]
# HTML$SIN1[691] <- NA
# HTML$SIN1[581] <- NA
# HTML$SIN1[675] <- NA
# HTML$SIN1[676] <- NA
# HTML$SIN1[677] <- NA
 # -----------------------------------------------------------------------------
# Current Goal is to replace all of the names in the HTML file (really need to rename that)
# Everything should be replaced by what is found within the Taxon file
# That should make everything work correctly





HTML %>%rename("Super Group" = Super.Group) %>% filter(`Super Group` == "Alveolata") %>% filter(!is.na(RAPTOR))
HTML <- select(HTML, -D1)
# Add in the possible protein data here
# Will come up with a more elegant solution in the future
# Using results gather by Dell and her Diamond Analysis work

HTML <- HTML %>%
  proteinPossible("Effrenium voratum","RAPTOR")%>%
  proteinPossible("Effrenium voratum","LST8") %>%
  proteinPossible("Durusdinium trenchii","RAPTOR")%>%
  proteinPossible("Paramecium primaurelia","RICTOR")%>%
  proteinPossible("Bonamia ostreae","RAPTOR")%>%
  proteinPossible("Lotharella oceanica","RICTOR")%>%
  proteinPossible("Lotharella oceanica","RAPTOR")%>%
  proteinPossible("Lotharella oceanica","TOR")%>%
  proteinPossible("Lotharella oceanica","LST8")%>%
  proteinPossible("Paulinella micropora","RAPTOR")%>%
  proteinPossible("Paulinella micropora","TOR")%>%
  proteinPossible("Paulinella micropora","LST8")%>%
  proteinPossible("Paulinella micropora","TOR")%>%
  proteinPossible("Tetradesmus obliquus","RAPTOR")%>%
  proteinPossible("Pseudo-nitzschia multistriata","LST8")%>%
  proteinPossible("Triticum urartu","RAPTOR")%>%
  proteinPossible("Bienertia sinuspersici","RAPTOR") %>%
  proteinPossible("Nannochloropsis gaditana","RICTOR")%>%
  proteinPossible("Phytophthora megakarya","RICTOR")%>%
  proteinPossible("Phytophthora megakarya","LST8")%>%
  proteinPossible("Euglena gracilis","RICTOR")%>%
  proteinPossible("Nannochloropsis gaditana CCMP526","RICTOR")%>%
  proteinPossible("Monocercomonoides exilis","RICTOR")%>%
  proteinPossible("Nelumbo nucifera", "LST8")%>%
  proteinPossible("Nelumbo nucifera","RAPTOR")%>%
  proteinPossible("Nelumbo nucifera", "TOR")%>%
  proteinPossible("Apium graveolens", "RAPTOR")%>%
  proteinPossible("Apium graveolens","LST8")%>%
  proteinPossible("Apium graveolens","TOR")%>%
  proteinPossible("Babesia caballi","TOR")%>%
  proteinPossible("Ceratopteris richardii","LST8")%>%
  proteinPossible("Stephania cephalantha","LST8")%>%
  proteinPossible("Morus notabilis","LST8")%>%
  proteinPossible("Capsicum chinense","LST8")%>%
  proteinPossible("Carex littledalei","LST8")%>%
  proteinPossible("Gossypium klotzschianum","LST8")%>%
  proteinPossible("Cladocopium goreaui","RAPTOR")

#Add cladocopium goreaui RAPTOR

  
  
# Modifications to some of the organism names and also the supergroups
which(HTML$Organism.Name == "Chlamydomonas reinhardtii", arr.ind = TRUE)
which(HTML$Organism.Name == "Neopyropia yezoensis", arr.ind = TRUE)
which(HTML$Organism.Name == "Euglena gracilis", arr.ind = TRUE)
which(HTML$Organism.Name == "Giardia lamblia ATCC 50803")
HTML$Super.Group[800] <- "Discoba"
HTML$Organism.Name[800]
HTML$Super.Group[695] <- "Metamonada"
HTML$Organism.Name[695]
HTML$Organism.Name[495]
HTML$Organism.Name[495] <- "Pyropia yezoensis"
HTML$Organism.Name[696]
HTML$SIN1[696] <- NA
HTML$SIN1[696]
#Look into aphanomyces for RICTOR. What can we conclude about it?






SARnoRICTOR <- HTML %>%
  filter(`Super.Group` != "Streptophyta") %>%
  filter(`Super.Group` != "Chlorophyta")%>%
  filter(`Super.Group` != "Rhodophyta") %>%
  filter(`Super.Group` != "Excavata") %>%
  filter(`Super.Group` != "Discoba") %>%
  filter(`Super.Group` != "Metamonada") %>% 
  filter(is.na(RICTOR)) %>%
  view()

SARYRICTOR <- HTML %>%
  filter(`Super.Group` != "Streptophyta") %>%
  filter(`Super.Group` != "Chlorophyta")%>%
  filter(`Super.Group` != "Rhodophyta") %>%
  filter(`Super.Group` != "Excavata") %>%
  filter(`Super.Group` != "Discoba") %>%
  filter(`Super.Group` != "Metamonada") %>% 
  filter(!is.na(RICTOR)) %>%
  view()


#HTML %>% filter(`Super.Group` != "Streptophyta") %>% filter(!is.na(RICTOR)) %>% view()


Chlorophyta <- HTML %>% filter(Super.Group == "Chlorophyta")
Rhodophyta <- HTML %>% filter(Super.Group == "Rhodophyta")
Streptophyta <- HTML %>% filter(Super.Group == "Streptophyta")
Discoba <- HTML %>% filter(Super.Group == "Discoba")
Metamonada <- HTML %>% filter(Super.Group == "Metamonada")
Alveolata <- HTML %>% filter(Super.Group  == "Alveolata")
Stramenopiles <- HTML %>% filter(Super.Group == "Stramenopiles")
Rhizaria <- HTML %>% filter(Super.Group == "Rhizaria")
Excavata <- HTML %>% filter(Super.Group != "Chlorophyta") %>%
  filter(Super.Group != "Rhodophyta") %>%
  filter(Super.Group != "Streptophyta") %>%
  filter(Super.Group != "Alveolata") %>%
  filter(Super.Group != "Stramenopiles") %>%
  filter(Super.Group != "Rhizaria")
ProbableOrganisms <- HTML %>% filter(RICTOR == "P"| RAPTOR == "P"| LST8 == "P"| SIN1 == "P" | TOR == "P")

# This is where we will have a section for the numeric data that is divided out
# One thing that needs to be done is to change the excavte data to discoba/metamonada
# 
which(Ndf$Group == "Excavata")
which(Ndf$Organism.Name == "Euglena gracilis", arr.ind = TRUE)
Ndf$Group[822] <- "Discoba"
which(Ndf$Organism.Name == "Giardia lamblia ATCC 50803")
Ndf$Group[823] <- "Metamonada"




write.table(HTML$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/All.txt", sep = "\t", row.names = F, col.names = F)
write.table(Alveolata$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Alveolata.txt", sep = "\t", row.names = F, col.names = F)
write.table(Stramenopiles$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Stramenopiles.txt", sep = "\t", row.names = F, col.names = F)
write.table(Rhizaria$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Rhizaria.txt", sep = "\t", row.names = F, col.names = F)
write.table(Chlorophyta$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Chlorophyta.txt", sep = "\t", row.names = F, col.names = F)
write.table(Streptophyta$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Streptophyta.txt", sep = "\t", row.names = F, col.names = F)
write.table(Rhodophyta$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Rhodophyta.txt", sep = "\t", row.names = F, col.names = F)
write.table(Discoba$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Discoba.txt", sep = "\t", row.names = F, col.names = F)
write.table(Metamonada$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Metamonada.txt", sep = "\t", row.names = F, col.names = F)
write.table(Excavata$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Excavata.txt", sep = "\t", row.names = F, col.names = F)


write.table(SARnoRICTOR$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/NoSARRICTOR.txt", sep = "\t", row.names = F, col.names = F)
write.table(SARYRICTOR$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/YSARRICTOR.txt", sep = "\t", row.names = F, col.names = F)

write.csv(HTML, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Test_Ground/Updated_Table.csv")
write.csv(ProbableOrganisms, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Test_Ground/Probable_Table.csv")

# tree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/AllTreeP.phy")
# tree$tip.label <- gsub("'","", tree$tip.label)
# tree$tip.label
# #Relocate the Organism name to the beginning before creating the tree
# HTML <- HTML %>% relocate(Organism_Name)
# AllTree <- ggtree(tree, layout = "circular", branch.length = "none")
# AllTree <- AllTree %<+% HTML
# AllTree+aes(color = Super.Group)+ geom_tiplab(aes(color = RICTOR))+geom_text(aes(label=node))


# ------------------------------------------------------------------------------

# Lets make a color palette that can be used going forward
# If we want to change anything color wise, feel free to do so
# These colors were chosen just for simplicity sake
pal <- c(
  "H" = "red",
  "M" = "blue",
  "L" = "green",
  "P" = "orange",
  "NA" = "grey"
)

# ------------------------------------------------------------------------------
#Test Ground

DiscobaNum <- filter(Ndf, Group == "Discoba")
DiscobaNum %>%
  ggplot(aes(x = SIN1All, y = SIN1Domain, color = C.score))+
  geom_point()






# ------------------------------------------------------------------------------


StramenopileTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/StramenopileTreeP.phy")
StramenopileTree$tip.label
StramenopileTree$tip.label <- gsub("'","", StramenopileTree$tip.label)
StramenopileTree$tip.label

Stramenopiles <- Stramenopiles %>% relocate(Organism.Name)
STP <- ggtree(StramenopileTree, branch.length = "none", ladderize = FALSE)+xlim(NA,+15)
STP <- STP  %<+% Stramenopiles
#RICTOR Stramenopiles
RISP <- STP + geom_tiplab(aes(color = RICTOR), size = 3, show.legend = TRUE)+geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  # geom_text(aes(label = node))+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_rootedge()+
  # geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  #geom_text(aes(label=node))+
  geom_point2(aes(subset=(node==19)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  geom_point2(aes(subset=(node==153)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==142)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==153)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==70)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  geom_point2(aes(subset=(node==91)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  geom_point2(aes(subset=(node==92)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  geom_point2(aes(subset=(node==7)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA))
RISP


#Sin1 Stramenopiles
SSP <- STP + geom_tiplab(aes(color = SIN1), size = 3, show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(fill = `SIN1`, x = 0, y = 0))+
  #geom_text(aes(label = node))+
  geom_point2(aes(subset=(node==144)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==153)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==134)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==115)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==142)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==30)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  geom_point2(aes(subset=(node==31)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  geom_point2(aes(subset=(node==40)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  geom_point2(aes(subset=(node==41)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  geom_point2(aes(subset=(node==140)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  geom_point2(aes(subset=(node==72)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  geom_point2(aes(subset=(node==70)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
  
  
SSP
# Note: Add in the dark blue locations to denote a possible error location
SRAPTORS <- STP + geom_tiplab(aes(color = RAPTOR), size = 3, show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  #geom_text(aes(label = node))+
  # geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==20)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==91)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==92)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
SRAPTORS

STORS <- STP + geom_tiplab(aes(color = TOR), size = 3, show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  # geom_text(aes(label = node))+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_point2(aes(subset=(node==91)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==92)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
STORS

SLST8S <- STP + geom_tiplab(aes(color = LST8), size = 3)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  #geom_text(aes(label = node))+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_point2(aes(subset=(node==91)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==92)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==52)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==99)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==74)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
  
SLST8S




# Generating the plots here
RISP+SSP
(SRAPTORS+SLST8S)
STORS




# ------------------------------------------------------------------------------




AlveolataTree <- read.tree(file = "~/Code/TOR_phylogenetics/Trees/AlveolataTreeP.phy")
AlveolataTree$tip.label
AlveolataTree$tip.label <- gsub("'","", AlveolataTree$tip.label)
AlveolataTree$tip.label
Alveolata <- Alveolata %>% relocate(Organism.Name)
AlvP <- ggtree(AlveolataTree, branch.length = "none", ladderize = FALSE)+xlim(NA,+15)
AlvP <- AlvP  %<+% Alveolata
# SIN1
ASinP <- AlvP + geom_tiplab(aes(color = SIN1), size = 3, show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  # geom_text(aes(label=node))+
  geom_point2(aes(subset=(node==165)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==139)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==137)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==134)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==131)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==10)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)

ASinP

# RICTOR
ARIC <- AlvP + geom_tiplab(aes(color = RICTOR), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  #geom_text(aes(label=node))+
  # geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==139)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==118)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==167)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==9)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
ARIC

# RAPTOR

ARAPT <- AlvP + geom_tiplab(aes(color = RAPTOR), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_text(aes(label=node))+
  # geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==132)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==139)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==118)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==168)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==121)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
ARAPT

ATOR <- AlvP + geom_tiplab(aes(color = TOR), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_text(aes(label=node))+
  # geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+=
  geom_point2(aes(subset=(node==111)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==112)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==114)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==128)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==123)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==144)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
ATOR

ALST8 <- AlvP + geom_tiplab(aes(color = LST8), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  # geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_text(aes(label = node))+
  geom_point2(aes(subset=(node==157)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==144)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==141)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==161)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==111)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==112)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==114)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==115)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==117)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==125)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==128)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==118)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
ALST8




ARIC + ASinP

(ARAPT+ALST8)

ATOR


# ------------------------------------------------------------------------------
# Initial Setup for the tree
RhizariaTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/RhizariaTreeP.phy")
RhizariaTree$tip.label
RhizariaTree$tip.label <- gsub("'","", RhizariaTree$tip.label)
RhizariaTree$tip.label
Rhizaria <- Rhizaria %>% relocate(Organism.Name)
RTP <- ggtree(RhizariaTree, branch.length = "none", ladderize = FALSE)+xlim(NA,+10)
RTP <- RTP%<+% Rhizaria

RTP$data$label

# Generating the tree plots with markings at specific node locations
RRICTOR <- RTP + geom_tiplab(aes(color = RICTOR), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  # geom_text(aes(label = node))+
  geom_point2(aes(subset=(node==15)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
RRICTOR

RSIN <- RTP + geom_tiplab(aes(color = SIN1), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==10)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
RSIN

RRAPTOR <-  RTP + geom_tiplab(aes(color = RAPTOR), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  geom_text(aes(label=node))+
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==7)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
RRAPTOR

RTOR <- RTP + geom_tiplab(aes(color = TOR), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  # geom_text(aes(label=node))+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
RTOR

RLST8 <- RTP + geom_tiplab(aes(color = LST8), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  # geom_text(aes(label=node))+
  geom_point2(aes(subset=(node==6)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==7)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
RLST8

# Plotting here
RRICTOR+RSIN

(RRAPTOR+RLST8)

RTOR



# ------------------------------------------------------------------------------

DiscobaTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/DiscobaTreeP.phy")
DiscobaTree$tip.label
DiscobaTree$tip.label <- gsub("'","", DiscobaTree$tip.label)
DiscobaTree$tip.label
Discoba <- Discoba %>% relocate(Organism.Name)
DTP <- ggtree(DiscobaTree, branch.length = "none", ladderize = FALSE)+xlim(NA,+15)
DTP <- DTP%<+% Discoba

DRAPTOR <- DTP + geom_tiplab(aes(color = RAPTOR), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  #geom_text(aes(label=node))+
  # geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==6)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==14)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==17)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==22)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
DRAPTOR

DRICTOR <- DTP + geom_tiplab(aes(color = RICTOR), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  #geom_text(aes(label=node))+
  # geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==6)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==13)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==14)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==22)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
DRICTOR

DSIN1 <- DTP + geom_tiplab(aes(color = SIN1), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==47)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
DSIN1

DTOR <- DTP + geom_tiplab(aes(color = TOR), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==14)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==22)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
DTOR

DLST8 <- DTP + geom_tiplab(aes(color = LST8), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  #geom_text(aes(label=node))+
  # geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==6)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==13)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==14)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==23)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==22)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
  
DLST8


DRICTOR+DSIN1

(DRAPTOR+DLST8)

DTOR


# ------------------------------------------------------------------------------

MetamonadaTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/MetamonadaTreeP.phy")
MetamonadaTree$tip.label <- gsub("'","", MetamonadaTree$tip.label)
MetamonadaTree$tip.label
Metamonada <- Metamonada %>% relocate(Organism.Name)
MTP <- ggtree(MetamonadaTree, branch.length = "none", ladderize = FALSE)+xlim(NA,+10)
MTP <- MTP %<+% Metamonada

MRICTOR <- MTP + geom_tiplab(aes(color = RICTOR), size = 3, show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  # geom_text(aes(label=node))+
  geom_point2(aes(subset=(node==4)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==7)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==12)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==31)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
MRICTOR

MSIN1 <- MTP + geom_tiplab(aes(color = SIN1), size = 3, show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==23)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==25)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==28)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
MSIN1


# No losses
MRAPTOR <- MTP + geom_tiplab(aes(color = RAPTOR), size = 3, show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
MRAPTOR

# No losses
MTOR <- MTP + geom_tiplab(aes(color = TOR), size = 3, show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
MTOR

# One possible loss though skeptical of its veracity
MLST8 <- MTP + geom_tiplab(aes(color = LST8), size = 3, show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==15)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .3)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
MLST8



# Done using the patchwork library
# Allows the combining of different ggplots together
MRICTOR + MSIN1

(MRAPTOR+MLST8)

MTOR

# ------------------------------------------------------------------------------
# Review this on 3/4

ChlorophytaTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/ChlorophytaTreeP.phy")
ChlorophytaTree$tip.label
ChlorophytaTree$tip.label <- gsub("'","", ChlorophytaTree$tip.label)
ChlorophytaTree$tip.label
Chlorophyta <- Chlorophyta %>% relocate(Organism.Name)
ChloroP <- ggtree(ChlorophytaTree, branch.length = "none", ladderize = FALSE)+xlim(NA,+10)
ChloroP <- ChloroP  %<+% Chlorophyta
ChloroP

#RICTOR
RicCh <- ChloroP + geom_tiplab(aes(color = RICTOR),size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  # geom_text(aes(label = node))+
  geom_point2(aes(subset=(node==90)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==85)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==83)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==89)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
RicCh

#RAPTOR
RapCh <- ChloroP + geom_tiplab(aes(color = RAPTOR), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  # geom_text(aes(label=node))+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  geom_point2(aes(subset=(node==59)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
RapCh

#SIN1
Sin1Ch <- ChloroP + geom_tiplab(aes(color = SIN1), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_text(aes(label = node))+
  # geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==90)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==89)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==85)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==83)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==82)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
Sin1Ch

#LST8

lst8Ch <- ChloroP + geom_tiplab(aes(color = LST8), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
lst8Ch

#TOR
TorCh <- ChloroP + geom_tiplab(aes(color = TOR), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  # geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
TorCh


# Create the plots
RicCh+Sin1Ch

(RapCh+lst8Ch)

TorCh




# ------------------------------------------------------------------------------


#Rhodophyta
RhodophytaTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/RhodophytaTreeP.phy")
RhodophytaTree$tip.label
RhodophytaTree$tip.label <- gsub("'","", RhodophytaTree$tip.label)
RhodophytaTree$tip.label 
Rhodophyta <- Rhodophyta %>% relocate(Organism.Name)
RhodoP <- ggtree(RhodophytaTree, branch.length = "none", laddarize = FALSE)+xlim(NA,+15)
RhodoP <- RhodoP %<+% Rhodophyta
RhodoP

#RICTOR
RicRh <- RhodoP + geom_tiplab(aes(color = RICTOR), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  # geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
   geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_point2(aes(subset=(node==15)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
RicRh

#RAPTOR
RapRh <- RhodoP + geom_tiplab(aes(color = RAPTOR), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
  # geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
RapRh

#SIN1
Sin1Rh <- RhodoP + geom_tiplab(aes(color = SIN1), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_point2(aes(subset=(node==15)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
  # geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+

Sin1Rh

#LST8
lst8Rh <- RhodoP + geom_tiplab(aes(color = LST8), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
  # geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
lst8Rh

#TOR
TorRh <- RhodoP + geom_tiplab(aes(color = TOR), size = 3,show.legend = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
  # geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
TorRh


RicRh+Sin1Rh

(RapRh+lst8Rh)

TorRh
# ------------------------------------------------------------------------------


StreptophytaTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/StreptophytaTreeP.phy")
StreptophytaTree$tip.label
StreptophytaTree$tip.label <- gsub("'", "", StreptophytaTree$tip.label)
StreptophytaTree$tip.label
Streptophyta <- Streptophyta %>% relocate(Organism.Name)
StrephP <- ggtree(StreptophytaTree, layout = "circular", branch.length = "none", laddarize = FALSE)
StrephP <- StrephP %<+% Streptophyta
StrephP 

#RICTOR
RicStr <- StrephP + geom_tiplab(aes(color = RICTOR), size = 2)+
  geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==357)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
RicStr

#RAPTOR
RapStr <- StrephP + geom_tiplab(aes(color = RAPTOR), size = 2)+
  geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_text(aes(label=node), size = 2)+
  geom_point2(aes(subset=(node==65)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==345)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  geom_point2(aes(subset=(node==283)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)
RapStr

#SIN1
SIN1Str <- StrephP + geom_tiplab(aes(color = SIN1), size = 2)+
  geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==357)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
SIN1Str

#LST8
LST8Str <- StrephP + geom_tiplab(aes(color = LST8), size = 2)+
  geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_text(aes(label=node), size = 2)+
  geom_point2(aes(subset=(node==330)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==283)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==48)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)
LST8Str

TorStr <- StrephP + geom_tiplab(aes(color = TOR), size = 2)+
  geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==283)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)
TorStr


#-------------------------------------------------------------------------------
ExcavataTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/ExcavataTreeP.phy")
ExcavataTree$tip.label <- gsub("'", "", ExcavataTree$tip.label)
Excavata <- Excavata %>% relocate(Organism.Name)

ExcavataP <- ggtree(ExcavataTree, layout = "circular", branch.length = "none", laddarize = FALSE)
ExcavataP <- ExcavataP %<+% Excavata


RicExc <- ExcavataP + geom_tiplab(aes(color = RICTOR), size = 2)+
  geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==357)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
RicExc

SinExc <- ExcavataP + geom_tiplab(aes(color = SIN1), size = 2)+
  geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==357)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
SinExc


RapExc <- ExcavataP + geom_tiplab(aes(color = RAPTOR), size = 2)+
  geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==357)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
RapExc


LST8Exc <- ExcavataP + geom_tiplab(aes(color = LST8), size = 2)+
  geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==357)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
LST8Exc


TorExc <- ExcavataP + geom_tiplab(aes(color = TOR), size = 2)+
  geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==357)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
TorExc

# ------------------------------------------------------------------------------
# A sort of "For fun tree". Much to large to be of any sort of importance
AllTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/AllTreeP.phy")
AllTree$tip.label <- gsub("'", "", AllTree$tip.label)
HTML <- HTML %>% relocate(Organism.Name)


AllTreeP <- ggtree(AllTree, layout = "circular", branch.length = "none", laddarize = FALSE)
AllTreeP <- AllTreeP %<+% HTML

RicAll <- AllTreeP + geom_tiplab(aes(color = RICTOR), size = 2)+
  geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2, color = "black")+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==357)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
RicAll

Sin1All <- AllTreeP + geom_tiplab(aes(color = SIN1), size = 2)+
  geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2, color = "black")+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==357)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
Sin1All

RapAll <- AllTreeP + geom_tiplab(aes(color = RAPTOR), size = 2)+
  geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2, color = "black")+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==357)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
RapAll

