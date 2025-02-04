# Created by:
# Kyle Johnson
# Dellaraam Pourkeramati
#







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


Taxon <- read.csv("~/GitHub/TOR_phylogenetics/Combined_Taxonomy.csv")
HTML <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Project.csv")
# Clean up the Combined Data set. Apparently had some extra columns attached somewhere
HTML <- HTML %>% select(-X) %>% select(-X.1) %>% select(-X.2)


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









HTML$Organism_Name <- str_replace(HTML$Organism_Name,"Neopyropia yezoensis","Pyropia yezoensis")
#HTML <- distinct(HTML, Organism_Name, .keep_all = TRUE)




HTML %>%rename("Super Group" = Super.Group) %>% filter(`Super Group` == "Alveolata") %>% filter(!is.na(RAPTOR))%>% view()
Chlorophyta <- HTML %>% filter(Super.Group == "Chlorophyta")
Rhodophyta <- HTML %>% filter(Super.Group == "Rhodophyta")
Streptophyta <- HTML %>% filter(Super.Group == "Streptophyta")
Discoba <- HTML %>% filter(Super.Group == "Discoba")
Metamonada <- HTML %>% filter(Super.Group == "Metamonada")
Alveolata <- HTML %>% filter(Super.Group  == "Alveolata")
Stramenopiles <- HTML %>% filter(Super.Group == "Stramenopiles")
Rhizaria <- HTML %>% filter(Super.Group == "Rhizaria")




# Play space/ Adding in additional collected data





write.table(HTML$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/All.txt", sep = "\t", row.names = F, col.names = F)
write.table(Alveolata$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Alveolata.txt", sep = "\t", row.names = F, col.names = F)
write.table(Stramenopiles$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Stramenopiles.txt", sep = "\t", row.names = F, col.names = F)
write.table(Rhizaria$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Rhizaria.txt", sep = "\t", row.names = F, col.names = F)
write.table(Chlorophyta$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Chlorophyta.txt", sep = "\t", row.names = F, col.names = F)
write.table(Streptophyta$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Streptophyta.txt", sep = "\t", row.names = F, col.names = F)
write.table(Rhodophyta$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Rhodophyta.txt", sep = "\t", row.names = F, col.names = F)
write.table(Discoba$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Discoba.txt", sep = "\t", row.names = F, col.names = F)
write.table(Metamonada$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Metamonada.txt", sep = "\t", row.names = F, col.names = F)


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
  "NA" = "grey"
)

# ------------------------------------------------------------------------------





# ------------------------------------------------------------------------------


StramenopileTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/StramenopileTreeP.phy")
StramenopileTree$tip.label
StramenopileTree$tip.label <- gsub("'","", StramenopileTree$tip.label)
StramenopileTree$tip.label

Stramenopiles <- Stramenopiles %>% relocate(Organism_Name)
STP <- ggtree(StramenopileTree, branch.length = "none", ladderize = FALSE)+xlim(NA,+10)
STP <- STP  %<+% Stramenopiles
#RICTOR Stramenopiles
RISP <- STP + geom_tiplab( aes(color = RICTOR), size = 3, show.legend = FALSE)+geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_rootedge()+
  geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==64)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==66)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5) +
  geom_point2(aes(subset=(node==14)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  geom_point2(aes(subset=(node==45)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==72)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .3)+
  geom_point2(aes(subset=(node==73)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .3)+
  geom_point2(aes(subset=(node==116)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5) +
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .3)+
  geom_point2(aes(subset=(node==111)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
#RISP
#Sin1 Stramenopiles
SSP <- STP + geom_tiplab(aes(color = SIN1), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==105)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==90)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==35)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==34)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==25)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==26)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
#SSP
# Note: Add in the dark blue locations to denote a possible error location
SRAPTORS <- STP + geom_tiplab(aes(color = RAPTOR), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==15)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==72)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==73)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) 
#SRAPTORS

STORS <- STP + geom_tiplab(aes(color = TOR), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==72)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==73)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) 
#STORS

SLST8S <- STP + geom_tiplab(aes(color = LST8), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==45)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==46)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==66)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==72)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==73)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==87)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) 
#SLST8S




# Generating the plots here
RISP+SSP
(SRAPTORS+SLST8S)

STORS


# ------------------------------------------------------------------------------




AlveolataTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/AlveolataTreeP.phy")
AlveolataTree$tip.label
AlveolataTree$tip.label <- gsub("'","", AlveolataTree$tip.label)
AlveolataTree$tip.label
Alveolata <- Alveolata %>% relocate(Organism_Name)
AlvP <- ggtree(AlveolataTree, branch.length = "none", ladderize = FALSE)+xlim(NA,+10)
AlvP <- AlvP  %<+% Alveolata
# SIN1
ASinP <- AlvP + geom_tiplab(aes(color = SIN1), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==165)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==139)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==137)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==134)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==131)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==10)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)
  
#ASinP

# RICTOR
ARIC <- AlvP + geom_tiplab(aes(color = RICTOR), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==139)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==118)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==167)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==9)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==17)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)
#ARIC

# RAPTOR

ARAPT <- AlvP + geom_tiplab(aes(color = RAPTOR), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==132)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==139)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==167)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==118)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)
#ARAPT

ATOR <- AlvP + geom_tiplab(aes(color = TOR), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==111)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==112)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==128)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==123)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==144)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==29)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==7)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)
#ATOR

ALST8 <- AlvP + geom_tiplab(aes(color = LST8), size = 3)+
  geom_rootedge()+
  geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==157)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==144)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==141)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==161)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==112)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==114)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==115)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==117)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==120)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==125)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==128)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)
#ALST8




ARIC + ASinP

(ARAPT+ALST8)

ATOR


# ------------------------------------------------------------------------------
# Initial Setup for the tree
RhizariaTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/RhizariaTreeP.phy")
RhizariaTree$tip.label
RhizariaTree$tip.label <- gsub("'","", RhizariaTree$tip.label)
RhizariaTree$tip.label
Rhizaria <- Rhizaria %>% relocate(Organism_Name)
RTP <- ggtree(RhizariaTree, branch.length = "none", ladderize = FALSE)+xlim(NA,+10)
RTP <- RTP%<+% Rhizaria

# Generating the tree plots with markings at specific node locations
RRICTOR <- RTP + geom_tiplab(aes(color = RICTOR), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==13)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==10)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)
#RRICTOR

RSIN <- RTP + geom_tiplab(aes(color = SIN1), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==9)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)
#RSIN

RRAPTOR <-  RTP + geom_tiplab(aes(color = RAPTOR), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==6)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==7)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==10)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)
#RRAPTOR

RTOR <- RTP + geom_tiplab(aes(color = TOR), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==3)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)
#RTOR

RLST8 <- RTP + geom_tiplab(aes(color = LST8), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==5)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==6)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==3)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)
#RLST8

# Plotting here
RRICTOR+RSIN

(RRAPTOR+RLST8)

RTOR



# ------------------------------------------------------------------------------

DiscobaTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/DiscobaTreeP.phy")
DiscobaTree$tip.label
DiscobaTree$tip.label <- gsub("'","", DiscobaTree$tip.label)
DiscobaTree$tip.label
Discoba <- Discoba %>% relocate(Organism_Name)
DTP <- ggtree(DiscobaTree, branch.length = "none", ladderize = FALSE)+xlim(NA,+10)
DTP <- DTP%<+% Discoba

DRAPTOR <- DTP + geom_tiplab(aes(color = RAPTOR), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==6)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==13)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==18)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==21)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)
#DRAPTOR

DRICTOR <- DTP + geom_tiplab(aes(color = RICTOR), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==6)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==12)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==13)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==18)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)
#DRICTOR

DSIN1 <- DTP + geom_tiplab(aes(color = SIN1), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==46)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)
#DSIN1

DTOR <- DTP + geom_tiplab(aes(color = TOR), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==13)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==18)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)
#DTOR

DLST8 <- DTP + geom_tiplab(aes(color = LST8), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==6)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==12)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==13)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==18)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==19)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)
  
#DLST8


DRICTOR+DSIN1

(DRAPTOR+DLST8)

DTOR


# ------------------------------------------------------------------------------

MetamonadaTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/MetamonadaTreeP.phy")
MetamonadaTree$tip.label <- gsub("'","", MetamonadaTree$tip.label)
MetamonadaTree$tip.label
Metamonada <- Metamonada %>% relocate(Organism_Name)
MTP <- ggtree(MetamonadaTree, branch.length = "none", ladderize = FALSE)+xlim(NA,+10)
MTP <- MTP %<+% Metamonada

MRICTOR <- MTP + geom_tiplab(aes(color = RICTOR), size = 3, show.legend = FALSE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==4)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==5)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==7)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==12)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==31)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)
#MRICTOR

MSIN1 <- MTP + geom_tiplab(aes(color = SIN1), size = 3, show.legend = FALSE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==23)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==25)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==28)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)
#MSIN1


# No losses
MRAPTOR <- MTP + geom_tiplab(aes(color = RAPTOR), size = 3, show.legend = FALSE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)
#MRAPTOR

# No losses
MTOR <- MTP + geom_tiplab(aes(color = TOR), size = 3, show.legend = FALSE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)
#MTOR

# One possible loss though skeptical of its veracity
MLST8 <- MTP + geom_tiplab(aes(color = LST8), size = 3, show.legend = FALSE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==15)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .3)
#MLST8



# Done using the patchwork library
# Allows the combining of different ggplots together
MRICTOR + MSIN1

(MRAPTOR+MLST8)

MTOR

# ------------------------------------------------------------------------------

ChlorophytaTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/ChlorophytaTreeP.phy")
ChlorophytaTree$tip.label
ChlorophytaTree$tip.label <- gsub("'","", ChlorophytaTree$tip.label)
ChlorophytaTree$tip.label
Chlorophyta <- Chlorophyta %>% relocate(Organism_Name)
ChloroP <- ggtree(ChlorophytaTree, branch.length = "none", ladderize = FALSE)+xlim(NA,+10)
ChloroP <- ChloroP  %<+% Chlorophyta
ChloroP

#RICTOR
RicCh <- ChloroP + geom_tiplab(aes(color = RICTOR),size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==3)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5) +
  geom_point2(aes(subset=(node==6)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5) +
  geom_point2(aes(subset=(node==63)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5) +
  geom_point2(aes(subset=(node==64)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5) + 
  geom_point2(aes(subset=(node==68)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
#RicCh

#RAPTOR
RapCh <- ChloroP + geom_tiplab(aes(color = RAPTOR), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==44)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) 
#RapCh

#SIN1
Sin1Ch <- ChloroP + geom_tiplab(aes(color = SIN1), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_text(aes(label = node))+
  geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==62)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==3)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+ 
  geom_point2(aes(subset=(node==6)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+ 
  geom_point2(aes(subset=(node==63)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+ 
  geom_point2(aes(subset=(node==64)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+ 
  geom_point2(aes(subset=(node==69)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+ 
  geom_point2(aes(subset=(node==81)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+ 
  geom_point2(aes(subset=(node==87)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==60)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==58)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==57)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==56)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==55)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==54)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5) 
#Sin1Ch

#LST8
lst8Ch <- ChloroP + geom_tiplab(aes(color = LST8), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)
#lst8Ch

#TOR
TorCh <- ChloroP + geom_tiplab(aes(color = TOR), size = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 3)+
  geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)
#TorCh


# Create the plots
RicCh+Sin1Ch

(RapCh+lst8Ch)

TorCh




# ------------------------------------------------------------------------------


#Rhodophyta
RhodophytaTree <- read.tree(file = "~/Code/TOR_phylogenetics/Trees/RhodophytaTreeP.phy")
RhodophytaTree$tip.label
RhodophytaTree$tip.label <- gsub("'","", RhodophytaTree$tip.label)
RhodophytaTree$tip.label 
Rhodophyta <- Rhodophyta %>% relocate(Organism_Name)
RhodoP <- ggtree(RhodophytaTree, branch.length = "none", laddarize = FALSE)+xlim(NA,+15)
RhodoP <- RhodoP %<+% Rhodophyta
RhodoP

#RICTOR
RicRh <- RhodoP + geom_tiplab(aes(color = RICTOR), size = 3)+
  geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==15)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
RicRh

#RAPTOR
RapRh <- RhodoP + geom_tiplab(aes(color = RAPTOR), size = 3)+
  geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)
RapRh

#SIN1
Sin1Rh <- RhodoP + geom_tiplab(aes(color = SIN1), size = 3)+
  geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==15)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
Sin1Rh

#LST8
lst8Rh <- RhodoP + geom_tiplab(aes(color = LST8), size = 3)+
  geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)
lst8Rh

#TOR
TorRh <- RhodoP + geom_tiplab(aes(color = TOR), size = 3)+
  geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)
TorRh


RicRh+Sin1Rh

(RapRh+lst8Rh)

TorRh
# ------------------------------------------------------------------------------


StreptophytaTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/StreptophytaTreeP.phy")
StreptophytaTree$tip.label
StreptophytaTree$tip.label <- gsub("'", "", StreptophytaTree$tip.label)
StreptophytaTree$tip.label
Streptophyta <- Streptophyta %>% relocate(Organism_Name)
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
  geom_point2(aes(subset=(node==89)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==187)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==193)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==65)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==79)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==345)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)
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
  geom_point2(aes(subset=(node==330)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==332)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==335)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==283)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==187)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==161)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==92)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==89)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==51)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==48)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==22)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) 
LST8Str

TorStr <- StrephP + geom_tiplab(aes(color = TOR), size = 2)+
  geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==187)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==89)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) 
TorStr




