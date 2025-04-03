# Created by:
# Kyle Johnson
# Dellaraam Pourkeramati
#



#source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Library_Script.R")
#source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Merging_Combined_CSV_Files.R")
#source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Numeric_Table_Script.R")
#source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/HTML_Additions.R")


#Final Modifications to csv files required for script
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

# Current Goal is to replace all of the names in the HTML file (really need to rename that)
# Everything should be replaced by what is found within the Taxon file
# That should make everything work correctly


SARnoRICTOR <- HTML %>%
  filter(`Super.Group` != "Streptophyta") %>%
  filter(`Super.Group` != "Chlorophyta")%>%
  filter(`Super.Group` != "Rhodophyta") %>%
  filter(`Super.Group` != "Excavata") %>%
  filter(`Super.Group` != "Discoba") %>%
  filter(`Super.Group` != "Metamonada") %>% 
  filter(is.na(RICTOR))

SARYRICTOR <- HTML %>%
  filter(`Super.Group` != "Streptophyta") %>%
  filter(`Super.Group` != "Chlorophyta")%>%
  filter(`Super.Group` != "Rhodophyta") %>%
  filter(`Super.Group` != "Excavata") %>%
  filter(`Super.Group` != "Discoba") %>%
  filter(`Super.Group` != "Metamonada") %>% 
  filter(!is.na(RICTOR))


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
  "H" = "#7e2323",
  "M" = "#d0771a",
  "L" = "#ffc30b",
  "P" = "#7dbe62",
  "NA" = "#FF000000"
)

pal2 <- c(
  "H" = "#f4931f",
  "M" = "#f5da5f",
  "L" = "#41ad49",
  "P" = "#027aa4",
  "NA" = "#FF000000"
)

pal3 <- c(
  "H" = "#ae5a41",
  "P" = "#DBC795",
  "M" = "#559e83",
  "L" = "#B2D4AD",
  na_value = "#FF000000"
)


modpal3 <- c(
  "H" = "#ae5a41",
  "M" = "#c3cb71",
  "L" = "#559e83",
  na_value = "#FF000000"
)


pal4 <- c(
  "H" = "#f4649e",
  "M" = "#49e5aa",
  "L" = "#d2ebfb",
  "P" = "#f0d14f",
  "NA" = "#FF000000"
)


UChicagoPal <- pal_uchicago("light")(8)
lancPal <- pal_lancet("lanonc")(5)
AAAsPal <- pal_aaas("default")(5)
NPGPal <- pal_npg("nrc")(5)
NEJMPal <- pal_nejm("default")(5)
# Blank
##00000000

# ------------------------------------------------------------------------------
#Test Ground

largeDataSet <- left_join(HTML, Ndf[c("SIN1All","SIN1Domain","RICTORAll","RICTORDomain","RAPTORAll","RAPTORDomain","TORAll","TORDomain","LST8All","LST8Domain","Organism_Taxonomic_ID")], by = "Organism_Taxonomic_ID")


largeDataSet <- mutate(largeDataSet, HasRictor = NA, HasRaptor = NA, HasTOR = NA, HasLST8 = NA, HasSIN1 = NA)

largeDataSet[!is.na(largeDataSet$RICTOR),]$HasRictor <- "Yes"
largeDataSet[!is.na(largeDataSet$RAPTOR),]$HasRaptor <- "Yes"
largeDataSet[!is.na(largeDataSet$SIN1),]$HasSIN1 <- "Yes"
largeDataSet[!is.na(largeDataSet$LST8),]$HasLST8 <- "Yes"
largeDataSet[!is.na(largeDataSet$TOR),]$HasTOR <- "Yes"



largeDataSet %>% filter(Super.Group == "Stramenopiles") %>% select(Organism.Name, SIN1)%>%
  pivot_longer(cols = !Organism.Name,names_to = "Protein", values_to = "Score") %>%
  ggplot(aes(x = Protein, y = Organism.Name, fill = Score), width = 0)+
  geom_tile()


# ------------------------------------------------------------------------------
StramenopilesL <- largeDataSet %>% filter(Super.Group == "Stramenopiles")
subsetdataframe <- StramenopilesL %>% select(Organism.Name, SIN1, RICTOR, RAPTOR, LST8,TOR) %>% distinct(Organism.Name, .keep_all = TRUE)
df <- column_to_rownames(subsetdataframe, var = "Organism.Name")










StramenopileTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/StramenopileTreeP.phy")
StramenopileTree$tip.label
StramenopileTree$tip.label <- gsub("'","", StramenopileTree$tip.label)
StramenopileTree$tip.label

Stramenopiles <- Stramenopiles %>% relocate(Organism.Name)
STP <- ggtree(StramenopileTree, branch.length = "none", ladderize = FALSE)
STP <- STP  %<+% Stramenopiles
#RICTOR Stramenopiles


RISP <- STP + xlim(NA, +20) + geom_tiplab(aes(color = RICTOR), size = 2, show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  #geom_text(aes(label = node))+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  #geom_text(aes(label=node))+
  geom_point2(aes(subset=(node==19)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==150)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==140)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==70)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==7)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
   scale_color_manual(values = pal, limits = c("H","M","L","P",NA),
                      guide = guide_legend())
RISP
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/RictorStramenopile.png",
       plot = RISP,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

#Sin1 Stramenopiles
SSP <- STP + xlim(NA, +20) + geom_tiplab(aes(color = SIN1), size = 2, show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(fill = `SIN1`, x = 0, y = 0))+
  #geom_text(aes(label = node))+
  geom_point2(aes(subset=(node==150)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==132)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==142)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==140)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==113)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==30)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==31)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==40)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==41)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==70)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==138)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
  
  
SSP
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/SIN1Stramenopile.png",
       plot = SSP,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)
# Note: Add in the dark blue locations to denote a possible error location
SRAPTORS <- STP + xlim(NA, +20) + geom_tiplab(aes(color = RAPTOR), size = 2, show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  #geom_text(aes(label = node))+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8) +
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
SRAPTORS
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/RaptorStramenopile.png",
       plot = SRAPTORS,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

  
STORS <- STP + xlim(NA, +20)+  geom_tiplab(aes(color = TOR), size = 2, show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  # geom_text(aes(label = node))+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+http://127.0.0.1:17413/graphics/plot_zoom_png?width=1707&height=912
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
STORS
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/TORStramenopile.png",
       plot = STORS,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

SLST8S <- STP + xlim(NA, +20) + geom_tiplab(aes(color = LST8), size = 2, show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  # geom_text(aes(label = node))+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==52)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==97)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+ 
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
  
SLST8S
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/LST8Stramenopile.png",
       plot = SLST8S,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)


HeatTreeStram <- STP + xlim(NA, +25) + geom_tiplab(size = 1.8, nudge_x = .3, linesize = .4, align = TRUE, aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#29AF7FFF", "#287D8EFF", "#39568CFF","#440154FF"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_rootedge()+
  labs(title = "Stramenopiles Phylogenetic Tree",
       subtitle = "With HMMER Score Map")
  #geom_nodelab(nudge_y = .5, nudge_x = -.7, size = 2)

HeatTreeStram
StramHeatPlot <- gheatmap(HeatTreeStram, df, offset = 5, width = .4, font.size = 2, colnames = FALSE)+
  scale_fill_manual(name = "HMMER Score",
                    breaks = c("H","M","L","P",NA),
                    values = pal3,
                    limits = c("H","M","L","P", NA),
                    na.value = "#FFFFFF",
                    drop = FALSE)+
  theme(
    text = element_text(family = "serif"),
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))
    
StramHeatPlot
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapStramenopile.png",
       plot = StramHeatPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)
# ------------------------------------------------------------------------------
AlveolataL <- largeDataSet %>% filter(Super.Group == "Alveolata")
subsetdataframe <- AlveolataL %>% select(Organism.Name, SIN1, RICTOR, RAPTOR, LST8,TOR) %>% distinct(Organism.Name, .keep_all = TRUE)
df <- column_to_rownames(subsetdataframe, var = "Organism.Name")








AlveolataTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/AlveolataTreeP.phy")
AlveolataTree$tip.label
AlveolataTree$tip.label <- gsub("'","", AlveolataTree$tip.label)
AlveolataTree$tip.label
Alveolata <- Alveolata %>% relocate(Organism.Name)
AlvP <- ggtree(AlveolataTree, branch.length = "none", ladderize = FALSE)
AlvP <- AlvP  %<+% Alveolata
AlvP
# SIN1
ASinP <- AlvP + xlim(NA,+40) + geom_tiplab(aes(color = SIN1), size = 2, show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  #geom_text(aes(label=node))+
  geom_point2(aes(subset=(node==169)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==143)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==138)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==135)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==141)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==134)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==10)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)

ASinP
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/SIN1Alveolata.png",
       plot = ASinP,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

# RICTOR
ARIC <- AlvP + xlim(NA,+40) + geom_tiplab(aes(color = RICTOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  #geom_text(aes(label=node))+
  # geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==172)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==143)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==170)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==9)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
ARIC
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/RictorAlveolata.png",
       plot = ARIC,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

# RAPTOR

ARAPT <- AlvP + xlim(NA,+40) + geom_tiplab(aes(color = RAPTOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  #geom_text(aes(label=node))+
  # geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==173)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==170)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==143)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==136)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==124)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==123)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==121)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
ARAPT
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/RaptorAlveolata.png",
       plot = ARAPT,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

ATOR <- AlvP + xlim(NA,+40) + geom_tiplab(aes(color = TOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  #geom_text(aes(label=node))+
  # geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+=
  geom_point2(aes(subset=(node==131)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==126)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==114)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==112)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==111)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==148)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==7)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
ATOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/TORAlveolata.png",
       plot = ATOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

ALST8 <- AlvP + xlim(NA,+40) + geom_tiplab(aes(color = LST8), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE, key_glyph = "rect")+
  geom_rootedge()+
  guides(color = guide_legend(override.aes = list(label = "", shape = 15, size = 5)))+
  # geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  #geom_text(aes(label = node))+
  geom_point2(aes(subset=(node==165)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==161)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==148)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==145)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==170)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  geom_point2(aes(subset=(node==117)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==115)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==114)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==112)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==111)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==131)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==128)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==7)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  scale_color_manual(name = "HMMER Score",
                    breaks = c("H","M","L","P","NA"),
                    values = pal3,
                    limits = c("H","M","L","P", "NA"),
                    na.value = "#FF000000",
                    drop = FALSE)
ALST8
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/LST8Alveolata.png",
       plot = ALST8,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

HeatTreeAlv <- AlvP + xlim(NA,+20) + geom_tiplab(size = 1.8, show.legend = TRUE, nudge_x = .3, linesize = .2, align = TRUE, aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#29AF7FFF", "#287D8EFF", "#39568CFF","#440154FF"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  geom_rootedge()+
  #geom_nodelab(nudge_y = 1.2, nudge_x = -.7, size = 1.5)+
  labs(title = "Alveolata Phylogenetic Tree",
       subtitle = "With HMMER Score Map")

AlvHeatPlot <- gheatmap(HeatTreeAlv, df, offset = 4, font.size = 2,, width = .4, colnames = FALSE)+
  scale_fill_manual(name = "HMMER Score",
                    breaks = c("H","M","L","P",NA),
                    values = pal3,
                    limits = c("H","M","L","P", NA),
                    na.value = "#FFFFFF",
                    drop = FALSE)+
  theme(
    text = element_text(family = "serif"),
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))

AlvHeatPlot

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapAlveolata.png",
       plot = AlvHeatPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapAlveolata.pdf",
       plot = AlvHeatPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)








# ------------------------------------------------------------------------------
# Initial Setup for the tree

RhizariaL <- largeDataSet %>% filter(Super.Group == "Rhizaria")
subsetdataframe <- RhizariaL %>% select(Organism.Name, SIN1, RICTOR, RAPTOR, LST8,TOR) %>% distinct(Organism.Name, .keep_all = TRUE)
df <- column_to_rownames(subsetdataframe, var = "Organism.Name")






RhizariaTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/RhizariaTreeP.phy")
RhizariaTree$tip.label
RhizariaTree$tip.label <- gsub("'","", RhizariaTree$tip.label)
RhizariaTree$tip.label
Rhizaria <- Rhizaria %>% relocate(Organism.Name)
RTP <- ggtree(RhizariaTree, branch.length = "none", ladderize = FALSE)
RTP <- RTP%<+% Rhizaria

# Generating the tree plots with markings at specific node locations
RRICTOR <- RTP +xlim(NA,+30)+ geom_tiplab(aes(color = RICTOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = .5, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  # geom_text(aes(label = node))+
  geom_point2(aes(subset=(node==15)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
RRICTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/RictorRhziaria.png",
       plot = RRICTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

RSIN <- RTP +xlim(NA,+30)+ geom_tiplab(aes(color = SIN1), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = .5, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==10)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
RSIN
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/SIN1Rhziaria.png",
       plot = RSIN,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

RRAPTOR <-  RTP +xlim(NA,+30)+ geom_tiplab(aes(color = RAPTOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = .5, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  geom_text(aes(label=node))+
  geom_point2(aes(subset=(node==2)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==7)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
RRAPTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/RaptorRhziaria.png",
       plot = RRAPTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

RTOR <- RTP +xlim(NA,+30)+ geom_tiplab(aes(color = TOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = .5, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  # geom_text(aes(label=node))+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
RTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/TORRhziaria.png",
       plot = RTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

RLST8 <- RTP +xlim(NA,+30)+ geom_tiplab(aes(color = LST8), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = .5, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  # geom_text(aes(label=node))+
  geom_point2(aes(subset=(node==6)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  geom_point2(aes(subset=(node==7)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .6)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
RLST8
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/LST8Rhziaria.png",
       plot = RLST8,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

HeatTreeRhiz <- RTP +xlim(NA,+20)+ geom_tiplab(size = 2, show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE,aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#29AF7FFF", "#287D8EFF", "#39568CFF","#440154FF"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  #geom_text(aes(label = node))+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_rootedge()+
  labs(title = "Rhizaria Phylogenetic Tree",
       subtitle = "With HMMER Score Map")

RhizHeatPlot <- gheatmap(HeatTreeRhiz, df, offset = 7, font.size = 1.8, width = .4, colnames = FALSE)+
  scale_fill_manual(name = "HMMER Score",
                    breaks = c("H","M","L","P",NA),
                    values = pal3,
                    limits = c("H","M","L","P", NA),
                    na.value = "#FFFFFF",
                    drop = FALSE)+
  theme(
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))+
  guides(fill = guide_legend(override.aes = list(label = "")))
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapRhizaria.png",
       plot = RhizHeatPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

RhizHeatPlot






# ------------------------------------------------------------------------------

DiscobaL <- largeDataSet %>% filter(Super.Group == "Discoba")
subsetdataframe <- DiscobaL %>% select(Organism.Name, SIN1, RICTOR, RAPTOR, LST8,TOR) %>% distinct(Organism.Name, .keep_all = TRUE)
df <- column_to_rownames(subsetdataframe, var = "Organism.Name")





DiscobaTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/DiscobaTreeP.phy")
DiscobaTree$tip.label
DiscobaTree$tip.label <- gsub("'","", DiscobaTree$tip.label)
DiscobaTree$tip.label
Discoba <- Discoba %>% relocate(Organism.Name)
DTP <- ggtree(DiscobaTree, branch.length = "none", ladderize = FALSE)+xlim(NA,+30)
DTP <- DTP%<+% Discoba

DRAPTOR <- DTP + geom_tiplab(aes(color = RAPTOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_text(aes(label=node))+
  # geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==6)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==20)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
DRAPTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/RaptorDiscoba.png",
       plot = DRAPTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

DRICTOR <- DTP + geom_tiplab(aes(color = RICTOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_text(aes(label=node))+
  # geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==6)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==8)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==12)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==45)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
DRICTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/RictorDiscoba.png",
       plot = DRICTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

DSIN1 <- DTP + geom_tiplab(aes(color = SIN1), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_text(aes(label=node))+
  # geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==46)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
DSIN1
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/SIN1Discoba.png",
       plot = DSIN1,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

DTOR <- DTP + geom_tiplab(aes(color = TOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
DTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/TORDiscoba.png",
       plot = DTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

DLST8 <- DTP + geom_tiplab(aes(color = LST8), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_text(aes(label=node))+
  # geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==6)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==12)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==18)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
DLST8
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/LST8Discoba.png",
       plot = DLST8,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)



HeatTreeDisc <- DTP + geom_tiplab(size = 2, show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE,aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#29AF7FFF", "#287D8EFF", "#39568CFF","#440154FF"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  # geom_text(aes(label = node))+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_rootedge()+
  labs(title = "Discoba Phylogenetic Tree",
       subtitle = "With HMMER Score Map")
HeatTreeDisc
DiscHeatPlot <- gheatmap(HeatTreeDisc, df, offset = 8, font.size = 2, width = .4, colnames = FALSE)+
  scale_fill_manual(name = "HMMER Score",
                    breaks = c("H","M","L",NA),
                    values = pal3,
                    limits = c("H","M","L",NA),
                    na.value = "#FFFFFF",
                    guide = guide_legend(order = 2),
                    drop = FALSE)+
  theme(
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))+
  guides(fill = guide_legend(override.aes = list(label = "")))
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapDiscoba.png",
       plot = DiscHeatPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)
DiscHeatPlot


# ------------------------------------------------------------------------------

MetamonadaL <- largeDataSet %>% filter(Super.Group == "Metamonada")
subsetdataframe <- MetamonadaL %>% select(Organism.Name, SIN1, RICTOR, RAPTOR, LST8,TOR) %>% distinct(Organism.Name, .keep_all = TRUE)
df <- column_to_rownames(subsetdataframe, var = "Organism.Name")




MetamonadaTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/MetamonadaTreeP.phy")
MetamonadaTree$tip.label <- gsub("'","", MetamonadaTree$tip.label)
MetamonadaTree$tip.label
Metamonada <- Metamonada %>% relocate(Organism.Name)
MTP <- ggtree(MetamonadaTree, branch.length = "none", ladderize = FALSE)+xlim(NA,+30)
MTP <- MTP %<+% Metamonada

MRICTOR <- MTP + geom_tiplab(aes(color = RICTOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  # geom_text(aes(label=node))+
  geom_point2(aes(subset=(node==32)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==21)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==12)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==7)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==5)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  geom_point2(aes(subset=(node==4)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
MRICTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/RictorMetamonada.png",
       plot = MRICTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

MSIN1 <- MTP + geom_tiplab(aes(color = SIN1), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  # geom_text(aes(label=node))+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==29)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==26)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  geom_point2(aes(subset=(node==24)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .8)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
MSIN1
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/SIN1Metamonada.png",
       plot = MSIN1,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

# No losses
MRAPTOR <- MTP + geom_tiplab(aes(color = RAPTOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
MRAPTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/RaptorMetamonada.png",
       plot = MRAPTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

# No losses
MTOR <- MTP + geom_tiplab(aes(color = TOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
MTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/TORMetamonada.png",
       plot = MTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

# One possible loss though skeptical of its veracity
MLST8 <- MTP + geom_tiplab(aes(color = LST8), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==15)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .8)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
MLST8
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/LST8Metamonada.png",
       plot = MLST8,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

HeatTreeMet <- MTP + geom_tiplab(size = 2, show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE,aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#29AF7FFF", "#287D8EFF", "#39568CFF","#440154FF"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  #geom_text(aes(label = node))+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_rootedge()+
  labs(title = "Metamonada Phylogenetic Tree",
       subtitle = "With HMMER Score Map")

MetHeatPlot <- gheatmap(HeatTreeMet, df, offset = 8, font.size = 2, width = .4, colnames = FALSE)+
  scale_fill_manual(name = "HMMER Score",
                    breaks = c("H","M","L",NA),
                    values = pal3,
                    limits = c("H","M","L",NA),
                    na.value = "#FFFFFF",
                    drop = FALSE)+
  theme(
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))+
  guides(fill = guide_legend(override.aes = list(label = "")))
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapMetamonada.png",
       plot = MetHeatPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

MetHeatPlot


# ------------------------------------------------------------------------------

ChlorophytaL <- largeDataSet %>% filter(Super.Group == "Chlorophyta")
subsetdataframe <- ChlorophytaL %>% select(Organism.Name, SIN1, RICTOR, RAPTOR, LST8,TOR) %>% distinct(Organism.Name, .keep_all = TRUE)
df <- column_to_rownames(subsetdataframe, var = "Organism.Name")




ChlorophytaTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/ChlorophytaTreeP.phy")
ChlorophytaTree$tip.label <- gsub("'","", ChlorophytaTree$tip.label)
Chlorophyta <- Chlorophyta %>% relocate(Organism.Name)
ChloroP <- ggtree(ChlorophytaTree, branch.length = "none", ladderize = FALSE)+xlim(NA,+30)
ChloroP <- ChloroP  %<+% Chlorophyta


#RICTOR
RicCh <- ChloroP + geom_tiplab(aes(color = RICTOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  # geom_text(aes(label = node))+
  geom_point2(aes(subset=(node==90)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==85)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==83)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  geom_point2(aes(subset=(node==89)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
RicCh
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/RictorChlorophyta.png",
       plot = RicCh,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

#RAPTOR
RapCh <- ChloroP + geom_tiplab(aes(color = RAPTOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  # geom_text(aes(label=node))+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
RapCh
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/RaptorChlorophyta.png",
       plot = RapCh,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

#SIN1
Sin1Ch <- ChloroP + geom_tiplab(aes(color = SIN1), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
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
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/SIN1Chlorophyta.png",
       plot = Sin1Ch,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

#LST8

lst8Ch <- ChloroP + geom_tiplab(aes(color = LST8), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  geom_point2(aes(subset=(node==1)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
lst8Ch
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/LST8Chlorophyta.png",
       plot = lst8Ch,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

#TOR
TorCh <- ChloroP + geom_tiplab(aes(color = TOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  # geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
TorCh
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/TORChlorophyta.png",
       plot = TorCh,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

HeatTreeChl <- ChloroP + geom_tiplab(size = 2, show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE,aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#29AF7FFF", "#287D8EFF", "#39568CFF","#440154FF"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  #geom_text(aes(label = node))+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_rootedge()+
  labs(title = "Chlorophyta Phylogenetic Tree",
       subtitle = "With HMMER Score Map")

ChlHeatPlot <- gheatmap(HeatTreeChl, df, offset = 8, font.size = 2, width = .4,  colnames = FALSE)+
  scale_fill_manual(name = "HMMER Score",
                    breaks = c("H","M","L",NA),
                    values = pal3,
                    limits = c("H","M","L",NA),
                    na.value = "#FFFFFF",
                    drop = FALSE)+
  theme(
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))+
  guides(fill = guide_legend(override.aes = list(label = "")))
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapChlorophyta.png",
       plot = ChlHeatPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)
ChlHeatPlot




# ------------------------------------------------------------------------------

RhodophytaL <- largeDataSet %>% filter(Super.Group == "Rhodophyta")
subsetdataframe <- RhodophytaL %>% select(Organism.Name, SIN1, RICTOR, RAPTOR, LST8,TOR) %>% distinct(Organism.Name, .keep_all = TRUE)
df <- column_to_rownames(subsetdataframe, var = "Organism.Name")





#Rhodophyta
RhodophytaTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/RhodophytaTreeP.phy")
RhodophytaTree$tip.label
RhodophytaTree$tip.label <- gsub("'","", RhodophytaTree$tip.label)
RhodophytaTree$tip.label 
Rhodophyta <- Rhodophyta %>% relocate(Organism.Name)
RhodoP <- ggtree(RhodophytaTree, branch.length = "none", laddarize = FALSE)
RhodoP <- RhodoP %<+% Rhodophyta


#RICTOR
RicRh <- RhodoP + geom_tiplab(aes(color = RICTOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  # geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
   geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  geom_point2(aes(subset=(node==15)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
RicRh
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/RictorRhodophyta.png",
       plot = RicRh,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

#RAPTOR
RapRh <- RhodoP + geom_tiplab(aes(color = RAPTOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
  # geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
RapRh
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/RaptorRhodophyta.png",
       plot = RapRh,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

#SIN1
Sin1Rh <- RhodoP + geom_tiplab(aes(color = SIN1), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  geom_point2(aes(subset=(node==15)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
  # geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
Sin1Rh
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/SIN1Rhodophyta.png",
       plot = Sin1Rh,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

#LST8
lst8Rh <- RhodoP + geom_tiplab(aes(color = LST8), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
  # geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
lst8Rh
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/LST8Rhodophyta.png",
       plot = lst8Rh,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

#TOR
TorRh <- RhodoP + geom_tiplab(aes(color = TOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_rootedge()+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+
  scale_color_manual(values = pal, limits = c("H","M","L","P",NA), drop = FALSE)
  # geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
TorRh
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/TORRhodophyta.png",
       plot = TorRh,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)


HeatTreeRho <- RhodoP+xlim(NA,+20)+ geom_tiplab(size = 2, show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE,aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#29AF7FFF", "#287D8EFF", "#39568CFF","#440154FF"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  #geom_text(aes(label = node))+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_rootedge()+
  labs(title = "Rhodophyta Phylogenetic Tree",
       subtitle = "With HMMER Score Map")

RhoHeatPlot <- gheatmap(HeatTreeRho, df, offset = 8, font.size = 1.8, width = .4,  colnames = FALSE)+
  scale_fill_manual(name = "HMMER Score",
                    breaks = c("H","M","L",NA),
                    values = pal3,
                    limits = c("H","M","L",NA),
                    na.value = "#FFFFFF",
                    drop = FALSE)+
  theme(
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))+
  guides(fill = guide_legend(override.aes = list(label = "")))
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapRhodophyta.png",
       plot = RhoHeatPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

RhoHeatPlot





# ------------------------------------------------------------------------------
#This will be interesting

StreptophytaL <- largeDataSet %>% filter(Super.Group == "Streptophyta")
subsetdataframe <- StreptophytaL %>% select(Organism.Name, SIN1, RICTOR, RAPTOR, LST8,TOR) %>% distinct(Organism.Name, .keep_all = TRUE)
df <- column_to_rownames(subsetdataframe, var = "Organism.Name")






StreptophytaTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/StreptophytaTreeP.phy")
StreptophytaTree$tip.label
StreptophytaTree$tip.label <- gsub("'", "", StreptophytaTree$tip.label)
StreptophytaTree$tip.label
Streptophyta <- Streptophyta %>% relocate(Organism.Name)
StrephP <- ggtree(StreptophytaTree, layout = "circular", branch.length = "none", laddarize = FALSE)
StrephP <- StrephP %<+% Streptophyta
StrephP 

#RICTOR
RicStr <- StrephP + geom_tiplab(aes(color = RICTOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==357)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
RicStr
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/RictorStreptophyta.png",
       plot = RicStr,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

#RAPTOR
RapStr <- StrephP + geom_tiplab(aes(color = RAPTOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_polygon(aes(color = `RAPTOR`, fill = `RAPTOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_text(aes(label=node), size = 2)+
  geom_point2(aes(subset=(node==345)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)
RapStr
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/RaptorStreptophyta.png",
       plot = RapStr,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

#SIN1
SIN1Str <- StrephP + geom_tiplab(aes(color = SIN1), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_polygon(aes(color = `SIN1`, fill = `SIN1`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_point2(aes(subset=(node==357)), shape = 23, color = "darkred", size = 6, fill = "darkred", alpha = .5)
SIN1Str
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/SIN1Streptophyta.png",
       plot = SIN1Str,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

#LST8

LST8Str <- StrephP + geom_tiplab(aes(color = LST8), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_polygon(aes(color = `LST8`, fill = `LST8`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)+
  geom_text(aes(label=node), size = 2)+
  geom_point2(aes(subset=(node==335)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==330)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5) +
  geom_point2(aes(subset=(node==48)), shape = 23, color = "darkblue", size = 6, fill = "darkblue", alpha = .5)
LST8Str
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/LST8Streptophyta.png",
       plot = LST8Str,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

TorStr <- StrephP + geom_tiplab(aes(color = TOR), size = 2,show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE)+
  geom_polygon(aes(color = `TOR`, fill = `TOR`, x = 0, y = 0))+
  scale_fill_manual(values = pal)+
  scale_color_manual(values = pal)
TorStr
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/TORStreptophyta.png",
       plot = TorStr,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

#
StrephP+geom_nodelab(size = 2.5)

subset1 <- tree_subset(StreptophytaTree, node = 421, levels_back = 0)
N421 <- subset1 %>%ggtree(branch.length = "none")
N421 <- N421 %<+% Streptophyta

subset2 <- tree_subset(StreptophytaTree, node = 465, levels_back = 0)
N465 <- subset2 %>% ggtree(branch.length = "none")
N465 <- N465 %<+% Streptophyta

subset3 <- tree_subset(StreptophytaTree, node = 376, levels_back = 0)
N376 <- subset3 %>% ggtree(branch.length = "none")
N376 <- N376 %<+% Streptophyta




HeatTreeStrep <- N421+xlim(NA,60)+geom_tiplab(size = 2, show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE,aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#29AF7FFF", "#287D8EFF", "#39568CFF","#440154FF"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_rootedge()+
  labs(title = "Asterids Phylogenetic Tree",
       subtitle = "With HMMER Score Map")

StrepHeatPlot <- gheatmap(HeatTreeStrep, df, offset = 10, font.size = 1.5, width = .4, colnames = FALSE)+
  scale_fill_manual(name = "HMMER Score",
                    breaks = c("H","M","L",NA),
                    values = pal3,
                    limits = c("H","M","L",NA),
                    na.value = "#FFFFFF",
                    drop = FALSE)+
  theme(
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))+
  guides(fill = guide_legend(override.aes = list(label = "")))
StrepHeatPlot
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapAsterids.png",
       plot = StrepHeatPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

HeatTreeStrep <- N465+xlim(NA,70)+geom_tiplab(size = 1.7, show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE,aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#29AF7FFF", "#287D8EFF", "#39568CFF","#440154FF"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_rootedge()+
  labs(title = "Rosids Phylogenetic Tree",
       subtitle = "With HMMER Score Map")

StrepHeatPlot <- gheatmap(HeatTreeStrep, df, offset = 16, font.size = 1.5, width = .4,  colnames = FALSE)+
  scale_fill_manual(name = "HMMER Score",
                    breaks = c("H","M","L",NA),
                    values = pal3,
                    limits = c("H","M","L",NA),
                    na.value = "#FFFFFF",
                    drop = FALSE)+
  theme(
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))+
  guides(fill = guide_legend(override.aes = list(label = "")))
StrepHeatPlot
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapRosids.png",
       plot = StrepHeatPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

HeatTreeStrep <- N376+xlim(NA,60)+geom_tiplab(size = 2, show.legend = TRUE, nudge_x = .3, linesize = .4, align = TRUE,aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#29AF7FFF", "#287D8EFF", "#39568CFF","#440154FF"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_rootedge()+
  labs(title = "Liliopsida Phylogenetic Tree",
       subtitle = "With HMMER Score Map")

StrepHeatPlot <- gheatmap(HeatTreeStrep, df, offset = 10, font.size = 1.5, width = .4,  colnames = FALSE)+
  scale_fill_manual(name = "HMMER Score",
                    breaks = c("H","M","L",NA),
                    values = pal3,
                    limits = c("H","M","L",NA),
                    na.value = "#FFFFFF",
                    drop = FALSE)+
  theme(
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))+
  guides(fill = guide_legend(override.aes = list(label = "")))
StrepHeatPlot
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapLiliopsida.png",
       plot = StrepHeatPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)




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

#Update the All Tree



subsetdataframe <- largeDataSet %>% select(Organism.Name, SIN1, RICTOR, RAPTOR, LST8,TOR) %>% distinct(Organism.Name, .keep_all = TRUE)
df <- column_to_rownames(subsetdataframe, var = "Organism.Name")


AllTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/AllTreeP.phy")
AllTree$tip.label <- gsub("'", "", AllTree$tip.label)
AllTree$node.label <- gsub("'", "", AllTree$node.label)
HTML <- HTML %>% relocate(Organism.Name)


AllTreeP <- ggtree(AllTree, layout = "circular", branch.length = "none", laddarize = FALSE)
AllTreeP <- AllTreeP %<+% HTML


tempdataframe <- largeDataSet
tempdataframe <- tempdataframe %>% mutate(NodeNumber = case_when(Super.Group == "Streptophyta" ~ which(AllTree$node.label == "Streptophyta") + length(AllTree$tip.label),
                                                                 Super.Group == "Alveolata" ~ which(AllTree$node.label == "Alveolata") + length(AllTree$tip.label),
                                                                 Super.Group == "Stramenopiles" ~ which(AllTree$node.label == "Stramenopiles") + length(AllTree$tip.label),
                                                                 Super.Group == "Rhizaria" ~ which(AllTree$node.label == "Rhizaria") + length(AllTree$tip.label),
                                                                 Super.Group == "Discoba" ~ which(AllTree$node.label == "Discoba") + length(AllTree$tip.label),
                                                                 Super.Group == "Metamonada" ~ which(AllTree$node.label == "Metamonada") + length(AllTree$tip.label),
                                                                 Super.Group == "Rhodophyta" ~ which(AllTree$node.label == "Rhodophyta") + length(AllTree$tip.label),
                                                                 Super.Group == "Chlorophyta" ~ which(AllTree$node.label == "Chlorophyta") + length(AllTree$tip.label))) %>%
  select(NodeNumber, Super.Group) %>% distinct(Super.Group, .keep_all = TRUE)

HeatTree <- AllTreeP+xlim(-30,NA)+
  #Streptophyta
  geom_highlight(data = tempdataframe,
                 mapping = aes(node = NodeNumber, fill = Super.Group))+
  scale_fill_manual(name = "Super Group",
                     breaks = c("Alveolata",
                                "Stramenopiles",
                                "Rhizaria",
                                "Streptophyta",
                                "Chlorophyta",
                                "Rhodophyta",
                                "Discoba",
                                "Metamonada"),
                     values = c("Alveolata" = "purple",
                                "Stramenopiles" = "brown",
                                "Rhizaria" = "orange",
                                "Streptophyta" = "red",
                                "Chlorophyta" = "green",
                                "Rhodophyta" = "yellow",
                                "Discoba" = "cyan",
                                "Metamonada" = "blue"
                     ))+
  new_scale_fill()


HeatTree

AllHeatPlot <- gheatmap(HeatTree, df, offset = 5, font.size = 1.5, width = .8, colnames = FALSE)+
  scale_fill_manual(name = "HMMER Score",
                    breaks = c("H","M","L","P","NA"),
                    values = pal3,
                    limits = c("H","M","L","P", "NA"),
                    na.value = "#FF000000",
                    drop = FALSE)+
  guides(fill = guide_legend(override.aes = list(label = "")))+
  theme(
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))

AllHeatPlot
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapAll.png",
       plot = AllHeatPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapAll.pdf",
       plot = AllHeatPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)


#Spacing In Between Tree
testSIN1Data <- largeDataSet %>% select(Organism.Name, SIN1)%>%
  pivot_longer(cols = !Organism.Name,names_to = "Protein", values_to = "Score")

testRICTORData <- largeDataSet %>% select(Organism.Name, RICTOR)%>%
  pivot_longer(cols = !Organism.Name,names_to = "Protein", values_to = "Score")

testRAPTORData <- largeDataSet %>% select(Organism.Name, RAPTOR)%>%
  pivot_longer(cols = !Organism.Name,names_to = "Protein", values_to = "Score")

testLST8Data <- largeDataSet %>% select(Organism.Name, LST8)%>%
  pivot_longer(cols = !Organism.Name,names_to = "Protein", values_to = "Score")

testTORData <- largeDataSet %>% select(Organism.Name, TOR)%>%
  pivot_longer(cols = !Organism.Name,names_to = "Protein", values_to = "Score")

testAllTree <- AllTreeP +
  geom_highlight(data = tempdataframe,
                 mapping = aes(node = NodeNumber, fill = Super.Group))+
  scale_fill_manual(name = "Super Group",
                    breaks = c("Alveolata",
                               "Stramenopiles",
                               "Rhizaria",
                               "Streptophyta",
                               "Chlorophyta",
                               "Rhodophyta",
                               "Discoba",
                               "Metamonada"),
                    values = c("Alveolata" = "purple",
                               "Stramenopiles" = "brown",
                               "Rhizaria" = "orange",
                               "Streptophyta" = "red",
                               "Chlorophyta" = "green",
                               "Rhodophyta" = "yellow",
                               "Discoba" = "cyan",
                               "Metamonada" = "blue"
                    ))+
  new_scale_fill()+
  geom_fruit(data = testSIN1Data,
             geom = geom_tile,
             mapping = aes(x = Protein, y = Organism.Name, fill = Score),width = .8)+
  geom_fruit(data = testRICTORData,
             geom = geom_tile,
             mapping = aes(x = Protein, y = Organism.Name, fill = Score), width = .8)+
  geom_fruit(data = testRAPTORData,
             geom = geom_tile,
             mapping = aes(x = Protein, y = Organism.Name, fill = Score), width = .8)+
  geom_fruit(data = testLST8Data,
             geom = geom_tile,
             mapping = aes(x = Protein, y = Organism.Name, fill = Score), width = .8)+
  geom_fruit(data = testTORData,
             geom = geom_tile,
             mapping = aes(x = Protein, y = Organism.Name, fill = Score), width = .8)+
  scale_fill_manual(name = "HMMER Score",
                    breaks = c("H","M","L","P","NA"),
                    values = pal3,
                    limits = c("H","M","L","P", "NA"),
                    na.value = "#FF000000",
                    drop = FALSE)+
  theme(
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))

testAllTree
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapAllv2.png",
       plot = testAllTree,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)









#This may be redundant at this point, work to eliminate

tdf <- HTML
tdf <- mutate(tdf, HasRictor = NA, HasRaptor = NA, HasTOR = NA, HasLST8 = NA, HasSIN1 = NA)
tdf[!is.na(tdf$RICTOR),]$HasRictor <- "Yes"
tdf[!is.na(tdf$RAPTOR),]$HasRaptor <- "Yes"
tdf[!is.na(tdf$SIN1),]$HasSIN1 <- "Yes"
tdf[!is.na(tdf$LST8),]$HasLST8 <- "Yes"
tdf[!is.na(tdf$TOR),]$HasTOR <- "Yes"





# Opisthokonta Tree File & Image
 testTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/TestingTreeP.phy")
 tree1 <- ggtree(testTree, layout = "daylight", branch.length = "none")
 tree1 <- tree1 + geom_highlight(node = 804, fill = "blue", alpha = .4)+
   geom_highlight(node = 891, fill = "brown", alpha = .4)+
   geom_highlight(node = 986, fill = "cyan", alpha = .4)+
   geom_highlight(node = 941, fill = "red", alpha = .4)+
   geom_highlight(node = 849, fill = "orange", alpha = .4)+
   geom_highlight(node = 819, fill = "purple", alpha = .4)+
   geom_highlight(node = 791, fill = "yellow", alpha = .4)+
   geom_highlight(node = 789, fill = "grey", alpha = .4)+
   geom_highlight(node = 843, fill = "pink", alpha = .4)+
   geom_highlight(node = 950, fill = "green", alpha = .4)
 tree1

 ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/TestImageOpisthokontTree.png",
        plot = tree1,
        width = 3840,
        height = 2160,
        units = "px",
        dpi = 320,
        limitsize = FALSE)




nodeids <- nodeid(AllTree, AllTree$node.label[nchar(AllTree$node.label)>6])
nodelab <- gsub("[\\.0-9]", "", AllTree$node.label[nchar(AllTree$node.label)>6])
nodedf <- data.frame(node=nodeids, label = nodelab)


P <- ggtree(AllTree, layout = "circular", branch.length = "none")
P <- P %<+% tdf
P +geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)


P <- P + geom_rootedge()+
  #Streptophyta
  geom_highlight(node =949,
                 fill= "red",
                 size=0.05)+
  #Metamonada
  geom_highlight(node =770,
                 fill="blue",
                 size=0.05)+
  #Stramenopiles
  geom_highlight(node =853,
                 fill="brown",
                 size=0.05)+
  #Alveolata
  geom_highlight(node =813,
                 fill="purple",
                 size=0.05)+
  #Chlorophyta
  geom_highlight(node =913,
                 fill="green",
                 size=0.05)+
  #Rhodophyta
  geom_highlight(node =904,
                 fill="yellow",
                 size=0.05)+
  #Discoba
  geom_highlight(node =783,
                 fill="cyan",
                 size=0.05)+
  #Rhizaria
  geom_highlight(node =807,
                 fill="orange",
                 size=0.05)


P
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/TestImageCladeTree.png",
       plot = P,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)


#Trying to add "fruit" to the tree
P2 <- P +
  guides(color = guide_legend(override.aes = list(label = "\u25A0", size = 10)))+
  geom_fruit(
    geom = geom_point,
    mapping = aes(x = HasSIN1, color = SIN1), offset = .05,
    shape = 15)+
  geom_fruit(
  geom = geom_point,
  mapping = aes(x = HasRictor, color = RICTOR), offset = .05,
  shape = 15)+
  geom_fruit(
    geom = geom_point,
    mapping = aes(x = HasRaptor, color = RAPTOR), offset = .05,
    shape = 15,
    axis.params = list(title = "RAPTOR"))+
  geom_fruit(
    geom = geom_point,
    mapping = aes(x = HasLST8, color = LST8), offset = .05,
    shape = 15)+
  geom_fruit(
    geom = geom_point,
    mapping = aes(x = HasTOR, color = TOR), offset = .05,
    shape = 15)+
  scale_color_manual(values = pal2, limits = c("H","M","L","P"), drop = FALSE, na.value = "#00000000")+
  guides(color = guide_legend(title = "Protein Score"))+
  theme(legend.position = "none")

P2

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/TestImageYesNoTree.png",
       plot = P2,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)


RicAll <- AllTreeP + geom_tiplab(aes(color = RICTOR), size = 2)+
  geom_polygon(aes(color = `RICTOR`, fill = `RICTOR`, x = 0, y = 0))+
  geom_rootedge()+
  geom_text(aes(label=node))+
  #geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2, color = "black")+
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

