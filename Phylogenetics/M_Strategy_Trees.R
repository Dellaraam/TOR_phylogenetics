# Photosynthesis trees and overall metabolism
# Kyle Johnson
# 4/3/2025

# Load in the master table

MasterTable <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Master_Table.csv")
MasterTable <- select(MasterTable, -X)

MasterTable %>% ggplot(aes(x = factor(RICTOR,level = c("H", "M", "L")), y = RICTORDomain, color = M.Strategy, size = C.score))+
  geom_jitter()+
  labs(title = "RICTOR Domain Score Distribution")+
  xlab(label = "RICTOR H/M/L")+
  ylab(label = "RICTOR Domain Scores")+
  #geom_text_repel(aes(label = Organism.Name), size = 2.2, show.legend = FALSE)+
  facet_wrap(~Super.Group,nrow = 1)




#Palettes-----------------------------------------------------------------------
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
pal4 <- c(
  "H" = "#f4649e",
  "M" = "#49e5aa",
  "L" = "#d2ebfb",
  "P" = "#f0d14f",
  "NA" = "#FF000000"
)

#Agreed on using Pal3 for heat maps going forward













#Stramenopile Tree -------------------------------------------------------------
StramenopileTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/StramenopileTreeP.phy")
StramenopileTree$tip.label <- gsub("'","", StramenopileTree$tip.label)


Stramenopiles <- MasterTable %>% filter(Super.Group == "Stramenopiles")
Stramenopiles <- Stramenopiles %>% relocate(Organism.Name)



subsetdataframe1 <- Stramenopiles%>% select(Organism.Name, SIN1, RICTOR, RAPTOR, LST8,TOR) %>% distinct(Organism.Name, .keep_all = TRUE)
df1 <- column_to_rownames(subsetdataframe1, var = "Organism.Name")


HeatTreeStramenopile <- StramenopileTree %>% ggtree(branch.length = "none", ladderize = FALSE)+geom_tiplab(size = 2)+xlim(NA,30)+geom_text(aes(label = node))+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 1)

HeatTreeStramenopile
StramHeatPlot <- gheatmap(HeatTreeStramenopile,df1, offset = 5, width = .8, font.size = 2, colnames = FALSE)
StramHeatPlot



#Alveolata ---------------------------------------------------------------------

AlveolataTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/AlveolataTreeP.phy")
AlveolataTree$tip.label <- gsub("'","", AlveolataTree$tip.label)

Alveolata <- MasterTable %>% filter(Super.Group == "Alveolata")
Alveolata <- Alveolata %>% relocate(Organism.Name)

subsetdataframe2 <- Alveolata %>% select(Organism.Name, 
                                         SIN1, 
                                         RICTOR, 
                                         RAPTOR, 
                                         LST8,
                                         TOR) %>% 
  distinct(Organism.Name, .keep_all = TRUE)

df2 <- column_to_rownames(subsetdataframe2, var = "Organism.Name")


AlvP <- ggtree(AlveolataTree, branch.length = "none", ladderize = FALSE)
AlvP <- AlvP  %<+% Alveolata
AlvP

HeatTreeAlv <- AlveolataTree %>% ggtree(branch.length = "none", ladderize = FALSE)+geom_tiplab(size = 2)+xlim(NA,40)+geom_text(aes(label = node))+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 1)+geom_rootedge()



HeatTreeAlv + geom_fruit(data = Alveolata, 
                         geom = geom_tile,
                         mapping = aes(x = M.Strategy, y = Organism.Name, fill = M.Strategy),width = .8)


AlvHeatPlot <- gheatmap(HeatTreeAlv,df2, offset = 5, width = .8, font.size = 2, colnames = FALSE)
AlvHeatPlot






# Excavata Combined Tree -------------------------------------------------------

ExcavataTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/ExcavataTreeP.phy")
ExcavataTree$tip.label <- gsub("'","", ExcavataTree$tip.label)

Excavata <- MasterTable %>% filter(Super.Group == "Discoba" | Super.Group == "Metamonada")
Excavata <- Excavata %>% relocate(Organism.Name)

subsetdataframe3 <- Excavata %>% select(Organism.Name, 
                                         SIN1, 
                                         RICTOR, 
                                         RAPTOR, 
                                         LST8,
                                         TOR) %>% 
  distinct(Organism.Name, .keep_all = TRUE)
df3 <- column_to_rownames(subsetdataframe3, var = "Organism.Name")




ExcavataTP <- ggtree(ExcavataTree, branch.length = "none", ladderize = FALSE)
ExcavataTP <- ExcavataTP  %<+% Excavata




ExcavataHeat <- ExcavataTree %>% ggtree(branch.length = "none", ladderize = FALSE)+xlim(NA,40)

ExcavataHeatPlot <- gheatmap(ExcavataHeat,df3, offset = 5, width = .8, font.size = 2, colnames = FALSE)+
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




ExcavataSavePlot <- ExcavataHeatPlot %<+% Excavata+geom_tiplab(size = 1.8, nudge_x = .3, linesize = .4, align = TRUE, aes(color=C.score), continuous = 'colour')+
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
  labs(title = "Excavates Phylogenetic Tree",
       subtitle = "With HMMER Score Map")

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapExcavates.png",
       plot = ExcavataSavePlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)




# Experimental -----------------------------------------------------------------
ARTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/ArTreeP.phy")
ARTree$tip.label <- gsub("'","", ARTree$tip.label)

AR <- MasterTable %>% filter(Super.Group == "Alveolata" | Super.Group == "Rhizaria")
AR <- AR %>% relocate(Organism.Name)

subsetdataframe4 <- AR %>% select(Organism.Name, 
                                        SIN1, 
                                        RICTOR, 
                                        RAPTOR, 
                                        LST8,
                                        TOR) %>% 
  distinct(Organism.Name, .keep_all = TRUE)
df4 <- column_to_rownames(subsetdataframe4, var = "Organism.Name")

Msubset1 <- AR %>% select(Organism.Name, M.Strategy)%>% distinct(Organism.Name, .keep_all = TRUE) #%>%
# pivot_longer(cols = !Organism.Name, names_to = "M.Strategy", values_to = "Type")
mdf1 <- column_to_rownames(Msubset1, var = "Organism.Name")




ARTP <- ggtree(ARTree, branch.length = "none", ladderize = FALSE)
ARTP <- ARTP  %<+% AR

ARTP


ARHeat <- ARTree %>% ggtree(branch.length = "none", ladderize = FALSE)+xlim(NA,40)

ARHeatPlot <- gheatmap(ARHeat,df4, offset = 5, width = .8, font.size = 2, colnames = FALSE)+
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

ARHeatPlot

ARSavePlot <- ARHeatPlot %<+% AR+geom_tiplab(size = 1.8, nudge_x = .3, linesize = .4, align = TRUE, aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#29AF7FFF", "#287D8EFF", "#39568CFF","#440154FF"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  geom_rootedge()+
  labs(title = "Alveolata and Rhizaria Phylogenetic Tree",
       subtitle = "With HMMER Score Map")

ARSavePlot

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapAlveolataRhizaria.png",
       plot = ARSavePlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)



# SAR --------------------------------------------------------------------------
SARTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/SARTreeP.phy")
SARTree$tip.label
SARTree$tip.label <- gsub("'","", SARTree$tip.label)
SARTree$tip.label


SAR <- MasterTable %>% filter(Super.Group == "Alveolata" | Super.Group == "Rhizaria" | Super.Group == "Stramenopiles")
SAR <- SAR %>% relocate(Organism.Name)

subsetdataframe5 <- SAR %>% select(Organism.Name, 
                                  SIN1, 
                                  RICTOR, 
                                  RAPTOR, 
                                  LST8,
                                  TOR) %>% 
  distinct(Organism.Name, .keep_all = TRUE)
df5 <- column_to_rownames(subsetdataframe5, var = "Organism.Name")


SARHeat <- SARTree %>% ggtree(ladderize = FALSE, branch.length = "none", layout = "fan", open.angle = 180)+geom_rootedge()
SARHeat



SARHeatPlot <- gheatmap(SARHeat,df5, offset = 5, width = .8, font.size = 2, colnames = FALSE)+
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
SARHeatPlot


SARSavePlot <- SARHeatPlot %<+% SAR+geom_tiplab(size = 1.8, nudge_x = .3, linesize = .4, align = TRUE, aes(color=C.score), continuous = 'colour')+
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
  labs(title = "SAR Phylogenetic Tree",
       subtitle = "With HMMER Score Map")

SARSavePlot







tempdataframe <- SAR
tempdataframe <- tempdataframe %>% mutate(NodeNumber = case_when(
                                                                 Super.Group == "Alveolata" ~ which(SARTree$node.label == "Alveolata") + length(SARTree$tip.label),
                                                                 Super.Group == "Stramenopiles" ~ which(SARTree$node.label == "Stramenopiles") + length(SARTree$tip.label),
                                                                 Super.Group == "Rhizaria" ~ which(SARTree$node.label == "Rhizaria") + length(SARTree$tip.label)))%>%
select(NodeNumber, Super.Group) %>% distinct(Super.Group, .keep_all = TRUE)








testSIN1Data <- SAR %>% select(Organism.Name, SIN1)%>%
  pivot_longer(cols = !Organism.Name,names_to = "Protein", values_to = "Score")

testRICTORData <- SAR %>% select(Organism.Name, RICTOR)%>%
  pivot_longer(cols = !Organism.Name,names_to = "Protein", values_to = "Score")

testRAPTORData <- SAR %>% select(Organism.Name, RAPTOR)%>%
  pivot_longer(cols = !Organism.Name,names_to = "Protein", values_to = "Score")

testLST8Data <- SAR %>% select(Organism.Name, LST8)%>%
  pivot_longer(cols = !Organism.Name,names_to = "Protein", values_to = "Score")

testTORData <- SAR %>% select(Organism.Name, TOR)%>%
  pivot_longer(cols = !Organism.Name,names_to = "Protein", values_to = "Score")

testAllTree <- SARHeat +
  geom_highlight(data = tempdataframe,
                 mapping = aes(node = NodeNumber, fill = Super.Group))+
  scale_fill_manual(name = "Super Group",
                    breaks = c("Alveolata","Stramenopiles","Rhizaria"),
                    values = c("Alveolata" = "#EFB911",
                               "Stramenopiles" = "#572100",
                               "Rhizaria" = "#FFD0AB"
                    ))+
  geom_rootedge()+
  new_scale_fill()+
  geom_fruit(data = testSIN1Data,
             geom = geom_tile,
             mapping = aes(x = Protein, y = Organism.Name, fill = Score),width = .5, offset = 0.04)+
  geom_fruit(data = testRICTORData,
             geom = geom_tile,
             mapping = aes(x = Protein, y = Organism.Name, fill = Score), width = .5, offset = 0.04)+
  geom_fruit(data = testRAPTORData,
             geom = geom_tile,
             mapping = aes(x = Protein, y = Organism.Name, fill = Score), width = .5, offset = 0.04)+
  geom_fruit(data = testLST8Data,
             geom = geom_tile,
             mapping = aes(x = Protein, y = Organism.Name, fill = Score), width = .5, offset = 0.04)+
  geom_fruit(data = testTORData,
             geom = geom_tile,
             mapping = aes(x = Protein, y = Organism.Name, fill = Score), width = .5, offset =0.04)+
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
    legend.key.spacing.x = unit(1,"cm"))+
  hexpand(.5, direction = -1)+
  vexpand(1, direction = -1)

testAllTree



ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapSAR.png",
       plot = testAllTree,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)
