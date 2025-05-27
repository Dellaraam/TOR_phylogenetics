# Excavata Tree
# Purpose of this script is to have a quick and easy method of changing the excavate tree should the need arise
# 


source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Library_Script.R")



# Load in the master table

MasterTable <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Master_Table.csv")
MasterTable <- select(MasterTable, -X)

MasterTable <- MasterTable %>% mutate(M.Strategy = if_else(Phylum.name == "Apicomplexa", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Phylum.name == "Perkinsozoa", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Super.Group == "Streptophyta" & M.Strategy == "Parasite", "Streptophyta Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Family.name == "Amoebophryaceae", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(M.Strategy == "Parasite" & Super.Group != "Streptophyta", "Non-Plastid Parasite", M.Strategy, missing = M.Strategy))


MasterTable <- MasterTable %>% mutate(RICTOR = if_else(Organism.Name == "Euglena gracilis","P",RICTOR, missing = RICTOR))
ExcSubset <- MasterTable %>% filter(Super.Group == "Discoba" | Super.Group == "Metamonada")
ExcSubset <- ExcSubset %>% filter(Organism.Name != "Trypanosoma brucei equiperdum",
                                  Organism.Name != "Trpanosoma equiperdum",
                                  Organism.Name != "Trypanosoma rangeli",
                                  Organism.Name != "Trypanosoma rangeli SC58",
                                  Organism.Name != "Strigomonas culicis",
                                  Organism.Name != "Perkinsela sp. CCAP 1560/4",
                                  Organism.Name != "Giardia lamblia ATCC 50803",
                                  Organism.Name != "Spironucleus salmonicida",
                                  Organism.Name != "Hexamita inflata",
                                  Organism.Name != "Streblomastix strix",
                                  Organism.Name != "Aduncisulcus paluster",
                                  Organism.Name != "Monocercomonoides exilis",
                                  Organism.Name != "Paratrimastix pyriformis",
                                  Organism.Name != "Novymonas esmeraldas",
                                  Organism.Name != "Euglena gracilis")
# Establish ID for NCBI Common Tree
write.table(ExcSubset$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/NewTruncatedExc.txt", sep = "\t", row.names = F, col.names = F)

# Color Palettes for Trees
pal3 <- c(
  "H" = "#ae5a41",
  "P" = "#DBC795",
  "M" = "#559e83",
  "L" = "#B2D4AD",
  na_value = "#FF000000"
)
EMpal2 <- c(
  "Autotrophic" = "#3CBA26",
  "Endosymbiotic" = "purple",
  "Heterotroph" = "#A87142",
  "Mixotroph" = "#63E3C5",
  "Non-Plastid Parasite" = "#FE4A49",
  "Plastid Parasite" = "#D0D1AC",
  "Streptophyta Parasite" = "#B6A39E",
  na_value = "blue"
)


# Tree Creation Section

ExcavataTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/NewTruncatedExcTreeP.phy")
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
Msubset2 <- Excavata %>% select(Organism.Name, M.Strategy)%>% distinct(Organism.Name, .keep_all = TRUE) #%>%
# pivot_longer(cols = !Organism.Name, names_to = "M.Strategy", values_to = "Type")
mdf2 <- column_to_rownames(Msubset2, var = "Organism.Name")

tempdataframe <- Excavata
tempdataframe <- tempdataframe %>% mutate(NodeNumber = case_when(
  Super.Group == "Discoba" ~ which(ExcavataTree$node.label == "Discoba") + length(ExcavataTree$tip.label),
  Super.Group == "Metamonada" ~ which(ExcavataTree$node.label == "Metamonada") + length(ExcavataTree$tip.label)))%>%
  select(NodeNumber, Super.Group) %>% distinct(Super.Group, .keep_all = TRUE)


ExcavataTP <- ggtree(ExcavataTree, branch.length = "none", ladderize = FALSE)
ExcavataTP <- ExcavataTP  %<+% Excavata




ExcavataHeat <- ExcavataTree %>% ggtree(branch.length = "none", ladderize = FALSE)+xlim(NA,40)

ExcavataHeatPlot <- gheatmap(ExcavataHeat,df3, offset = 12, width = .6, font.size = 2, colnames = FALSE)+
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
    legend.key.spacing.x = unit(1,"cm"))+
  new_scale_fill()




ExcavataSavePlot <- ExcavataHeatPlot %<+% Excavata+geom_tiplab(size = 3, nudge_x = .3, linesize = .4, align = TRUE, aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#B88100", "#3083DC","#D71D36"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_rootedge()

experimentalExcplot <- gheatmap(ExcavataSavePlot,mdf2, offset = 18, width = .11, colnames = FALSE)+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = force,
                    na.value = "grey",
                    drop = FALSE)+
  theme(
    text = element_text(family = "serif"),
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))+
  new_scale_fill()+
  geom_highlight(data = tempdataframe,
                 mapping = aes(node = NodeNumber, fill = Super.Group), alpha = .2)+
  scale_fill_manual(name = "Super Group",
                    breaks = c("Alveolata",
                               "Stramenopiles",
                               "Rhizaria",
                               "Streptophyta",
                               "Chlorophyta",
                               "Rhodophyta",
                               "Discoba",
                               "Metamonada"),
                    values = c("Alveolata" = "#EFB911",
                               "Stramenopiles" = "#572100",
                               "Rhizaria" = "#2F0147",
                               "Streptophyta" = "#678516",
                               "Chlorophyta" = "#525601",
                               "Rhodophyta" = "#681114",
                               "Discoba" = "cyan",
                               "Metamonada" = "blue"
                    ))

experimentalExcplot

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/NewHeatMapExcavates.svg",
       plot = experimentalExcplot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)









