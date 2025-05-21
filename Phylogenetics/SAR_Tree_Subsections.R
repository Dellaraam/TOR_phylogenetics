#SAR Heat/Metabolic Tree
# Splitting this off from the main M_Strategy_Trees for convenience sake
# Goal will be to create "zoomed in" portions of the SAR tree

source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Library_Script.R")



# Load in the master table
# Change M strategy values to be more informative

MasterTable <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Master_Table.csv")
MasterTable <- select(MasterTable, -X)

MasterTable <- MasterTable %>% mutate(M.Strategy = if_else(Phylum.name == "Apicomplexa", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Phylum.name == "Perkinsozoa", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Super.Group == "Streptophyta" & M.Strategy == "Parasite", "Streptophyta Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Family.name == "Amoebophryaceae", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(M.Strategy == "Parasite" & Super.Group != "Streptophyta", "Non-Plastid Parasite", M.Strategy, missing = M.Strategy))


# Create the subsets for the S.A.R
StramSubset <- MasterTable %>% filter(Super.Group == "Stramenopiles")
StramSubset <- StramSubset %>%
  filter(Organism.Name != "Phytophthora citrophthora",
         Organism.Name != "Aphanomyces stellatus",
         Organism.Name != "Minidiscus trioculatus",
         Organism.Name != "Labyrinthula sp. Ha",
         Organism.Name != "Cafeteria roenbergensis",
         Organism.Name != "Phytophthora fragariaefolia",
         Organism.Name != "Phytophthora lilii",
         Organism.Name != "Peronosclerospora sorghi",
         Organism.Name != "Nothophytophthora sp. Chile5",
         Organism.Name != "Ochromonadaceae sp. CCMP2298")


AlvSubset <- MasterTable %>% filter(Super.Group == "Alveolata")
AlvSubset <- AlvSubset %>% filter(Organism.Name != "Moneuplotes crassus",
                                  Organism.Name != "Symbiodinium pilosum",
                                  Organism.Name != "Symbiodinium sp. CCMP2456",
                                  Organism.Name != "Symbiodinium necroappetens",
                                  Organism.Name != "Eimeria mitis",
                                  Organism.Name != "Eimeria necatrix",
                                  Organism.Name != "Eimeria praecox")

RhizSubset <- MasterTable %>% filter(Super.Group == "Rhizaria")
RhizSubset <- RhizSubset %>% filter(Organism.Name != "Marteilia pararefringens",
                                    Organism.Name != "Paramarteilia canceri",
                                    Organism.Name != "Cercozoa sp. M6MM")

SARSubset <- rbind(StramSubset, AlvSubset, RhizSubset)
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



SARTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/SARTreeP.phy")
SARTree$tip.label
SARTree$tip.label <- gsub("'","", SARTree$tip.label)
SARTree$tip.label


SARTree <- move.lineage(SARTree, 239, 247)
SARTree <- move.lineage(SARTree, 239, 274)
SARTree$node.label


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

Msubset4 <- SAR %>% select(Organism.Name, M.Strategy)%>% distinct(Organism.Name, .keep_all = TRUE) #%>%
# pivot_longer(cols = !Organism.Name, names_to = "M.Strategy", values_to = "Type")
mdf4 <- column_to_rownames(Msubset4, var = "Organism.Name")

tempdataframe <- SAR
tempdataframe <- tempdataframe %>% mutate(NodeNumber = case_when(
  Super.Group == "Alveolata" ~ which(SARTree$node.label == "Alveolata") + length(SARTree$tip.label),
  Super.Group == "Stramenopiles" ~ which(SARTree$node.label == "Stramenopiles") + length(SARTree$tip.label),
  Super.Group == "Rhizaria" ~ which(SARTree$node.label == "Rhizaria") + length(SARTree$tip.label)))%>%
  select(NodeNumber, Super.Group) %>% distinct(Super.Group, .keep_all = TRUE)


SARHeat <- SARTree %>% ggtree(ladderize = FALSE,branch.length = "none")+geom_rootedge()+geom_tree(linewidth = .25)+xlim(NA,20)
SARHeat + geom_nodelab(size = 2)+geom_text(aes(label = node),size = 4)



SARHeatPlot <- gheatmap(SARHeat,df5, offset = 3, width = .3, font.size = 2, colnames = FALSE)+
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
SARHeatPlot


c

SARSavePlot

SARSavePlotF <- gheatmap(SARSavePlot,mdf4, offset = 6, width = .05, colnames = FALSE)+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite","Endosymbiotic"),
                    values = EMpal2,
                    limits = force,
                    na.value = "grey",
                    drop = TRUE)+
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


SARSavePlotF






#-------------------------------------------------------------------------------
#Subsetting Section
#Stramenopiles Phytophthora Branch
subsettedTree <- tree_subset(SARTree,"Achlya hypogyna", levels_back = 2)
Test1 <- subsettedTree %>% ggtree(branch.length = "none")+
  xlim(NA,15)

Test1 <- Test1 %<+% SAR

TestHeat <- gheatmap(Test1,df5, offset = 3, width = .3, font.size = 2, colnames = FALSE)+
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

TestHeatWords <- TestHeat + xlim(NA, +25) + geom_tiplab(size = 2, nudge_x = .3, linesize = .4, align = TRUE, aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#B88100", "#3083DC","#D71D36"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  geom_rootedge()+
  geom_nodelab()+
  labs(title = "Subsection Phylo Tree",
       subtitle = "")


TestHeatMet <- gheatmap(TestHeatWords,mdf4, offset = 6, width = .05, colnames = FALSE)+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite","Endosymbiotic"),
                    values = EMpal2,
                    limits = force,
                    na.value = "grey",
                    drop = TRUE)+
  theme(
    text = element_text(family = "serif"),
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10),
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))+
  new_scale_fill()
TestHeatMet

#-------------------------------------------------------------------------------
# Hondaea fermentalgiana branch
subsettedTree <- tree_subset(SARTree,"Hondaea fermentalgiana", levels_back = 2)
Test1 <- subsettedTree %>% ggtree(branch.length = "none")+
  xlim(NA,15)

Test1 <- Test1 %<+% SAR

TestHeat <- gheatmap(Test1,df5, offset = 3, width = 2, font.size = 2, colnames = FALSE)+
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

TestHeatWords <- TestHeat + xlim(NA, +25) + geom_tiplab(size = 1.8, nudge_x = .3, linesize = .4, align = TRUE, aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#B88100", "#3083DC","#D71D36"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  geom_rootedge()+
  geom_nodelab()+
  labs(title = "Subsection Phylo Tree",
       subtitle = "")


TestHeatMet <- gheatmap(TestHeatWords,mdf4, offset = 6, width = .05, colnames = FALSE)+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite","Endosymbiotic"),
                    values = EMpal2,
                    limits = force,
                    na.value = "grey",
                    drop = TRUE)+
  theme(
    text = element_text(family = "serif"),
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10),
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))+
  new_scale_fill()
TestHeatMet





#-------------------------------------------------------------------------------
#Ochrophyta Branch
subsettedTree <- tree_subset(SARTree,"Parmales sp. scaly parma", levels_back = 2)
Test1 <- subsettedTree %>% ggtree(branch.length = "none")+
  xlim(NA,15)

Test1 <- Test1 %<+% SAR

TestHeat <- gheatmap(Test1,df5, offset = 3, width = .8, font.size = 2, colnames = FALSE)+
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

TestHeatWords <- TestHeat + xlim(NA, +25) + geom_tiplab(size = 2, nudge_x = .3, linesize = .4, align = TRUE, aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#B88100", "#3083DC","#D71D36"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  geom_rootedge()+
  geom_nodelab()+
  labs(title = "Subsection Phylo Tree",
       subtitle = "")


TestHeatMet <- gheatmap(TestHeatWords,mdf4, offset = 9, width = .15, colnames = FALSE)+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite","Endosymbiotic"),
                    values = EMpal2,
                    limits = force,
                    na.value = "grey",
                    drop = TRUE)+
  theme(
    text = element_text(family = "serif"),
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10),
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))+
  new_scale_fill()
TestHeatMet


#-------------------------------------------------------------------------------


#Plasmodium inui San Antonio 1

subsettedTree <- tree_subset(SARTree,"Plasmodium inui San Antonio 1", levels_back = 2)
Test1 <- subsettedTree %>% ggtree(branch.length = "none")+
  xlim(NA,15)

Test1 <- Test1 %<+% SAR

TestHeat <- gheatmap(Test1,df5, offset = 3, width = .3, font.size = 2, colnames = FALSE)+
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

TestHeatWords <- TestHeat + xlim(NA, +25) + geom_tiplab(size = 2, nudge_x = .3, linesize = .4, align = TRUE, aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#B88100", "#3083DC","#D71D36"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  geom_rootedge()+
  geom_nodelab()+
  labs(title = "Subsection Phylo Tree",
       subtitle = "")


TestHeatMet <- gheatmap(TestHeatWords,mdf4, offset = 6, width = .05, colnames = FALSE)+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite","Endosymbiotic"),
                    values = EMpal2,
                    limits = force,
                    na.value = "grey",
                    drop = TRUE)+
  theme(
    text = element_text(family = "serif"),
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10),
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))+
  new_scale_fill()
TestHeatMet


#-------------------------------------------------------------------------------
# Blepharisma stoltei
subsettedTree <- tree_subset(SARTree,"Blepharisma stoltei", levels_back = 1)
Test1 <- subsettedTree %>% ggtree(branch.length = "none")+
  xlim(NA,15)

Test1 <- Test1 %<+% SAR

TestHeat <- gheatmap(Test1,df5, offset = 3, width = .3, font.size = 2, colnames = FALSE)+
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

TestHeatWords <- TestHeat + xlim(NA, +25) + geom_tiplab(size = 2, nudge_x = .3, linesize = .4, align = TRUE, aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#B88100", "#3083DC","#D71D36"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  geom_rootedge()+
  geom_nodelab()+
  labs(title = "Subsection Phylo Tree",
       subtitle = "")


TestHeatMet <- gheatmap(TestHeatWords,mdf4, offset = 6, width = .05, colnames = FALSE)+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite","Endosymbiotic"),
                    values = EMpal2,
                    limits = force,
                    na.value = "grey",
                    drop = TRUE)+
  theme(
    text = element_text(family = "serif"),
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10),
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))+
  new_scale_fill()
TestHeatMet







