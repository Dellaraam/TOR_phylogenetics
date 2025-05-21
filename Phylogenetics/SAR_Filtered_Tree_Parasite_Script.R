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
FilteredSAR <- SARSubset %>% filter(M.Strategy != "Non-Plastid Parasite",
                                    M.Strategy != "Endosymbiotic",
                                    M.Strategy != "Plastid Parasite")
#Create IDs
write.table(FilteredSAR$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/FilteredSAR.txt", sep = "\t", row.names = F, col.names = F)



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



SARTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/FilteredSARTreeP.phy")
SARTree$tip.label
SARTree$tip.label <- gsub("'","", SARTree$tip.label)
SARTree$tip.label

SAR <- FilteredSAR %>% relocate(Organism.Name)

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


SARHeat <- SARTree %>% ggtree(ladderize = FALSE,branch.length = "none", layout = "circular")+geom_rootedge()+geom_tree(linewidth = .25)
SARHeat + geom_nodelab(size = 2)+geom_text(aes(label = node),size = 4)



SARHeatPlot <- gheatmap(SARHeat,df5, offset = 3, width = .6, font.size = 2, colnames = FALSE)+
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

SARSavePlot <- SARHeatPlot %<+% SAR+geom_tree(aes(color = C.score))+
  scale_color_gradientn(colours=c("#B88100", "#3083DC","#D71D36"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  # geom_cladelab(node=106, label="Heterotrophic", align = FALSE, geom = 'label',offset=2.5,barsize = 3)+
  # geom_cladelab(node=2, label="Filter-Feeder", align = FALSE, geom = 'label',offset=2.5,barsize = 3http://127.0.0.1:41255/graphics/plot_zoom_png?width=1707&height=912)+
  # geom_cladelab(node=92, label="Parasite", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=116, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=111, label="Autotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  # geom_cladelab(node=94, label="Heterotrophic", align = FALSE, geom = 'label',offset=3,barsize = 3)+
  geom_rootedge()+
  labs(title = "Figure 6",
       subtitle = paste0("Phylogenetic HMMER Score Map: \n",
                         "SAR Super Groups"))

SARSavePlot

SARSavePlotF <- gheatmap(SARSavePlot,mdf4, offset = 9, width = .15, colnames = FALSE)+
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

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/SARParasiteFilteredPlot.png",
       plot = SARSavePlotF,
       width = 3840,
       height = 7320,
       units = "px",
       dpi = 320,
       limitsize = FALSE)





data <- FilteredSAR%>%count(M.Strategy)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie1 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategies Breakdown")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))



data <- FilteredSAR%>%filter(!is.na(RICTOR))%>%count(M.Strategy)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie1 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategies Breakdown")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie1

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/FilteredPieWithRictor.png",
       plot = pie1,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/FilteredPieWithRictor.pptx",
       figure = pie1,
       units = "inches",
       width = 10,
       height = 7)










data <- FilteredSAR%>%filter(is.na(RICTOR))%>%count(M.Strategy)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie1 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategies Breakdown")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie1


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/FilteredPieNoRictor.png",
       plot = pie1,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/FilteredPieNoRictor.pptx",
       figure = pie1,
       units = "inches",
       width = 10,
       height = 7)








