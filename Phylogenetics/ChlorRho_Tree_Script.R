
source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Library_Script.R")



# Load in the master table

MasterTable <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Master_Table.csv")
MasterTable <- select(MasterTable, -X)

MasterTable <- MasterTable %>% mutate(M.Strategy = if_else(Phylum.name == "Apicomplexa", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Phylum.name == "Perkinsozoa", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Super.Group == "Streptophyta" & M.Strategy == "Parasite", "Streptophyta Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Family.name == "Amoebophryaceae", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Nitzschia putrida", "Heterotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Carpediemonas membranifera", "Heterotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Lagenidium giganteum", "Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Apatococcus lobatus", "Mixotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Chromochloris zofingiensis", "Mixotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Scenedesmus", "Mixotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Prototheca wickerhamii", "Heterotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Apatococcus fuscideae", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Symbiochloris irregularis", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Trebouxia sp. C0004", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Trebouxia sp. C0006", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Trebouxia sp. C0005", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Trebouxia sp. C0009 RCD-2024", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Trebouxia sp. C0010 RCD-2024", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(M.Strategy == "Parasite" & Super.Group != "Streptophyta", "Non-Plastid Parasite", M.Strategy, missing = M.Strategy))





ChloroRhoSubset <- MasterTable %>% filter(Super.Group == "Chlorophyta" | Super.Group == "Rhodophyta")
ChloroRhoSubset <- ChloroRhoSubset %>% filter(Organism.Name != "Cymbomonas tetramitiformis",
                                              Organism.Name != "Picocystis sp. ML")



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

# Chlorophyta Rhodophyta Tree with Heat Maps
ChlorRhoTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/ChloroRodoTree.phy")
ChlorRhoTree$tip.label
ChlorRhoTree$tip.label <- gsub("'","", ChlorRhoTree$tip.label)
ChlorRhoTree$tip.label <- gsub("'","", ChlorRhoTree$tip.label)
ChlorRhoTree$tip.label
ChlorRhoTree$node.label

#Chlorophyta Rhodophyta Data to add to the tree
ChlorRho <- MasterTable %>% filter(Super.Group == "Rhodophyta" | Super.Group == "Chlorophyta")
ChlorRho <- ChlorRho %>% filter(Organism.Name != "Cymbomonas tetramitiformis",
                                Organism.Name != "Picocystis sp. ML") 
ChlorRho <- ChlorRho %>% relocate(Organism.Name)
ChlorRho$Organism.Name
ChlorRho$Organism.Name <- gsub("'","", ChlorRho$Organism.Name)

#TOR Components subsetted data for Chlorophyta/Rhodophyta
subsetdataframe6 <- ChlorRho %>% select(Organism.Name, 
                                        SIN1, 
                                        RICTOR, 
                                        RAPTOR, 
                                        LST8,
                                        TOR) %>% 
  distinct(Organism.Name, .keep_all = TRUE)
df6 <- column_to_rownames(subsetdataframe6, var = "Organism.Name")

#Metabolic Subsetted Data for Chlorophyta/Rhodophyta
Msubset3 <- ChlorRho %>% select(Organism.Name, M.Strategy)%>% distinct(Organism.Name, .keep_all = TRUE) #%>%
# pivot_longer(cols = !Organism.Name, names_to = "M.Strategy", values_to = "Type")
mdf3 <- column_to_rownames(Msubset3, var = "Organism.Name")

#For Clade coloring
tempdataframe <- ChlorRho
tempdataframe <- tempdataframe %>% mutate(NodeNumber = case_when(
  Super.Group == "Chlorophyta" ~ which(ChlorRhoTree$node.label == "Chlorophyta") + length(ChlorRhoTree$tip.label),
  Super.Group == "Rhodophyta" ~ which(ChlorRhoTree$node.label == "Rhodophyta") + length(ChlorRhoTree$tip.label)))%>%
  select(NodeNumber, Super.Group) %>% distinct(Super.Group, .keep_all = TRUE)



#Plain Tree
ChlorRhoTreeP <- ggtree(ChlorRhoTree, branch.length = "none", ladderize = FALSE)
ChlorRhoTreeP <- ChlorRhoTreeP  %<+% ChlorRho
ChlorRhoTreeP+geom_text(aes(label = node))+geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 2)+geom_tiplab()


#Layer 1
# Tip label layer and C.score heatmap layer
ChloRhoLayer1 <- ChlorRhoTreeP + xlim(NA, +25) + geom_tiplab(size = 1.8, nudge_x = .3, linesize = .4, align = TRUE, aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#B88100", "#3083DC","#D71D36"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  geom_rootedge()

# Layer 2
# TOR Components Heat map layer. Requires the first layer to attach to
# Also requires a new_scale_fill() function call to establish layered fill scales
ChloRhoLayer2 <- gheatmap(ChloRhoLayer1, df6, offset = 3.5, width = .55, font.size = 2, colnames = FALSE)+
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

ChloRhoLayer2
# Layer 3
# Metabolic Strategy Layer
# Requires the second layer
ChloRhoLayer3 <- gheatmap(ChloRhoLayer2,mdf3, offset = 7.1, width = .125, colnames = FALSE)+
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

#Finished Tree
ChloRhoLayer3


topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/ChlorophytaRhodophytaTree.pptx",
       figure = ChloRhoLayer3,
       units = "inches",
       width = 10,
       height = 7)

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/ChlorophytaRhodophyta.png",
       plot = ChloRhoLayer3,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/ChlorophytaRhodophyta.svg",
       plot = ChloRhoLayer3,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)