# Streptophyta Tree
# Kyle Johnson


# Source necessary libraries
source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Maintenance_Scripts/Library_Script.R")

#Prepare necessary color palettes
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

# Read in the Master Table
MasterTable <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Master_Table.csv")
MasterTable <- select(MasterTable, -X)

MasterTable <- MasterTable %>% mutate(M.Strategy = if_else(Phylum.name == "Apicomplexa", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Phylum.name == "Perkinsozoa", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Super.Group == "Streptophyta" & M.Strategy == "Parasite", "Plastid Parasite", M.Strategy, missing = M.Strategy),
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
                                      M.Strategy = if_else(M.Strategy == "Parasite" & Super.Group != "Streptophyta", "Non-Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Chlorella sorokiniana", "Mixotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Chlorella", "Mixotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Micractinium conductrix", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Ostreobium quekettii", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Helicosporidium sp. ATCC 50920", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Micromonas", "Mixotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Monoraphidium minutum", "Mixotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == " Chlorella sp. A99", "Mixotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Asterochloris sp. Cgr/DA1pho", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Apatococcus lobatus", "Mixotroph", M.Strategy, missing = M.Strategy))



StrepTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/TruncatedStrepTreeP.phy")
StrepTree$tip.label
StrepTree$tip.label <- gsub("'","", StrepTree$tip.label)


Strep <- MasterTable %>% filter(Super.Group == "Streptophyta")
Strep <- Strep %>% relocate(Organism.Name)
Strep$Organism.Name <- gsub("'","", Strep$Organism.Name)


#Protein Subsetted Data
subsetdataframeProteins <- Strep %>% select(Organism.Name, 
                                            SIN1, 
                                            RICTOR, 
                                            RAPTOR, 
                                            LST8,
                                            TOR) %>% 
  distinct(Organism.Name, .keep_all = TRUE)
dfProteins <- column_to_rownames(subsetdataframeProteins, var = "Organism.Name")


#Metabolic Subsetted Data for Streptophyta
MsubsetStrep <- Strep %>% select(Organism.Name, M.Strategy)%>% distinct(Organism.Name, .keep_all = TRUE) #%>%
# pivot_longer(cols = !Organism.Name, names_to = "M.Strategy", values_to = "Type")
mdfStrep <- column_to_rownames(MsubsetStrep, var = "Organism.Name")


C.scoreStrep <- Strep %>% select(Organism.Name, C.score)
dfCscore <- column_to_rownames(C.scoreStrep, var = "Organism.Name")




#Naked Tree with associated data
StrepTreeP <- ggtree(StrepTree, branch.length = "none", layout = "circular", ladderize = FALSE)
StrepTreeP <- StrepTreeP  %<+% Strep


#Layer 1
StrepLayer1 <- StrepTreeP+geom_tree(aes(color = C.score))+
  scale_color_gradientn(colours=c("#B88100", "#3083DC","#D71D36"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  geom_rootedge()+
  theme(legend.position = "none")
StrepLayer1


StrepLayer1
Streplayer2 <- gheatmap(StrepLayer1, dfProteins, offset = .5, width = .55, font.size = 2, colnames = FALSE)+
  scale_fill_manual(name = "HMMER Score",
                    breaks = c("H","M","L","P",NA),
                    values = pal3,
                    limits = c("H","M","L","P", NA),
                    na.value = "#FFFFFF",
                    drop = FALSE)+
  theme(
    legend.position = "none",
    text = element_text(family = "serif"),
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))+
  new_scale_fill()

Streplayer2

StrepLayer3 <- gheatmap(Streplayer2,mdfStrep, offset = 12.5, width = .125, colnames = FALSE)+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = force,
                    na.value = "grey",
                    drop = FALSE)+
  theme_void() +
  theme(
    legend.position = "none",
    text = element_text(family = "serif"),
    legend.background=element_rect(fill=NA),
    legend.title=element_text(size=10), 
    legend.text=element_text(size=5.5),
    legend.spacing.y = unit(0.02, "cm"),
    legend.key.spacing.x = unit(1,"cm"))+
  new_scale_fill()


StrepLayer3

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapMetMapStrep.png",
       plot = Streplayer2,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapMetMapStrep.svg",
       plot = StrepLayer3,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)


