# Source the appropriate library file
source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Maintenance_Scripts/Library_Script.R")

#Palettes for later
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



# Load in the master table
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







#Main set of data to be used for the trees
EndoParasites <- MasterTable %>% filter(M.Strategy == "Plastid Parasite" | M.Strategy == "Non-Plastid Parasite" | M.Strategy == "Endosymbiotic")
EndoParasites <- EndoParasites %>% relocate(Organism.Name)
EndoParasites$Organism.Name <- gsub("'","", EndoParasites$Organism.Name)

#Remove the species we didn't have good data for here
EndoParasites <- EndoParasites %>% filter(Organism.Name != "Moneuplotes crassus",
                                          Organism.Name != "Symbiodinium pilosum",
                                          Organism.Name != "Symbiodinium sp. CCMP2456",
                                          Organism.Name != "Symbiodinium necroappetens",
                                          Organism.Name != "Eimeria mitis",
                                          Organism.Name != "Eimeria necatrix",
                                          Organism.Name != "Eimeria praecox",
                                          Organism.Name != "Marteilia pararefringens",
                                          Organism.Name != "Paramarteilia canceri",
                                          Organism.Name != "Cercozoa sp. M6MM",
                                          Organism.Name != "Trypanosoma brucei equiperdum",
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
                                          Organism.Name != "Cymbomonas tetramitiformis",
                                          Organism.Name != "Picocystis sp. ML")


#Read in the Tree file
ParasiteTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/ParasiteEndoTreeP.phy")
ParasiteTree$tip.label <- gsub("'","", ParasiteTree$tip.label)
ParasiteTree$tip.label

#For the first heat map (Scores)
subsetdataframe <- EndoParasites %>% select(Organism.Name, 
                                        SIN1, 
                                        RICTOR, 
                                        RAPTOR, 
                                        LST8,
                                        TOR) %>% 
  distinct(Organism.Name, .keep_all = TRUE)
df <- column_to_rownames(subsetdataframe, var = "Organism.Name")

#For the second heat map (Metabolic Strategy)
Msubset <- EndoParasites %>% select(Organism.Name, M.Strategy)%>% distinct(Organism.Name, .keep_all = TRUE)
mdf <- column_to_rownames(Msubset, var = "Organism.Name")

ParasiteTree$node.label == "Rhizaria"

#Error can't recylcle here, Come back to later
cladecolorframe <- EndoParasites
cladecolorframe <- cladecolorframe %>% mutate(NodeNumber = case_when(
  Super.Group == "Stramenopiles" ~ which(ParasiteTree$node.label == "Stramenopiles") + length(ParasiteTree$tip.label),
  Super.Group == "Alveolata" ~ which(ParasiteTree$node.label == "Alveolata") + length(ParasiteTree$tip.label),
  Super.Group == "Discoba" ~ which(ParasiteTree$node.label == "Discoba") + length(ParasiteTree$tip.label),
  Super.Group == "Metamonada" ~ which(ParasiteTree$node.label == "Metamonada") + length(ParasiteTree$tip.label),
  Super.Group == "Rhizaria" ~ which(ParasiteTree$node.label == "Rhizaria") + length(ParasiteTree$tip.label),
  Super.Group == "Streptophyta" ~ which(ParasiteTree$node.label == "Streptophyta") + length(ParasiteTree$tip.label),
  Super.Group == "Chlorophyta" ~ which(ParasiteTree$node.label == "Chlorophyta") + length(ParasiteTree$tip.label)))%>%
   select(NodeNumber, Super.Group) %>% distinct(Super.Group, .keep_all = TRUE)
#   ,
#  ,
#   Super.Group == "Metamonada" ~ which(ParasiteTree$node.label == "Metamonada") + length(ParasiteTree$tip.label)




#Plain Tree (circular)
ParasiteTreeLayer1c <- ggtree(ParasiteTree, branch.length = "none", layout = "circular")
ParasiteTreeLayer1c <- ParasiteTreeLayer1c %<+% EndoParasites

#Plain Tree (rectangular)
ParasiteTreeLayer1r <- ggtree(ParasiteTree, branch.length = "none")
ParasiteTreeLayer1r %<+% EndoParasites

#Plain Tree (half-circle)
ParasiteTreeLayer1d <- ggtree(ParasiteTree, branch.length = "none", layout = "fan", open.angle = 180)
ParasiteTreeLayer1d <- ParasiteTreeLayer1d



ParasiteTreelayer2c <- ParasiteTreeLayer1c +
  geom_rootedge()
ParasiteTreelayer2c


ParasiteTreelayer3c <- gheatmap(ParasiteTreelayer2c, df, offset = 6.5, width = .55, font.size = 2, colnames = FALSE)+
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
ParasiteTreelayer3c


ParasiteTreelayer4c <- gheatmap(ParasiteTreelayer3c,mdf, offset = 19, width = .125, colnames = FALSE)+
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
  new_scale_fill()


ParasiteTreeLayer5c<- ParasiteTreelayer4c + geom_highlight(data = cladecolorframe,
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

ParasiteTreeLayer5c

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/ParasitesEndosymbiotes.png",
       plot = ParasiteTreeLayer5c,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/ParasitesEndosymbiotes.svg",
       plot = ParasiteTreeLayer5c,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)
