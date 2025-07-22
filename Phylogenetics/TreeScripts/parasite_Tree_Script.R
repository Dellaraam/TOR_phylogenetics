
source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Maintenance_Scripts/Library_Script.R")


MasterTable <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Master_Table.csv")
MasterTable <- select(MasterTable, -X)

MasterTable <- MasterTable %>% mutate(M.Strategy = if_else(Phylum.name == "Apicomplexa", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Phylum.name == "Perkinsozoa", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Super.Group == "Streptophyta" & M.Strategy == "Parasite", "Streptophyta Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Family.name == "Amoebophryaceae", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(M.Strategy == "Parasite" & Super.Group != "Streptophyta", "Non-Plastid Parasite", M.Strategy, missing = M.Strategy))






parasites <- MasterTable %>% filter(M.Strategy == "Plastid Parasite" |
                                      M.Strategy == "Non-Plastid Parasite")


#Will have to do some removal of species that we have decided to remove prior
parasites <- parasites %>% filter(Organism.Name != "Moneuplotes crassus",
                                  Organism.Name != "Symbiodinium pilosum",
                                  Organism.Name != "Symbiodinium sp. CCMP2456",
                                  Organism.Name != "Symbiodinium necroappetens",
                                  Organism.Name != "Eimeria mitis",
                                  Organism.Name != "Eimeria necatrix",
                                  Organism.Name != "Eimeria praecox",
                                  Organism.Name != "Phytophthora citrophthora",
                                  Organism.Name != "Aphanomyces stellatus",
                                  Organism.Name != "Minidiscus trioculatus",
                                  Organism.Name != "Labyrinthula sp. Ha",
                                  Organism.Name != "Cafeteria roenbergensis",
                                  Organism.Name != "Phytophthora fragariaefolia",
                                  Organism.Name != "Phytophthora lilii",
                                  Organism.Name != "Peronosclerospora sorghi",
                                  Organism.Name != "Nothophytophthora sp. Chile5",
                                  Organism.Name != "Ochromonadaceae sp. CCMP2298",
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
                                  Organism.Name != "Marteilia pararefringens",
                                  Organism.Name != "Paramarteilia canceri",
                                  Organism.Name != "Cercozoa sp. M6MM")


#Create the ID for the tree from NCBI
write.table(parasites$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/parasites.txt", sep = "\t", row.names = F, col.names = F)


# Colors for the Tree
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



#Read in the tree and perform any necessary changes
parasiteTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/ParasitesTreeP.phy")
parasiteTree$tip.label
parasiteTree$tip.label <- gsub("'","", parasiteTree$tip.label)
parasiteTree$tip.label <- gsub("'","", parasiteTree$tip.label)
parasiteTree$tip.label
parasiteTree$node.label

#Prepare the parasites data for the tree
parasites <- parasites %>% relocate(Organism.Name)

#Subsetted parasites dataframe for the HMMER BIT scores Heat Map
subsetdataframe <- parasites %>% select(Organism.Name, 
                                        SIN1, 
                                        RICTOR, 
                                        RAPTOR, 
                                        LST8,
                                        TOR) %>% 
  distinct(Organism.Name, .keep_all = TRUE)
df <- column_to_rownames(subsetdataframe, var = "Organism.Name")

#Metabolic data for the Metabolic Strategies Heat Map
mSubSet <- parasites %>% select(Organism.Name, M.Strategy)%>% distinct(Organism.Name, .keep_all = TRUE) #%>%
# pivot_longer(cols = !Organism.Name, names_to = "M.Strategy", values_to = "Type")
mdf <- column_to_rownames(mSubSet, var = "Organism.Name")


#Subset data for the clade colorings
cdataframe <- parasites
cdataframe <- cdataframe %>% mutate(NodeNumber = case_when(
  Super.Group == "Discoba" ~ which(parasiteTree$node.label == "Discoba") + length(parasiteTree$tip.label),
  Super.Group == "Metamonada" ~ which(parasiteTree$node.label == "Metamonada") + length(parasiteTree$tip.label),
  Super.Group == "Stramenopiles" ~ which(parasiteTree$node.label == "Stramenopiles") + length(parasiteTree$tip.label),
  Super.Group == "Alveolata" ~ which(parasiteTree$node.label == "Alveolata") + length(parasiteTree$tip.label),
  Super.Group == "Rhizaria" ~ which(parasiteTree$node.label == "Rhizaria") + length(parasiteTree$tip.label)))%>%
  select(NodeNumber, Super.Group) %>% distinct(Super.Group, .keep_all = TRUE)



#Plain Tree
parasiteTreeP <- ggtree(parasiteTree, branch.length = "none", ladderize = FALSE)
parasiteTreeP <- parasiteTreeP  %<+% parasites

parasiteTreeP


#Layer 1: C score data and tip label data
parasiteTreeLayer1 <- parasiteTreeP+xlim(NA,25)+geom_tiplab(size = 1.7, nudge_x = .3, linesize = .4, align = TRUE, aes(color=C.score), continuous = 'colour')+
  scale_color_gradientn(colours=c("#B88100", "#3083DC","#D71D36"),
                        guide = guide_colorbar(order =1),
                        name = "Completeness Score")+
  geom_rootedge()


#parasiteTreeLayer1

#Layer 2: HMMER BIT score data and associated heatmap
parasiteTreeLayer2 <- gheatmap(parasiteTreeLayer1, df, offset = 4.5, width = .35, font.size = 2, colnames = FALSE)+
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

#parasiteTreeLayer2


#Layer 3 : Metabolic Data and associated heatmap
parasiteTreeLayer3 <- gheatmap(parasiteTreeLayer2,mdf, offset = 9.1, width = .08, colnames = FALSE)+
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



#parasiteTreeLayer3

parasiteTreeLayer4 <- parasiteTreeLayer3 +geom_highlight(data = cdataframe,
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

parasiteTreeLayer4


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapParasite.png",
       plot = parasiteTreeLayer4,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Tree_Images/HeatMapParasite.svg",
       plot = parasiteTreeLayer4,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)



