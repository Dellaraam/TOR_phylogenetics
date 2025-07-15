# Metabolic Strategies Breakdown for All Species
# Goal of the program is to take in the Master table with some edits
# To the metabolic Strategies (Especially in Chlorophyta, Rhodophyta,etc)


# source in the library file
source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Maintenance_Scripts/Library_Script.R")

# Read in the master table
MasterTable <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Master_Table.csv")
MasterTable <- select(MasterTable, -X)

# Edit out any species that we ultimately decided to remove from the list
# This should be every single clade we have looked at
MasterTable <- MasterTable %>%filter(Organism.Name != "Marteilia pararefringens",
                                     Organism.Name != "Paramarteilia canceri",
                                     Organism.Name != "Cercozoa sp. M6MM",
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
                                     Organism.Name != "Moneuplotes crassus",
                                     Organism.Name != "Symbiodinium pilosum",
                                     Organism.Name != "Symbiodinium sp. CCMP2456",
                                     Organism.Name != "Symbiodinium necroappetens",
                                     Organism.Name != "Eimeria mitis",
                                     Organism.Name != "Eimeria necatrix",
                                     Organism.Name != "Eimeria praecox",
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
                                     Organism.Name != "Picocystis sp. ML",
                                     Organism.Name != "Pistacia atlantica",
                                     Organism.Name != "Euglena gracilis")


# Make some additional edits to the metabolic strategies as discovered by additional research
MasterTable <- MasterTable %>% mutate(M.Strategy = if_else(Phylum.name == "Apicomplexa", "Plastid-Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Phylum.name == "Perkinsozoa", "Plastid-Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Super.Group == "Streptophyta" & M.Strategy == "Parasite", "Plastid-Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Family.name == "Amoebophryaceae", "Plastid-Parasite", M.Strategy, missing = M.Strategy),
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
                                      M.Strategy = if_else(Organism.Name == "Helicosporidium sp. ATCC 50920", "Plastid-Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Micromonas", "Mixotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Monoraphidium minutum", "Mixotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == " Chlorella sp. A99", "Mixotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Asterochloris sp. Cgr/DA1pho", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Apatococcus lobatus", "Mixotroph", M.Strategy, missing = M.Strategy))

#Final cleanup for metabolic strategies

MasterTable <- MasterTable %>% mutate(M.Strategy = if_else(Phylum.name == "Apicomplexa", "Plastid-Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Phylum.name == "Perkinsozoa", "Plastid-Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Family.name == "Amoebophryaceae", "Plastid-Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Nitzschia putrida", "Heterotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Carpediemonas membranifera", "Heterotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Lagenidium giganteum", "Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(M.Strategy == "Parasite" & Super.Group != "Streptophyta", "Non-Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(M.Strategy == "Parasite" & Super.Group == "Chlorophyta" | M.Strategy == "Parasite" & Super.Group == "Rhodophyta" | M.Strategy == "Parasite" & Super.Group == "Streptophyta", "Plastid-Parasite", M.Strategy, missing = M.Strategy))

# Reuse the same color palettes we have been using for other scripts
# Metabolic Colors
EMpal2 <- c(
  "Autotrophic" = "#3CBA26",
  "Endosymbiotic" = "purple",
  "Heterotroph" = "#A87142",
  "Mixotroph" = "#63E3C5",
  "Non-Plastid Parasite" = "#FE4A49",
  "Plastid-Parasite" = "#D0D1AC",
  "Streptophyta Parasite" = "#B6A39E"
)


#Torc presence colors
TORCPal <- c(
  "TORC1 & TORC2" ="#DACC3E",
  "TORC1 Only" = "#9CC5A1",
  "TORC2 Only" = "#CE8964",
  "No TORC" ="#819595"
  
)

# Creating the first pie chart
# This pie chart will give the metabolic breakdown for all species that have TORC1
# Create a copy of the master table that will be used as data for piechart
data1 <- MasterTable %>%
  filter(!is.na(RAPTOR) & is.na(RICTOR)) %>%
  count(M.Strategy)
data1 <- data1 %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data1$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

Torc1MetaPie <- ggplot(data1, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategies Breakdown TORC1 Only")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid-Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid-Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
Torc1MetaPie


MasterTable %>% filter(!is.na(RAPTOR) & !is.na(RICTOR)) %>% view()

# TORC1 and TORC2 present
data2 <- MasterTable %>%
  filter(!is.na(RAPTOR) & !is.na(RICTOR)) %>%
  count(M.Strategy)

data2 <- data2 %>%
  arrange(desc(M.Strategy)) %>%
  mutate(prop = n/sum(data2$n) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop)

Torc2MetaPie <- ggplot(data2, aes(x = "", y = prop, fill = M.Strategy))+
  geom_bar(stat = "identity", width = 2, color = "black")+
  coord_polar("y", start = 0 )+
  theme_void()+
  theme(legend.position = "none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy, "\n", round(prop,3), "%")), color = "black", size = 6)+
  labs(title = "Metabolic Strategies Breakdown TORC1 & TORC2")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid-Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid-Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
Torc2MetaPie


# No TORC components
data3 <- MasterTable %>%
  filter(is.na(RAPTOR) & is.na(RICTOR)) %>%
  count(M.Strategy)

data3 <- data3 %>%
  arrange(desc(M.Strategy)) %>%
  mutate(prop = n/sum(data3$n) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop)


NoTorcMetaPie <- ggplot(data3, aes(x = "", y = prop, fill = M.Strategy))+
  geom_bar(stat = "identity", width = 2, color = "black")+
  coord_polar("y", start = 0 )+
  theme_void()+
  theme(legend.position = "none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy, "\n", round(prop,3), "%")), color = "black", size = 6)+
  labs(title = "Metabolic Strategies Breakdown No TORC")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid-Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid-Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
NoTorcMetaPie


data4 <- MasterTable %>%
  filter(is.na(RAPTOR) & !is.na(RICTOR))%>%
  count(M.Strategy)

data4 <- data4 %>%
  arrange(desc(M.Strategy)) %>%
  mutate(prop = n/sum(data4$n) * 100) %>%
  mutate(ypos = cumsum(prop) -0.5*prop)


Torc2OnlyMetaPie <- ggplot(data4, aes(x = "", y = prop, fill = M.Strategy))+
  geom_bar(stat = "identity", width = 2, color = "black")+
  coord_polar("y", start = 0 )+
  theme_void()+
  theme(legend.position = "none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy, "\n", round(prop,3), "%")), color = "black", size = 6)+
  labs(title = "Metabolic Strategies Breakdown TORC2 Only")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid-Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid-Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
Torc2OnlyMetaPie




#Finally output the piecharts for editing work (powerpoint)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Torc1MetaPie.pptx",
       figure = Torc1MetaPie,
       units = "inches",
       width = 10,
       height = 7)
topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Torc2MetaPie.pptx",
       figure = Torc2MetaPie,
       units = "inches",
       width = 10,
       height = 7)
topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/NoTorcMetaPie.pptx",
       figure = NoTorcMetaPie,
       units = "inches",
       width = 10,
       height = 7)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Torc2OnlyMetaPie.pptx",
       figure = Torc2OnlyMetaPie,
       units = "inches",
       width = 10,
       height = 7)



