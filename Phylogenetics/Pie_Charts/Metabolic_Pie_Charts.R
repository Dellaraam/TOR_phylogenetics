#Metabolic Charts Super Group Divided

source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Library_Script.R")

MasterTable <- read_csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Master_Table.csv")
MasterTable <- MasterTable %>% select(-...1)
MasterTable <- MasterTable %>%
  mutate(SIN1Domain = if_else(Super.Group == "Chlorophyta",NA,SIN1Domain),
         SIN1All = if_else(Super.Group == "Chlorophyta", NA, SIN1All),
         RICTORDomain = if_else(Super.Group == "Chlorophyta", NA, RICTORDomain),
         RICTORAll = if_else(Super.Group == "Chlorophyta", NA, RICTORAll))


MasterTable <- MasterTable %>% mutate(M.Strategy = if_else(Phylum.name == "Apicomplexa", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Phylum.name == "Perkinsozoa", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Super.Group == "Streptophyta" & M.Strategy == "Parasite", "Streptophyta parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Family.name == "Amoebophryaceae", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(M.Strategy == "Parasite" & Super.Group != "Streptophyta", "Non-Plastid Parasite", M.Strategy, missing = M.Strategy))


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





EMpal2 <- c(
  "Autotrophic" = "#3CBA26",
  "Endosymbiotic" = "purple",
  "Heterotroph" = "#A87142",
  "Mixotroph" = "#63E3C5",
  "Non-Plastid Parasite" = "#FE4A49",
  "Plastid Parasite" = "#D0D1AC",
  "Streptophyta Parasite" = "#B6A39E"
)



TORCPal <- c(
  "TORC1 & TORC2" ="#DACC3E",
  "TORC1 Only" = "#9CC5A1",
  "TORC2 Only" = "#CE8964",
  "No TORC" ="#819595"
  
)






data <- MasterTable %>%filter(Super.Group == "Alveolata") %>%count(M.Strategy)
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
  labs(title = "Metabolic Strategies Breakdown Alveolata")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie1

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/MetabolicAlveolataPieChart.png",
       plot = pie1,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/MetabolicAlveolataPieChart.pptx",
       figure = pie1,
       units = "inches",
       width = 10,
       height = 7)





data <- MasterTable %>%filter(Super.Group == "Stramenopiles") %>%count(M.Strategy)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie2 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategies Breakdown Stramenopiles")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie2

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/MetabolicStramenopilesPieChart.png",
       plot = pie2,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/MetabolicStramenopilesPieChart.pptx",
       figure = pie2,
       units = "inches",
       width = 10,
       height = 7)


data <- MasterTable %>%filter(Super.Group == "Rhizaria") %>%count(M.Strategy)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie3 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategies Breakdown Rhizaria")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie3


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/MetabolicRhizariaPieChart.png",
       plot = pie3,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/MetabolicRhizariaPieChart.pptx",
       figure = pie3,
       units = "inches",
       width = 10,
       height = 7)


data <- MasterTable %>%filter(Super.Group == "Alveolata"|Super.Group == "Stramenopiles"|Super.Group == "Rhizaria") %>%count(M.Strategy)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie4 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategies Breakdown SAR")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie4

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/MetabolicSARPieChart.png",
       plot = pie4,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/MetabolicSARPieChart.pptx",
       figure = pie4,
       units = "inches",
       width = 10,
       height = 7)



data <- MasterTable %>%filter(Super.Group == "Discoba") %>%count(M.Strategy)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie5 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategies Discoba")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie5


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/MetabolicDiscobaPieChart.png",
       plot = pie5,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/MetabolicDiscobaPieChart.pptx",
       figure = pie5,
       units = "inches",
       width = 10,
       height = 7)

data <- MasterTable %>%filter(Super.Group == "Metamonada") %>%count(M.Strategy)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie6 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategies Metamonada")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie6


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/MetabolicMetamonadaPieChart.png",
       plot = pie6,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/MetabolicMetamonadaPieChart.pptx",
       figure = pie6,
       units = "inches",
       width = 10,
       height = 7)



data <- MasterTable %>%filter(Super.Group == "Discoba"|Super.Group == "Metamonada") %>%count(M.Strategy)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie7 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategies Breakdown Excavates")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie7

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/MetabolicExcavatesPieChart.png",
       plot = pie7,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/MetabolicExcavatesPieChart.pptx",
       figure = pie7,
       units = "inches",
       width = 10,
       height = 7)


data <- MasterTable %>%filter(Super.Group == "Streptophyta") %>%count(M.Strategy)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie8 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategies Breakdown Streptophyta")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie8

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/MetabolicStreptophytaPieChart.png",
       plot = pie8,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/MetabolicStreptophytaPieChart.pptx",
       figure = pie8,
       units = "inches",
       width = 10,
       height = 7)




data <- MasterTable %>%filter(Super.Group == "Chlorophyta") %>%count(M.Strategy)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie9 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategies Breakdown Chlorophyta")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie9

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/MetabolicChlorophytaPieChart.png",
       plot = pie9,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/MetabolicChlorophytaPieChart.pptx",
       figure = pie9,
       units = "inches",
       width = 10,
       height = 7)



data <- MasterTable %>%filter(Super.Group == "Rhodophyta") %>%count(M.Strategy)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie10 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategies Breakdown Rhodophyta")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie10

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/MetabolicRhodophytaPieChart.png",
       plot = pie10,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/MetabolicRhodophytaPieChart.pptx",
       figure = pie10,
       units = "inches",
       width = 10,
       height = 7)



data <- MasterTable %>%filter(Super.Group == "Chlorophyta"|Super.Group == "Rhodophyta"|Super.Group == "Streptophyta") %>%count(M.Strategy)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie11 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategies Breakdown Archaeplastida")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie11

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/MetabolicArchPieChart.png",
       plot = pie11,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/MetabolicArchPieChart.pptx",
       figure = pie11,
       units = "inches",
       width = 10,
       height = 7)


data <- MasterTable%>%count(M.Strategy)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie12 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategies Breakdown All Super Groups")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie12

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/MetabolicAllPieChart.png",
       plot = pie12,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/MetabolicAllPieChart.pptx",
       figure = pie12,
       units = "inches",
       width = 10,
       height = 7)


