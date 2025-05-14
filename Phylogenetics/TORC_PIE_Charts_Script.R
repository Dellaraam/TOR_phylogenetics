




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



MasterTable <- MasterTable %>%mutate(Components = NA) %>% mutate(Components = if_else(!is.na(RICTOR) & is.na(RAPTOR),"TORC2 Only",Components, missing = Components),
                                                                 Components = if_else(is.na(RICTOR) & !is.na(RAPTOR), "TORC1 Only", Components, missing = Components),
                                                                 Components = if_else(!is.na(RICTOR) & !is.na(RAPTOR), "TORC1 & TORC2", Components, missing = Components),
                                                                 Components = if_else(is.na(RICTOR) & is.na(RAPTOR), "No TORC", Components, missing = Components))

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


#Palettes

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

#-------------------------------------------------------------------------------



data <- MasterTable %>%filter(Super.Group == "Alveolata") %>%count(Components)
data <- data %>%
  arrange(desc(Components))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie1 <- ggplot(data, aes(x="", y=prop, fill=Components)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(Components,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Alveolata")+
  scale_fill_manual(name = "TOR Components",
                    breaks = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    values = TORCPal,
                    limits = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie1



ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/ComponentsAlveolataPieChart.png",
       plot = pie1,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/ComponentsAlveolataPieChart.pptx",
       figure = pie1,
       units = "inches",
       width = 10,
       height = 7)


data <- MasterTable %>%filter(Super.Group == "Stramenopiles")%>%count(Components)
data <- data %>%
  arrange(desc(Components))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie2 <- ggplot(data, aes(x="", y=prop, fill=Components)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(Components,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Stramenopiles")+
  scale_fill_manual(name = "TOR Components",
                    breaks = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    values = TORCPal,
                    limits = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie2

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/ComponentsStramenopilesPieChart.png",
       plot = pie2,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/ComponentsStramenopilesPieChart.pptx",
       figure = pie2,
       units = "inches",
       width = 10,
       height = 7)



data <- MasterTable %>%filter(Super.Group == "Rhizaria") %>%count(Components)
data <- data %>%
  arrange(desc(Components))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie3 <- ggplot(data, aes(x="", y=prop, fill=Components)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(Components,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Rhizaria")+
  scale_fill_manual(name = "TOR Components",
                    breaks = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    values = TORCPal,
                    limits = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie3


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/ComponentsRhizariaPieChart.png",
       plot = pie3,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/ComponentsRhizariaPieChart.pptx",
       figure = pie3,
       units = "inches",
       width = 10,
       height = 7)

data <- MasterTable %>%filter(Super.Group == "Metamonada") %>% count(Components)
data <- data %>%
  arrange(desc(Components))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie4 <- ggplot(data, aes(x="", y=prop, fill=Components)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(Components,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Metamonada")+
  scale_fill_manual(name = "TOR Components",
                    breaks = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    values = TORCPal,
                    limits = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie4


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/ComponentsMetamonadaPieChart.png",
       plot = pie4,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/ComponentsMetamonadaPieChart.pptx",
       figure = pie4,
       units = "inches",
       width = 10,
       height = 7)




data <- MasterTable %>%filter(Super.Group == "Discoba") %>% count(Components)
data <- data %>%
  arrange(desc(Components))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie5 <- ggplot(data, aes(x="", y=prop, fill=Components)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(Components,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Discoba")+
  scale_fill_manual(name = "TOR Components",
                    breaks = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    values = TORCPal,
                    limits = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie5


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/ComponentsDiscobaPieChart.png",
       plot = pie5,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/ComponentsDiscobaPieChart.pptx",
       figure = pie5,
       units = "inches",
       width = 10,
       height = 7)



data <- MasterTable %>%filter(Super.Group == "Streptophyta") %>% count(Components)
data <- data %>%
  arrange(desc(Components))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie6 <- ggplot(data, aes(x="", y=prop, fill=Components)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(Components,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Streptophyta")+
  scale_fill_manual(name = "TOR Components",
                    breaks = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    values = TORCPal,
                    limits = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie6


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/ComponentsStreptophytaPieChart.png",
       plot = pie6,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/ComponentsStreptophytaPieChart.pptx",
       figure = pie6,
       units = "inches",
       width = 10,
       height = 7)




data <- MasterTable %>%filter(Super.Group == "Chlorophyta")%>%count(Components)
data <- data %>%
  arrange(desc(Components))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie7 <- ggplot(data, aes(x="", y=prop, fill=Components)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(Components,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Chlorophyta")+
  scale_fill_manual(name = "TOR Components",
                    breaks = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    values = TORCPal,
                    limits = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie7

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/ComponentsChlorophytaPieChart.png",
       plot = pie7,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/ComponentsChlorophytaPieChart.pptx",
       figure = pie7,
       units = "inches",
       width = 10,
       height = 7)


data <- MasterTable %>%filter(Super.Group == "Rhodophyta") %>% count(Components)
data <- data %>%
  arrange(desc(Components))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie8 <- ggplot(data, aes(x="", y=prop, fill=Components)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(Components,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Rhodophyta")+
  scale_fill_manual(name = "TOR Components",
                    breaks = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    values = TORCPal,
                    limits = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie8

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/ComponentsRhodophytaPieChart.png",
       plot = pie8,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/ComponentsRhodophytaPieChart.pptx",
       figure = pie8,
       units = "inches",
       width = 10,
       height = 7)


data <- MasterTable %>% count(Components)
data <- data %>%
  arrange(desc(Components))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie9 <- ggplot(data, aes(x="", y=prop, fill=Components)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(Components,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown All Super Groups")+
  scale_fill_manual(name = "TOR Components",
                    breaks = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    values = TORCPal,
                    limits = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie9

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/ComponentsAllPieChart.png",
       plot = pie9,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/ComponentsAllPieChart.pptx",
       figure = pie9,
       units = "inches",
       width = 10,
       height = 7)



data <- MasterTable %>%filter(Super.Group == "Alveolata"|Super.Group == "Stramenopiles" | Super.Group == "Rhizaria")%>%count(Components)
data <- data %>%
  arrange(desc(Components))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie10 <- ggplot(data, aes(x="", y=prop, fill=Components)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(Components,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown SAR")+
  scale_fill_manual(name = "TOR Components",
                    breaks = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    values = TORCPal,
                    limits = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie10


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/ComponentsSARPieChart.png",
       plot = pie10,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/ComponentsSARPieChart.pptx",
       figure = pie10,
       units = "inches",
       width = 10,
       height = 7)


data <- MasterTable %>%filter(Super.Group == "Streptophyta"|Super.Group == "Chlorophyta" | Super.Group == "Rhodophyta")%>%count(Components)
data <- data %>%
  arrange(desc(Components))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie11 <- ggplot(data, aes(x="", y=prop, fill=Components)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(Components,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Archaeplastida")+
  scale_fill_manual(name = "TOR Components",
                    breaks = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    values = TORCPal,
                    limits = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie11

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/ComponentsArchaeplastidaPieChart.png",
       plot = pie11,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/ComponentsArchaeplastidaPieChart.pptx",
       figure = pie11,
       units = "inches",
       width = 10,
       height = 7)


data <- MasterTable %>%filter(Super.Group == "Discoba"|Super.Group == "Metamonada")%>%count(Components)
data <- data %>%
  arrange(desc(Components))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie12 <- ggplot(data, aes(x="", y=prop, fill=Components)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(Components,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Excavata")+
  scale_fill_manual(name = "TOR Components",
                    breaks = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    values = TORCPal,
                    limits = c("TORC1 & TORC2","TORC1 Only","TORC2 Only","No TORC"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie12

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/ComponentsExcavataPieChart.png",
       plot = pie12,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/ComponentsExcavataPieChart.pptx",
       figure = pie12,
       units = "inches",
       width = 10,
       height = 7)



