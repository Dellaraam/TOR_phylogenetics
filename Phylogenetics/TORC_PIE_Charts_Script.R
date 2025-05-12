




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

#Currently working here. Finish up the conditional statements
#Need to do the RAPTOR one, the Both one, and the None(?)


MasterTable <- MasterTable %>%mutate(Components = NA) %>% mutate(Components = if_else(!is.na(RICTOR) & is.na(RAPTOR),"TORC2 Only",Components, missing = Components),
                                                                                                    Components = if_else(is.na(RICTOR) & !is.na(RAPTOR), "TORC1 Only", Components, missing = Components),
                                                                                                    Components = if_else(!is.na(RICTOR) & !is.na(RAPTOR), "TORC1 & TORC2", Components, missing = Components),
                                                                                                    Components = if_else(is.na(RICTOR) & is.na(RAPTOR), "No TORC", Components, missing = Components))



data <- MasterTable %>%filter(Super.Group == "Alveolata") %>% count(Components)
data <- data %>%
  arrange(desc(Components))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie1 <- ggplot(data, aes(x="", y=prop, fill=Components)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(Components,"\n",n)), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Alveolata")+
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


data <- MasterTable %>%filter(Super.Group == "Stramenopiles") %>% count(Components)
data <- data %>%
  arrange(desc(Components))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie2 <- ggplot(data, aes(x="", y=prop, fill=Components)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(Components,"\n",n)), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Stramenopiles")+
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



data <- MasterTable %>%filter(Super.Group == "Rhizaria") %>% count(Components)
data <- data %>%
  arrange(desc(Components))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie3 <- ggplot(data, aes(x="", y=prop, fill=Components)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(Components,"\n",n)), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Rhizaria")+
  theme(text = element_text(family = "serif"))
pie3


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/ComponentsAllPieChart.png",
       plot = pie3,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/ComponentsAllPieChart.pptx",
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
  geom_text(aes(y = ypos, label = paste(Components,"\n",n)), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Metamonada")+
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
  geom_text(aes(y = ypos, label = paste(Components,"\n",n)), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Discoba")+
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
  geom_text(aes(y = ypos, label = paste(Components,"\n",n)), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Streptophyta")+
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




data <- MasterTable %>%filter(Super.Group == "Chlorophyta") %>% count(Components)
data <- data %>%
  arrange(desc(Components))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie7 <- ggplot(data, aes(x="", y=prop, fill=Components)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(Components,"\n",n)), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Chlorophyta")+
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
  geom_text(aes(y = ypos, label = paste(Components,"\n",n)), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown Rhodophyta")+
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
  geom_text(aes(y = ypos, label = paste(Components,"\n",n)), color = "black", size=6)+
  labs(title = "TOR Complexes Breakdown All Super Groups")+
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



