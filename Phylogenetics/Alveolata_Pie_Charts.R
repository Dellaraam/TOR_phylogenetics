# Distributions of Super Groups






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


MasterTable %>% filter(M.Strategy == "Parasite")%>% view()
MasterTable %>% filter(is.na(RAPTOR) & M.Strategy != "Plastid Parasite") %>% view()
MasterTable %>% filter(is.na(RICTOR) & M.Strategy == "Heterotroph") %>% view()
MasterTable %>% filter(is.na(RAPTOR) & M.Strategy == "Non-Plastid Parasite")%>%view()


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


testdata <- MasterTable %>% filter(Super.Group == "Alveolata")%>%mutate(Components = NA) %>% mutate(Components = if_else(!is.na(RICTOR) & is.na(RAPTOR),"TORC2 Only",Components, missing = Components),
                                                          Components = if_else(is.na(RICTOR) & !is.na(RAPTOR), "TORC1 Only", Components, missing = Components),
                                                          Components = if_else(!is.na(RICTOR) & !is.na(RAPTOR), "TORC1 & TORC2", Components, missing = Components),
                                                          Components = if_else(is.na(RICTOR) & is.na(RAPTOR), "No TORC", Components, missing = Components))







Overall <- MasterTable %>% filter(Super.Group == "Alveolata")%>%count(M.Strategy)
Overall <- Overall %>% rename(total = "n")

#Number of species that do have RICTOR
data <- MasterTable %>%filter(Super.Group == "Alveolata") %>% filter(!is.na(RICTOR)) %>% count(M.Strategy)
data <- left_join(data, Overall)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie1 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",n,"/",total)), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown Alveolata Rictor")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie1

#Number of species that do not have RICTOR
data <- MasterTable %>%filter(Super.Group == "Alveolata") %>% filter(is.na(RICTOR)) %>% count(M.Strategy)
data <- left_join(data, Overall)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie2 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",n,"/",total)), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown Alveolata No Rictor")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie2

#Number of species that have both RICTOR and RAPTOR
data <- MasterTable %>%filter(Super.Group == "Alveolata") %>% filter(!is.na(RICTOR) & !is.na(RAPTOR)) %>% count(M.Strategy)
data <- left_join(data, Overall)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)
pie3 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",n,"/",total)), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown Alveolata Rictor & Raptor")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie3






#Number of species missing both RICTOR and RAPTOR
data <- MasterTable %>%filter(Super.Group == "Alveolata") %>% filter(is.na(RICTOR) & is.na(RAPTOR)) %>% count(M.Strategy)
data <- left_join(data, Overall)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie4 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",n,"/",total)), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown Alveolata No Rictor/Raptor")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie4


data <- MasterTable %>%filter(Super.Group == "Alveolata") %>% filter(!is.na(TOR)) %>% count(M.Strategy)
data <- left_join(data, Overall)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie5 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",n,"/",total)), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown Alveolata TOR")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie5

data <- MasterTable %>%filter(Super.Group == "Alveolata") %>% filter(is.na(TOR)) %>% count(M.Strategy)
data <- left_join(data, Overall)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie6 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",n,"/",total)), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown Alveolata TOR")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie6

data <- MasterTable %>%filter(Super.Group == "Alveolata") %>% filter(!is.na(RAPTOR)) %>% count(M.Strategy)
data <- left_join(data, Overall)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie7 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",n,"/",total)), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown Alveolata TOR")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie7
