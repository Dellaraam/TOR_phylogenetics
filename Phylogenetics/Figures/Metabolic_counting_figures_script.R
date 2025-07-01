#load in metabolic table

df <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Final_Metabolic_Table.csv")

Ydf <- filter(df, HasRICTOR == "YES", .keep_all = TRUE)
Ndf <- filter(df, HasRICTOR == "NO", .keep_all = TRUE)


Ndf %>% filter(M.Strategy != "NA") %>% ggplot()+
  geom_bar(aes(x = M.Strategy, fill = Super.Group), position = "dodge")+
  labs(title = "Organisms without RICTOR")+
  theme_bw()

Ndf %>% filter(M.Strategy != "NA") %>% ggplot(aes(x = Super.Group, y = as.factor(M.Strategy)))+
  geom_jitter()+
  theme_bw()


Ydf %>% filter(M.Strategy != "NA")%>%ggplot()+
  geom_bar(aes(x = M.Strategy, fill = Super.Group), position = "dodge")+
  labs(title = "Organisms with RICTOR")+
  theme_bw()

Ydf %>% filter(M.Strategy != "NA") %>% ggplot(aes(x = Super.Group, y = as.factor(M.Strategy), color = RICTOR))+
  geom_jitter()+
  theme_bw()

Ydf %>% filter(M.Strategy != "NA") %>% ggplot(aes(x = Super.Group, y = as.factor(M.Strategy))) +
  geom_violin()

Ydf %>% ggplot(aes(x = Super.Group, y = as.factor(RICTOR), color = M.Strategy))+
  geom_jitter()

