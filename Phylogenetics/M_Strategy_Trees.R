# Photosynthesis trees and overall metabolism
# Kyle Johnson
# 4/3/2025

# Load in the master table

MasterTable <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Master_Table.csv")


MasterTable %>% ggplot(aes(x = factor(RICTOR,level = c("H", "M", "L")), y = RICTORDomain, color = M.Strategy, size = C.score))+
  geom_jitter(shape = 18)+
  labs(title = "RICTOR Domain Score Distribution")+
  xlab(label = "RICTOR H/M/L")+
  ylab(label = "RICTOR Domain Scores")+
  #geom_text_repel(aes(label = Organism.Name), size = 2.2, show.legend = FALSE)+
  facet_wrap(~Super.Group,nrow = 1)














#Stramenopile Tree -------------------------------------------------------------
StramenopileTree <- read.tree(file = "~/GitHub/TOR_phylogenetics/Trees/StramenopileTreeP.phy")
StramenopileTree$tip.label <- gsub("'","", StramenopileTree$tip.label)


Stramenopiles <- MasterTable %>% filter(Super.Group == "Stramenopiles")
Stramenopiles <- Stramenopiles %>% relocate(Organism.Name)



subsetdataframe1 <- Stramenopiles%>% select(Organism.Name, SIN1, RICTOR, RAPTOR, LST8,TOR) %>% distinct(Organism.Name, .keep_all = TRUE)
df1 <- column_to_rownames(subsetdataframe1, var = "Organism.Name")


HeatTreeStramenopile <- StramenopileTree %>% ggtree(branch.length = "none", ladderize = FALSE)+geom_tiplab(size = 2)+xlim(NA,30)+geom_text(aes(label = node))+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 1)

HeatTreeStramenopile
StramHeatPlot <- gheatmap(HeatTreeStramenopile,df1, offset = 5, width = .8, font.size = 2, colnames = FALSE)
StramHeatPlot



#Alveolata ---------------------------------------------------------------------

AlveolataTree <- read.tree(file = "~/Github/TOR_phylogenetics/Trees/AlveolataTreeP.phy")
AlveolataTree$tip.label <- gsub("'","", AlveolataTree$tip.label)

Alveolata <- MasterTable %>% filter(Super.Group == "Alveolata")
Alveolata <- Alveolata %>% relocate(Organism.Name)

subsetdataframe2 <- Alveolata %>% select(Organism.Name, 
                                         SIN1, 
                                         RICTOR, 
                                         RAPTOR, 
                                         LST8,
                                         TOR) %>% 
  distinct(Organism.Name, .keep_all = TRUE)

df2 <- column_to_rownames(subsetdataframe2, var = "Organism.Name")


AlvP <- ggtree(AlveolataTree, branch.length = "none", ladderize = FALSE)
AlvP <- AlvP  %<+% Alveolata
AlvP

HeatTreeAlv <- AlveolataTree %>% ggtree(branch.length = "none", ladderize = FALSE)+geom_tiplab(size = 2)+xlim(NA,40)+geom_text(aes(label = node))+
  geom_nodelab(nudge_y = 1, nudge_x = -.5, size = 1)+geom_rootedge()
HeatTreeAlv
AlvHeatPlot <- gheatmap(HeatTreeAlv,df2, offset = 5, width = .8, font.size = 2, colnames = FALSE)
AlvHeatPlot

