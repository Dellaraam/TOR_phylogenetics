#Script for generating plots for Raw Data Analysis
#Focusing on Stramenopiles 4/17/2025

source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Library_Script.R")
#Stramenopile Raw Data
#Need to combine both the JGI information and the NCBI Information

#Information Section
#All information csvs will be loaded in here
TaxonomicInformation <- read.csv(file="~/GitHub/TOR_phylogenetics/Combined_Taxonomy.csv")

PhycoInformation <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Phycocosm_Information.csv")
PhycoInformation <- PhycoInformation %>% rename(Accn = "portal", Organism_Taxonomic_ID = "NCBI.Taxon")
PhycoInformation$Accn <- sub("\\_.*", "", PhycoInformation$Accn)

StramInformationNCBI <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Stramenopiles_Information_2.csv")
StramInformationNCBI <- rename(StramInformationNCBI, Accn = "Assembly.Accession")
ChloroInformationNCBI <- read_tsv(file="C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/Chlorophyta_Names.tsv")
ChloroInformationNCBI <- rename(ChloroInformationNCBI, Accn = "Assembly Accession", Organism.Name = "Organism Name", Organism_Taxonomic_ID = "Organism Taxonomic ID")
AlvInformationNCBI <- read.csv(file="C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Alveolata_Information_2.csv")
AlvInformationNCBI <- rename(AlvInformationNCBI, Accn = "Assembly.Accession")
DiscobaInformationNCBI <- read_tsv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Discoba/Discoba_Names.tsv")
DiscobaInformationNCBI <- rename(DiscobaInformationNCBI, Accn = "Assembly Accession", Organism.Name = "Organism Name", Organism_Taxonomic_ID = "Organism Taxonomic ID")
MetamonadaInformationNCBI <- read_tsv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/Metamonada_Names.tsv")
MetamonadaInformationNCBI <- rename(MetamonadaInformationNCBI, Accn = "Assembly Accession", Organism.Name = "Organism Name", Organism_Taxonomic_ID = "Organism Taxonomic ID")












StramRictorJGI <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_RICTOR.csv")
StramRictorJGI$Accn<- sub("\\_.*", "", StramRictorJGI$Accn)
StramRictorJGI <- merge(StramRictorJGI,PhycoInformation[c("Organism_Taxonomic_ID", "name", "Accn")], by="Accn")
StramRictorJGI <- rename(StramRictorJGI, Organism.Name = "name")

StramRaptorJGI <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_RAPTOR.csv")
StramRaptorJGI$Accn<- sub("\\_.*", "", StramRaptorJGI$Accn)
StramRaptorJGI <- merge(StramRaptorJGI,PhycoInformation[c("Organism_Taxonomic_ID", "name", "Accn")], by="Accn")
StramRaptorJGI <- rename(StramRaptorJGI, Organism.Name = "name")



StramRictorNCBI <- read.csv(file="C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/RICTORStramenopiles.csv")
StramRictorNCBI <- merge(StramRictorNCBI, StramInformationNCBI[c("Organism_Taxonomic_ID","Accn", "Organism.Name")], by = "Accn")
FinalStramRictor <- rbind(StramRictorNCBI,StramRictorJGI)
FinalStramRictor <- merge(FinalStramRictor, TaxonomicInformation, by = "Organism_Taxonomic_ID")
StramRictorRawPlot <- FinalStramRictor %>% ggplot()+
  geom_jitter(aes(x = sca, y = scd), size = 2)+
  geom_hline(yintercept = 100, linetype = "dashed")+
  geom_vline(xintercept = 100, linetype = "dashed")+
  geom_rect(aes(xmin = 150, xmax = Inf, ymin = 150, ymax = Inf),linetype = "dashed", color = "black", fill = "#ffffffff", alpha = 0)+
  geom_rect(aes(xmin = 300, xmax = Inf, ymin = 300, ymax = Inf),linetype = "dashed", color = "black", fill = "#ffffffff", alpha = 0)+
  annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, fill = "purple", alpha = .3)+
  annotate("rect", xmin = 100, xmax = Inf, ymin = 0, ymax = 100, fill = "red", alpha = .3)+
  annotate("rect",xmin = 0, xmax = 100, ymin = 100, ymax = Inf, fill = "red", alpha =.3)+
  
  theme_bw()+
  labs(title = "Overall Hit Score vs Best Domain Hit Score RICTOR",
       subtitle = "Stramenopiles")+
  xlab(label = "Overall HMMER Score")+
  ylab(label = "Best Domain Score")+
  labs(color = "Group")+
  theme(text = element_text(family = "Times New Roman"))


FinalStramRictor %>%distinct(Organism_Taxonomic_ID, .keep_all = TRUE)%>%ggplot()+
  geom_jitter(aes(x = sca, y = scd), size = 2)+
  geom_hline(yintercept = 100, linetype = "dashed")+
  geom_vline(xintercept = 100, linetype = "dashed")+
  geom_rect(aes(xmin = 150, xmax = Inf, ymin = 150, ymax = Inf),linetype = "dashed", color = "black", fill = "#ffffffff", alpha = 0)+
  geom_rect(aes(xmin = 300, xmax = Inf, ymin = 300, ymax = Inf),linetype = "dashed", color = "black", fill = "#ffffffff", alpha = 0)+
  annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, fill = "purple", alpha = .3)+
  annotate("rect", xmin = 100, xmax = Inf, ymin = 0, ymax = 100, fill = "red", alpha = .3)+
  annotate("rect",xmin = 0, xmax = 100, ymin = 100, ymax = Inf, fill = "red", alpha =.3)+
  
  theme_bw()+
  labs(title = "Overall Hit Score vs Best Domain Hit Score RICTOR",
       subtitle = "Stramenopiles Distinct ID Scores")+
  xlab(label = "Overall HMMER Score")+
  ylab(label = "Best Domain Score")+
  labs(color = "Group")




StramRictorRawPlot
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/CutOffDeterminationStramenopileRictor.png",
       plot = StramRictorRawPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/CutOffDeterminationStramenopileRictor.pptx",
       figure = StramRictorRawPlot,
       units = "inches",
       width = 10,
       height = 7)






StramRaptorNCBI <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/RAPTORStramenopiles.csv")
StramRaptorNCBI <- merge(StramRaptorNCBI, StramInformationNCBI[c("Organism_Taxonomic_ID","Accn", "Organism.Name")], by = "Accn")
FinalStramRaptor <- rbind(StramRaptorNCBI,StramRaptorJGI)
FinalStramRaptor <- merge(FinalStramRaptor, TaxonomicInformation, by = "Organism_Taxonomic_ID")
StramRAPTORRawPlot <- FinalStramRaptor %>% ggplot()+
  geom_jitter(aes(x = sca, y = scd), size = 2)+
  geom_hline(yintercept = 100, linetype = "dashed")+
  geom_vline(xintercept = 100, linetype = "dashed")+
  geom_rect(aes(xmin = 150, xmax = Inf, ymin = 150, ymax = Inf),linetype = "dashed", color = "black", fill = "#ffffffff", alpha = 0)+
  geom_rect(aes(xmin = 300, xmax = Inf, ymin = 300, ymax = Inf),linetype = "dashed", color = "black", fill = "#ffffffff", alpha = 0)+
  annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, fill = "purple", alpha = .3)+
  annotate("rect", xmin = 100, xmax = Inf, ymin = 0, ymax = 100, fill = "red", alpha = .3)+
  annotate("rect",xmin = 0, xmax = 100, ymin = 100, ymax = Inf, fill = "red", alpha =.3)+
  
  theme_bw()+
  labs(title = "Overall Hit Score vs Best Domain Hit Score RAPTOR",
       subtitle = "Stramenopiles")+
  xlab(label = "Overall HMMER Score")+
  ylab(label = "Best Domain Score")+
  labs(color = "Group")+
  theme(text = element_text(family = "Times New Roman"))
StramRictorRawPlot
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/CutOffDeterminationStramenopileRaptor.png",
       plot = StramRAPTORRawPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

StramRictorRawPlot
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/CutOffDeterminationStramenopileRaptor.png",
       plot = StramRAPTORRawPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)










ChloroRictorJGI <- read.csv(file="C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_RICTOR.csv")
ChloroRictorJGI$Accn<- sub("\\_.*", "", ChloroRictorJGI$Accn)
ChloroRictorJGI <- merge(ChloroRictorJGI,PhycoInformation[c("Organism_Taxonomic_ID", "name", "Accn")], by="Accn")
ChloroRictorJGI <- rename(ChloroRictorJGI, Organism.Name = "name")

ChloroRaptorJGI <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_RAPTOR.csv")
ChloroRaptorJGI$Accn<- sub("\\_.*", "", ChloroRaptorJGI$Accn)
ChloroRaptorJGI <- merge(ChloroRaptorJGI,PhycoInformation[c("Organism_Taxonomic_ID", "name", "Accn")], by="Accn")
ChloroRaptorJGI <- rename(ChloroRaptorJGI, Organism.Name = "name")





ChloroRictorNCBI <- read.csv(file="C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/RICTOR_Chlorophyta.csv")
ChloroRictorNCBI <- merge(ChloroRictorNCBI, ChloroInformationNCBI[c("Organism_Taxonomic_ID","Accn", "Organism.Name")], by = "Accn")
FinalChloroRictor <- rbind(ChloroRictorNCBI, ChloroRictorJGI)
FinalChloroRictor <- merge(FinalChloroRictor, TaxonomicInformation, by = "Organism_Taxonomic_ID")
FinalChloroRictor %>% ggplot(aes(x = Group.name, y = sca))+
  geom_jitter()+
  theme_minimal()+
  xlab(label = "Group")+
  ylab(label = "Overall HMMER Score")+
  labs(color = "Best Domain Hit")


ChloroRaptorNCBI <- read.csv(file="C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/RAPTOR_Chlorophyta.csv")
ChloroRaptorNCBI <- merge(ChloroRaptorNCBI, ChloroInformationNCBI[c("Organism_Taxonomic_ID","Accn", "Organism.Name")], by = "Accn")
FinalChloroRaptor <- rbind(ChloroRaptorNCBI, ChloroRaptorJGI)
FinalChloroRaptor <- merge(FinalChloroRaptor, TaxonomicInformation, by = "Organism_Taxonomic_ID")
FinalChloroRaptor %>% ggplot(aes(x = Group.name, y = sca))+
  geom_jitter()+
  theme_minimal()














AlvRictorJGI <- read.csv(file="C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Alveolata_JGI/AlveolataJGI_RICTOR.csv")
AlvRictorJGI$Accn<- sub("\\_.*", "", AlvRictorJGI$Accn)
AlvRictorJGI <- merge(AlvRictorJGI,PhycoInformation[c("Organism_Taxonomic_ID", "name", "Accn")], by="Accn")
AlvRictorJGI <- rename(AlvRictorJGI, Organism.Name = "name")


AlvRictorNCBI <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/RICTORAlveolata.csv")
AlvRictorNCBI <- merge(AlvRictorNCBI, AlvInformationNCBI[c("Organism_Taxonomic_ID","Accn", "Organism.Name")], by = "Accn")
FinalAlvRictor <- rbind(AlvRictorNCBI, AlvRictorJGI)
FinalAlvRictor <-  merge(FinalAlvRictor, TaxonomicInformation, by = "Organism_Taxonomic_ID")




FinalAlvPlot <- FinalAlvRictor %>% ggplot()+
  geom_jitter(aes(x = sca, y = scd), size = 2)+
  geom_hline(yintercept = 100, linetype = "dashed")+
  geom_vline(xintercept = 100, linetype = "dashed")+
  geom_rect(aes(xmin = 150, xmax = Inf, ymin = 150, ymax = Inf),linetype = "dashed", color = "black", fill = "#ffffffff", alpha = 0)+
  geom_rect(aes(xmin = 300, xmax = Inf, ymin = 300, ymax = Inf),linetype = "dashed", color = "black", fill = "#ffffffff", alpha = 0)+
  annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, fill = "purple", alpha = .3)+
  annotate("rect", xmin = 100, xmax = Inf, ymin = 0, ymax = 100, fill = "red", alpha = .3)+
  annotate("rect",xmin = 0, xmax = 100, ymin = 100, ymax = Inf, fill = "red", alpha =.3)+
  
  theme_bw()+
  labs(title = "Overall Hit Score vs Best Domain Hit Score RAPTOR",
       subtitle = "Alveolata Raw Hits")+
  xlab(label = "Overall HMMER Score")+
  ylab(label = "Best Domain Score")+
  labs(color = "Group")+
  theme(text = element_text(family = "Times New Roman"))


FinalAlvPlotDist <- FinalAlvRictor %>% distinct(Organism_Taxonomic_ID, .keep_all = TRUE)%>%ggplot()+
  geom_jitter(aes(x = sca, y = scd), size = 2)+
  geom_hline(yintercept = 100, linetype = "dashed")+
  geom_vline(xintercept = 100, linetype = "dashed")+
  geom_rect(aes(xmin = 150, xmax = Inf, ymin = 150, ymax = Inf),linetype = "dashed", color = "black", fill = "#ffffffff", alpha = 0)+
  geom_rect(aes(xmin = 300, xmax = Inf, ymin = 300, ymax = Inf),linetype = "dashed", color = "black", fill = "#ffffffff", alpha = 0)+
  annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, fill = "purple", alpha = .3)+
  annotate("rect", xmin = 100, xmax = Inf, ymin = 0, ymax = 100, fill = "red", alpha = .3)+
  annotate("rect",xmin = 0, xmax = 100, ymin = 100, ymax = Inf, fill = "red", alpha =.3)+
  
  theme_bw()+
  labs(title = "Overall Hit Score vs Best Domain Hit Score RAPTOR",
       subtitle = "Alveolata Raw Hits Distinct ID")+
  xlab(label = "Overall HMMER Score")+
  ylab(label = "Best Domain Score")+
  labs(color = "Group")+
  theme(text = element_text(family = "Times New Roman"))











ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/CutOffDeterminationAlvRictor.png",
       plot = FinalAlvPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)



ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/CutOffDeterminationAlvRictorDistinct.png",
       plot = FinalAlvPlotDist,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)


ExcRictorJGI <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Excavata_JGI/ExcavataJGI_RICTOR.csv")
ExcRictorJGI$Accn <- sub("\\_.*", "", ExcRictorJGI$Accn)
ExcRictorJGI <- merge(ExcRictorJGI,PhycoInformation[c("Organism_Taxonomic_ID", "name", "Accn")], by="Accn")
ExcRictorJGI <- rename(ExcRictorJGI, Organism.Name = "name")

DiscobaRictorNCBI <- read.csv(file = 'C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Discoba/RICTOR_Discoba.csv')
DiscobaRictorNCBI <- merge(DiscobaRictorNCBI, DiscobaInformationNCBI[c("Organism_Taxonomic_ID","Accn", "Organism.Name")], by = "Accn")
MetamonadaRictorNCBI <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/RICTOR_Metamonada.csv")
MetamonadaRictorNCBI <- merge(MetamonadaRictorNCBI, MetamonadaInformationNCBI[c("Organism_Taxonomic_ID","Accn", "Organism.Name")], by = "Accn")


FinalExcRictor <- rbind(DiscobaRictorNCBI, MetamonadaRictorNCBI, ExcRictorJGI)
FinalExcRictor <-  merge(FinalExcRictor, TaxonomicInformation, by = "Organism_Taxonomic_ID")

ExcRictorPlot <- FinalExcRictor %>% ggplot()+
  geom_jitter(aes(x = sca, y = scd), size = 2)+
  geom_hline(yintercept = 100, linetype = "dashed")+
  geom_vline(xintercept = 100, linetype = "dashed")+
  geom_rect(aes(xmin = 150, xmax = Inf, ymin = 150, ymax = Inf),linetype = "dashed", color = "black", fill = "#ffffffff", alpha = 0)+
  geom_rect(aes(xmin = 300, xmax = Inf, ymin = 300, ymax = Inf),linetype = "dashed", color = "black", fill = "#ffffffff", alpha = 0)+
  annotate("rect", xmin = 0, xmax = 100, ymin = 0, ymax = 100, fill = "purple", alpha = .3)+
  annotate("rect", xmin = 100, xmax = Inf, ymin = 0, ymax = 100, fill = "red", alpha = .3)+
  annotate("rect",xmin = 0, xmax = 100, ymin = 100, ymax = Inf, fill = "red", alpha =.3)+
  
  theme_bw()+
  labs(title = "Overall Hit Score vs Best Domain Hit Score Rictor",
       subtitle = "Excavata Raw Hits")+
  xlab(label = "Overall HMMER Score")+
  ylab(label = "Best Domain Score")+
  labs(color = "Group")+
  theme(text = element_text(family = "Times New Roman"))
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/CutOffDeterminationExcRictor.png",
       plot = ExcRictorPlot,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)







