#Script for generating plots for Raw Data Analysis
#Focusing on Stramenopiles 4/17/2025


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
FinalStramRictor %>% ggplot(aes(x = sca, y = scd, color = Group.name))+
  geom_jitter()+
  theme_minimal()+
  xlab(label = "Overall HMMER Score")+
  ylab(label = "Best Domain Hit Score")+
  labs(color = "Group")


StramRaptorNCBI <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/RAPTORStramenopiles.csv")
StramRaptorNCBI <- merge(StramRaptorNCBI, StramInformationNCBI[c("Organism_Taxonomic_ID","Accn", "Organism.Name")], by = "Accn")
FinalStramRaptor <- rbind(StramRaptorNCBI,StramRaptorJGI)
FinalStramRaptor <- merge(FinalStramRaptor, TaxonomicInformation, by = "Organism_Taxonomic_ID")
FinalStramRaptor %>% ggplot(aes(x = Group.name, y = sca))+
  geom_jitter()










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
FinalAlvRictor %>% ggplot(aes(x = Group.name, y = sca, color = scd))+
  geom_jitter()+
  theme_minimal()+
  xlab(label = "Group")+
  ylab(label = "Overall HMMER Score")+
  labs(color = "Best Domain Hit")








