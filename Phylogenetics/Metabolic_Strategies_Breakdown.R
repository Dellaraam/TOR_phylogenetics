MasterTable <- read_csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Master_Table.csv")
MasterTable <- MasterTable %>% select(-...1)
MasterTable <- MasterTable %>%
  mutate(SIN1Domain = if_else(Super.Group == "Chlorophyta",NA,SIN1Domain),
         SIN1All = if_else(Super.Group == "Chlorophyta", NA, SIN1All),
         RICTORDomain = if_else(Super.Group == "Chlorophyta", NA, RICTORDomain),
         RICTORAll = if_else(Super.Group == "Chlorophyta", NA, RICTORAll))



MixPossesRictor <- MasterTable %>% filter(!is.na(RICTOR))%>%filter(M.Strategy == "Mixotroph")
MixNoRictor <- MasterTable %>% filter(is.na(RICTOR))%>%filter(M.Strategy == "Mixotroph")



write.table(MixPossesRictor[c("Organism.Name","Super.Group")], file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Metabolic_Information_Species/MixotrophicWRictor.txt", sep = "\t", row.names = F, col.names = T, quote=FALSE)
write.table(MixNoRictor[c("Organism.Name","Super.Group")], file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Metabolic_Information_Species/MixotrophicWORictor.txt", sep = "\t", row.names = F, col.names = T, quote=FALSE)




AutoPosessRictor <- MasterTable %>% filter(!is.na(RICTOR))%>%filter(M.Strategy == "Autotrophic")
AutoNoRictor <- MasterTable %>% filter(is.na(RICTOR))%>%filter(M.Strategy == "Autotrophic")

write.table(AutoPosessRictor[c("Organism.Name","Super.Group")], file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Metabolic_Information_Species/AutotrophicWRictor.txt", sep = "\t", row.names = F, col.names = T, quote=FALSE)
write.table(AutoNoRictor[c("Organism.Name","Super.Group")], file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Metabolic_Information_Species/AutotrophicWORictor.txt", sep = "\t", row.names = F, col.names = T, quote=FALSE)


ParasitePosessRictor <- MasterTable %>% filter(!is.na(RICTOR))%>%filter(M.Strategy == "Parasite")
ParasiteNoRictor <- MasterTable %>% filter(is.na(RICTOR))%>%filter(M.Strategy == "Parasite")

write.table(ParasitePosessRictor[c("Organism.Name","Super.Group")], file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Metabolic_Information_Species/ParasiteWRictor.txt", sep = "\t", row.names = F, col.names = T, quote=FALSE)
write.table(ParasiteNoRictor[c("Organism.Name","Super.Group")], file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Metabolic_Information_Species/ParasiteWORictor.txt", sep = "\t", row.names = F, col.names = T, quote=FALSE)



EndoPosessRictor <- MasterTable %>% filter(!is.na(RICTOR))%>%filter(M.Strategy == "Endosymbiotic")
EndoNoRictor <- MasterTable %>% filter(is.na(RICTOR))%>%filter(M.Strategy == "Endosymbiotic")

write.table(EndoPosessRictor[c("Organism.Name","Super.Group")], file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Metabolic_Information_Species/EndosymbioticWRictor.txt", sep = "\t", row.names = F, col.names = T, quote=FALSE)
write.table(EndoNoRictor[c("Organism.Name","Super.Group")], file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Metabolic_Information_Species/EndosymbioticWORictor.txt", sep = "\t", row.names = F, col.names = T, quote=FALSE)



HetPosessRictor <- MasterTable %>% filter(!is.na(RICTOR))%>%filter(M.Strategy == "Heterotroph")
HetNoRictor <- MasterTable %>% filter(is.na(RICTOR))%>%filter(M.Strategy == "Heterotroph")

write.table(HetPosessRictor[c("Organism.Name","Super.Group")], file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Metabolic_Information_Species/HeterotrophicWRictor.txt", sep = "\t", row.names = F, col.names = T, quote=FALSE)
write.table(HetNoRictor[c("Organism.Name","Super.Group")], file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Metabolic_Information_Species/HeterotrophicWORictor.txt", sep = "\t", row.names = F, col.names = T, quote=FALSE)





