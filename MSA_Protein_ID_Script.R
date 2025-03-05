#MSA Protein Identification Script
#Kyle Johnson
# 3/4/2025


# Going to read in the csvs for the Raw Data
# Then we are going to filter the data the exact same way that was used for the Complete Tables

library(tidyverse)
library(jsonlite)
library(purrr)


#Might make some function calls here
mergeClean <- function(df, namedf){
  df <- merge(df, namedf[c("Organism_Taxonomic_ID", "Organism.Name", "Accn")], by = "Accn")
  df <- df %>% filter(sca >= 100 & scd >= 100)%>%
    group_by(Organism_Taxonomic_ID) %>%
    slice_max(scd) %>%
    slice_max(evd)%>%
    ungroup
  return(df)
}





#Start with RICTOR
# Remember this is only for NCBI. Have to do something special for the JGI version
# Will Work on that tomorrow

Taxon <- read.csv(file = "~/GitHub/TOR_phylogenetics/Combined_Taxonomy.csv")
Taxon <- rename(Taxon, Organism.Name = "Tax.name")

AlveolataNames <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Alveolata_Information_2.csv")
AlveolataNames <- rename(AlveolataNames, Accn = "Assembly.Accession")
AlveolataRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/RICTORAlveolata.csv")

StramenopileNames <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Stramenopiles_Information_2.csv")
StramenopileNames <- rename(StramenopileNames, Accn = "Assembly.Accession")
StramenopilesRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/RICTORStramenopiles.csv" )
StramenopilesRictor <- mutate(StramenopilesRictor, Source = "NCBI")
Temp <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/HRictor.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
HeterokontsRictor <- Temp %>% select(tar, tid, qry, qid, eva, sca, bia, evd, scd, bid, Accn, Organism_Taxonomic_ID, Organism.Name, Source)

StramenopilesRaptor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/RAPTORStramenopiles.csv")
StramenopilesRaptor <- mutate(StramenopilesRaptor, Source = "NCBI")
Temp <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/HRaptor.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
HeterokontsRaptor <- Temp %>% select(tar, tid, qry, qid, eva, sca, bia, evd, scd, bid, Accn, Organism_Taxonomic_ID, Organism.Name, Source)

StramenopilesSIN1 <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/SIN1Stramenopiles.csv")
StramenopilesSIN1 <- mutate(StramenopilesSIN1, Source = "NCBI")
Temp <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/HSIN1.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
HeterokontsSIN1 <- Temp %>% select(tar, tid, qry, qid, eva, sca, bia, evd, scd, bid, Accn, Organism_Taxonomic_ID, Organism.Name, Source)

StramenopilesLST8 <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/LST8Stramenopiles.csv")
StramenopilesLST8 <- mutate(StramenopilesLST8, Source = "NCBI")
Temp <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/HLST8.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
HeterokontsLST8 <- Temp %>% select(tar, tid, qry, qid, eva, sca, bia, evd, scd, bid, Accn, Organism_Taxonomic_ID, Organism.Name, Source)

StramenopilesTOR <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/TORStramenopiles.csv")
StramenopilesTOR <- mutate(StramenopilesTOR, Source = "NCBI")
Temp <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/HTOR.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
HeterokontsTOR <- Temp %>% select(tar, tid, qry, qid, eva, sca, bia, evd, scd, bid, Accn, Organism_Taxonomic_ID, Organism.Name, Source)

RhizariaNames <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_Information_2.csv")
RhizariaNames <- rename(RhizariaNames, Accn = "Assembly.Accession")
RhizariaRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/RICTORRhizaria.csv")
# Will Have to add the fasta files for rhizaria manually I believe


#Do the filtering
AlveolataRictor <- merge(AlveolataRictor,AlveolataNames[c("Organism_Taxonomic_ID", "Organism.Name", "Accn")], by = "Accn")
AlveolataRictor <- AlveolataRictor %>% filter(sca >= 100 & scd >= 100)%>%
  group_by(Organism_Taxonomic_ID) %>%
  slice_max(scd) %>%
  slice_max(evd)%>%
  ungroup
AlveolataRictor <- distinct(AlveolataRictor, Organism_Taxonomic_ID, .keep_all = TRUE)
AlveolataRictor <- mutate(AlveolataRictor, Source = "NCBI")

StramenopilesRictor <- merge(StramenopilesRictor, StramenopileNames[c("Organism_Taxonomic_ID", "Organism.Name", "Accn")], by = "Accn")
StramenopilesRictor <- StramenopilesRictor %>% filter(sca >= 100 & scd >= 100)%>%
  group_by(Organism_Taxonomic_ID) %>%
  slice_max(scd) %>%
  slice_max(evd)%>%
  ungroup
#Extra step here due to the JGI data that has to be added in
StramenopilesRictor <- rbind(StramenopilesRictor,HeterokontsRictor)
StramenopilesRictor <- distinct(StramenopilesRictor, Organism_Taxonomic_ID, .keep_all = TRUE)

StramenopilesRaptor <- mergeClean(StramenopilesRaptor,StramenopileNames)
StramenopilesRaptor <- rbind(StramenopilesRaptor,HeterokontsRaptor)
StramenopilesRaptor <- distinct(StramenopilesRaptor, Organism_Taxonomic_ID, .keep_all = TRUE)

StramenopilesSIN1 <- mergeClean(StramenopilesSIN1,StramenopileNames)
StramenopilesSIN1 <- rbind(StramenopilesSIN1,HeterokontsSIN1)
StramenopilesSIN1 <- distinct(StramenopilesSIN1,Organism_Taxonomic_ID, .keep_all = TRUE)

StramenopilesLST8 <- mergeClean(StramenopilesLST8, StramenopileNames)
StramenopilesLST8 <- rbind(StramenopilesLST8,HeterokontsLST8)
StramenopilesLST8 <- distinct(StramenopilesLST8,Organism_Taxonomic_ID, .keep_all = TRUE)

StramenopilesTOR <- mergeClean(StramenopilesTOR, StramenopileNames)
StramenopilesTOR <- rbind(StramenopilesTOR,HeterokontsTOR)
StramenopilesTOR <- distinct(StramenopilesTOR, Organism_Taxonomic_ID, .keep_all = TRUE)


RhizariaRictor <- merge(RhizariaRictor, RhizariaNames[c("Organism_Taxonomic_ID", "Organism.Name", "Accn")], by = "Accn")
RhizariaRictor <- RhizariaRictor %>% filter(sca >= 100 & scd >= 100)%>%
  group_by(Organism_Taxonomic_ID) %>%
  slice_max(scd) %>%
  slice_max(evd)%>%
  ungroup
RhizariaRictor <- distinct(RhizariaRictor, Organism_Taxonomic_ID, .keep_all = TRUE)
RhizariaRictor <- mutate(AlveolataRictor, Source = "NCBI")













#For Stramenopiles, I need the sca and scd to find the fasta files. Making subsets
sdf <- StramenopilesLST8 %>%
  select(Accn,sca,scd,Source)%>%
  subset(Source=="JGI")
sdf2 <- StramenopilesRaptor %>%
  select(Accn,sca,scd,Source)%>%
  subset(Source=="JGI")



write.csv(StramenopilesRictor, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/StramenopilesRICTORFinal.csv")
write.csv(StramenopilesRaptor, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/StramenopilesRAPTORFinal.csv")
write.csv(StramenopilesSIN1, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/StramenopilesSIN1Final.csv")
write.csv(StramenopilesLST8, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/StramenopilesLST8Final.csv")
write.csv(StramenopilesTOR, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/StramenopilesTORFinal.csv")




write.table(AlveolataRictor$tar, file = "~/GitHub/TOR_phylogenetics/IDs/AlveolataRictor.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)

subset <- subset(StramenopilesRaptor, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/StramenopileRaptorNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(StramenopilesSIN1, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/StramenopileSIN1NCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(StramenopilesLST8, Source =="NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/StramenopileLST8NCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(StramenopilesTOR, Source =="NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/StramenopileTORNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)







