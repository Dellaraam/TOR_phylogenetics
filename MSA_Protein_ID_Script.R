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
AlveolataRaptor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/RAPTORAlveolata.csv")
AlveolataSIN1 <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/SIN1Alveolata.csv")
AlveolataLST8 <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/LST8Alveolata.csv")
AlveolataTOR <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/TORAlveolata.csv")



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


#-------------------------------------------------------------------------------
RhizariaNames <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_Information_2.csv")
RhizariaNames <- rename(RhizariaNames, Accn = "Assembly.Accession")
RhizariaRictor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/RICTORRhizaria.csv")
RhizariaRictor <- mutate(RhizariaRictor, Source = "NCBI")
Temp <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/RhizRICTOR.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
RhizariaJGIRictor <- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)

RhizariaRaptor <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/RAPTORRhizaria.csv")
RhizariaRaptor <- mutate(RhizariaRaptor, Source = "NCBI")
Temp <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/RhizRAPTOR.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
RhizariaJGIRaptor <- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)


RhizariaSIN1 <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/SIN1Rhizaria.csv")
RhizariaSIN1 <- mutate(RhizariaSIN1, Source = "NCBI")
Temp <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/RhizSIN1.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
RhizariaJGISIN1 <- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)


RhizariaLST8 <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/LST8Rhizaria.csv")
RhizariaLST8 <- mutate(RhizariaLST8, Source = "NCBI")
Temp <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/RhizLST8.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
RhizariaJGILST8 <- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)


RhizariaTOR <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/TORRhizaria.csv")
RhizariaTOR <- mutate(RhizariaTOR, Source = "NCBI")
Temp <- read.csv(file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/RhizTOR.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
RhizariaJGITOR <- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)





























#Do the filtering
AlveolataRictor <- mergeClean(AlveolataRictor,AlveolataNames)
AlveolataRictor <- distinct(AlveolataRictor, Organism_Taxonomic_ID, .keep_all = TRUE)
AlveolataRictor <- mutate(AlveolataRictor, Source = "NCBI")

AlveolataRaptor <- mergeClean(AlveolataRaptor, AlveolataNames)
AlveolataRaptor <- distinct(AlveolataRaptor, Organism_Taxonomic_ID, .keep_all = TRUE)
AlveolataRaptor <- mutate(AlveolataRaptor, Source = "NCBI")

AlveolataSIN1 <- mergeClean(AlveolataSIN1, AlveolataNames)
AlveolataSIN1 <- distinct(AlveolataSIN1, Organism_Taxonomic_ID, .keep_all = TRUE)
AlveolataSIN1 <- mutate(AlveolataSIN1, Source = "NCBI")

AlveolataLST8 <- mergeClean(AlveolataLST8, AlveolataNames)
AlveolataLST8 <- distinct(AlveolataLST8, Organism_Taxonomic_ID, .keep_all = TRUE)
AlveolataLST8 <- mutate(AlveolataLST8, Source = "NCBI")

AlveolataTOR <- mergeClean(AlveolataTOR, AlveolataNames)
AlveolataTOR <- distinct(AlveolataTOR, Organism_Taxonomic_ID, .keep_all = TRUE)
AlveolataTOR <- mutate(AlveolataTOR, Source = "NCBI")


StramenopilesRictor <- mergeClean(StramenopilesRictor,StramenopileNames)
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

RhizariaRictor <- mergeClean(RhizariaRictor,RhizariaNames)
RhizariaRictor <- rbind(RhizariaRictor,RhizariaJGIRictor)
RhizariaRictor <- distinct(RhizariaRictor, Organism_Taxonomic_ID, .keep_all = TRUE)

RhizariaRaptor <- mergeClean(RhizariaRaptor,RhizariaNames)
RhizariaRaptor <- rbind(RhizariaRaptor, RhizariaJGIRaptor)
RhizariaRaptor <- distinct(RhizariaRaptor, Organism_Taxonomic_ID, .keep_all = TRUE)

RhizariaSIN1 <- mergeClean(RhizariaSIN1,RhizariaNames)
RhizariaSIN1 <- rbind(RhizariaSIN1, RhizariaJGISIN1)
RhizariaSIN1 <- distinct(RhizariaSIN1, Organism_Taxonomic_ID, .keep_all = TRUE)

RhizariaLST8 <- mergeClean(RhizariaLST8,RhizariaNames)
RhizariaLST8 <- rbind(RhizariaLST8, RhizariaJGILST8)
RhizariaLST8 <- distinct(RhizariaLST8, Organism_Taxonomic_ID, .keep_all = TRUE)

RhizariaTOR <- mergeClean(RhizariaTOR,RhizariaNames)
RhizariaTOR <- rbind(RhizariaTOR, RhizariaJGITOR)
RhizariaTOR <- distinct(RhizariaTOR, Organism_Taxonomic_ID, .keep_all = TRUE)










#For Stramenopiles, I need the sca and scd to find the fasta files. Making subsets
sdf <- StramenopilesLST8 %>%
  select(Accn,sca,scd,Source)%>%
  subset(Source=="JGI")
sdf2 <- StramenopilesRaptor %>%
  select(Accn,sca,scd,Source)%>%
  subset(Source=="JGI")


# Finalized Version of the CSV files will go here
write.csv(StramenopilesRictor, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/StramenopilesRICTORFinal.csv")
write.csv(StramenopilesRaptor, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/StramenopilesRAPTORFinal.csv")
write.csv(StramenopilesSIN1, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/StramenopilesSIN1Final.csv")
write.csv(StramenopilesLST8, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/StramenopilesLST8Final.csv")
write.csv(StramenopilesTOR, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/StramenopilesTORFinal.csv")

write.csv(AlveolataRictor, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/AlveolataRICTORFinal.csv")
write.csv(AlveolataRaptor, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/AlveolataRAPTORFinal.csv")
write.csv(AlveolataSIN1, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/AlveolataSIN1Final.csv")
write.csv(AlveolataLST8, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/AlveolataLST8Final.csv")
write.csv(AlveolataTOR, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/AlveolataTORFinal.csv")

write.csv(RhizariaRictor, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/RhizariaRICTORFinal.csv")
write.csv(RhizariaRaptor, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/RhizariaRAPTORFinal.csv")
write.csv(RhizariaSIN1, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/RhizariaSIN1Final.csv")
write.csv(RhizariaLST8, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/RhizariaLST8Final.csv")
write.csv(RhizariaTOR, file = "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/RhizariaTORFinal.csv")


subset <- subset(StramenopilesRaptor, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/StramenopileRaptorNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(StramenopilesSIN1, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/StramenopileSIN1NCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(StramenopilesLST8, Source =="NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/StramenopileLST8NCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(StramenopilesTOR, Source =="NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/StramenopileTORNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)



#IDs for Alveolata
write.table(AlveolataRictor$tar, file = "~/GitHub/TOR_phylogenetics/IDs/AlveolataRictor.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
write.table(AlveolataRaptor$tar, file = "~/GitHub/TOR_phylogenetics/IDs/AlveolataRaptor.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
write.table(AlveolataSIN1$tar, file = "~/GitHub/TOR_phylogenetics/IDs/AlveolataSIN1.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
write.table(AlveolataLST8$tar, file = "~/GitHub/TOR_phylogenetics/IDs/AlveolataLST8.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
write.table(AlveolataTOR$tar, file = "~/GitHub/TOR_phylogenetics/IDs/AlveolataTOR.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)

write.table(RhizariaRictor$tar, file = "~/GitHub/TOR_phylogenetics/IDs/RhizariaRictor.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
write.table(RhizariaRaptor$tar, file = "~/GitHub/TOR_phylogenetics/IDs/RhizariaRaptor.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
write.table(RhizariaSIN1$tar, file = "~/GitHub/TOR_phylogenetics/IDs/RhizariaSIN1.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
write.table(RhizariaLST8$tar, file = "~/GitHub/TOR_phylogenetics/IDs/RhizariaLST8.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
write.table(RhizariaTOR$tar, file = "~/GitHub/TOR_phylogenetics/IDs/RhizariaTOR.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)



