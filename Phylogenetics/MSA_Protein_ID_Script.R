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

AlveolataNames <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Alveolata_Information_2.csv")
AlveolataNames <- rename(AlveolataNames, Accn = "Assembly.Accession")
AlveolataRictor <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/RICTORAlveolata.csv")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/AlvRICTOR.csv")
Temp <- rename(Temp,Organism.Name = "Organism_Name")
AlveolataJGIRictor <- Temp %>% select(tar, tid, qry, qid, eva, sca, bia, evd, scd, bid, Accn, Organism_Taxonomic_ID, Organism.Name, Source)

AlveolataRaptor <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/RAPTORAlveolata.csv")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/AlvRAPTOR.csv")
Temp <- rename(Temp,Organism.Name = "Organism_Name")
AlveolataJGIRaptor <- Temp %>% select(tar, tid, qry, qid, eva, sca, bia, evd, scd, bid, Accn, Organism_Taxonomic_ID, Organism.Name, Source)

AlveolataSIN1 <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/SIN1Alveolata.csv")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/AlvSIN1.csv")
Temp <- rename(Temp,Organism.Name = "Organism_Name")
AlveolataJGISIN1 <- Temp %>% select(tar, tid, qry, qid, eva, sca, bia, evd, scd, bid, Accn, Organism_Taxonomic_ID, Organism.Name, Source)

AlveolataLST8 <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/LST8Alveolata.csv")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/AlvRICTOR.csv")
Temp <- rename(Temp,Organism.Name = "Organism_Name")
AlveolataJGILST8 <- Temp %>% select(tar, tid, qry, qid, eva, sca, bia, evd, scd, bid, Accn, Organism_Taxonomic_ID, Organism.Name, Source)

AlveolataTOR <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/TORAlveolata.csv")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/AlvRICTOR.csv")
Temp <- rename(Temp,Organism.Name = "Organism_Name")
AlveolataJGITOR <- Temp %>% select(tar, tid, qry, qid, eva, sca, bia, evd, scd, bid, Accn, Organism_Taxonomic_ID, Organism.Name, Source)

#-------------------------------------------------------------------------------
StramenopileNames <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Stramenopiles_Information_2.csv")
StramenopileNames <- rename(StramenopileNames, Accn = "Assembly.Accession")
StramenopilesRictor <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/RICTORStramenopiles.csv" )
StramenopilesRictor <- mutate(StramenopilesRictor, Source = "NCBI")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/HRictor.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
HeterokontsRictor <- Temp %>% select(tar, tid, qry, qid, eva, sca, bia, evd, scd, bid, Accn, Organism_Taxonomic_ID, Organism.Name, Source)

StramenopilesRaptor <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/RAPTORStramenopiles.csv")
StramenopilesRaptor <- mutate(StramenopilesRaptor, Source = "NCBI")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/HRaptor.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
HeterokontsRaptor <- Temp %>% select(tar, tid, qry, qid, eva, sca, bia, evd, scd, bid, Accn, Organism_Taxonomic_ID, Organism.Name, Source)

StramenopilesSIN1 <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/SIN1Stramenopiles.csv")
StramenopilesSIN1 <- mutate(StramenopilesSIN1, Source = "NCBI")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/HSIN1.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
HeterokontsSIN1 <- Temp %>% select(tar, tid, qry, qid, eva, sca, bia, evd, scd, bid, Accn, Organism_Taxonomic_ID, Organism.Name, Source)

StramenopilesLST8 <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/LST8Stramenopiles.csv")
StramenopilesLST8 <- mutate(StramenopilesLST8, Source = "NCBI")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/HLST8.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
HeterokontsLST8 <- Temp %>% select(tar, tid, qry, qid, eva, sca, bia, evd, scd, bid, Accn, Organism_Taxonomic_ID, Organism.Name, Source)

StramenopilesTOR <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/TORStramenopiles.csv")
StramenopilesTOR <- mutate(StramenopilesTOR, Source = "NCBI")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/HTOR.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
HeterokontsTOR <- Temp %>% select(tar, tid, qry, qid, eva, sca, bia, evd, scd, bid, Accn, Organism_Taxonomic_ID, Organism.Name, Source)


#-------------------------------------------------------------------------------
RhizariaNames <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_Information_2.csv")
RhizariaNames <- rename(RhizariaNames, Accn = "Assembly.Accession")
RhizariaRictor <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/RICTORRhizaria.csv")
RhizariaRictor <- mutate(RhizariaRictor, Source = "NCBI")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/RhizRICTOR.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
RhizariaJGIRictor <- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)

RhizariaRaptor <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/RAPTORRhizaria.csv")
RhizariaRaptor <- mutate(RhizariaRaptor, Source = "NCBI")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/RhizRAPTOR.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
RhizariaJGIRaptor <- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)


RhizariaSIN1 <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/SIN1Rhizaria.csv")
RhizariaSIN1 <- mutate(RhizariaSIN1, Source = "NCBI")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/RhizSIN1.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
RhizariaJGISIN1 <- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)


RhizariaLST8 <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/LST8Rhizaria.csv")
RhizariaLST8 <- mutate(RhizariaLST8, Source = "NCBI")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/RhizLST8.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
RhizariaJGILST8 <- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)


RhizariaTOR <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/TORRhizaria.csv")
RhizariaTOR <- mutate(RhizariaTOR, Source = "NCBI")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/RhizTOR.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
RhizariaJGITOR <- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)
#-------------------------------------------------------------------------------

ChlorophytaNames <- read_tsv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/Chlorophyta_Names.tsv")
ChlorophytaNames <- rename(ChlorophytaNames, Organism.Name = "Organism Name", Accn = "Assembly Accession", Organism_Taxonomic_ID = "Organism Taxonomic ID")

ChlorophytaRictor <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/RICTOR_Chlorophyta.csv")
ChlorophytaRictor <- mutate(ChlorophytaRictor, Source = "NCBI")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/ChRICTOR.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
ChlorophytaJGIRictor <- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)

ChlorophytaRaptor <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/RAPTOR_Chlorophyta.csv")
ChlorophytaRaptor <- mutate(ChlorophytaRaptor, Source = "NCBI")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/ChRAPTOR.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
ChlorophytaJGIRaptor <- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)

ChlorophytaSIN1 <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/SIN1_Chlorophyta.csv")
ChlorophytaSIN1 <- mutate(ChlorophytaSIN1, Source = "NCBI")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/ChSIN1.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
ChlorophytaJGISIN1 <- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)


ChlorophytaLST8 <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/LST8_Chlorophyta.csv")
ChlorophytaLST8 <- mutate(ChlorophytaLST8, Source = "NCBI")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/ChLST8.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
ChlorophytaJGILST8 <- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)

ChlorophytaTOR <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/TOR_Chlorophyta.csv")
ChlorophytaTOR <- mutate(ChlorophytaTOR, Source = "NCBI")
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/ChTOR.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
ChlorophytaJGITOR<- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)

#-------------------------------------------------------------------------------
#Now we need to do something a little interesting for the Excavates.
#Since JGI didn't divide them between Discoba and Metamonada, I may need to do some manual work here
#First I will read in the Discoba and the metamonada
#I will add Super Groups to these for distinguishing marks

DiscobaNames <- read_tsv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Discoba/Discoba_Names.tsv")
DiscobaNames <- rename(DiscobaNames, Organism.Name = "Organism Name", Accn = "Assembly Accession", Organism_Taxonomic_ID = "Organism Taxonomic ID")

DiscobaRictor <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Discoba/RICTOR_Discoba.csv")
DiscobaRictor<- mutate(DiscobaRictor, Source = "NCBI", Super.Group = "Discoba")

DiscobaRaptor <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Discoba/RAPTOR_Discoba.csv")
DiscobaRaptor <- mutate(DiscobaRaptor, Source = "NCBI", Super.Group = "Discoba")

DiscobaSIN1 <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Discoba/SIN1_Discoba.csv")
DiscobaSIN1 <- mutate(DiscobaSIN1, Source = "NCBI", Super.Group = "Discoba")

DiscobaLST8 <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Discoba/LST8_Discoba.csv")
DiscobaLST8 <- mutate(DiscobaLST8, Source = "NCBI", Super.Group = "Discoba")

DiscobaTOR <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Discoba/TOR_Discoba.csv")
DiscobaTOR <- mutate(DiscobaTOR, Source = "NCBI", Super.Group = "Discoba")

MetamonadaNames <- read_tsv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/Metamonada_Names.tsv")
MetamonadaNames <- rename(MetamonadaNames, Organism.Name = "Organism Name", Accn = "Assembly Accession", Organism_Taxonomic_ID = "Organism Taxonomic ID")

MetamonadaRictor <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/RICTOR_Metamonada.csv")
MetamonadaRictor <- mutate(MetamonadaRictor, Source = "NCBI", Super.Group = "Metamonada")

MetamonadaRaptor <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/RAPTOR_Metamonada.csv")
MetamonadaRaptor <- mutate(MetamonadaRaptor, Source = "NCBI", Super.Group = "Metamonada")

MetamonadaSIN1 <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/SIN1_Metamonada.csv")
MetamonadaSIN1 <- mutate(MetamonadaSIN1, Source = "NCBI", Super.Group = "Metamonada")

MetamonadaLST8 <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/LST8_Metamonada.csv")
MetamonadaLST8 <- mutate(MetamonadaLST8, Source = "NCBI", Super.Group = "Metamonada")

MetamonadaTOR <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/TOR_Metamonada.csv")
MetamonadaTOR <- mutate(MetamonadaTOR, Source = "NCBI", Super.Group = "Metamonada")


#Other Excavates (JGI)
Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/ExRAPTOR.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
ExcavataJGIRaptor<- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)
ExcavataJGIRaptor <- mutate(ExcavataJGIRaptor, Super.Group = "Excavata")

Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/ExRICTOR.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
ExcavataJGIRictor<- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)
ExcavataJGIRictor <- mutate(ExcavataJGIRictor, Super.Group = "Excavata")

Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/ExSIN1.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
ExcavataJGISIN1<- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)
ExcavataJGISIN1 <- mutate(ExcavataJGISIN1, Super.Group = "Excavata")

Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/ExLST8.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
ExcavataJGILST8<- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)
ExcavataJGILST8 <- mutate(ExcavataJGILST8, Super.Group = "Excavata")

Temp <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Cleaned_JGI_csv/ExTOR.csv")
Temp <- rename(Temp, Organism.Name = "Organism_Name")
ExcavataJGITOR<- Temp %>% select(tar,tid,qry,qid,eva,sca,bia,evd,scd,bid,Accn,Organism_Taxonomic_ID,Organism.Name,Source)
ExcavataJGITOR <- mutate(ExcavataJGITOR, Super.Group = "Excavata")

#Rhodophyta---------------------------------------------------------------------



RhodophytaNames <- read_tsv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhodophyta/Rhodophyta_Names.tsv")
RhodophytaNames <- rename(RhodophytaNames, Organism.Name = "Organism Name", Accn = "Assembly Accession", Organism_Taxonomic_ID = "Organism Taxonomic ID")

RhodophytaRictor <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhodophyta/RICTOR_Rhodophyta.csv")
RhodophytaRictor <- mutate(RhodophytaRictor, Source = "NCBI", Super.Group = "Rhodophyta")
RhodophytaRaptor <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhodophyta/RAPTOR_Rhodophyta.csv")
RhodophytaRaptor <- mutate(RhodophytaRaptor, Source = "NCBI", Super.Group = "Rhodophyta")
RhodophytaSIN1 <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhodophyta/SIN1_Rhodophyta.csv")
RhodophytaSIN1 <- mutate(RhodophytaSIN1, Source = "NCBI", Super.Group = "Rhodophyta")
RhodophytaLST8 <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhodophyta/LST8_Rhodophyta.csv")
RhodophytaLST8 <- mutate(RhodophytaLST8, Source = "NCBI", Super.Group = "Rhodophyta")
RhodophytaTOR <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhodophyta/TOR_Rhodophyta.csv")
RhodophytaTOR <- mutate(RhodophytaTOR, Source = "NCBI", Super.Group = "Rhodophyta")

#Streptophyta ------------------------------------------------------------------


StreptophytaNames <- read_tsv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Streptophyta/Streptophyta_Names.tsv")
StreptophytaNames <- rename(StreptophytaNames, Organism.Name = "Organism Name", Accn = "Assembly Accession", Organism_Taxonomic_ID = "Organism Taxonomic ID")

StreptophytaRaptor <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Streptophyta/RAPTOR_Streptophyta.csv")
StreptophytaRaptor <- mutate(StreptophytaRaptor, Source = "NCBI", Super.Group = "Streptophyta")
StreptophytaLST8 <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Streptophyta/LST8_Streptophyta.csv")
StreptophytaLST8 <- mutate(StreptophytaLST8, Source = "NCBI", Super.Group = "Streptophyta")
StreptophytaTOR <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Streptophyta/TOR_Streptophyta.csv")
StreptophytaTOR <- mutate(StreptophytaTOR, Source = "NCBI", Super.Group = "Streptophyta")








#Do the filtering
AlveolataRictor <- mergeClean(AlveolataRictor,AlveolataNames)
AlveolataRictor <- distinct(AlveolataRictor, Organism_Taxonomic_ID, .keep_all = TRUE)
AlveolataRictor <- mutate(AlveolataRictor, Source = "NCBI")
AlveolataRictor <- rbind(AlveolataRictor, AlveolataJGIRictor)

AlveolataRaptor <- mergeClean(AlveolataRaptor, AlveolataNames)
AlveolataRaptor <- distinct(AlveolataRaptor, Organism_Taxonomic_ID, .keep_all = TRUE)
AlveolataRaptor <- mutate(AlveolataRaptor, Source = "NCBI")
AlveolataRaptor <- rbind(AlveolataRaptor, AlveolataJGIRaptor)

AlveolataSIN1 <- mergeClean(AlveolataSIN1, AlveolataNames)
AlveolataSIN1 <- distinct(AlveolataSIN1, Organism_Taxonomic_ID, .keep_all = TRUE)
AlveolataSIN1 <- mutate(AlveolataSIN1, Source = "NCBI")
AlveolataSIN1 <- rbind(AlveolataSIN1, AlveolataJGISIN1)

AlveolataLST8 <- mergeClean(AlveolataLST8, AlveolataNames)
AlveolataLST8 <- distinct(AlveolataLST8, Organism_Taxonomic_ID, .keep_all = TRUE)
AlveolataLST8 <- mutate(AlveolataLST8, Source = "NCBI")
AlveolataLST8 <- rbind(AlveolataLST8, AlveolataJGILST8)

AlveolataTOR <- mergeClean(AlveolataTOR, AlveolataNames)
AlveolataTOR <- distinct(AlveolataTOR, Organism_Taxonomic_ID, .keep_all = TRUE)
AlveolataTOR <- mutate(AlveolataTOR, Source = "NCBI")
AlveolataTOR <- rbind(AlveolataTOR, AlveolataJGITOR)


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

ChlorophytaRictor <- mergeClean(ChlorophytaRictor, ChlorophytaNames)
ChlorophytaRictor <- rbind(ChlorophytaRictor, ChlorophytaJGIRictor)
ChlorophytaRictor <- distinct(ChlorophytaRictor, Organism_Taxonomic_ID, .keep_all = TRUE)

ChlorophytaRaptor <- mergeClean(ChlorophytaRaptor, ChlorophytaNames)
ChlorophytaRaptor <- rbind(ChlorophytaRaptor, ChlorophytaJGIRaptor)
ChlorophytaRaptor <- distinct(ChlorophytaRaptor, Organism_Taxonomic_ID, .keep_all = TRUE)

ChlorophytaSIN1 <- mergeClean(ChlorophytaSIN1, ChlorophytaNames)
ChlorophytaSIN1 <- rbind(ChlorophytaSIN1, ChlorophytaJGISIN1)
ChlorophytaSIN1 <- distinct(ChlorophytaSIN1, Organism_Taxonomic_ID, .keep_all = TRUE)

ChlorophytaLST8 <- mergeClean(ChlorophytaLST8, ChlorophytaNames)
ChlorophytaLST8 <- rbind(ChlorophytaLST8, ChlorophytaJGILST8)
ChlorophytaLST8 <- distinct(ChlorophytaLST8, Organism_Taxonomic_ID, .keep_all = TRUE)

ChlorophytaTOR <- mergeClean(ChlorophytaTOR, ChlorophytaNames)
ChlorophytaTOR <- rbind(ChlorophytaTOR, ChlorophytaJGITOR)
ChlorophytaTOR <- distinct(ChlorophytaTOR, Organism_Taxonomic_ID, .keep_all = TRUE)

DiscobaRaptor <- mergeClean(DiscobaRaptor, DiscobaNames)
DiscobaRaptor <- mutate(DiscobaRaptor, Source = "NCBI")
DiscobaRictor <- mergeClean(DiscobaRictor, DiscobaNames)
DiscobaRictor <- mutate(DiscobaRictor, Source = "NCBI")
DiscobaSIN1 <- mergeClean(DiscobaSIN1, DiscobaNames)
DiscobaSIN1 <- mutate(DiscobaSIN1, Source = "NCBI")
DiscobaLST8 <- mergeClean(DiscobaLST8, DiscobaNames)
DiscobaLST8 <- mutate(DiscobaLST8, Source = "NCBI")
DiscobaTOR <- mergeClean(DiscobaTOR, DiscobaNames)
DiscobaTOR <- mutate(DiscobaTOR, Source = "NCBI")

MetamonadaRaptor <- mergeClean(MetamonadaRaptor, MetamonadaNames)
MetamonadaRaptor <- mutate(MetamonadaRaptor, Source = "NCBI")
MetamonadaRictor <- mergeClean(MetamonadaRictor, MetamonadaNames)
MetamonadaRictor <- mutate(MetamonadaRictor, Source = "NCBI")
MetamonadaSIN1 <- mergeClean(MetamonadaSIN1, MetamonadaNames)
MetamonadaSIN1 <- mutate(MetamonadaSIN1, Source = "NCBI")
MetamonadaLST8 <- mergeClean(MetamonadaLST8, MetamonadaNames)
MetamonadaLST8 <- mutate(MetamonadaLST8, Source = "NCBI")
MetamonadaTOR <- mergeClean(MetamonadaTOR, MetamonadaNames)
MetamonadaTOR <- mutate(MetamonadaTOR, Source = "NCBI")

RhodophytaRictor <- mergeClean(RhodophytaRictor, RhodophytaNames)
RhodophytaRaptor <- mergeClean(RhodophytaRaptor, RhodophytaNames)
RhodophytaLST8 <- mergeClean(RhodophytaLST8, RhodophytaNames)
RhodophytaSIN1 <- mergeClean(RhodophytaSIN1, RhodophytaNames)
RhodophytaTOR <- mergeClean(RhodophytaTOR, RhodophytaNames)

StreptophytaRaptor <- mergeClean(StreptophytaRaptor, StreptophytaNames)
StreptophytaLST8 <- mergeClean(StreptophytaLST8, StreptophytaNames)
StreptophytaTOR <- mergeClean(StreptophytaTOR, StreptophytaNames)





#now to combine the Discobas and the Metamonadas for the time being




ExcavataRaptor <- rbind(DiscobaRaptor, MetamonadaRaptor, ExcavataJGIRaptor)
ExcavataRaptor <- distinct(ExcavataRaptor, Organism_Taxonomic_ID, .keep_all = TRUE)
ExcavataRictor <- rbind(DiscobaRictor, MetamonadaRictor, ExcavataJGIRictor)
ExcavataRictor <- distinct(ExcavataRictor, Organism_Taxonomic_ID, .keep_all = TRUE)
ExcavataSIN1 <- rbind(DiscobaSIN1, MetamonadaSIN1, ExcavataJGISIN1)
ExcavataSIN1 <- distinct(ExcavataSIN1, Organism_Taxonomic_ID, .keep_all = TRUE)
ExcavataLST8 <- rbind(DiscobaLST8, MetamonadaLST8, ExcavataJGILST8)
ExcavataLST8 <- distinct(ExcavataLST8, Organism_Taxonomic_ID, .keep_all = TRUE)
ExcavataTOR <- rbind(DiscobaTOR, MetamonadaTOR, ExcavataJGITOR)
ExcavataTOR <- distinct(ExcavataTOR, Organism_Taxonomic_ID, .keep_all = TRUE)



#which(ExcavataTOR$Organism.Name == "Euglena gracilis CCAP 1224/5Z v1.0")


#eug <- which(ExcavataTOR$Organism.Name == "Euglena gracilis CCAP 1224/5Z v1.0")
#gia <- which(ExcavataTOR$Organism.Name == "Giardia intestinalis ATCC 50803")

#Makes the Excavates have a proper super group based upon current data
#ExcavataRaptor[eug, "Super.Group"] <- "Discoba"
#ExcavataRaptor[gia, "Super.Group"] <- "Metamonada"
#ExcavataRictor[eug, "Super.Group"] <- "Discoba"
#ExcavataRictor[gia, "Super.Group"] <- "Metamonada"
#ExcavataSIN1[eug, "Super.Group"] <- "Discoba"
#ExcavataSIN1[gia, "Super.Group"] <- "Metamonada"
#ExcavataLST8[eug, "Super.Group"] <- "Discoba"
#ExcavataLST8[gia, "Super.Group"] <- "Metamonada"
#ExcavataTOR[eug, "Super.Group"] <- "Discoba"
#ExcavataTOR[gia, "Super.Group"] <- "Metamonada"







#For Stramenopiles, I need the sca and scd to find the fasta files. Making subsets
sdf <- StramenopilesLST8 %>%
  select(Accn,sca,scd,Source)%>%
  subset(Source=="JGI")
sdf2 <- StramenopilesRaptor %>%
  select(Accn,sca,scd,Source)%>%
  subset(Source=="JGI")


# Finalized Version of the CSV files will go here
write.csv(StramenopilesRictor, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/StramenopilesRICTORFinal.csv")
write.csv(StramenopilesRaptor, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/StramenopilesRAPTORFinal.csv")
write.csv(StramenopilesSIN1, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/StramenopilesSIN1Final.csv")
write.csv(StramenopilesLST8, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/StramenopilesLST8Final.csv")
write.csv(StramenopilesTOR, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/StramenopilesTORFinal.csv")

write.csv(AlveolataRictor, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/AlveolataRICTORFinal.csv")
write.csv(AlveolataRaptor, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/AlveolataRAPTORFinal.csv")
write.csv(AlveolataSIN1, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/AlveolataSIN1Final.csv")
write.csv(AlveolataLST8, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/AlveolataLST8Final.csv")
write.csv(AlveolataTOR, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/AlveolataTORFinal.csv")

write.csv(RhizariaRictor, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/RhizariaRICTORFinal.csv")
write.csv(RhizariaRaptor, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/RhizariaRAPTORFinal.csv")
write.csv(RhizariaSIN1, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/RhizariaSIN1Final.csv")
write.csv(RhizariaLST8, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/RhizariaLST8Final.csv")
write.csv(RhizariaTOR, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/RhizariaTORFinal.csv")

write.csv(ChlorophytaRictor, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/ChlorophytaRICTORFinal.csv")
write.csv(ChlorophytaRaptor, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/ChlorophytaRAPTORFinal.csv")
write.csv(ChlorophytaSIN1, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/ChlorophytaSIN1Final.csv")
write.csv(ChlorophytaLST8, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/ChlorophytaLST8Final.csv")
write.csv(ChlorophytaTOR, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/ChlorophytaTORFinal.csv")




subset <- subset(StramenopilesRaptor, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/StramenopileRaptorNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(StramenopilesSIN1, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/StramenopileSIN1NCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(StramenopilesLST8, Source =="NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/StramenopileLST8NCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(StramenopilesTOR, Source =="NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/StramenopileTORNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)

subset <- subset(AlveolataRaptor, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/AlveolataRaptorNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(AlveolataRictor, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/AlveolataRictorNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(AlveolataSIN1, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/AlveolataSIN1NCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(AlveolataLST8, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/AlveolataLST8NCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(AlveolataTOR, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/AlveolataTORNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)

subset <- subset(RhizariaRaptor, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/RhizariaRaptorNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(RhizariaRictor, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/RhizariaRictorNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(RhizariaSIN1, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/RhizariaSIN1NCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(RhizariaLST8, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/RhizariaLST8NCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(RhizariaTOR, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/RhizariaTORNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)

#Creating the Discoba ID's
subset <- subset(ExcavataRaptor, Super.Group == "Discoba")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/DiscobaRaptorNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)

subset <- subset(ExcavataRictor, Super.Group == "Discoba")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/DiscobaRictorNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)

subset <- subset(ExcavataSIN1, Super.Group == "Discoba")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/DiscobaSIN1NCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)

subset <- subset(ExcavataLST8, Super.Group == "Discoba")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/DiscobaLST8NCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)

subset <- subset(ExcavataTOR, Super.Group == "Discoba")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/DiscobaTORNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)

subset <- subset(ChlorophytaRaptor, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/ChlorophytaRaptorNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(ChlorophytaRictor, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/ChlorophytaRictorNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(ChlorophytaSIN1, Source =="NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/ChlorophytaSIN1NCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(ChlorophytaLST8, Source =="NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/ChlorophytaLST8NCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(ChlorophytaTOR, Source =="NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/ChlorophytaTORNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)

subset <- subset(RhodophytaRaptor, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/RhodophytaRaptorNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(RhodophytaLST8, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/RhodophytaLST8NCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(RhodophytaTOR, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/RhodophytaTORNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)

subset <- subset(StreptophytaRaptor, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/StreptophytaRaptorNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(StreptophytaLST8, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/StreptophytaLST8NCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
subset <- subset(StreptophytaTOR, Source == "NCBI")
write.table(subset$tar, file = "~/GitHub/TOR_phylogenetics/IDs/StreptophytaTORNCBI.txt", sep = "\t", row.names = F, col.names = F, quote=FALSE)
















