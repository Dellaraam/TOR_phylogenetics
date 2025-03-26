# Script for combining all of the numeric scores and outputting to file
# Kyle Johnson
# 2/19/2025

#First we read in the Data for RICTOR, RAPTOR, LST8, SIN1, TOR
## We will do this for each of the clades we are interested in
## We will also include the JGI dataset as well at a later point


source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Phylogenetic_Heterokonta.R")
library(tidyverse)

JoinInfo <- function(RawData, cladeInfo, GroupInfo ,TaxonomicInformation, sourceInfo){
  
  #view(RawData)
  #view(GroupInfo)
  print(GroupInfo)
  RawData <- merge(RawData, GroupInfo[c("Organism_Taxonomic_ID", "Organism.Name", "Accn")], by="Accn")
  cleanedData <- RawData %>% filter(sca >= 100 & scd >= 100) %>%
    group_by(Organism_Taxonomic_ID) %>%
    slice_max(scd) %>%
    slice_max(evd)%>%
    ungroup
  cleanedData <- cleanedData %>% left_join(select(TaxonomicInformation, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
  cleanedData <- cleanedData %>% mutate(Super.Group = cladeInfo) %>% mutate(Source = sourceInfo)
  cleanedData <- cleanedData %>% relocate(Super.Group)%>% rename(Organism_Name = "Organism.Name")
  
  return(cleanedData)
  
}

ModCombine <- function(Grouping, Namedf, RICTORdf, SIN1df, RAPTORdf, LST8df, TORdf){
  
  
  # Make the empty dataframe
  returnDf <- data.frame()
  
  # Add in the taxid and species names columns to the dataframe
  returnDF <- subset(Namedf, select = c("Accn","Organism_Taxonomic_ID", "Organism.Name"))
  
  # Now create empty columns and rows that will be filled in later in the function
  # These will be the "checking" sections for each of the specified proteins
  
  returnDF <- returnDF %>% add_column(SIN1All = NA,
                                      SIN1Domain = NA,
                                      RICTORAll = NA,
                                      RICTORDomain = NA,
                                      RAPTORAll = NA,
                                      RAPTORDomain = NA,
                                      TORAll = NA,
                                      TORDomain = NA,
                                      LST8All = NA,
                                      LST8Domain = NA)
  returnDF <- returnDF %>% add_column(Group = Grouping, .before = "Accn")
  
  #Check the input dataframes to see if they are of length 0. If they are, do something about it
  
  
  if(nrow(RICTORdf) != 0){# First loop to check for RICTOR
    for (n in 1:nrow(returnDF)){
      for (m in 1:nrow(RICTORdf)){
        #condition check to see if the taxIDs line up
        if(returnDF[n, "Organism_Taxonomic_ID"] == RICTORdf[m, "Organism_Taxonomic_ID"]){
            returnDF$RICTORDomain[n] <- RICTORdf$scd[m]
            returnDF$RICTORAll[n] <- RICTORdf$sca[m]
        } else{
          next
        }
      }
    }
    print("Finished RICTOR")
  }else{}
  #Second Loop for SIN1
  if(nrow(SIN1df) != 0){
    for (n in 1:nrow(returnDF)){
      for (m in 1:nrow(SIN1df)){
        #condition check to see if the taxIDs line up
        if(returnDF[n, "Organism_Taxonomic_ID"] == SIN1df[m, "Organism_Taxonomic_ID"]){
            returnDF$SIN1Domain[n] <- SIN1df$scd[m]
            returnDF$SIN1All[n] <- SIN1df$sca[m]
        }else{
          next
        }
      }
    }
    print("Finished SIN1")
  }else{}
  
  #Third Loop for RAPTOR
  if(nrow(RAPTORdf) != 0){
    for (n in 1:nrow(returnDF)){
      for (m in 1:nrow(RAPTORdf)){
        #condition check to see if the taxIDs line up
        if(returnDF[n, "Organism_Taxonomic_ID"] == RAPTORdf[m, "Organism_Taxonomic_ID"]){
            returnDF$RAPTORDomain[n] <- RAPTORdf$scd[m]
            returnDF$RAPTORAll[n] <- RAPTORdf$sca[m]
        }else{
          next
        }
      }
    }
    print("Finished RAPTOR")
  }else{}
  
  
  #Fourth Loop for LST8
  if(nrow(LST8df) != 0){
    for (n in 1:nrow(returnDF)){
      for (m in 1:nrow(LST8df)){
        #condition check to see if the taxIDs line up
        if(returnDF[n,"Organism_Taxonomic_ID"] == LST8df[m, "Organism_Taxonomic_ID"]){
            returnDF$LST8Domain[n] <- LST8df$scd[m]
            returnDF$LST8All[n] <- LST8df$sca[m]
        } else{
          next
        }
      }
    }
    print("Finished LST8")
  }else{}
  
  #Fourth Loop for TOR
  if(nrow(TORdf) != 0){
    for (n in 1:nrow(returnDF)){
      for (m in 1:nrow(TORdf)){
        #condition check to see if the taxIDs line up
        if(returnDF[n, "Organism_Taxonomic_ID"] == TORdf[m, "Organism_Taxonomic_ID"]){
            returnDF$TORDomain[n] <- TORdf$scd[m]
            returnDF$TORAll[n] <- TORdf$sca[m]
        } else{
          next
        }
      }
      
    }
  }else{}
  # Finally return the completed dataframe   
  return(returnDF)
  
  
}
#-------------------------------------------------------------------------------








Taxon <- read.csv("~/GitHub/TOR_phylogenetics/Combined_Taxonomy.csv")
#-------------------------------------------------------------------------------
AlveolataInfo <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Alveolata_Information_2.csv")
AlveolataInfo <- AlveolataInfo %>% rename(Accn = "Assembly.Accession")

RICTORAlveolata <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/RICTORAlveolata.csv")
RICTORAlveolata <- JoinInfo(RICTORAlveolata, "Alveolata",AlveolataInfo,Taxon,"NCBI")

RAPTORAlveolata <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/RAPTORAlveolata.csv")
RAPTORAlveolata <- JoinInfo(RAPTORAlveolata, "Alveolata", AlveolataInfo, Taxon, "NCBI")

SIN1Alveolata <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/SIN1Alveolata.csv")
SIN1Alveolata <- JoinInfo(SIN1Alveolata, "Alveolata", AlveolataInfo, Taxon, "NCBI")

LST8Alveolata <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/LST8Alveolata.csv")
LST8Alveolata <- JoinInfo(LST8Alveolata,"Alveolata",AlveolataInfo, Taxon, "NCBI")

TORAlveolata <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/TORAlveolata.csv")
TORAlveolata <- JoinInfo(TORAlveolata,"Alveolata",AlveolataInfo,Taxon,"NCBI")

Alveolata <- ModCombine("Alveolata",AlveolataInfo,RICTORAlveolata,SIN1Alveolata,RAPTORAlveolata,LST8Alveolata,TORAlveolata)
Alveolata <- Alveolata %>% mutate(source = "NCBI") %>% left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))


#-------------------------------------------------------------------------------

AlveolataJGI_Info <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/AlveolataJGI_Information.csv")
AlveolataJGI_Info <- AlveolataJGI_Info %>% rename(Accn = "portal", Organism_Taxonomic_ID = "NCBI.Taxon", Organism.Name = "name")
AlveolataJGI_Info$Accn <- sub("\\_.*", "", AlveolataJGI_Info$Accn)

RICTORAlveolataJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Alveolata_JGI/AlveolataJGI_RICTOR.csv")
RICTORAlveolataJGI$Accn <- sub("\\_.*", "", RICTORAlveolataJGI$Accn)
RICTORAlveolataJGI <- JoinInfo(RICTORAlveolataJGI,"Alveolata",AlveolataJGI_Info,Taxon,"JGI")

RAPTORAlveolataJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Alveolata_JGI/AlveolataJGI_RAPTOR.csv")
RAPTORAlveolataJGI$Accn <- sub("\\_.*", "", RAPTORAlveolataJGI$Accn)
RAPTORAlveolataJGI <- JoinInfo(RAPTORAlveolataJGI,"Alveolata",AlveolataJGI_Info,Taxon,"JGI")

SIN1AlveolataJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Alveolata_JGI/AlveolataJGI_SIN1.csv")
SIN1AlveolataJGI$Accn <- sub("\\_.*", "", SIN1AlveolataJGI$Accn)
SIN1AlveolataJGI <- JoinInfo(SIN1AlveolataJGI,"Alveolata",AlveolataJGI_Info,Taxon,"JGI")

LST8AlveolataJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Alveolata_JGI/AlveolataJGI_LST8.csv")
LST8AlveolataJGI$Accn <- sub("\\_.*", "", LST8AlveolataJGI$Accn)
LST8AlveolataJGI <- JoinInfo(LST8AlveolataJGI,"Alveolata",AlveolataJGI_Info,Taxon,"JGI")

TORAlveolataJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Alveolata_JGI/AlveolataJGI_TOR.csv")
TORAlveolataJGI$Accn <- sub("\\_.*", "", TORAlveolataJGI$Accn)
TORAlveolataJGI <- JoinInfo(TORAlveolataJGI,"Alveolata",AlveolataJGI_Info,Taxon,"JGI")

AlveolataJGI <- ModCombine("Alveolata", AlveolataJGI_Info, RICTORAlveolataJGI,SIN1AlveolataJGI,RAPTORAlveolataJGI,LST8AlveolataJGI,TORAlveolataJGI)
AlveolataJGI %>% filter(!if_all(5:9, is.na))
AlveolataJGI <- AlveolataJGI %>% mutate(source = "JGI") %>% left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
#-------------------------------------------------------------------------------

AlveolataF <- rbind(Alveolata,AlveolataJGI)

#-------------------------------------------------------------------------------
#Rhodophyta Here

RhodophytaInfo <- read_tsv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhodophyta/Rhodophyta_Names.tsv")
RhodophytaInfo <- RhodophytaInfo %>%
  rename(Accn = "Assembly Accession", Organism.Name = "Organism Name", Organism_Taxonomic_ID = "Organism Taxonomic ID")


RICTORRhodo <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhodophyta/RICTOR_Rhodophyta.csv")
RICTORRhodo <- JoinInfo(RICTORRhodo, "Rhodophyta",RhodophytaInfo, Taxon, "NCBI")

RAPTORRhodo <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhodophyta/RAPTOR_Rhodophyta.csv")
RAPTORRhodo <- JoinInfo(RAPTORRhodo, "Rhodophyta",RhodophytaInfo, Taxon, "NCBI")

SIN1Rhodo <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhodophyta/SIN1_Rhodophyta.csv")
SIN1Rhodo <- JoinInfo(SIN1Rhodo, "Rhodophyta",RhodophytaInfo, Taxon, "NCBI")

LST8Rhodo <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhodophyta/LST8_Rhodophyta.csv")
LST8Rhodo <- JoinInfo(LST8Rhodo, "Rhodophyta",RhodophytaInfo, Taxon, "NCBI")

TORRhodo <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhodophyta/TOR_Rhodophyta.csv")
TORRhodo <- JoinInfo(TORRhodo, "Rhodophyta",RhodophytaInfo, Taxon, "NCBI")

Rhodophyta <- ModCombine("Rhodophyta",RhodophytaInfo,RICTORRhodo,SIN1Rhodo,RAPTORRhodo,LST8Rhodo,TORRhodo)
Rhodophyta <- Rhodophyta %>% 
  mutate(source = "NCBI") %>%
  left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))

#-------------------------------------------------------------------------------
StreptophytaInfo <- read_tsv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Streptophyta/Streptophytina_Information.tsv")
StreptophytaInfo <- StreptophytaInfo %>%
  rename(Accn = "Assembly Accession", Organism.Name = "Organism Name", Organism_Taxonomic_ID = "Organism Taxonomic ID")

RICTORStreptophyta <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Streptophyta/RICTOR_Streptophyta.csv")
RICTORStreptophyta <- JoinInfo(RICTORStreptophyta, "Streptophyta",StreptophytaInfo,Taxon,"NCBI")

RAPTORStreptophyta <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Streptophyta/RAPTOR_Streptophyta.csv")
RAPTORStreptophyta <- JoinInfo(RAPTORStreptophyta,"Streptophyta",StreptophytaInfo,Taxon,"NCBI")

SIN1Streptophyta <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Streptophyta/SIN1_Streptophyta.csv")
SIN1Streptophyta <- JoinInfo(SIN1Streptophyta,"Streptophyta",StreptophytaInfo,Taxon,"NCBI")

LST8Streptophyta <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Streptophyta/LST8_Streptophyta.csv")
LST8Streptophyta <- JoinInfo(LST8Streptophyta,"Streptophyta",StreptophytaInfo,Taxon,"NCBI")

TORStreptophyta <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Streptophyta/TOR_Streptophyta.csv")
TORStreptophyta <- JoinInfo(TORStreptophyta,"Streptophyta",StreptophytaInfo,Taxon,"NCBI")

Streptophyta <- ModCombine("Streptophyta", StreptophytaInfo, RICTORStreptophyta, SIN1Streptophyta, RAPTORStreptophyta, LST8Streptophyta, TORStreptophyta)
Streptophyta <- Streptophyta %>% 
  mutate(source = "NCBI") %>%
  left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))


#-------------------------------------------------------------------------------
Stramenopile_Info <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Stramenopiles_Information_2.csv")
Stramenopile_Info <- Stramenopile_Info %>% rename(Accn = "Assembly.Accession")

RICTORStramenopiles <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/RICTORStramenopiles.csv")
RICTORStramenopiles <- JoinInfo(RICTORStramenopiles,"Stramenopiles",Stramenopile_Info,Taxon,"NCBI")


RAPTORStramenopiles <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/RAPTORStramenopiles.csv")
RAPTORStramenopiles <- JoinInfo(RAPTORStramenopiles,"Stramenopiles",Stramenopile_Info,Taxon,"NCBI")

SIN1Stramenopiles <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/SIN1Stramenopiles.csv")
SIN1Stramenopiles <- JoinInfo(SIN1Stramenopiles,"Stramenopiles",Stramenopile_Info,Taxon,"NCBI")


LST8Stramenopiles <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/LST8Stramenopiles.csv")
LST8Stramenopiles <- JoinInfo(LST8Stramenopiles,"Stramenopiles",Stramenopile_Info,Taxon,"NCBI")

TORStramenopiles <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/TORStramenopiles.csv")
TORStramenopiles <- JoinInfo(TORStramenopiles,"Stramenopiles",Stramenopile_Info,Taxon,"NCBI")

Stramenopiles <- ModCombine("Stramenopiles",Stramenopile_Info,RICTORStramenopiles,SIN1Stramenopiles,RAPTORStramenopiles,LST8Stramenopiles,TORStramenopiles)
Stramenopiles <- Stramenopiles %>% mutate(source = "NCBI") %>% left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))

#-------------------------------------------------------------------------------
# Heterokonta (To be merged with Stramenopiles)
HeterokontaInfo <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Information.csv")
HeterokontaInfo <- HeterokontaInfo %>% rename(Accn = "portal", Organism_Taxonomic_ID = "NCBI.Taxon", Organism.Name = "name")
HeterokontaInfo$Accn <- sub("\\_.*", "", HeterokontaInfo$Accn)

RICTORHeterokonta <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_RICTOR.csv")
RICTORHeterokonta$Accn <- sub("\\_.*", "", RICTORHeterokonta$Accn)
RICTORHeterokonta <- JoinInfo(RICTORHeterokonta,"Stramenopiles",HeterokontaInfo,Taxon,"JGI")


RAPTORHeterokonta <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_RAPTOR.csv")
RAPTORHeterokonta$Accn <- sub("\\_.*", "", RAPTORHeterokonta$Accn) 
RAPTORHeterokonta <- JoinInfo(RAPTORHeterokonta,"Stramenopiles",HeterokontaInfo,Taxon,"JGI")

SIN1Heterokonta <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_SIN1.csv")
SIN1Heterokonta$Accn <- sub("\\_.*", "", SIN1Heterokonta$Accn)
SIN1Heterokonta <- JoinInfo(SIN1Heterokonta,"Stramenopiles",HeterokontaInfo,Taxon,"JGI")

LST8Heterokonta <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_LST8.csv")
LST8Heterokonta$Accn <- sub("\\_.*", "", LST8Heterokonta$Accn)
LST8Heterokonta <- JoinInfo(LST8Heterokonta,"Stramenopiles",HeterokontaInfo,Taxon,"JGI")

TORHeterokonta <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_TOR.csv")
TORHeterokonta$Accn <- sub("\\_.*", "", TORHeterokonta$Accn)
TORHeterokonta <- JoinInfo(TORHeterokonta,"Stramenopiles",HeterokontaInfo,Taxon,"JGI")

Heterokonts <- ModCombine("Stramenopiles", HeterokontaInfo, RICTORHeterokonta,SIN1Heterokonta,RAPTORHeterokonta,LST8Heterokonta,TORHeterokonta)
Heterokonts %>% filter(!if_all(5:9, is.na))
Heterokonts <- Heterokonts %>% mutate(source = "JGI") %>% left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))

#-------------------------------------------------------------------------------

StramenopilesF <- rbind(Stramenopiles,Heterokonts)

#-------------------------------------------------------------------------------

RhizariaInfo <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_Information_2.csv")
RhizariaInfo <- RhizariaInfo %>% rename(Accn = "Assembly.Accession", Organism.Name = "Organism.Name", Organism_Taxonomic_ID = "Organism_Taxonomic_ID")

RhizariaJGI_Info <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/RhizariaJGI_Information.csv")
RhizariaJGI_Info <- RhizariaJGI_Info %>% rename(Accn = "portal", Organism_Taxonomic_ID = "NCBI.Taxon", Organism.Name = "name")
RhizariaJGI_Info$Accn <- sub("\\_.*", "", RhizariaJGI_Info$Accn)


RICTORRhizaria <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/RICTORRhizaria.csv")
RICTORRhizaria <- JoinInfo(RICTORRhizaria,"Rhizaria",RhizariaInfo,Taxon,"NCBI")

RAPTORRhizaria <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/RAPTORRhizaria.csv")
RAPTORRhizaria <- JoinInfo(RAPTORRhizaria,"Rhizaria",RhizariaInfo,Taxon,"NCBI")

SIN1Rhizaria <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/SIN1Rhizaria.csv")
SIN1Rhizaria <- JoinInfo(SIN1Rhizaria,"Rhizaria",RhizariaInfo,Taxon,"JGI")

LST8Rhizaria <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/LST8Rhizaria.csv")
LST8Rhizaria <- JoinInfo(LST8Rhizaria,"Rhizaria",RhizariaInfo,Taxon,"JGI")

TORRhizaria <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Rhizaria/TORRhizaria.csv")
TORRhizaria <- JoinInfo(TORRhizaria,"Rhizaria",RhizariaInfo,Taxon,"JGI")

RICTORRJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_JGI/RhizariaJGI_RICTOR.csv")
RICTORRJGI$Accn <- sub("\\_.*", "", RICTORRJGI$Accn)
RICTORRJGI <- JoinInfo(RICTORRJGI,"Rhizaria",RhizariaJGI_Info,Taxon,"JGI")


RAPTORRJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_JGI/RhizariaJGI_RAPTOR.csv")
RAPTORRJGI$Accn <- sub("\\_.*", "", RAPTORRJGI$Accn)
RAPTORRJGI <- JoinInfo(RAPTORRJGI,"Rhizaria",RhizariaJGI_Info,Taxon,"JGI")


SIN1RJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_JGI/RhizariaJGI_SIN1.csv")
SIN1RJGI$Accn <- sub("\\_.*", "", SIN1RJGI$Accn)
SIN1RJGI <- JoinInfo(SIN1RJGI,"Rhizaria",RhizariaJGI_Info,Taxon,"JGI")


LST8RJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_JGI/RhizariaJGI_LST8.csv")
LST8RJGI$Accn <- sub("\\_.*", "", LST8RJGI$Accn)
LST8RJGI <- JoinInfo(LST8RJGI,"Rhizaria",RhizariaJGI_Info,Taxon,"JGI")


TORRJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_JGI/RhizariaJGI_TOR.csv")
TORRJGI$Accn <- sub("\\_.*", "", TORRJGI$Accn)
TORRJGI <- JoinInfo(TORRJGI,"Rhizaria",RhizariaJGI_Info,Taxon,"JGI")


Rhizaria <- ModCombine("Rhizaria",RhizariaInfo,RICTORRhizaria, SIN1Rhizaria, RAPTORRhizaria, LST8Rhizaria, TORRhizaria)
Rhizaria <- Rhizaria %>%
  mutate(source = "NCBI") %>%
  left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))

JGIRhizaria <- ModCombine("Rhizaria", RhizariaJGI_Info, RICTORRJGI, SIN1RJGI, RAPTORRJGI, LST8RJGI, TORRJGI)
JGIRhizaria <- JGIRhizaria %>%
  mutate(source = "JGI") %>%
  left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))

RhizariaF <- rbind(Rhizaria,JGIRhizaria)




#-------------------------------------------------------------------------------

ChlorophytaInfo <- read_tsv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/Chlorophyta_Names.tsv")
ChlorophytaInfo <- ChlorophytaInfo %>% rename(Accn = "Assembly Accession", Organism.Name = "Organism Name", Organism_Taxonomic_ID = "Organism Taxonomic ID")

RICTORChlorophyta <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/RICTOR_Chlorophyta.csv")
RICTORChlorophyta <- JoinInfo(RICTORChlorophyta,"Chlorophyta",ChlorophytaInfo,Taxon,"NCBI")

RAPTORChlorophyta <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/RAPTOR_Chlorophyta.csv")
RAPTORChlorophyta <- JoinInfo(RAPTORChlorophyta,"Chlorophyta",ChlorophytaInfo,Taxon,"NCBI")

SIN1Chlorophyta <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/SIN1_Chlorophyta.csv")
SIN1Chlorophyta <- JoinInfo(SIN1Chlorophyta,"Chlorophyta",ChlorophytaInfo,Taxon,"NCBI")

LST8Chlorophyta <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/LST8_Chlorophyta.csv")
LST8Chlorophyta <- JoinInfo(LST8Chlorophyta,"Chlorophyta",ChlorophytaInfo,Taxon,"NCBI") 

TORChlorophyta <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/TOR_Chlorophyta.csv")
TORChlorophyta <- JoinInfo(TORChlorophyta,"Chlorophyta",ChlorophytaInfo,Taxon,"NCBI")

Chlorophyta <- ModCombine("Chlorophyta", ChlorophytaInfo, RICTORChlorophyta, SIN1Chlorophyta, RAPTORChlorophyta, LST8Chlorophyta, TORChlorophyta)
Chlorophyta <- Chlorophyta %>% mutate(source = "NCBI") %>% left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))

#-------------------------------------------------------------------------------
#Chlorophyta JGI

ChlorophytaJGI_Info <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/ChlorophytaJGI_Information.csv")
ChlorophytaJGI_Info <- ChlorophytaJGI_Info %>% rename(Accn = "portal", Organism_Taxonomic_ID = "NCBI.Taxon", Organism.Name = "name")
ChlorophytaJGI_Info$Accn <- sub("\\_.*", "", ChlorophytaJGI_Info$Accn)

RICTORChlorophytaJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_RICTOR.csv")
RICTORChlorophytaJGI$Accn <- sub("\\_.*", "", RICTORChlorophytaJGI$Accn)
RICTORChlorophytaJGI <- JoinInfo(RICTORChlorophytaJGI,"Chlorophyta",ChlorophytaJGI_Info,Taxon,"JGI")

RAPTORChlorophytaJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_RAPTOR.csv")
RAPTORChlorophytaJGI$Accn <- sub("\\_.*", "", RAPTORChlorophytaJGI$Accn)
RAPTORChlorophytaJGI <- JoinInfo(RAPTORChlorophytaJGI,"Chlorophyta",ChlorophytaJGI_Info,Taxon,"JGI")

SIN1ChlorophytaJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_SIN1.csv")
SIN1ChlorophytaJGI$Accn <- sub("\\_.*", "", SIN1ChlorophytaJGI$Accn)
SIN1ChlorophytaJGI <- JoinInfo(SIN1ChlorophytaJGI, "Chlorophyta",ChlorophytaJGI_Info, Taxon, "JGI")

LST8ChlorophytaJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_LST8.csv")
LST8ChlorophytaJGI$Accn <- sub("\\_.*", "", LST8ChlorophytaJGI$Accn)
LST8ChlorophytaJGI <- JoinInfo(LST8ChlorophytaJGI, "Chlorophyta",ChlorophytaJGI_Info, Taxon, "JGI")

TORChlorophytaJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_TOR.csv")
TORChlorophytaJGI$Accn <- sub("\\_.*", "", TORChlorophytaJGI$Accn)
TORChlorophytaJGI <- JoinInfo(TORChlorophytaJGI, "Chlorophyta",ChlorophytaJGI_Info, Taxon, "JGI")


ChlorophytaJGI <- ModCombine("Chlorophyta",ChlorophytaJGI_Info, RICTORChlorophytaJGI, SIN1ChlorophytaJGI, RAPTORChlorophytaJGI, LST8ChlorophytaJGI, TORChlorophytaJGI)
ChlorophytaJGI <- ChlorophytaJGI %>% filter(!if_all(5:9, is.na)) %>%
  mutate(source = "JGI") %>%
  left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
#-------------------------------------------------------------------------------

ChlorophytaF <- rbind(Chlorophyta, ChlorophytaJGI)


#-------------------------------------------------------------------------------
# Doing the Discoba and Metamonada first, followed with the extra excavata

MetamonadaInfo <- read_tsv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/Metamonada_Names.tsv")
MetamonadaInfo <- MetamonadaInfo %>%
  rename(Accn = "Assembly Accession", Organism.Name = "Organism Name", Organism_Taxonomic_ID = "Organism Taxonomic ID")
DiscobaInfo <- read_tsv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Discoba/Discoba_Names.tsv")
DiscobaInfo <- DiscobaInfo %>%
  rename(Accn = "Assembly Accession", Organism.Name = "Organism Name", Organism_Taxonomic_ID = "Organism Taxonomic ID")

ExcavataJGI_Info <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/ExcavatesJGI_Information.csv")
ExcavataJGI_Info <- ExcavataJGI_Info %>% rename(Accn = "portal", Organism_Taxonomic_ID = "NCBI.Taxon", Organism.Name = "name")
ExcavataJGI_Info$Accn <- sub("\\_.*", "", ExcavataJGI_Info$Accn)


RICTORMetamonada <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/RICTOR_Metamonada.csv")
RICTORMetamonada <- JoinInfo(RICTORMetamonada,"Metamonada",MetamonadaInfo,Taxon,"NCBI")

RAPTORMetamonada <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/RAPTOR_Metamonada.csv")
RAPTORMetamonada <- JoinInfo(RAPTORMetamonada,"Metamonada",MetamonadaInfo,Taxon,"NCBI")

SIN1Metamonada <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/SIN1_Metamonada.csv")
SIN1Metamonada <- JoinInfo(SIN1Metamonada,"Metamonada",MetamonadaInfo,Taxon,"NCBI")

LST8Metamonada <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/LST8_Metamonada.csv")
LST8Metamonada <- JoinInfo(LST8Metamonada,"Metamonada",MetamonadaInfo,Taxon,"NCBI")

TORMetamonada <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/TOR_Metamonada.csv")
TORMetamonada <- JoinInfo(TORMetamonada,"Metamonada",MetamonadaInfo,Taxon,"NCBI")

RICTORDiscoba <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Discoba/RICTOR_Discoba.csv")
RICTORDiscoba <- JoinInfo(RICTORDiscoba, "Discoba", DiscobaInfo, Taxon, "NCBI")

RAPTORDiscoba <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Discoba/RAPTOR_Discoba.csv")
RAPTORDiscoba <- JoinInfo(RAPTORDiscoba, "Discoba", DiscobaInfo, Taxon, "NCBI")

SIN1Discoba <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Discoba/SIN1_Discoba.csv")
SIN1Discoba <- JoinInfo(SIN1Discoba, "Discoba", DiscobaInfo, Taxon, "NCBI")

LST8Discoba <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Discoba/LST8_Discoba.csv")
LST8Discoba <- JoinInfo(LST8Discoba, "Discoba", DiscobaInfo, Taxon, "NCBI")

TORDiscoba <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Discoba/TOR_Discoba.csv")
TORDiscoba <- JoinInfo(TORDiscoba, "Discoba", DiscobaInfo, Taxon,"NCBI")

RICTORExc <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Excavata_JGI/ExcavataJGI_RICTOR.csv")
RICTORExc$Accn <- sub("\\_.*", "", RICTORExc$Accn)
RICTORExc <- JoinInfo(RICTORExc, "Excavata", ExcavataJGI_Info, Taxon, "JGI")

RAPTORExc <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Excavata_JGI/ExcavataJGI_RAPTOR.csv")
RAPTORExc$Accn <- sub("\\_.*", "", RAPTORExc$Accn)
RAPTORExc <- JoinInfo(RAPTORExc, "Excavata", ExcavataJGI_Info, Taxon, "JGI")

SIN1Exc <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Excavata_JGI/ExcavataJGI_SIN1.csv")
SIN1Exc$Accn <- sub("\\_.*", "", SIN1Exc$Accn)
SIN1Exc <- JoinInfo(SIN1Exc, "Excavata", ExcavataJGI_Info, Taxon, "JGI")

LST8Exc <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Excavata_JGI/ExcavataJGI_LST8.csv")
LST8Exc$Accn <- sub("\\_.*", "", LST8Exc$Accn)
LST8Exc <- JoinInfo(LST8Exc, "Excavata", ExcavataJGI_Info, Taxon, "JGI")

TORExc <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Excavata_JGI/ExcavataJGI_TOR.csv")
TORExc$Accn <- sub("\\_.*", "", TORExc$Accn)
TORExc <- JoinInfo(TORExc, "Excavata", ExcavataJGI_Info, Taxon, "JGI")



Metamonada <- ModCombine("Metamonada",MetamonadaInfo,RICTORMetamonada, SIN1Metamonada, RAPTORMetamonada, LST8Metamonada, TORMetamonada)
Metamonada <- Metamonada %>%
  mutate(source = "NCBI") %>%
  left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))

Discoba <- ModCombine("Discoba", DiscobaInfo, RICTORDiscoba, SIN1Discoba, RAPTORDiscoba, LST8Discoba, TORDiscoba)
Discoba <- Discoba %>%
  mutate(source = "NCBI") %>%
  left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))


ExcavataJGI <- ModCombine("Excavata", ExcavataJGI_Info, RICTORExc, SIN1Exc, RAPTORExc, LST8Exc, TORExc)
ExcavataJGI <- ExcavataJGI %>%
  mutate(source = "JGI") %>%
  left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))

ExcavataF <- rbind(Discoba, Metamonada, ExcavataJGI)
which(ExcavataF$Organism.Name == "Euglena gracilis CCAP 1224/5Z v1.0")

#-------------------------------------------------------------------------------


Final_Table <- rbind(StramenopilesF, AlveolataF, RhizariaF, Rhodophyta, Streptophyta, ChlorophytaF, ExcavataF)
Final_Table <- distinct(Final_Table, Organism_Taxonomic_ID, .keep_all = TRUE)
Eug <- which(Final_Table$Organism.Name == "Euglena gracilis CCAP 1224/5Z v1.0")
Final_Table$Group[Eug] <- "Discoba"
GI <- which(Final_Table$Organism.Name == "Giardia intestinalis ATCC 50803")
Final_Table$Group[GI] <- "Metamonada"
write.csv(Final_Table,file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/NumericTable.csv")

rm(list = ls())
