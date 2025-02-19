# Script for combining all of the numeric scores and outputting to file
# Kyle Johnson
# 2/19/2025

#First we read in the Data for RICTOR, RAPTOR, LST8, SIN1, TOR
## We will do this for each of the clades we are interested in
## We will also include the JGI dataset as well at a later point

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
  
  returnDF <- returnDF %>% add_column(SIN1 = NA, RICTOR = NA, RAPTOR = NA, TOR = NA, LST8 = NA)
  returnDF <- returnDF %>% add_column(Group = Grouping, .before = "Accn")
  
  #Check the input dataframes to see if they are of length 0. If they are, do something about it
  
  
  if(nrow(RICTORdf) != 0){# First loop to check for RICTOR
    for (n in 1:nrow(returnDF)){
      for (m in 1:nrow(RICTORdf)){
        #condition check to see if the taxIDs line up
        if(returnDF[n, "Organism_Taxonomic_ID"] == RICTORdf[m, "Organism_Taxonomic_ID"]){
            returnDF$RICTOR[n] <- RICTORdf$scd[m]
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
            returnDF$SIN1[n] <- SIN1df$scd[m]
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
            returnDF$RAPTOR[n] <- RAPTORdf$scd[m]
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
            returnDF$LST8[n] <- LST8df$scd[m]
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
            returnDF$TOR[n] <- TORdf$scd[m]
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








Taxon <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/Combined_Taxonomy.csv")
#-------------------------------------------------------------------------------
AlveolataInfo <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Alveolata_Information_2.csv")
AlveolataInfo <- AlveolataInfo %>% rename(Accn = "Assembly.Accession")

RICTORAlveolata <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/RICTORAlveolata.csv")
RICTORAlveolata <- JoinInfo(RICTORAlveolata, "Alveolata",AlveolataInfo,Taxon,"NCBI")

RAPTORAlveolata <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/RAPTORAlveolata.csv")
RAPTORAlveolata <- JoinInfo(RAPTORAlveolata, "Alveolata", AlveolataInfo, Taxon, "NCBI")

SIN1Alveolata <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/SIN1Alveolata.csv")
SIN1Alveolata <- JoinInfo(SIN1Alveolata, "Alveolata", AlveolataInfo, Taxon, "NCBI")

LST8Alveolata <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/LST8Alveolata.csv")
LST8Alveolata <- JoinInfo(LST8Alveolata,"Alveolata",AlveolataInfo, Taxon, "NCBI")

TORAlveolata <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Alveolata/TORAlveolata.csv")
TORAlveolata <- JoinInfo(TORAlveolata,"Alveolata",AlveolataInfo,Taxon,"NCBI")

Alveolata <- ModCombine("Alveolata",AlveolataInfo,RICTORAlveolata,SIN1Alveolata,RAPTORAlveolata,LST8Alveolata,TORAlveolata)
Alveolata <- Alveolata %>% mutate(source = "NCBI") %>% left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
#-------------------------------------------------------------------------------

StreptophytaInfo <- read_tsv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Streptophyta/Streptophytina_Information.tsv")
StreptophytaInfo <- StreptophytaInfo %>%
  rename(Accn = "Assembly Accession", Organism.Name = "Organism Name", Organism_Taxonomic_ID = "Organism Taxonomic ID")

RICTORStreptophyta <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Streptophyta/RICTOR_Streptophyta.csv")
RICTORStreptophyta <- JoinInfo(RICTORStreptophyta, "Streptophyta",StreptophytaInfo,Taxon,"NCBI")

RAPTORStreptophyta <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Streptophyta/RAPTOR_Streptophyta.csv")
RAPTORStreptophyta <- JoinInfo(RAPTORStreptophyta,"Streptophyta",StreptophytaInfo,Taxon,"NCBI")

SIN1Streptophyta <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Streptophyta/SIN1_Streptophyta.csv")
SIN1Streptophyta <- JoinInfo(SIN1Streptophyta,"Streptophyta",StreptophytaInfo,Taxon,"NCBI")

LST8Streptophyta <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Streptophyta/LST8_Streptophyta.csv")
LST8Streptophyta <- JoinInfo(LST8Streptophyta,"Streptophyta",StreptophytaInfo,Taxon,"NCBI")

TORStreptophyta <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Streptophyta/TOR_Streptophyta.csv")
TORStreptophyta <- JoinInfo(TORStreptophyta,"Streptophyta",StreptophytaInfo,Taxon,"NCBI")

Streptophyta <- ModCombine("Streptophyta", StreptophytaInfo, RICTORStreptophyta, SIN1Streptophyta, RAPTORStreptophyta, LST8Streptophyta, TORStreptophyta)
Streptophyta <- Streptophyta %>% 
  mutate(source = "NCBI") %>%
  left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))


#-------------------------------------------------------------------------------
Stramenopile_Info <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Stramenopiles_Information_2.csv")
Stramenopile_Info <- Stramenopile_Info %>% rename(Accn = "Assembly.Accession")

RICTORStramenopiles <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/RICTORStramenopiles.csv")
RICTORStramenopiles <- JoinInfo(RICTORStramenopiles,"Stramenopiles",Stramenopile_Info,Taxon,"NCBI")


RAPTORStramenopiles <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/RAPTORStramenopiles.csv")
RAPTORStramenopiles <- JoinInfo(RAPTORStramenopiles,"Stramenopiles",Stramenopile_Info,Taxon,"NCBI")

SIN1Stramenopiles <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/SIN1Stramenopiles.csv")
SIN1Stramenopiles <- JoinInfo(SIN1Stramenopiles,"Stramenopiles",Stramenopile_Info,Taxon,"NCBI")


LST8Stramenopiles <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/LST8Stramenopiles.csv")
LST8Stramenopiles <- JoinInfo(LST8Stramenopiles,"Stramenopiles",Stramenopile_Info,Taxon,"NCBI")

TORStramenopiles <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Stramenopiles/TORStramenopiles.csv")
TORStramenopiles <- JoinInfo(TORStramenopiles,"Stramenopiles",Stramenopile_Info,Taxon,"NCBI")

Stramenopiles <- ModCombine("Stramenopiles",Stramenopile_Info,RICTORStramenopiles,SIN1Stramenopiles,RAPTORStramenopiles,LST8Stramenopiles,TORStramenopiles)
Stramenopiles <- Stramenopiles %>% mutate(source = "NCBI") %>% left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))

#-------------------------------------------------------------------------------
# Heterokonta (To be merged with Stramenopiles)
HeterokontaInfo <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Information.csv")
HeterokontaInfo <- HeterokontaInfo %>% rename(Accn = "portal", Organism_Taxonomic_ID = "NCBI.Taxon", Organism.Name = "name")
HeterokontaInfo$Accn <- sub("\\_.*", "", HeterokontaInfo$Accn)

RICTORHeterokonta <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_RICTOR.csv")
RICTORHeterokonta$Accn <- sub("\\_.*", "", RICTORHeterokonta$Accn)
RICTORHeterokonta <- JoinInfo(RICTORHeterokonta,"Stramenopiles",HeterokontaInfo,Taxon,"JGI")


RAPTORHeterokonta <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_RAPTOR.csv")
RAPTORHeterokonta$Accn <- sub("\\_.*", "", RAPTORHeterokonta$Accn) 
RAPTORHeterokonta <- JoinInfo(RAPTORHeterokonta,"Stramenopiles",HeterokontaInfo,Taxon,"JGI")

SIN1Heterokonta <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_SIN1.csv")
SIN1Heterokonta$Accn <- sub("\\_.*", "", SIN1Heterokonta$Accn)
SIN1Heterokonta <- JoinInfo(SIN1Heterokonta,"Stramenopiles",HeterokontaInfo,Taxon,"JGI")

LST8Heterokonta <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_LST8.csv")
LST8Heterokonta$Accn <- sub("\\_.*", "", LST8Heterokonta$Accn)
LST8Heterokonta <- JoinInfo(LST8Heterokonta,"Stramenopiles",HeterokontaInfo,Taxon,"JGI")

TORHeterokonta <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_TOR.csv")
TORHeterokonta$Accn <- sub("\\_.*", "", TORHeterokonta$Accn)
TORHeterokonta <- JoinInfo(TORHeterokonta,"Stramenopiles",HeterokontaInfo,Taxon,"JGI")

Heterokonts <- ModCombine("Stramenopiles", HeterokontaInfo, RICTORHeterokonta,SIN1Heterokonta,RAPTORHeterokonta,LST8Heterokonta,TORHeterokonta)
Heterokonts %>% filter(!if_all(5:9, is.na))
Heterokonts <- Heterokonts %>% mutate(source = "JGI") %>% left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))

#-------------------------------------------------------------------------------

StramenopilesF <- rbind(Stramenopiles,Heterokonts)

#-------------------------------------------------------------------------------

ChlorophytaInfo <- read_tsv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/Chlorophyta_Names.tsv")
ChlorophytaInfo <- ChlorophytaInfo %>% rename(Accn = "Assembly Accession", Organism.Name = "Organism Name", Organism_Taxonomic_ID = "Organism Taxonomic ID")

RICTORChlorophyta <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/RICTOR_Chlorophyta.csv")
RICTORChlorophyta <- JoinInfo(RICTORChlorophyta,"Chlorophyta",ChlorophytaInfo,Taxon,"NCBI")

RAPTORChlorophyta <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/RAPTOR_Chlorophyta.csv")
RAPTORChlorophyta <- JoinInfo(RAPTORChlorophyta,"Chlorophyta",ChlorophytaInfo,Taxon,"NCBI")

SIN1Chlorophyta <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/SIN1_Chlorophyta.csv")
SIN1Chlorophyta <- JoinInfo(SIN1Chlorophyta,"Chlorophyta",ChlorophytaInfo,Taxon,"NCBI")

LST8Chlorophyta <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/LST8_Chlorophyta.csv")
LST8Chlorophyta <- JoinInfo(LST8Chlorophyta,"Chlorophyta",ChlorophytaInfo,Taxon,"NCBI") 

TORChlorophyta <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Chlorophyta/TOR_Chlorophyta.csv")
TORChlorophyta <- JoinInfo(TORChlorophyta,"Chlorophyta",ChlorophytaInfo,Taxon,"NCBI")

Chlorophyta <- ModCombine("Chlorophyta", ChlorophytaInfo, RICTORChlorophyta, SIN1Chlorophyta, RAPTORChlorophyta, LST8Chlorophyta, TORChlorophyta)
Chlorophyta <- Chlorophyta %>% mutate(source = "NCBI") %>% left_join(select(Taxon, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))

#-------------------------------------------------------------------------------
#Chlorophyta JGI

ChlorophytaJGI_Info <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/ChlorophytaJGI_Information.csv")
ChlorophytaJGI_Info <- ChlorophytaJGI_Info %>% rename(Accn = "portal", Organism_Taxonomic_ID = "NCBI.Taxon", Organism.Name = "name")
ChlorophytaJGI_Info$Accn <- sub("\\_.*", "", ChlorophytaJGI_Info$Accn)

RICTORChlorophytaJGI <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_RICTOR.csv")
RICTORChlorophytaJGI$Accn <- sub("\\_.*", "", RICTORChlorophytaJGI$Accn)
RICTORChlorophytaJGI <- JoinInfo(RICTORChlorophytaJGI,"Chlorophyta",ChlorophytaJGI_Info,Taxon,"JGI")

RAPTORChlorophytaJGI <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_RAPTOR.csv")
RAPTORChlorophytaJGI$Accn <- sub("\\_.*", "", RAPTORChlorophytaJGI$Accn)
RAPTORChlorophytaJGI <- JoinInfo(RAPTORChlorophytaJGI,"Chlorophyta",ChlorophytaJGI_Info,Taxon,"JGI")

SIN1ChlorophytaJGI <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_SIN1.csv")
SIN1ChlorophytaJGI$Accn <- sub("\\_.*", "", SIN1ChlorophytaJGI$Accn)
SIN1ChlorophytaJGI <- JoinInfo(SIN1ChlorophytaJGI, "Chlorophyta",ChlorophytaJGI_Info, Taxon, "JGI")

LST8ChlorophytaJGI <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_LST8.csv")
LST8ChlorophytaJGI$Accn <- sub("\\_.*", "", LST8ChlorophytaJGI$Accn)
LST8ChlorophytaJGI <- JoinInfo(LST8ChlorophytaJGI, "Chlorophyta",ChlorophytaJGI_Info, Taxon, "JGI")

TORChlorophytaJGI <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_TOR.csv")
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

MetamonadaInfo <- read_tsv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/Metamonada_Names.tsv")
MetamonadaInfo <- MetamonadaInfo %>%
  rename(Accn = "Assembly Accession", Organism.Name = "Organism Name", Organism_Taxonomic_ID = "Organism Taxonomic ID")
DiscobaInfo <- read_tsv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Discoba/Discoba_Names.tsv")
DiscobaInfo <- DiscobaInfo %>%
  rename(Accn = "Assembly Accession", Organism.Name = "Organism Name", Organism_Taxonomic_ID = "Organism Taxonomic ID")


RICTORMetamonada <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/RICTOR_Metamonada.csv")
RICTORMetamonada <- JoinInfo(RICTORMetamonada,"Metamonada",MetamonadaInfo,Taxon,"NCBI")

RAPTORMetamonada <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/RAPTOR_Metamonada.csv")
RAPTORMetamonada <- JoinInfo(RAPTORMetamonada,"Metamonada",MetamonadaInfo,Taxon,"NCBI")

SIN1Metamonada <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/SIN1_Metamonada.csv")
SIN1Metamonada <- JoinInfo(SIN1Metamonada,"Metamonada",MetamonadaInfo,Taxon,"NCBI")

LST8Metamonada <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/LST8_Metamonada.csv")
LST8_Metamonada <- JoinInfo(LST8Metamonada,"Metamonada",MetamonadaInfo,Taxon,"NCBI")

TORMetamonada <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Updated_Metamonada/TOR_Metamonada.csv")
TORMetamonada <- JoinInfo(TORMetamonada,"Metamonada",MetamonadaInfo,Taxon,"NCBI")




