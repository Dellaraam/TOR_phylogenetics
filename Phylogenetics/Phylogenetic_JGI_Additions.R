

# Kyle Johnson
# Program to imput heterokonta csv files and perform data cleanup work


if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ggtree")
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Biostrings")
library("Biostrings")
library(tidyverse)
library(ggplot2)
library(ggtree) #Have citation requirements for their usage. See attached message
# LG Wang, TTY Lam, S Xu, Z Dai, L Zhou, T Feng, P Guo, CW Dunn, BR Jones, T Bradley, H Zhu, Y Guan, Y Jiang, G Yu. treeio: an R package for
# phylogenetic tree input and output with richly annotated and associated data. Molecular Biology and Evolution. 2020, 37(2):599-603. doi:
#  10.1093/molbev/msz240
library(treeio)
library(ggrepel)
library(gridExtra)
library(plotly)
library(kableExtra)
library(knitr)
library(patchwork)
library(xtable)

install.packages("tableHTML")
library(tableHTML)

install.packages("useful")
library(useful)
# ------------------------------------------------------------------------------
#Functions Here
mainCombine <- function(Grouping, Namedf, RICTORdf, SIN1df, RAPTORdf, LST8df, TORdf, D1df){
  
  nrow(D1df)
  
  # Make the empty dataframe
  returnDf <- data.frame()
  
  # Add in the taxid and species names columns to the dataframe
  returnDF <- subset(Namedf, select = c("Accn","Organism_Taxonomic_ID", "Organism_Name"))
  
  # Now create empty columns and rows that will be filled in later in the function
  # These will be the "checking" sections for each of the specified proteins
  
  returnDF <- returnDF %>% add_column(SIN1 = NA, RICTOR = NA, RAPTOR = NA, TOR = NA, LST8 = NA, D1 = NA)
  returnDF <- returnDF %>% add_column(Group = Grouping, .before = "Accn")
  
  #Check the input dataframes to see if they are of length 0. If they are, do something about it
  
  
  if(nrow(RICTORdf) != 0){# First loop to check for RICTOR
    for (n in 1:nrow(returnDF)){
      for (m in 1:nrow(RICTORdf)){
        #condition check to see if the taxIDs line up
        if(returnDF[n, "Organism_Taxonomic_ID"] == RICTORdf[m, "Organism_Taxonomic_ID"]){
          if (RICTORdf[m, "scd"] > 300){
            returnDF$RICTOR[n] <- "H"
          }else if (RICTORdf[m,"scd"] < 300 & RICTORdf[m,"scd"] > 150){
            returnDF$RICTOR[n] <- "M"
          }else if (RICTORdf[m,"scd"] < 150){
            returnDF$RICTOR[n] <- "L"
          }
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
          if (SIN1df[m, "scd"] > 300){
            returnDF$SIN1[n] <- "H"
          }else if (SIN1df[m,"scd"] < 300 & SIN1df[m,"scd"] > 150){
            returnDF$SIN1[n] <- "M"
          }else if (SIN1df[m,"scd"] < 150){
            returnDF$SIN1[n] <- "L"
          }
        } else{
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
          if (RAPTORdf[m, "scd"] > 300){
            returnDF$RAPTOR[n] <- "H"
          }else if (RAPTORdf[m,"scd"] < 300 & RAPTORdf[m,"scd"] > 150){
            returnDF$RAPTOR[n] <- "M"
          }else if (RAPTORdf[m,"scd"] < 150){
            returnDF$RAPTOR[n] <- "L"
          }
        } else{
          next
        }
      }
      
    }
    print("Finished RAPTOR")
  }else{}
  
  
  #Fourth Loop for LST8
  print(nrow(LST8df))
  if(nrow(LST8df) != 0){
    for (n in 1:nrow(returnDF)){
      for (m in 1:nrow(LST8df)){
        #condition check to see if the taxIDs line up
        if(returnDF[n,"Organism_Taxonomic_ID"] == LST8df[m, "Organism_Taxonomic_ID"]){
          if (LST8df[m, "scd"] > 300){
            returnDF$LST8[n] <- "H"
          }else if (LST8df[m,"scd"] < 300 & LST8df[m,"scd"] > 150){
            returnDF$LST8[n] <- "M"
          }else if (LST8df[m,"scd"] < 150){
            returnDF$LST8[n] <- "L"
          }
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
          if (TORdf[m, "scd"] > 300){
            returnDF$TOR[n] <- "H"
          }else if (TORdf[m,"scd"] < 300 & TORdf[m,"scd"] > 150){
            returnDF$TOR[n] <- "M"
          }else if (TORdf[m,"scd"] < 150){
            returnDF$TOR[n] <- "L"
          }
        } else{
          next
        }
      }
      
    }
  }else{}
  #Fifth Loop for D1
  if(nrow(D1df) != 0){
    for (n in 1:nrow(returnDF)){
      for (m in 1:nrow(D1df)){
        #condition check to see if the taxIDs line up
        if(returnDF[n, "Organism_Taxonomic_ID"] == D1df[m, "Organism_Taxonomic_ID"]){
          if (D1df[m, "scd"] > 300){
            returnDF$D1[n] <- "H"
          }else if (D1df[m,"scd"] < 300 & D1df[m,"scd"] > 150){
            returnDF$D1[n] <- "M"
          }else if (D1df[m,"scd"] < 150){
            returnDF$D1[n] <- "L"
          }
        } else{
          next
        }
      }
      
    } 
  }else{
    returnDF$D1 <- NA
  }
  
  
  # Finally return the completed dataframe   
  return(returnDF)  
}






JoinInfo <- function(RawData, cladeInfo, GroupInfo ,TaxonomicInformation){
  
  #view(RawData)
  #view(GroupInfo)
  RawData <- merge(RawData, GroupInfo[c("Organism_Taxonomic_ID", "name", "Accn")], by="Accn")
  cleanedData <- RawData %>% filter(sca >= 100 & scd >= 100) %>%
    group_by(Organism_Taxonomic_ID) %>%
    slice_max(scd) %>%
    slice_max(evd)%>%
    ungroup
  cleanedData <- cleanedData %>% left_join(select(TaxonomicInformation, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
  cleanedData <- cleanedData %>% mutate(Super.Group = cladeInfo) %>% mutate(Source = "JGI")
  cleanedData <- cleanedData %>% relocate(Super.Group)%>% rename(Organism_Name = "name")
  
  return(cleanedData)
  
}



#-------------------------------------------------------------------------------
# Ultimately we will need to create a list of all of the papers required
# This section is dealing with Heterokonta so use appropriately



#Read in combined Data for later
CombinedStramenopiles <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Combined_Stramenopile.csv")
#remove any X columns that get made for some reason(figure out why)
CombinedStramenopiles <- select(CombinedStramenopiles, -X, -C.score, -Frag.score)
CombinedStramenopiles <- mutate(CombinedStramenopiles, Source = "NCBI")

CombinedChlorophyta <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Combined_Chlorophyta.csv")
CombinedChlorophyta <- select(CombinedChlorophyta,-C.score, -Frag.score)
CombinedChlorophyta <- mutate(CombinedChlorophyta, Source = "NCBI")
#This is an optional/As needed command for chlorophyta
CombinedChlorophyta <- select(CombinedChlorophyta,-X, -Phylum)
CombinedChlorophyta <- rename(CombinedChlorophyta, Organism.Name = "Organism_Name")
CombinedChlorophyta <- mutate(CombinedChlorophyta, Group = "Chlorophyta")


CombinedRhizaria <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Combined_Rhizaria.csv")
CombinedRhizaria <- select(CombinedRhizaria, -C.score, -Frag.score, -X)
CombinedRhizaria <- mutate(CombinedRhizaria, Source = "NCBI")

#For excavata, have to do some special additions
#Need to read in both the discoba and the metamonada as they are excavates
CombinedDiscoba <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Combined_Discoba.csv")
CombinedDiscoba <- select(CombinedDiscoba, -C.score, -Frag.score, -X)
CombinedDiscoba <- mutate(CombinedDiscoba, Source = "NCBI")
CombinedDiscoba <- rename(CombinedDiscoba, Organism.Name = "Organism_Name", Group = "Phylum")

CombinedMetamonada <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Combined_Metamonada.csv")
CombinedMetamonada <- select(CombinedMetamonada, -C.score, -Frag.score, -X)
CombinedMetamonada <- mutate(CombinedMetamonada, Source="NCBI")
CombinedMetamonada <- rename(CombinedMetamonada, Organism.Name = "Organism_Name", Group = "Phylum")

#Use this going forward for the Excavates
CombinedExcavates <- rbind(CombinedMetamonada,CombinedDiscoba)




# ------------------------------------------------------------------------------
#Read in BUSCO here:
Busco_Chlorophyta_Original <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta-BUSCO-Sheet1.csv")
Busco_Chlorophyta_Original <- select(Busco_Chlorophyta_Original, Accession, C.score, Frag.score) %>% rename(Accn = "Accession")


Busco_Rhizaria_Original <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_BUSCO_Sheet1.csv")
Busco_Rhizaria_Original <- select(Busco_Rhizaria_Original, Accession, C.score, Frag.score) %>% rename(Accn = "Accession")


Busco_Stramenopiles_Original <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Stramenopiles BUSCO - Sheet1.csv")
Busco_Stramenopiles_Original <- select(Busco_Stramenopiles_Original, Accession, C.score, Frag.score) %>% rename(Accn = "Accession")


Busco_Excavata_Original <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Excavata-BUSCO-Sheet1.csv")
Busco_Excavata_Original <- select(Busco_Excavata_Original, Accession, C.score, Frag.score) %>% rename(Accn = "Accession")

# New versions to be added to the original
Busco_ExcavataJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/ExcavataJGIBusco.csv")
Busco_ExcavataJGI <- select(Busco_ExcavataJGI, Accn, C.score, Frag.score)
Busco_ExcavataJGI$Accn <- sub("\\_.*", "", Busco_ExcavataJGI$Accn)

Busco_HeterokontaJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/HeterokontaJGIBusco.csv")
Busco_HeterokontaJGI <- select(Busco_HeterokontaJGI, Accn, C.score, Frag.score)
Busco_HeterokontaJGI$Accn <- sub("\\_.*", "", Busco_HeterokontaJGI$Accn)

Busco_ChlorophytaJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/ChlorophytaJGIBusco.csv")
Busco_ChlorophytaJGI <- select(Busco_ChlorophytaJGI, Accn, C.score, Frag.score)
Busco_ChlorophytaJGI$Accn <- sub("\\_.*", "", Busco_ChlorophytaJGI$Accn)

Busco_RhizariaJGI <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/RhizariaJGIBusco.csv")
Busco_RhizariaJGI <- select(Busco_RhizariaJGI, Accn, C.score, Frag.score)
Busco_RhizariaJGI$Accn <- sub("\\_.*", "", Busco_RhizariaJGI$Accn)

#-------------------------------------------------------------------------------
# Combine the Buscos

RhizariaBusco <- rbind(Busco_Rhizaria_Original,Busco_RhizariaJGI)
ChlorophytaBusco <- rbind(Busco_Chlorophyta_Original, Busco_ChlorophytaJGI)
StramenopileBusco <- rbind(Busco_Stramenopiles_Original, Busco_HeterokontaJGI)
ExcavataBusco <- rbind(Busco_Excavata_Original,Busco_ExcavataJGI)


# Read in the following information: RICTOR for heterokonts and the Heterokonta information packet (different than the ncbi version)
HRictorRaw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_RICTOR.csv")
HRaptorRaw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_RAPTOR.csv")
HTorRaw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_TOR.csv")
HLST8Raw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_LST8.csv")
HSIN1Raw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_SIN1.csv")
HD1Raw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_D1.csv")


#Chlorophyta Raw data here
ChRictorRaw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_RICTOR.csv")
ChRaptorRaw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_RAPTOR.csv")
ChTorRaw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_TOR.csv")
ChLST8Raw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_LST8.csv")
ChSIN1Raw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_SIN1.csv")
ChD1Raw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta_JGI/ChlorophytaJGI_D1.csv")

RhizRICTORRaw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_JGI/RhizariaJGI_RICTOR.csv")
RhizRAPTORRaw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_JGI/RhizariaJGI_RAPTOR.csv")
RhizTORRaw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_JGI/RhizariaJGI_TOR.csv")
RhizSIN1Raw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_JGI/RhizariaJGI_SIN1.csv")
RhizLST8Raw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_JGI/RhizariaJGI_LST8.csv")
RhizD1Raw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_JGI/RhizariaJGI_D1.csv")

ExRICTORRaw <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Excavata_JGI/ExcavataJGI_RICTOR.csv")
ExRAPTORRaw <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Excavata_JGI/ExcavataJGI_RAPTOR.csv")
ExTORRaw <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Excavata_JGI/ExcavataJGI_TOR.csv")
ExSIN1Raw <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Excavata_JGI/ExcavataJGI_SIN1.csv")
ExLST8Raw <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Excavata_JGI/ExcavataJGI_LST8.csv")
ExD1Raw <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Excavata_JGI/ExcavataJGI_D1.csv")

# Information from JGI that is needed to tie the data collected to the ncbi information
Phyco_Information <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Phycocosm_Information.csv")
Phyco_Information <- Phyco_Information %>% rename(Accn = "portal", Organism_Taxonomic_ID = "NCBI.Taxon")

Heterok_Information <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Information.csv")
Heterok_Information <- Heterok_Information %>% rename(Accn = "portal", Organism_Taxonomic_ID = "NCBI.Taxon", Organism_Name = "name")

ChlorophytaJGI_Information <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/ChlorophytaJGI_Information.csv")
ChlorophytaJGI_Information <- ChlorophytaJGI_Information %>% rename(Accn = "portal", Organism_Taxonomic_ID = "NCBI.Taxon", Organism_Name = "name")

RhizariaJGI_Information <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/RhizariaJGI_Information.csv")
RhizariaJGI_Information <- RhizariaJGI_Information %>% rename(Accn = "portal", Organism_Taxonomic_ID = "NCBI.Taxon", Organism_Name = "name")

ExcavataJGI_Information <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/ExcavatesJGI_Information.csv")
ExcavataJGI_Information <- ExcavataJGI_Information %>% rename(Accn = "portal", Organism_Taxonomic_ID = "NCBI.Taxon", Organism_Name = "name")



Taxonomic_Information <- read.csv(file="~/GitHub/TOR_phylogenetics/Combined_Taxonomy.csv")




# Now we need to get rid of everything in the Accn number column after a "_"
# This code block removes anything past a "_" and replaces it with nothing
# Maybe replace this in the future with a function call as well
HRictorRaw$Accn<- sub("\\_.*", "", HRictorRaw$Accn)
HRaptorRaw$Accn<- sub("\\_.*", "", HRaptorRaw$Accn)
HTorRaw$Accn<- sub("\\_.*", "", HTorRaw$Accn)
HLST8Raw$Accn<- sub("\\_.*", "", HLST8Raw$Accn)
HSIN1Raw$Accn<- sub("\\_.*", "", HSIN1Raw$Accn)
HD1Raw$Accn<- sub("\\_.*", "", HD1Raw$Accn)

ChRictorRaw$Accn <- sub("\\_.*", "", ChRictorRaw$Accn)
ChRaptorRaw$Accn <- sub("\\_.*", "", ChRaptorRaw$Accn)
ChTorRaw$Accn <- sub("\\_.*", "", ChTorRaw$Accn)
ChLST8Raw$Accn <- sub("\\_.*", "", ChLST8Raw$Accn)
ChSIN1Raw$Accn <- sub("\\_.*", "", ChSIN1Raw$Accn)
ChD1Raw$Accn <- sub("\\_.*", "", ChD1Raw$Accn)

RhizRAPTORRaw$Accn <- sub("\\_.*", "", RhizRAPTORRaw$Accn)
RhizRICTORRaw$Accn <- sub("\\_.*", "", RhizRICTORRaw$Accn)
RhizSIN1Raw$Accn <- sub("\\_.*", "", RhizSIN1Raw$Accn)
RhizLST8Raw$Accn <- sub("\\_.*", "", RhizLST8Raw$Accn)
RhizD1Raw$Accn <- sub("\\_.*", "", RhizD1Raw$Accn)
RhizTORRaw$Accn <- sub("\\_.*", "", RhizTORRaw$Accn)

ExRAPTORRaw$Accn <- sub("\\_.*", "", ExRAPTORRaw$Accn)
ExRICTORRaw$Accn <- sub("\\_.*", "", ExRICTORRaw$Accn)
ExSIN1Raw$Accn <- sub("\\_.*", "", ExSIN1Raw$Accn)
ExLST8Raw$Accn <- sub("\\_.*", "", ExLST8Raw$Accn)
ExD1Raw$Accn <- sub("\\_.*", "", ExD1Raw$Accn)
ExTORRaw$Accn <- sub("\\_.*", "", ExTORRaw$Accn)


Phyco_Information$Accn <- sub("\\_.*", "", Phyco_Information$Accn)


# Now we can attempt to combine the information
# Future note: Need to fix the target information. We need it to be able to reproduce the data
# As it stands, the values should be able to quickly identify but it would be needed soon
# Note to self, need to make a function that does the below. Save on some code here (see JoinInfo function)

HRictorRaw <- merge(HRictorRaw,Phyco_Information[c("Organism_Taxonomic_ID", "name", "Accn")], by="Accn")
HRictor <- HRictorRaw %>% filter(sca >= 100 & scd >= 100)%>%
  group_by(Organism_Taxonomic_ID) %>%
  slice_max(scd) %>%
  slice_max(evd)%>%
  ungroup
HRictor <-HRictor %>% left_join(select(Taxonomic_Information, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
HRictor <- HRictor %>% mutate(Super.Group = "Stramenopiles") %>% mutate(Source = "JGI")
HRictor <- HRictor %>% relocate(Super.Group)%>% rename(Organism_Name = "name")


HRaptorRaw <- merge(HRaptorRaw,Phyco_Information[c("Organism_Taxonomic_ID", "name", "Accn")], by="Accn")
HRaptor <- HRaptorRaw %>% filter(sca >= 100 & scd >= 100)%>%
  group_by(Organism_Taxonomic_ID) %>%
  slice_max(scd) %>%
  slice_max(evd)%>%
  ungroup
HRaptor <-HRaptor %>% left_join(select(Taxonomic_Information, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
HRaptor <- HRaptor %>% mutate(Super.Group = "Stramenopiles") %>% mutate(Source = "JGI")
HRaptor <- HRaptor %>% relocate(Super.Group)%>% rename(Organism_Name = "name")

HTorRaw <- merge(HTorRaw,Phyco_Information[c("Organism_Taxonomic_ID", "name", "Accn")], by="Accn")
HTor <- HTorRaw %>% filter(sca >= 100 & scd >= 100)%>%
  group_by(Organism_Taxonomic_ID) %>%
  slice_max(scd) %>%
  slice_max(evd)%>%
  ungroup
HTor <-HTor %>% left_join(select(Taxonomic_Information, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
HTor <- HTor %>% mutate(Super.Group = "Stramenopiles") %>% mutate(Source = "JGI")
HTor <- HTor %>% relocate(Super.Group)%>% rename(Organism_Name = "name")



HLST8Raw <- merge(HLST8Raw,Phyco_Information[c("Organism_Taxonomic_ID", "name", "Accn")], by="Accn")
HLST8 <- HLST8Raw %>% filter(sca >= 100 & scd >= 100)%>%
  group_by(Organism_Taxonomic_ID) %>%
  slice_max(scd) %>%
  slice_max(evd)%>%
  ungroup
HLST8 <-HLST8 %>% left_join(select(Taxonomic_Information, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
HLST8 <- HLST8 %>% mutate(Super.Group = "Stramenopiles") %>% mutate(Source = "JGI")
HLST8 <- HLST8 %>% relocate(Super.Group) %>% rename(Organism_Name = "name")


HSIN1Raw <- merge(HSIN1Raw,Phyco_Information[c("Organism_Taxonomic_ID", "name", "Accn")], by="Accn")
HSIN1 <- HSIN1Raw %>% filter(sca >= 100 & scd >= 100)%>%
  group_by(Organism_Taxonomic_ID) %>%
  slice_max(scd) %>%
  slice_max(evd)%>%
  ungroup
HSIN1 <-HSIN1 %>% left_join(select(Taxonomic_Information, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
HSIN1 <- HSIN1 %>% mutate(Super.Group = "Stramenopiles") %>% mutate(Source = "JGI")
HSIN1 <- HSIN1 %>% relocate(Super.Group)%>% rename(Organism_Name = "name")

HD1Raw <- merge(HD1Raw,Phyco_Information[c("Organism_Taxonomic_ID", "name", "Accn")], by="Accn")
HD1 <- HD1Raw %>% filter(sca >= 100 & scd >= 100)%>%
  group_by(Organism_Taxonomic_ID) %>%
  slice_max(scd) %>%
  slice_max(evd)%>%
  ungroup
HD1 <-HD1 %>% left_join(select(Taxonomic_Information, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
HD1 <- HD1 %>% mutate(Super.Group = "Stramenopiles") %>% mutate(Source = "JGI")
HD1 <- HD1 %>% relocate(Super.Group)%>% rename(Organism_Name = "name")



ChRAPTOR <- JoinInfo(ChRaptorRaw,"Chlorophyta",Phyco_Information,Taxonomic_Information)
ChRICTOR <- JoinInfo(ChRictorRaw, "Chlorophyta", Phyco_Information, Taxonomic_Information)
ChLST8 <- JoinInfo(ChLST8Raw, "Chlorophyta", Phyco_Information, Taxonomic_Information)
ChSIN1 <- JoinInfo(ChSIN1Raw, "Chlorophyta", Phyco_Information, Taxonomic_Information)
ChTor <- JoinInfo(ChTorRaw, "Chlorophyta", Phyco_Information, Taxonomic_Information)
ChD1 <- JoinInfo(ChD1Raw, "Chlorophyta", Phyco_Information, Taxonomic_Information)

RhizRAPTOR <- JoinInfo(RhizRAPTORRaw,"Rhizaria",Phyco_Information,Taxonomic_Information)
RhizRICTOR <- JoinInfo(RhizRICTORRaw, "Rhizaria", Phyco_Information,Taxonomic_Information)
RhizLST8 <- JoinInfo(RhizLST8Raw, "Rhizaria", Phyco_Information,Taxonomic_Information)
RhizTOR <- JoinInfo(RhizTORRaw, "Rhizaria", Phyco_Information,Taxonomic_Information)
RhizSIN1 <- JoinInfo(RhizSIN1Raw, "Rhizaria", Phyco_Information,Taxonomic_Information)
RhizD1 <- JoinInfo(RhizD1Raw, "Rhizaria", Phyco_Information,Taxonomic_Information)

ExRAPTOR <- JoinInfo(ExRAPTORRaw,"Excavata",Phyco_Information,Taxonomic_Information)
ExRICTOR <- JoinInfo(ExRICTORRaw,"Excavata",Phyco_Information,Taxonomic_Information)
ExLST8 <- JoinInfo(ExLST8Raw,"Excavata",Phyco_Information,Taxonomic_Information)
ExTOR <- JoinInfo(ExTORRaw,"Excavata",Phyco_Information,Taxonomic_Information)
ExSIN1 <- JoinInfo(ExSIN1Raw,"Excavata",Phyco_Information,Taxonomic_Information)
ExD1 <- JoinInfo(ExD1Raw,"Excavata",Phyco_Information,Taxonomic_Information)







CombinedHeterokonta <- mainCombine('Stramenopiles',Heterok_Information,HRictor,HSIN1,HRaptor,HLST8,HTor,HD1)
CombinedChlorophytaJGI <- mainCombine('Chlorophyta', ChlorophytaJGI_Information, ChRICTOR, ChSIN1, ChRAPTOR, ChLST8, ChTor, ChD1)
CombinedRhizariaJGI <- mainCombine("Rhizaria",RhizariaJGI_Information, RhizRICTOR,RhizSIN1,RhizRAPTOR,RhizLST8,RhizTOR,RhizD1)
CombinedExcavataJGI <- mainCombine("Excavata", ExcavataJGI_Information, ExRICTOR, ExSIN1, ExRAPTOR, ExLST8, ExTOR, ExD1)
#Here is where I would add in the BUSCO data
#Just finished here with the Excavata. Now I need to do the filtering and the joining

#Now I need to remove anything that has na in all! columns of SIN1 etc
# This would be columns 5 through 10
CombinedHeterokonta <- CombinedHeterokonta %>% filter(!if_all(5:10, is.na))
CombinedHeterokonta <- CombinedHeterokonta %>% left_join(select(Taxonomic_Information, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
CombinedHeterokonta <- CombinedHeterokonta %>% rename(Organism.Name = "Organism_Name")%>%mutate(Source = "JGI")
CombinedHeterokonta$Accn <- sub("\\_.*", "", CombinedHeterokonta$Accn)


CombinedChlorophytaJGI <- CombinedChlorophytaJGI %>% filter(!if_all(5:10, is.na))
CombinedChlorophytaJGI <- CombinedChlorophytaJGI %>% left_join(select(Taxonomic_Information, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
CombinedChlorophytaJGI <- CombinedChlorophytaJGI %>% rename(Organism.Name = "Organism_Name")%>%mutate(Source = "JGI")
CombinedChlorophytaJGI$Accn <- sub("\\_.*", "", CombinedChlorophytaJGI$Accn)

CombinedRhizariaJGI <- CombinedRhizariaJGI %>% filter(!if_all(5:10, is.na))
CombinedRhizariaJGI <- CombinedRhizariaJGI %>% left_join(select(Taxonomic_Information, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
CombinedRhizariaJGI <- CombinedRhizariaJGI %>% rename(Organism.Name = "Organism_Name")%>%mutate(Source = "JGI")
CombinedRhizariaJGI$Accn <- sub("\\_.*", "", CombinedRhizariaJGI$Accn)

CombinedExcavataJGI <- CombinedExcavataJGI %>% filter(!if_all(5:10, is.na))
CombinedExcavataJGI <- CombinedExcavataJGI %>%left_join(select(Taxonomic_Information, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
CombinedExcavataJGI <- CombinedExcavataJGI %>% rename(Organism.Name = "Organism_Name")%>%mutate(Source = "JGI")
CombinedExcavataJGI$Accn <- sub("\\_.*", "", CombinedExcavataJGI$Accn)





#Stramenopiles
TestMerge <- rbind(CombinedStramenopiles, CombinedHeterokonta)
TestMerge <- distinct(TestMerge, Organism_Taxonomic_ID, .keep_all = TRUE)
TestMerge <- left_join(TestMerge, StramenopileBusco, by = "Accn")
TestMerge$C.score <- as.numeric(sub("%", "", TestMerge$C.score))
TestMerge$Frag.score <- as.numeric(sub("%", "", TestMerge$Frag.score))

#Chlorophyta
TestMerge2 <- rbind(CombinedChlorophyta, CombinedChlorophytaJGI)
TestMerge2 <- distinct(TestMerge2, Organism_Taxonomic_ID, .keep_all = TRUE)
TestMerge2 <- left_join(TestMerge2, ChlorophytaBusco, by = "Accn")
TestMerge2$C.score <- as.numeric(sub("%", "", TestMerge2$C.score))
TestMerge2$Frag.score <- as.numeric(sub("%", "", TestMerge2$Frag.score))

TestMerge3 <- rbind(CombinedRhizaria, CombinedRhizariaJGI)
TestMerge3 <- distinct(TestMerge3, Organism_Taxonomic_ID, .keep_all = TRUE)
TestMerge3 <- left_join(TestMerge3, RhizariaBusco, by = "Accn")
TestMerge3$C.score <- as.numeric(sub("%", "", TestMerge3$C.score))
TestMerge3$Frag.score <- as.numeric(sub("%", "", TestMerge3$Frag.score))

TestMerge4 <- rbind(CombinedExcavates, CombinedExcavataJGI)
TestMerge4 <- distinct(TestMerge4, Organism_Taxonomic_ID, .keep_all = TRUE)
TestMerge4 <- left_join(TestMerge4, ExcavataBusco, by = "Accn")
TestMerge4$C.score <- as.numeric(sub("%", "", TestMerge4$C.score))
TestMerge4$Frag.score <- as.numeric(sub("%", "", TestMerge4$Frag.score))

write.csv(TestMerge, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Updated_Stramenopiles_Combined.csv")
write.csv(TestMerge2, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Updated_Chlorophyta_Combined.csv")
write.csv(TestMerge3, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Updated_Rhizaria_Combined.csv")
write.csv(TestMerge4, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Updated_Excavata_Combined.csv")

# Don't forget I also have to make the "combined tables with the H/M/L
# Do so here with a function call and any other combined things
# To do in another script:
# Read in all of the combined Data and add the following
# Add in a source of the data (JGI vs NCBI)
# Change any needed names to be more fitting/uniform
# Clean up any extraneous issues as needed


#Grab some Taxonomy Trees
#Need the ID numbers from each of the combined dataframes

# write.table(TestMerge$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Stramenopiles.txt", sep = "\t", row.names = F, col.names = F)
# write.table(TestMerge2$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Chlorophyta.txt", sep = "\t", row.names = F, col.names = F)
# write.table(TestMerge3$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Rhizaria.txt", sep = "\t", row.names = F, col.names = F)
# write.table(TestMerge4$Organism_Taxonomic_ID, file = "~/GitHub/TOR_phylogenetics/IDs/Excavata.txt", sep = "\t", row.names = F, col.names = F)






