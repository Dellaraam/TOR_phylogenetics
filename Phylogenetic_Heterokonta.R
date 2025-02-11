

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
  }else{}
  
  
  # Finally return the completed dataframe   
  return(returnDF)  
}
#-------------------------------------------------------------------------------
# Ultimately we will need to create a list of all of the papers required
# This section is dealing with Heterokonta so use appropriately



#Read in combined Data for later
CombinedStramenopiles <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/Combined_Stramenopile.csv")
#remove any X columns that get made for some reason(figure out why)
CombinedStramenopiles <- select(CombinedStramenopiles, -X, -C.score, -Frag.score)
CombinedStramenopiles <- mutate(CombinedStramenopiles, Source = "NCBI")


# Read in the following information: RICTOR for heterokonts and the Heterokonta information packet (different than the ncbi version)
HRictorRaw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_RICTOR.csv")
HRaptorRaw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_RAPTOR.csv")
HTorRaw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_TOR.csv")
HLST8Raw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_LST8.csv")
HSIN1Raw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_SIN1.csv")
HD1Raw <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Stramenopiles/Heterokonts_D1.csv")



# Information from JGI that is needed to tie the data collected to the ncbi information
Phyco_Information <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Phycocosm_Information.csv")
Phyco_Information <- Phyco_Information %>% rename(Accn = "portal", Organism_Taxonomic_ID = "NCBI.Taxon")

Heterok_Information <- read.csv(file="~/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Information.csv")
Heterok_Information <- Heterok_Information %>% rename(Accn = "portal", Organism_Taxonomic_ID = "NCBI.Taxon", Organism_Name = "name")


Taxonomic_Information <- read.csv(file="~/GitHub/TOR_phylogenetics/Combined_Taxonomy.csv")
# Now we need to get rid of everything in the Accn number column after a "_"
# This code block removes anything past a "_" and replaces it with nothing
HRictorRaw$Accn<- sub("\\_.*", "", HRictorRaw$Accn)
HRaptorRaw$Accn<- sub("\\_.*", "", HRaptorRaw$Accn)
HTorRaw$Accn<- sub("\\_.*", "", HTorRaw$Accn)
HLST8Raw$Accn<- sub("\\_.*", "", HLST8Raw$Accn)
HSIN1Raw$Accn<- sub("\\_.*", "", HSIN1Raw$Accn)
HD1Raw$Accn<- sub("\\_.*", "", HD1Raw$Accn)



Phyco_Information$Accn <- sub("\\_.*", "", Phyco_Information$Accn)


# Now we can attempt to combine the information
# Future note: Need to fix the target information. We need it to be able to reproduce the data
# As it stands, the values should be able to quickly identify but it would be needed soon


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


CombinedHeterokonta <- mainCombine('Stramenopiles',Heterok_Information,HRictor,HSIN1,HRaptor,HLST8,HTor,HD1)
#Here is where I would add in the BUSCO data


#Now I need to remove anything that has na in all! columns of SIN1 etc
# This would be columns 5 through 10
CombinedHeterokonta <- CombinedHeterokonta %>% filter(!if_all(5:10, is.na))
CombinedHeterokonta <- CombinedHeterokonta %>% left_join(select(Taxonomic_Information, `Class.name`,`Phylum.name`,`Order.name`, `Family.name`, `Genus.name`, Organism_Taxonomic_ID), by=c("Organism_Taxonomic_ID"))
CombinedHeterokonta <- CombinedHeterokonta %>% rename(Organism.Name = "Organism_Name")%>%mutate(Source = "JGI")

#Need to add in if it was from JGI or from NCBI
#Create a column named source

TestMerge <- rbind(CombinedStramenopiles,CombinedHeterokonta)


# Don't forget I also have to make the "combined tables with the H/M/L
# Do so here with a function call and any other combined things






