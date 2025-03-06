library(tidyverse)



proteinPossible <- function(df,OrganismName,Protein){
  
  temp <- df %>% filter(Organism.Name == OrganismName)
  df <- subset(df, Organism.Name != OrganismName)
  temp[,Protein] <-"P"
  df <- rbind(df,temp)
  return(df)
}

proteinPossibleSwap <- function(dfNo,dfYes,OrganismName, Protein){
  
  temp<- dfNo %>% filter(Organism.Name == OrganismName)
  temp[,Protein] <-"P"
  dfYes <- rbind(dfYes, temp)
  return(dfYes)
}


Taxon <- read.csv("~/Github/TOR_phylogenetics/Combined_Taxonomy.csv")
Taxon <- rename(Taxon, Organism.Name = "Tax.name")
HTML <- read.csv("~/Github/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/New_Combined_Table_218.csv")
HTML <- select(HTML, -Organism.Name, -X, -D1)
HTML <- left_join(HTML, Taxon[c("Organism.Name", "Organism_Taxonomic_ID")], by = "Organism_Taxonomic_ID")
HTML <- relocate(HTML, Organism.Name, .after = Organism_Taxonomic_ID)





# Add in the possible protein data here
# Will come up with a more elegant solution in the future
# Using results gather by Dell and her Diamond Analysis work

HTML <- HTML %>%
  proteinPossible("Effrenium voratum","RAPTOR")%>%
  proteinPossible("Effrenium voratum","LST8") %>%
  proteinPossible("Durusdinium trenchii","RAPTOR")%>%
  proteinPossible("Paramecium primaurelia","RICTOR")%>%
  proteinPossible("Bonamia ostreae","RAPTOR")%>%
  proteinPossible("Lotharella oceanica","RICTOR")%>%
  proteinPossible("Lotharella oceanica","RAPTOR")%>%
  proteinPossible("Lotharella oceanica","TOR")%>%
  proteinPossible("Lotharella oceanica","LST8")%>%
  proteinPossible("Paulinella micropora","RAPTOR")%>%
  proteinPossible("Paulinella micropora","TOR")%>%
  proteinPossible("Paulinella micropora","LST8")%>%
  proteinPossible("Paulinella micropora","TOR")%>%
  proteinPossible("Tetradesmus obliquus","RAPTOR")%>%
  proteinPossible("Pseudo-nitzschia multistriata","LST8")%>%
  proteinPossible("Triticum urartu","RAPTOR")%>%
  proteinPossible("Bienertia sinuspersici","RAPTOR") %>%
  proteinPossible("Nannochloropsis gaditana","RICTOR")%>%
  proteinPossible("Phytophthora megakarya","RICTOR")%>%
  proteinPossible("Phytophthora megakarya","LST8")%>%
  proteinPossible("Euglena gracilis","RICTOR")%>%
  proteinPossible("Nannochloropsis gaditana CCMP526","RICTOR")%>%
  proteinPossible("Monocercomonoides exilis","RICTOR")%>%
  proteinPossible("Nelumbo nucifera", "LST8")%>%
  proteinPossible("Nelumbo nucifera","RAPTOR")%>%
  proteinPossible("Nelumbo nucifera", "TOR")%>%
  proteinPossible("Apium graveolens", "RAPTOR")%>%
  proteinPossible("Apium graveolens","LST8")%>%
  proteinPossible("Apium graveolens","TOR")%>%
  proteinPossible("Babesia caballi","TOR")%>%
  proteinPossible("Ceratopteris richardii","LST8")%>%
  proteinPossible("Stephania cephalantha","LST8")%>%
  proteinPossible("Morus notabilis","LST8")%>%
  proteinPossible("Capsicum chinense","LST8")%>%
  proteinPossible("Carex littledalei","LST8")%>%
  proteinPossible("Gossypium klotzschianum","LST8")


# Modifications to some of the organism names and also the supergroups
which(HTML$Organism.Name == "Chlamydomonas reinhardtii", arr.ind = TRUE)
which(HTML$Organism.Name == "Neoyropia yezoensis", arr.ind = TRUE)
which(HTML$Organism.Name == "Euglena gracilis", arr.ind = TRUE)
which(HTML$Organism.Name == "Giardia lamblia ATCC 50803")
HTML$Super.Group[801] <- "Discoba"
HTML$Organism.Name[801]
HTML$Super.Group[696] <- "Metamonada"
HTML$Organism.Name[696]
HTML$Organism.Name[496] <- "Pyropia yezoensis"
HTML$Organism.Name[496] 
HTML$SIN1[697] <- NA
HTML$SIN1[697]
#Look into aphanomyces for RICTOR. What can we conclude about it?

write.table(HTML, file = "~/GitHub/TOR_phylogenetics/IDs/HTML.tsv", sep = "\t")
