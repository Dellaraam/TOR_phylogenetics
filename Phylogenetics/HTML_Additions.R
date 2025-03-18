library(tidyverse)



proteinPossible <- function(df,OrganismName,Protein,value){
  
  temp <- df %>% filter(Organism.Name == OrganismName)
  df <- subset(df, Organism.Name != OrganismName)
  temp[,Protein] <- value
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
HTML <- read.csv("~/Github/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/CompleteTable.csv")
HTML <- select(HTML, -Organism.Name, -D1) %>%
  rename(Organism_Taxonomic_ID = "Organism.Taxonomic.ID")
HTML <- left_join(HTML, Taxon[c("Organism.Name", "Organism_Taxonomic_ID")], by = "Organism_Taxonomic_ID")
HTML <- relocate(HTML, Organism.Name, .after = Organism_Taxonomic_ID)





# Add in the possible protein data here
# Will come up with a more elegant solution in the future
# Using results gather by Dell and her Diamond Analysis work

HTML <- HTML %>%
  proteinPossible("Effrenium voratum","RAPTOR","P")%>%
  proteinPossible("Effrenium voratum","LST8","P") %>%
  proteinPossible("Durusdinium trenchii","RAPTOR","P")%>%
  proteinPossible("Paramecium primaurelia","RICTOR","P")%>%
  proteinPossible("Bonamia ostreae","RAPTOR","P")%>%
  proteinPossible("Lotharella oceanica","RICTOR","P")%>%
  proteinPossible("Lotharella oceanica","RAPTOR","P")%>%
  proteinPossible("Lotharella oceanica","TOR","P")%>%
  proteinPossible("Lotharella oceanica","LST8","P")%>%
  proteinPossible("Paulinella micropora","RAPTOR","P")%>%
  proteinPossible("Paulinella micropora","TOR","P")%>%
  proteinPossible("Paulinella micropora","LST8","P")%>%
  proteinPossible("Paulinella micropora","TOR","P")%>%
  proteinPossible("Tetradesmus obliquus","RAPTOR","P")%>%
  proteinPossible("Pseudo-nitzschia multistriata","LST8","P")%>%
  proteinPossible("Triticum urartu","RAPTOR","P")%>%
  proteinPossible("Bienertia sinuspersici","RAPTOR","P") %>%
  proteinPossible("Nannochloropsis gaditana","RICTOR","P")%>%
  proteinPossible("Phytophthora megakarya","RICTOR","P")%>%
  proteinPossible("Phytophthora megakarya","LST8","P")%>%
  proteinPossible("Nannochloropsis gaditana CCMP526","RICTOR","P")%>%
  proteinPossible("Nannochloropsis gaditana CCMP526","LST8","P")%>%
  proteinPossible("Nelumbo nucifera", "LST8","P")%>%
  proteinPossible("Nelumbo nucifera","RAPTOR","P")%>%
  proteinPossible("Nelumbo nucifera", "TOR","P")%>%
  proteinPossible("Apium graveolens", "RAPTOR","P")%>%
  proteinPossible("Apium graveolens","LST8","P")%>%
  proteinPossible("Apium graveolens","TOR","P")%>%
  proteinPossible("Babesia caballi","TOR","P")%>%
  proteinPossible("Ceratopteris richardii","LST8","P")%>%
  proteinPossible("Stephania cephalantha","LST8","P")%>%
  proteinPossible("Morus notabilis","LST8","P")%>%
  proteinPossible("Capsicum chinense","LST8","P")%>%
  proteinPossible("Carex littledalei","LST8","P")%>%
  proteinPossible("Gossypium klotzschianum","LST8","P")%>%
  proteinPossible("Dichanthelium oligosanthes", "RAPTOR","P")%>%
  proteinPossible("Aphanomyces cochlioides", "RAPTOR","P")%>%
  proteinPossible("Trypanosoma congolense IL3000", "LST8","P")


#Make a for loop for the change of the excavates to the Discoba
#Use conditional statements to determine to find the specific names and then change the data


HTML$Organism.Name <- str_replace(HTML$Organism.Name, "Neopyropia yezoensis", "Pyropia yezoensis")


eug = which(HTML$Organism.Name == "Euglena gracilis")
gia = which(HTML$Organism.Name == "Giardia lamblia ATCC 50803")
HTML[eug, "Group"] <- "Discoba"
HTML[gia, "Group"] <- "Metamonada"

# Modifications to some of the organism names and also the supergroups

#Removing the SIN1 and RICTOR Hits as they are, as it turns out, false positives
#This was based upon BLAST results on the specific 
HTML <- HTML %>%
  proteinPossible("Cichorium endivia", "SIN1",NA) %>%
  proteinPossible("Cichorium endivia","RICTOR",NA)%>%
  proteinPossible("Salix suchowensis","SIN1",NA)%>%
  proteinPossible("Adiantum nelumboides","SIN1",NA)%>%
  proteinPossible("Adiantum nelumboides","RICTOR",NA)%>%
  proteinPossible("Persea americana","SIN1",NA)%>%
  proteinPossible("Sphagnum jensenii","RICTOR",NA)%>%
  proteinPossible("Carpinus fangiana","RICTOR",NA)%>%
  proteinPossible("Klebsormidium nitens","RICTOR",NA)
  
HTML <- HTML %>%
  filter(Organism_Taxonomic_ID != 1535356,Organism_Taxonomic_ID != 1535362,Organism_Taxonomic_ID != 2494336,Organism_Taxonomic_ID != 1055687,Organism_Taxonomic_ID != 1068625)

write.table(HTML, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/HTML.tsv", sep = "\t", row.names = F)

