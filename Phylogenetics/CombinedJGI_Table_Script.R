#Merge the JGI Information together into one table



AlveolataJGI <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/AlveolataJGI_Information.csv")
StramenopilesJGI <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Heterokonta_Information.csv")
RhizariaJGI <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/RhizariaJGI_Information.csv")

ChlorophytaJGI <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/ChlorophytaJGI_Information.csv")
ExcavataJGI <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/ExcavatesJGI_Information.csv")


JGICombinedTable <- rbind(AlveolataJGI, StramenopilesJGI, RhizariaJGI, ChlorophytaJGI, ExcavataJGI)
JGICombinedTable <- JGICombinedTable %>% rename(Accn = "portal", Organism_Taxonomic_ID = "NCBI.Taxon", Organism.Name = "name")
JGICombinedTable$Accn <- sub("\\_.*", "", JGICombinedTable$Accn)


write.csv(JGICombinedTable, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/JGICombinedInfoTable.csv")
