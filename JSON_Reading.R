install.packages("rjson")
install.packages("jsonlite")
install.packages("RJSONIO")

library(tidyverse)
library(jsonlite)

sample <- read_json(path = "C:/Users/kajoh/Desktop/Powers_Lab_Project/BUSCO_SCORES/Stramenopiles/BUSCO_Aphanomyces_invadans_protein.faa/short_summary.specific.stramenopiles_odb10.BUSCO_Aphanomyces_invadans_protein.faa.json", flatten = TRUE)
name <- basename("C:/Users/kajoh/Desktop/Powers_Lab_Project/BUSCO_SCORES/Stramenopiles/BUSCO_Aphanomyces_invadans_protein.faa/short_summary.specific.stramenopiles_odb10.BUSCO_Aphanomyces_invadans_protein.faa.json")
df <- do.call(rbind, sample[4])
