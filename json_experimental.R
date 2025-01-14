library(tidyverse)
library(jsonlite)
library(purrr)
library(rjson)


# Read in jsons
# Read in the path for everything in a directory

files = dir("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Streptophyta_Jsons", pattern = "*.json")
files = paste0("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Streptophyta_Jsons/", files)
jsonList = lapply(files, fromJSON)


# from the list of lists, grab the parameters list and assign it to a var 
parameters <- lapply(jsonList, `[[`, 'parameters')
final <- lapply(jsonList, `[[`, 'results')
# Cast the values within the list as a dataframe to a dataframe variable
parameterdf <- as.data.frame(do.call(cbind, parameters))
df <- as.data.frame(do.call(cbind, final))

# Re-orient the dataframes from a longer to a wider (more columns as opposed to more rows)
df <- df %>%
  tibble::rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value)

parameterdf <- parameterdf %>%
  tibble::rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value)

# Change the values within the name column in the primary dataframe with values
# from the parameter dataframe

df$name <- parameterdf$out
# coerce the data and replace lists as characters. This is to write to csv output
dfinal <- data.frame(lapply(df, as.character))
dfinal <- rename(dfinal, "Frag.score" = Frag.score)
dfinal <- rename(dfinal, 'C.score' = Complete.percentage)
dfinal <- rename(dfinal, 'Accession' = name)
dfinal <- rename(dfinal, "Accn" = Accession)





write_csv(dfinal, file="C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Streptophyta_Busco.csv")
write.table(df, file="C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Streptophyta_Busco.csv")


