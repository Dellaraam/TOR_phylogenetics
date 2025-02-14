library(tidyverse)
library(jsonlite)
library(purrr)

# Could maybe turn this into a function call
# Going to give it a shot
# Read in jsons
# Read in the path for everything in a directory
JsontoCSV <- function(DirectoryPath,OutputDirectory){
  
  # This section adds in a / to the directory ouput
  # This would be easier in python but R has a finicky way of handling
  # Adding things to strings overall
  
  #print("About to Load in")
  extra <- "/"
  vec <- cbind(DirectoryPath,extra)
  S <- paste(DirectoryPath,extra, sep="")
  M <- paste(vec,collapse = "")
  #print(M)
  
  
  
  # This section reads in the json files (just the file names at first)
  # It then adds in the full path name to the overall list with the paste function
  # It then creates a json list using the files and the fromJSON function from jsonlite
  
  #print("About to create files")
  files = dir(DirectoryPath, pattern = "*.json")
  files = paste0(M, files)
  #Use lapply to run the function fromJSON over the entire list of json files
  jsonList = lapply(files, fromJSON)
  #print("Finished Loading In")
  
  # Grabbing parameters, making final versions of things for a dataframe
  # from the list of lists, grab the parameters list and assign it to a var 
  # Also grab the results and apply that to a variable
  # Combine these two variables into dataframes that will later be combined
  # Into a final 
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
  dfinal <- rename(dfinal, "Frag.score" = Fragmented.percentage)
  dfinal <- rename(dfinal, 'C.score' = Complete.percentage)
  dfinal <- rename(dfinal, 'Accession' = name)
  dfinal <- rename(dfinal, "Accn" = Accession)
  
  
  write_csv(dfinal, file= OutputDirectory)
 }


# Now to just run this multiple times

JsontoCSV("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Streptophyta_Jsons/", "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/StreptophytaBusco.csv")



