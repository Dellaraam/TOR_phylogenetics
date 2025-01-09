# Phylogenetic Tree Experimentation
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")
alibrary(tidyverse)
library(ggplot2)
library(ggtree) #Have citation requirements for their usage. See attached message
# LG Wang, TTY Lam, S Xu, Z Dai, L Zhou, T Feng, P Guo, CW Dunn, BR Jones, T Bradley, H Zhu, Y Guan, Y Jiang, G Yu. treeio: an R package for
# phylogenetic tree input and output with richly annotated and associated data. Molecular Biology and Evolution. 2020, 37(2):599-603. doi:
#  10.1093/molbev/msz240

library(treeio)

tree <- read.tree('C:/Users/kajoh/Desktop/Verification_Proteins/TOR_File.nwk')
tempdata <- full_join(cleanedTORAlveolata, cleanedTORStramenopiles)
tempdata <- full_join(tempdata, cleanedTORRhizaria)
tree$tip.label[1]

for( i in 1:length(tree$tip.label)){
  print(tree$tip.label[i])
  tree$tip.label[i] <- trimws(tree$tip.label[i], whitespace = "/.*")
  print(tree$tip.label[i])
  for(j in 1:nrow(tempdata)){
    print(i)
    print(j)
    if(tree$tip.label[i] == tempdata$tar[j]){
      tree$tip.label[i] <- tempdata$Organism.Name[j]
    }
  }
  
}
tree$tip.label

# Would need to get the complete list of target proteins and have them readily available along with the scientific names
# Something like the following commands:
# for i in 1:nrow(list$label)
# for j in sar dataframe
  # if the list$label == sar$tar
  # list$label[i] <- sar$name[j]


ggtree(tree, layout = "circular") + geom_tiplab(aes(angle=angle), size = 2, color = "blue")




