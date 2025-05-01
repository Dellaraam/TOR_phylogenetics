#-------------------------------------------------------------------------------
#Library and installing packages file

#Make sure to add in the commented out install packages here as necessary
#Have citation requirements for their usage. See attached message
# LG Wang, TTY Lam, S Xu, Z Dai, L Zhou, T Feng, P Guo, CW Dunn, BR Jones, T Bradley, H Zhu, Y Guan, Y Jiang, G Yu. treeio: an R package for
# phylogenetic tree input and output with richly annotated and associated data. Molecular Biology and Evolution. 2020, 37(2):599-603. doi:
#  10.1093/molbev/msz240
#install.packages("paletteer")
#install.packages("dichromat")
#install.packages("tidytree")
#install.packages("ggnewscale")
#install.packages("tidyverse")
#install.packages("reshape2")
#install.packages("ggtreeExtra")
#install.packages("tableHTML")
#install.packages("ape")
#install.packages("remotes")
#install.packages("ggsci")
#install.packages("eoffice")
#install.packages("RRphylo")
#install.packages("treemapify")
#remotes::install_github("djw533/micro.gen.extra")
# https://rdrr.io/github/djw533/micro.gen.extra/

# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("treeio")
# 
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("ggtree")
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("Biostrings")
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("ggtreeExtra")

library(remotes)
library("Biostrings")
library(tidyverse)
library(ggplot2)
library(reshape2)
library(ggtree) 
library(treeio)
library(ggrepel)
library(gridExtra)
library(plotly)
library(kableExtra)
library(knitr)
library(patchwork)
library(xtable)
library(ape)
library(ggnewscale)
library(ggtreeExtra)
library(tidytree)
library(ggsci)
library(dichromat)
library(paletteer)
library(tableHTML)
library(eoffice)
library(RRphylo)
library(scales)
library(treemapify)
