#-------------------------------------------------------------------------------
#Library and installing packages file

#Make sure to add in the commented out install packages here as necessary


install.packages("tidyverse")
install.packages("reshape2")
install.packages("ggtreeExtra")
install.packages("tableHTML")


if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ggtree")
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Biostrings")
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ggtreeExtra")


library("Biostrings")
library(tidyverse)
library(ggplot2)
library(reshape2)
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

library(ggtreeExtra)


library(tableHTML)