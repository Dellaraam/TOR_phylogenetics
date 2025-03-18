

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Biostrings")


install.packages("bio3d")

library(bio3d)
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
library(msa)


sequence <-read.fasta("C:/Users/kajoh/Desktop/Stramenopile_NCBI_JGI/MSA/StramenopileRictorClustalO.fa")
seq2 <- read.fasta("C:/Users/kajoh/Desktop/Stramenopile_NCBI_JGI/MSA/StramenopileRaptorClustalO.fa")

Identity <- seqidentity(sequence)
id2 <- seqidentity(seq2)
mean(Identity)
mean(id2)
