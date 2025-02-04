


if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ggtree")
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Biostrings")
library("Biostrings")
library(tidyverse)
library(ggplot2)
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

install.packages("tableHTML")
library(tableHTML)

 install.packages("reshape")
library(reshape)

 install.packages("useful")
library(useful)


# Read in the numeric table

Ndf <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Combined_CSVs/NumericTable.csv")
Shio <- read.csv("~/GitHub/TOR_phylogenetics/GitHub_CSV/Copy of Comparison_table_Shiozaki_collected_data_Johnson_collected_data_v3 - Sheet1.csv")
Shio <- Shio[-9,]
Shio <- Shio[-97,]





Ndf %>%
  group_by(Super.Group) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  summarise(mean_score_RICTOR = mean(RICTOR_scd))

Ndf %>%
  group_by(Super.Group) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  summarise(mean_score_RICTOR = mean(RICTOR_scd)) %>%
  ggplot()+
  geom_col(aes(x = Super.Group, y = mean_score_RICTOR, fill = Super.Group))



Ndf %>%
  group_by(Super.Group) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  ggplot()+
  geom_