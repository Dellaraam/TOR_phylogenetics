


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
Shio <- Shio[-96,]
Shio <- Shio[-95,]
Shio <- Shio[-94,]
Shio <- Shio[-93,]

Shio <- select(Shio, -c("Edge_case", "HMMER_Bias_warning", "Error_Warning"))
Shio <- select(Shio, -c("HMMER_Results_RAPTOR_Johnson", "HMMER_Results_RICTOR_Pianissimo_Johnson","HMMER_Results_SIN1_Johnson", "Alphafold_DB_Entry_SIN1"))
Shio <- select(Shio, -c("Alphafold_DB_Entry_RICTOR", "Alphafold_DB_Entry_RAPTOR"))



# Comparing the shiozaki results to my results
# Using a dual layered histogram method
Shio %>%
  group_by(Supergroup) %>%
  ggplot()+
  geom_histogram(aes(x = Johnson_SIN1_Results, fill = Clade), stat="count", position = "dodge")+
  geom_histogram(aes(x = Shiozaki_SIN1_Results, fill = Clade), alpha = .2, stat="count", position = "dodge")+
  theme_minimal()

Shio %>%
  group_by(Supergroup) %>%
  ggplot()+
  geom_histogram(aes(x = Johnson_RICTOR_Results, fill = Supergroup), stat="count", position = "dodge")+
  geom_histogram(aes(x = Shiozaki_RICTOR_Results, fill = Supergroup), alpha = .2, stat="count", position = "dodge")+
  theme_minimal()

Shio %>%
  group_by(Supergroup) %>%
  ggplot()+
  geom_histogram(aes(x = Johnson_RAPTOR_Results, fill = Supergroup), stat="count", position = "dodge")+
  geom_histogram(aes(x = Shiozaki_RAPTOR_Results, fill = Supergroup), alpha = .2, stat="count", position = "dodge")+
  theme_minimal()





Ndf %>%
  group_by(Super.Group) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  summarise(mean_score_C_score = mean(C.score), 
            median_score_C_score = median(C.score), 
            mean_score_RICTOR = mean(RICTOR_scd), 
            median_score_RICTOR = median(RICTOR_scd),
            mean_score_SIN1 = mean(SIN1_scd),
            median_score_SIN1 = median(SIN1_scd),
            mean_score_RAPTOR = mean(RAPTOR_scd),
            median_score_RAPTOR = median(RAPTOR_scd)) %>%
  ggplot()+
  geom_col(aes(x = Super.Group, y = mean_score_RICTOR, fill = median_score_C_score))

Ndf %>%
  group_by(Super.Group) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  ggplot()+
  geom_violin(aes(x = Super.Group, y = RAPTOR_sca, fill = Super.Group))

Ndf %>%
  group_by(Super.Group) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  ggplot()+
  geom_area(aes(x = RICTOR_scd, y = RICTOR_sca, fill = Super.Group))





# Count the number of elements in the columns minus the NAs
colSums(!is.na(Ndf))

Ndf %>%
  group_by(Super.Group) %>%
  summarize(mean_RICTOR = mean(RICTOR_scd, na.rm = TRUE),
            mean_SIN1 = mean(SIN1_scd, na.rm = TRUE),
            mean_RAPTOR = mean(RAPTOR_scd, na.rm = TRUE),
            mean_LST8 = mean(LST8_scd, na.rm = TRUE),
            mean_TOR = mean(TOR_scd, na.rm = TRUE)) %>%
  ggplot()+
  geom_col



# Summary function that just gives out the summary statistics. Less control
summary(Ndf)






Ndf %>%
  group_by(Super.Group) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  ggplot()+
  geom_count(aes(x = Super.Group, y = RICTOR_sca, color = Phylum.name))+
  theme_minimal()


# RICTOR
Ndf %>%
  group_by(Super.Group) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  ggplot()+
  geom_jitter(aes(x = RICTOR_sca, y = Super.Group, fill = Phylum.name, color = Phylum.name), size = 2)+
  xlab("Rictor Scores")+
  ylab("Super Groups")+
  theme_minimal()

Ndf %>%
  group_by(Super.Group) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  ggplot()+
  geom_jitter(aes(x = SIN1_sca, y = Super.Group, fill = Phylum.name, color = Phylum.name), size = 2)+
  xlab("SIN1 Scores")+
  ylab("Super Groups")+
  theme_minimal()

Ndf %>%
  group_by(Super.Group) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  ggplot()+
  geom_jitter(aes(x = RAPTOR_sca, y = Super.Group, fill = Phylum.name, color = Phylum.name), size = 2)+
  xlab("RAPTOR Scores")+
  ylab("Super Groups")+
  theme_minimal()

Ndf %>%
  group_by(Super.Group) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  ggplot()+
  geom_jitter(aes(x = LST8_sca, y = Super.Group, fill = Phylum.name, color = Phylum.name), size = 2)+
  xlab("LST8 Scores")+
  ylab("Super Groups")+
  theme_minimal()

Ndf %>%
  group_by(Super.Group) %>% mutate(across(where(is.numeric), ~replace_na(., 0)))%>%
  ggplot()+
  geom_jitter(aes(x = TOR_sca, y = Super.Group, fill = Phylum.name, color = Phylum.name), size = 2)+
  xlab("TOR Scores")+
  ylab("Super Groups")+
  theme_minimal()


