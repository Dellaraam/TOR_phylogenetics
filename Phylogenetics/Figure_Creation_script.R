
# Figure Creation Script: Plots
#source(file = ")


if (!requireNamespace("devtools", quietly=TRUE))
  install.packages("devtools")
devtools::install_github("YuLab-SMU/ggmsa")

library(ggmsa)
library(tidyverse)
library(ggrepel)

# ------------------------------------------------------------------------------
#Test Ground

largeDataSet <- left_join(HTML, Ndf[c("SIN1All","SIN1Domain","RICTORAll","RICTORDomain","RAPTORAll","RAPTORDomain","TORAll","TORDomain","LST8All","LST8Domain","Organism_Taxonomic_ID")], by = "Organism_Taxonomic_ID")
summaryData <- summarise(largeDataSet)



jitterRICTOR<- largeDataSet %>%filter(!is.na(RICTOR) & RICTOR != "P") %>% ggplot(aes(x = factor(RICTOR,level = c("H", "M", "L")), y = RICTORDomain, color = Super.Group, size = C.score))+
  geom_jitter(shape = 18)+
  labs(title = "RICTOR Domain Score Distribution")+
  xlab(label = "RICTOR H/M/L")+
  ylab(label = "RICTOR Domain Scores")+
  #geom_text_repel(aes(label = Organism.Name), size = 2.2, show.legend = FALSE)+
  facet_wrap(~Super.Group,nrow = 1)
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/JitterRICTOR.png",
       plot = jitterRICTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)




jitterRAPTOR<- largeDataSet %>%filter(!is.na(RAPTOR) & RAPTOR != "P") %>% ggplot(aes(x = factor(RAPTOR,level = c("H", "M", "L")), y = RAPTORDomain, color = Super.Group, size = C.score))+
  geom_jitter(shape = 18)+
  labs(title = "RAPTOR Domain Score Distribution")+
  xlab(label = "RAPTOR H/M/L")+
  ylab(label = "RAPTOR Domain Scores")+
  facet_wrap(~Super.Group)
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/JitterRAPTOR.png",
       plot = jitterRAPTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)



jitterSIN1 <- largeDataSet %>%filter(!is.na(SIN1) & SIN1 != "P") %>% ggplot(aes(x = factor(SIN1, level = c("H", "M", "L")), y = SIN1Domain, color = Super.Group, size = C.score))+
  geom_jitter(shape = 18)+
  labs(title = "SIN1 Domain Score Distribution")+
  xlab(label = "SIN1 H/M/L")+
  ylab(label = "SIN1 Domain Scores")+
  facet_wrap(~Super.Group)
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/JitterSIN1.png",
       plot = jitterSIN1,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

jitterLST8 <- largeDataSet %>%filter(!is.na(LST8) & LST8 != "P") %>% ggplot(aes(x = factor(LST8, level = c("H", "M", "L")), y = LST8Domain, color = Super.Group, size = C.score))+
  geom_jitter(shape = 18)+
  labs(title = "LST8 Domain Score Distribution")+
  xlab(label = "LST8 H/M/L")+
  ylab(label = "LST8 Domain Scores")+
  facet_wrap(~Super.Group)
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/JitterLST8.png",
       plot = jitterLST8,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

jitterTOR <- largeDataSet %>%filter(!is.na(TOR) & TOR != "P") %>% ggplot(aes(x = factor(TOR, level = c("H", "M", "L")), y = TORDomain, color = Super.Group, size = C.score))+
  geom_jitter(shape = 18)+
  labs(title = "TOR Domain Score Distribution")+
  xlab(label = "TOR H/M/L")+
  ylab(label = "TOR Domain Scores")+
  facet_wrap(~Super.Group)
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/JitterTOR.png",
       plot = jitterTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)





BoxRictor <- largeDataSet %>% ggplot(aes(x = Super.Group, y = RICTORDomain))+
  geom_boxplot(linetype = "dashed", outlier.shape = NA)+
  #geom_jitter()+
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..), outlier.shape = 1)+
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..))+
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..))+
  labs(title = "Figure N")+
  xlab(label = "Super Groups")+
  ylab(label = "RICTOR Domain Score")+
  theme_classic()

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxRictor.png",
       plot = BoxRictor,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

BoxRaptor <- largeDataSet %>% ggplot(aes(x = Super.Group, y = RAPTORDomain))+
  geom_boxplot(linetype = "dashed", outlier.shape = NA)+
  #geom_jitter()+
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..))+
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..))+
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..))+
  labs(title = "Figure N")+
  xlab(label = "Super Groups")+
  ylab(label = "RAPTOR Domain Score")+
  theme_classic()
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxRaptor.png",
       plot = BoxRaptor,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)


BoxTOR <- largeDataSet %>% ggplot(aes(x = Super.Group, y = TORDomain))+
  geom_boxplot(linetype = "dashed", outlier.shape = NA)+
  #geom_jitter()+
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..))+
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..))+
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..))+
  labs(title = "Figure N")+
  xlab(label = "Super Groups")+
  ylab(label = "TOR Domain Score")+
  theme_classic()
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxTOR.png",
       plot = BoxTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

BoxSIN1 <- largeDataSet %>% ggplot(aes(x = Super.Group, y = SIN1Domain))+
  geom_boxplot(linetype = "dashed", outlier.shape = NA)+
  #geom_jitter()+
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..))+
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..))+
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..))+
  labs(title = "Figure N")+
  xlab(label = "Super Groups")+
  ylab(label = "SIN1 Domain Score")+
  theme_classic()
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxSIN1.png",
       plot = BoxSIN1,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)


BoxLST8 <- largeDataSet %>% ggplot(aes(x = Super.Group, y = LST8Domain))+
  geom_boxplot(linetype = "dashed", outlier.shape = NA)+
  #geom_jitter()+
  stat_boxplot(aes(ymin = ..lower.., ymax = ..upper..))+
  stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..))+
  stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..))+
  labs(title = "Figure N")+
  xlab(label = "Super Groups")+
  ylab(label = "LST8 Domain Score")+
  theme_classic()
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxLST8.png",
       plot = BoxLST8,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)



# ------------------------------------------------------------------------------
