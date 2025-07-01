
# Figure Creation Script: Plots
# Kyle Johnson
# Dellaraam Pourkeramati
# Script for creating different plots and charts for metabolic strategy


source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Library_Script.R")


# ------------------------------------------------------------------------------
MasterTable <- read_csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Master_Table.csv")
MasterTable <- MasterTable %>% select(-...1)
MasterTable <- MasterTable %>%
  mutate(SIN1Domain = if_else(Super.Group == "Chlorophyta",NA,SIN1Domain),
         SIN1All = if_else(Super.Group == "Chlorophyta", NA, SIN1All),
         RICTORDomain = if_else(Super.Group == "Chlorophyta", NA, RICTORDomain),
         RICTORAll = if_else(Super.Group == "Chlorophyta", NA, RICTORAll))


MasterTable <- MasterTable %>% mutate(M.Strategy = if_else(Phylum.name == "Apicomplexa", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                       M.Strategy = if_else(Phylum.name == "Perkinsozoa", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                       M.Strategy = if_else(Super.Group == "Streptophyta" & M.Strategy == "Parasite", "Streptophyta parasite", M.Strategy, missing = M.Strategy),
                       M.Strategy = if_else(Family.name == "Amoebophryaceae", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                       M.Strategy = if_else(M.Strategy == "Parasite" & Super.Group != "Streptophyta", "Non-Plastid Parasite", M.Strategy, missing = M.Strategy))


MasterTable %>% filter(M.Strategy == "Parasite")%>% view()
MasterTable %>% filter(is.na(RAPTOR) & M.Strategy != "Plastid Parasite") %>% view()
MasterTable %>% filter(is.na(RICTOR) & M.Strategy == "Heterotroph") %>% view()
MasterTable %>% filter(is.na(RAPTOR) & M.Strategy == "Non-Plastid Parasite")%>%view()



MasterTable %>% group_by(Super.Group)%>%
  mutate(count = n())%>% ungroup()%>%
  view()


# Add to this palette:
# Non-Plastid Parasite
# Plastid Parasite


EMpal2 <- c(
  "Autotrophic" = "#3CBA26",
  "Endosymbiotic" = "purple",
  "Heterotroph" = "#A87142",
  "Mixotroph" = "#63E3C5",
  "Non-Plastid Parasite" = "#FE4A49",
  "Plastid Parasite" = "#D0D1AC",
  "Streptophyta Parasite" = "#B6A39E"
)















jitterRICTOR<- MasterTable %>%group_by(Super.Group)%>%
  mutate(count = n())%>% ungroup()%>%filter(!is.na(RICTOR) & RICTOR != "P") %>% ggplot(aes(x = factor(RICTOR,level = c("H", "M", "L")), y = RICTORDomain))+
  geom_jitter(shape = 18, size = 2)+
  labs(title = "RICTOR Domain Score Distribution")+
  xlab(label = "RICTOR H/M/L")+
  ylab(label = "RICTOR Domain Scores")+
  #geom_text_repel(aes(label = Organism.Name), size = 2.2, show.legend = FALSE)+
  facet_wrap(~Super.Group,nrow = 1)+
  geom_text( aes(label = paste("N:",count), y = Inf, x  = -Inf), vjust = 1, hjust = 0)+
  theme_bw()
jitterRICTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/JitterRICTOR.png",
       plot = jitterRICTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Jitter_Plot_Rictor_Domain.pptx",
       figure = jitterRICTOR,
       units = "inches",
       width = 10,
       height = 7)

jitterRICTOR<- MasterTable %>%filter(!is.na(RICTOR) & RICTOR != "P") %>% ggplot(aes(x = factor(RICTOR,level = c("H", "M", "L")), y = RICTORAll))+
  geom_jitter(shape = 18, size = 2)+
  labs(title = "RICTOR Overall Score Distribution")+
  xlab(label = "RICTOR H/M/L")+
  ylab(label = "RICTOR Overall Scores")+
  #geom_text_repel(aes(label = Organism.Name), size = 2.2, show.legend = FALSE)+
  facet_wrap(~Super.Group,nrow = 1)+
  theme_bw()
jitterRICTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/JitterRICTORAll.png",
       plot = jitterRICTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)
topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Jitter_Plot_Rictor_All.pptx",
       figure = jitterRICTOR,
       units = "inches",
       width = 10,
       height = 7)









jitterRAPTOR<- MasterTable %>%filter(!is.na(RAPTOR) & RAPTOR != "P") %>% ggplot(aes(x = factor(RAPTOR,level = c("H", "M", "L")), y = RAPTORDomain))+
  geom_jitter(shape = 18, size = 2)+
  labs(title = "RAPTOR Domain Score Distribution")+
  xlab(label = "RAPTOR H/M/L")+
  ylab(label = "RAPTOR Domain Scores")+
  facet_wrap(~Super.Group)+
  theme_bw()
jitterRAPTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/JitterRAPTOR.png",
       plot = jitterRAPTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)
topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Jitter_Plot_Raptor_Domain.pptx",
       figure = jitterRAPTOR,
       units = "inches",
       width = 10,
       height = 7)


jitterRAPTOR<- MasterTable %>%filter(!is.na(RAPTOR) & RAPTOR != "P") %>% ggplot(aes(x = factor(RAPTOR,level = c("H", "M", "L")), y = RAPTORAll))+
  geom_jitter(shape = 18, size = 2)+
  labs(title = "RAPTOR Overall Score Distribution")+
  xlab(label = "RAPTOR H/M/L")+
  ylab(label = "RAPTOR Overall Scores")+
  facet_wrap(~Super.Group)+
  theme_bw()
jitterRAPTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/JitterRAPTORAll.png",
       plot = jitterRAPTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Jitter_Plot_Raptor_All.pptx",
       figure = jitterRAPTOR,
       units = "inches",
       width = 10,
       height = 7)














jitterSIN1 <- MasterTable %>%filter(!is.na(SIN1) & SIN1 != "P" & Super.Group != "Rhizaria") %>% ggplot(aes(x = factor(SIN1, level = c("H", "M", "L")), y = SIN1Domain))+
  geom_jitter(shape = 18, size = 2)+
  labs(title = "SIN1 Domain Score Distribution")+
  xlab(label = "SIN1 H/M/L")+
  ylab(label = "SIN1 Domain Scores")+
  facet_wrap(~Super.Group)+
  theme_bw()
jitterSIN1
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/JitterSIN1.png",
       plot = jitterSIN1,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Jitter_Plot_SIN1_Domain.pptx",
       figure = jitterSIN1,
       units = "inches",
       width = 10,
       height = 7)

jitterSIN1 <- MasterTable %>%filter(!is.na(SIN1) & SIN1 != "P" & Super.Group != "Rhizaria") %>% ggplot(aes(x = factor(SIN1, level = c("H", "M", "L")), y = SIN1All))+
  geom_jitter(shape = 18, size = 2)+
  labs(title = "SIN1 Overall Score Distribution")+
  xlab(label = "SIN1 H/M/L")+
  ylab(label = "SIN1 Overall Scores")+
  facet_wrap(~Super.Group)+
  theme_bw()
jitterSIN1
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/JitterSIN1All.png",
       plot = jitterSIN1,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Jitter_Plot_SIN1_All.pptx",
       figure = jitterSIN1,
       units = "inches",
       width = 10,
       height = 7)
















jitterLST8 <- MasterTable %>%filter(!is.na(LST8) & LST8 != "P") %>% ggplot(aes(x = factor(LST8, level = c("H", "M", "L")), y = LST8Domain))+
  geom_jitter(shape = 18, size = 2)+
  labs(title = "LST8 Domain Score Distribution")+
  xlab(label = "LST8 H/M/L")+
  ylab(label = "LST8 Domain Scores")+
  facet_wrap(~Super.Group)+
  theme_bw()
jitterLST8
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/JitterLST8.png",
       plot = jitterLST8,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Jitter_Plot_LST8_Domain.pptx",
       figure = jitterLST8,
       units = "inches",
       width = 10,
       height = 7)


jitterLST8 <- MasterTable %>%filter(!is.na(LST8) & LST8 != "P") %>% ggplot(aes(x = factor(LST8, level = c("H", "M", "L")), y = LST8All))+
  geom_jitter(shape = 18, size = 2)+
  labs(title = "LST8 Overall Score Distribution")+
  xlab(label = "LST8 H/M/L")+
  ylab(label = "LST8 OVerall Scores")+
  facet_wrap(~Super.Group)+
  theme_bw()
jitterLST8
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/JitterLST8All.png",
       plot = jitterLST8,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Jitter_Plot_LST8_All.pptx",
       figure = jitterLST8,
       units = "inches",
       width = 10,
       height = 7)







jitterTOR <- MasterTable %>%filter(!is.na(TOR) & TOR != "P") %>% ggplot(aes(x = factor(TOR, level = c("H", "M", "L")), y = TORDomain))+
  geom_jitter(shape = 18, size = 2)+
  labs(title = "TOR Domain Score Distribution")+
  xlab(label = "TOR H/M/L")+
  ylab(label = "TOR Domain Scores")+
  facet_wrap(~Super.Group)+
  theme_bw()
jitterTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/JitterTOR.png",
       plot = jitterTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Jitter_Plot_TOR_Domain.pptx",
       figure = jitterTOR,
       units = "inches",
       width = 10,
       height = 7)


jitterTOR <- MasterTable %>%filter(!is.na(TOR) & TOR != "P") %>% ggplot(aes(x = factor(TOR, level = c("H", "M", "L")), y = TORAll))+
  geom_jitter(shape = 18, size = 2)+
  labs(title = "TOR Overall Score Distribution")+
  xlab(label = "TOR H/M/L")+
  ylab(label = "TOR Overall Scores")+
  facet_wrap(~Super.Group)+
  theme_bw()
jitterTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/JitterTORAll.png",
       plot = jitterTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Jitter_Plot_TOR_All.pptx",
       figure = jitterTOR,
       units = "inches",
       width = 10,
       height = 7)









BoxRictor <- MasterTable %>%group_by(Super.Group)%>%
  mutate(count = n())%>% ungroup() %>% filter(Super.Group != "Chlorophyta")%>%ggplot(aes(x = Super.Group, y = RICTORDomain))+
  stat_boxplot(aes(Super.Group, RICTORDomain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = RICTORDomain),notch = FALSE, outlier.shape = NA)+
  labs(title = "RICTOR Domain Scores", subtitle = "By Associated Clade")+
  xlab(label = "Clade")+
  ylab(label = "RICTOR Domain Score")+
  theme_bw()+
  theme(text = element_text(family = "serif",
                            size = 20))

BoxRictor

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxRictor.png",
       plot = BoxRictor,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Box_Plot_Rictor_Domain.pptx",
       figure = BoxRictor,
       units = "inches",
       width = 10,
       height = 7)


BoxRictor <- MasterTable %>% filter(Super.Group != "Chlorophyta") %>% ggplot(aes(x = Super.Group, y = RICTORAll))+
  stat_boxplot(aes(Super.Group, RICTORDomain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = RICTORDomain),notch = FALSE, outlier.shape = NA)+
  labs(title = "RICTOR Overall Scores", subtitle = "By Associated Clade")+
  xlab(label = "Clade")+
  ylab(label = "RICTOR Overall Score")+
  theme_bw()+
  theme(text = element_text(family = "serif",
                            size = 20))

BoxRictor

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxRictorAll.png",
       plot = BoxRictor,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Box_Plot_Rictor_All.pptx",
       figure = BoxRictor,
       units = "inches",
       width = 10,
       height = 7)














BoxRaptor <- MasterTable %>% ggplot(aes(x = Super.Group, y = RAPTORDomain))+
  stat_boxplot(aes(Super.Group, RAPTORDomain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = RAPTORDomain),notch = FALSE, outlier.shape = NA)+
  labs(title = "RAPTOR Domain Scores", subtitle = "By Associated Clade")+
  xlab(label = "Clade")+
  ylab(label = "RAPTOR Domain Score")+
  theme_bw()+
  theme(text = element_text(family = "serif",
                            size = 20))
BoxRaptor

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxRaptor.png",
       plot = BoxRaptor,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Box_Plot_Raptor_Domain.pptx",
       figure = BoxRaptor,
       units = "inches",
       width = 10,
       height = 7)






BoxRaptor <- MasterTable %>% ggplot(aes(x = Super.Group, y = RAPTORAll))+
  stat_boxplot(aes(Super.Group, RAPTORDomain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = RAPTORDomain),notch = FALSE, outlier.shape = NA)+
  labs(title = "RAPTOR Overall Scores", subtitle = "By Associated Clade")+
  xlab(label = "Clade")+
  ylab(label = "RAPTOR Overall Score")+
  theme_bw()+
  theme(text = element_text(family = "serif",
                            size = 20))
BoxRaptor

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxRaptorAll.png",
       plot = BoxRaptor,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Box_Plot_Raptor_All.pptx",
       figure = BoxRaptor,
       units = "inches",
       width = 10,
       height = 7)












BoxTOR <- MasterTable %>% ggplot(aes(x = Super.Group, y = TORDomain))+
  stat_boxplot(aes(Super.Group, TORDomain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = TORDomain),notch = FALSE, outlier.shape = NA)+
  #geom_jitter()+
  labs(title = "TOR Domain Scores", subtitle = "By Associated Clade")+
  xlab(label = "Clade")+
  ylab(label = "TOR Domain Score")+
  theme_bw()+
  theme(text = element_text(family = "serif",
                            size = 20))
BoxTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxTOR.png",
       plot = BoxTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Box_Plot_TOR_Domain.pptx",
       figure = BoxTOR,
       units = "inches",
       width = 10,
       height = 7)







BoxTOR <- MasterTable %>% ggplot(aes(x = Super.Group, y = TORAll))+
  stat_boxplot(aes(Super.Group, TORDomain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = TORDomain),notch = FALSE, outlier.shape = NA)+
  #geom_jitter()+
  labs(title = "TOR Overall Scores", subtitle = "By Associated Clade")+
  xlab(label = "Clade")+
  ylab(label = "TOR Overall Score")+
  theme_bw()+
  theme(text = element_text(family = "serif",
                            size = 20))
BoxTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxTORAll.png",
       plot = BoxTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Box_Plot_TOR_All.pptx",
       figure = BoxTOR,
       units = "inches",
       width = 10,
       height = 7)











BoxSIN1 <- MasterTable %>%filter(Super.Group != "Chlorophyta")%>%ggplot(aes(x = Super.Group, y = SIN1Domain))+
  stat_boxplot(aes(Super.Group, SIN1Domain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = SIN1Domain),notch = FALSE, outlier.shape = NA)+
  #geom_jitter()+
  labs(title = "SIN1 Domain Scores", subtitle = "By Associated Clade")+
  xlab(label = "Clade")+
  ylab(label = "SIN1 Domain Score")+
  theme_bw()+
  theme(text = element_text(family = "serif",
                            size = 20))
BoxSIN1


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxSIN1.png",
       plot = BoxSIN1,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Box_Plot_SIN1_Domain.pptx",
       figure = BoxSIN1,
       units = "inches",
       width = 10,
       height = 7)




BoxSIN1 <- MasterTable %>% filter(Super.Group != "Chlorophyta")%>%ggplot(aes(x = Super.Group, y = SIN1All))+
  stat_boxplot(aes(Super.Group, SIN1Domain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = SIN1Domain),notch = FALSE, outlier.shape = NA)+
  #geom_jitter()+
  labs(title = "SIN1 Overall Scores", subtitle = "By Associated Clade")+
  xlab(label = "Clade")+
  ylab(label = "SIN1 Overall Score")+
  theme_bw()+
  theme(text = element_text(family = "serif",
                            size = 20))
BoxSIN1


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxSIN1All.png",
       plot = BoxSIN1,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Box_Plot_SIN1_All.pptx",
       figure = BoxSIN1,
       units = "inches",
       width = 10,
       height = 7)












BoxLST8 <- MasterTable %>% ggplot(aes(x = Super.Group, y = LST8Domain))+
  stat_boxplot(aes(Super.Group, LST8Domain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = LST8Domain),notch = FALSE, outlier.shape = NA)+
  #geom_jitter()+
  labs(title = "LST8 Domain Score", subtitle = "By Associated Clade")+
  xlab(label = "Clade")+
  ylab(label = "LST8 Domain Score")+
  theme_bw()+
  theme(text = element_text(family = "serif",
                            size = 20))
BoxLST8


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxLST8.png",
       plot = BoxLST8,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Box_Plot_LST8_Domain.pptx",
       figure = BoxLST8,
       units = "inches",
       width = 10,
       height = 7)











BoxLST8 <- MasterTable %>% ggplot(aes(x = Super.Group, y = LST8All))+
  stat_boxplot(aes(Super.Group, LST8Domain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = LST8Domain),notch = FALSE, outlier.shape = NA)+
  #geom_jitter()+
  labs(title = "LST8 Overall Score", subtitle = "By Associated Clade")+
  xlab(label = "Clade")+
  ylab(label = "LST8 Overall Score")+
  theme_bw()+
  theme(text = element_text(family = "serif",
                            size = 20))
BoxLST8


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxLST8All.png",
       plot = BoxLST8,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Box_Plot_LST8_All.pptx",
       figure = BoxLST8,
       units = "inches",
       width = 10,
       height = 7)













BoxPlotCscore <- MasterTable %>% ggplot()+
  stat_boxplot(aes(Super.Group, C.score), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = C.score),notch = FALSE, outlier.shape = NA)+
  theme_bw()+
  labs(title = "Completeness Score Distribution",
       subtitle = "By Associated Clade")+
  xlab(label = "Clade")+
  ylab(label = "Completeness Score")+
  theme(text = element_text(family = "serif",
                            size = 20))




ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/CscorePlot.png",
       plot = BoxPlotCscore,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/Box_Plot_C_Score.pptx",
       figure = BoxPlotCscore,
       units = "inches",
       width = 10,
       height = 7)





#Those with RICTOR metabolic count
 YesPlot <- MasterTable %>% filter(!is.na(RICTORDomain) | !is.na(RICTORAll))%>%filter(Super.Group == "Alveolata" | Super.Group == "Stramenopiles" | Super.Group == "Rhizaria")%>%
   ggplot()+
   geom_bar(aes(x = M.Strategy, fill = Super.Group,stat = "identity"), position = position_dodge(preserve = 'single'), width = .5, color = "black")+
   labs(title = "Organisms with RICTOR Protein",
        subtitle = "Streptophyta Excluded")+
   xlab(label = "Metabolic Strategy Employed")+
   ylab(label = "Overall Count per Super Group")+
   scale_fill_manual(name = "Super Group",
                     breaks = c("Alveolata",
                                "Stramenopiles",
                                "Rhizaria",
                                "Streptophyta",
                                "Chlorophyta",
                                "Rhodophyta",
                                "Discoba",
                                "Metamonada"),
                     values = c("Alveolata" = "#EFB911",
                                "Stramenopiles" = "#572100",
                                "Rhizaria" = "#FFD0AB",
                                "Streptophyta" = "#678516",
                                "Chlorophyta" = "#525601",
                                "Rhodophyta" = "#681114",
                                "Discoba" = "cyan",
                                "Metamonada" = "blue"
                     ))+
   theme_bw()+
   scale_x_discrete(drop = FALSE)
 YesPlot
 
 
 
 MasterTable %>% filter(!is.na(RICTORDomain) | !is.na(RICTORAll))%>%
   ggplot()+
   stat_boxplot(aes(M.Strategy, RICTORAll), geom = "errorbar", linetype = 1, width = 0.5)+
   geom_boxplot(aes(x = M.Strategy, y = RICTORAll), position = position_dodge(preserve = 'single'), width = .5, color = "black", outlier.shape = NA)+
   labs(title = "Score Distributions Amongst Species with RICTOR Protein")+
   xlab(label = "Metabolic Strategy Employed")+
   ylab(label = "Bit Score")+
   theme_bw()
 
 
 
 #Pie Chart Section for Metabolic Strategies -----------------------------------
 MasterTable <- MasterTable %>%filter(Organism.Name != "Marteilia pararefringens",
                                      Organism.Name != "Paramarteilia canceri",
                                      Organism.Name != "Cercozoa sp. M6MM",
                                      Organism.Name != "Phytophthora citrophthora",
                                      Organism.Name != "Aphanomyces stellatus",
                                      Organism.Name != "Minidiscus trioculatus",
                                      Organism.Name != "Labyrinthula sp. Ha",
                                      Organism.Name != "Cafeteria roenbergensis",
                                      Organism.Name != "Phytophthora fragariaefolia",
                                      Organism.Name != "Phytophthora lilii",
                                      Organism.Name != "Peronosclerospora sorghi",
                                      Organism.Name != "Nothophytophthora sp. Chile5",
                                      Organism.Name != "Ochromonadaceae sp. CCMP2298",
                                      Organism.Name != "Moneuplotes crassus",
                                      Organism.Name != "Symbiodinium pilosum",
                                      Organism.Name != "Symbiodinium sp. CCMP2456",
                                      Organism.Name != "Symbiodinium necroappetens",
                                      Organism.Name != "Eimeria mitis",
                                      Organism.Name != "Eimeria necatrix",
                                      Organism.Name != "Eimeria praecox",
                                      Organism.Name != "Trypanosoma brucei equiperdum",
                                      Organism.Name != "Trpanosoma equiperdum",
                                      Organism.Name != "Trypanosoma rangeli",
                                      Organism.Name != "Trypanosoma rangeli SC58",
                                      Organism.Name != "Strigomonas culicis",
                                      Organism.Name != "Perkinsela sp. CCAP 1560/4",
                                      Organism.Name != "Giardia lamblia ATCC 50803",
                                      Organism.Name != "Spironucleus salmonicida",
                                      Organism.Name != "Hexamita inflata",
                                      Organism.Name != "Streblomastix strix",
                                      Organism.Name != "Aduncisulcus paluster",
                                      Organism.Name != "Monocercomonoides exilis",
                                      Organism.Name != "Paratrimastix pyriformis",
                                      Organism.Name != "Novymonas esmeraldas",
                                      Organism.Name != "Picocystis sp. ML",
                                      Organism.Name != "Pistacia atlantica",
                                      Organism.Name != "Euglena gracilis")


data <- MasterTable %>% filter(!is.na(RICTOR)) %>% count(M.Strategy)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie1 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown of Organisms That Have Rictor")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie1

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/PieWithRictor.png",
       plot = pie1,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/PieWithRictor.pptx",
       figure = pie1,
       units = "inches",
       width = 10,
       height = 7)

 
data2 <- MasterTable %>% filter(is.na(RICTOR))%>% filter(M.Strategy != "Heterotroph")%>% count(M.Strategy)
data2 <- data2 %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data2$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


pie2<- ggplot(data2, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text_repel(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown of Organisms That Don't Have Rictor")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
pie2
 
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/PieWithoutRictor.png",
       plot = pie2,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/PieWithOutRictor.pptx",
       figure = pie2,
       units = "inches",
       width = 10,
       height = 7)








data3 <- MasterTable %>% filter(is.na(RAPTOR) & Organism.Name != "Perkinsela sp. CCAP 1560/4" & Organism.Name != "Trypanosoma brucei equiperdum" & Organism.Name != "Marteilia pararefringens") %>%
  filter(M.Strategy != "Autotrophic")%>%count(M.Strategy)
data3 <- data3 %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data3$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


pie3<- ggplot(data3, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text_repel(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown of Organisms That Don't Have RAPTOR")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))

pie3
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/PieWithoutRAPTOR.png",
       plot = pie3,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/PieWithOutRAPTOR.pptx",
       figure = pie3,
       units = "inches",
       width = 10,
       height = 7)


data4 <- MasterTable %>% filter(!is.na(RAPTOR)) %>% count(M.Strategy)
data4 <- data4 %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data4$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


pie4<- ggplot(data4, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text_repel(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown of Organisms That Have RAPTOR")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))

pie4


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/PieWithRAPTOR.png",
       plot = pie4,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/PieWithRAPTOR.pptx",
       figure = pie4,
       units = "inches",
       width = 10,
       height = 7)



data5 <- MasterTable %>% filter(!is.na(RICTOR)) %>% filter(Super.Group == "Alveolata" | Super.Group == "Stramenopiles" | Super.Group == "Rhizaria")%>%count(M.Strategy)
data5 <- data5 %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data5$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


pie5<- ggplot(data5, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text_repel(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown of SAR Organisms That Have RICTOR")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))

pie5

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/SARWithRictor.png",
       plot = pie5,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/SARWithRictor.pptx",
       figure = pie5,
       units = "inches",
       width = 10,
       height = 7)



data6 <- MasterTable %>% filter(is.na(RICTOR)) %>% filter(Super.Group == "Alveolata" | Super.Group == "Stramenopiles" | Super.Group == "Rhizaria")%>%count(M.Strategy)
data6 <- data6 %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data6$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


pie6<- ggplot(data6, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text_repel(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown of SAR Organisms That Don't Have RICTOR")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))

pie6

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/SARNoRictor.png",
       plot = pie6,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/SARNoRictor.pptx",
       figure = pie6,
       units = "inches",
       width = 10,
       height = 7)

data7 <- MasterTable %>% filter(is.na(RAPTOR)) %>% filter(Super.Group == "Alveolata" | Super.Group == "Stramenopiles" | Super.Group == "Rhizaria")%>%count(M.Strategy)
data7 <- data7 %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data7$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


pie7<- ggplot(data7, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text_repel(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown of SAR Organisms That Don't Have RAPTOR")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))

pie7


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/SARNoRaptor.png",
       plot = pie7,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/SARNoRaptor.pptx",
       figure = pie7,
       units = "inches",
       width = 10,
       height = 7)




data8 <- MasterTable %>% filter(!is.na(RAPTOR)) %>% filter(Super.Group == "Alveolata" | Super.Group == "Stramenopiles" | Super.Group == "Rhizaria")%>%count(M.Strategy)
data8 <- data8 %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data8$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


pie8<- ggplot(data8, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text_repel(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown of SAR Organisms That Have RAPTOR")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))

pie8

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/SARwithRaptor.png",
       plot = pie8,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/SARWithRaptor.pptx",
       figure = pie8,
       units = "inches",
       width = 10,
       height = 7)


data9 <- MasterTable %>% filter(is.na(TOR)) %>% filter(Super.Group == "Alveolata" | Super.Group == "Stramenopiles" | Super.Group == "Rhizaria")%>%count(M.Strategy)
data9 <- data9 %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data9$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


pie9<- ggplot(data9, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text_repel(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown of SAR Organisms That Don't Have TOR")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))

pie9

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/SARNoTOR.png",
       plot = pie9,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/SARNoTOR.pptx",
       figure = pie9,
       units = "inches",
       width = 10,
       height = 7)










#-------------------------------------------------------------------------------
Overall <- MasterTable %>% count(Super.Group)
Overall <- Overall %>% rename(Total = "n")
RaptorData <- MasterTable %>% filter(!is.na(RAPTOR))%>%count(Super.Group)
RaptorData <- left_join(RaptorData,Overall)

RaptorData <- RaptorData %>%
  arrange(desc(Super.Group))%>%
  mutate(prop = n/sum(RaptorData$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)
RaptorPie<- ggplot(RaptorData, aes(x="", y=prop, fill=Super.Group)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text_repel(aes(y = ypos, label = paste(Super.Group,"\n",n,"/",Total)), color = "black", size=6)+
  labs(title = "Count of RAPTOR Containing Species")+
  scale_fill_manual(name = "Super Group",
                    breaks = c("Alveolata",
                               "Stramenopiles",
                               "Rhizaria",
                               "Streptophyta",
                               "Chlorophyta",
                               "Rhodophyta",
                               "Discoba",
                               "Metamonada"),
                    values = c("Alveolata" = "#EFB911",
                               "Stramenopiles" = "#572100",
                               "Rhizaria" = "#FFD0AB",
                               "Streptophyta" = "#678516",
                               "Chlorophyta" = "#525601",
                               "Rhodophyta" = "#681114",
                               "Discoba" = "cyan",
                               "Metamonada" = "blue"
                    ))+
  theme(text = element_text(family = "serif"))
RaptorPie

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/RaptorPie.pptx",
       figure = RaptorPie,
       units = "inches",
       width = 10,
       height = 7)






RictorData <- MasterTable %>% filter(!is.na(RICTOR)) %>% count(Super.Group)
RictorData <- left_join(RictorData,Overall)
RictorData <- RictorData %>%
  arrange(desc(Super.Group))%>%
  mutate(prop = n/sum(RictorData$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


RictorPie<- ggplot(RictorData, aes(x="", y=prop, fill=Super.Group)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text_repel(aes(y = ypos, label = paste(Super.Group,"\n",n,"/",Total)), color = "black", size=6)+
  labs(title = "Count of RICTOR Containing Species")+
  scale_fill_manual(name = "Super Group",
                    breaks = c("Alveolata",
                               "Stramenopiles",
                               "Rhizaria",
                               "Streptophyta",
                               "Chlorophyta",
                               "Rhodophyta",
                               "Discoba",
                               "Metamonada"),
                    values = c("Alveolata" = "#EFB911",
                               "Stramenopiles" = "#572100",
                               "Rhizaria" = "#FFD0AB",
                               "Streptophyta" = "#678516",
                               "Chlorophyta" = "#525601",
                               "Rhodophyta" = "#681114",
                               "Discoba" = "cyan",
                               "Metamonada" = "blue"
                    ))+
  theme(text = element_text(family = "serif"))
RictorPie

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/RictorPie.pptx",
       figure = RictorPie,
       units = "inches",
       width = 10,
       height = 7)

TorData <- MasterTable %>% filter(!is.na(TOR)) %>% count(Super.Group)
TorData <- left_join(TorData,Overall)
TorData <- TorData %>%
  arrange(desc(Super.Group))%>%
  mutate(prop = n/sum(TorData$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)
TorPie<- ggplot(TorData, aes(x="", y=prop, fill=Super.Group)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text_repel(aes(y = ypos, label = paste(Super.Group,"\n",n,"/",Total)), color = "black", size=6)+
  labs(title = "Count of TOR Containing Species")+
  scale_fill_manual(name = "Super Group",
                    breaks = c("Alveolata",
                               "Stramenopiles",
                               "Rhizaria",
                               "Streptophyta",
                               "Chlorophyta",
                               "Rhodophyta",
                               "Discoba",
                               "Metamonada"),
                    values = c("Alveolata" = "#EFB911",
                               "Stramenopiles" = "#572100",
                               "Rhizaria" = "#FFD0AB",
                               "Streptophyta" = "#678516",
                               "Chlorophyta" = "#525601",
                               "Rhodophyta" = "#681114",
                               "Discoba" = "cyan",
                               "Metamonada" = "blue"
                    ))+
  theme(text = element_text(family = "serif"))
TorPie

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/TorPie.pptx",
       figure = TorPie,
       units = "inches",
       width = 10,
       height = 7)






#-------------------------------------------------------------------------------
#SAR Metabolic Breakdown

SARdata <- MasterTable %>% filter(Super.Group == "Stramenopiles" | Super.Group == "Rhizaria" | Super.Group == "Alveolata")


SARdata %>% filter(is.na(RAPTOR))%>%view()


SARNoRictor <- SARdata %>% filter(is.na(RICTOR) & M.Strategy != "Heterotroph") %>% count(M.Strategy)
SARNoRictor <- SARNoRictor %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(SARNoRictor$n) * 100)%>%
  mutate(ypos= cumsum(prop) - 0.5*prop)

SARNoRictorPie<- ggplot(SARNoRictor, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text_repel(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown SAR:",
       subtitle = "No RICTOR")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))
#Save Here
topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/SARPieWithOutRictor.pptx",
       figure = SARNoRictorPie,
       units = "inches",
       width = 10,
       height = 7)


SARWithRictor <- SARdata %>% filter(!is.na(RICTOR))%>%count(M.Strategy)
SARWithRictor <- SARWithRictor %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(SARWithRictor$n) * 100)%>%
  mutate(ypos= cumsum(prop) - 0.5*prop)

SARWithRictorPie <- ggplot(SARWithRictor, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text_repel(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown SAR:",
       subtitle = "With RICTOR")+
  scale_fill_manual(name = "Metabolic Strategy",
                    breaks = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    values = EMpal2,
                    limits = c("Autotrophic","Heterotroph","Mixotroph","Plastid Parasite","Non-Plastid Parasite","Streptophyta Parasite", "Endosymbiotic"),
                    na.value = "grey",
                    drop = FALSE)+
  theme(text = element_text(family = "serif"))

SARWithRictorPie
#Save Here
topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/SARPieWithRictor.pptx",
       figure = SARWithRictorPie,
       units = "inches",
       width = 10,
       height = 7)





SARNoRaptor <- SARdata %>% filter(is.na(RAPTOR))%>%count(M.Strategy)
SARNoRaptor <- SARNoRaptor %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(SARNoRaptor$n) * 100)%>%
  mutate(ypos= cumsum(prop) - 0.5*prop)

SARNoRaptorPie <- ggplot(SARNoRaptor, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=2, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text_repel(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,3),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown SAR:",
       subtitle = "No Raptor")+
   scale_fill_manual(name = "Super Group",
                     breaks = c("Alveolata",
                                "Stramenopiles",
                                "Rhizaria",
                                "Streptophyta",
                                "Chlorophyta",
                                "Rhodophyta",
                                "Discoba",
                                "Metamonada"),
                     values = c("Alveolata" = "#EFB911",
                                "Stramenopiles" = "#572100",
                                "Rhizaria" = "#FFD0AB",
                                "Streptophyta" = "#678516",
                                "Chlorophyta" = "#525601",
                                "Rhodophyta" = "#681114",
                                "Discoba" = "cyan",
                                "Metamonada" = "blue"
                     ))+
   theme_bw()+
  theme(text = element_text(family = "serif"))
SARNoRaptorPie

topptx(file = "~/GitHub/TOR_phylogenetics/Images/Figures_PPT/SARPieWithOutRaptor.pptx",
       figure = SARNoRaptorPie,
       units = "inches",
       width = 10,
       height = 7)













 
 #------------------------------------------------------------------------------
 #Tree Maps---------------------------------------------------------------------



TreeMapWithRictor <- MasterTable %>% filter(!is.na(RICTORDomain) | !is.na(RICTORAll))%>%count(Super.Group, M.Strategy)%>%
   ggplot(aes(area = n, fill = M.Strategy, subgroup = Super.Group, label = as.character(n))) +
   geom_treemap()+
   geom_treemap_subgroup_border()+
   geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.2, colour =
                                "black", fontface = "italic", min.size = 0)+
   geom_treemap_text(colour = "white", place = "centre", reflow = T)+
   labs(title = "Organisms with RICTOR")+
   scale_fill_manual(name = "Metabolic Strategy",
                     breaks = c("Autotrophic","Heterotroph","Mixotroph","Parasite", "Endosymbiotic"),
                     values = EMpal2,
                     limits = c("Autotrophic", "Heterotroph", "Mixotroph", "Parasite", "Endosymbiotic"),
                     na.value = "grey",
                     drop = FALSE)+
   theme(
     text = element_text(family = "serif"),
     legend.background=element_rect(fill=NA),
     legend.title=element_text(size=10), 
     legend.text=element_text(size=5.5),
     legend.spacing.y = unit(0.02, "cm"),
     legend.key.spacing.x = unit(1,"cm"))+
   new_scale_fill()
TreeMapWithRictor
 
 ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/TreeMapWithRictor.png",
        plot = TreeMapWithRictor,
        width = 3840,
        height = 2160,
        units = "px",
        dpi = 320,
        limitsize = FALSE)
 
 TreeMapWithoutRictor <- MasterTable %>% filter(is.na(RICTORDomain) | is.na(RICTORAll))%>%count(Super.Group, M.Strategy)%>%
   ggplot(aes(area = n, fill = M.Strategy, subgroup = Super.Group, label = as.character(n))) +
   geom_treemap()+
   geom_treemap_subgroup_border()+
   geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.2, colour =
                                "black", fontface = "italic", min.size = 0)+
   geom_treemap_text(colour = "white", place = "centre", reflow = T)+
   labs(title = "Organisms without RICTOR")+
   scale_fill_manual(name = "Metabolic Strategy",
                     breaks = c("Autotrophic","Heterotroph","Mixotroph","Parasite", "Endosymbiotic"),
                     values = EMpal2,
                     limits = c("Autotrophic", "Heterotroph", "Mixotroph", "Parasite", "Endosymbiotic"),
                     na.value = "grey",
                     drop = FALSE)+
   theme(
     text = element_text(family = "serif"),
     legend.background=element_rect(fill=NA),
     legend.title=element_text(size=10), 
     legend.text=element_text(size=5.5),
     legend.spacing.y = unit(0.02, "cm"),
     legend.key.spacing.x = unit(1,"cm"))+
   new_scale_fill()
 
 
 ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/TreeMapWithoutRictor.png",
        plot = TreeMapWithoutRictor,
        width = 3840,
        height = 2160,
        units = "px",
        dpi = 320,
        limitsize = FALSE)
 # ------------------------------------------------------------------------------
 YesPlot <- MasterTable %>% filter(!is.na(RICTORDomain) | !is.na(RICTORAll))%>% group_by(M.Strategy, Super.Group) %>% summarize(count=n())
   ggplot(aes(y = factor(M.Strategy, level = c("Mixotroph", "Autotrophic", "Heterotroph","Parasite","Endosymbiotic")), x = as.factor(Super.Group), fill = Super.Group, color = Super.Group))+
   geom_jitter()+
   labs(title = "Organisms with RICTOR")

 
 ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/UpdatedMetabolicCountYes.png",
        plot = YesPlot,
        width = 3840,
        height = 2160,
        units = "px",
        dpi = 320,
        limitsize = FALSE)
 
 
 NoPlot <- MasterTable %>% filter(is.na(RICTORDomain) | is.na(RICTORAll))%>%filter(Super.Group == "Alveolata" | Super.Group == "Stramenopiles" | Super.Group == "Rhizaria")%>%
   ggplot()+
   geom_bar(aes(x = M.Strategy, fill = Super.Group), position = position_dodge(preserve = 'single'), width = .5, color = "black")+
   labs(title = "Organisms without RICTOR Protein",
        subtitle = "Streptophyta Excluded")+
   xlab(label = "Metabolic Strategy Employed")+
   ylab(label = "Overall Count per Super Group")+
   scale_fill_manual(name = "Super Group",
                     breaks = c("Alveolata",
                                "Stramenopiles",
                                "Rhizaria",
                                "Streptophyta",
                                "Chlorophyta",
                                "Rhodophyta",
                                "Discoba",
                                "Metamonada"),
                     values = c("Alveolata" = "#EFB911",
                                "Stramenopiles" = "#572100",
                                "Rhizaria" = "#FFD0AB",
                                "Streptophyta" = "#678516",
                                "Chlorophyta" = "#525601",
                                "Rhodophyta" = "#681114",
                                "Discoba" = "cyan",
                                "Metamonada" = "blue"
                     ))+
   theme_bw()
   
 NoPlot

 ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/UpdatedMetabolicCountNo.png",
        plot = NoPlot,
        width = 3840,
        height = 2160,
        units = "px",
        dpi = 320,
        limitsize = FALSE)

# ------------------------------------------------------------------------------
