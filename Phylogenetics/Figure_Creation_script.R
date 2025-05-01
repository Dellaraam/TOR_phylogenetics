
# Figure Creation Script: Plots
#source(file = ")


# ------------------------------------------------------------------------------
#Test Ground
MasterTable <- read_csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Master_Table.csv")
MasterTable <- MasterTable %>% select(-...1)
MasterTable <- MasterTable %>%
  mutate(SIN1Domain = if_else(Super.Group == "Chlorophyta",NA,SIN1Domain),
         SIN1All = if_else(Super.Group == "Chlorophyta", NA, SIN1All),
         RICTORDomain = if_else(Super.Group == "Chlorophyta", NA, RICTORDomain),
         RICTORAll = if_else(Super.Group == "Chlorophyta", NA, RICTORAll))






EMpal2 <- c(
  "Autotrophic" = "#3CBA26",
  "Endosymbiotic" = "purple",
  "Heterotroph" = "#A87142",
  "Mixotroph" = "#63E3C5",
  "Parasite" = "#FE4A49" 
)















jitterRICTOR<- MasterTable %>%filter(!is.na(RICTOR) & RICTOR != "P") %>% ggplot(aes(x = factor(RICTOR,level = c("H", "M", "L")), y = RICTORDomain))+
  geom_jitter(shape = 18, size = 2)+
  labs(title = "RICTOR Domain Score Distribution")+
  xlab(label = "RICTOR H/M/L")+
  ylab(label = "RICTOR Domain Scores")+
  #geom_text_repel(aes(label = Organism.Name), size = 2.2, show.legend = FALSE)+
  facet_wrap(~Super.Group,nrow = 1)+
  theme_bw()
jitterRICTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/JitterRICTOR.png",
       plot = jitterRICTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)

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









BoxRictor <- MasterTable %>%filter(Super.Group != "Chlorophyta")%>%ggplot(aes(x = Super.Group, y = RICTORDomain))+
  stat_boxplot(aes(Super.Group, RICTORDomain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = RICTORDomain),notch = FALSE, outlier.shape = NA)+
  labs(title = "RICTOR Domain Scores", subtitle = "By Associated Super Group")+
  xlab(label = "Super Groups")+
  ylab(label = "RICTOR Domain Score")+
  theme_bw()

BoxRictor

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxRictor.png",
       plot = BoxRictor,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)


BoxRictor <- MasterTable %>% filter(Super.Group != "Chlorophyta") %>% ggplot(aes(x = Super.Group, y = RICTORAll))+
  stat_boxplot(aes(Super.Group, RICTORDomain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = RICTORDomain),notch = FALSE, outlier.shape = NA)+
  labs(title = "RICTOR Overall Scores", subtitle = "By Associated Super Group")+
  xlab(label = "Super Groups")+
  ylab(label = "RICTOR Overall Score")+
  theme_bw()

BoxRictor

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxRictorAll.png",
       plot = BoxRictor,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)













BoxRaptor <- MasterTable %>% ggplot(aes(x = Super.Group, y = RAPTORDomain))+
  stat_boxplot(aes(Super.Group, RAPTORDomain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = RAPTORDomain),notch = FALSE, outlier.shape = NA)+
  labs(title = "RAPTOR Domain Scores", subtitle = "By Associated Super Group")+
  xlab(label = "Super Groups")+
  ylab(label = "RAPTOR Domain Score")+
  theme_bw()
BoxRaptor

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxRaptor.png",
       plot = BoxRaptor,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)





BoxRaptor <- MasterTable %>% ggplot(aes(x = Super.Group, y = RAPTORAll))+
  stat_boxplot(aes(Super.Group, RAPTORDomain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = RAPTORDomain),notch = FALSE, outlier.shape = NA)+
  labs(title = "RAPTOR Overall Scores", subtitle = "By Associated Super Group")+
  xlab(label = "Super Groups")+
  ylab(label = "RAPTOR Overall Score")+
  theme_bw()
BoxRaptor

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxRaptorAll.png",
       plot = BoxRaptor,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)












BoxTOR <- MasterTable %>% ggplot(aes(x = Super.Group, y = TORDomain))+
  stat_boxplot(aes(Super.Group, TORDomain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = TORDomain),notch = FALSE, outlier.shape = NA)+
  #geom_jitter()+
  labs(title = "TOR Domain Scores", subtitle = "By Associated Super Group")+
  xlab(label = "Super Groups")+
  ylab(label = "TOR Domain Score")+
  theme_bw()
BoxTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxTOR.png",
       plot = BoxTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)






BoxTOR <- MasterTable %>% ggplot(aes(x = Super.Group, y = TORAll))+
  stat_boxplot(aes(Super.Group, TORDomain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = TORDomain),notch = FALSE, outlier.shape = NA)+
  #geom_jitter()+
  labs(title = "TOR Overall Scores", subtitle = "By Associated Super Group")+
  xlab(label = "Super Groups")+
  ylab(label = "TOR Overall Score")+
  theme_bw()
BoxTOR
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxTORAll.png",
       plot = BoxTOR,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)











BoxSIN1 <- MasterTable %>%filter(Super.Group != "Chlorophyta")%>%ggplot(aes(x = Super.Group, y = SIN1Domain))+
  stat_boxplot(aes(Super.Group, SIN1Domain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = SIN1Domain),notch = FALSE, outlier.shape = NA)+
  #geom_jitter()+
  labs(title = "SIN1 Domain Scores", subtitle = "By Associated Super Group")+
  xlab(label = "Super Groups")+
  ylab(label = "SIN1 Domain Score")+
  theme_bw()
BoxSIN1


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxSIN1.png",
       plot = BoxSIN1,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)




BoxSIN1 <- MasterTable %>% filter(Super.Group != "Chlorophyta")%>%ggplot(aes(x = Super.Group, y = SIN1All))+
  stat_boxplot(aes(Super.Group, SIN1Domain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = SIN1Domain),notch = FALSE, outlier.shape = NA)+
  #geom_jitter()+
  labs(title = "SIN1 Overall Scores", subtitle = "By Associated Super Group")+
  xlab(label = "Super Groups")+
  ylab(label = "SIN1 Overall Score")+
  theme_bw()
BoxSIN1


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxSIN1All.png",
       plot = BoxSIN1,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)












BoxLST8 <- MasterTable %>% ggplot(aes(x = Super.Group, y = LST8Domain))+
  stat_boxplot(aes(Super.Group, LST8Domain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = LST8Domain),notch = FALSE, outlier.shape = NA)+
  #geom_jitter()+
  labs(title = "LST8 Domain Score", subtitle = "By Associated Super Group")+
  xlab(label = "Super Groups")+
  ylab(label = "LST8 Domain Score")+
  theme_bw()
BoxLST8


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxLST8.png",
       plot = BoxLST8,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)











BoxLST8 <- MasterTable %>% ggplot(aes(x = Super.Group, y = LST8All))+
  stat_boxplot(aes(Super.Group, LST8Domain), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = LST8Domain),notch = FALSE, outlier.shape = NA)+
  #geom_jitter()+
  labs(title = "LST8 Overall Score", subtitle = "By Associated Super Group")+
  xlab(label = "Super Groups")+
  ylab(label = "LST8 Overall Score")+
  theme_bw()
BoxLST8


ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/BoxLST8All.png",
       plot = BoxLST8,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)













BoxPlotCscore <- MasterTable %>% ggplot()+
  stat_boxplot(aes(Super.Group, C.score), geom = "errorbar", linetype = 1, width = 0.5)+
  geom_boxplot(aes( x = Super.Group, y = C.score),notch = FALSE, outlier.shape = NA)+
  theme_bw()+
  labs(title = "Completeness Score Distribution",
       subtitle = "By Associated Super Group")+
  xlab(label = "Super Group")+
  ylab(label = "Completeness Score")




ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/CscorePlot.png",
       plot = BoxPlotCscore,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)





#Those with RICTOR metabolic counthttp://127.0.0.1:41963/graphics/plot_zoom_png?width=1707&height=912
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
 YesPlot
 
 
 
 MasterTable %>% filter(!is.na(RICTORDomain) | !is.na(RICTORAll))%>%
   ggplot()+
   stat_boxplot(aes(M.Strategy, RICTORAll), geom = "errorbar", linetype = 1, width = 0.5)+
   geom_boxplot(aes(x = M.Strategy, y = RICTORAll), position = position_dodge(preserve = 'single'), width = .5, color = "black", outlier.shape = NA)+
   labs(title = "Score Distributions Amongst Species with RICTOR Protein")+
   xlab(label = "Metabolic Strategy Employed")+
   ylab(label = "Bit Score")+http://127.0.0.1:41963/graphics/plot_zoom_png?width=1707&height=912
   theme_bw()
 
 
 
 
 MasterTable %>% filter(!is.na(RICTORDomain) | !is.na(RICTORAll))%>%count(Super.Group, M.Strategy)%>%
   ggplot()+
   geom_bar(aes(x = M.Strategy, y = n, fill = Super.Group), stat = "identity", position = "dodge", alpha = .8)+
   coord_polar()+
   theme_bw()
 
 
 
 
pie1 <- MasterTable %>% filter(!is.na(RICTORDomain) | !is.na(RICTORAll))%>%
  count(M.Strategy)%>%
  ggplot(aes(x="", y = n, fill = M.Strategy))+
  geom_bar(stat="identity", width=1, color = "white")+
  coord_polar("y", start = 0)+
  labs(title = "Breakdown of Metabolic Strategies that include Rictor")


data <- MasterTable %>% filter(!is.na(RICTORDomain) | !is.na(RICTORAll)) %>% count(M.Strategy)
data <- data %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

pie1 <- ggplot(data, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,4),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown of Organisms That Have Rictor")
 

ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/PieWithRictor.png",
       plot = pie1,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)



 
data2 <- MasterTable %>% filter(is.na(RICTORDomain) | is.na(RICTORAll)) %>% count(M.Strategy)
data2 <- data2 %>%
  arrange(desc(M.Strategy))%>%
  mutate(prop = n/sum(data2$n) * 100)%>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


pie2<- ggplot(data2, aes(x="", y=prop, fill=M.Strategy)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none")+
  geom_text(aes(y = ypos, label = paste(M.Strategy,"\n",round(prop,4),"%")), color = "black", size=6)+
  labs(title = "Metabolic Strategy Breakdown of Organisms That Don't Have Rictor")
 
ggsave("~/GitHub/TOR_phylogenetics/Images/Updated_Figure_Images/PieWithoutRictor.png",
       plot = pie2,
       width = 3840,
       height = 2160,
       units = "px",
       dpi = 320,
       limitsize = FALSE)
 
 
 





































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
 
 # MasterTable %>% filter(!is.na(RICTORDomain) | !is.na(RICTORAll))%>% group_by(M.Strategy, Super.Group) %>% summarize(count=n())
 #   ggplot(aes(y = factor(M.Strategy, level = c("Mixotroph", "Autotrophic", "Heterotroph","Parasite","Endosymbiotic")), x = as.factor(Super.Group), fill = Super.Group, color = Super.Group))+
 #   geom_jitter()+
 #   labs(title = "Organisms with RICTOR")
 # 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
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
