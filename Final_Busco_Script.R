# Finalizing BUSCO Tables

library(tidyverse)


AlveolataBusco <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Alveolata_BUSCO_Sheet1.csv")
AlveolataBusco <- AlveolataBusco %>%
  rename(Accn = "Accession") %>%
  select(Accn, C.score, Frag.score) %>%
  mutate(across(everything(), ~ sub("%$", "", .x))) %>%
  type.convert(as.is = TRUE)

AlveolataJGIBusco <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/AlveolataJGIBusco.csv")
AlveolataJGIBusco <- AlveolataJGIBusco %>% select(Accn, C.score, Frag.score)
AlveolataJGIBusco$Accn <- sub("\\_.*", "", AlveolataJGIBusco$Accn)

AlveolataFinalBusco <- rbind(AlveolataBusco, AlveolataJGIBusco)


StramenopileBusco <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Stramenopiles BUSCO - Sheet1.csv")
StramenopileBusco <- StramenopileBusco %>%
  rename(Accn = "Accession") %>%
  select(Accn, C.score, Frag.score) %>%
  mutate(across(everything(), ~ sub("%$", "", .x))) %>%
  type.convert(as.is = TRUE)

StramenopileJGIBusco <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/HeterokontaJGIBusco.csv")
StramenopileJGIBusco <- StramenopileJGIBusco %>% select(Accn, C.score, Frag.score)
StramenopileJGIBusco$Accn <- sub("\\_.*", "", StramenopileJGIBusco$Accn)

StramenopileFinalBusco <- rbind(StramenopileBusco,StramenopileJGIBusco)

RhizariaBusco <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Rhizaria_BUSCO_Sheet1.csv")
RhizariaBusco <- RhizariaBusco %>%
  rename(Accn = "Accession") %>%
  select(Accn, C.score, Frag.score) %>%
  mutate(across(everything(), ~ sub("%$", "", .x))) %>%
  type.convert(as.is = TRUE)

RhizariaJGIBusco <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/RhizariaJGIBusco.csv")
RhizariaJGIBusco <- RhizariaJGIBusco %>% select(Accn, C.score, Frag.score)
RhizariaJGIBusco$Accn <- sub("\\_.*", "", RhizariaJGIBusco$Accn)

RhizariaFinalBusco <- rbind(RhizariaBusco,RhizariaJGIBusco)

ChlorophytaBusco <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Chlorophyta-BUSCO-Sheet1.csv")
ChlorophytaBusco <- ChlorophytaBusco %>%
  rename(Accn = "Accession") %>%
  select(Accn, C.score, Frag.score) %>%
  mutate(across(everything(), ~ sub("%$", "", .x))) %>%
  type.convert(as.is = TRUE)

ChlorophytaJGIBusco <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/ChlorophytaJGIBusco.csv")
ChlorophytaJGIBusco <- ChlorophytaJGIBusco %>% select(Accn, C.score, Frag.score)
ChlorophytaJGIBusco$Accn <- sub("\\_.*", "", ChlorophytaJGIBusco$Accn)

ChlorophytaFinalBusco <- rbind(ChlorophytaBusco,ChlorophytaJGIBusco)

RhodophytaBusco <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Rhodophyta-BUSCO-Sheet1.csv")
RhodophytaBusco <- RhodophytaBusco %>%
  rename(Accn = "Accession") %>%
  select(Accn, C.score, Frag.score) %>%
  mutate(across(everything(), ~ sub("%$", "", .x))) %>%
  type.convert(as.is = TRUE)

RhodophytaFinalBusco <- RhodophytaBusco

StreptophytaBusco <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/StreptophytaBusco.csv")
StreptophytaBusco <- StreptophytaBusco %>% select(Accn, C.score, Frag.score)
StreptophytaFinalBusco <- StreptophytaBusco

ExcavataBusco <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/Excavata-BUSCO-Sheet1.csv")
ExcavataBusco <- ExcavataBusco %>%
  rename(Accn = "Accession") %>%
  select(Accn, C.score, Frag.score) %>%
  mutate(across(everything(), ~ sub("%$", "", .x))) %>%
  type.convert(as.is = TRUE)

ExcavataJGIBusco <- read.csv("C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/ExcavataJGIBusco.csv")
ExcavataJGIBusco <- ExcavataJGIBusco %>% select(Accn, C.score, Frag.score)
ExcavataJGIBusco$Accn <- sub("\\_.*", "", ExcavataJGIBusco$Accn)

ExcavataFinalBusco <- rbind(ExcavataBusco, ExcavataJGIBusco)



FinalBusco <- rbind(ChlorophytaFinalBusco, StreptophytaFinalBusco, RhodophytaFinalBusco, ExcavataFinalBusco, AlveolataFinalBusco, StramenopileFinalBusco, RhizariaFinalBusco)
write.csv(FinalBusco, "C:/Users/kajoh/Documents/GitHub/TOR_phylogenetics/GitHub_CSV/FinalBusco.csv")
