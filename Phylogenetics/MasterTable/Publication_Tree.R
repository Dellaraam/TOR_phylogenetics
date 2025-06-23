# Creation of Table for Publication
# Used to Update the Master Table
# Adding new metabolic information


source(file = "~/GitHub/TOR_phylogenetics/Phylogenetics/Library_Script.R")

MasterTable <- read.csv(file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Master_Table.csv")
MasterTable <- select(MasterTable, -X)

MasterTable <- MasterTable %>% mutate(M.Strategy = if_else(Phylum.name == "Apicomplexa", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Phylum.name == "Perkinsozoa", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Super.Group == "Streptophyta" & M.Strategy == "Parasite", "Streptophyta Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Family.name == "Amoebophryaceae", "Plastid Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Nitzschia putrida", "Heterotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Carpediemonas membranifera", "Heterotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Lagenidium giganteum", "Parasite", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Apatococcus lobatus", "Mixotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Chromochloris zofingiensis", "Mixotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Scenedesmus", "Mixotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Prototheca wickerhamii", "Heterotroph", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Apatococcus fuscideae", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Symbiochloris irregularis", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Trebouxia sp. C0004", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Trebouxia sp. C0006", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Trebouxia sp. C0005", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Trebouxia sp. C0009 RCD-2024", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(Organism.Name == "Trebouxia sp. C0010 RCD-2024", "Endosymbiotic", M.Strategy, missing = M.Strategy),
                                      M.Strategy = if_else(M.Strategy == "Parasite" & Super.Group != "Streptophyta", "Non-Plastid Parasite", M.Strategy, missing = M.Strategy))


write.csv(MasterTable, file = "~/GitHub/TOR_phylogenetics/GitHub_CSV/Finalized_CSVs/Publication_Ready_Table.csv")

