if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("msa")

library(msa)
library(tidyverse)



myseq <- readAAStringSet(filepath = "C:/Users/kajoh/Desktop/Test_Fasta.fa",
                         format = "fasta")

multipleseqA <- msa(myseq, method = "ClustalOmega")
print(multipleseqA, show = "complete")
msaPrettyPrint()
msaPrettyPrint(multipleseqA,
               output = "pdf",
               file = "C:/Users/kajoh/Desktop/R_msa_test.pdf"
               )
