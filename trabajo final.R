# TRABAJO FINAL
garbanzo<-read.xlsx("C:/Users/100032608/Documents/veranomulti_exp.1.xlsx", sheetIndex = 1)
garbanzo
view(garbanzo)
library(tidyverse)

lapply(garbanzo,summary)
