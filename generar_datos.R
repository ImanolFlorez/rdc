
args <- commandArgs(trailingOnly = TRUE)

numero <- as.numeric(args[1])

library(readxl)
library(readr)
library(dplyr)

 read_excel("datos/RDC_2019_V1.xlsx") %>%
   # AREA es NA, pues no se tiene al inicio y es lo que se quiere predecir
   mutate(AREA = NA) %>%
  slice_sample(n=numero) %>%
  write_csv("muestra.csv")



