# ------------------------ Análsis de retornados Honduras ---------------------#
library(dplyr)
library(readr)
library(readxl)

rm(list = ls())

# Defino las rutas y el directorio
path <- "/Users/florenciaruiz/Library/Mobile Documents/com~apple~CloudDocs/World Bank/Retronados"
data_path <- paste(path,"/data", sep = "")
setwd(path)

# Importo la base
retornados <- read_xlsx(paste(data_path,"/Datos_Anonimizados_BM_al_03_06_2025.xlsx", sep=""))
