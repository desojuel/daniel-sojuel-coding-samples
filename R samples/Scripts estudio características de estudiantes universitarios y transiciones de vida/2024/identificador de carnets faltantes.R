library(tidyverse)
library(janitor)
library(readxl)
library(writexl)
library(crosstable)

# Read the Excel files and create data frames
carnets_marzo <- data.frame(carnet = na.omit(read_xlsx("marzo_2024.xlsx")$Carné))
carnets_abril <- data.frame(carnet = na.omit(read_xlsx("export-15031542.xlsx")$Carné))

# nrow(carnets_abril) - nrow(carnets_marzo)

carnets_todos <- data.frame(carnet = na.omit(read_xlsx("nuevo_ingreso_2024_asignados.xlsx")$`Nuevo ingreso 2024: carnet`))
nrow(carnets_todos)
# Find values in carnets_todos but not in carnets_marzo
faltantes <- data.frame(carnet = setdiff(carnets_todos$carnet, carnets_abril$carnet))
no_primer_ingreso <- data.frame(carnet = setdiff(carnets_abril$carnet,carnets_todos$carnet))

faltantes$correos <- paste(faltantes$carnet, "@a.psicousac.edu.gt", sep = "")

write_xlsx(faltantes,"faltantes.xlsx")
