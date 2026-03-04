library(tidyverse)
library(janitor)
library(readxl)
library(writexl)
library(crosstable)

carnets_2024 <- data.frame(carnet = na.omit(read_xlsx("Carnets 2024.xlsx")$Carnet))



carnets_2025 <- data.frame(carnet = na.omit(read_xlsx("Carnets 2025.xlsx")))

# nrow(carnets_abril) - nrow(carnets_marzo)
# 
# carnets_todos <- data.frame(carnet = na.omit(read_xlsx("nuevo_ingreso_2024_asignados.xlsx")$`Nuevo ingreso 2024: carnet`))
# nrow(carnets_todos)

# Find values in carnets_todos but not in carnets_marzo

faltantes <- data.frame(carnet = setdiff(carnets_2024$carnet, carnets_2025$carnet))

faltantes$correos <- paste(faltantes$carnet, "@a.psicousac.edu.gt", sep = "")

write_xlsx(faltantes,"faltantes_2025.xlsx")

df |> 
  crosstable(cohorte, by=participacion_2024)


### df con variable de seguimiento

v_carnets_2024 <- carnets_2024[[1]]

df <- df |> 
  mutate(seguimiento = ifelse(carnet %in% v_carnets_2024,
                              "Sí participó en 2024",
                              "No participó en 2024")) |> 
  relocate(seguimiento, .after = carnet)

write_xlsx(df, "df_limpia_para_analisis.xlsx")




