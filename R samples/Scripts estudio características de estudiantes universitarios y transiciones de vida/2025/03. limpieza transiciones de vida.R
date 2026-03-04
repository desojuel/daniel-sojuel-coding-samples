library(tidyverse)
library(readxl)
library(writexl)

df <- read_xlsx("df_2025_limpia_2.xlsx")

# Enfermedades graves ----

df$cambios_que_enfermedad[df$cambios_que_enfermedad == "Anemia severa"] <- "Anemia"
df$cambios_que_enfermedad[df$cambios_que_enfermedad == "Apendisitis"] <- "Apendicitis"
df$cambios_que_enfermedad[df$cambios_que_enfermedad == "ciática"] <- "Ciática"
df$cambios_que_enfermedad[df$cambios_que_enfermedad == "crisis asmática"] <- "Asma"
df$cambios_que_enfermedad[df$cambios_que_enfermedad == "DENGUE"] <- "Dengue"
df$cambios_que_enfermedad[df$cambios_que_enfermedad == "Dengue H"] <- "Dengue"
df$cambios_que_enfermedad[df$cambios_que_enfermedad == "Dengue hemorragico"] <- "Dengue"
df$cambios_que_enfermedad[df$cambios_que_enfermedad == "un diagnóstico de una enfermedad genética"] <- "Enfermedad genética"

# cirugías ----

df$cambios_que_cirugia[df$cambios_que_cirugia == "Apendice"] <- "Apéndice"
df$cambios_que_cirugia[df$cambios_que_cirugia == "Vasectomia"] <- "Vasectomía"

# otros cambios

df$cambios_otro[df$cambios_otro == "."] <- NA

df$cambios_otro[df$cambios_otro == "N/A"] <- NA
df$cambios_otro[df$cambios_otro == "nada"] <- NA
df$cambios_otro[df$cambios_otro == "Ninguna"] <- NA
df$cambios_otro[df$cambios_otro == "Ninguno"] <- NA
df$cambios_otro[df$cambios_otro == "no"] <- NA
df$cambios_otro[df$cambios_otro == "No"] <- NA
df$cambios_otro[df$cambios_otro == "No hubo"] <- NA
df$cambios_otro[df$cambios_otro == "No hubo ningún cambio"] <- NA
df$cambios_otro[df$cambios_otro == "No ninguno"] <- NA
df$cambios_otro[df$cambios_otro == "No."] <- NA
df$cambios_otro[df$cambios_otro == "Que yo recuerde no"] <- NA
df$cambios_otro[df$cambios_otro == "volvi a dibujar"] <- "Volví a dibujar"

write_xlsx(df, "df_2025_limpia_3.xlsx")
