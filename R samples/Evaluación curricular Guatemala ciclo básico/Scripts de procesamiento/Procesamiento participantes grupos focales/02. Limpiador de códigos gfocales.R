gfocales <- read_xlsx(here("Datos","Formulario grupos focales","gfocales.xlsx"))

# muestra cuali

muestra_cuali <- read_xlsx(here("Muestra","muestra_cuali.xlsx"))

# arreglar códigos
gfocales$codigo <- gsub("\\.", "", gfocales$codigo) 
gfocales$codigo <- gsub("\\+", "-", gfocales$codigo)
gfocales$codigo <- gsub("\\(", "-", gfocales$codigo)
gfocales$codigo <- gsub("\\)4", "", gfocales$codigo)

gfocales$codigo <- gsub("-55$", "-45", gfocales$codigo)
gfocales$codigo <- gsub("-44$", "-45", gfocales$codigo)
gfocales$codigo <- gsub("-40$", "-45", gfocales$codigo)
gfocales$codigo <- gsub("\n", "", gfocales$codigo)


gfocales$codigo[gfocales$codigo == "09"] <- "09-01-3614-45"
gfocales$codigo[gfocales$codigo == "37"] <- "22-04-0192-45"
gfocales$codigo[gfocales$codigo == "20-05-0246-45"] <- "20-05-0256-45"
gfocales$codigo[gfocales$codigo == "06-15-0418-45"] <- "06-14-0418-45"
gfocales$codigo[gfocales$codigo == "06-19-0013-45"] <- "06-10-0013-45"
gfocales$codigo[gfocales$codigo == "06-20-0013-45"] <- "06-10-0013-45"
gfocales$codigo[gfocales$codigo == "16-01-0160-45"] <- "16-07-0160-45"
gfocales$codigo[gfocales$codigo == "14-04-6345-45"] <- "13-04-6345-45"
gfocales$codigo[gfocales$codigo == "16-09-3729-45"] <- "17-09-3729-45"
gfocales$codigo[gfocales$codigo == "17-20-5962-45"] <- "14-20-5962-45"
gfocales$codigo[gfocales$codigo == "16-14-2027-45"] <- "16-14-1027-45"
gfocales$codigo[gfocales$codigo == "25-01-0101-45"] <- "15-01-0101-45"
gfocales$codigo[gfocales$codigo == "17-01-0959-45"] <- "17-01-0059-45"
gfocales$codigo[gfocales$codigo == "17-10-4951-45"] <- "17-10-4051-45"
gfocales$codigo[gfocales$codigo == "19-01-0085-45"] <- "19-01-0084-45"

# anti join

mal_codificados <-  gfocales |> 
  anti_join(muestra_cuali, by = "codigo") 

# arreglar variable de tipo de participante

gfocales <- gfocales |> 
  mutate(rol_centro = case_match(rol_centro,
                                 "Otro" ~ "Otro actor (en grupo de docentes y directores)",
                                 .default = rol_centro)) |> 
  mutate(participante = case_when(
    grupo_focal == "Padres y madres de familia" & parentesco_estudiante == "Padre o madre" & sexo == "Mujer" ~ "Madre de familia",
    grupo_focal == "Padres y madres de familia" & parentesco_estudiante == "Padre o madre" & sexo == "Hombre" ~ "Padre de familia",
    grupo_focal == "Padres y madres de familia" & parentesco_estudiante == "Encargado/a" ~ "Encargado/a",
    grupo_focal == "Docentes y directores" ~ rol_centro
  ))


write_xlsx(gfocales, here("Datos","Listos para análisis","Formulario grupos focales", "gfocales.xlsx"))
