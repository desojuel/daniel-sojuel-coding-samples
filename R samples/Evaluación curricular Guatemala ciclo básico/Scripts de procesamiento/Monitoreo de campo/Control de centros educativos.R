library(pacman)

pacman::p_load(readxl,tidyverse,janitor,writexl,here)

muestra <- read_xlsx("Muestra/muestra completa.xlsx", sheet = "Hoja4")

muestra_campo <- muestra |> 
  filter(!is.na(campo)) |> 
  select(-c(pik,Factor_de_expansion_ESCUELAS,orden_seleccion,ID_ESCUELA,ELEGIBLE)) |> 
  rename(codigo = CodEstablecimiento)

muestra_cuali <- muestra_campo |> 
  filter(campo == "aplicadores")

# control de insumos cualitativos ----

# registradas_en_insumos <- read.delim(pipe("pbpaste"))
# 
# # Códigos de control_centros_dyd que NO están en muestra_campo
# no_en_muestra_insumos <-  muestra_cuali |> 
#   anti_join(registradas_en_insumos, by = "codigo") |> 
#   select(codigo)
# 
# no_en_muestra_insumos <-  registradas_en_insumos |> 
#   anti_join(muestra_cuali, by = "codigo") 



# lectura

reporte_consultores <- read_xlsx("datasets para procesamiento/reporte_consultores.xlsx")
df_dyd <- read_xlsx("Datos/Listos para análisis/Encuesta docente/df_dyd.xlsx")
df_est <- read_xlsx("Datos/Listos para análisis/Encuesta estudiantes/df_est.xlsx")
df_obs01 <- read_xlsx("Datos/Listos para análisis/Observacion a nivel de centros/obs01.xlsx")
df_obs02 <- read_xlsx("Datos/Listos para análisis/Observacion de aulas/obs02.xlsx")
gfocales <- read_xlsx(here("Datos","Listos para análisis","Formulario grupos focales", "gfocales.xlsx"))

# conteo

## docentes 

control_centros_dyd <- df_dyd |>
  filter(!is.na(codigo)) |>
  tabyl(codigo) |> 
  rename(encuesta_docente_director = n) |> 
  select(-percent)

control_centros_dyd_puesto <- df_dyd |>
  filter(!is.na(codigo)) |>
  tabyl(codigo,puesto) |> 
  rename(Excluidos = NA_)

control_centros_est <- df_est |>
  filter(!is.na(codigo)) |>
  tabyl(codigo) |> 
  rename(encuesta_estudiantes = n) |> 
  select(-percent)

control_centros_obs01 <- df_obs01 |>
  filter(!is.na(codigo)) |>
  tabyl(codigo) |> 
  rename(observacion_centros = n) |> 
  select(-percent)

control_centros_obs02 <- df_obs02 |>
  filter(!is.na(codigo)) |>
  tabyl(codigo) |> 
  tabyl(codigo) |> 
  rename(observacion_areas = n) |> 
  select(-percent)

periodos_observados_obs02 <- df_obs02 |>
  filter(!is.na(codigo)) |>
  tabyl(codigo) |> 
  rename(periodos_observados = n) |> 
  select(-percent)

gfocales <- gfocales %>%
  mutate(
    participante = fct_relevel(
      participante,
      "Director",
      "Director/Docente",
      "Docente",
      "Otro actor (en grupo de docentes y directores)",
      "Madre de familia",
      "Padre de familia",
      "Encargado/a"
    )
  )

gfocales_participantes <- gfocales |> 
  tabyl(codigo,participante)

# joins

muestra_campo <- muestra_campo %>%
  left_join(control_centros_dyd, by = "codigo")

muestra_campo <- muestra_campo %>%
  left_join(control_centros_dyd_puesto, by = "codigo")

muestra_campo <- muestra_campo %>%
  left_join(control_centros_est, by = "codigo")

muestra_campo <- muestra_campo %>%
  left_join(control_centros_obs01, by = "codigo")

muestra_campo <- muestra_campo %>%
  left_join(control_centros_obs02, by = "codigo")

muestra_campo <- muestra_campo %>%
  left_join(periodos_observados_obs02, by = "codigo")

muestra_campo <- muestra_campo %>%
  left_join(gfocales_participantes, by = "codigo")

muestra_campo <- muestra_campo %>%
  left_join(reporte_consultores, by = "codigo")

muestra_campo <- muestra_campo %>%
  relocate(aplicador, .after = campo)



# diferencias reportes areas

muestra_campo <- muestra_campo |> 
  mutate(diferencias_reportes = periodos_observados - reporte_consultores)


# anti joins

# Códigos de control_centros_dyd que NO están en muestra_campo
no_en_muestra_dyd <- control_centros_dyd %>%
  anti_join(muestra_campo, by = "codigo")

# Códigos de control_centros_est que NO están en muestra_campo
no_en_muestra_est <- control_centros_est %>%
  anti_join(muestra_campo, by = "codigo")

# Códigos de control_centros_obs que NO están en muestra_campo

no_en_muestra_obs <- control_centros_obs01 %>%
  anti_join(muestra_campo, by = "codigo")

# perdidas

dic <- tibble(
  codigo = c(
    "04-01-2479-45", "16-06-1226-45", "16-08-0211-45", "04-12-2627-45",
    "07-18-0368-45", "14-15-0209-45", "12-09-0134-45", "16-09-0803-45",
    "12-07-0215-45", "09-23-0042-45", "20-10-1522-45", "13-15-6199-45"
  ),
  Perdidas = c(
    "Jornada", "Acceso", "Acceso", "Acceso", "Problemas subvención",
    "Acceso", "Distancia", "Acceso", "Temporalidad",
    "Cambio de ubicación", "Steg", "Director no autorizó"
  )
)

muestra_campo <- muestra_campo |> 
  select(-Perdidas) |> 
  left_join(dic, by = "codigo")


# write xls

write_xlsx(muestra_campo,"Monitoreo de campo/monitoreo_campo.xlsx")
write_xlsx(no_en_muestra_dyd,"Monitoreo de campo/no_aparecen_encuesta_docentes.xlsx")
write_xlsx(no_en_muestra_est,"Monitoreo de campo/no_aparecen_encuesta_estudiantes.xlsx")
write_xlsx(no_en_muestra_obs,"Monitoreo de campo/no_aparecen_observaciones.xlsx")
#write_xlsx(no_en_muestra_insumos,"no_muestra_insumos.xlsx")