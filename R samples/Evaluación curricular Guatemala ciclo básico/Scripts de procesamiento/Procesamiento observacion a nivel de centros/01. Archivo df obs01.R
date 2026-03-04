# Generador de atributos para el proceso cuali

# preámbulo

# lecturas 

obs01 <- read_xlsx(here("Datos","Observación a nivel de centro educativo", "observacion centro 18.11.25.xlsx"))

# cols_obs01 <- as.vector(na.omit(read_xlsx(here("datasets para procesamiento","colnames evaluacion curricular basico.xlsx")[[1]])))

cols_obs01 <- read_xlsx(
  here("datasets para procesamiento", "colnames evaluacion curricular basico.xlsx"),
  col_names = T
)[[1]] |> 
  na.omit() |> 
  as.vector()

colnames(obs01) <- cols_obs01

obs01 <- obs01 |> 
  filter(acceder == "Sí")

## eliminar duplicados

obs01_dups <- obs01[obs01$codigo %in% obs01$codigo[duplicated(obs01$codigo)], ]
obs01 <- obs01[!duplicated(obs01$codigo), ]

# merge municipios

rango_vars <- match("guatemala", colnames(obs01)):match("jutiapa", colnames(obs01))

obs01 <- obs01 %>%
  mutate(
    municipio = across(all_of(rango_vars)) |> 
      reduce(coalesce)
  ) |> 
  relocate(municipio, .after = "jutiapa")


# muestra completa y cuali

muestra <- read_xlsx(here("Muestra", "muestra completa.xlsx"), sheet = "Hoja4")

muestra_campo <- muestra |> 
  filter(!is.na(campo)) |> 
  select(-c(pik,Factor_de_expansion_ESCUELAS,orden_seleccion,ID_ESCUELA,ELEGIBLE)) |> 
  rename(codigo = CodEstablecimiento)

muestra_cuali <- muestra_campo |> 
  filter(campo == "aplicadores")

muestra_cuali <- clean_names(muestra_cuali)


# arreglador de códigos

obs01$codigo <- gsub("\\.", "", obs01$codigo) # quita puntos


# corrección de mal codificados

## buscador de códigos correctos

muestra_cuali |> 
  filter(str_detect(str_to_lower(nombredelestablecimiento), "91")) %>%
  select(codigo, nombredelestablecimiento)


muestra_cuali |> 
  filter(str_detect(str_to_lower(municipio), "carcha")) %>%
  select(codigo, nombredelestablecimiento,departamento,municipio)


obs01 |> 
  filter(str_detect(nombre, "Núcleo Familiar Educativo para el Desarrollo NUFED No. 91")) %>%
  select(codigo, nombre)

## arreglar códigos

obs01$codigo[obs01$codigo == "20-05-00256-45"] <- "20-05-0256-45"
obs01$codigo[obs01$codigo == "1"] <- "16-09-0388-45"
obs01$codigo[obs01$codigo == "13-02-0214-45."] <- "13-02-0214-45"
obs01$codigo[obs01$codigo == "02-02-0705-45."] <- "02-02-0705-45"
obs01$codigo[obs01$codigo == "14-13-5914-45."] <- "14-13-5914-45"

# anti join ----

mal_codificados <-  obs01 |> 
  anti_join(muestra_cuali, by = "codigo") 



# capturar las varialbes 

info_centros <- c("index",
                  "id",
                  "uuid",
                  
                  "codigo_observador",
                  "codigo",
                  "nombre",
                  "departamento",
                  "municipio",
                  "sector",
                  "modalidad",
                  "plan",
                  "jornada",
                  "poblacion")

obs01_atributos <- obs01 |> 
  select(all_of(info_centros))

# merge el resto de códigos que hacen falta

muestra_cuali_codigos <- muestra_cuali["codigo"]

obs01_atributos <- full_join(obs01_atributos,muestra_cuali_codigos, by = "codigo")


# Generador de df cuali


variables_cualis <- c("comentario_energia_electrica",
                      "comentarios_agua",
                      "comentarios_banos_hombres",
                      "comentarios_banos_mujeres",
                      "comentarios_banos_mixtos",
                      "comentarios_seguridad_hombres",
                      "comentarios_seguridad_mujeres",
                      "comentarios_seguridad_banos_mixtos",
                      "comentarios_banos_limpieza",
                      "comentarios_falta_banos_centro",
                      "comentarios_lavado_manos",
                      "comentarios_gestion_riesgo",
                      "comentarios_laboratorio_tac",
                      "comentarios_educacion_fisica_espacio",
                      "comentarios_salon_musica",
                      "comentarios_salon_artes_visuales",
                      "comentarios_salon_teatro",
                      "comentarios_salon_danza",
                      "comentarios_laboratorio_ciencias",
                      "comentario_espacio_emprendimiento",
                      "comentarios_otros_salones",
                      "comentarios_donaciones_centro",
                      "comentarios_timbre_o_campana_centro",
                      "comentarios_cocina",
                      "comentarios_remozamiento",
                      "comentarios_observaciones_generales",
                      "comentarios_educacion_inclusiva",
                      "comentario_idioma_estudiantes",
                      "comentarios_aspectos_fisicos_aula",
                      "comentarios_mobiliario",
                      "comentario_adicional_relevante",
                      "comentario_fotografia",
                      "razon_sin_acceso_centro",
                      "incidente")

df_cuali_obs01 <- obs01 |> 
  select(all_of(c(info_centros,variables_cualis)))


# writexl

write_xlsx(obs01_atributos, here("Insumos cualis","obs01_atributos.xlsx"))
# write_xlsx(obs01_analisis, here("Datos","Listos para análisis","Observacion a nivel de centros", "obs01.xlsx"))

# write_xlsx(obs01, here("Datos","Observación a nivel de centro educativo", "obs01.xlsx"))

write_xlsx(df_cuali_obs01, here("Insumos cualis","df_cuali_obs01.xlsx"))
write_xlsx(muestra_cuali, here("Muestra","muestra_cuali.xlsx"))
write_xlsx(muestra_campo, here("Muestra","muestra_campo.xlsx"))

