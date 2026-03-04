# merge salones ----

obs01_salones_ad <- obs01_rost_salones_ad |> 
  select(c(salon_adicional,
           area_salon_adicional,
           index = parent_index)) |> 
  mutate(grupo = "principal",
         vinculador = "15. Otros salones") |> 
  pivot_longer(c(salon_adicional, area_salon_adicional),
               names_to = "variables1",
               values_to = "valores1") |> 
  mutate(label = case_when(
                            variables1 == "area_salon_adicional" ~ "¿Para qué área o áreas se utiliza el otro salón o espacio observado?",
                            variables1 == "salon_adicional" ~ "Tipo del salón, laboratorio o espacio adicional observado"
  )) |> 
  mutate(vinculador2 = case_when(
    label == "¿Para qué área o áreas se utiliza el otro salón o espacio observado?" ~ "Área salón adicional",
    label == "Tipo del salón, laboratorio o espacio adicional observado" ~ "Salón adicional"
  ))



demograficos_salones_ad <- obs01_salones_long_completo |> 
  select(-c(valores1, valores2, variables1, variables2, label, vinculador,vinculador2, grupo)) |> 
  distinct()

obs01_salones_ad <- obs01_salones_ad |> 
  left_join(demograficos_salones_ad, by = "index")

obs01_salones_full <- bind_rows(obs01_salones_long_completo,obs01_salones_ad)

# merge areas y periodos ----

obs01_areas_impartidas$periodos <- as.numeric(obs01_areas_impartidas$periodos)

obs01_rost_areas_ad <- obs01_rost_areas_ad |> 
  select(c(areas_impartidas = nombre_area,
           periodos = periodos_otra_area,
           index = parent_index,
           debug)) 


lookup_index <- obs01_areas_impartidas |>
  select(-c(areas_impartidas, periodos)) |> 
  distinct()


obs01_rost_areas_ad <- obs01_rost_areas_ad |>
  left_join(lookup_index, by = "index")

obs01_areas_full <- bind_rows(obs01_areas_impartidas,
                     obs01_rost_areas_ad)

obs01_areas_full <- obs01_areas_full |>
  mutate(
    total_children = as.numeric(total_children),
    duracion_periodo_clase = coalesce(otra_duracion_periodo_clase, duracion_periodo_clase),
    total_children = if_else(
      is.na(hombres),
      coalesce(mujeres, total_children),
      total_children
    ))

# debugging

bad_adicionales <- c("Ciencias Naturales",
                     "Ciencias Sociales",
                     "Comunicación y Lenguaje",
                     "Educación Física",
                     "Expresión Artística",
                     "Idiomas",
                     "Ciencias Sociales Formación Ciudadana e Interculturalidad")

obs01_areas_full <- obs01_areas_full |> 
  mutate(index_for_deletion = c(1:nrow(obs01_areas_full))) |> 
  mutate(bad_case = if_else(areas_impartidas %in% bad_adicionales & !is.na(debug),
                            1,
                            NA
                            )
           )

obs01_areas_full_bad_cases <- obs01_areas_full |> 
  filter((bad_case == 1 &
         !is.na(debug)) |
         areas_impartidas %in% bad_adicionales) |> 
  group_by(codigo,areas_impartidas) |> 
  filter(n() >= 2) |>
  ungroup()

index_for_deletion_out <- c(1269,1272,1275,1276,1307,1309,1336,1338,1340,1345,1350,1354)

obs01_areas_full <- obs01_areas_full |> 
  filter(!(index_for_deletion %in% index_for_deletion_out)) |> 
  select(-c(index_for_deletion, bad_case))

obs01_areas_full <- obs01_areas_full |> 
  mutate(areas_impartidas = case_match(areas_impartidas,
    "Ciencias Sociales" ~ "Ciencias Sociales Formación Ciudadana e Interculturalidad",
    .default = areas_impartidas
  ))

# merge docentes ----

# docentes roster ----

# labels

df_labels_obs01 <- read_xlsx(here("datasets para procesamiento/labels obs01 long roster char docentes.xlsx"))

labels_char_obs01 <- df_labels_obs01$name_char[!is.na(df_labels_obs01$name_char)]

# select variables 

obs01_char_docentes_long1 <- obs01_rost_car_doc |> 
  select(all_of(c(index = "parent_index", 
                  area = "caracteristicas_docente_area",
                  lenguaje_educacion_artistica = "lenguaje_educacion_artistica",
                  labels_char_obs01))) |> 
  pivot_longer(all_of(labels_char_obs01), names_to = "variables1",
               values_to = "valores1") 

df_labels_obs01_1 <- df_labels_obs01 |> 
  rename(variables1 = name_char)

obs01_char_docentes_long <- obs01_char_docentes_long1 |> 
  left_join(df_labels_obs01_1,by = "variables1")

lookup_index_docentes <- obs01 |>
  select(all_of(obs01_demograficos)) |> 
  distinct()

obs01_char_docentes_long <- obs01_char_docentes_long |> 
  left_join(lookup_index_docentes, by = "index")

obs01_docentes_profesionales_long <- bind_rows(obs01_char_docentes_long,
                                               obs01_otros_pro_long)

obs01_docentes_profesionales_long <- obs01_docentes_profesionales_long |> 
  filter(!(vinculador == "Características docentes" &
           is.na(area)
           )
         )

# exports ----

write_xlsx(obs01_salones_full, here("Datos/Listos para análisis/Observacion a nivel de centros/obs01_salones_full.xlsx"))

write_xlsx(obs01_areas_full, here("Datos/Listos para análisis/Observacion a nivel de centros/obs01_areas_full.xlsx"))

write_xlsx(obs01_docentes_profesionales_long, here("Datos/Listos para análisis/Observacion a nivel de centros/obs01_docentes_full.xlsx"))


