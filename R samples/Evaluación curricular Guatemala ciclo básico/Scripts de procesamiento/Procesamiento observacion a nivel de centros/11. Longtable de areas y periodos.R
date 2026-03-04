# demográficos

obs01_char_salones <- c("grado",
                        "hombres",
                        "mujeres",
                        "total_children",
                        "grado_salon_propio",
                        "duracion_periodo_clase",
                        "otra_duracion_periodo_clase")


obs01_areas_periodos <- c("periodos_ciencias_naturales_semana",
                          "periodos_sociales_semana",
                          "periodos_espanol_semana",
                          "periodos_ingles_semana",
                          "periodos_cultura_maya_semana",
                          "periodos_artistica_integrada_semana",
                          "periodos_musica_semana",
                          "periodos_visuales_semana",
                          "periodos_teatro_semana",
                          "periodos_danza_semana",
                          "periodos_educacion_fisica_semana",
                          "periodos_emprendimiento_semana",
                          "periodos_matematicas_semana",
                          "periodos_tac_semana"
                          )

obs01_areas_otro_periodos <- c(
  "otro_periodos_ciencias_naturales_semana",
  "otro_periodos_sociales_semana",
  "otro_periodos_espanol_semana",
  "otro_periodos_ingles_semana",
  "otro_periodos_cultura_maya_semana",
  "otro_periodos_artistica_integrada_semana",
  "otro_periodos_musica_semana",
  "otro_periodos_visuales_semana",
  "otro_periodos_teatro_semana",
  "otro_periodos_danza_semana",
  "otro_periodos_educacion_fisica_semana",
  "otro_periodos_emprendimiento_semana",
  "otro_periodos_matematicas_semana",
  "otro_periodos_tac_semana"
)


# select variables

obs01_areas_impartidas <- obs01 |> 
  select(all_of(c(obs01_demograficos,
           obs01_char_salones,
           "areas_impartidas",
           obs01_areas_periodos,
           obs01_areas_otro_periodos))) 


rango_vars <- match("periodos_ciencias_naturales_semana", colnames(obs01_areas_impartidas)):match("periodos_tac_semana", colnames(obs01_areas_impartidas))

rango_vars_otros <- match("otro_periodos_ciencias_naturales_semana", colnames(obs01_areas_impartidas)):match("otro_periodos_tac_semana", colnames(obs01_areas_impartidas))

# lontable por área
obs01_areas_impartidas <- obs01_areas_impartidas |> 
  separate_longer_delim(areas_impartidas, delim = ";") |> 
  filter(areas_impartidas != "Otro") 

# limpiar periodos solo para el área donde se necesita

obs01_areas_impartidas <- obs01_areas_impartidas |> 
  periodos_clean(periodos_ciencias_naturales_semana,
                 "Ciencias Naturales")

obs01_areas_impartidas <- obs01_areas_impartidas |> 
  periodos_clean(periodos_sociales_semana,
                 "Ciencias Sociales Formación Ciudadana e Interculturalidad")

obs01_areas_impartidas <- obs01_areas_impartidas |> 
  periodos_clean(periodos_espanol_semana,
                 "Idioma Español")

obs01_areas_impartidas <- obs01_areas_impartidas |> 
  periodos_clean(periodos_ingles_semana,
                 "Idioma Extranjero Inglés")

obs01_areas_impartidas <- obs01_areas_impartidas |> 
  periodos_clean(periodos_cultura_maya_semana,
                 "Culturas e Idiomas Mayas, Garífuna o Xinka")

obs01_areas_impartidas <- obs01_areas_impartidas |> 
  periodos_clean(periodos_educacion_fisica_semana,
                 "Educación Física")

obs01_areas_impartidas <- obs01_areas_impartidas |> 
  periodos_clean(periodos_matematicas_semana,
                 "Matemáticas")

obs01_areas_impartidas <- obs01_areas_impartidas |> 
  periodos_clean(periodos_tac_semana,
                 "Tecnologías del Aprendizaje y la Comunicación -TAC-")

obs01_areas_impartidas <- obs01_areas_impartidas |> 
  periodos_clean(periodos_artistica_integrada_semana,
                 "Educación Artística (integrada)")

obs01_areas_impartidas <- obs01_areas_impartidas |> 
  periodos_clean(periodos_emprendimiento_semana,
                 "Emprendimiento para la productividad")

obs01_areas_impartidas <- obs01_areas_impartidas |> 
  periodos_clean(periodos_musica_semana,
                 "Educación Artística (Educación Musical)")

obs01_areas_impartidas <- obs01_areas_impartidas |> 
  periodos_clean(periodos_visuales_semana,
                 "Educación Artística (Artes Visuales)")

obs01_areas_impartidas <- obs01_areas_impartidas |> 
  periodos_clean(periodos_teatro_semana,
                 "Educación Artística (Teatro)")

obs01_areas_impartidas <- obs01_areas_impartidas |> 
  periodos_clean(periodos_danza_semana,
                 "Educación Artística (Danza)")


# coalece 

obs01_areas_impartidas <- obs01_areas_impartidas |> 
  mutate(
    periodos_d = across(all_of(rango_vars)) |> 
      reduce(coalesce),
    otros_periodos = across(all_of(rango_vars_otros)) |> reduce(coalesce)
  ) |> 
  relocate(periodos_d, .after = areas_impartidas) |> 
  select(-(all_of(c(obs01_areas_periodos,obs01_areas_otro_periodos))))

obs01_areas_impartidas <- obs01_areas_impartidas |>
  mutate(
    periodos = as.numeric(
      if_else(
        as.character(periodos_d) == "Otro",
        as.character(otros_periodos),
        as.character(periodos_d)
      )
    )
  ) |>
  select(-periodos_d, -otros_periodos)