df_est <- read_xlsx(here("Datos","Listos para análisis","Encuesta estudiantes","df_est.xlsx"))

# demográficos para todos

est_demograficos <- c("codigo",
"sector",
"modalidad",
"plan",
"jornada",
"departamento",
"participante",
"modalidad_encuesta",
"sexo",
"edad",
"autoidentificacion",
"grado",
"repitencia",
"dificultad_ver",
"dificultad_oir",
"dificultad_caminar",
"dificultad_recordar",
"dificultad_vestirse",
"dificultad_comunicarse",
"interrupcion_encuesta",
# "musica_recibes_clase",
# "musica_tiempo_recibe_clase",
# "musica_otro_tiempo_recibe_clase",
# "visuales_recibes_clase",
# "visuales_tiempo_recibe_clase",
# "visuales_otro_tiempo_recibe_clase",
# "teatro_recibes_clase",  
# "teatro_tiempo_recibe_clase",
# "teatro_otro_tiempo_recibe_clase",
# "danza_recibes_clase",               
# "danza_tiempo_recibe_clase",
# "danza_otro_tiempo_recibe_clase",
"index")

# labels general

labels_est <- read_xlsx(here("datasets para procesamiento","labels items estudiantes.xlsx"))

# Ciencias Naturales ----

df_cn_long <- purrr::map_dfr(
  list(
    c("Primero básico", "cn1", 1, "Ciencias Naturales"),
    c("Segundo básico", "cn2", 1, "Ciencias Naturales"),
    c("Tercero básico", "cn3", 1, "Ciencias Naturales")
  ),
  ~ prep_cn_grado(
    grado_txt     = .x[1],
    cn_tag        = .x[2],
    n_estudiantes = as.numeric(.x[3]),
    area_items    = .x[4]
  ))

df_cn_long$area <- "Ciencias Naturales"

df_cn_long <- df_cn_long |> 
  filter(!is.na(respuesta_likert))

# Ciencias Sociales ----

df_cs_long <- purrr::map_dfr(
  list(
    c("Primero básico", "cs1", 2, "Ciencias Sociales,"),
    c("Segundo básico", "cs2", 2, "Ciencias Sociales,"),
    c("Tercero básico", "cs3", 2, "Ciencias Sociales,")
  ),
  ~ prep_cn_grado(
    grado_txt     = .x[1],
    cn_tag        = .x[2],
    n_estudiantes = as.numeric(.x[3]),
    area_items    = .x[4]
  ))

df_cs_long$area <- "Ciencias Sociales, Formación Ciudadana e Interculturalidad"

df_cs_long <- df_cs_long |> 
  filter(!is.na(respuesta_likert))

# Comunicación y lenguaje ----

df_cl_long <- purrr::map_dfr(
  list(
    c("Primero básico", "cl1", 3, "Comunicación y Lenguaje Idioma Español"),
    c("Segundo básico", "cl2", 3, "Comunicación y Lenguaje Idioma Español"),
    c("Tercero básico", "cl3", 3, "Comunicación y Lenguaje Idioma Español")
  ),
  ~ prep_cn_grado(
    grado_txt     = .x[1],
    cn_tag        = .x[2],
    n_estudiantes = as.numeric(.x[3]),
    area_items    = .x[4]
  ))

df_cl_long$area <- "Comunicación y Lenguaje Idioma Español"

df_cl_long <- df_cl_long |> 
  filter(!is.na(respuesta_likert))

# Idioma extranjero ----

df_in_long <- purrr::map_dfr(
  list(
    c("Primero básico", "in1", 4, "Idioma Extranjero Inglés"),
    c("Segundo básico", "in2", 4, "Idioma Extranjero Inglés"),
    c("Tercero básico", "in3", 4, "Idioma Extranjero Inglés")
  ),
  ~ prep_cn_grado(
    grado_txt     = .x[1],
    cn_tag        = .x[2],
    n_estudiantes = as.numeric(.x[3]),
    area_items    = .x[4]
  ))

df_in_long$area <- "Comunicación y Lenguaje Idioma Extranjero Inglés"

df_in_long <- df_in_long |> 
  filter(!is.na(respuesta_likert))

# Culturas e Idiomas Mayas, Garífuna o Xinka ----

df_cm_long <- purrr::map_dfr(
  list(
    c("Primero básico", "cm1", 5, "Culturas e Idiomas Mayas, Garífuna o Xinka"),
    c("Segundo básico", "cm2", 5, "Culturas e Idiomas Mayas, Garífuna o Xinka"),
    c("Tercero básico", "cm3", 5, "Culturas e Idiomas Mayas, Garífuna o Xinka")
  ),
  ~ prep_cn_grado(
    grado_txt     = .x[1],
    cn_tag        = .x[2],
    n_estudiantes = as.numeric(.x[3]),
    area_items    = .x[4]
  ))

df_cm_long$area <- "Culturas e Idiomas Mayas, Garífuna o Xinka"

df_cm_long <- df_cm_long |> 
  filter(!is.na(respuesta_likert))

# Educación Artística - Música ----

df_musica_long <- purrr::map_dfr(
  list(
    c("Primero básico", "musica1", 2, "(Educación Musical)"),
    c("Segundo básico", "musica2", 2, "(Educación Musical)"),
    c("Tercero básico", "musica3", 2, "(Educación Musical)")
  ),
  ~ prep_cn_grado(
    grado_txt     = .x[1],
    cn_tag        = .x[2],
    n_estudiantes = as.numeric(.x[3]),
    area_items    = .x[4]
  ))

df_musica_long$area <- "Educación Musical"

df_musica_long <- df_musica_long |> 
  filter(!is.na(respuesta_likert))

# Educación Artística - Artes visuales ----

df_visuales_long <- purrr::map_dfr(
  list(
    c("Primero básico", "visuales1", 2, "(Artes Visuales)"),
    c("Segundo básico", "visuales2", 2, "(Artes Visuales)"),
    c("Tercero básico", "visuales3", 2, "(Artes Visuales)")
  ),
  ~ prep_cn_grado(
    grado_txt     = .x[1],
    cn_tag        = .x[2],
    n_estudiantes = as.numeric(.x[3]),
    area_items    = .x[4]
  ))

df_visuales_long$area <- "Artes Visuales"

df_visuales_long <- df_visuales_long |> 
  filter(!is.na(respuesta_likert))

# Educación Artística - Teatro ----

df_teatro_long <- purrr::map_dfr(
  list(
    c("Primero básico", "teatro1", 2, "(Teatro)"),
    c("Segundo básico", "teatro2", 2, "(Teatro)"),
    c("Tercero básico", "teatro3", 2, "(Teatro)")
  ),
  ~ prep_cn_grado(
    grado_txt     = .x[1],
    cn_tag        = .x[2],
    n_estudiantes = as.numeric(.x[3]),
    area_items    = .x[4]
  ))

df_teatro_long$area <- "Teatro"

df_teatro_long <- df_teatro_long |> 
  filter(!is.na(respuesta_likert))

# Educación Artística - Danza ----

df_danza_long <- purrr::map_dfr(
  list(
    c("Primero básico", "danza1", 2, "(Danza)"),
    c("Segundo básico", "danza2", 2, "(Danza)"),
    c("Tercero básico", "danza3", 2, "(Danza)")
  ),
  ~ prep_cn_grado(
    grado_txt     = .x[1],
    cn_tag        = .x[2],
    n_estudiantes = as.numeric(.x[3]),
    area_items    = .x[4]
  ))

df_danza_long$area <- "Danza"

df_danza_long <- df_danza_long |> 
  filter(!is.na(respuesta_likert))

# Educación Física ----

df_fisica_long <- purrr::map_dfr(
  list(
    c("Primero básico", "fisica1", 5, "Educación Física"),
    c("Segundo básico", "fisica2", 5, "Educación Física"),
    c("Tercero básico", "fisica3", 5, "Educación Física")
  ),
  ~ prep_cn_grado(
    grado_txt     = .x[1],
    cn_tag        = .x[2],
    n_estudiantes = as.numeric(.x[3]),
    area_items    = .x[4]
  ))

df_fisica_long$area <- "Educación Física"

df_fisica_long <- df_fisica_long |> 
  filter(!is.na(respuesta_likert))

# Emprendimiendo para la productividad ----

df_emp_long <- purrr::map_dfr(
  list(
    c("Primero básico", "emp1", 1, "Emprendimiento para la Productividad"),
    c("Segundo básico", "emp2", 1, "Emprendimiento para la Productividad"),
    c("Tercero básico", "emp3", 1, "Emprendimiento para la Productividad")
  ),
  ~ prep_cn_grado(
    grado_txt     = .x[1],
    cn_tag        = .x[2],
    n_estudiantes = as.numeric(.x[3]),
    area_items    = .x[4]
  ))

df_emp_long$area <- "Emprendimiento para la Productividad"

df_emp_long <- df_emp_long |> 
  filter(!is.na(respuesta_likert))

# Matemáticas ----

df_mate_long <- purrr::map_dfr(
  list(
    c("Primero básico", "mate1", 4, "Matemáticas"),
    c("Segundo básico", "mate2", 4, "Matemáticas"),
    c("Tercero básico", "mate3", 4, "Matemáticas")
  ),
  ~ prep_cn_grado(
    grado_txt     = .x[1],
    cn_tag        = .x[2],
    n_estudiantes = as.numeric(.x[3]),
    area_items    = .x[4]
  ))

df_mate_long$area <- "Matemáticas"

df_mate_long <- df_mate_long |> 
  filter(!is.na(respuesta_likert))

# TAC ----

df_tac_long <- purrr::map_dfr(
  list(
    c("Primero básico", "tac1", 3, "Tecnologías del Aprendizaje y la Comunicación -TAC-"),
    c("Segundo básico", "tac2", 3, "Tecnologías del Aprendizaje y la Comunicación -TAC-"),
    c("Tercero básico", "tac3", 3, "Tecnologías del Aprendizaje y la Comunicación -TAC-")
  ),
  ~ prep_cn_grado(
    grado_txt     = .x[1],
    cn_tag        = .x[2],
    n_estudiantes = as.numeric(.x[3]),
    area_items    = .x[4]
  ))

df_tac_long$area <- "Tecnologías del Aprendizaje y la Comunicación -TAC-"

df_tac_long <- df_tac_long |> 
  filter(!is.na(respuesta_likert))

write_xlsx(list("a_df_est" = df_est,
                "b_ciencias" = df_cn_long,
                "c_sociales" = df_cs_long,
                "d_espanol" = df_cl_long,
                "n_idextranjero" = df_in_long,
                "e_culturas" = df_cm_long,
                "f_musica" = df_musica_long,
                "g_visuales" = df_visuales_long,
                "h_teatro" = df_teatro_long,
                "i_danza" = df_danza_long,
                "j_fisica" = df_fisica_long,
                "k_emprendimiento" = df_emp_long,
                "l_matematicas" = df_mate_long,
                "m_tac" = df_tac_long), here("Datos","Listos para análisis", "Encuesta estudiantes", "df_est_long.xlsx"))


todos_longs <- list(df_cn_long,
  df_cs_long,
  df_cl_long,
  df_in_long,
  df_cm_long,
  df_musica_long,
  df_visuales_long,
  df_teatro_long,
  df_danza_long,
  df_fisica_long,
  df_emp_long,
  df_mate_long,
  df_tac_long)

total_rows <- todos_longs |>
  map_int(nrow) |>
  sum()

df_all_longs <- do.call(rbind, todos_longs)

write_xlsx(list("df_est" = df_est,
                "df_est_long" = df_all_longs), here("Datos","Listos para análisis", "Encuesta estudiantes", "df_est_long_long_long.xlsx"))
