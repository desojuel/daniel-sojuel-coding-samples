df_dyd <- read_xlsx(here("Datos","Listos para análisis","Encuesta docente","df_dyd.xlsx"))


# función

pivot_secciones <- function(seccion_var) {
  
  names_sel <- df_dyd |>
    select(ends_with(seccion_var)) |>
    names()
  
  df_dyd |>
    select(all_of(c(dyd_demograficos, names_sel))) |>
    pivot_longer(
      cols = all_of(names_sel),
      names_to  = "name",
      values_to = "valores"
    ) |>
    filter(!is.na(.data[["valores"]])) |> 
  mutate(variable = seccion_var)
}


dyd_demograficos <- c("sexo",
                      "autoidentificacion",
                      "departamento",
                      "sector",
                      "modalidad",
                      "plan",
                      "area",
                      "lenguajes_educacion_artistica",
                      "index")


# var names new

labels_dyd <- read_xlsx(here("datasets para procesamiento","labels docentes 1.xlsx"))

names_dyd <- df_dyd |> select(ends_with("_grados")) |> 
  names()

## grados

df_dyd_long_grados <- pivot_secciones("grados")
  
## secciones primero

df_dyd_long_secciones_primero <- pivot_secciones("secciones_primero")
  
## secciones segundo

df_dyd_long_secciones_segundo <- pivot_secciones("secciones_segundo")

## secciones tercero

df_dyd_long_secciones_tercero <- pivot_secciones("secciones_tercero")

## recursos_centro_educativo

df_dyd_long_recursos_centro_educativo <- pivot_secciones("recursos_centro_educativo") 
## recursos_propios

df_dyd_long_recursos_propios <- pivot_secciones("recursos_propios")

## libros_mineduc

df_dyd_long_libros_mineduc <- pivot_secciones("libros_mineduc")

## ultima_vez_libros_mineduc

df_dyd_long_ultima_vez_libros_mineduc <- pivot_secciones("ultima_vez_libros_mineduc") 
## otro_ultima_vez_libros_mineduc

df_dyd_long_otro_ultima_vez_libros_mineduc <- pivot_secciones("otro_ultima_vez_libros_mineduc")

## recepcion_materiales__mineduc

df_dyd_long_recepcion_materiales__mineduc <- pivot_secciones("recepcion_materiales__mineduc") 

## tipo_materiales_recibidos

df_dyd_long_tipo_materiales_recibidos <- pivot_secciones("tipo_materiales_recibidos") 
## recepcion_materiales_apoyo

df_dyd_long_recepcion_materiales_apoyo <- pivot_secciones("recepcion_materiales_apoyo")

## ajustes

df_dyd_long_ajustes <- pivot_secciones("ajustes") 

todos_longs <- list(df_dyd_long_grados,
                    df_dyd_long_secciones_primero,
                    df_dyd_long_secciones_segundo,
                    df_dyd_long_secciones_tercero,
                    df_dyd_long_recursos_centro_educativo,
                    df_dyd_long_recursos_propios,
                    df_dyd_long_libros_mineduc,
                    df_dyd_long_ultima_vez_libros_mineduc,
                    df_dyd_long_otro_ultima_vez_libros_mineduc,
                    df_dyd_long_recepcion_materiales__mineduc,
                    df_dyd_long_tipo_materiales_recibidos,
                    df_dyd_long_recepcion_materiales_apoyo,
                    df_dyd_long_ajustes)

df_dyd_all_longs <- do.call(rbind, todos_longs)

df_dyd_all_longs$name <- gsub("_.*$", "", df_dyd_all_longs$name)

df_dyd_all_longs <- df_dyd_all_longs |> 
  mutate(area_impartida = case_match(name,
                                     "mate" ~ "Matemáticas",
                                     "teatro" ~ "Teatro",
                                     "danza" ~ "Danza",
                                     "musica" ~ "Música",
                                     "visuales" ~ "Artes Visuales",
                                     "emp" ~ "Emprendimiento para la Productividad",
                                     "cs" ~ "Ciencias Sociales, Formación Ciudadana e Interculturalidad",
                                     "cm" ~ "Culturas e Idiomas Mayas, Garífuna o Xinka",
                                     "cn" ~ "Ciencias Naturales",
                                     "cl" ~ "Comunicación y Lenguaje Idioma Español",
                                     "in" ~ "Comunicación y Lenguaje Idioma Extranjero",
                                     "tac" ~ "Tecnologías del Aprendizaje y la Comunicación -TAC-",
                                     "fisica" ~ "Educación Física"
                                     ))


df_dyd_all_longs <- df_dyd_all_longs |>
  mutate(
    primer_grado  = if_else(str_detect(valores, "Primer grado"),  "Primer grado", NA_character_),
    segundo_grado = if_else(str_detect(valores, "Segundo grado"), "Segundo grado", NA_character_),
    tercer_grado  = if_else(str_detect(valores, "Tercer grado"),  "Tercer grado", NA_character_)
  )


write_xlsx(df_dyd_all_longs, here("Datos", "Listos para análisis", "Encuesta docente", "df_dyd_long1.xlsx"))
           