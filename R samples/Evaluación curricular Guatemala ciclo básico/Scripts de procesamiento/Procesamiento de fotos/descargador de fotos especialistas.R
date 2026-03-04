library(pacman)
pacman::p_load("tidyverse",
               "readxl",
               "httr",
               "tools",
               "here")

# leer archivos ----

obs03 <- read_xlsx(here("Datos","Listos para análisis","Observacion de especialistas","obs03.xlsx"))

rename_folders <- obs03 |> 
  select(actual = uuid,
         codigo,
         grado,
         area,
         index)

rename_folders <- rename_folders |> 
  filter(!is.na(area))

fotos_descargar <- read_xlsx(
  here("Datos","Observación especialistas","observacion especialistas 25.11.25 con versiones.xlsx"),
  sheet = "group_rx2gm75",
  col_types = "text"
)

fotos_descargar <- fotos_descargar |> 
  select(categoria = `71. Categoría de fotografías del desarrollo del área`,
         otra_categoria_especificar = `72. Otra categoría (especificar)`,
         extension = `73. Cargar fotografía`,
         url = `73. Cargar fotografía_URL`,
         por_que_es_relevante = `74. ¿Por qué esta fotografía es relevante desde su perspectiva como especialista?`,
         actual = `_submission__uuid`) |> 
  filter(!is.na(url))

# rename categorías

## imputar na con el anterior ----
## (esto sucedió porque cuando subieron 2 fotos de una misma categoría, por eso es válido imputar con el anterior)

fotos_descargar <- fotos_descargar |> 
  fill(categoria, .direction = "down")

fotos_descargar <- fotos_descargar |> 
  mutate(categoria = case_match(categoria,
                                "Cuadernos" ~ "01. Cuadernos",
                                "Pizarrón" ~ "02. Pizarrón",
                                "Cuadro de notas del docente" ~ "03. Cuadro de notas del docente",
                                "Otras evidencias del mejoramiento del aprendizaje" ~ "04. Otras evidencias del mejoramiento del aprendizaje",
                                "Libros utilizados" ~ "05. Libros utilizados",
                                "Materiales tecnológicos utilizados" ~ "06. Materiales tecnológicos utilizados",
                                "Mobiliario" ~ "07. Mobiliario",
                                "Diario pedagógico" ~ "08. Diario pedagógico",
                                "Otra categoría" ~ "09. Otra categoría",
                                "Planificaciones" ~ "10. Planificaciones"))

# concatenar otra categoría

rename_files_fotos_periodos <- fotos_descargar |> 
  mutate(
    file_name = case_when(
      categoria == "09. Otra categoría" ~ str_c(categoria, ". ", otra_categoria_especificar),
      TRUE ~ categoria
    ) 
  )

# código descargador ----

base_dir <- "fotos_especialistas"

# Tu token de Kobo
token <- "566d26556e65f60e67c1ce4d9d71000186b28939"


# Unir la información de archivos con la información de carpetas
# (usamos la variable 'actual' como llave)
df_final <- rename_files_fotos_periodos %>%
  left_join(rename_folders, by = "actual")


df_final <- df_final |> 
  mutate(id_for_files = seq(1:132)) |> 
  mutate(
    file_name = str_c(file_name, " ", id_for_files)
  )

# Ahora usamos pwalk para descargar
pwalk(
  list(df_final$url, df_final$extension, df_final$file_name, df_final$codigo, df_final$grado, df_final$area),
  function(url, extension, file_name, codigo, grado, area) {
    # 1️⃣ Crear estructura de carpetas: codigo -> grado -> area
    dir_path <- file.path(base_dir, codigo, grado, area)
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
    
    # 2️⃣ Extraer extensión del URL
    ext <- tools::file_ext(extension)
    if (ext == "") ext <- "jpg"  # fallback por si no la detecta
    
    # 3️⃣ Construir ruta completa del archivo
    file_path <- file.path(dir_path, paste0(file_name, ".", ext))
    
    # 4️⃣ Descargar el archivo
    tryCatch({
      GET(
        url,
        add_headers(Authorization = paste("Token", token)),
        write_disk(file_path, overwrite = TRUE)
      )
      message("Descargado: ", file_path)
    }, error = function(e) {
      message("❌ Error al descargar ", url, ": ", e$message)
    })
  }
)

fotos_word_obs03 <- df_final |> 
  select(codigo,
         grado,
         area,
         file_name,
         por_que_es_relevante
         )

write_xlsx(fotos_word_obs03, here("Insumos cualis","descripcion_fotos_word_obs03.xlsx"))


