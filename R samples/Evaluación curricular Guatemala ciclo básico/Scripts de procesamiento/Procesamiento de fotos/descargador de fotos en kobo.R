library(pacman)
pacman::p_load("here",
               "tidyverse",
               "readxl",
               "httr",
               "tools")

# leer archivos ----

rename_folders <- obs01 |> 
  select(actual = uuid,
         nuevo = codigo)


fotos_obs01 <- read_xlsx(here("Datos","Observación a nivel de centro educativo","observacion centro 18.11.25.xlsx"),sheet="group_gv7hd99")


rename_files_fotos <- fotos_obs01 |> 
  select(categoria = `297. Categoría de fotografía`,
         otra_categoria_especificar = `298. Otra categoría (especificar)`,
         extension = `299. Cargar fotografía`,
         url = `299. Cargar fotografía_URL`,
         actual = `_submission__uuid`) |> 
  filter(actual %in% obs01$uuid)


# renombrar la variable y los valores de categorías ----

rename_files_fotos <- rename_files_fotos |> 
  mutate(categoria = case_match(categoria,
                                "Consentimiento informado" ~ "01. Consentimiento informado",
                                "Fachada del Centro Educativo" ~ "02. Fachada del Centro Educativo",
                                "Energía eléctrica" ~ "03. Energía eléctrica",
                                "Baños disponibles para estudiantes (hombres)" ~ "04. Baños disponibles para estudiantes (hombres)",
                                "Baños disponibles para estudiantes (mujeres)" ~ "05. Baños disponibles para estudiantes (mujeres)",
                                "Cubículo de baño para estudiantes con movilidad o visión reducida" ~ "06. Cubículo de baño para estudiantes con movilidad o visión reducida",
                                "Instalaciones para el lavado de manos" ~ "07. Instalaciones para el lavado de manos",
                                "Laboratorio del área de tecnología (TAC)" ~ "08. Laboratorio del área de tecnología (TAC)",
                                "Espacio para el área de Educación Física" ~ "09. Espacio para el área de Educación Física",
                                "Salón de Educación Musical" ~ "10. Salón de Educación Musical",
                                "Salón de Artes Visuales" ~ "11. Salón de Artes Visuales",
                                "Salón de Teatro" ~ "12. Salón de Teatro",
                                "Salón de Danza" ~ "13. Salón de Danza",
                                "Salón de la sección asignada en un momento donde no haya niños o niñas presentes" ~ "14. Salón de la sección asignada en un momento donde no haya niños o niñas presentes",
                                "Mobiliario de la sección asignada" ~ "15. Mobiliario de la sección asignada",
                                "Horario establecido en el Centro Educativo para la sección asignada para observar" ~ "16. Horario establecido en el Centro Educativo para la sección asignada para observar",
                                "Almuerzo o refacción que brinda el Centro Educativo" ~ "17. Almuerzo o refacción que brinda el Centro Educativo",
                                "Cocina" ~ "18. Cocina",
                                "Recursos tecnológicos del Centro Educativo" ~ "19. Recursos tecnológicos del Centro Educativo",
                                "Infraestructura para personas con discapacidad o limitación de movilidad" ~ "20. Infraestructura para personas con discapacidad o limitación de movilidad",
                                "Otra categoría" ~ "21. Otra categoría"))





# imputar na con el anterior ----
## (esto sucedió porque cuando subieron 2 fotos de una misma categoría, por eso es válido imputar con el anterior)

rename_files_fotos <- rename_files_fotos |> 
  fill(categoria, .direction = "down")

# crear variable con las categorías oficiales que luego se usarán para renombrar archivos ----

rename_files_fotos <- rename_files_fotos |> 
  mutate(
    new_file_name = case_when(
      categoria == "21. Otra categoría" ~ str_c(categoria, ". ", otra_categoria_especificar),
      TRUE ~ categoria
    )
  )




# código descargador ----

base_dir <- "descargas_fotos"

# Tu token de Kobo
token <- "566d26556e65f60e67c1ce4d9d71000186b28939"

# Descargar todos los archivos
pwalk(
  list(rename_files_fotos$extension,
       rename_files_fotos$url,
       rename_files_fotos$new_file_name, 
       rename_files_fotos$actual),
  function(extension, url, new_file_name, actual) {
    
    # 1. Crear carpeta si no existe
    dir_path <- file.path(base_dir, actual)
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
    
    # 2. Extraer la extensión del archivo desde el URL
    ext <- file_ext(extension)  # "jpg", "heic", "png", etc.
    file_path <- file.path(dir_path, paste0(new_file_name, ".", ext))
    
    # 3. Descargar archivo usando el token
    tryCatch({
      httr::GET(
        url,
        add_headers(Authorization = paste("Token", token)),
        write_disk(file_path, overwrite = TRUE)
      )
      message("Descargado: ", file_path)
    }, error = function(e) {
      message("Error al descargar ", url, ": ", e$message)
    })
  }
)

# renombrar carpetas ----

# Iterar sobre cada fila de rename_folders
rename_folders %>%
  pwalk(function(actual, nuevo) {
    
    # Ruta actual y nueva
    old_path <- file.path(base_dir, actual)
    new_path <- file.path(base_dir, nuevo)
    
    # Verificar si la carpeta actual existe
    if (dir.exists(old_path)) {
      
      # Renombrar carpeta
      success <- file.rename(old_path, new_path)
      
      if (success) {
        message("Carpeta renombrada: ", old_path, " → ", new_path)
      } else {
        message("Error al renombrar: ", old_path)
      }
      
    } else {
      message("No existe la carpeta: ", old_path)
    }
  })
               
                                       
                                                                                                                  
