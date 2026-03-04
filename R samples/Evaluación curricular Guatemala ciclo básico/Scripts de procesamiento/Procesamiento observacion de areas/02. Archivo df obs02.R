# Generador de atributos para el proceso cuali

# lecturas 

obs02 <- read_xlsx(here("Datos","Observación aulas","obs02.xlsx"))

# limpieza por NA

# muestra completa y cuali

muestra_cuali <- read_xlsx(here("Muestra","muestra_cuali.xlsx"))


# arreglador de códigos

obs02$codigo <- gsub("\\.", "", obs02$codigo) # quita puntos
obs02$codigo <- trimws(obs02$codigo) # Quita espacios y saltos de línea al inicio y al final de cada valor
obs02$codigo <- gsub("\n", "", obs02$codigo) # También elimina saltos de línea internos si los hubiera
obs02$codigo <- gsub("\r", "", obs02$codigo) # También elimina saltos de línea internos si los hubiera
obs02$codigo <- gsub("[[:space:][:cntrl:]]", "", obs02$codigo) #elimina todos los espacios (incluidos tabs, saltos de línea, espacios raros y retornos)
obs02$codigo <- gsub("^([0-9]{2}-[0-9]{2})([0-9]{4}-[0-9]{2})$", "\\1-0\\2", obs02$codigo)
obs02$codigo <- gsub(" ", "", obs02$codigo) #Elimina espacios invisibles


## buscador de códigos correctos

muestra_cuali |> 
  filter(str_detect(str_to_lower(nombredelestablecimiento), "olam")) %>%
  select(codigo, nombredelestablecimiento)

muestra_cuali |> 
  filter(str_detect(str_to_lower(codigo), "01260")) %>%
  select(codigo, nombredelestablecimiento,departamento,municipio)

obs02 |> 
  filter(str_detect(str_to_lower(codigo), "22-05-2331-45")) %>%
  select(codigo, nombre, codigo_observador)

## arreglar códigos

obs02$codigo[obs02$codigo == "COLEGIO VALVERDE"] <- "00-01-9531-45"
obs02$codigo[obs02$codigo == "COLEGIOVALVERDE"] <- "00-01-9531-45"
obs02$codigo[obs02$codigo == "20-05-0246-45"] <- "20-05-0256-45"
obs02$codigo[obs02$nombre == "Colegio Cristiano El Olam"] <- "20-05-0256-45"
obs02$codigo[obs02$codigo == "00058194-45"] <- "00-05-8194-45"
obs02$codigo[obs02$codigo == "12-021260-45"] <- "12-02-1260-45"
obs02$codigo[obs02$codigo == "O7-05-0569-45"] <- "07-05-0569-45"
obs02$codigo[obs02$codigo == "13-24-0040+45"] <- "13-24-0040-45"
obs02$codigo[obs02$codigo == "13-24-0040-43"] <- "13-24-0040-45"
obs02$codigo[obs02$codigo == "03-05-9762-45"] <- "01-03-9762-45"
obs02$codigo[obs02$codigo == "07-13-0330-44"] <- "07-13-0330-45"
obs02$codigo[obs02$codigo == "16-011370-45"] <- "16-01-1370-45"
obs02$codigo[obs02$codigo == "16-01-2370-45"] <- "16-01-1370-45"
obs02$codigo[obs02$codigo == "07-13-03-0330-45"] <- "07-13-0330-45"
obs02$codigo[obs02$codigo == "09-01-0109-45"] <- "09-01-0129-45"
obs02$codigo[obs02$codigo == "01-14-0049- 45"] <- "01-14-0049-45"
obs02$codigo[obs02$codigo == "Q6-03-0071-45"] <- "16-03-0071-45"
obs02$codigo[obs02$codigo == "07-07-0732-45"] <- "07-04-0732-45"
obs02$codigo[obs02$codigo == "0w-02-0705-45"] <- "02-02-0705-45"
obs02$codigo[obs02$codigo == "O7-18-0686-45"] <- "07-18-0686-45"
obs02$codigo[obs02$codigo == "05-07-3187-45"] <- "04-07-3187-45"
obs02$codigo[obs02$codigo == "05-04-00-28-45"] <- "05-04-0028-45"
obs02$codigo[obs02$codigo %in% c("09-04-0449-45",
                                 "00-04-0449-45")] <- "05-04-0028-45"
obs02$codigo[obs02$codigo == "15-14-1027-45"] <- "16-14-1027-45"
obs02$codigo[obs02$codigo == "06-19-0013-45"] <- "06-10-0013-45"
obs02$codigo[obs02$codigo == "16-24-1027-45"] <- "16-14-1027-45"
obs02$codigo[obs02$codigo == "05-07- 0036-45"] <- "05-07-0036-45"
obs02$codigo[obs02$codigo == "16-08-1067-45"] <- "16-08-0167-45"
obs02$codigo[obs02$codigo == "08-07-003845"] <- "08-07-0038-45"
obs02$codigo[obs02$codigo == "06-14-0418-46"] <- "06-14-0418-45"
obs02$codigo[obs02$codigo == "04-12-0007-45"] <- "04-11-0007-45"
obs02$codigo[obs02$codigo =="17-00-3729-45"] <- "17-09-3729-45"
obs02$codigo[obs02$codigo =="04-0048-45"] <- "04-12-0048-45"
obs02$codigo[obs02$codigo %in% c("16-08-00167-45", "16-07-0167-45")] <- "16-08-0167-45"
obs02$codigo[obs02$codigo =="12-02-01260-45"] <- "12-02-1260-45"
obs02$nombre[obs02$codigo =="COLEGIOVALVERDE"] <- "COLEGIO VALVERDE"
obs02$codigo[obs02$codigo =="COLEGIOVALVERDE"] <- "00-01-9531-45"
obs02$codigo[obs02$codigo =="16-01-01370-45"] <- "16-01-1370-45"
obs02$codigo[obs02$codigo =="17-10-4951-45"] <- "17-10-4051-45"



obs02$incidente[obs02$codigo == "06-09-1229-45"] <- "Durante la semana de observación no se pudo observar todas las áreas porque estaban en exámenes bimestrales y los docentes se encontraban calificando tareas atrasadas y cuadernos"
obs02$codigo[obs02$codigo == "12-02-01260-45"] <- "12-02-1260-45"
obs02$codigo[obs02$codigo == "16-01-01370-45"] <- "16-01-1370-45"
obs02$codigo[obs02$codigo == "10-01-0078-45"] <- "10-01-0070-45"

obs02$codigo[obs02$index == 465] <- "15-01-0101-45"
obs02$codigo[obs02$index == 667] <- "20-05-0256-45"

# elimna na

obs02 <- obs02 |> 
  filter(!is.na(codigo))


# filas que contienen caracteres no imprimibles (control chars, saltos, etc.)


# anti join

mal_codificados <-  obs02 |> 
  anti_join(muestra_cuali, by = "codigo") 


# join con el df centros educativos


obs01 <- read_xlsx(here("Datos","Listos para análisis","Observacion a nivel de centros","obs01.xlsx"))

atributos <- read_xlsx(here("Insumos cualis","obs01_atributos.xlsx"))

obs01_merge <- obs01 |> 
  select(colnames(atributos),
         -c(index,id,uuid,nombre,sector,modalidad,codigo_observador)) 

sin_merge_entre_obss <-  obs02 |> 
  anti_join(obs01_merge, by = "codigo") 

sin_merge_entre_obss <-  obs01_merge |> 
  anti_join(obs02, by = "codigo") 

obs02 <- inner_join(obs02,obs01_merge,
                    by = "codigo", relationship = "many-to-one") |> 
  relocate(all_of(colnames(obs01_merge)), .after = "codigo") |> 
  relocate(nombre, .after = codigo) |> 
  relocate(c(index,id,uuid))

## devuelde los códigos que están solo en obs02 y no en obs01 

# sin_merge_entre_obss %>%
#   select(codigo, codigo_observador, nombre) %>%
#   distinct(codigo, .keep_all = TRUE)


# Generador de df cuali obs02

variables_cualis <- c("index",
                      "id",
                      "codigo_observador",
                      "codigo",
                      "nombre",
                      "departamento",
                      "municipio",
                      "plan",
                      "jornada",
                      "poblacion",
                      "sector",
                      "modalidad",
                      "grado",
                      "dia",
                      "incidente_despues",
                      "area",
                      "educacion_artistica",
                      "otra_area",
                      "orientacion_emprendimiento",
                      "otra_orientacion_emprendimiento",
                      "orientacion_agricola",
                      "otra_orientacion_agricola",
                      "orientacion_industrial",
                      "otra_orientacion_industrial",
                      "orientacion_economia",
                      "otra_orientacion_economia",
                      "orientacion_comercial",
                      "otra_orientacion_comercial",
                      "orientacion_emprendimiento_ineboi",
                      "otra_orientacion_emprendimiento_ineboi",
                      "numero",
                      "otro_numero",
                      "docente_inicia_clase",
                      "actividades_desarrollo_clase",
                      "actividades_cuadernos",
                      "uso_pizarron",
                      "gestionar_conducta",
                      "gestionar_atencion",
                      "tarea",
                      "explicacion_tarea",
                      "acciones_evaluacion_sumativa",
                      "acciones_evaluacion_formativa",
                      "cierre_sesion",
                      "vinculacion_clase",
                      "acciones_participacion",
                      "acciones_comprension",
                      "acciones_dudas",
                      "acciones_retroalimentacion",
                      "relevante",
                      "razon_ningun_area",
                      "razon_suspension",
                      "incidente")

df_cuali_obs02 <- obs02 |> 
  select(all_of(variables_cualis))

# writexl

write_xlsx(obs02, here("Datos","Listos para análisis","Observacion de aulas/obs02.xlsx"))
write_xlsx(df_cuali_obs02, here("Insumos cualis","df_cuali_obs02.xlsx"))