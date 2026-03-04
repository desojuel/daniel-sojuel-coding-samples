# Generador de atributos para el proceso cuali

# lecturas 

obs03 <- read_xlsx(here("Datos","Observación especialistas","obs03.xlsx"))

# crear variable nombre


obs03 <- obs03 |>
  mutate(codigo = str_replace(codigo, "(45)\\s+", "\\1; "))

obs03 <- obs03 %>%
  # Primero separa el código del resto
  separate(codigo, into = c("codigo", "resto"), sep = ";") |> 
  mutate(codigo = coalesce(codigo, codigo_digef)) |> 
  select(-c(resto,digecur_obs3,sector, codigo_digef))

# arreglar error con aqueche

obs03$codigo[obs03$codigo == "22-04-0192-45"] <- "00-01-0206-45"


# join con el df centros educativos

muestra_cuali <- read_xlsx(here("Muestra","muestra_cuali.xlsx"))

obs01 <- read_xlsx(here("Datos","Listos para análisis","Observacion a nivel de centros","obs01.xlsx"))

atributos <- read_xlsx(here("Insumos cualis","obs01_atributos.xlsx"))

obs01_merge <- obs01 |> 
  select(colnames(atributos),
         -c(index,id,uuid)) 

sin_merge_entre_obss <-  obs03 |> 
  anti_join(obs01_merge, by = "codigo") 

## devuelde los códigos que están solo en obs03 y no en obs01 

sin_merge_entre_obss %>%
  select(codigo) %>%
  distinct(codigo, .keep_all = TRUE)

# merge

obs03 <- inner_join(obs03,obs01_merge,
                    by = "codigo", relationship = "many-to-one") |> 
  relocate(all_of(colnames(obs01_merge)), .after = "codigo")



# verificar quienes ya llenaron las observaciones finales

obs03 |> 
  filter(comentarios == "Comentarios finales de la observación") |> 
  select(observador, observador_digef,area)

# obs03 <- obs03 |> 
#   filter(observador %in% c("Carlos Galicia",
#                            "Brenda Morales",
#                            "Daniel Ajanel",
#                            "Edwin Sosa") |
#          observador_digef %in% c("fmorozco",
#                                  "jlcux",
#                                  "fmorozco",
#                                  "ngrecopalchi",
#                                  "bmrodriguez",
#                                  "lmcontreras"))

obs03 <- obs03 |> 
  relocate(c(index,id,uuid))

# Generador de df cuali obs03

variables_cualis <- c("index",
                      "id",
                      "uuid",
                      "observador",
                      "codigo",
                      "nombre",
                      "departamento",
                      "municipio",
                      "sector",
                      "plan",
                      "jornada",
                      "poblacion",
                      "area",
                      "comentarios",
                      "grado",
                      "seccion",
                      "dia",
                      "comentarios_docente",
                      "comentarios_planificaciones",
                      "secuencia_didactica",
                      "actividades_clase",
                      "aplicacion_enfoque_competencias",
                      "enfoque_desarrollo_area",
                      "estrategias_ensenanza",
                      "contenido",
                      "desarrollo_contenido",
                      "gestion_aula",
                      "comunicacion_docente_estudiantes",
                      "adecuaciones_curriculares",
                      "bilinguismo_aula",
                      "asignacion_tareas",
                      "evaluacion_sumativa_formativa",
                      "calificacion_tareas",
                      "instrumentos_evaluacion",
                      "uso_cuadernos",
                      "uso_pizarron",
                      "uso_libros",
                      "uso_recursos_didacticos",
                      "verificacion_comprension_estudiantes",
                      "clarificacion_dudas",
                      "retroalimentacion",
                      "instrucciones_recordatorios",
                      "acompanamiento",
                      "actividades_grupales",
                      "actividades_adecuaciones_curriculares",
                      "uso_laboratorio",
                      "vinculacion_anterior",
                      "vinculacion_anterior_necesaria",
                      "comentarios_vinculacion",
                      "practicas_seguimiento",
                      "revision_tareas",
                      "retroalimentacion_actividades_previas",
                      "repaso_sesion_anterior",
                      "retomar_actividades_sesion_anterior",
                      "relacion_contenidos_previos",
                      "otras_actividades_seguimiento",
                      "razon_ningun_periodo",
                      "incidente_periodo",
                      "comentarios_adicionales_periodo",
                      "recursos_materiales",
                      "planificacion_docente",
                      "actividades_aprendizaje",
                      "actitud_docente",
                      "dominio_docente",
                      "desarrollo_area_cnb",
                      "metodologias_docente",
                      "adecuaciones_curriculares_cnb",
                      "desarrollo_area_contexto_estudiantes",
                      "infraestructura",
                      "enfoque_competencias_relacion_docente",
                      "evaluacion_indicadores_logro",
                      "comentarios_adicionales_finalizando_semana"
)

df_cuali_obs03 <- obs03 |> 
  select(all_of(variables_cualis))

# writexl

write_xlsx(obs03, here("Datos","Listos para análisis",
                       "Observacion de especialistas","obs03.xlsx"))
write_xlsx(df_cuali_obs03, here("Insumos cualis","df_cuali_obs03.xlsx"))