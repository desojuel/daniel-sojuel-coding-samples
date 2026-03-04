df_obs03 <- read_xlsx(here("Datos","Observación especialistas","obs03.xlsx"))

## Sección 1. Información del Centro Educativo y grado asignado ---- 
## Frecuencias y Porcentaje de la variable Área del especialista
area_obs03 <- df_obs03 |> 
  make_freq_table(area,"Área del especialista")

## Frecuencias y Porcentaje de la variable Comentarios en desarrollo o finales
comentarios_obs03 <- df_obs03 |> 
  make_freq_table(comentarios,"Comentarios en desarrollo o finales")

## Frecuencias y Porcentaje de la variable Al menos una observación este día
registrar_observacion_dia_obs03 <- df_obs03 |> 
  filter(comentarios == "Comentarios durante la semana") |>
  make_freq_table(registrar_observacion_dia,"Al menos una observación este día")

## Frecuencias y Porcentaje de la variable Grado
grado_obs03 <- df_obs03 |> 
  filter(registrar_observacion_dia == "Sí") |>
  make_freq_table(grado,"Grado")

## Frecuencias y Porcentaje de la variable Sección
seccion_obs03 <- df_obs03 |> 
  filter(registrar_observacion_dia == "Sí") |>
  make_freq_table(seccion,"Sección", levels_order = c("A", "B", "C", "D"))

## output list
output_list1 <- list(
  "Área del especialista" = area_obs03, 
  "Comentarios en desarrollo o finales" = comentarios_obs03, 
  "Al menos una observación este día" = registrar_observacion_dia_obs03, 
  "Grado" = grado_obs03, 
  "Sección" = seccion_obs03)

## Sección 2. Registro de observaciones del área 
## Frecuencias y Porcentaje de la variable Día de observación
dia_obs03 <- df_obs03 |> 
  mutate(dia = as.character(dia),
         dia = recode(dia, .missing = "Sin información")) |> 
  make_freq_table(dia,"Día de observación", levels_order = c("1", "2", "3", "4", "5", 
                                                             "Sin información"))

## Frecuencias y Porcentaje de la variable Número de observación
numero_observacion_obs03 <- df_obs03 |> 
  mutate(numero_observacion = as.character(numero_observacion),
         numero_observacion = recode(numero_observacion, .missing = "Sin información")) |> 
  make_freq_table(numero_observacion,"Número de observación", 
                  levels_order = c("1", "2", "3", "4","Sin información"))
  
## output list
output_list2 <- list(
  "Día de observación" =  dia_obs03, 
  "Número de observación" = numero_observacion_obs03)

## Sección 3. Comentarios sobre los docentes del área ----
## Frecuencias y Porcentaje de la variable Grado tiene docente especializado
grado_especialista_obs03 <- df_obs03 |> 
  filter(comentarios_caracteristicas_docente == "Sí" |
           comentarios_observaciones_previas == "No") |>
  make_freq_table(grado_especialista,"Grado tiene docente especializado")

## output list
output_list3 <- list(
  "Grado tiene docente especializado" = grado_especialista_obs03)

## Sección 4. Planificaciones ----
## Frecuencias y Porcentaje de la variable Docente tenía planificaciones para si mismo
docente_planificaciones_obs03 <- df_obs03 |> 
  make_freq_table(docente_planificaciones,"Docente tenía planificaciones para si mismo")

## Frecuencias y Porcentaje de la variable Formato de planificaciones
formato_planificaciones_obs03 <- df_obs03 |> 
  filter(docente_planificaciones == "Sí") |>
  select_multiple(formato_planificaciones,"Formato de planificaciones")

## Frecuencias y Porcentaje de la variable Docente dio copias de planificaciones
docente_entrego_copias_planificaciones_obs03 <- df_obs03 |>
  filter(docente_planificaciones == "Sí") |>
  make_freq_table(docente_entrego_copias_planificaciones,"Docente dio copias de planificaciones")

## Frecuencias y Porcentaje de la variable Fotografías de planificaciones
fotografias_planificaciones_obs03 <- df_obs03 |>
  make_freq_table(fotografias_planificaciones,"Fotografías de planificaciones")

## Frecuencias y Porcentaje de la variable Periodo de planificaciones entregadas
periodo_planificaciones_obs03 <- df_obs03 |>
  filter(str_detect(docente_planificaciones, "Sí") | 
           str_detect(fotografias_planificaciones, "Sí")) |>
  make_freq_table(periodo_planificaciones,"Periodo de planificaciones entregadas", 
                  levels_order = c("Diaria", "Semanal", "Quincenal", "Mensual", "Bimestral", 
                                   "Anual", "Sin especificar o no se puede identificar la temporalidad", 
                                   "Sin información"))

## Frecuencias y Porcentaje de la variable Se subirán fotografías de planificaciones
carga_fotografias_planificaciones_obs03 <- df_obs03 |>
  make_freq_table(carga_fotografias_planificaciones,"Se subirán fotografías de planificaciones")

## Frecuencias y Porcentaje de la variable Elementos de planificaciones
elementos_planificaciones_obs03 <- df_obs03 |>
  select_multiple(elementos_planificaciones,"Elementos de planificaciones")

## output list
output_list4 <- list(
  "Docente tenía planificaciones para si mismo" = docente_planificaciones_obs03, 
  "Formato de planificaciones" = formato_planificaciones_obs03, 
  "Docente dio copias de planificaciones" = docente_entrego_copias_planificaciones_obs03, 
  "Fotografías de planificaciones" = fotografias_planificaciones_obs03, 
  "Periodo de planificaciones entregadas" = periodo_planificaciones_obs03, 
  "Se subirán fotografías de planificaciones" = carga_fotografias_planificaciones_obs03, 
  "Elementos de planificaciones" = elementos_planificaciones_obs03)

## Sección 6. Comentarios sobre prácticas o acciones observadas en el periodo ----
## Frecuencias y Porcentaje de la variable Prácticas durante el periodo
acciones_practicas_obs03 <- df_obs03 |>
  select_multiple(acciones_practicas, "Prácticas durante el periodo")

## output list
output_list6 <- list(
  "Prácticas durante el periodo" = acciones_practicas_obs03)

## Sección 7. Prácticas de seguimiento ----  
## Frecuencias y Porcentaje de la variable Vinculación presente periodo con el anterior
vinculacion_anterior_obs03 <- df_obs03 |>
  make_freq_table(vinculacion_anterior, "Vinculación presente periodo con el anterior")

## Frecuencias y Porcentaje de la variable Era necesaria acción de seguimiento
vinculacion_anterior_necesaria_obs03 <- df_obs03 |>
  filter(vinculacion_anterior == "No") |>
  make_freq_table(vinculacion_anterior_necesaria, "Era necesaria acción de seguimiento")

## Frecuencias y Porcentaje de la variable Prácticas de seguimiento
practicas_seguimiento_obs03 <- df_obs03 |>
  filter(vinculacion_anterior == "Sí") |>
  select_multiple(practicas_seguimiento, "Prácticas de seguimiento")

## output list
output_list7 <- list(
  "Vinculación presente periodo con el anterior" = vinculacion_anterior_obs03, 
  "Era necesaria acción de seguimiento" = vinculacion_anterior_necesaria_obs03, 
  "Prácticas de seguimiento" = practicas_seguimiento_obs03)

# generar archivos de excel ----
write_xlsx(output_list1, here("Resultados","Observación especialistas","tablas_seccion_1.xlsx"))
write_xlsx(output_list2, here("Resultados","Observación especialistas","tablas_seccion_2.xlsx"))
write_xlsx(output_list3, here("Resultados","Observación especialistas","tablas_seccion_3.xlsx"))
write_xlsx(output_list4, here("Resultados","Observación especialistas","tablas_seccion_4.xlsx"))
write_xlsx(output_list6, here("Resultados","Observación especialistas","tablas_seccion_6.xlsx"))
write_xlsx(output_list7, here("Resultados","Observación especialistas","tablas_seccion_7.xlsx"))

