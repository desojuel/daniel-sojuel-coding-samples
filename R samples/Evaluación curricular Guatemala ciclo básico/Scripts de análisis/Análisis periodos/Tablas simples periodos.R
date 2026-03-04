df_obs02 <- read_xlsx(here("Datos","Listos para análisis","Observacion de aulas","obs02.xlsx"))

## Sección 1. Información del Centro Educativo y Grado asignado ---- 
## Frecuencias y Porcentaje de la variable Sector 
sector_obs02 <- df_obs02 |> 
  make_freq_table(sector,"Sector")

## Frecuencias y Porcentaje de la variable Tipo de modalidad
modalidad_obs02 <- df_obs02 |> 
  filter(str_detect(sector, "Oficial")) |> 
  select_multiple(modalidad,"Tipo de modalidad")

## Frecuencias y Porcentaje de la variable Grado 
grado_obs02 <- df_obs02 |> 
  make_freq_table(grado,"Grado", levels_order = c("Primer grado", "Segundo grado", 
                                                  "Tercer grado"))

## Frecuencias y Porcentaje de la variable Sección 
seccion_obs02 <- df_obs02 |> 
  make_freq_table(seccion,"Sección", levels_order = c("A", "B", "C", "D", "Única", 
                                                      "Otra"))

## output list
output_list1 <- list(
  "Sector" = sector_obs02,
  "Tipo de modalidad" = modalidad_obs02, 
  "Grado" = grado_obs02, 
  "Sección" = seccion_obs02)

## Sección 2. Número de observaciones ---- 
## Frecuencias y Porcentaje de la variable Día de observación
dia_obs02 <- df_obs02 |> 
  mutate(dia = as.character(dia),
         dia = recode(dia, .missing = "Sin información")) |> 
  make_freq_table(dia,"Día de observación")

## Frecuencias y Porcentaje de la variable Incidente impide observación 
incidente_despues_obs02 <- df_obs02 |> 
  make_freq_table(incidente_despues,"Incidente impide observación")

## Frecuencias y Porcentaje de la variable Cantidad de áreas observadas 
cantidad_areas_obs02 <- df_obs02 |> 
  mutate(cantidad_areas = as.character(cantidad_areas),
         cantidad_areas = recode(cantidad_areas, .missing = "Sin información")) |> 
  make_freq_table(cantidad_areas,"Cantidad de áreas observadas", levels_order = 
                    c("0", "1", "2", "3", "4", "5", "6", "7", "8", "Sin información"))

## output list
output_list2 <- list(
  "Día de observación" = dia_obs02, 
  "Incidente impide observación" = incidente_despues_obs02, 
  "Cantidad de áreas observadas" = cantidad_areas_obs02)

## Sección 3. Registro de observaciones de áreas ----
## Frecuencias y Porcentaje de la variable Área
area_obs02 <- df_obs02 |> 
  make_freq_table(area,"Área")

## Frecuencias y Porcentaje de la variable Lenguaje de Educación Artística
educacion_artistica_obs02 <- df_obs02 |> 
  filter(str_detect(area, "Educación Artística")) |> 
  make_freq_table(educacion_artistica,"Lenguaje de Educación Artística")

## Frecuencias y Porcentaje de la variable Orientación de Emprendimiento para productividad
orientacion_emprendimiento_obs02 <- df_obs02 |> 
  filter(str_detect(area, fixed("Emprendimiento para la productividad")), 
         str_detect(modalidad, fixed("INEBE (PEMEM)"))) |> 
  make_freq_table(orientacion_emprendimiento,"Orientación de Emprendimiento para productividad")

## Frecuencias y Porcentaje de la variable Orientación agrícola y agropecuaria
## esta variable no tiene datos (verificado el 23-11-2025) ## no tomar en cuenta para Rmd
## orientacion_agricola_obs02 <- df_obs02 |> 
##  filter(str_detect(orientacion_emprendimiento, ("Orientación Agrícola y Agropecuaria"))) |> 
##  make_freq_table(orientacion_agricola,"Orientación agrícola y agropecuaria")

## Frecuencias y Porcentaje de la variable Orientación Industrial
orientacion_industrial_obs02 <- df_obs02 |> 
  filter(str_detect(orientacion_emprendimiento, ("Orientación Industrial"))) |> 
  make_freq_table(orientacion_industrial,"Orientación Industrial")

## Frecuencias y Porcentaje de la variable Orientación Economía Doméstica
orientacion_economia_obs02 <- df_obs02 |> 
  filter(str_detect(orientacion_emprendimiento, ("Economía Doméstica"))) |> 
  make_freq_table(orientacion_economia,"Orientación Economía Doméstica")

## Frecuencias y Porcentaje de la variable Orientación Comercial
orientacion_comercial_obs02 <- df_obs02 |> 
  filter(str_detect(orientacion_emprendimiento, ("Orientación Comercial"))) |> 
  make_freq_table(orientacion_comercial,"Orientación Comercial")

## Frecuencias y Porcentaje de la variable Orientación de Emprendimiento INEBOI
## esta variable no tiene datos (verificado el 23-11-2025) ## no tomar en cuenta para Rmd
## orientacion_emprendimiento_ineboi_obs02 <- df_obs02 |> 
##  filter(str_detect(area, fixed("Emprendimiento para la productividad")), 
##         str_detect(modalidad, fixed("INEBOI"))) |> 
##  make_freq_table(orientacion_emprendimiento_ineboi,"Orientación de Emprendimiento INEBOI")

## Frecuencias y Porcentaje de la variable Número de observación
numero_obs02 <- df_obs02 |> 
  make_freq_table(numero,"Número de observación", 
                  levels_order = c("Primera observación", "Segunda observación",
                                   "Tercera observación", "Cuarta observación", 
                                   "Quinta observación", "Sin información"))

## Frecuencias y Porcentaje de la variable Docente tiene sus planificaciones
planificaciones_docente_obs02 <- df_obs02 |> 
  filter(str_detect(numero, "Primera observación")) |>
  make_freq_table(planificaciones_docente,"Docente tiene sus planificaciones")

## Frecuencias y Porcentaje de la variable Formato de planificaciones del docente
planificaciones_formato_docente_obs02 <- df_obs02 |> 
  filter(str_detect(planificaciones_docente, "Sí")) |>
  select_multiple(planificaciones_formato_docente,"Formato de planificaciones del docente")

## Frecuencias y Porcentaje de la variable Docente dio copia impresa de planificaciones
planificaciones_copia_impresa_obs02 <- df_obs02 |> 
  filter(str_detect(planificaciones_docente, "Sí")) |>
  make_freq_table(planificaciones_copia_impresa,"Docente dio copia impresa de planificaciones")

## Frecuencias y Porcentaje de la variable Fotografías de planificaciones
planificaciones_fotografias_obs02 <- df_obs02 |> 
  make_freq_table(planificaciones_fotografias,"Fotografías de planificaciones")

## Frecuencias y Porcentaje de la variable Periodo de planificaciones entregadas
planificaciones_periodo_obs02 <- df_obs02 |> 
  filter(str_detect(planificaciones_docente, "Sí") | 
           str_detect(planificaciones_fotografias, "Sí")) |>
  make_freq_table(planificaciones_periodo,"Periodo de planificaciones entregadas", 
                  levels_order = c("Diaria", "Semanal", "Quincenal", "Mensual", "Bimestral", 
                                   "Anual", "Sin especificar o no se puede identificar la temporalidad", 
                                   "Sin información", "Option 7"))

## Frecuencias y Porcentaje de la variable Se subirán fotografías de planificaciones
planificaciones_carga_fotografias_obs02 <- df_obs02 |> 
  make_freq_table(planificaciones_carga_fotografias,"Se subirán fotografías de planificaciones")

## Frecuencias y Porcentaje de la variable Elementos presentes en planificaciones
planificaciones_elementos_obs02 <- df_obs02 |> 
  select_multiple(planificaciones_elementos,"Elementos presentes en planificaciones")

## Frecuencias y Porcentaje de la variable Espacio de desarrollo de clase
espacio_desarrollo_clase_obs02 <- df_obs02 |> 
  make_freq_table(espacio_desarrollo_clase,"Espacio de desarrollo de clase")

## Frecuencias y Porcentaje de la variable Competencia desarrollada según planificación
competencia_segun_planificacion_obs02 <- df_obs02 |> 
  select_multiple(competencia_segun_planificacion,"Competencia desarrollada según planificación")

## Frecuencias y Porcentaje de la variable Misma competencia desarrollada que la anterior
competencia_anterior_obs02 <- df_obs02 |> 
  filter(numero != "Primera observación",
         competencia_segun_planificacion == "Sí") |>
  make_freq_table(competencia_anterior,"Misma competencia desarrollada que la anterior")

## Frecuencias y Porcentaje de la variable Competencia se encuentra en el CNB
competencia_cnb_obs02 <- df_obs02 |> 
  filter(competencia_primera != '' |
         competencia_presente != '') |>
  make_freq_table(competencia_cnb,"Competencia se encuentra en el CNB")

## Frecuencias y Porcentaje de la variable Competencia corresponde al área y grado
competencia_area_obs02 <- df_obs02 |> 
  filter(competencia_cnb == "Sí") |>
  make_freq_table(competencia_area,"Competencia corresponde al área y grado")

## Frecuencias y Porcentaje de la variable Contenidos corresponden al área y grado
contenidos_cnb_obs02 <- df_obs02 |> 
  filter(contenidos_desarrollados != '') |>
  make_freq_table(contenidos_cnb,"Contenidos corresponden al área y grado")

## Frecuencias y Porcentaje de la variable Contenidos corresponden a competencia identificada
contenidos_corresponden_competencia_obs02 <- df_obs02 |> 
  filter(competencia_segun_planificacion == "Sí",
         competencia_cnb == "Sí") |>
  make_freq_table(contenidos_corresponden_competencia,"Contenidos corresponden a competencia identificada")

## Frecuencias y Porcentaje de la variable Secuencia didáctica
secuencia_didactica_obs02 <- df_obs02 |> 
  filter(numero == "Primera observación") |>
  make_freq_table(secuencia_didactica,"Secuencia didáctica")

## Frecuencias y Porcentaje de la variable Docente dejó alguna tarea
tarea_obs02 <- df_obs02 |> 
  make_freq_table(tarea,"Docente dejó alguna tarea")

## Frecuencias y Porcentaje de la variable Recurso principal de clases Telesecundaria
recurso_principal_telesecundaria_obs02 <- df_obs02 |> 
  filter(modalidad == "Telesecundaria") |>
  make_freq_table(recurso_principal_telesecundaria,"Recurso principal de clases Telesecundaria")

## Frecuencias y Porcentaje de la variable Actividad fuera del aula
actividad_fuera_obs02 <- df_obs02 |> 
  make_freq_table(actividad_fuera,"Actividad fuera del aula")

## Frecuencias y Porcentaje de la variable Evaluación sumativa
evaluacion_sumativa_obs02 <- df_obs02 |> 
  make_freq_table(evaluacion_sumativa,"Evaluación sumativa")

## Frecuencias y Porcentaje de la variable Evaluación formativa
evaluacion_formativa_obs02 <- df_obs02 |> 
  make_freq_table(evaluacion_formativa,"Evaluación formativa")

## Frecuencias y Porcentaje de la variable Instrumento para evaluaciones
instrumento_obs02 <- df_obs02 |> 
  filter(evaluacion_sumativa == "Sí" |
         evaluacion_formativa == "Sí") |>
  make_freq_table(instrumento,"Instrumento para evaluaciones")

## Frecuencias y Porcentaje de la variable Criterios de evaluación claros
criterios_evaluacion_obs02 <- df_obs02 |> 
  filter(evaluacion_sumativa == "Sí" |
         evaluacion_formativa == "Sí") |>
  make_freq_table(criterios_evaluacion,"Criterios de evaluación claros")

## Frecuencias y Porcentaje de la variable Nivel de participación
nivel_participacion_obs02 <- df_obs02 |> 
  make_freq_table(nivel_participacion,"Nivel de participación", 
                  levels_order = c("Bajo: Menos del 25% de los estudiantes participaron en clase",
                                   "Medio: entre 25-50% de los estudiantes participaron en clase.",
                                   "Alto: entre 50-75% de los estudiantes participaron en clase.",
                                   "Muy alto: Más del 75% de los estudiantes participaron en clase.",
                                   "Sin información"))

## output list
output_list3 <- list(
  "Área" = area_obs02, 
  "Lenguaje de Educación Artística" = educacion_artistica_obs02, 
  "Orientación de Emprendimiento para productividad" = orientacion_emprendimiento_obs02, 
  "Orientación Industrial" = orientacion_industrial_obs02, 
  "Orientación Economía Doméstica" = orientacion_economia_obs02, 
  "Orientación Comercial" = orientacion_comercial_obs02, 
  "Número de observación" = numero_obs02, 
  "Docente tiene sus planificaciones" = planificaciones_docente_obs02, 
  "Formato de planificaciones del docente" = planificaciones_formato_docente_obs02, 
  "Docente dio copia impresa de planificaciones" = planificaciones_copia_impresa_obs02, 
  "Fotografías de planificaciones" = planificaciones_fotografias_obs02, 
  "Periodo de planificaciones entregadas" = planificaciones_periodo_obs02, 
  "Se subirán fotografías de planificaciones" = planificaciones_carga_fotografias_obs02, 
  "Elementos presentes en planificaciones" = planificaciones_elementos_obs02, 
  "Espacio de desarrollo de clase" = espacio_desarrollo_clase_obs02, 
  "Competencia desarrollada según planificación" = competencia_segun_planificacion_obs02, 
  "Misma competencia desarrollada que la anterior" = competencia_anterior_obs02, 
  "Competencia se encuentra en el CNB" = competencia_cnb_obs02, 
  "Competencia corresponde al área y grado" = competencia_area_obs02, 
  "Contenidos corresponden al área y grado" = contenidos_cnb_obs02, 
  "Contenidos corresponden a competencia identificada" = contenidos_corresponden_competencia_obs02, 
  "Secuencia didáctica" = secuencia_didactica_obs02, 
  "Docente dejó alguna tarea" = tarea_obs02, 
  "Recurso principal de clases Telesecundaria" = recurso_principal_telesecundaria_obs02, 
  "Actividad fuera del aula" = actividad_fuera_obs02, 
  "Evaluación sumativa" = evaluacion_sumativa_obs02, 
  "Evaluación formativa" = evaluacion_formativa_obs02, 
  "Instrumento para evaluaciones" = instrumento_obs02, 
  "Criterios de evaluación claros" = criterios_evaluacion_obs02, 
  "Nivel de participación" = nivel_participacion_obs02)

# generar archivos de excel ----
write_xlsx(output_list1, here("Resultados","Observacion de aulas","tablas_seccion_1.xlsx"))
write_xlsx(output_list2, here("Resultados","Observacion de aulas","tablas_seccion_2.xlsx"))
write_xlsx(output_list3, here("Resultados","Observacion de aulas","tablas_seccion_3.xlsx"))
