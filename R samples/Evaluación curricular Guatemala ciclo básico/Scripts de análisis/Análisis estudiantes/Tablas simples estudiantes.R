df_est <- read_xlsx(here("Datos","Listos para análisis","Encuesta estudiantes","df_est.xlsx"))

# Tablas sección 2 ----
## Frecuencias y Porcentaje de la variable Sector
sector_est <- df_est |>
  make_freq_table(sector,"Sector")

## Frecuencias y Porcentaje de la variable Modalidad
modalidad_est <- df_est |>
  filter(sector == "Oficial") |>
  make_freq_table(modalidad,"Modalidad")

## Frecuencias y Porcentaje de la variable Plan
plan_est <- df_est |>
  make_freq_table(plan,"Plan")

## Frecuencias y Porcentaje de la variable Jornada
jornada_est <- df_est |>
  make_freq_table(jornada,"Jornada")

## Frecuencias y Porcentaje de la variable Departamento
departamento_est <- df_est |>
  make_freq_table(departamento,"Departamento")

## Frecuencias y Porcentaje de la variable Tipo de Participante
participante_est <- df_est |>
  make_freq_table(participante,"Tipo de Participante")

## Frecuencias y Porcentaje de la variable Modalidad de Encuesta
modalidad_encuesta_est <- df_est |>
  make_freq_table(modalidad_encuesta,"Modalidad de Encuesta")

## output list

output_list2 <- list(
  "Sector" = sector_est,
  "Modalidad" = modalidad_est,
  "Plan" = plan_est,
  "Jornada" = jornada_est,
  "Departamento" = departamento_est,
  "Tipo de Participante" = participante_est,
  "Modalidad de Encuesta" = modalidad_encuesta_est
)



# Tablas sección 4 ----

## Frecuencias y Porcentaje de la variable Sexo del estudiante
sexo_est <- df_est |>
  make_freq_table(sexo,"Sexo del Estudiante")

## Frecuencias y Porcentaje de la variable Edad del estudiante
edad_est <- df_est |>
  tabla_resumen(edad)

## Frecuencias y Porcentaje de la variable Autoidentificación
autoidentificacion_est <- df_est |>
  make_freq_table(autoidentificacion, "Autoidentificación")

## Frecuencias y Porcentaje de la variable Grado del estudiante
grado_est <- df_est |>
  make_freq_table(grado, "Grado del estudiante", levels_order = c("Primero básico",
                                                                  "Segundo básico", "Tercero básico"))

## Frecuencias y Porcentaje de la variable Número de estudiante encuestado
numero_estudiantes_encuestado_est <- df_est |>
  mutate(numero_estudiantes_encuestado = replace_na(
    as.character(numero_estudiantes_encuestado), 
    "Sin información"
  )) |>
  make_freq_table(numero_estudiantes_encuestado, "Número de estudiante encuestado")

## Frecuencias y Porcentaje de la variable Repitencia
repitencia_est <- df_est |>
  make_freq_table(repitencia, "Repitencia")

## Frecuencias y Porcentaje de la variable Razones de Repitencia
razones_repitencia_est <- df_est |>
  filter(repitencia == "Sí") |>
  make_freq_table(razones_repitencia, "Razones de repitencia")

## output list

output_list4 <- list(
  "Sexo del Estudiante" = sexo_est,
  "Edad" = edad_est,
  "Autoidentificación" = autoidentificacion_est,
  "Grado del estudiante" = grado_est,
  "Número de estudiante encuestado" = numero_estudiantes_encuestado_est,
  "Repitencia" = repitencia_est,
  "Razones de repitencia" = razones_repitencia_est
)


# Tablas sección 5 ----

## Las siguientes variables no tienen información:
### musica_recibes_clase / musica_tiempo_recibe_clase / visuales_recibes_clase / visuales_tiempo_recibe_clase
### teatro_recibes_clase / teatro_tiempo_recibe_clase / danza_recibes_clase / danza_tiempo_recibe_clase


# Tablas sección 6 ----

## Frecuencias y Porcentaje de la variable Interrupcion de encuesta
interrupcion_encuesta_est <- df_est |>
  make_freq_table(interrupcion_encuesta,"Interrupcion de encuesta")

## output list
output_list6 <- list(
  "Interrupcion de encuesta" = interrupcion_encuesta_est)

# generar archivos de excel ----
write_xlsx(output_list2, here("Resultados","Encuesta estudiantes","tablas_seccion_2.xlsx"))
write_xlsx(output_list4, here("Resultados","Encuesta estudiantes","tablas_seccion_4.xlsx"))
write_xlsx(output_list6, here("Resultados","Encuesta estudiantes", "tablas_seccion_6.xlsx"))


