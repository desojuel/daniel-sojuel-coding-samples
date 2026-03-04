df_dyd <- read_xlsx(here("Datos","Listos para análisis","Encuesta docente","df_dyd.xlsx"))
df_dydR <- read_xlsx(here("Datos", "Encuesta docente","encuesta docente 18.11.25 con versiones.xlsx"), ,
                     sheet = 2)

## Sección 1 Información del centro educativo ---- 
## Frecuencias y Porcentaje de la variable Sector
sector_dyd <- df_dyd |> 
  make_freq_table(sector,"Sector")

## Frecuencias y Porcentaje de la variable Modalidad
modalidad_dyd <- df_dyd |> 
  filter(sector == "Oficial") |>
  make_freq_table(modalidad,"Modalidad")

## Frecuencias y Porcentaje de la variable Plan
plan_dyd <- df_dyd |> 
  make_freq_table(plan,"Plan")

## Frecuencias y Porcentaje de la variable Jornada
jornada_dyd <- df_dyd |> 
  make_freq_table(jornada,"Jornada")

## Frecuencias y Porcentaje de la variable Departamento
departamento_dyd <- df_dyd |> 
  make_freq_table(departamento,"Departamento")

## output list
output_list1 <- list(
  "Sector" = sector_dyd,  
  "Modalidad" = modalidad_dyd, 
  "Plan" = plan_dyd, 
  "Jornada" = jornada_dyd, 
  "Departamento" = departamento_dyd) 

## Sección 2 Información sociodemográfica ----
## Frecuencias y Porcentaje de la variable Sexo
sexo_dyd <- df_dyd |> 
  make_freq_table(sexo,"Sexo")

## Frecuencias y Porcentaje de la variable Edad
edad_dyd <- df_dyd |> 
  tabla_resumen(edad)

## Frecuencias y Porcentaje de la variable Estado conyugal
estado_conyugal_dyd <- df_dyd |> 
  make_freq_table(estado_conyugal,"Estado conyugal")

## Frecuencias y Porcentaje de la variable Tiene hijos
hijos_dyd <- df_dyd |> 
  make_freq_table(hijos,"Tiene hijos")

## Frecuencias y Porcentaje de la variable Autoidentificación
autoidentificacion_dyd <- df_dyd |> 
  make_freq_table(autoidentificacion,"Autoidentificación")

## Frecuencias y Porcentaje de la variable Comunidad lingüística
comunidad_ling_dyd <- df_dyd |> 
  filter(autoidentificacion == "Maya") |>
  make_freq_table(comunidad_ling,"Comunidad lingüística")

## Frecuencias y Porcentaje de la variable Transporte principal
transporte_dyd <- df_dyd |> 
  make_freq_table(transporte,"Transporte principal")

## Frecuencias y Porcentaje de la variable Tiempo para llegar al centro
tiempo_llegada_dyd <- df_dyd |> 
  make_freq_table(tiempo_llegada,"Tiempo para llegar al centro")

## Frecuencias y Porcentaje de la variable Último nivel educativo aprobado
nivel_educativo_dyd <- df_dyd |> 
  make_freq_table(nivel_educativo,"Último nivel educativo aprobado")

## Frecuencias y Porcentaje de la variable Estudios en PADEP
estudios_padep_dyd <- df_dyd |> 
  filter(nivel_educativo != "Diversificado") |>
  make_freq_table(estudios_padep,"Estudios en PADEP")

## Frecuencias y Porcentaje de la variable Número de títulos universitarios
numero_titulos_dyd <- df_dyd |> 
  tabla_resumen(numero_titulos)

## Frecuencias y Porcentaje de la variable Título diversificado
titulo_diversificado_dyd <- df_dyd |> 
  make_freq_table(titulo_diversificado,"Título diversificado")

## Frecuencias y Porcentaje de la variable Tipo de magisterio
tipo_magisterio_dyd <- df_dyd |> 
  filter(titulo_diversificado == "Magisterio") |>
  make_freq_table(tipo_magisterio,"Tipo de magisterio")

## Frecuencias y Porcentaje de la variable Especialización de diversificado
especializacion_diversificado_dyd <- df_dyd |> 
  filter(titulo_diversificado != "Magisterio") |>
  make_freq_table(especializacion_diversificado,"Especialización de diversificado")

## Frecuencias y Porcentaje de la variable Estudios actuales
estudios_actuales_dyd <- df_dyd |> 
  make_freq_table(estudios_actuales,"Estudios actuales")

## Frecuencias y Porcentaje de la variable Nivel de estudios actuales
nivel_estudios_actuales_dyd <- df_dyd |> 
  filter(estudios_actuales == "Sí") |>
  select_multiple(nivel_estudios_actuales,"Nivel de estudios actuales")

## Frecuencias y Porcentaje de la variable Estudios actuales relacionados con área impartida
estudios_actuales_relacion_ciclo_basico_dyd <- df_dyd |> 
  filter(estudios_actuales == "Sí") |>
  make_freq_table(estudios_actuales_relacion_ciclo_basico,"Estudios actuales relacionados con área impartida")

## Frecuencias y Porcentaje de la variable Estudia actualmente en PADEP
estudios_actuales_profesorado_paded_dyd <- df_dyd |> 
  filter(str_detect(nivel_estudios_actuales, "Profesorado") | 
         str_detect(nivel_estudios_actuales, "Licenciatura")) |>
  make_freq_table(estudios_actuales_profesorado_paded,"Estudia actualmente en PADEP")

## output list
output_list2 <- list(
  "Sexo" = sexo_dyd, 
  "Edad" = edad_dyd, 
  "Estado conyugal" = estado_conyugal_dyd, 
  "Tiene hijos" = hijos_dyd, 
  "Autoidentificación" = autoidentificacion_dyd, 
  "Comunidad lingüística" = comunidad_ling_dyd, 
  "Transporte principal" = transporte_dyd, 
  "Tiempo para llegar al centro" = tiempo_llegada_dyd, 
  "Último nivel educativo aprobado" = nivel_educativo_dyd, 
  "Estudios en PADEP" = estudios_padep_dyd, 
  "Número de títulos universitarios" = numero_titulos_dyd, 
  "Título diversificado" = titulo_diversificado_dyd, 
  "Tipo de magisterio" = tipo_magisterio_dyd, 
  "Especialización de diversificado" = especializacion_diversificado_dyd, 
  "Estudios actuales" = estudios_actuales_dyd, 
  "Nivel de estudios actuales" = nivel_estudios_actuales_dyd, 
  "Estudios actuales relacionados con área impartida" = estudios_actuales_relacion_ciclo_basico_dyd, 
  "Estudia actualmente en PADEP" = estudios_actuales_profesorado_paded_dyd)

## Sección 3 Capacitaciones ----
## Frecuencias y Porcentaje de la variable Temas de capacitaciones 2023 - 2025
capacitaciones_cnb_dyd <- df_dyd |> 
  select_multiple(capacitaciones_cnb,"Temas de capacitaciones 2023 - 2025")

## Frecuencias y Porcentaje de la variable Número de capacitaciones 2025
capacitaciones_numero_dyd <- df_dyd |> 
  make_freq_table(capacitaciones_numero,"Número de capacitaciones 2025", 
                  levels_order = c("0", "1", "2", "3", "4", "5", "6", "7", "8", 
                                   "Más de 8", "Sin información"))

## Frecuencias y Porcentaje de la variable Proveedor de capacitaciones
capacitaciones_proveedor_dyd <- df_dyd |> 
  filter(str_detect(capacitaciones_cnb, "No he recibido capacitaciones sobre los temas mencionados anteriormente") | 
        (capacitaciones_numero != "0")) |>
  select_multiple(capacitaciones_proveedor,"Proveedor de capacitaciones")

## output list
output_list3 <- list(
  "Temas de capacitaciones 2023 - 2025" = capacitaciones_cnb_dyd, 
  "Número de capacitaciones 2025" = capacitaciones_numero_dyd, 
  "Proveedor de capacitaciones" = capacitaciones_proveedor_dyd)


## Sección 4 Puesto que desempeña ----
## Frecuencias y Porcentaje de la variable Puesto que desempeña
puesto_dyd <- df_dyd |> 
  make_freq_table(puesto,"Puesto que desempeña")

## Frecuencias y Porcentaje de la variable Contratante
contratante_dyd <- df_dyd |> 
  make_freq_table(contratante,"Puesto que Contratante")

## Frecuencias y Porcentaje de la variable Renglón de contratación
renglon_contratacion_dyd <- df_dyd |> 
  filter(str_detect(sector, "Oficial")) |>
  make_freq_table(renglon_contratacion,"Renglón de contratación")

## Frecuencias y Porcentaje de la variable Situación laboral Oficial
situacion_laboral_dyd <- df_dyd |> 
  filter(str_detect(renglon_contratacion, "No sé") | 
         str_detect(renglon_contratacion, "Ningún renglón") ) |>
  make_freq_table(situacion_laboral,"Situación laboral Oficial")

## Frecuencias y Porcentaje de la variable Situación laboral no oficial
situacion_laboral_no_oficial_dyd <- df_dyd |> 
  filter(sector != "Oficial")|>
  make_freq_table(situacion_laboral_no_oficial,"Situación laboral no oficial")

## Frecuencias y Porcentaje de la variable Tiempo de experiencia
tiempo_experiencia_dyd <- df_dyd |> 
  make_freq_table(tiempo_experiencia,"Tiempo de experiencia")

## Frecuencias y Porcentaje de la variable Años en puesto actual
anios_experiencia_dyd <- df_dyd |> 
  filter(tiempo_experiencia != "Menos de un año")|>
  tabla_resumen(anios_experiencia)

## output list
output_list4 <- list(
  "Puesto que desempeña" = puesto_dyd, 
  "Contratante" = contratante_dyd, 
  "Renglón de contratación" = renglon_contratacion_dyd, 
  "Situación laboral Oficial" = situacion_laboral_dyd, 
  "Situación laboral no oficial"= situacion_laboral_no_oficial_dyd, 
  "Tiempo de experiencia" = tiempo_experiencia_dyd, 
  "Años en puesto actual" = anios_experiencia_dyd)
  
## Sección 6 Consulta del CNB ---- 
## Frecuencias y Porcentaje de la variable Recepción CNB impreso
cnb_recepcion_impreso_dyd <- df_dyd |> 
  make_freq_table(cnb_recepcion_impreso,"Recepción CNB impreso")

## Frecuencias y Porcentaje de la variable Año recepción CNB impreso
cnb_ano_recepcion_impreso_dyd <- df_dyd |>
  mutate(cnb_ano_recepcion_impreso = as.character(cnb_ano_recepcion_impreso),
         cnb_ano_recepcion_impreso = recode(cnb_ano_recepcion_impreso, .missing = "Sin información")) |>
  filter(cnb_recepcion_impreso == "Sí")|>
  make_freq_table(cnb_ano_recepcion_impreso, "Año recepción CNB impreso", 
                  levels_order = c("2018", "2019", "2020", "2021", "2022",
                                   "2023", "2024", "2025"))

## Frecuencias y Porcentaje de la variable Frecuencia de consulta CNB
cnb_frecuencia_consulta_dyd <- df_dyd |> 
  make_freq_table(cnb_frecuencia_consulta,"Frecuencia de consulta CNB", 
                  levels_order = c("Diariamente", "Semanalmente", "Mensualmente", "Bimensualmente",
                                   "Otro", "No consulto el CNB", "Sin información"))

## Frecuencias y Porcentaje de la variable Medio de consulta CNB
cnb_medio_consulta_dyd <- df_dyd |> 
  filter(cnb_frecuencia_consulta != "No consulto el CNB")|>
  make_freq_table(cnb_medio_consulta, "Medio de consulta CNB")

## Frecuencias y Porcentaje de la variable Criterios de evaluación CNB
cnb_criterios_evaluacion_dyd <- df_dyd |> 
  filter(cnb_frecuencia_consulta != "No consulto el CNB")|>
  make_freq_table(cnb_criterios_evaluacion, "Criterios de evaluación CNB")

## output list
output_list6 <- list(
  "Recepción CNB impreso" = cnb_recepcion_impreso_dyd, 
  "Año recepción CNB impreso" = cnb_ano_recepcion_impreso_dyd, 
  "Frecuencia de consulta CNB" = cnb_frecuencia_consulta_dyd, 
  "Medio de consulta CNB" = cnb_medio_consulta_dyd, 
  "Criterios de evaluación CNB" = cnb_criterios_evaluacion_dyd)


## Sección 7 Preguntas para docentes ---- 
## Frecuencias y Porcentaje de la variable Número de centros donde trabaja
cantidad_centros_educativos_dyd <- df_dyd |> 
  filter(puesto == "Director/Docente" | 
         puesto == "Docente") |>
  make_freq_table(cantidad_centros_educativos, "Número de centros donde trabaja")

## Frecuencias y Porcentaje de la variable Docente trabaja en otro centro
docencia_dyd <- df_dyd |> 
  make_freq_table(docencia, "Docente trabaja en otro centro")

## Frecuencias y Porcentaje de la variable Otro nivel educativo trabaja docente 
especificar_docencia_dyd <- df_dyd |> 
  filter(docencia == "Sí") |>
  make_freq_table(especificar_docencia, "Otro nivel educativo trabaja docente", levels_order = 
                    c("Nivel inicial", "Nivel primario", "Diversificado", "Nivel superior" ))

## Frecuencias y Porcentaje de la variable Áreas impartidas
area_dyd <- df_dyd |> 
  select_multiple(area, "Áreas impartidas")

## Frecuencias y Porcentaje de la variable Lenguajes de Educación Artística impartidos
lenguajes_educacion_artistica_dyd <- df_dyd |> 
  filter(str_detect(area, "Educación Artística")) |>
  select_multiple(lenguajes_educacion_artistica, "Lenguajes de Educación Artística impartidos")

## Ciencias Naturales
## Frecuencias y Porcentaje de la variable Grados de Ciencias Naturales
cn_grados_dyd <- df_dyd |> 
  filter(str_detect(area, "Ciencias Naturales")) |>
  select_multiple(cn_grados, "Grados de Ciencias Naturales")

## Frecuencias y Porcentaje de la variable Secciones primero básico CN
cn_secciones_primero_dyd <- df_dyd |> 
  filter(str_detect(cs_grados, "Primer grado")) |>
  filter(str_detect(area, "Ciencias Naturales"))|>
  tabla_resumen(cn_secciones_primero)

## Frecuencias y Porcentaje de la variable Secciones segundo básico CN
cn_secciones_segundo_dyd <- df_dyd |> 
  filter(str_detect(cs_grados, "Segundo grado")) |>
  filter(str_detect(area, "Ciencias Naturales"))|>
  tabla_resumen(cn_secciones_segundo)

## Frecuencias y Porcentaje de la variable Secciones tercero básico CN
cn_secciones_tercero_dyd <- df_dyd |> 
  filter(str_detect(cs_grados, "Primer grado")) |>
  filter(str_detect(area, "Ciencias Naturales"))|>
  tabla_resumen(cn_secciones_tercero)

## Frecuencias y Porcentaje de la variable Uso de libros CN
cn_uso_libros_dyd <- df_dyd |>
  filter(str_detect(area, "Ciencias Naturales")) |>
  make_freq_table(cn_uso_libros, "Uso de libros CN")

## Frecuencias y Porcentaje de la variable Recepción libros CN Mineduc
cn_libros_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Ciencias Naturales")) |>
  make_freq_table(cn_libros_mineduc, "Recepción libros CN Mineduc")

## Frecuencias y Porcentaje de la variable Última vez recepción libros CN Mineduc
cn_ultima_vez_libros_mineduc_dyd <- df_dyd |> 
  filter(cn_libros_mineduc == "Sí") |>
  make_freq_table(cn_ultima_vez_libros_mineduc, "Última vez recepción libros CN Mineduc", 
                  levels_order = c("El 2021", "El 2022", "El 2023", "El 2024",
                                   "Este año", "Otro"))

## Frecuencias y Porcentaje de la variable Recepción materiales CN Mineduc
cn_recepcion_materiales__mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Ciencias Naturales")) |>
  make_freq_table(cn_recepcion_materiales__mineduc, "Recepción materiales CN Mineduc")

## Frecuencias y Porcentaje de la variable Materiales recibidos CN
cn_recepcion_materiales_apoyo_dyd <- df_dyd |> 
  filter(str_detect(area, "Ciencias Naturales")) |>
  select_multiple(cn_recepcion_materiales_apoyo, "Materiales recibidos CN")

## Frecuencias y Porcentaje de la variable Necesidad de ajustes CN
cn_ajustes_dyd <- df_dyd |> 
  filter(str_detect(area, "Ciencias Naturales")) |>
  make_freq_table(cn_ajustes, "Necesidad de ajustes CN")

## Ciencias Sociales, Formación Ciudadana e Interculturalidad
## Frecuencias y Porcentaje de la variable Grados de Ciencias Sociales
cs_grados_dyd <- df_dyd |> 
  filter(str_detect(area, "Ciencias Sociales, Formación Ciudadana e Interculturalidad")) |>
  select_multiple(cs_grados, "Grados de Ciencias Sociales")

## Frecuencias y Porcentaje de la variable Secciones primero básico CS
cs_secciones_primero_dyd <- df_dyd |> 
  filter(str_detect(cs_grados, "Primer grado")) |>
  filter(str_detect(area, "Ciencias Sociales, Formación Ciudadana e Interculturalidad"))|>
  tabla_resumen(cs_secciones_primero)

## Frecuencias y Porcentaje de la variable Secciones segundo básico CS
cs_secciones_segundo_dyd <- df_dyd |> 
  filter(str_detect(cs_grados, "Segundo grado")) |>
  filter(str_detect(area, "Ciencias Sociales, Formación Ciudadana e Interculturalidad"))|>
  tabla_resumen(cs_secciones_segundo)

## Frecuencias y Porcentaje de la variable Secciones tercero básico CS
cs_secciones_tercero_dyd <- df_dyd |> 
  filter(str_detect(cs_grados, "Tercer grado")) |>
  filter(str_detect(area, "Ciencias Sociales, Formación Ciudadana e Interculturalidad"))|>
  tabla_resumen(cs_secciones_tercero)

## Frecuencias y Porcentaje de la variable Uso de libros CS
cs_uso_libros_dyd <- df_dyd |> 
  filter(str_detect(area, "Ciencias Sociales, Formación Ciudadana e Interculturalidad")) |> 
  make_freq_table(cs_uso_libros, "Uso de libros CS")

## Frecuencias y Porcentaje de la variable Recepción libros CS Mineduc
cs_libros_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Ciencias Sociales, Formación Ciudadana e Interculturalidad")) |>
  make_freq_table(cs_libros_mineduc, "Recepción libros CS Mineduc")

## Frecuencias y Porcentaje de la variable Última vez recepción libros CS Mineduc
cs_ultima_vez_libros_mineduc_dyd <- df_dyd |> 
  filter(cs_libros_mineduc == "Sí") |>
  make_freq_table(cs_ultima_vez_libros_mineduc, "Última vez recepción libros CS Mineduc", 
                  levels_order = c("El 2021", "El 2022", "El 2023", "El 2024",
                                   "Este año", "Otro"))

## Frecuencias y Porcentaje de la variable Recepción materiales CS Mineduc
cs_recepcion_materiales_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Ciencias Sociales, Formación Ciudadana e Interculturalidad")) |>
  make_freq_table(cs_recepcion_materiales_mineduc, "Recepción materiales CS Mineduc")

## Frecuencias y Porcentaje de la variable Materiales recibidos CS
cs_recepcion_materiales_apoyo_dyd <- df_dyd |> 
  filter(str_detect(area, "Ciencias Sociales, Formación Ciudadana e Interculturalidad")) |>
  select_multiple(cs_recepcion_materiales_apoyo, "Materiales recibidos CS")

## Frecuencias y Porcentaje de la variable Necesidad de ajustes CS
cs_ajustes_dyd <- df_dyd |> 
  filter(str_detect(area, "Ciencias Sociales, Formación Ciudadana e Interculturalidad")) |>
  make_freq_table(cs_ajustes, "Necesidad de ajustes CS")

## Comunicación y Lenguaje Idioma Español
## Frecuencias y Porcentaje de la variable Grados de Comunicación y lenguaje
cl_grados_dyd <- df_dyd |> 
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Español")) |>
  select_multiple(cl_grados, "Grados de Comunicación y lenguaje")

## Frecuencias y Porcentaje de la variable Secciones primero básico CL
cl_secciones_primero_dyd <- df_dyd |> 
  filter(str_detect(cl_grados, "Primer grado")) |>
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Español"))|>
  tabla_resumen(cl_secciones_primero)

## Frecuencias y Porcentaje de la variable Secciones segundo básico CL
cl_secciones_segundo_dyd <- df_dyd |> 
  filter(str_detect(cl_grados, "Segundo grado")) |>
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Español"))|>
  tabla_resumen(cl_secciones_segundo)

## Frecuencias y Porcentaje de la variable Secciones tercero básico CL
cl_secciones_tercero_dyd <- df_dyd |> 
  filter(str_detect(cl_grados, "Tercer grado")) |>
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Español"))|>
  tabla_resumen(cl_secciones_tercero)

## Frecuencias y Porcentaje de la variable Uso de libros CL
cl_uso_libros_dyd <- df_dyd |> 
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Español")) |> 
  make_freq_table(cl_uso_libros, "Uso de libros CL")

## Frecuencias y Porcentaje de la variable Recepción libros CL Mineduc
cl_libros_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Español")) |>
  make_freq_table(cl_libros_mineduc, "Recepción libros CL Mineduc")

## Frecuencias y Porcentaje de la variable Última vez recepción libros CL Mineduc
cl_ultima_vez_libros_mineduc_dyd <- df_dyd |> 
  filter(cl_libros_mineduc == "Sí") |>
  make_freq_table(cl_ultima_vez_libros_mineduc, "Última vez recepción libros CL Mineduc", 
                  levels_order = c("El 2021", "El 2022", "El 2023", "El 2024",
                                   "Este año", "Otro"))

## Frecuencias y Porcentaje de la variable Recepción materiales CL Mineduc
cl_recepcion_materiales_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Español")) |>
  make_freq_table(cl_recepcion_materiales_mineduc, "Recepción materiales CL Mineduc")

## Frecuencias y Porcentaje de la variable Materiales recibidos CL
cl_recepcion_materiales_apoyo_dyd <- df_dyd |> 
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Español")) |>
  select_multiple(cl_recepcion_materiales_apoyo, "Materiales recibidos CL")

## Frecuencias y Porcentaje de la variable Necesidad de ajustes CL
cl_ajustes_dyd <- df_dyd |> 
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Español")) |>
  make_freq_table(cl_ajustes, "Necesidad de ajustes CL")

## Comunicación y Lenguaje Idioma Extranjero Inglés 
## Frecuencias y Porcentaje de la variable Grados Idioma inglés
in_grados_dyd <- df_dyd |> 
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Extranjero")) |>
  select_multiple(in_grados, "Grados Idioma inglés")

## Frecuencias y Porcentaje de la variable Secciones primero básico IN
in_secciones_primero_dyd <- df_dyd |> 
  filter(str_detect(in_grados, "Primer grado")) |>
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Extranjero"))|>
  tabla_resumen(in_secciones_primero)

## Frecuencias y Porcentaje de la variable Secciones segundo básico IN
in_secciones_segundo_dyd <- df_dyd |> 
  filter(str_detect(in_grados, "Segundo grado")) |>
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Extranjero"))|>
  tabla_resumen(in_secciones_segundo)

## Frecuencias y Porcentaje de la variable Secciones tercero básico IN
in_secciones_tercero_dyd <- df_dyd |> 
  filter(str_detect(in_grados, "Tercer grado")) |>
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Extranjero"))|>
  tabla_resumen(in_secciones_tercero)

## Frecuencias y Porcentaje de la variable Uso de libros IN
in_uso_libros_dyd <- df_dyd |> 
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Extranjero")) |> 
  make_freq_table(in_uso_libros, "Uso de libros IN")

## Frecuencias y Porcentaje de la variable Recepción libros IN Mineduc
in_libros_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Extranjero")) |>
  make_freq_table(in_libros_mineduc, "Recepción libros IN Mineduc")

## Frecuencias y Porcentaje de la variable Última vez recepción libros IN Mineduc
in_ultima_vez_libros_mineduc_dyd <- df_dyd |> 
  filter(in_libros_mineduc == "Sí") |>
  make_freq_table(in_ultima_vez_libros_mineduc, "Última vez recepción libros IN Mineduc", 
                  levels_order = c("El 2021", "El 2022", "El 2023", "El 2024",
                                   "Este año", "Otro"))

## Frecuencias y Porcentaje de la variable Recepción materiales IN Mineduc
in_recepcion_materiales_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Extranjero")) |>
  make_freq_table(in_recepcion_materiales_mineduc, "Recepción materiales IN Mineduc")

## Frecuencias y Porcentaje de la variable Materiales recibidos IN
in_recepcion_materiales_apoyo_dyd <- df_dyd |> 
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Extranjero")) |>
  select_multiple(in_recepcion_materiales_apoyo, "Materiales recibidos IN")

## Frecuencias y Porcentaje de la variable Necesidad de ajustes IN
in_ajustes_dyd <- df_dyd |> 
  filter(str_detect(area, "Comunicación y Lenguaje Idioma Extranjero")) |>
  make_freq_table(in_ajustes, "Necesidad de ajustes IN")

## Culturas e Idiomas Mayas, Garífuna o Xinka

## Frecuencias y Porcentaje de la variable Grados Culturas Mayas
cm_grados_dyd <- df_dyd |> 
  filter(str_detect(area, "Educación Artística")) |>
  select_multiple(cm_grados, "Grados Culturas Mayas")

## Frecuencias y Porcentaje de la variable Secciones primero básico CM
cm_secciones_primero_dyd <- df_dyd |> 
  filter(str_detect(cm_grados, "Primer grado")) |>
  filter(str_detect(area, "Culturas e Idiomas Mayas, Garífuna o Xinka"))|>
  tabla_resumen(cm_secciones_primero)

## Frecuencias y Porcentaje de la variable Secciones segundo básico CM
cm_secciones_segundo_dyd <- df_dyd |> 
  filter(str_detect(cm_grados, "Segundo grado")) |>
  filter(str_detect(area, "Culturas e Idiomas Mayas, Garífuna o Xinka"))|>
  tabla_resumen(cm_secciones_segundo)

## Frecuencias y Porcentaje de la variable Secciones tercero básico CM
cm_secciones_tercero_dyd <- df_dyd |> 
  filter(str_detect(cm_grados, "Tercer grado")) |>
  filter(str_detect(area, "Culturas e Idiomas Mayas, Garífuna o Xinka"))|>
  tabla_resumen(cm_secciones_tercero)

## Frecuencias y Porcentaje de la variable Uso de libros CM
cm_uso_libros_dyd <- df_dyd |> 
  filter(str_detect(area, "Culturas e Idiomas Mayas, Garífuna o Xinka")) |> 
  make_freq_table(cm_uso_libros, "Uso de libros CM")

## Frecuencias y Porcentaje de la variable Recepción libros CM Mineduc
cm_libros_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Culturas e Idiomas Mayas, Garífuna o Xinka")) |>
  make_freq_table(cm_libros_mineduc, "Recepción libros CM Mineduc")

## Frecuencias y Porcentaje de la variable Última vez recepción libros CM Mineduc
cm_ultima_vez_libros_mineduc_dyd <- df_dyd |> 
  filter(cm_libros_mineduc == "Sí") |>
  make_freq_table(cm_ultima_vez_libros_mineduc, "Última vez recepción libros CM Mineduc", 
                  levels_order = c("El 2021", "El 2022", "El 2023", "El 2024",
                                   "Este año", "Otro"))

## Frecuencias y Porcentaje de la variable Recepción materiales CM Mineduc
cm_recepcion_materiales_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Culturas e Idiomas Mayas, Garífuna o Xinka")) |>
  make_freq_table(cm_recepcion_materiales_mineduc, "Recepción materiales CM Mineduc")

## Frecuencias y Porcentaje de la variable Materiales recibidos CM
cm_recepcion_materiales_apoyo_dyd <- df_dyd |> 
  filter(str_detect(area, "Culturas e Idiomas Mayas, Garífuna o Xinka")) |>
  select_multiple(cm_recepcion_materiales_apoyo, "Materiales recibidos CM")

## Frecuencias y Porcentaje de la variable Necesidad de ajustes CM
cm_ajustes_dyd <- df_dyd |> 
  filter(str_detect(area, "Culturas e Idiomas Mayas, Garífuna o Xinka")) |>
  make_freq_table(cm_ajustes, "Necesidad de ajustes CM")

## Educación Artística Teatro 

## Frecuencias y Porcentaje de la variable Grados Teatro
teatro_grados_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Teatro") | 
         str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  select_multiple(teatro_grados, "Grados Teatro")

## Frecuencias y Porcentaje de la variable Secciones primero básico Teatro
teatro_secciones_primero_dyd <- df_dyd |> 
  filter(str_detect(teatro_grados, "Primer grado")) |>
  filter(str_detect(lenguajes_educacion_artistica, "Teatro") | 
         str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  tabla_resumen(teatro_secciones_primero)

## Frecuencias y Porcentaje de la variable Secciones segundo básico Teatro
teatro_secciones_segundo_dyd <- df_dyd |> 
  filter(str_detect(teatro_grados, "Segundo grado")) |>
  filter(str_detect(lenguajes_educacion_artistica, "Teatro") | 
         str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  tabla_resumen(teatro_secciones_segundo)

## Frecuencias y Porcentaje de la variable Secciones tercero básico Teatro
teatro_secciones_tercero_dyd <- df_dyd |> 
  filter(str_detect(teatro_grados, "Tercer grado")) |>
  filter(str_detect(lenguajes_educacion_artistica, "Teatro") | 
         str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  tabla_resumen(teatro_secciones_tercero)

## Frecuencias y Porcentaje de la variable Uso de libros Teatro
teatro_uso_libros_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Teatro") | 
         str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  make_freq_table(teatro_uso_libros, "Uso de libros Teatro")

## Frecuencias y Porcentaje de la variable Recepción libros Teatro Mineduc
teatro_libros_mineduc_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Teatro") | 
         str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  make_freq_table(teatro_libros_mineduc, "Recepción libros Teatro Mineduc")

## Frecuencias y Porcentaje de la variable Última vez recepción libros Teatro Mineduc
teatro_ultima_vez_libros_mineduc_dyd <- df_dyd |> 
  filter(teatro_libros_mineduc == "Sí") |>
  make_freq_table(teatro_ultima_vez_libros_mineduc, "Última vez recepción libros Teatro Mineduc", 
                  levels_order = c("El 2021", "El 2022", "El 2023", "El 2024",
                                   "Este año", "Otro"))

## Frecuencias y Porcentaje de la variable Recepción materiales Teatro Mineduc
teatro_recepcion_materiales_mineduc_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Teatro") | 
         str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  make_freq_table(teatro_recepcion_materiales_mineduc, "Recepción materiales Teatro Mineduc")

## Frecuencias y Porcentaje de la variable Materiales recibidos Teatro
teatro_recepcion_materiales_apoyo_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Teatro") | 
         str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  select_multiple(teatro_recepcion_materiales_apoyo, "Materiales recibidos Teatro")

## Frecuencias y Porcentaje de la variable Necesidad de ajustes Teatro
teatro_ajustes_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Teatro") | 
         str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  make_freq_table(teatro_ajustes, "Necesidad de ajustes Teatro")

## Danza 

## Frecuencias y Porcentaje de la variable Grados Danza
danza_grados_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Danza") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  select_multiple(danza_grados, "Grados Danza")

## Frecuencias y Porcentaje de la variable Secciones primero básico Danza
danza_secciones_primero_dyd <- df_dyd |> 
  filter(str_detect(danza_grados, "Primer grado")) |>
  filter(str_detect(lenguajes_educacion_artistica, "Danza") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  tabla_resumen(danza_secciones_primero)

## Frecuencias y Porcentaje de la variable Secciones segundo básico Danza
danza_secciones_segundo_dyd <- df_dyd |> 
  filter(str_detect(danza_grados, "Segundo grado")) |>
  filter(str_detect(lenguajes_educacion_artistica, "Danza") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  tabla_resumen(danza_secciones_segundo)

## Frecuencias y Porcentaje de la variable Secciones tercero básico Danza
danza_secciones_tercero_dyd <- df_dyd |> 
  filter(str_detect(danza_grados, "Tercer grado")) |>
  filter(str_detect(lenguajes_educacion_artistica, "Danza") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  tabla_resumen(danza_secciones_tercero)

## Frecuencias y Porcentaje de la variable Uso de libros Danza
danza_uso_libros_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Danza") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  make_freq_table(danza_uso_libros, "Uso de libros Danza")

## Frecuencias y Porcentaje de la variable Recepción libros Danza Mineduc
danza_libros_mineduc_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Danza") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  make_freq_table(danza_libros_mineduc, "Recepción libros Danza Mineduc")

## Frecuencias y Porcentaje de la variable Última vez recepción libros Danza Mineduc
danza_ultima_vez_libros_mineduc_dyd <- df_dyd |> 
  filter(danza_libros_mineduc == "Sí") |>
  make_freq_table(danza_ultima_vez_libros_mineduc, "Última vez recepción libros Danza Mineduc", 
                  levels_order = c("El 2021", "El 2022", "El 2023", "El 2024",
                                   "Este año", "Otro"))

## Frecuencias y Porcentaje de la variable Recepción materiales Danza Mineduc
danza_recepcion_materiales_mineduc_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Danza") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  make_freq_table(danza_recepcion_materiales_mineduc, "Recepción materiales Danza Mineduc")

## Frecuencias y Porcentaje de la variable Materiales recibidos Danza
danza_recepcion_materiales_apoyo_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Danza") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  select_multiple(danza_recepcion_materiales_apoyo, "Materiales recibidos Danza")

## Frecuencias y Porcentaje de la variable Necesidad de ajustes Danza
danza_ajustes_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Danza") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  make_freq_table(danza_ajustes, "Necesidad de ajustes Danza")

## Educación Musical 

## Frecuencias y Porcentaje de la variable Grados Educación Musical
musica_grados_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Educación Musical") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  select_multiple(musica_grados, "Grados Educación Musical")

## Frecuencias y Porcentaje de la variable Secciones primero básico Educación Musical
musica_secciones_primero_dyd <- df_dyd |> 
  filter(str_detect(musica_grados, "Primer grado")) |>
  filter(str_detect(lenguajes_educacion_artistica, "Educación Musical") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  tabla_resumen(musica_secciones_primero)

## Frecuencias y Porcentaje de la variable Secciones segundo básico Educación Musical
musica_secciones_segundo_dyd <- df_dyd |> 
  filter(str_detect(musica_grados, "Segundo grado")) |>
  filter(str_detect(lenguajes_educacion_artistica, "Educación Musical") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  tabla_resumen(musica_secciones_segundo)

## Frecuencias y Porcentaje de la variable Secciones tercero básico Educación Musical
musica_secciones_tercero_dyd <- df_dyd |> 
  filter(str_detect(musica_grados, "Tercer grado")) |>
  filter(str_detect(lenguajes_educacion_artistica, "Educación Musical") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  tabla_resumen(musica_secciones_tercero)

## Frecuencias y Porcentaje de la variable Uso de libros Educación Musical
musica_uso_libros_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Educación Musical") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  make_freq_table(musica_uso_libros, "Uso de libros Educación Musical")

## Frecuencias y Porcentaje de la variable Recepción libros Educación Musical Mineduc
musica_libros_mineduc_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Educación Musical") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  make_freq_table(musica_libros_mineduc, "Recepción libros Educación Musical Mineduc")

## Frecuencias y Porcentaje de la variable Última vez recepción libros Educación Musical Mineduc
musica_ultima_vez_libros_mineduc_dyd <- df_dyd |> 
  filter(musica_libros_mineduc == "Sí") |>
  make_freq_table(musica_ultima_vez_libros_mineduc, "Última vez recepción libros Educación Musical Mineduc", 
                  levels_order = c("El 2021", "El 2022", "El 2023", "El 2024",
                                   "Este año", "Otro"))

## Frecuencias y Porcentaje de la variable Recepción materiales Educación Musical Mineduc
musica_recepcion_materiales_mineduc_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Educación Musical") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  make_freq_table(musica_recepcion_materiales_mineduc, "Recepción materiales Educación Musical Mineduc")

## Frecuencias y Porcentaje de la variable Materiales recibidos Educación Musical
musica_recepcion_materiales_apoyo_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Educación Musical") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  select_multiple(musica_recepcion_materiales_apoyo, "Materiales recibidos Educación Musical")

## Frecuencias y Porcentaje de la variable Necesidad de ajustes Educación Musical
musica_ajustes_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Educación Musical") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  make_freq_table(musica_ajustes, "Necesidad de ajustes Educación Musical")

## Artes Visuales 

## Frecuencias y Porcentaje de la variable Grados Artes Visuales
visuales_grados_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Artes Visuales") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  select_multiple(visuales_grados, "Grados Artes Visuales")

## Frecuencias y Porcentaje de la variable Secciones primero básico Artes Visuales
visuales_secciones_primero_dyd <- df_dyd |> 
  filter(str_detect(visuales_grados, "Primer grado")) |>
  filter(str_detect(lenguajes_educacion_artistica, "Artes Visuales") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  tabla_resumen(visuales_secciones_primero)

## Frecuencias y Porcentaje de la variable Secciones segundo básico Artes Visuales
visuales_secciones_segundo_dyd <- df_dyd |> 
  filter(str_detect(visuales_grados, "Segundo grado")) |>
  filter(str_detect(lenguajes_educacion_artistica, "Artes Visuales") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  tabla_resumen(visuales_secciones_segundo)

## Frecuencias y Porcentaje de la variable Secciones tercero básico Artes Visuales
visuales_secciones_tercero_dyd <- df_dyd |> 
  filter(str_detect(visuales_grados, "Tercer grado")) |>
  filter(str_detect(lenguajes_educacion_artistica, "Artes Visuales") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  tabla_resumen(visuales_secciones_tercero)

## Frecuencias y Porcentaje de la variable Uso de libros Artes Visuales
visuales_uso_libros_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Artes Visuales") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  make_freq_table(visuales_uso_libros, "Uso de libros Artes Visuales")

## Frecuencias y Porcentaje de la variable Recepción libros Artes Visuales Mineduc
visuales_libros_mineduc_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Artes Visuales") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  make_freq_table(visuales_libros_mineduc, "Recepción libros Artes Visuales Mineduc")

## Frecuencias y Porcentaje de la variable Última vez recepción libros Artes Visuales Mineduc
visuales_ultima_vez_libros_mineduc_dyd <- df_dyd |> 
  filter(visuales_libros_mineduc == "Sí") |>
  make_freq_table(visuales_ultima_vez_libros_mineduc, "Última vez recepción libros Artes Visuales Mineduc", 
                  levels_order = c("El 2021", "El 2022", "El 2023", "El 2024",
                                   "Este año", "Otro"))

## Frecuencias y Porcentaje de la variable Recepción materiales Artes Visuales Mineduc
visuales_recepcion_materiales_mineduc_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Artes Visuales") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  make_freq_table(visuales_recepcion_materiales_mineduc, "Recepción materiales Artes Visuales Mineduc")

## Frecuencias y Porcentaje de la variable Materiales recibidos Artes Visuales
visuales_recepcion_materiales_apoyo_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Artes Visuales") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  select_multiple(visuales_recepcion_materiales_apoyo, "Materiales recibidos Artes Visuales")

## Frecuencias y Porcentaje de la variable Necesidad de ajustes Artes Visuales
visuales_ajustes_dyd <- df_dyd |> 
  filter(str_detect(lenguajes_educacion_artistica, "Artes Visuales") | 
           str_detect(lenguajes_educacion_artistica, "Educación Artística \\(Integrada\\)")) |> 
  make_freq_table(visuales_ajustes, "Necesidad de ajustes Artes Visuales")

## Educación Física 

## Frecuencias y Porcentaje de la variable Grados de Educación Física
fisica_grados_dyd <- df_dyd |> 
  filter(str_detect(area, "Educación Física")) |>
  select_multiple(fisica_grados, "Grados de Educación Física")

## Frecuencias y Porcentaje de la variable Secciones primero básico Educación Física
fisica_secciones_primero_dyd <- df_dyd |> 
  filter(str_detect(fisica_grados, "Primer grado")) |>
  filter(str_detect(area, "Educación Física"))|>
  tabla_resumen(fisica_secciones_primero)

## Frecuencias y Porcentaje de la variable Secciones segundo básico Educación Física
fisica_secciones_segundo_dyd <- df_dyd |> 
  filter(str_detect(fisica_grados, "Segundo grado")) |>
  filter(str_detect(area, "Educación Física"))|>
  tabla_resumen(fisica_secciones_segundo)

## Frecuencias y Porcentaje de la variable Secciones tercero básico Educación Física
fisica_secciones_tercero_dyd <- df_dyd |> 
  filter(str_detect(fisica_grados, "Tercer grado")) |>
  filter(str_detect(area, "Educación Física"))|>
  tabla_resumen(fisica_secciones_tercero)

## Frecuencias y Porcentaje de la variable Uso de libros Educación Física
fisica_uso_libros_dyd <- df_dyd |> 
  filter(str_detect(area, "Educación Física")) |> 
  make_freq_table(fisica_uso_libros, "Uso de libros Educación Física")

## Frecuencias y Porcentaje de la variable Recepción libros Educación Física Mineduc
fisica_libros_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Educación Física")) |>
  make_freq_table(fisica_libros_mineduc, "Recepción libros Educación Física Mineduc")

## Frecuencias y Porcentaje de la variable Última vez recepción libros Educación Física Mineduc
fisica_ultima_vez_libros_mineduc_dyd <- df_dyd |> 
  filter(!is.na(fisica_libros_mineduc == "Sí")) |>
    make_freq_table(fisica_ultima_vez_libros_mineduc, "Última vez recepción libros Educación Física Mineduc", 
                  levels_order = c("El 2021", "El 2022", "El 2023", "El 2024",
                                   "Este año", "Otro"))

## Frecuencias y Porcentaje de la variable Recepción materiales Educación Física Mineduc
fisica_recepcion_materiales_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Educación Física")) |>
  make_freq_table(fisica_recepcion_materiales_mineduc, "Recepción materiales Educación Física Mineduc")

## Frecuencias y Porcentaje de la variable Materiales recibidos Educación Física
fisica_recepcion_materiales_apoyo_dyd <- df_dyd |> 
  filter(str_detect(area, "Educación Física")) |>
  select_multiple(fisica_recepcion_materiales_apoyo, "Materiales recibidos Educación Física")

## Frecuencias y Porcentaje de la variable Necesidad de ajustes Educación Física
fisica_ajustes_dyd <- df_dyd |> 
  filter(str_detect(area, "Educación Física")) |>
  make_freq_table(fisica_ajustes, "Necesidad de ajustes Educación Física")

## Emprendimiento para la Productividad 

## Frecuencias y Porcentaje de la variable Grados de Emprendimiento
emp_grados_dyd <- df_dyd |> 
  filter(str_detect(area, "Emprendimiento para la Productividad")) |>
  select_multiple(emp_grados, "Grados de Emprendimiento")

## Frecuencias y Porcentaje de la variable Secciones primero básico Emprendimiento
emp_secciones_primero_dyd <- df_dyd |> 
  filter(str_detect(emp_grados, "Primer grado")) |>
  filter(str_detect(area, "Emprendimiento para la Productividad"))|>
  tabla_resumen(emp_secciones_primero)

## Frecuencias y Porcentaje de la variable Secciones segundo básico Emprendimiento
emp_secciones_segundo_dyd <- df_dyd |> 
  filter(str_detect(emp_grados, "Segundo grado")) |>
  filter(str_detect(area, "Emprendimiento para la Productividad"))|>
  tabla_resumen(emp_secciones_segundo)

## Frecuencias y Porcentaje de la variable Secciones tercero básico Emprendimiento
emp_secciones_tercero_dyd <- df_dyd |> 
  filter(str_detect(emp_grados, "Tercer grado")) |>
  filter(str_detect(area, "Emprendimiento para la Productividad"))|>
  tabla_resumen(emp_secciones_tercero)

## Frecuencias y Porcentaje de la variable Uso de libros Emprendimiento
emp_uso_libros_dyd <- df_dyd |> 
  filter(str_detect(area, "Emprendimiento para la Productividad")) |> 
  make_freq_table(emp_uso_libros, "Uso de libros Emprendimiento")

## Frecuencias y Porcentaje de la variable Recepción libros Emprendimiento Mineduc
emp_libros_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Emprendimiento para la Productividad")) |>
  make_freq_table(emp_libros_mineduc, "Recepción libros Emprendimiento Mineduc")

## Frecuencias y Porcentaje de la variable Última vez recepción libros Emprendimiento Mineduc
emp_ultima_vez_libros_mineduc_dyd <- df_dyd |> 
  filter(!is.na(emp_libros_mineduc == "Sí")) |>
  make_freq_table(emp_ultima_vez_libros_mineduc, "Última vez recepción libros Emprendimiento Mineduc", 
                  levels_order = c("El 2021", "El 2022", "El 2023", "El 2024",
                                   "Este año", "Otro"))

## Frecuencias y Porcentaje de la variable Recepción materiales Emprendimiento Mineduc
emp_recepcion_materiales_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Emprendimiento para la Productividad")) |>
  make_freq_table(emp_recepcion_materiales_mineduc, "Recepción materiales Emprendimiento Mineduc")

## Frecuencias y Porcentaje de la variable Materiales recibidos Emprendimiento
emp_recepcion_materiales_apoyo_dyd <- df_dyd |> 
  filter(str_detect(area, "Emprendimiento para la Productividad")) |>
  select_multiple(emp_recepcion_materiales_apoyo, "Materiales recibidos Emprendimiento")

## Frecuencias y Porcentaje de la variable Necesidad de ajustes Emprendimiento
emp_ajustes_dyd <- df_dyd |> 
  filter(str_detect(area, "Emprendimiento para la Productividad")) |>
  make_freq_table(emp_ajustes, "Necesidad de ajustes Emprendimiento")

## Matemáticas 

## Frecuencias y Porcentaje de la variable Grados de Matemáticas
mate_grados_dyd <- df_dyd |> 
  filter(str_detect(area, "Matemáticas")) |>
  select_multiple(mate_grados, "Grados de Matemáticas")

## Frecuencias y Porcentaje de la variable Secciones primero básico Matemáticas
mate_secciones_primero_dyd <- df_dyd |> 
  filter(str_detect(mate_grados, "Primer grado")) |>
  filter(str_detect(area, "Matemáticas"))|>
  tabla_resumen(mate_secciones_primero)

## Frecuencias y Porcentaje de la variable Secciones segundo básico Matemáticas
mate_secciones_segundo_dyd <- df_dyd |> 
  filter(str_detect(mate_grados, "Segundo grado")) |>
  filter(str_detect(area, "Matemáticas"))|>
  tabla_resumen(mate_secciones_segundo)

## Frecuencias y Porcentaje de la variable Secciones tercero básico Matemáticas
mate_secciones_tercero_dyd <- df_dyd |> 
  filter(str_detect(mate_grados, "Tercer grado")) |>
  filter(str_detect(area, "Matemáticas"))|>
  tabla_resumen(mate_secciones_tercero)

## Frecuencias y Porcentaje de la variable Uso de libros Matemáticas
mate_uso_libros_dyd <- df_dyd |> 
  filter(str_detect(area, "Matemáticas")) |> 
  make_freq_table(mate_uso_libros, "Uso de libros Matemáticas")

## Frecuencias y Porcentaje de la variable Recepción libros Matemáticas Mineduc
mate_libros_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Matemáticas")) |>
  make_freq_table(mate_libros_mineduc, "Recepción libros Matemáticas Mineduc")

## Frecuencias y Porcentaje de la variable Última vez recepción libros Matemáticas Mineduc
mate_ultima_vez_libros_mineduc_dyd <- df_dyd |> 
  filter(!is.na(mate_libros_mineduc == "Sí")) |>
  make_freq_table(mate_ultima_vez_libros_mineduc, "Última vez recepción libros Matemáticas Mineduc", 
                  levels_order = c("El 2021", "El 2022", "El 2023", "El 2024",
                                   "Este año", "Otro"))

## Frecuencias y Porcentaje de la variable Recepción materiales Matemáticas Mineduc
mate_recepcion_materiales_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Matemáticas")) |>
  make_freq_table(mate_recepcion_materiales_mineduc, "Recepción materiales Matemáticas Mineduc")

## Frecuencias y Porcentaje de la variable Materiales recibidos Matemáticas
mate_recepcion_materiales_apoyo_dyd <- df_dyd |> 
  filter(str_detect(area, "Matemáticas")) |>
  select_multiple(mate_recepcion_materiales_apoyo, "Materiales recibidos Matemáticas")

## Frecuencias y Porcentaje de la variable Necesidad de ajustes Matemáticas
mate_ajustes_dyd <- df_dyd |> 
  filter(str_detect(area, "Matemáticas")) |>
  make_freq_table(mate_ajustes, "Necesidad de ajustes Matemáticas")

## Tecnologías del Aprendizaje y la Comunicación -TAC-

## Frecuencias y Porcentaje de la variable Grados de TAC
tac_grados_dyd <- df_dyd |> 
  filter(str_detect(area, "Tecnologías del Aprendizaje y la Comunicación -TAC-")) |>
  select_multiple(tac_grados, "Grados de TAC")

## Frecuencias y Porcentaje de la variable Secciones primero básico TAC
tac_secciones_primero_dyd <- df_dyd |> 
  filter(str_detect(tac_grados, "Primer grado")) |>
  filter(str_detect(area, "Tecnologías del Aprendizaje y la Comunicación -TAC-"))|>
  tabla_resumen(tac_secciones_primero)

## Frecuencias y Porcentaje de la variable Secciones segundo básico TAC
tac_secciones_segundo_dyd <- df_dyd |> 
  filter(str_detect(tac_grados, "Segundo grado")) |>
  filter(str_detect(area, "Tecnologías del Aprendizaje y la Comunicación -TAC-"))|>
  tabla_resumen(tac_secciones_segundo)

## Frecuencias y Porcentaje de la variable Secciones tercero básico TAC
tac_secciones_tercero_dyd <- df_dyd |> 
  filter(str_detect(tac_grados, "Tercer grado")) |>
  filter(str_detect(area, "Tecnologías del Aprendizaje y la Comunicación -TAC-"))|>
  tabla_resumen(tac_secciones_tercero)

## Frecuencias y Porcentaje de la variable Uso de libros TAC
tac_uso_libros_dyd <- df_dyd |> 
  filter(str_detect(area, "Tecnologías del Aprendizaje y la Comunicación -TAC-")) |> 
  make_freq_table(tac_uso_libros, "Uso de libros TAC")

## Frecuencias y Porcentaje de la variable Recepción libros TAC Mineduc
tac_libros_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Tecnologías del Aprendizaje y la Comunicación -TAC-")) |>
  make_freq_table(tac_libros_mineduc, "Recepción libros TAC Mineduc")

## Frecuencias y Porcentaje de la variable Última vez recepción libros TAC Mineduc
tac_ultima_vez_libros_mineduc_dyd <- df_dyd |> 
  filter(!is.na(tac_libros_mineduc == "Sí")) |>
  make_freq_table(tac_ultima_vez_libros_mineduc, "Última vez recepción libros TAC Mineduc", 
                  levels_order = c("El 2021", "El 2022", "El 2023", "El 2024",
                                   "Este año", "Otro"))

## Frecuencias y Porcentaje de la variable Recepción materiales TAC Mineduc
tac_recepcion_materiales_mineduc_dyd <- df_dyd |> 
  filter(str_detect(area, "Tecnologías del Aprendizaje y la Comunicación -TAC-")) |>
  make_freq_table(tac_recepcion_materiales_mineduc, "Recepción materiales TAC Mineduc")

## Frecuencias y Porcentaje de la variable Materiales recibidos TAC
tac_recepcion_materiales_apoyo_dyd <- df_dyd |> 
  filter(str_detect(area, "Tecnologías del Aprendizaje y la Comunicación -TAC-")) |>
  select_multiple(tac_recepcion_materiales_apoyo, "Materiales recibidos TAC")

## Frecuencias y Porcentaje de la variable Necesidad de ajustes TAC
tac_ajustes_dyd <- df_dyd |> 
  filter(str_detect(area, "Tecnologías del Aprendizaje y la Comunicación -TAC-")) |>
  make_freq_table(tac_ajustes, "Necesidad de ajustes TAC")

## Necesidades educativas especiales y adecuaciones curriculares

## Frecuencias y Porcentaje de la variable Diferencia NEE discapacidad y no discapacidad
diferencia_nne_discapacidad_dyd <- df_dyd |> 
  make_freq_table(diferencia_nne_discapacidad, "Diferencia NEE discapacidad y no discapacidad")

## Frecuencias y Porcentaje de la variable Tipos de discapacidad NEE
tipos_nee_asociadas_discapacidad_dyd <- df_dyd |> 
  select_multiple(tipos_nee_asociadas_discapacidad, "Tipos de discapacidad NEE")

## Frecuencias y Porcentaje de la variable Tipos de discapacidad no asociadas NEE
tipos_nee_no_asociadas_discapacidad_dyd <- df_dyd |> 
  select_multiple(tipos_nee_no_asociadas_discapacidad, "Tipos de discapacidad no asociadas NEE")

## Frecuencias y Porcentaje de la variable Adecuaciones curriculares
adecuaciones_curriculares_dyd <- df_dyd |> 
  filter(!str_detect(tipos_nee_asociadas_discapacidad, 
  "No imparto clase a estudiantes con Necesidades Educativas Especiales asociadas a discapacidad") | 
        !str_detect(tipos_nee_no_asociadas_discapacidad, 
  "No imparto clase a estudiantes con Necesidades Educativas Especiales No asociadas a discapacidad")) |> 
  make_freq_table(adecuaciones_curriculares, "Adecuaciones curriculares")

## Frecuencias y Porcentaje de la variable Cantidad de informes adecuaciones curriculares
cantidad_informes_adecuaciones_dyd <- df_dyd |> 
  filter(adecuaciones_curriculares == "Sí") |>
  tabla_resumen(cantidad_informes_adecuaciones)

  ## Frecuencias y Porcentaje de la variable Elementos adecuaciones curriculares
elemento_adecuaciones_dyd <- df_dyd |> 
  filter(adecuaciones_curriculares == "Sí") |>
  select_multiple(elemento_adecuaciones, "Elementos adecuaciones curriculares")

## Bilingüismo 

## Frecuencias y Porcentaje de la variable Estudiantes bilingües
bilinguismo_dyd <- df_dyd |> 
  make_freq_table(bilinguismo, "Estudiantes bilingües")

## Frecuencias y Porcentaje de la variable Idioma de los estudiantes
bilinguismo_idiomas_dyd <- df_dyd |> 
  filter(bilinguismo == "Sí") |>
  select_multiple(bilinguismo_idiomas, "Idioma de los estudiantes")

## Frecuencias y Porcentaje de la variable Idioma Maya de los estudiantes
bilinguismo_maya_dyd <- df_dyd |> 
  filter(str_detect(bilinguismo, "Sí") ,  
         str_detect(bilinguismo_idiomas, "Idioma maya")) |> 
  make_freq_table(bilinguismo_maya, "Idioma Maya de los estudiantes")

## Frecuencias y Porcentaje de la variable Idioma docentes con estudiantes
bilinguismo_docente_con_estudiantes_dyd <- df_dyd |> 
  select_multiple(bilinguismo_docente_con_estudiantes, "Idioma docentes con estudiantes")

## Frecuencias y Porcentaje de la variable Opciones para planificaciones
opciones_planificacion_dyd <- df_dyd |> 
  select_multiple(opciones_planificacion, "Opciones para planificaciones")

## Frecuencias y Porcentaje de la variable Dificultades de implementación CNB
dificultades_implementacion_cnb_dyd <- df_dyd |> 
  select_multiple(dificultades_implementacion_cnb, "Dificultades de implementación CNB")
  
## output list
output_list7 <- list(
  "Número de centros donde trabaja" = cantidad_centros_educativos_dyd, 
  "Docente trabaja en otro centro" = docencia_dyd, 
  "Otro nivel educativo trabaja docente" = especificar_docencia_dyd, 
  "Áreas impartidas" = area_dyd, 
  "Lenguajes de Educación Artística impartidos" = lenguajes_educacion_artistica_dyd, 
  "Grados de Ciencias Naturales" = cn_grados_dyd, 
  "Secciones primero básico CN" = cn_secciones_primero_dyd, 
  "Secciones segundo básico CN" = cn_secciones_segundo_dyd, 
  "Secciones tercero básico CN" = cn_secciones_tercero_dyd, 
  "Uso de libros CN" = cn_uso_libros_dyd, 
  "Recepción libros CN Mineduc" = cn_libros_mineduc_dyd, 
  "Última vez recepción libros CN Mineduc" = cn_ultima_vez_libros_mineduc_dyd, 
  "Recepción materiales CN Mineduc" = cn_recepcion_materiales__mineduc_dyd, 
  "Materiales recibidos CN" = cn_recepcion_materiales_apoyo_dyd,   
  "Necesidad de ajustes CN" = cn_ajustes_dyd, 
  "Grados de Ciencias Sociales" = cs_grados_dyd, 
  "Secciones primero básico CS" = cs_secciones_primero_dyd, 
  "Secciones segundo básico CS" = cs_secciones_segundo_dyd, 
  "Secciones tercero básico CS" = cs_secciones_tercero_dyd, 
  "Uso de libros CS" = cs_uso_libros_dyd, 
  "Recepción libros CS Mineduc" = cs_libros_mineduc_dyd, 
  "Última vez recepción libros CS Mineduc" = cs_ultima_vez_libros_mineduc_dyd, 
  "Recepción materiales CS Mineduc" = cs_recepcion_materiales_mineduc_dyd, 
  "Materiales recibidos CS" = cs_recepcion_materiales_apoyo_dyd, 
  "Necesidad de ajustes CS" = cs_ajustes_dyd, 
  "Grados de Comunicación y lenguaje" = cl_grados_dyd, 
  "Secciones primero básico CL" = cl_secciones_primero_dyd, 
  "Secciones segundo básico CL" = cl_secciones_segundo_dyd, 
  "Secciones tercero básico CL" = cl_secciones_tercero_dyd, 
  "Uso de libros CL" = cl_uso_libros_dyd, 
  "Recepción libros CL Mineduc" = cl_libros_mineduc_dyd, 
  "Última vez recepción libros CL Mineduc" = cl_ultima_vez_libros_mineduc_dyd, 
  "Recepción materiales CL Mineduc" = cl_recepcion_materiales_mineduc_dyd, 
  "Materiales recibidos CL" = cl_recepcion_materiales_apoyo_dyd, 
  "Necesidad de ajustes CL" = cl_ajustes_dyd, 
  "Grados Idioma inglés" = in_grados_dyd, 
  "Secciones primero básico IN" = in_secciones_primero_dyd, 
  "Secciones segundo básico IN" = in_secciones_segundo_dyd, 
  "Secciones tercero básico IN" = in_secciones_tercero_dyd, 
  "Uso de libros IN" = in_uso_libros_dyd, 
  "Recepción libros IN Mineduc" = in_libros_mineduc_dyd, 
  "Última vez recepción libros IN Mineduc" = in_ultima_vez_libros_mineduc_dyd, 
  "Recepción materiales IN Mineduc" = in_recepcion_materiales_mineduc_dyd, 
  "Materiales recibidos IN" = in_recepcion_materiales_apoyo_dyd,
  "Necesidad de ajustes IN" = in_ajustes_dyd, 
  "Grados Culturas Mayas" = cm_grados_dyd, 
  "Secciones primero básico CM" = cm_secciones_primero_dyd, 
  "Secciones segundo básico CM" = cm_secciones_segundo_dyd, 
  "Secciones tercero básico CM" = cm_secciones_tercero_dyd, 
  "Uso de libros CM" = cm_uso_libros_dyd, 
  "Recepción libros CM Mineduc" = cm_libros_mineduc_dyd, 
  "Última vez recepción libros CM Mineduc" = cm_ultima_vez_libros_mineduc_dyd, 
  "Recepción materiales CM Mineduc" = cm_recepcion_materiales_mineduc_dyd, 
  "Materiales recibidos CM" = cm_recepcion_materiales_apoyo_dyd, 
  "Necesidad de ajustes CM" = cm_ajustes_dyd, 
  "Grados Teatro" = teatro_grados_dyd, 
  "Secciones primero básico Teatro" = teatro_secciones_primero_dyd, 
  "Secciones segundo básico Teatro" = teatro_secciones_segundo_dyd, 
  "Secciones tercero básico Teatro" = teatro_secciones_tercero_dyd, 
  "Uso de libros Teatro" = teatro_uso_libros_dyd, 
  "Recepción libros Teatro Mineduc" = teatro_libros_mineduc_dyd, 
  "Última vez recepción libros Teatro Mineduc" = teatro_ultima_vez_libros_mineduc_dyd, 
  "Recepción materiales Teatro Mineduc" = teatro_recepcion_materiales_mineduc_dyd, 
  "Materiales recibidos Teatro" = teatro_recepcion_materiales_apoyo_dyd, 
  "Necesidad de ajustes Teatro" = teatro_ajustes_dyd, 
  "Grados Danza" = danza_grados_dyd, 
  "Secciones primero básico Danza" = danza_secciones_primero_dyd, 
  "Secciones segundo básico Danza" = danza_secciones_segundo_dyd, 
  "Secciones tercero básico Danza" = danza_secciones_tercero_dyd, 
  "Uso de libros Danza" = danza_uso_libros_dyd, 
  "Recepción libros Danza Mineduc" = danza_libros_mineduc_dyd, 
  "Última vez recepción libros Danza Mineduc" = danza_ultima_vez_libros_mineduc_dyd, 
  "Recepción materiales Danza Mineduc" = danza_recepcion_materiales_mineduc_dyd, 
  "Materiales recibidos Danza" = danza_recepcion_materiales_apoyo_dyd, 
  "Necesidad de ajustes Danza" = danza_ajustes_dyd, 
  "Grados Educación Musical" = musica_grados_dyd, 
  "Secciones primero básico Educación Musical" = musica_secciones_primero_dyd, 
  "Secciones segundo básico Educación Musical" = musica_secciones_segundo_dyd, 
  "Secciones tercero básico Educación Musical" = musica_secciones_tercero_dyd, 
  "Uso de libros Educación Musical" = musica_uso_libros_dyd, 
  "Recepción libros Educación Musical Mineduc" = musica_libros_mineduc_dyd, 
  "Última vez recepción libros Educación Musical Mineduc" = musica_ultima_vez_libros_mineduc_dyd, 
  "Recepción materiales Educación Musical Mineduc" = musica_recepcion_materiales_mineduc_dyd, 
  "Materiales recibidos Educación Musical" = musica_recepcion_materiales_apoyo_dyd, 
  "Necesidad de ajustes Educación Musical" = musica_ajustes_dyd, 
  "Grados Artes Visuales" = visuales_grados_dyd, 
  "Secciones primero básico Artes Visuales" = visuales_secciones_primero_dyd, 
  "Secciones segundo básico Artes Visuales" = visuales_secciones_segundo_dyd, 
  "Secciones tercero básico Artes Visuales" = visuales_secciones_tercero_dyd, 
  "Uso de libros Artes Visuales" = visuales_uso_libros_dyd, 
  "Recepción libros Artes Visuales Mineduc" = visuales_libros_mineduc_dyd, 
  "Última vez recepción libros Artes Visuales Mineduc" = visuales_ultima_vez_libros_mineduc_dyd, 
  "Recepción materiales Artes Visuales Mineduc" = visuales_recepcion_materiales_mineduc_dyd, 
  "Materiales recibidos Artes Visuales" = visuales_recepcion_materiales_apoyo_dyd, 
  "Necesidad de ajustes Artes Visuales" = visuales_ajustes_dyd, 
  "Grados de Educación Física" = fisica_grados_dyd, 
  "Secciones primero básico Educación Física" = fisica_secciones_primero_dyd, 
  "Secciones segundo básico Educación Física" = fisica_secciones_segundo_dyd, 
  "Secciones tercero básico Educación Física" = fisica_secciones_tercero_dyd, 
  "Uso de libros Educación Física" = fisica_uso_libros_dyd, 
  "Recepción libros Educación Física Mineduc" = fisica_libros_mineduc_dyd, 
  "Última vez recepción libros Educación Física Mineduc" = fisica_ultima_vez_libros_mineduc_dyd, 
  "Recepción materiales Educación Física Mineduc" = fisica_recepcion_materiales_mineduc_dyd, 
  "Materiales recibidos Educación Física" = fisica_recepcion_materiales_apoyo_dyd, 
  "Necesidad de ajustes Educación Física" = fisica_ajustes_dyd, 
  "Grados de Emprendimiento" = emp_grados_dyd, 
  "Secciones primero básico Emprendimiento" = emp_secciones_primero_dyd, 
  "Secciones segundo básico Emprendimiento" = emp_secciones_segundo_dyd, 
  "Secciones tercero básico Emprendimiento" = emp_secciones_tercero_dyd, 
  "Uso de libros Emprendimiento" = emp_uso_libros_dyd, 
  "Recepción libros Emprendimiento Mineduc" = emp_libros_mineduc_dyd, 
  "Última vez recepción libros Emprendimiento Mineduc" = emp_ultima_vez_libros_mineduc_dyd, 
  "Recepción materiales Emprendimiento Mineduc" = emp_recepcion_materiales_mineduc_dyd, 
  "Materiales recibidos Emprendimiento" = emp_recepcion_materiales_apoyo_dyd, 
  "Necesidad de ajustes Emprendimiento" = emp_ajustes_dyd, 
  "Grados de Matemáticas" = mate_grados_dyd, 
  "Secciones primero básico Matemáticas" = mate_secciones_primero_dyd, 
  "Secciones segundo básico Matemáticas" = mate_secciones_segundo_dyd, 
  "Secciones tercero básico Matemáticas" = mate_secciones_tercero_dyd, 
  "Uso de libros Matemáticas" = mate_uso_libros_dyd, 
  "Recepción libros Matemáticas Mineduc" = mate_libros_mineduc_dyd, 
  "Última vez recepción libros Matemáticas Mineduc" = mate_ultima_vez_libros_mineduc_dyd, 
  "Recepción materiales Matemáticas Mineduc" = mate_recepcion_materiales_mineduc_dyd, 
  "Materiales recibidos Matemáticas" = mate_recepcion_materiales_apoyo_dyd, 
  "Necesidad de ajustes Matemáticas" = mate_ajustes_dyd, 
  "Grados de TAC" = tac_grados_dyd, 
  "Secciones primero básico TAC" = tac_secciones_primero_dyd, 
  "Secciones segundo básico TAC" = tac_secciones_segundo_dyd, 
  "Secciones tercero básico TAC" = tac_secciones_tercero_dyd, 
  "Uso de libros TAC" = tac_uso_libros_dyd, 
  "Recepción libros TAC Mineduc" = tac_libros_mineduc_dyd, 
  "Última vez recepción libros TAC Mineduc" = tac_ultima_vez_libros_mineduc_dyd, 
  "Recepción materiales TAC Mineduc" = tac_recepcion_materiales_mineduc_dyd, 
  "Materiales recibidos TAC" = tac_recepcion_materiales_apoyo_dyd, 
  "Necesidad de ajustes TAC" = tac_ajustes_dyd, 
  "Diferencia NEE discapacidad y no discapacidad" = diferencia_nne_discapacidad_dyd,
  "Tipos de discapacidad NEE" = tipos_nee_asociadas_discapacidad_dyd, 
  "Tipos de discapacidad no asociadas NE" = tipos_nee_no_asociadas_discapacidad_dyd,
  "Adecuaciones curriculares" = adecuaciones_curriculares_dyd, 
  "Cantidad de informes adecuaciones curriculares" = cantidad_informes_adecuaciones_dyd, 
  "Elementos adecuaciones curriculares" = elemento_adecuaciones_dyd, 
  "Estudiantes bilingües" = bilinguismo_dyd, 
  "Idioma de los estudiantes" = bilinguismo_idiomas_dyd, 
  "Idioma Maya de los estudiantes" = bilinguismo_maya_dyd, 
  "Idioma docentes con estudiantes" = bilinguismo_docente_con_estudiantes_dyd, 
  "Opciones para planificaciones" = opciones_planificacion_dyd, 
  "Dificultades de implementación CNB" = dificultades_implementacion_cnb_dyd)
  
## Sección 8 Preguntas solo para directores ----

## Frecuencias y Porcentaje de la variable Remozamiento
remozamiento_dyd <- df_dyd |> 
  filter(puesto != "Docente", 
         sector == "Oficial")|>
  make_freq_table(remozamiento, "Remozamiento")

## Frecuencias y Porcentaje de la variable Año de remozamiento
ano_remozamiento_dyd <- df_dyd |> 
  mutate(ano_remozamiento = as.character(ano_remozamiento),
         ano_remozamiento = recode(ano_remozamiento, .missing = "Sin información")) |>
  filter(remozamiento == "Sí")|>
  make_freq_table(ano_remozamiento, "Año de remozamiento")

## Frecuencias y Porcentaje de la variable Mes de remozamiento
mes_remozamiento_dyd <- df_dyd |> 
  filter(remozamiento == "Sí")|>
  make_freq_table(mes_remozamiento, "Mes de remozamiento", 
                  levels_order = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                                   "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre",
                                   "Diciembre"))

## Frecuencias y Porcentaje de la variable Fuente de energía eléctrica
energia_fuente_dyd <- df_dyd |> 
  filter(puesto != "Docente")|>
  make_freq_table(energia_fuente, "Fuente de energía eléctrica")

## Frecuencias y Porcentaje de la variable Actualmente hay energía eléctrica
energia_cuenta_actualmente_dyd <- df_dyd |> 
  filter(energia_fuente != "El Centro Educativo no cuenta con ninguna fuente de energía")|>
  make_freq_table(energia_cuenta_actualmente, "Actualmente hay energía eléctrica")

## Frecuencias y Porcentaje de la variable Tiempo sin energía eléctrica
energia_tiempo_dyd <- df_dyd |> 
  filter(energia_cuenta_actualmente == "No")|>
  make_freq_table(energia_tiempo, "Tiempo sin energía eléctrica")

## Frecuencias y Porcentaje de la variable Nunca ha tenido energía eléctrica
energia_razon_nunca_dyd <- df_dyd |> 
  filter(energia_tiempo == "El Centro Educativo nunca ha tenido energía eléctrica" | 
         energia_fuente == "El Centro Educativo no cuenta con ninguna fuente de energía")|>
  make_freq_table(energia_razon_nunca, "Nunca ha tenido energía eléctrica")

## Frecuencias y Porcentaje de la variable Centro no tiene energía eléctrica
energia_razon_dyd <- df_dyd |> 
  filter(energia_cuenta_actualmente == "No")|>
  make_freq_table(energia_razon, "Centro no tiene energía eléctrica")

## Frecuencias y Porcentaje de la variable Centro cuenta con agua
agua_centro_dyd <- df_dyd |> 
  filter(puesto != "Docente")|>
  make_freq_table(agua_centro, "Centro cuenta con agua")

## Frecuencias y Porcentaje de la variable Tiempo sin agua
agua_tiempo_dyd <- df_dyd |> 
  filter(agua_centro == "No")|>
  make_freq_table(agua_tiempo, "Tiempo sin agua", 
                  levels_order = c("Menos de una semana", "Un mes", "Entre 2 y 3 meses", "Un año",
                                   "Más de un año", "El Centro Educativo nunca ha tenido agua potable"))

## Frecuencias y Porcentaje de la variable Centro cuenta con CAT
cat_laboratorio_centro_dyd <- df_dyd |> 
  filter(puesto != "Docente")|>
  make_freq_table(cat_laboratorio_centro, "Centro cuenta con CAT")

## Frecuencias y Porcentaje de la variable CAT externo
cat_encargado_dyd <- df_dyd |> 
  filter(cat_laboratorio_centro == "No")|>
  make_freq_table(cat_encargado, "CAT externo")

## Frecuencias y Porcentaje de la variable Centro cuenta con internet
internet_centro_dyd <- df_dyd |> 
  filter(energia_tiempo != "El Centro Educativo nunca ha tenido energía eléctrica" | 
         energia_cuenta_actualmente == "Sí")|>
  make_freq_table(internet_centro, "Centro cuenta con internet")

## Frecuencias y Porcentaje de la variable Visita del supervisor
visita_supervisor_dyd <- df_dyd |> 
  filter(puesto != "Docente")|>
  make_freq_table(visita_supervisor, "Visita del supervisor", 
                  levels_order = c("Cada semana", "Cada quince días", "Cada mes", "Cada bimestre", 
                                   "Cada semestre", "Cada año", "Otra temporalidad", 
                                   "No visita el Centro Educativo"))

## Frecuencias y Porcentaje de la variable Visita del coordinador
visita_coordinador_dyd <- df_dyd |> 
  filter(puesto != "Docente")|>
  make_freq_table(visita_coordinador, "Visita del coordinador", 
                  levels_order = c("Cada semana", "Cada quince días", "Cada mes", "Cada bimestre", 
                                   "Cada semestre", "Cada año", "Otra temporalidad", 
                                   "No visita el Centro Educativo"))

## Frecuencias y Porcentaje de la variable Visita de otro personal
visita_otro_personal_dyd <- df_dyd |> 
  filter(puesto != "Docente")|>
  make_freq_table(visita_otro_personal, "Visita de otro personal", 
                  levels_order = c("Cada semana", "Cada quince días", "Cada mes", "Cada bimestre", 
                                   "Cada semestre", "Cada año", "Otra temporalidad", 
                                   "No visita el Centro Educativo"))

## Frecuencias y Porcentaje de la variable Visita DIGEMOCA
visita_digemoca_dyd <- df_dyd |> 
  filter(puesto != "Docente")|>
  make_freq_table(visita_digemoca, "Visita DIGEMOCA", 
                  levels_order = c("Cada semana", "Cada quince días", "Cada mes", "Cada bimestre", 
                                   "Cada semestre", "Cada año", "Otra temporalidad", 
                                   "No visita el Centro Educativo"))

## Frecuencias y Porcentaje de la variable Visita DEFOCE
visita_defoce_dyd <- df_dyd |> 
  filter(puesto != "Docente")|>
  make_freq_table(visita_defoce, "Visita DEFOCE", 
                  levels_order = c("Cada semana", "Cada quince días", "Cada mes", "Cada bimestre", 
                                   "Cada semestre", "Cada año", "Otra temporalidad", 
                                   "No visita el Centro Educativo"))

## Frecuencias y Porcentaje de la variable Necesidades del centro
necesidades_tipo_dyd <- df_dyd |> 
  filter(puesto != "Docente")|>
  select_multiple(necesidades_tipo, "Necesidades del centro")

## Frecuencias y Porcentaje de la variable Necesidades implementación CNB
necesidades_implementacion_cnb_dyd <- df_dyd |> 
  filter(puesto != "Docente")|>
  select_multiple(necesidades_implementacion_cnb, "Necesidades implementación CNB")

## output list
output_list8 <- list(
  "Remozamiento" = remozamiento_dyd, 
  "Año de remozamiento" = ano_remozamiento_dyd, 
  "Mes de remozamiento" = mes_remozamiento_dyd, 
  "Fuente de energía eléctrica" = energia_fuente_dyd, 
  "Actualmente hay energía eléctrica" = energia_cuenta_actualmente_dyd, 
  "Tiempo sin energía eléctrica" = energia_tiempo_dyd, 
  "Nunca ha tenido energía eléctrica" = energia_razon_nunca_dyd, 
  "Centro no tiene energía eléctrica" = energia_razon_dyd, 
  "Centro cuenta con agua" = agua_centro_dyd, 
  "Tiempo sin agua" = agua_tiempo_dyd, 
  "Centro cuenta con CAT" = cat_laboratorio_centro_dyd, 
  "CAT externo" = cat_encargado_dyd, 
  "Centro cuenta con internet" = internet_centro_dyd, 
  "Visita del supervisor" = visita_supervisor_dyd, 
  "Visita del coordinador" = visita_coordinador_dyd, 
  "Visita de otro personal" = visita_otro_personal_dyd, 
  "Visita DIGEMOCA" = visita_digemoca_dyd, 
  "Visita DEFOCE" = visita_defoce_dyd, 
  "Necesidades del centro" = necesidades_tipo_dyd, 
  "Necesidades implementación CNB" = necesidades_implementacion_cnb_dyd)

## Sección Roster Títulos obtenidos
## no está el roster en el excel "Listos para análisis", se usó la hoja 2 del otro archivo 
## Frecuencias y Porcentaje de la variable Grado académico reportado
reportado_grado_academico_dydR <- df_dydR |> 
make_freq_table(`26. ¿Qué grado académico reportará?`, "Grado académico reportado")

##output list
output_list9 <- list(
"Grado académico reportado" = reportado_grado_academico_dydR)
  
# generar archivos de excel ----
write_xlsx(output_list1, here("Resultados","Encuesta docente","tablas_seccion_1.xlsx"))
write_xlsx(output_list2, here("Resultados","Encuesta docente","tablas_seccion_2.xlsx"))
write_xlsx(output_list3, here("Resultados","Encuesta docente","tablas_seccion_3.xlsx"))
write_xlsx(output_list4, here("Resultados","Encuesta docente","tablas_seccion_4.xlsx"))
write_xlsx(output_list6, here("Resultados","Encuesta docente","tablas_seccion_6.xlsx"))
write_xlsx(output_list7, here("Resultados","Encuesta docente","tablas_seccion_7.xlsx"))
write_xlsx(output_list8, here("Resultados","Encuesta docente","tablas_seccion_8.xlsx"))
write_xlsx(output_list9, here("Resultados","Encuesta docente","tablas_roster.xlsx"))

