library(tidyverse)
library(readxl)
library(writexl)

df <- read_xlsx("df_2025_limpia_1.xlsx")

# eliminar variables que no sirven

df <- df |> 
  select(-c(old_version_municipio_de_origen))

# imputations

## no binario

df$genero[df$otro_genero == "Persona no binaria"] <- "No binario"
df$genero[df$otro_genero == "No binario"] <- "No binario"

# con quien vive otro cual

df$con_quien_vive_otro_cual[df$con_quien_vive_otro_cual == "Cuñado y sobrina"] <- "Cuñado/s, Sobrina/s"
df$con_quien_vive_otro_cual[df$con_quien_vive_otro_cual == "Sobrinos y cuñado"] <- "Sobrino/s, Cuñada/s"
df$con_quien_vive_pareja[df$con_quien_vive_otro_cual == "Esposo"] <- "Pareja"
df$con_quien_vive_otro_cual[df$con_quien_vive_otro_cual == "Esposo"] <- NA
df$con_quien_vive_otro_cual[df$con_quien_vive_otro_cual == "Cuñado"] <- "Cuñado/s"
df$con_quien_vive_otro_cual[df$con_quien_vive_otro_cual == "Vivo sola pero mi papa duerme 1 o 2 veces al mes en mi apartamento"] <- "Solo/a"
df$con_quien_vive_otro_cual[df$con_quien_vive_otro_cual == "Sobrino"] <- "Sobrino/s"
df$con_quien_vive_otro_cual[df$con_quien_vive_otro_cual == "Sobrina"] <- "Sobrina/s"
df$con_quien_vive_otro_cual[df$con_quien_vive_otro_cual == "Sobrinos"] <- "Sobrino/s"
df$con_quien_vive_otro_cual[df$con_quien_vive_otro_cual == "Cuñada y Sobrino"] <- "Cuñada/s, Sobrino/s"
df$con_quien_vive_otro_cual[df$con_quien_vive_otro_cual == "2 sobrinos"] <- "Sobrino/s"
df$con_quien_vive_otro_cual[df$con_quien_vive_otro_cual == "Cuñada y sobrina"] <- "Cuñada/s, Sobrina/s"
df$con_quien_vive_otro_cual[df$con_quien_vive_otro_cual == "Independientemente"] <- "Solo/a"
df$con_quien_vive_otro_cual[df$con_quien_vive_otro_cual == "Cuñada y 2 sobrinos menores de 6 años"] <- "Cuñada/s, Sobrino/s"
df$con_quien_vive_otro_cual[df$con_quien_vive_otro_cual == "Hija de mi pareja, papas de mi pareja"] <- "Hijastra, Suegros"

# tiempo fuera de la jornada laboral, hora ocio, horas ejercicio

df$tiempo_fuera_joranda_laboral <- ifelse(
  df$tiempo_fuera_joranda_laboral == "Otro (escriba en números las horas)",
  as.numeric(as.character(df$otro_tiempo_fuera_joranda_laboral)),
  as.numeric(df$tiempo_fuera_joranda_laboral)
)

df$horas_ocio <- ifelse(
  df$horas_ocio == "Otro (escriba en números las horas)",
  as.numeric(as.character(df$otro_horas_ocio)),
  as.numeric(df$horas_ocio)
)

df$horas_semana_ejercicio <- ifelse(
  df$horas_semana_ejercicio == "Otro (escriba en números las horas)",
  as.numeric(as.character(df$otro_horas_semana_ejercicio)),
  as.numeric(df$horas_semana_ejercicio)
)

df <- df |> 
  select(-c(otro_tiempo_fuera_joranda_laboral,
            otro_horas_ocio,
            otro_horas_semana_ejercicio))

## meses buscando empleo

df <- df |> 
  mutate(meses_buscando_empleo = recode(meses_buscando_empleo,
                                                      "1 año" = "12",
                                                      "1 año y 2 meses" = "14",
                                                      "1 mes" = "1",
                                                      "1 mes y medio aprox" = "1",
                                                      "1 mes" = "1",
                                                      "1 semana" = "0",
                                                      "18 meses" = "18",
                                                      "1mes" = "1",
                                                      "2 años" = "24",
                                                      "2 meses" = "2",
                                                      "2 semanas" = "0",
                                                      "24 meses" = "24",
                                                      "28 meses" = "28",
                                                      "3 meses" = "3",
                                                      "4 meses" = "4",
                                                      "5 meses" = "5",
                                                      "6 años" = "72",
                                                      "6 meses" = "6",
                                                      "6-7 meses" = "7",
                                                      "7 meses" = "7",
                                                      "8 meses" = "8",
                                                      "Aproximadamente 2" = "2",
                                                      "Cinco meses" = "5",
                                                      "Como 9 meses" = "9",
                                                      "Dos meses" = "2",
                                                      "Empezando" = "0",
                                                      "Hace menos de un mes" = "0",
                                                      "Hace unas semanas" = "0",
                                                      "Hace unos meses" = NA_character_,  
                                                      "iniciando busca de opciones" = "0",
                                                      "Iniciando llevo una semana" = "0",
                                                      "Llevo 3 meses pero no me urge, solo es para pasar el tiempo" = "3",
                                                      "Menos de un mes" = "0",
                                                      "Ni uno jaj" = "0",
                                                      "Ninguno, solo quiero encontrar un mejor empleo donde pueda ganar el salario mínimo" = "0",
                                                      "Recién tomo la iniciativa" = "0",
                                                      "Un mes" = "1",
                                                      "Un par" = "2",
                                                      "Un par de semanas" = "0",
                                                      "Una semana" = "0",
                                                      "1 mes, 2 meses sin empleo" = "1")) |> 
  mutate(meses_buscando_empleo = as.numeric(meses_buscando_empleo))

## momrona

df$que_religion_practica[df$que_religion_practica == "Momrona"] <- "Mormona"

# Merge municipios

rango_vars <- match("municipio_de_origen_guatemala", colnames(df)):match("municipio_de_origen_izabal", colnames(df))

df <- df %>%
  mutate(municipio_de_origen = pmap_chr(df[, rango_vars], ~ coalesce(...)))


# eliminar variables

df <- df |> 
  dplyr::select(-c(rango_vars)) |> 
  relocate(municipio_de_origen, .after = departamento_de_origen)

df <- df |> 
  select(-c(time_started,
            date_submitted,
            consentimiento,
            otro_genero,
            otro_autoidentificacion_etnica,
            old_version_con_quienes_vive,
            old_version_en_que_semestre_tiene_cursos_asignados,
            mas_especificar_numero_cuantos_hijos_tiene))

# contar enfermedades

rango_enfermedades <- match("enfermedad_diabetes", colnames(df)):match("enfermedad_otra", colnames(df))

df <- df %>%
  mutate(total_enfermedades = rowSums(!is.na(df[, rango_enfermedades]))) |> 
  relocate(total_enfermedades, .after = "enfermedad_otra_cual")

## lista de enfermedades

df <- df |> 
  unite(lista_enfermedades, all_of(rango_enfermedades), remove = F, na.rm = T, sep = ", ") |> 
  relocate(lista_enfermedades, .after = "total_enfermedades")
  
## lista de con quienes vive

rango_con_quien_vive <- match("con_quien_vive_papa", colnames(df)):match("con_quien_vive_otro_cual", colnames(df))

rango_con_quien_vive <- rango_con_quien_vive[-(length(rango_con_quien_vive)-1)]

df <- df |> 
  unite(lista_con_quien_vive, all_of(rango_con_quien_vive), remove = F, na.rm = T, sep = ", ") |> 
  relocate(lista_con_quien_vive, .after = "con_quien_vive_otro_cual")

# horas de trabajo al día

df <- df |> 
  mutate(horas_trabajo_al_dia = case_match(horas_trabajo_al_dia,
                                       8.3 ~ 8,
                                       9.3 ~ 9,
                                       48 ~ NA,
                                       89 ~ NA,
                                       .default = horas_trabajo_al_dia
                                       ))

df <- df |> 
  mutate(horas_ocio = case_match(horas_ocio,
                                           30 ~ NA,
                                           .default = horas_ocio
  ))

df <- df |> 
  mutate(horas_semana_ejercicio = case_match(horas_semana_ejercicio,
                                 130 ~ NA,
                                 .default = horas_semana_ejercicio
  ))

df$horas_totales_diarias_trabajo <- rowSums(df[,c("horas_trabajo_al_dia","tiempo_fuera_joranda_laboral")]) 

df <- df |> 
  relocate(horas_totales_diarias_trabajo, .after = tiempo_fuera_joranda_laboral)

table(df$medio_trasporte_universidad)

df$medio_trasporte_universidad[df$otro_medio_trasporte_universidad == "Bus universitario"] <- "Bus universitario"

df$medio_trasporte_universidad[df$otro_medio_trasporte_universidad == "Automóvil y transporte público"] <- "Automóvil y transporte público"

df$medio_trasporte_universidad[df$otro_medio_trasporte_universidad == "Uso automóvil y regreso en transporte público"] <- "Automóvil y transporte público"

df$medio_trasporte_universidad[df$otro_medio_trasporte_universidad == "Auto propio. Bus urbano y a pie"] <- "Automóvil, transporte público y a pie"

# convertir variables de ingresos

df <- df |> 
  mutate(rango_ingresos_personal_numeric = case_match(rango_ingresos_personal,
                                                     "Menos de 1,000 quetzales"  ~ 1,
                                                     "1,001 - 3,000 quetzales" ~ 2,
                                                     "3,001 - 5,000 quetzales" ~ 3,
                                                     "5,001 - 7,000 quetzales" ~ 4,
                                                     "7,001 - 9,000 quetzales" ~ 5,
                                                     "9,001 - 11,000 quetzales" ~ 6,
                                                     "Más de 11,000 quetzales" ~ 7
  )) |> 
  relocate(rango_ingresos_personal_numeric, .after = rango_ingresos_personal)

df <- df |> 
  mutate(rango_ingresos_familia_numeric = case_match(rango_ingresos_familia,
                                                      "Menos de Q. 1,000"  ~ 1,
                                                      "Q. 1,001 - Q. 3,000" ~ 2,
                                                      "Q. 3,001 - Q. 5,000" ~ 3,
                                                      "Q. 5,001 - Q. 7,000" ~ 4,
                                                      "Q. 7,001 - Q. 9,000" ~ 5,
                                                      "Q. 9,001 - Q. 11,000" ~ 6,
                                                      "Q. 11,001 - Q. 15,000" ~ 7,
                                                     "Más de Q.15.000" ~ 8
  )) |> 
  relocate(rango_ingresos_familia_numeric, .after = rango_ingresos_familia)

df <- df |> 
  mutate(percepcion_situacion_economica_numeric = case_match(percepcion_situacion_economica,
                                                     "Mi familia (o quienes vivimos juntos/as) percibimos menos ingresos de los necesarios para cubrir nuestros gastos."  ~ 1,
                                                     "Mi familia (o quienes vivimos juntos/as) percibimos los ingresos necesarios para cubrir nuestros gastos, pero no logramos ahorrar o gastar en lujos." ~ 2,
                                                     "Mi familia (o quienes vivimos juntos/as) percibimos los ingresos necesarios para cubrir nuestros gastos y vivir con comodidades, logramos algún ahorro y gastamos eventualmente en un gusto o lujo." ~ 3,
                                                     'Mi familia (o quienes vivimos juntos/as) percibimos más ingresos de los que necesitamos (podemos ahorrar y gastar en "gustos" o "lujos").' ~ 4
  )) |> 
  relocate(percepcion_situacion_economica_numeric, .after = percepcion_situacion_economica)

# años cuando empezó a trabajar

df$edad_de_inicio_trabajo[df$edad_de_inicio_trabajo == 0] <- NA

# número de idiomas

rango_idiomas <- match("idiomas_espanol", colnames(df)):match("idiomas_otro", colnames(df))

df <- df %>%
  mutate(total_idiomas = rowSums(!is.na(df[, rango_idiomas]))) |> 
  relocate(total_idiomas, .after = "idiomas_otro_cual")

write_xlsx(df, "df_2025_limpia_2.xlsx")

