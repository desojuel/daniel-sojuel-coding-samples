# demográficos ---- 

obs01_demograficos <- c("index",
                    "codigo",
                    "departamento",
                    "municipio",
                    "punto_geografico",
                    "punto_geografico_latitude",
                    "punto_geografico_longitude",
                    "sector",
                    "modalidad",
                    "plan",
                    "jornada",
                    "poblacion",
                    "grado",
                    "lenguajes_educacion_artistica")

# change class ---- 

obs01$ano_remozamiento <- as.character(obs01$ano_remozamiento)

# demograficas long

obs01_demograficos_long_array <- c("modalidad",
                        "plan",
                        "jornada",
                        "poblacion",
                        "grado")

obs01_demograficos_long <- obs01 |> 
  select(all_of(obs01_demograficos)) |> 
  mutate(
    across(
      all_of(obs01_demograficos_long_array),
    ~ .x,
    .names = "p_{.col}")
  ) |> 
  pivot_longer(starts_with("p_"),
               names_to = "variable",
               values_to = "valores") |> 
  mutate(variable = case_match(variable,
                               "p_jornada" ~ "Jornada",
                               "p_plan" ~ "Plan",
                               "p_grado" ~ "Grado",
                               "p_poblacion" ~ "Población",
                               "p_modalidad" ~ "Modalidad"))


# salones ----

# labels 

df_labels_obs01 <- read_xlsx(here("datasets para procesamiento","labels obs01 long salones.xlsx"))

labels_char_obs01 <- df_labels_obs01$name_char[!is.na(df_labels_obs01$name_char)]

labels_double_obs01 <- df_labels_obs01$name_double[!is.na(df_labels_obs01$name_double)]

vars_obs01_salones_long <- c(obs01_demograficos,
                             labels_char_obs01,
                             labels_double_obs01)

# select variables 

obs01_salones_long1 <- obs01 |> 
  select(all_of(c(obs01_demograficos,
                  labels_char_obs01))) |> 
  pivot_longer(all_of(labels_char_obs01), names_to = "variables1",
               values_to = "valores1") 

df_labels_obs01_1 <- df_labels_obs01 |> 
  rename(variables1 = name_char) |> 
  select(-name_double)

obs01_salones_long1 <- obs01_salones_long1 |> 
  left_join(df_labels_obs01_1,by = "variables1")


obs01_salones_long2 <- obs01 |> 
  select(all_of(c(obs01_demograficos,labels_double_obs01))) |> 
  pivot_longer(all_of(labels_double_obs01), names_to = "variables2",
               values_to = "valores2")


df_labels_obs01_2 <- df_labels_obs01 |> 
  rename(variables2 = name_double) |> 
  select(-name_char)

obs01_salones_long2 <- obs01_salones_long2 |> 
  left_join(df_labels_obs01_2,by = "variables2")


# esto hace un join sin que sean exactamente las mismas variables, si hay una extra, solo la agrega al final

obs01_salones_long <- bind_rows(obs01_salones_long1,
                                obs01_salones_long2)


# infraestructura ----

# labels 

df_labels_obs01 <- read_xlsx(here("datasets para procesamiento","labels obs01 long infraestructura.xlsx"))

labels_char_obs01 <- df_labels_obs01$name_char[!is.na(df_labels_obs01$name_char)]

labels_double_obs01 <- df_labels_obs01$name_double[!is.na(df_labels_obs01$name_double)]

vars_obs01_infra_long <- c(obs01_demograficos,
                           labels_char_obs01,
                           labels_double_obs01)

# select variables 

obs01_infra_long1 <- obs01 |> 
  select(all_of(c(obs01_demograficos,labels_char_obs01))) |> 
  pivot_longer(all_of(labels_char_obs01), names_to = "variables1",
               values_to = "valores1") 

df_labels_obs01_1 <- df_labels_obs01 |> 
  rename(variables1 = name_char) |> 
  select(-name_double)

obs01_infra_long1 <- obs01_infra_long1 |> 
  left_join(df_labels_obs01_1,by = "variables1")

obs01_infra_long2 <- obs01 |> 
  select(all_of(c(obs01_demograficos,labels_double_obs01))) |> 
  pivot_longer(all_of(labels_double_obs01), names_to = "variables2",
               values_to = "valores2")

df_labels_obs01_2 <- df_labels_obs01 |> 
  rename(variables2 = name_double) |> 
  select(-name_char)

obs01_infra_long2 <- obs01_infra_long2 |> 
  left_join(df_labels_obs01_2,by = "variables2")


obs01_infra_long <- bind_rows(obs01_infra_long1,
                              obs01_infra_long2)


# recreos y refacciones ----

# labels 

df_labels_obs01 <- read_xlsx(here("datasets para procesamiento/labels obs01 long recreos y refacciones.xlsx"))

labels_char_obs01 <- df_labels_obs01$name_char[!is.na(df_labels_obs01$name_char)]

labels_double_obs01 <- df_labels_obs01$name_double[!is.na(df_labels_obs01$name_double)]

vars_obs01_recref_long <- c(obs01_demograficos,
                           labels_char_obs01,
                           labels_double_obs01)

# select variables 

obs01_recref_long1 <- obs01 |> 
  select(all_of(c(obs01_demograficos,labels_char_obs01))) |> 
  pivot_longer(all_of(labels_char_obs01), names_to = "variables1",
               values_to = "valores1")

df_labels_obs01_1 <- df_labels_obs01 |> 
  rename(variables1 = name_char) |> 
  select(-name_double)

obs01_refrec_long1 <- obs01_recref_long1 |> 
  left_join(df_labels_obs01_1,by = "variables1")

obs01_refrec_long2 <- obs01 |> 
  select(all_of(c(obs01_demograficos,labels_double_obs01))) |> 
  pivot_longer(all_of(labels_double_obs01), names_to = "variables2",
               values_to = "valores2")

df_labels_obs01_2 <- df_labels_obs01 |> 
  rename(variables2 = name_double) |> 
  select(-name_char)

obs01_refrec_long2 <- obs01_refrec_long2 |> 
  left_join(df_labels_obs01_2,by = "variables2")


obs01_refrec_long <- bind_rows(obs01_refrec_long1,
                              obs01_refrec_long2)

# nee ----

# labels 

df_labels_obs01 <- read_xlsx(here("datasets para procesamiento/labels obs01 long nee.xlsx"))

labels_char_obs01 <- df_labels_obs01$name_char[!is.na(df_labels_obs01$name_char)]

labels_double_obs01 <- df_labels_obs01$name_double[!is.na(df_labels_obs01$name_double)]

vars_obs01_nee_long <- c(obs01_demograficos,
                            labels_char_obs01,
                            labels_double_obs01)

# select variables 

obs01_nee_long1 <- obs01 |> 
  select(all_of(c(obs01_demograficos,labels_char_obs01))) |> 
  pivot_longer(all_of(labels_char_obs01), names_to = "variables1",
               values_to = "valores1") 

df_labels_obs01_1 <- df_labels_obs01 |> 
  rename(variables1 = name_char) |> 
  select(-name_double)

obs01_nee_long1 <- obs01_nee_long1 |> 
  left_join(df_labels_obs01_1,by = "variables1")

obs01_nee_long2 <- obs01 |> 
  select(all_of(c(obs01_demograficos,labels_double_obs01))) |> 
  pivot_longer(all_of(labels_double_obs01), names_to = "variables2",
               values_to = "valores2")

df_labels_obs01_2 <- df_labels_obs01 |> 
  rename(variables2 = name_double) |> 
  select(-name_char)

obs01_nee_long2 <- obs01_nee_long2 |> 
  left_join(df_labels_obs01_2,by = "variables2")


obs01_nee_long <- bind_rows(obs01_nee_long1,
                               obs01_nee_long2)

# aula ----

# labels 

df_labels_obs01 <- read_xlsx(here("datasets para procesamiento/labels obs01 long aula.xlsx"))

labels_char_obs01 <- df_labels_obs01$name_char[!is.na(df_labels_obs01$name_char)]

vars_obs01_aula_long <- c(obs01_demograficos,
                             labels_char_obs01)

# select variables 

obs01_aula_long1 <- obs01 |> 
  select(all_of(c(obs01_demograficos,labels_char_obs01))) |> 
  pivot_longer(all_of(labels_char_obs01), names_to = "variables1",
               values_to = "valores1") 

df_labels_obs01_1 <- df_labels_obs01 |> 
  rename(variables1 = name_char)

obs01_aula_long <- obs01_aula_long1 |> 
  left_join(df_labels_obs01_1,by = "variables1")

# otros profesionales ----

# labels 

df_labels_obs01 <- read_xlsx(here("datasets para procesamiento/labels obs01 long otros profesionales.xlsx"))

labels_char_obs01 <- df_labels_obs01$name_char[!is.na(df_labels_obs01$name_char)]

vars_obs01_otros_pro_long <- c(obs01_demograficos,
                             labels_char_obs01)

# select variables 

obs01_otros_pro_long1 <- obs01 |> 
  select(all_of(c(obs01_demograficos,labels_char_obs01))) |> 
  pivot_longer(all_of(labels_char_obs01), names_to = "variables1",
               values_to = "valores1") 

df_labels_obs01_1 <- df_labels_obs01 |> 
  rename(variables1 = name_char)

obs01_otros_pro_long <- obs01_otros_pro_long1 |> 
  left_join(df_labels_obs01_1,by = "variables1")




# exports ----

## directo a listos para análisis

write_xlsx(obs01_demograficos_long, here("Datos/Listos para análisis/Observacion a nivel de centros/obs01_demograficos_long.xlsx"))

