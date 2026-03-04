# Merge separators con longs ---- 

## salones ----

### pivot longer y separate ----

separators_salones <- read_xlsx(here("datasets para procesamiento/labels obs01 long salones separators.xlsx"))

obs01_salones_separators <- separa_long_tables(obs01,
                                               separators_salones)

### rbind ----
 
obs01_salones_long_completo <- bind_rows(obs01_salones_long,
                                   obs01_salones_separators)


## infraestructura ---- 

### pivot longer y separate ----

separators_infra <- read_xlsx(here("datasets para procesamiento/labels obs01 long infraestructura separators.xlsx")) |> 
  select(-name_double)

obs01_separators <- separa_long_tables(obs01,
                                       separators_infra)

### rbind ----

obs01_infra_long_completo <- bind_rows(obs01_infra_long,
                                   obs01_separators)

## recreos y refacciones ---- 

### pivot longer y separate ----

separators_recrefa <- read_xlsx(here("datasets para procesamiento/labels obs01 long recreos y refacciones separators.xlsx"))

obs01_recrefa_separators <- separa_long_tables(obs01,
                                               separators_recrefa)

### rbind ----

obs01_recrefa_long_completo <- bind_rows(obs01_refrec_long,
                                     obs01_recrefa_separators)


## nee ---- 

### pivot longer y separate ----

separators_nee <- read_xlsx(here("datasets para procesamiento/labels obs01 long nee separators.xlsx"))

obs01_nee_separators <- separa_long_tables(obs01,
                                               separators_nee)

### rbind ----

obs01_nee_completo <- bind_rows(obs01_nee_long,
                            obs01_nee_separators)

## idiomas ---- 

### pivot longer y separate ----

separators_idiomas <- read_xlsx(here("datasets para procesamiento/labels obs01 long idiomas.xlsx"))

obs01_idiomas_separators <- separa_long_tables(obs01,
                                           separators_idiomas) |> 
  select(-c("variables2","valores2"))

obs01_idiomas <- obs01 |> 
  select(index, idiomas_estudiantes)

obs01_idiomas_separators <- obs01_idiomas_separators |> 
  left_join(obs01_idiomas, by = "index")

## aula ---- 

### pivot longer y separate ----

separators_aula <- read_xlsx(here("datasets para procesamiento/labels obs01 long aula separators.xlsx"))

obs01_aula_separators <- separa_long_tables(obs01,
                                            separators_aula) |> 
  select(-c("variables2","valores2"))

### rbind ----

obs01_aula_completo <- bind_rows(obs01_aula_long,
                            obs01_aula_separators)


# exports ----

write_xlsx(obs01_infra_long_completo, here("Datos/Listos para análisis/Observacion a nivel de centros/obs01_infra_full.xlsx"))

write_xlsx(obs01_recrefa_long_completo, here("Datos/Listos para análisis/Observacion a nivel de centros/obs01_recrefa_full.xlsx"))

write_xlsx(obs01_nee_completo, here("Datos/Listos para análisis/Observacion a nivel de centros/obs01_nee_full.xlsx"))

write_xlsx(obs01_idiomas_separators, here("Datos/Listos para análisis/Observacion a nivel de centros/obs01_idiomas_full.xlsx"))

write_xlsx(obs01_aula_completo, here("Datos/Listos para análisis/Observacion a nivel de centros/obs01_aula_full.xlsx"))