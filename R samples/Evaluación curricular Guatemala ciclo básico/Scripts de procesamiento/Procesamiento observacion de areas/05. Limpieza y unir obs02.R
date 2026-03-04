obs02 <- read_xlsx(here("Datos","Observación aulas", "obs02.xlsx"))

val_cleaned <- read_xlsx(here("datasets para procesamiento/limpieza de obs02.xlsx"))

#planificaciones_otro_formato----
var1_cleaned <- val_cleaned |> 
  filter(nombre == "planificaciones_otro_formato") |> 
  select(-nombre)

valores_educacion_fisica_otro_espacio <- obs02 |> 
  filter(!(is.na(planificaciones_otro_formato))) |> 
  select(planificaciones_formato_docente, planificaciones_otro_formato)

select_multiple_puntocoma(obs02, planificaciones_formato_docente, "Var original")


obs02 <- obs02 |> 
  rename(valor = planificaciones_otro_formato) |> 
  left_join(var1_cleaned, by = "valor") |> 
  rename(planificaciones_otro_formato = categoria) |> 
  select(-valor) %>% 
  relocate(planificaciones_otro_formato, .after = planificaciones_formato_docente)

obs02 <- obs02 |> 
  mutate(planificaciones_formato_docente = str_replace_all(planificaciones_formato_docente, "Otros", planificaciones_otro_formato))

#planificaciones_otros_elementos----
var2_cleaned <- val_cleaned |> 
  filter(nombre == "planificaciones_otros_elementos") |> 
  select(-nombre)

valores_planificaciones_otros_elementos <- obs02 |> 
  filter(!(is.na(planificaciones_otros_elementos))) |> 
  select(planificaciones_elementos, planificaciones_otros_elementos)

select_multiple_puntocoma(obs02, planificaciones_elementos, "Var original")


obs02 <- obs02 |> 
  rename(valor = planificaciones_otros_elementos) |> 
  left_join(var2_cleaned, by = "valor") |> 
  rename(planificaciones_otros_elementos = categoria) |> 
  select(-valor) %>% 
  relocate(planificaciones_otros_elementos, .after = planificaciones_elementos)

obs02 <- obs02 |> 
  mutate(planificaciones_elementos = str_replace_all(planificaciones_elementos, "Otros", planificaciones_otros_elementos))

#otro_espacio_desarrollo_clase----
var3_cleaned <- val_cleaned |> 
  filter(nombre == "otro_espacio_desarrollo_clase") |> 
  select(-nombre)

valores_otro_espacio_desarrollo_clase <- obs02 |> 
  filter(!(is.na(otro_espacio_desarrollo_clase))) |> 
  select(espacio_desarrollo_clase, otro_espacio_desarrollo_clase)

select_multiple_puntocoma(obs02, espacio_desarrollo_clase, "Var original")


obs02 <- obs02 |> 
  rename(valor = otro_espacio_desarrollo_clase) |> 
  left_join(var3_cleaned, by = "valor") |> 
  rename(otro_espacio_desarrollo_clase = categoria) |> 
  select(-valor) %>% 
  relocate(otro_espacio_desarrollo_clase, .after = espacio_desarrollo_clase)

obs02 <- obs02 |> 
  mutate(espacio_desarrollo_clase = str_replace_all(espacio_desarrollo_clase, "Otros", otro_espacio_desarrollo_clase))

#Para encontrar perdidos----
#val_cleaned %>%
  #group_by(valor) %>%
  #filter(n() > 1) %>%
  #arrange(valor)