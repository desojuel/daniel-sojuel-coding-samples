val_cleaned <- read_xlsx(here("datasets para procesamiento/limpieza de obs01.xlsx"))

# otros_recursos_contenido_botiquin----

## filtrar la variable que se limpiará

var1_cleaned <- val_cleaned |> 
  filter(nombre == "otros_recursos_contenido_botiquin") |> 
  select(-nombre)

## comprobar el proceso de limpieza 

valores_otros_recursos_contenido_botiquin <- obs01 |> 
  filter(!(is.na(otros_recursos_contenido_botiquin))) |> 
  select(contenido_botiquin, otros_recursos_contenido_botiquin)
 
## solo para ver los valores de las variables originales

# select_multiple_puntocoma(obs01, contenido_botiquin, "Var original")


## Join a partir de variable sucia como llave

obs01 <- obs01 |> 
  rename(valor = otros_recursos_contenido_botiquin) |> 
  left_join(var1_cleaned, by = "valor") |> 
  rename(otros_recursos_contenido_botiquin = categoria) |> 
  select(-valor) %>% 
  relocate(otros_recursos_contenido_botiquin, .after = contenido_botiquin)

## Unir al Otro

obs01 <- obs01 |> 
  mutate(contenido_botiquin = str_replace_all(contenido_botiquin, "Otros", otros_recursos_contenido_botiquin))

# laboratorio_otro_tipo_software_computadoras----

var2_cleaned <- val_cleaned |> 
  filter(nombre == "laboratorio_otro_tipo_software_computadoras") |> 
  select(-nombre)

## comprobar el proceso de limpieza 

valores_laboratorio_otro_tipo_software_computadoras <- obs01 |> 
  filter(!(is.na(laboratorio_otro_tipo_software_computadoras))) |> 
  select(laboratorio_software_computadoras, laboratorio_otro_tipo_software_computadoras)

## solo para ver los valores de las variables originales

# select_multiple_puntocoma(obs01, laboratorio_software_computadoras, "Var original")


## Join a partir de variable sucia como llave

obs01 <- obs01 |> 
  rename(valor = laboratorio_otro_tipo_software_computadoras) |> 
  left_join(var2_cleaned, by = "valor") |> 
  rename(laboratorio_otro_tipo_software_computadoras = categoria) |> 
  select(-valor) %>% 
  relocate(laboratorio_otro_tipo_software_computadoras, .after = laboratorio_software_computadoras)

## Unir al Otro

obs01 <- obs01 |> 
  mutate(laboratorio_software_computadoras = str_replace_all(laboratorio_software_computadoras, "Otros", laboratorio_otro_tipo_software_computadoras))

#educacion_fisica_otro_espacio----
var3_cleaned <- val_cleaned |> 
  filter(nombre == "educacion_fisica_otro_espacio") |> 
  select(-nombre)

valores_educacion_fisica_otro_espacio <- obs01 |> 
  filter(!(is.na(educacion_fisica_otro_espacio))) |> 
  select(educacion_fisica_cuales_espacio, educacion_fisica_otro_espacio)

# select_multiple_puntocoma(obs01, educacion_fisica_cuales_espacio, "Var original")


obs01 <- obs01 |> 
  rename(valor = educacion_fisica_otro_espacio) |> 
  left_join(var3_cleaned, by = "valor") |> 
  rename(educacion_fisica_otro_espacio = categoria) |> 
  select(-valor) %>% 
  relocate(educacion_fisica_otro_espacio, .after = educacion_fisica_cuales_espacio)

obs01 <- obs01 |> 
  mutate(educacion_fisica_cuales_espacio = str_replace_all(educacion_fisica_cuales_espacio, "Otros", educacion_fisica_otro_espacio))
#educacion_fisica_otro_espacio_fuera_centro----
var4_cleaned <- val_cleaned |> 
  filter(nombre == "educacion_fisica_otro_espacio_fuera_centro") |> 
  select(-nombre)

valores_educacion_fisica_otro_espacio <- obs01 |> 
  filter(!(is.na(educacion_fisica_otro_espacio_fuera_centro))) |> 
  select(educacion_fisica_espacio_fuera_centro, educacion_fisica_otro_espacio_fuera_centro)

# select_multiple_puntocoma(obs01, educacion_fisica_espacio_fuera_centro, "Var original")


obs01 <- obs01 |> 
  rename(valor = educacion_fisica_otro_espacio_fuera_centro) |> 
  left_join(var4_cleaned, by = "valor") |> 
  rename(educacion_fisica_otro_espacio_fuera_centro = categoria) |> 
  select(-valor) %>% 
  relocate(educacion_fisica_otro_espacio_fuera_centro, .after = educacion_fisica_espacio_fuera_centro)

obs01 <- obs01 |> 
  mutate(educacion_fisica_espacio_fuera_centro = str_replace_all(educacion_fisica_espacio_fuera_centro, "Otros", educacion_fisica_otro_espacio_fuera_centro))
#educacion_fisica_otra_propiedad_espacio_fuera_centro----
var5_cleaned <- val_cleaned |> 
  filter(nombre == "educacion_fisica_otra_propiedad_espacio_fuera_centro") |> 
  select(-nombre)

valores_educacion_fisica_otra_propiedad_espacio_fuera_centro <- obs01 |> 
  filter(!(is.na(educacion_fisica_otra_propiedad_espacio_fuera_centro))) |> 
  select(educacion_fisica_propiedad_espacio_fuera_centro, educacion_fisica_otra_propiedad_espacio_fuera_centro)

# select_multiple_puntocoma(obs01, educacion_fisica_propiedad_espacio_fuera_centro, "Var original")


obs01 <- obs01 |> 
  rename(valor = educacion_fisica_otra_propiedad_espacio_fuera_centro) |> 
  left_join(var5_cleaned, by = "valor") |> 
  rename(educacion_fisica_otra_propiedad_espacio_fuera_centro = categoria) |> 
  select(-valor) %>% 
  relocate(educacion_fisica_otra_propiedad_espacio_fuera_centro, .after = educacion_fisica_propiedad_espacio_fuera_centro)

obs01 <- obs01 |> 
  mutate(educacion_fisica_propiedad_espacio_fuera_centro = str_replace_all(educacion_fisica_propiedad_espacio_fuera_centro, "Otros", educacion_fisica_otro_espacio_fuera_centro))
#otros_instrumentos_musicales----
var6_cleaned <- val_cleaned |> 
  filter(nombre == "otros_instrumentos_musicales") |> 
  select(-nombre)

valores_otros_instrumentos_musicales <- obs01 |> 
  filter(!(is.na(otros_instrumentos_musicales))) |> 
  select(cuales_instrumentos_musicales, otros_instrumentos_musicales)

# select_multiple_puntocoma(obs01, cuales_instrumentos_musicales, "Var original")


obs01 <- obs01 |> 
  rename(valor = otros_instrumentos_musicales) |> 
  left_join(var6_cleaned, by = "valor") |> 
  rename(otros_instrumentos_musicales = categoria) |> 
  select(-valor) %>% 
  relocate(otros_instrumentos_musicales, .after = cuales_instrumentos_musicales)

obs01 <- obs01 |> 
  mutate(cuales_instrumentos_musicales = str_replace_all(cuales_instrumentos_musicales, "Otros", otros_instrumentos_musicales)) 
#otros_recursos_salon_artes_visuales----
var7_cleaned <- val_cleaned |> 
  filter(nombre == "otros_recursos_salon_artes_visuales") |> 
  select(-nombre)

valores_otros_recursos_salon_artes_visuales <- obs01 |> 
  filter(!(is.na(otros_recursos_salon_artes_visuales))) |> 
  select(recursos_salon_artes_visuales, otros_recursos_salon_artes_visuales)

# select_multiple_puntocoma(obs01, recursos_salon_artes_visuales, "Var original")


obs01 <- obs01 |> 
  rename(valor = otros_recursos_salon_artes_visuales) |> 
  left_join(var7_cleaned, by = "valor") |> 
  rename(otros_recursos_salon_artes_visuales = categoria) |> 
  select(-valor) %>% 
  relocate(otros_recursos_salon_artes_visuales, .after = recursos_salon_artes_visuales)

obs01 <- obs01 |> 
  mutate(recursos_salon_artes_visuales = str_replace_all(recursos_salon_artes_visuales, "Otros", otros_recursos_salon_artes_visuales)) 

#laboratorio_ciencias_otros_materiales----

var8_cleaned <- val_cleaned |> 
  filter(nombre == "laboratorio_ciencias_otros_materiales") |> 
  select(-nombre)

valores_laboratorio_ciencias_otros_materiales <- obs01 |> 
  filter(!(is.na(laboratorio_ciencias_otros_materiales))) |> 
  select(laboratorio_ciencias_recursos, laboratorio_ciencias_otros_materiales)

# select_multiple_puntocoma(obs01, laboratorio_ciencias_recursos, "Var original")


obs01 <- obs01 |> 
  rename(valor = laboratorio_ciencias_otros_materiales) |> 
  left_join(var8_cleaned, by = "valor") |> 
  rename(laboratorio_ciencias_otros_materiales = categoria) |> 
  select(-valor) %>% 
  relocate(laboratorio_ciencias_otros_materiales, .after = laboratorio_ciencias_recursos)

obs01 <- obs01 |> 
  mutate(laboratorio_ciencias_recursos = str_replace_all(laboratorio_ciencias_recursos, "Otros", laboratorio_ciencias_otros_materiales)) 

#otro_uso_timbre_o_campana----

var9_cleaned <- val_cleaned |> 
  filter(nombre == "otro_uso_timbre_o_campana") |> 
  select(-nombre)

valores_otro_uso_timbre_o_campana <- obs01 |> 
  filter(!(is.na(otro_uso_timbre_o_campana))) |> 
  select(uso_timbre_o_campana, otro_uso_timbre_o_campana)

# select_multiple_puntocoma(obs01, uso_timbre_o_campana, "Var original")


obs01 <- obs01 |> 
  rename(valor = otro_uso_timbre_o_campana) |> 
  left_join(var9_cleaned, by = "valor") |> 
  rename(otro_uso_timbre_o_campana = categoria) |> 
  select(-valor) %>% 
  relocate(otro_uso_timbre_o_campana, .after = uso_timbre_o_campana)

obs01 <- obs01 |> 
  mutate(uso_timbre_o_campana = str_replace_all(uso_timbre_o_campana, "Otros", otro_uso_timbre_o_campana)) 

#grupo_recreos_quien----
var10_cleaned <- val_cleaned |> 
  filter(nombre == "grupo_recreos_quien") |> 
  select(-nombre)

valores_grupo_recreos_quien <- obs01 |> 
  filter(!(is.na(grupo_recreos_quien))) |> 
  select(grupo_recreos, grupo_recreos_quien)

# select_multiple_puntocoma(obs01, grupo_recreos, "Var original")


obs01 <- obs01 |> 
  rename(valor = grupo_recreos_quien) |> 
  left_join(var10_cleaned, by = "valor") |> 
  rename(grupo_recreos_quien = categoria) |> 
  select(-valor) %>% 
  relocate(grupo_recreos_quien, .after = grupo_recreos)

obs01 <- obs01 |> 
  mutate(grupo_recreos = str_replace_all(grupo_recreos, "Otros", grupo_recreos_quien)) 

#otro_tipo_remozamiento----
var11_cleaned <- val_cleaned |> 
  filter(nombre == "otro_tipo_remozamiento") |> 
  select(-nombre)

valores_grupo_recreos_quien <- obs01 |> 
  filter(!(is.na(otro_tipo_remozamiento))) |> 
  select(tipo_remozamiento, otro_tipo_remozamiento)

# select_multiple_puntocoma(obs01, tipo_remozamiento, "Var original")


obs01 <- obs01 |> 
  rename(valor = otro_tipo_remozamiento) |> 
  left_join(var11_cleaned, by = "valor") |> 
  rename(otro_tipo_remozamiento = categoria) |> 
  select(-valor) %>% 
  relocate(otro_tipo_remozamiento, .after = tipo_remozamiento)

obs01 <- obs01 |> 
  mutate(tipo_remozamiento = str_replace_all(tipo_remozamiento, "Otros", otro_tipo_remozamiento)) 

#otra_duracion_periodo_clase----
var12_cleaned <- val_cleaned |> 
  filter(nombre == "otra_duracion_periodo_clase") |> 
  select(-nombre)

valores_duracion_periodo_clase <- obs01 |> 
  filter(!(is.na(otra_duracion_periodo_clase))) |> 
  select(duracion_periodo_clase, otra_duracion_periodo_clase)

# select_multiple_puntocoma(obs01, duracion_periodo_clase, "Var original")


obs01 <- obs01 |> 
  rename(valor = otra_duracion_periodo_clase) |> 
  left_join(var12_cleaned, by = "valor") |> 
  rename(otra_duracion_periodo_clase = categoria) |> 
  select(-valor) %>% 
  relocate(otra_duracion_periodo_clase, .after = duracion_periodo_clase)

obs01 <- obs01 |> 
  mutate(duracion_periodo_clase = str_replace_all(duracion_periodo_clase, "Otros", otra_duracion_periodo_clase))

#otro_idiomas_estudiantes----
var13_cleaned <- val_cleaned |> 
  filter(nombre == "otro_idiomas_estudiantes") |> 
  select(-nombre)

valores_duracion_periodo_clase <- obs01 |> 
  filter(!(is.na(otro_idiomas_estudiantes))) |> 
  select(idiomas_estudiantes, otro_idiomas_estudiantes)

# select_multiple_puntocoma(obs01, idiomas_estudiantes, "Var original")


obs01 <- obs01 |> 
  rename(valor = otro_idiomas_estudiantes) |> 
  left_join(var13_cleaned, by = "valor") |> 
  rename(otro_idiomas_estudiantes = categoria) |> 
  select(-valor) %>% 
  relocate(otro_idiomas_estudiantes, .after = idiomas_estudiantes)

obs01 <- obs01 |> 
  mutate(idiomas_estudiantes = str_replace_all(idiomas_estudiantes, "Otros", otro_idiomas_estudiantes))
#otro_tipos_mobiliario----
var14_cleaned <- val_cleaned |> 
  filter(nombre == "otro_tipos_mobiliario") |> 
  select(-nombre)

valores_otro_tipos_mobiliario <- obs01 |> 
  filter(!(is.na(otro_tipos_mobiliario))) |> 
  select(tipos_mobiliario, otro_tipos_mobiliario)

# select_multiple_puntocoma(obs01, tipos_mobiliario, "Var original")


obs01 <- obs01 |> 
  rename(valor = otro_tipos_mobiliario) |> 
  left_join(var14_cleaned, by = "valor") |> 
  rename(otro_tipos_mobiliario = categoria) |> 
  select(-valor) %>% 
  relocate(otro_tipos_mobiliario, .after = tipos_mobiliario)

obs01 <- obs01 |> 
  mutate(tipos_mobiliario = str_replace_all(tipos_mobiliario, "Otros", otro_tipos_mobiliario))

#otros_riesgos----
var15_cleaned <- val_cleaned |> 
  filter(nombre == "otros_riesgos") |> 
  select(-nombre)

valores_otros_riesgos <- obs01 |> 
  filter(!(is.na(otros_riesgos))) |> 
  select(posibles_riesgos, otros_riesgos)

# select_multiple_puntocoma(obs01, posibles_riesgos, "Var original")


obs01 <- obs01 |> 
  rename(valor = otros_riesgos) |> 
  left_join(var15_cleaned, by = "valor") |> 
  rename(otros_riesgos = categoria) |> 
  select(-valor) %>% 
  relocate(otros_riesgos, .after = posibles_riesgos)

obs01 <- obs01 |> 
  mutate(posibles_riesgos = str_replace_all(posibles_riesgos, "Otros", otros_riesgos))

#otros_recursos_tecnologicos----
var16_cleaned <- val_cleaned |> 
  filter(nombre == "otros_recursos_tecnologicos") |> 
  select(-nombre)

valores_otros_recursos_tecnologicos <- obs01 |> 
  filter(!(is.na(otros_recursos_tecnologicos))) |> 
  select(recursos_tecnologicos, otros_recursos_tecnologicos)

# select_multiple_puntocoma(obs01, recursos_tecnologicos, "Var original")

obs01 <- obs01 |> 
  rename(valor = otros_recursos_tecnologicos) |> 
  left_join(var16_cleaned, by = "valor") |> 
  rename(otros_recursos_tecnologicos = categoria) |> 
  select(-valor) %>% 
  relocate(otros_recursos_tecnologicos, .after = recursos_tecnologicos)

obs01 <- obs01 |> 
  mutate(recursos_tecnologicos = str_replace_all(recursos_tecnologicos, "Otros", otros_recursos_tecnologicos))


# unir ----


