df_est <- read_xlsx(here("Datos","Encuesta estudiantes","encuesta estudiantes 18.11.25 con versiones.xlsx"))

# limpiar por fecha
df_est <- df_est[df_est$start >= as.POSIXct("2025-08-04 00:00:00"), ]

df_est <- clean_names(df_est)


# unir variables duplicadas por las versiones

df_est <- df_est %>% 
  mutate(explicar_el_significado_de_las_palabras_tomando_en_cuenta_el_contexto_468 = coalesce(
    explicar_el_significado_de_las_palabras_tomando_en_cuenta_el_contexto_468, 
    explicar_el_significado_de_las_palabras_tomando_en_cuenta_el_contexto_1067),
    x64_cual_es_el_motivo_por_el_que_se_interrumpio_la_encuesta = coalesce(
      x64_cual_es_el_motivo_por_el_que_se_interrumpio_la_encuesta, 
      x63_cual_es_el_motivo_por_el_que_se_interrumpio_la_encuesta), 
    x65_observaciones_generales_de_la_encuesta = coalesce(
      x65_observaciones_generales_de_la_encuesta, observaciones_generales_de_la_encuesta))


# seleccionar las variables oficiales de df_est

rango_vars <- c(match("start",colnames(df_est)):match("x65_observaciones_generales_de_la_encuesta",colnames(df_est)),
                match("id",colnames(df_est)):match("index",colnames(df_est)))

df_est <- df_est |> 
  select(all_of(rango_vars))


# leer archivo colnames con nombres de variables oficiales y asignarlos

cols_df_est <- read_xlsx(
  here("datasets para procesamiento", "colnames evaluacion curricular basico.xlsx"),
  col_names = T
)[[3]] |> 
  na.omit() |> 
  as.vector()

colnames(df_est) <- cols_df_est

# exportar obs02 para la siguiente fase de limpieza

write_xlsx(df_est, here("Datos","Encuesta estudiantes","df_est.xlsx"))


