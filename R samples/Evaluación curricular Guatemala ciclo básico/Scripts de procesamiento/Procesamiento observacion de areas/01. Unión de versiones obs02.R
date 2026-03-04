obs02 <- read_xlsx(here("Datos","Observación aulas","observacion periodos 25.11.25 con versiones.xlsx"))

# limpiar por fecha
obs02 <- obs02[obs02$start >= as.POSIXct("2025-08-04 00:00:00"), ]

obs02 <- clean_names(obs02)

# unir variables duplicadas por las versiones

obs02 <- obs02 %>% 
  mutate(x35_se_logro_tomar_fotografias_de_las_planificaciones_durante_esta_observacion_39 = coalesce(
    x35_se_logro_tomar_fotografias_de_las_planificaciones_durante_esta_observacion_39, 
    x35_se_logro_tomar_fotografias_de_las_planificaciones_durante_esta_observacion_154))

# seleccionar las variables oficiales de obs02

rango_vars <- c(match("start",colnames(obs02)):match("x87_hubo_algun_incidente_durante_la_observacion_de_esta_area",colnames(obs02)),
                                     match("id",colnames(obs02)):match("index",colnames(obs02)))

obs02 <- obs02 |> 
  select(all_of(rango_vars))


# leer archivo colnames con nombres de variables oficiales y asignarlos

cols_obs02 <- read_xlsx(
  here("datasets para procesamiento", "colnames evaluacion curricular basico.xlsx"),
  col_names = T
)[[2]] |> 
  na.omit() |> 
  as.vector()

colnames(obs02) <- cols_obs02

# exportar obs02 para la siguiente fase de limpieza

write_xlsx(obs02, here("Datos","Observación aulas","obs02.xlsx"))