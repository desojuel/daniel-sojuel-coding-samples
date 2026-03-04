obs02 <- read_xlsx(here("Datos","Observación aulas", "obs02.xlsx"))

# -------------------------------------------------------- 
# Hay que correr la función para arreglar separadores en la carpeta de funciones para que funcione este código 
# -------------------------------------------------------- 

# planificaciones_formato_docente ----

valores_planificaciones_formato_docente <- obs02 |> 
  filter(!(is.na(planificaciones_formato_docente))) |> 
  select(planificaciones_formato_docente)


labels_planificaciones_formato_docente <- c(
  "Impresas", "Digitales", "En cuaderno", "Otro formato"
)

obs02 <- obs02 |>
  mutate(
    planificaciones_formato_docente = map_chr(planificaciones_formato_docente,
                                 fix_sources,
                                 labels = labels_planificaciones_formato_docente)
  )

# planificaciones_elementos----
valores_planificaciones_elementos <- obs02 |> 
  filter(!(is.na(planificaciones_elementos))) |> 
  select(planificaciones_elementos)


labels_planificaciones_elementos <- c(
  "Competencias", "Indicadores de logro", "Contenidos", 
  "Actividades de aprendizaje", "Evaluación (o actividades de evaluación)",
"Recursos", "Metodología o enfoque para la instrucción", 
"Secuencia de clase (por ejemplo, inicio, desarrollo y final)", 
"Conocimientos previos", "Otro"
)

obs02 <- obs02 |>
  mutate(
    planificaciones_elementos = map_chr(planificaciones_elementos,
                                              fix_sources,
                                              labels = labels_planificaciones_elementos)
  )

#competencia_segun_planificacion----
valores_competencia_segun_planificacion <- obs02 |> 
  filter(!(is.na(competencia_segun_planificacion))) |> 
  select(competencia_segun_planificacion)


labels_competencia_segun_planificacion <- c(
  "Sí", "No", "No aplica (las planificaciones no cuentan con competencias)",
"Aún no se han recibido, fotografiado o visto las planificaciones del docente"
)

obs02 <- obs02 |>
  mutate(
    competencia_segun_planificacion = map_chr(competencia_segun_planificacion,
                                        fix_sources,
                                        labels = labels_competencia_segun_planificacion)
  )