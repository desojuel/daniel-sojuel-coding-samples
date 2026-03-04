obs02 <- obs02_05

# número de observación ----

obs02 <- obs02 |> 
  group_by(codigo, area) |> 
  mutate(numero_observacion = seq_len(n())) |> 
  ungroup()

"index",
"codigo",
"nombre",
"departamento",
"plan",
"jornada",
"poblacion",
"sector",
"modalidad",
"grado",
"seccion",
"area",
"educacion_artistica",
"orientacion_emprendimiento",
"orientacion_emprendimiento",
"orientacion_industrial",
"orientacion_economia",
"orientacion_comercial"
"fecha",
"inicio_clase",
"fin_clase",
"cantidad_areas",
"incidente_despues"



