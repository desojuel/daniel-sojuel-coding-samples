# -------------------------------------------------------- 
# Hay que correr la función para arreglar separadores en la carpeta de funciones para que funcione este código 
# -------------------------------------------------------- 

# fuente_agua_centro ----

valores_fuente_agua_centro <- obs01 |> 
  filter(!(is.na(fuente_agua_centro))) |> 
  select(fuente_agua_centro)


labels_fuente_agua_centro <- c(
  "Transporte externo (camión cisterna, recipientes, etc.)",
  "Recolección directa (de río, lago, lluvia, etc., sin tubería ni sistema de almacenamiento)",
  "Tubería (conectada a red pública, pozo, río, lago, etc.)",
  "Pozo con extracción manual o mecánica (no entubado)",
  "Otra fuente"
)

obs01 <- obs01 |>
  mutate(
    fuente_agua_centro = map_chr(fuente_agua_centro,
                                 fix_sources,
                                 labels = labels_fuente_agua_centro)
  )

# banos_hombres_recursos----
valores_banos_hombres_recursos <- obs01 |> 
  filter(!(is.na(banos_hombres_recursos))) |> 
  select(banos_hombres_recursos)

labels_banos_hombres_recursos <- c(
  "Papel higiénico", "Dispensadores de toallas de papel","Secadores de manos", 
  "Basureros","Jabón"
)

obs01 <- obs01 |>
  mutate(
    banos_hombres_recursos = map_chr(banos_hombres_recursos,
                                 fix_sources,
                                 labels = labels_banos_hombres_recursos)
  )

#Banos_mujeres_recursos----
valores_banos_mujeres_recursos <- obs01 |> 
  filter(!(is.na(banos_mujeres_recursos))) |> 
  select(banos_mujeres_recursos)

labels_banos_mujeres_recursos <- c(
  "Papel higiénico", "Dispensadores de toallas de papel","Secadores de manos", 
  "Basureros","Jabón"
)

obs01 <- obs01 |>
  mutate(
    banos_mujeres_recursos = map_chr(banos_mujeres_recursos,
                                     fix_sources,
                                     labels = labels_banos_mujeres_recursos)
  )

#recursos_banos_mixtos----
valores_recursos_banos_mixtos <- obs01 |> 
  filter(!(is.na(recursos_banos_mixtos))) |> 
  select(recursos_banos_mixtos)

labels_recursos_banos_mixtos <- c(
  "Papel higiénico", "Dispensadores de toallas de papel","Secadores de manos", 
  "Basureros","Jabón"
)

obs01 <- obs01 |>
  mutate(
    recursos_banos_mixtos = map_chr(recursos_banos_mixtos,
                                     fix_sources,
                                     labels = labels_recursos_banos_mixtos)
  )

#contenido_botiquin----

valores_contenido_botiquin <- obs01 |> 
  filter(!(is.na(contenido_botiquin))) |> 
  select(contenido_botiquin)

labels_contenido_botiquin <- c(
  "Gasas", "Alcohol o alcohol en gel", "Antisépticos (cremas u otro)","Vendas",
  "Curitas", "Termómetro", "Tijeras", "Medicamentos básicos (analgésicos, antipiréticos, etc.)",  
   "Otros"
)

obs01 <- obs01 |>
  mutate(
    contenido_botiquin = map_chr(contenido_botiquin,
                                    fix_sources,
                                    labels = labels_contenido_botiquin)
  )

#laboratorio_adquisicion_computadoras----

valores_laboratorio_adquisicion_computadoras <- obs01 |> 
  filter(!(is.na(laboratorio_adquisicion_computadoras))) |> 
  select(laboratorio_adquisicion_computadoras)

labels_laboratorio_adquisicion_computadoras <- c(
  "Las compró el Centro Educativo", "Fueron recibidas de parte del Ministerio de Educación", 
  "Fueron donadas" 
)

obs01 <- obs01 |>
  mutate(
    laboratorio_adquisicion_computadoras = map_chr(laboratorio_adquisicion_computadoras,
                                 fix_sources,
                                 labels = labels_laboratorio_adquisicion_computadoras)
  )

#laboratorio_software_computadoras----

valores_laboratorio_software_computadoras <- obs01 |> 
  filter(!(is.na(laboratorio_software_computadoras))) |> 
  select(laboratorio_software_computadoras)

labels_laboratorio_software_computadoras <- c(
  "Sistema operativo (Windows, Linux, etc.)", "Programas de oficina (Word, Excel, PowerPoint, etc.)", 
  "Navegador de Internet", "Software educativo (juegos didácticos, simuladores, plataformas escolares, etc.)", 
  "Programas de diseño",  "Programas para editar videos","Otro" 
)

obs01 <- obs01 |>
  mutate(
    laboratorio_software_computadoras = map_chr(laboratorio_software_computadoras,
                                                   fix_sources,
                                                   labels = labels_laboratorio_software_computadoras)
  )

#educacion_fisica_cuales_espacio----
valores_educacion_fisica_cuales_espacio <- obs01 |> 
  filter(!(is.na(educacion_fisica_cuales_espacio))) |> 
  select(educacion_fisica_cuales_espacio)

labels_educacion_fisica_cuales_espacio <- c(
  "Gimnasio", "Patio","Cancha polideportiva", "Cancha de baloncesto", 
  "Cancha de papi fútbol", "Cancha de Voleibol", "Salón de usos múltiples",   
  "Espacio fuera del Centro Educativo", "Otro"
)

obs01 <- obs01 |>
  mutate(
    educacion_fisica_cuales_espacio = map_chr(educacion_fisica_cuales_espacio,
                                                fix_sources,
                                                labels = labels_educacion_fisica_cuales_espacio)
  )

#lenguajes_educacion_artistica----

valores_lenguajes_educacion_artistica <- obs01 |> 
  filter(!(is.na(lenguajes_educacion_artistica))) |> 
  select(lenguajes_educacion_artistica)

labels_lenguajes_educacion_artistica <- c(
  "Educación Musical", "Artes Visuales", "Teatro", "Danza",  
  "Se desarrolla Educación Artística de manera integrada "
)

obs01 <- obs01 |>
  mutate(
   lenguajes_educacion_artistica = map_chr(lenguajes_educacion_artistica,
                                              fix_sources,
                                              labels = labels_lenguajes_educacion_artistica)
  )

#cuales_instrumentos_musicales----

valores_cuales_instrumentos_musicales <- obs01 |> 
  filter(!(is.na(cuales_instrumentos_musicales))) |> 
  select(cuales_instrumentos_musicales)

labels_cuales_instrumentos_musicales <- c(
  "Marimba/s", "Teclado/s", "Guitarra/s", "Flauta/s", "Otros "
)

obs01 <- obs01 |>
  mutate(
    cuales_instrumentos_musicales = map_chr(cuales_instrumentos_musicales,
                                            fix_sources,
                                            labels = labels_cuales_instrumentos_musicales)
  )

#adquisicion_instrumentos_musicales----

valores_adquisicion_instrumentos_musicales <- obs01 |> 
  filter(!(is.na(adquisicion_instrumentos_musicales))) |> 
  select(adquisicion_instrumentos_musicales)

labels_adquisicion_instrumentos_musicales <- c(
  "Fueron comprados por el Centro Educativo", "Se recibieron de parte del Ministerio de Educación", 
  "Fueron donados"
)

obs01 <- obs01 |>
  mutate(
    adquisicion_instrumentos_musicales = map_chr(adquisicion_instrumentos_musicales,
                                            fix_sources,
                                            labels = labels_adquisicion_instrumentos_musicales)
  )

#recursos_salon_artes_visuales----

valores_recursos_salon_artes_visuales <- obs01 |> 
  filter(!(is.na(recursos_salon_artes_visuales))) |> 
  select(recursos_salon_artes_visuales)

labels_recursos_salon_artes_visuales <- c(
  "Mesas", "Sillas", "Caballetes", "Otro "
)

obs01 <- obs01 |>
  mutate(
    recursos_salon_artes_visuales = map_chr(recursos_salon_artes_visuales,
                                                 fix_sources,
                                                 labels = labels_recursos_salon_artes_visuales)
  )

#laboratorio_ciencias_recursos----

valores_laboratorio_ciencias_recursos <- obs01 |> 
  filter(!(is.na(laboratorio_ciencias_recursos))) |> 
  select(laboratorio_ciencias_recursos)

labels_laboratorio_ciencias_recursos <- c(
"Pizarrón", "Mesa de demonstración", "Computadoras", "Microscopios", "Modelos anatómicos (cuerpo humano, células)",   
"Termómetros", "Cristalería (por ejemplo, tubos de ensayo)", "Básculas e instrumentos de medición",
"Extintores", "Botiquín de primeros auxilios","Estaciones de lavado de manos","Otros" 
)

obs01 <- obs01 |>
  mutate(
    laboratorio_ciencias_recursos = map_chr(laboratorio_ciencias_recursos,
                                            fix_sources,
                                            labels = labels_laboratorio_ciencias_recursos)
  )

#espacio_emprendimiento_orientaciones----
valores_espacio_emprendimiento_orientaciones <- obs01 |> 
  filter(!(is.na(espacio_emprendimiento_orientaciones))) |> 
  select(espacio_emprendimiento_orientaciones)

labels_espacio_emprendimiento_orientaciones <- c(
  "Agrícola y Agropecuaria", "Orientación Industrial", "Economía Doméstica", 
  "Orientación Comercial"
)

obs01 <- obs01 |>
  mutate(
    espacio_emprendimiento_orientaciones = map_chr(espacio_emprendimiento_orientaciones,
                                            fix_sources,
                                            labels = labels_espacio_emprendimiento_orientaciones)
  )

#cargos_directivos_centro----

valores_cargos_directivos_centros <- obs01 |> 
  filter(!(is.na(cargos_directivos_centro))) |> 
  select(cargos_directivos_centro)

labels_cargos_directivos_centro <- c(
  "Director general", "Director de nivel", "Director técnico",
  "Director administrativo", "Otro" 
)

obs01 <- obs01 |>
  mutate(
    cargos_directivos_centro = map_chr(cargos_directivos_centro,
                                                   fix_sources,
                                                   labels = labels_cargos_directivos_centro)
  )

#uso_timbre_o_campana----

valores_uso_timbre_o_campana <- obs01 |> 
  filter(!(is.na(uso_timbre_o_campana))) |> 
  select(uso_timbre_o_campana)

labels_uso_timbre_o_campana <- c(
  "Horario de entrada", "Recreo", "Cambio de periodo", "Horario de salida",
  "Tiempo de refacción", "Otro" 
)

obs01 <- obs01 |>
  mutate(
    uso_timbre_o_campana = map_chr(uso_timbre_o_campana,
                                       fix_sources,
                                       labels = labels_uso_timbre_o_campana)
  )

#tipo_remozamiento----

valores_tipo_remozamiento <- obs01 |> 
  filter(!(is.na(tipo_remozamiento))) |> 
  select(tipo_remozamiento)

labels_tipo_remozamiento <- c(
  "Muros", "Cubierta de lámina e impermeabilización de losa", "Baños", "Piso", 
  "Puertas y ventanas", "Instalaciones de agua", "Drenajes", "Red eléctrica",
  "Pintura", "Otro"  
)

obs01 <- obs01 |>
  mutate(
    tipo_remozamiento = map_chr(tipo_remozamiento,
                                   fix_sources,
                                   labels = labels_tipo_remozamiento)
  )

#areas_impartidas----

valores_areas_impartidas <- obs01 |> 
  filter(!(is.na(areas_impartidas))) |> 
  select(areas_impartidas)

labels_areas_impartidas<- c(
"Ciencias Naturales", "Ciencias Sociales Formación Ciudadana e Interculturalidad", 
"Idioma Español", "Idioma Extranjero Inglés", "Culturas e Idiomas Mayas, Garífuna o Xinka", 
"Educación Artística (Educación Musical)", "Educación Artística (Teatro)","Educación Artística (Artes Visuales)", 
"Educación Artística (Danza)", "Educación Artística (integrada)", "Educación Física",
"Emprendimiento para la productividad", "Matemáticas", 
"Tecnologías del Aprendizaje y la Comunicación -TAC-", "Otro"  
)

obs01 <- obs01 |>
  mutate(
    areas_impartidas = map_chr(areas_impartidas,
                                fix_sources,
                                labels = labels_areas_impartidas)
  )

# tipos_nee_identificadas----
valores_tipos_nee_identificadas <- obs01 |> 
  filter(!(is.na(tipos_nee_identificadas))) |> 
  select(tipos_nee_identificadas)

labels_tipos_nee_identificadas<- c(
  "Discapacidad auditiva", "Discapacidad visual", "Gente pequeña", "Discapacidad física o motora",
 "Enfermedades raras o síndrome asociado a discapacidad", "Discapacidad Intelectual", "Trastorno del Espectro Autista", 
 "Discapacidad múltiple (dos o más discapacidades)", 
 "No se pueden identificar las necesidades educativas especiales asociadas a discapacidad en el aula"  
)

obs01 <- obs01 |>
  mutate(
    tipos_nee_identificadas = map_chr(tipos_nee_identificadas,
                               fix_sources,
                               labels = labels_tipos_nee_identificadas)
  )

#idiomas_estudiantes DUDA ----

valores_idiomas_estudiantes <- obs01 |> 
  filter(!(is.na(idiomas_estudiantes))) |> 
  select(idiomas_estudiantes)

labels_idiomas_estudiantes<- c(
"Español","Un idioma maya", "Xinka", "Garífuna", "Inglés", "Otro "
)

obs01 <- obs01 |>
  mutate(
    idiomas_estudiantes = map_chr(idiomas_estudiantes,
                                      fix_sources,
                                      labels = labels_idiomas_estudiantes)
  )

#idiomas_mayas----

valores_idiomas_mayas <- obs01 |> 
  filter(!(is.na(idiomas_mayas))) |> 
  select(idiomas_mayas)

labels_idiomas_mayas<- c(
  "Achi", "Akateko", "Awakateko", "Chalchiteko", "Chorti’", "Chuj", "Itza",
  "Ixil", "Jakalteko", "Qánjob’al", "Kaqchikel", "K’iche’", "Mam", "Mopan",
  "Poqomam", "Poqomchi", "Q’eqchi’", "Sakapulteko", "Sipakapens", "Tektiteko",
  "Tz’utujil", "Uspanteko"
)

obs01 <- obs01 |>
  mutate(
    idiomas_mayas = map_chr(idiomas_mayas,
                                      fix_sources,
                                      labels = labels_idiomas_mayas)
  )

#idioma_clases_generall----

valores_idioma_clases_general <- obs01 |> 
  filter(!(is.na(idioma_clases_general))) |> 
  select(idioma_clases_general)

labels_idioma_clases_general<- c(
"Estudiantes mayas/garífunas/xinkas piden aclaraciones en su idioma materno.", 
"Estudiantes muestran dificultad para entender las explicaciones en español.",
"Estudiantes usan espontáneamente su idioma materno para interactuar." 
)

obs01 <- obs01 |>
  mutate(
    idioma_clases_general = map_chr(idioma_clases_general,
                            fix_sources,
                            labels = labels_idioma_clases_general)
  )

#tipos_mobiliario----
valores_tipos_mobiliario <- obs01 |> 
  filter(!(is.na(tipos_mobiliario))) |> 
  select(tipos_mobiliario)

labels_tipos_mobiliario<- c(
  "Pupitres (estructura unificada de mesa y silla) para estudiantes",
  "Mesas y sillas separadas para estudiantes", 
  "Mesa para docentes", "Silla para docentes", "Otro"
)

obs01 <- obs01 |>
  mutate(
    tipos_mobiliario = map_chr(tipos_mobiliario,
                                    fix_sources,
                                    labels = labels_tipos_mobiliario)
  )

#posibles_riesgos----
valores_posibles_riesgos <- obs01 |> 
  filter(!(is.na(posibles_riesgos))) |> 
  select(posibles_riesgos)

labels_posibles_riesgos<- c(
  "Objetos colgados o apoyados que no están bien fijados y podrían caer (como pizarras, estantes, ventiladores, etc.).",
  "Objetos punzocortantes o materiales peligrosos al alcance de los estudiantes.",
"Instalaciones eléctricas expuestas o en mal estado.", 
"Pisos resbalosos o deteriorados",
"Mobiliario quebrado o inestable.",
"Paredes, techos o ventanas en mal estado.", 
"Espacios reducidos que dificultan la movilidad.",
"Otros riesgos",
"No se identificaron riesgos en el aula"
)

obs01 <- obs01 |>
  mutate(
    posibles_riesgos = map_chr(posibles_riesgos,
                               fix_sources,
                               labels = labels_posibles_riesgos)
  )

#recursos_tecnologicos----
valores_recursos_tecnologicos <- obs01 |> 
  filter(!(is.na(recursos_tecnologicos))) |> 
  select(recursos_tecnologicos)

labels_recursos_tecnologicos<- c(
  "Cañonera", "Televisión (pantalla)", "Pantalla para proyectar",
  "Computadora de escritorio", "Laptop", "Ninguno", "Otro" 
)

obs01 <- obs01 |>
  mutate(
    recursos_tecnologicos = map_chr(recursos_tecnologicos,
                               fix_sources,
                               labels = labels_recursos_tecnologicos)
  )