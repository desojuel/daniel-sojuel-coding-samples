obs03 <- read_xlsx(here("Datos","Observación especialistas", "obs03.xlsx"))

# razon_docente_no_entrego_copias ----

## ver errores

valores_razon_docente_no_entrego_copias <- obs03 |>
  filter(docente_entrego_copias_planificaciones == "No") |>	
  filter(!(is.na(razon_docente_no_entrego_copias))) |>	
  select(razon_docente_no_entrego_copias) 

dput(valores_razon_docente_no_entrego_copias)

## Define los vectores ----

### no tenia copias
valores_sin_copia2 <- c(
  "No las tenía a la mano",
  "Lleva muchos trabajos y no la puede andar cargando para arriba y para abajo.",
  "Dijo que no lo cargaba y que otro día me lo dará",
  "Indico que no la habia bajado del vehículo.",
  "No llevaba la planificación y es la misma docente de la sección A",
  "No lo cargar",
  "Se le olvidó cargarlas como es la.misma actividad manifestó",
  "No lo cargaba", "Es practicante. Dice que se le olvidó",
  "Dice que no suele traerlas. Se le volvió a pedir y quedó que mañana trae la de bloque y la de clase"
) 

### no quiso 
valores_noquiso <- c(
  "Se le pidió pero no la compartió.","No quiero entregarlas",
  "No las entregó a pesar que se le pidieron"
)

### entregó antes
valores_entregada_antes <- c(
  "Ya las proporcionó.",
  "Ya lo hizo el día anterior.",
  "Ya lo hizo en la sesión anterior.",
  "Ya lo hizo en la sesión anterior. La docente trabaja con las tres secciones de tercer grado.",
  "Ya se hizo en la sesión anterior, el plan es semanal.",
  "Lo hizo en sesiones anteriores.","Dijo que era el mismo del anterior",
  "Ya lo hizo en la sesión anterior de la sección B.",
  "Ya lo hizo en sesiones anteriores.",
  "Lo hizo en la sesión anterior.",
  "Lo hizo en sesiones anteriores.",
  "Lo hizo en sesiones anteriores.","La había entregado a la comisión de evaluación.",
  "La planificación se había entregado a la comisión."
)

### motivo laboral
valores_actividades_laborales <- c(
  "Expresó que le iba a dar seguimiento a las tareas y nota del tercer bimestre.",
  "Indicó que le correspondía a los estudiantes diseñar su carátula del cuarto bimestre.",
  "Le tocaba calificar actividades con las hojas contables dónde elaboraron presupuestos.",
  "Esta calificando tareas y sumando nota final del tercer bimestre.",
  "Los primeros 20 minutos atendió a una madre de un estudiante."
)

### no especifica
valores_noespecifica_plani <- c(
  "Ninguna",
  "Ninguna, omitió el tema.", "No se sabe dijo que después lo entregará",
  "No mencionó",
  "Se desconoce",
  "Se desconoce",
  "Se desconoce",
  "Se desconoce",
  "No mencionó"
)

### practicante
valores_practicante <- c(
  "Es practicante. Las presentó antes y no las lleva consigo."
)


### docente nuevo
valores_docente_nuevo <- c(
  "Es docente nuevo y solo está terminando el ciclo escolar pero tiene  muy activo a los estudiantes les hace preguntas como la importancia de hablar dos o más idiomas o también saber de la interculturalidad y multiculturalidad tradiciones y costumbres",
  "Según él porque acaba de tomar posesión en el puesto como docente de Educación Física en el centro educativo"
  
)

### no esta presente
valores_sinpresencia <- c(
  "Esta suspendida",
  "no se encuentra en el centro educativo, porque no tiene tiempo completo"
)

### no hay docente especializado en el area
valores_noespecializa_area <- c(
  "por que el docente no es nombrado o es de la especialidad de la clase de educación física",
  "Por que no se cuenta con maestro de educación física, y se imparte de forma empírica",
  "actualmente el director supervisa el curso ya que no cuentan con maestro de educación física, ya que solo los primeros 3 bimestres recibieron el curso de educación fisica por un docente especializado",
  "no posee planificación ya que no es un docente especializado",
  "el docente no posee planificación, ya que no es un docente especializado en la clase de educación física",
  "no posee planificación ya que no es un docente especializado en el curso de educación física"
  
) 

## Recode con case match ----

obs03 <- obs03 |>
  mutate(
    razon_docente_no_entrego_copias = case_match(
      razon_docente_no_entrego_copias,
      all_of(valores_noespecializa_area) ~ "No es docente especializado en el área",
      all_of(valores_sinpresencia) ~ "No está presente el docente",
      all_of(valores_docente_nuevo) ~ "Es docente nuevo",
      all_of(valores_practicante) ~ "Es practicante",
      all_of(valores_noespecifica_plani) ~ "No se especifica",
      all_of(valores_actividades_laborales) ~ "Realizaba actividades laborales",
      all_of(valores_entregada_antes) ~ "Las entregó antes",
      all_of(valores_noquiso) ~ "No quiso entregarlas",
      all_of(valores_sin_copia2) ~ "No tenía copias",
      .default = razon_docente_no_entrego_copias
    )
  )

## volver a ver errores

valores_razon_docente_no_entrego_copias <- obs03 |>
  filter(docente_entrego_copias_planificaciones == "No") |>	
  filter(!(is.na(razon_docente_no_entrego_copias))) |>	
  select(razon_docente_no_entrego_copias)

#View(valores_razon_docente_no_entrego_copias)

# otro_elementos_planificaciones ----

## ver errores

valores_otro_elementos_planificaciones <- obs03 |>
  filter(elementos_planificaciones == "Otro") |>	
  filter(!(is.na(otro_elementos_planificaciones))) |>	
  select(otro_elementos_planificaciones) 

dput(valores_otro_elementos_planificaciones)

## Define los vectores ----

### ninguno

valores_ninguna_plani <- c(
  "Ninguna",
  "Ninguno","Dice que es su primer día de clase. Que la maestra anterior le tiene que dar la planificación",
  "No sé evidencia", "No entrego planificación",
  "ninguno, no poseen planificación",
  "no cuenta con planificación",
  "no se presento, por qué no se cuenta con docente de educación física"
)

### competencia e indicadores de logro
valores_elementos_curriculares <- c(
  "COMPETENCIA, INDICADOR DE LOGRO Y CONTENIDOS"
)

### cronograma
valores_cronograma <- c(
  "El cronograma de actividades de cuarto bimestre."
)


### horario clases
valores_horario_clases <- c(
  "Horario de clases",
  "horario de clases",
  "horario de clases"
)

## Recode con case match ----

obs03 <- obs03 |>
  mutate(
    otro_elementos_planificaciones = case_match(
      otro_elementos_planificaciones,
      all_of(valores_horario_clases) ~ "Horario de clases",
      all_of(valores_cronograma) ~ "Cronograma",
      all_of(valores_elementos_curriculares) ~ "Competencias e indicadores de logro",
      all_of(valores_ninguna_plani) ~ "No indica",
      .default = otro_elementos_planificaciones
    )
  )

## volver a ver errores

valores_otro_elementos_planificaciones <- obs03 |>
  filter(elementos_planificaciones == "Otro") |>	
  filter(!(is.na(otro_elementos_planificaciones))) |>	
  select(otro_elementos_planificaciones)

#View(valores_otro_elementos_planificaciones)

# lugar_desarrollo_clase ----

## ver errores

valores_lugar_desarrollo_clase <- obs03 |>
  filter(!(is.na(lugar_desarrollo_clase))) |>	
  select(lugar_desarrollo_clase) 

dput(valores_lugar_desarrollo_clase)

## Define los vectores ----

### salon de clases
valores_salon_clases <- c(
  "Salón de clases",
  "Salón de clases",
  "Salón de clases",
  "Salón de clases",
  "Salón de clases",
  "Salón de clases",
  "Salón de clases",
  "2C segundo",
  "Salon de clases",
  "Salón de clases.",
  "Salón de clases.",
  "Salón de clases",
  "Salón de clases",
  "Salón de clases",
  "Salón de clases",
  "Salón de clases",
  "Salón de clases",
  "Un aula con buena iluminación y ventilación, no cuenta con basurero y se observan escrituras y dibujos en la pared",
  "Salón de clases",
  "Salón de clases",
  "Salón de clases",
  "Salón de clases.",
  "Salón de clases",
  "Salón de clases.",
  "Muy estrecho y sin ventilador",
  "Salón de clases",
  "Salón de clases.",
  "Salón de clases.",
  "Salón de clases.",
  "En el salón de clases.",
  "Salon de clases.",
  "Salon de clases",
  "Salon de clases",
  "Salon de clase",
  "Salón de clases.",
  "Salón de clases",
  "Salón de clase.",
  "Salón de clases.",
  "Salón de clases",
  "Salón de clases",
  "Salón de clases",
  "Salón de clases",
  "Salón de clases",
  "Salón de clases","Salón de clase en virtud de encontrarse en evaluación escrita del curso y una semana antes se realizo la evaluación práctica.",
  "En el salón de clases.",
  "Salón de clases",
  "En el salón de clases.",
  "En el salón de clases.",
  "Salón de clases.",
  "El aula es amplia, bien iluminada y tiene buena ventilación. Los escritorios están en buenas condiciones.",
  "Él habla es amplia bien iluminada bien ventilada de la pintura está descuidada y tiene rótulos y escrituras en la pared. También se observa basura en el piso entre las filas de escritorios",
  "El aula es amplia, bien iluminada y ventilada. Hay papeles tirados en el piso y rótulos y dibujos inadecuados en las paredes",
  "Él habla es amplia bien iluminada y bien ventilada. Se observan limpieza en el aula y buen comportamiento de los estudiantes quienes participaban libremente.",
  "Actividad fuera y dentro del salón de clases.",
  "Salón de clases",
  "Salón de clases.",
  "Salón de clases.",
  "Salón de clases.",
  "En el salón de clases.",
  "Salón de clases.",
  "Salón de clases.",
  "Salón de clases.",
  "Salón de clases.",
  "Salón de clases.",
  "Salón de clases.",
  "Salón de clases",
  "Salón de clase",
  "Salon de clase",
  "Salón de clase dispuesto en filas.",
  "Aula dispuesta en filas.",  "Clase",
  "Clase", "Clase",
  "Clase", "Clase","Clase",
  "Clase","Clase",
  "Clase",  "Clase",
  "Clase","Clase",
  "Clase",  "Clase",
  "Clase","Clase", "Clase",
  "Clase","Clase", "Clase"
)

### cancha deportiva

valores_cancha_deportiva <- c(
  "Cancha polidepartiva",
  "Cancha polideportiva",
  "Cancha deportiva",
  "Cancha polideportiva",
  "en la cancha",
  "Patio el Establecimiento",
  "Cancha de baloncesto",
  "Cancha de baloncesto",
  "Cancha deportiva",
  "Cancha Deportiva",
  "Cancha Deportiva",
  "Cancha Deportiva",
  "Polideportivo",
  "en la cancha",
  "En una cancha pero hay chorros parquedos.",
  "Cancha deportiva",
  "Chanca polideportiva",
  "Polideportivo",
  "Polideportivo",
  "Polideportivo",
  "Cancha",
  "Cancha polideportiva"
)

### actividad fuera de clase
valores_fuera_clase <-  c(
  "En el corredor de las instalaciones."
)

### no se observó desarrollo de clase
valores_no_observable <- c(
  "No se pudo observar porque la practicante de psicología llegó a trabajar el mismo tema. Ya estaba cuando llegamos del otro salón.",
  ". No había plan, faltó control de la clase, no hubo feed back, se evidencia una improvisación de la clase",
  "No hubo por la razón de ser experimental y haber finalizado ya el Ciclo escolar."
)

## Recode con case match ----

obs03 <- obs03 |>
  mutate(
    lugar_desarrollo_clase = case_match(
      lugar_desarrollo_clase,
      all_of(valores_no_observable) ~ "No se observó desarrollo de clase",
      all_of(valores_fuera_clase) ~ "Actividad fuera de clase",
      all_of(valores_cancha_deportiva) ~ "Cancha deportiva",
      all_of(valores_salon_clases) ~ "Salón de clases",
      .default = lugar_desarrollo_clase
    )
  )

## volver a ver errores

valores_lugar_desarrollo_clase <- obs03 |>
  filter(!(is.na(lugar_desarrollo_clase))) |>	
  select(lugar_desarrollo_clase)

#View(valores_lugar_desarrollo_clase)

write_xlsx(obs03, here("Datos","Listos para análisis","Observación especialistas", "obs03.xlsx"))