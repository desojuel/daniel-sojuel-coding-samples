df_est <- read_xlsx(here("Datos","Encuesta estudiantes", "df_est.xlsx"))

# participante_no_oficial ----

## ver errores 

valores_participante_no_oficial <- df_est |> 
  filter(participante == "No es parte de la muestra oficial") |> 
  filter(!(is.na(participante_no_oficial))) |> 
  select(participante_no_oficial)


## Define los vectores

### Retiro del estudiante

valores_retiro <- c(
  "La persona seleccionada está retirada", "Porque los  5 estudiantes oficiales 3 se trasladaron a otros esblecimiemtos.",
  "Muestra no se completa por retiro de estudiantes titulares y reemplazo.", "Los alumnos de la muestra y reemplazo ya no asisten al establecimiento.",
  "Hubo un estudiante de la muestra principal no siguió estudiando", "la estudiante asignada es retirada",
  "Porque ya no asiste la asignada", "SE RETIRO DEL ESTABLECIMIENTO", "PORQUE SE RETIRARON DEL ESTABLECIMIENTO ALGUNOS DE LA MUESTRA",
  "Se retiró un estudiante muestra principal y reemplazo", "El estudiante de muestra principal está retirado en forma definitiva",
  "Por retiro de la muestra principal", "Un estudiante de la muestra principal ya no asiste; otro ausente, un estudiante de reemplazo ya no asiste.",
  "Dejo de venir no hay remplazo", "Porque el estudiante de la muestra principal ya no asiste al establecimiento", "Estudiantes fueron trasladadas a otro establecimiento",
  "Se retiro el principal no hay reemplazo", "EL ALUMNO DE LA MUESTRA ESTA  RETIRADO", "Esta retirada no hay remplazo",
  "Retiro", "EL ESTUDIANTE DE LA MUESTRA ESTA RETIRADO", "Ya se tomo un reemplazo y el otro reemplazo se encuentra retirado.",
  "Por motivos de retiro del centro educativo", "Se retiraron ya no asisten", "RETIRO DEL ALUMNO ASIGNADO", "Porque se retiró",
  "Porque la estudiante principal ya no está asistiendo a clases", "El estudiante muestra es retirado", "Estudiante muestra es retirado",
  "El alumno del segundo reemplazo está retirado del establecimiento", "Tres estudiantes de muestra se han retirado del centro educativo ya en el último bloque",
  "Están retirados 3 titulares y 1 suplente", "Están retirados 3 de la muestra y 1 de los suplentes",
  "EL ALUMNO DE LA MUESTRA ESTA RETIRADO"
)

### No asistió el estudiante

valores_no_asistio <- c(
  "No asistieron los que formaban parte de la muestra",
  "Ausencia titulares y reemplazos",
  "Ausencia de titulares y reemplazo.  Número 10 de la lista.",
  "Ausencia de titulares y reemplazo.",
  "No están los alumnos de la muestra",
  "No están los estudiantes de la muestra",
  "Porque no están los estudiantes de la muestra",
  "No asiste por problemas familiares", "No vino la estudiente que correspondía",
  "De la muestra principal, un estudiante ya no asiste, dos estudiantes no se presentaron; ya se aplicó Encuesta a los estudiantes de la muestra reemplazo.",
  "No estaba el titular ni el remplazo",
  "No están las muestras principales, ni la de remplazo",
  "No estaba presente la muestra principal, y en reemplazos cubrieron a muestra principal",
  "No se presentaron los titulares y los suplentes",
  "No se presentaron los titulares y los suplentes",
  "No asistieron a clase el estudiante de la muestra principal ni el reemplazo por problemas de lluvia",
  "No asistieron a clases el estudiante de la muestra principal ni el reemplazo por problemas de fuertes lluvias",
  "No se presentó",
  "Los estudiantes de la muestra principal y reemplazo no asistieron a clases por problemas de fuertes lluvias",
  "Inasistencia de la muestra principal",
  "No están presentes los titulares y los suplentes por lluvia",
  "Los demás no se presentaron",
  "No se encontraban los enlistados.",
  "No asistió a clases",
  "no estaba presente el estudiante",
  "No se encontraba el estudiante",
  "NO ESTABA PRESENTE EL ESTUDIANTE",
  "no estaba presente estudiante",
  "No estaba presente el estudiante",
  "No estaba el estudiante",
  "Alumnos muestra y reemplazo incompletos.  Haciéndose necesario completar.",
  "Los seleccionados no estaban en el centro educativo",
  "No se prrsento el estudiante y el reemplazo es de tercero no es de segundo.",
  "Se usentó",
  "No podía asistir una estudiante de muestra principal",
  "No asistió último estudiante que forma parte de la muestra.  Ni hay reemplazos disponibles.",
  "No está muestra principal ni reemplazo",
  "No presente muestra oficial ni reemplazo",
  "Porque el titular y el reemplazo no asistieron",
  "No sé encontro el estudiante principal ni reemplazo",
  "No están presente el alumno Principal ni reemplazo",
  "No asisten alumnos de muestra principal ni reemplazo",
  "Porque no están los estudiantes",
  "No asistió el estudiante seleccionado y reemplazo",
  "Ausencia de los anteriores",
  "No se presentó la seleccionada",
  "No podian presentarse por su trabajo",
  "No se presento muestra principal por razones personales",
  "No pudo asistir la muestra principal",
  "Ausencia muestra principal y reemplazo.",
  "Ausencia de muestra principal y reemplazo", 
  "Ausencia de muestra y de reemplazo",
  "No se logró contactar a los estudiantes de la muestra, el establecimiento está en remozamiento.",
  "No vinieron los alumnos ee la muestra principal",
  "No estaban los estudiantes de la muestra principal y se usaron los reemplazos pero faltaba uno",
  "El titular no se presentó en este día",
  "No asistencia de la muestra y reemplazos por lluvias",
  "No asistencia de la muestra y reemplazo por lluvia",
  "No asistencia de la muestra y reemplazo por lluvia",
  "No presencia de la muestra principal y suplentes por lluvia",
  "Los nombrados estan en otro  departamento",
  "La estudiante de la muestra principal no está asistiendo a clases",
  "Los estudiantes no se encuentran, por la mañana deportiva.",
  "Los reemplazos no se presentaron el dia de la entrevista",
  "No se encontraron a los estudiantes de la muestra",
  "No se encontraba ninguno de la muestra",
  "No se encontraban los estudiantes de la muestra principal",
  "No se encontraba ninguno de los propuestos",
  "Porque el estudiante reemplazo del listado el alumno de segundo básico",
  "No se encuentra titular ni remplazo el día de hoy",
  "No están presentes los estudiantes de la muestra principal",
  "NO DE ENCUENTRA AL ESTUDIANTE", "No se encontraban los demás",
  "No asistieron", 
  "NO VINO A CLASES",
  "No asistieron los estudiantes de la muestra. Algunos reciben clases a distancia",
  "No vinieron a estudiar todos los estudiantes y algunos reciben clases a distancia", 
  "No asistieron a clases",
  "Inundado el lugar donde vive",
  "Los estudiantes de la muestra principal no están todos",
  "Porque el alumno no se presentó",
  "Por no estar el compañero",
  "No sé encontraba el compañero",
  "No sé presentó su compañero",
  "No estaban presentes todos los estudiantes el.dia de la entrevista",
  "No estaban todos los estudiantes presentes el día de la entrevista",
  "No asistieron todos los estudiantes de la sección",
  "Los estudiantes ya se encuentran de vacaciones y lograron comunicación a traves de la vía de llamadas.",
  "No se presentaron los estudiantes de la muestra principal",
  "Porque el estudiante titular y suplente estaban ausentes",
  "El titular ya no está estudiando, la suplente está en segundo básico",
  "La muestra Oficial No se encuentra",
  "No sé encuentra el oficial",
  "Porque no asistieron los participantes de la muestra principal, ni de reemplazo",
  "No asistieron a clases los participantes en la muestra principal y de reemplazo", 
  "No se encontraban presentes los de la muestra principal",
  "No se encontraban los estudiantes seleccionados en la muestra",
  "Los seleccionados no estaban en el centro educativo",
  "Porque no asistieron algunos titulares y reemplazos",
  "No asistió a clases", "No asistió el estudiante muestra principal ni reemplazo",
  "El estudiante principal no asistió",
  "Las otras no llegaron", "No llegaron 2 de la muestra principal y 1 estudiante del reemplazo",
  "No se presento a clases el estudiante",
  "No llegaron los estudiantes de la muestra y reemplazo",
  "No llegaron los estudiantes de la muestra, tampoco los de reemplazo",
  "No llegaron los de la muestra, tampoco los de reemplazo",
  "No llegaron los de la muestra, tampoco de reemplazo",
  "No se encontraban presentes los intregantes de la muestra principal",
  "Sustitución de muestra principal y de reemplazo",
  "No se encontraban integrantes de la muestra principal",
  "No se presentó el que venia en el listado",
  "No sé presentó que está en el listado",
  "No se presentó el que venía en el listado",
  "No se precento el que venía en el listado",
  "No se presento el que venía en el listado",
  "No se presentó el que estaba en el listado",
  "No se encontró a los estudiantes de la muestra",
  "Uno por enfermedad, uno por evaluación de TAC y dos no pertenecen a la muestra.",
  "Ausencia de titulares y reemplazo. Número 10 de la lista.",
  "No se encontraban presentes los integrantes de la muestra principal",
  "Ausencia de titulares y reemplazo. Número 10 de la lista.",
  "Falta de estudiantes oficiales",
  "No asistió último estudiante que forma parte de la muestra. Ni hay reemplazos disponibles."
)

### falta por enfermedad

valores_problema_salud <- c(
  "Problemas de salud", "Por motivos laborales y por quebrantos de salud", 
  "Por quebrantos de salud de la estudiante seleccionada",
  "Los estudiantes de la muestra estaban enfermos", "Por estar enfermo el principal y el suplente no asistió a clases",
  "Porque el estudiante de la muestra principal se encuentra enfermo y el de la muestra suplente no pertenece al establecimiento",
  "No se presento la principal por enfermedad", "Por razones de salud.",
  "No asistió a clases por enfermedad", "El estudiante de la muestra principal se encuentra enfermo",
  "Porque la niña a qu a quien le correspondía tiene discapacidad intelectual con edad cronológica de 16 años y mental 7años, con ella utilizan adecuación curricular, y ya no había muestra de reemplazo.",
  "Por estar enfermo los de la muestra", "La muestra principal no se presentó por enfermedad",
  "No se presentó al Centro Educativo por hospitalización."
)

### por fallecimiento

valores_fallecimiento <- c(
  "Sustitución por fallecimiento de estudiante de muestra principal", 
  "Fallecimiento del alumno"
)

### falta de estudiantes

valores_pocos_estudiantes <- c(
  "Se le seleccionó ya que en el grado de segundo básico son  seis estudiantes.",
  "Se tuvo que tomar en cuenta a la alumna ya que del listado oficial solo un alumno estaba en plan diario.",
  "Se eligió a la estudiante ya que son únicamente siete alumnos de tercero y ella no figura en el listado oficial.",
  "Esta mal seleccionada la muestra un estudiante pertenece a tercero básico no a segundo y el próximo suplente se retiró",
  "Se toma como muestra ya que en el listado oficial de alumnos solo aparece un alumno inscrito en plan diario, el resto son de fin de semana.",
  "Se agotaron los titulares y los reemplazo.", "No pudo presentarse de otra muestra", 
  "Se le seleccionó ya que en el grado de segundo básico son seis estudiantes.", 
  "Alumnos muestra y reemplazo incompletos. Haciéndose necesario completar.",
  "Alumnos muestra y reemplazo incompletos. Haciéndose necesario completar."
)

### trabajo

valores_trabajo <- c(
  "Por trabajo del principal", "Por trabajo de la muestra Principal",
  "Trabajo"
)

### voluntario

valores_voluntario <- c(
  "Se presentó voluntariamente para ser encuestado", "Por permiso"
)

### eleccion autoridad
valores_seleccion_autoridad <- c(
  "Selección de la directora del establecimiento"
)

### no especifica
valores_sin_especificar <- c(
  "....", "...", "KIMBERLY PAOLA TAYÚN AJANEL"
)

## Recode con case_match
df_est <- df_est |>
  mutate(
    participante_no_oficial = case_match(
      participante_no_oficial,
      all_of(valores_retiro) ~ "El estudiante titular se retiró",
      all_of(valores_no_asistio) ~ "El estudiante titular no está",
      all_of(valores_problema_salud) ~ "El estudiante titular está enfermo",
      all_of(valores_fallecimiento) ~ "El estudiante titular falleció",
      all_of(valores_pocos_estudiantes) ~ "Falta de estudiantes oficiales",
      all_of(valores_trabajo) ~ "Por trabajo del estudiante titular",
      all_of(valores_voluntario) ~ "Participó voluntariamente",
      all_of(valores_seleccion_autoridad) ~ "Selección por autoridad del establecimiento",
      all_of(valores_sin_especificar) ~ "No especifica",
      .default = participante_no_oficial
    )
  )

## volver a ver errores
valores_participante_no_oficial <- df_est |> 
  filter(participante == "No es parte de la muestra oficial") |>
  filter(!(is.na(participante_no_oficial))) |> 
  select(participante_no_oficial)


# otras_razones_repitencia ----

## ver errores 

valores_otras_razones_repitencia <- df_est |> 
  filter(razones_repitencia == "Otro") |> 
  filter(!(is.na(otras_razones_repitencia))) |> 
  select(otras_razones_repitencia)


## Define los vectores ----

### por enfermedad

valores_enfermedad <- c(
  "Enfermedad", "Por enfermedad",
  "Se retiró a medio año por enfermedad en el Ciclo de 2022."
)

### retiro del establecimiento

valores_traslado <- c(
  "Transferencia", "Por traslado de centro educativo" 
)

### dejo de estudiar

valores_desercion <- c(
  "Ausente mucho tiempo al establecimiento", "Se retiró", "Porque tenía planes de salir de país",
  "Por ausencias durante el año", "Dejo de estudiar", "Por abandono", "No iba a clases",
  "Dejó de estudiar", "No estudio el año anterior", "Por no cumplir con las tareas y ausencia",
  "PORQUE SUSPENDIO ESTUDIOS DESDE EL 2021"
)

### problemas personales, familiares o economicos

valores_problemas <- c(
  "Por problemas familiares", "Por motivo personal", "Situación económica",
  "Por no pagar colegiaturas no dejaron fuera y dejo de estudiar tres años\nHector Lima",
  "Problemas familiares", "Problemas familiares", "Problema familiar",
  "Unos jovenes querían meterlo en una pandilla"
)

### problemas en el establecimiento

valores_administracion <- c(
  "Problemas administrativos en el colegio.", "Por problemas con una docente en el establecimiento anterior.",
  "Porque no entregaron notas de  todo el grado  y la docente nunca se presento siempre estaba enferma.",
  "Falta de tiempo en el anterior establecimiento", "Por acoso individuos"
)

### por trabajo

valores_trabajo_repitencia <- c(
  "Porque trabaje el año pasado", "Por trabajo, ya no pudo terminar el año."
)

### bajas calificaciones

valores_calificacion <- c(
  "No entregaba tareas", "Por no cumplir en las clases", "No se evaluó",
  "No logro ganar los curso", "No puse de mi parte"
)

### problema de conducta
valores_conducta <- c(
  "mala conducta"
)

### no repite
valores_no_repitencia <- c(
  "No está repitiendo el grado"
)

## Recode con case_match ----
df_est <- df_est |>
  mutate(
    otras_razones_repitencia = case_match(
      otras_razones_repitencia,
      all_of(valores_traslado) ~ "Cambió de establecimiento",
      all_of(valores_desercion) ~ "Dejó de estudiar",
      all_of(valores_problemas) ~ "Por problemas familiares y económicos",
      all_of(valores_administracion) ~ "Problemas de sistema del establecimiento",
      all_of(valores_calificacion) ~ "No alcanzó zona mínima",
      all_of(valores_conducta) ~ "Mala conducta",
      all_of(valores_no_repitencia) ~ "No está repitiendo",
      all_of(valores_trabajo_repitencia) ~ "Empezó a trabajar",
      all_of(valores_enfermedad) ~ "Enfermó",
      .default = otras_razones_repitencia
    )
  )

## volver a ver errores

valores_otras_razones_repitencia <- df_est |>
  filter(razones_repitencia == "Otro") |>
  filter(!(is.na(otras_razones_repitencia))) |>
  select(otras_razones_repitencia) 

# motivo_interrupcion_encuesta ----

## ver errores

valores_motivo_interrupcion_encuesta <- df_est |>
  filter(interrupcion_encuesta == "Sí") |>
  filter(!(is.na(motivo_interrupcion_encuesta))) |>
  select(motivo_interrupcion_encuesta)


## Define los vectores

### actividad academica
valores_actividad <- c(
  "Por exámen de emprendimiento Pero el alumno regreso",
  "El alumno tenía que participar en un devocional, pero se finalizó después"
)

### errores con el sistema
valores_sistema <- c(
  "El sistema ya no desplegó las preguntas de matemática",
  "El sistema no desplegó las preguntas de ciencias naturales",
  "Errores del sistema, no despliega la información",
  "La plataforma me envió a la pregunta 63",
  "No dió ninguna encuesta por no estar marcada el área de Ciencias Naturales",
  "se trabo",
  "NO DEJO GUARDAR",
  "No dejo guardar la encuesta",
  "Me regreso al inicio y me toco volver a llenar.",
  "Me saco de sistema",
  "Ocultó preguntas",
  "El sistema no desplegó las preguntas(problemas técnicos del sistema)",
  "Problemas del sistema.",
  "Esque se salio"
)

### fallo internet
valores_internet <- c(
  "Por fallo de señal de Internet.", "Fallo de señal de internet",
  "Mala Señal de Internet", "Falta de señal",
  "El internet falla constante", "La señal de internet\n Se conectó nuevamente"
)

### por interrupción de personas
valores_personas <- c(
  "Presencia de un líder Sindical",
  "Los del sindicato interrumpieron", "Me llevaron café",
  "Atención a una persona. Consulta", "LLEGO EL DIRECTOR A MOSTRAR UN DOCUMENTO",
  "Nos visito la autoridad del lugar", "El encuestador fue al baño", 
  "Pasó un vehículo con auto parlantes con mucho volumen"
)

### llamada telefonica
valores_llamada <- c(
  "Por contestar un mensaje de la autoridad.",
  "Una llamada del jefe superior",
  "Entró una llamada telefónica",
  "Llamada de la jefa"
  
)

### clima 
valores_lluvia <- c(
  "Mucha lluvia, techo de lamina no se escuchaba correctamente",
  "Llovía intensamente, techo de lamina impedía el poder seguir.",
  "por la lluvia y truenos ,no se escucha",
  "Probablemente señal denil por el clima",
  "Probablemente la incidencia del clima"
)

### desconoce
valores_desconoce <- c(
  "No se", "No se"
)

### falla dispositivo
valores_dispositivo <- c(
  "Me sacó por virus",
  "Se me resfalo el celilar"
)

### dificultad habilidad y atencion
valores_dificultad <- c(
  "Porque me cuesta un poco",
  "Por el horario de receso, y por la falta de atención del estudiante, que explicó sus razones."
)

### cambio de area
valores_cambio_area <- c(
  "Nos cambiaron de lugar",
  "Cambio de area",
  "Por una descarga eléctrica"
)

### problema de organizacion
valores_organizacion <- c(
  "No sé facilitaron los horarios a tiempo.",
  "Por no tener coherencia de áreas que estudian"
)

### no interrupcion
valores_sin_interrupcion <- c(
  "Fue un gusto aser esta evaluacion",
  "No", "Ninguno"
)

## Recode con case_match
df_est <- df_est |> 
  mutate(
    motivo_interrupcion_encuesta = case_match(
      motivo_interrupcion_encuesta,
      all_of(valores_sin_interrupcion) ~ "No se interrumpió",
      all_of(valores_organizacion) ~ "Por logística al realizarla",
      all_of(valores_cambio_area) ~ "Cambio de área",
      all_of(valores_dificultad) ~ "Falta de habilidad y atención",
      all_of(valores_dispositivo) ~ "Falla del dispositivo",
      all_of(valores_desconoce) ~ "No sabe",
      all_of(valores_lluvia) ~ "Lluvia intensa y ruidosa",
      all_of(valores_llamada) ~ "Por atender llamada",
      all_of(valores_personas) ~ "Llegó otra persona",
      all_of(valores_internet) ~ "Señal de internet",
      all_of(valores_sistema) ~ "Fallas del sistema",
      all_of(valores_actividad) ~ "Actividad académica",
      .default = motivo_interrupcion_encuesta
    )
    
  )

## volver a ver errores

valores_motivo_interrupcion_encuesta <- df_est |>
  filter(interrupcion_encuesta == "Sí") |>
  filter(!(is.na(motivo_interrupcion_encuesta))) |>
  select(motivo_interrupcion_encuesta) 

## ver errores ----

#valores_otras_razones_repitencia <- df_est |> 
  #filter(razones_repitencia == "Otro") |> 
  #filter(!(is.na(otras_razones_repitencia))) |> 
  #select(otras_razones_repitencia)

#dput(valores_otras_razones_repitencia)
#View(valores_otras_razones_repitencia)

## Define los vectores ----


### No sabe

valores_no_sabe <- c(
  "Desconoce, es nuevo", "Es nuevo y desconoce", "Desconoce es nueva en la escuela.",
  "Desconoce, es nueva", "Desconoce esta prestada", "Desconoce, tiene 3 meses en el establecimiento",
  "Desconoce", "Desconoce es nueva", "Desconoce es nuevo", "Es nueva desconoce",
  "Desconoce depende del grado", "Desconoce, es nueva en la escuela.",
  "Desconoce ella atendía primero y recibía cada sño", "TIENE UN MES DE INGRESO EN LA INSTITUCIÓN Y DESCONOCE CADA CUANTO LLEGAN",
  "Es nuevo en la escuela desconoce", "Es nueva, desconoce",
  "No recuerda", "No sabe, es nueva en el establecimiento",
  "NO SABE, ES PRIMER AÑO EN EL ESTABLECIMIENTO",
  "NO SABE ES PRIMER AÑO EN EL ESTABLECIMIENTO",
  "No sabe y no tiene libros de texto", "No tengo evidencia es mi primer año aquí",
  "Ingresé este año, no tengo evidencia",
  "Este año nos dieron libros, no tengo evidencia de años anteriores, es mi primer año en la escuela.",
  "EN ESTA ESCUELA NO SABE , EN OTRAS ESCUELAS",
  "LIBRO YO DECIDO SOLO ESOS LIBROS  PRIMER AÑO EN LA ESCUELA Y NO SABE",
  "Es su primer año en el establecimiento y no sabe.",
  "La docente no sabe", "No tiene esa información", "Es nueva",
  "LA DOCENTE ES NUEVA EN LA ESCUELA, IGNORA SI RECIBIERON LIBROS ESTE AÑO.",
  "DOCENTE NUEVA EN EL ESTABLECIMIENTO, NO HAY EVIDENCIA.",
  "Es.nuevo y únicamente recibió primero.",
  "EL DOCENTE INICIO EN ESTE AÑO A LABORAR EN EL ESTABLECIMIENTO",
  "El docente por el poco tiempo en el establecimiento no conoce el dato",
  "ES PRIMER AÑO EN EL ESTABLECIMIENTO Y NA SABE CON QUE FRECUENCIA RECIBEN"
)

### cada año

valores_cada_ano <- c(
  "Varia , regularmente cada año.", "Para primero dos veces al año a mitad de año los de lectura",
  "sOLO GUÍAS DE APRENDE MÁS UNA VEZ POR AÑO", "KEMON CHABA ES ANUAL SU ENTREGA",
  "CADA AÑO PERO SOLO EL DE KEMON CH'AB'AL", "LIBROS DE TEXTO RECIBEN CADA DOS AAÑOS LOS DE KEMON CADA AÑO",
  "CADA AÑO SOLO A PRIMERO LE DAN LIBROS", "SOLO EL GRADO DE PRIMERO RECIBE LIBROS CADA AÑO",
  "Primero cada año otros grados cada 2 o 3 grados.", "1ro. Cada año.  Otros grados cada 2 años.",
  "Cada año pero viene para diferente grado y va a depender a quién le toca y los textos perdón pero es más fácil",
  "ESTE AÑOS SE RECIBIO DE SEGUNDO Y PRIMERO, RECIBE CADA AÑO PERO VARIAN LOS GRADOS",
  "envian cada año diferentes textos para diferentes áreas", "CADA 6 MESE UNA CANTIDAD AL PRINCIPIO DE AÑO Y MEDIADOS DE AÑO"
)

### categoría 

valores_cada_ano_tarde <- c(
  "Cada año.  Tres Meses atrasados", "Cada año retroactivo.  Viene a finales.",
  "SE RECIBEN CADA AÑO PERO MUY TARDE CASI HASTA SEPTIEMBRE",
  "CADA AÑO PERO TARDE, ESTE AÑO NOS E HA RECIBIDO Y LE HACEN FALTA PORQUE LOS UTILIZA MÁS EN EL ÁREA DE MATEMATICA",
  "CADA AÑO PERO LOS LIBROS VIENEN AL FINAL DEL AÑO",
  "Cada año fuera de fecha", "SE RECIBEN FINALIZANDO EL AÑO"
)

## Recode con case_match ----
df_est <- df_est |> 
  mutate(
    participante_no_oficial = case_match(
      participante_no_oficial,
      all_of(valores_no_sabe) ~ "No sabe",
      all_of(valores_cada_ano) ~ "Cada año",
      all_of(valores_cada_ano_tarde) ~ "Cada año, pero tarde",
      .default = participante_no_oficial
    )
  )

## volver a ver errores ----

#valores_participante_no_oficial <- df_est |> 
 # filter(participante == "No es parte de la muestra oficial") |> 
  #filter(!(is.na(participante_no_oficial))) |> 
  #select(participante_no_oficial) |> 
  #tabyl(participante_no_oficial)

# exportar ----

write_xlsx(df_est, here("Datos","Encuesta estudiantes","df_est.xlsx"))

write_xlsx(df_est, here("Datos","Listos para análisis","Encuesta estudiantes/df_est.xlsx"))
