obs02 <- read_xlsx(here("Datos","Observación aulas", "obs02.xlsx"))

# otra_area ----

## ver errores
valores_otra_area <- obs02 |>
  filter(area == "Otra área") |>	
  filter(!(is.na(otra_area))) |>	
  select(otra_area) 

dput(valores_otra_area)

## Define los vectores ----

### contabilidad
valores_contabilidad_otra <- c(
  "CONTABILIDAD","Contabilidad",
  "Contabilidad","Contabilidad",
  "Contabilidad","Contabilidad",
  "Contabilidad","Contabilidad",
  "Contabilidad", "Contabilidad",
  "Contabilidad", "Contabilidad",
  "Contabilidad","Contabilidad", "Contabilidad",
  "Contabilidad", "Contabilidad",
  "Contabilidad general", "CONTABILIDAD", "Contabilidad",
  "Contabilidad","Contabilidad","Contabilidad","Contabilidad"
)

### lectura
valores_lectura_area <- c(
  "Comprensión lectora",
  "Lectura",
  "Lectura",
  "Lectura",
  "Lectura",
  "Lectura",
  "Lectura obligatoria.",
  "Lectura obligatoria.",
  "Lectura",
  "Lectura",
  "Lectura y Valores",
  "Lectura",
  "Lectura",
  "Lectura",
  "Lectura",
  "Lectura",
  "Lectura"
)


### educación en valores y orientacion
valores_edu_valores <- c(
  "Educación en Valores", "Orientación","Orientación"
)

### educacion cristicana
valores_edu_cristiana <- c(
  "EDUCACION CRISTIANA"
)

### robotica y programación
valores_tecno_progra <- c(
  "Robótica",
  "Taller de robótica",
  "Programación",
  "Tecnología"
)

### fisica y quimica
valores_fisica_quimica <-c(
  "Química",
  "Física Fundamental",
  "Física Fundamental"
)

###hogar y emprendimiento
valores_hogar <- c(
  "Hogar (es parte del área de Emprendimiento)",
  "Hogar-Industriales",
  "Hogar",
  "Taller de cocina",
  "Cocina y repostería",
  "Corte y confección",
  "Área Ocupacional",
  "Área  ocupacional cultivos regionales"
)

###comercio y mercadeo
valores_comercio_mercadeo <- c(
  "Técnicas de mercadeo II",
  "Técnicas de mercadeo II",
  "Comercio",
  "Comercio y Servicios"
)

### ortografia y redaccion
valores_redaccion <- c(
  "Lenguage Arts",
  "Ortografía",
  "Redacción y correspondencia"
)

### alternancia
valores_alternancia <- c(
  "Alternancia", "Alternancia",
  "Alternancia","Alternancia"
)

### no especifica
valores_sinespecificar_area2 <- c(
  "Aprender a aprender"
)

### expresión artística
valores_expresion_artistica <- c(
  "Artes Plásticas",
  "Expresión Artística",
  "Expresión Artística.",
  "Baile de convite"
)


## Recode con case match ----
obs02 <- obs02 |>
  mutate(
    otra_area = case_match(
      otra_area,
      all_of(valores_sinespecificar_area2) ~ "No especifica",
      all_of(valores_alternancia) ~ "Alternancia",
      all_of(valores_redaccion) ~ "Ortografía y redacción",
      all_of(valores_comercio_mercadeo) ~ "Comercio y Mercadeo",
      all_of(valores_hogar) ~ "Hogar y Emprendimiento",
      all_of(valores_fisica_quimica) ~ "Física y química",
      all_of(valores_tecno_progra) ~ "Robótica y Programación",
      all_of(valores_edu_cristiana) ~ "Educación cristiana",
      all_of(valores_edu_valores) ~ "Educación en valores",
      all_of(valores_lectura_area) ~ "Lectura",
      all_of(valores_contabilidad_otra) ~ "Contabilidad",
      all_of(valores_expresion_artistica) ~ "Expresión artística",
      .default = otra_area
    )
  )


## volver a ver errores

valores_otra_area <- obs02 |>
  filter(area == "Otra área") |>	
  filter(!(is.na(otra_area))) |>	
  select(otra_area) 

#View(valores_otra_area)


# planificaciones_no_entrega_copias ----

## ver errores

valores_planificaciones_no_entrega_copias <- obs02 |>
  filter(planificaciones_copia_impresa == "No") |>	
  filter(!(is.na(planificaciones_no_entrega_copias))) |>	
  select(planificaciones_no_entrega_copias) 

dput(valores_planificaciones_no_entrega_copias)

## Define los vectores ----

### formato digital
valores_plani_digital <- c(
  "Las trabajan digitales",
  "Las tenía en forma digital.",
  "Porque no las tenía impresas, únicamente digitales, a las cuales se le tomó fotografía.",
  "La tiene únicamente en digital.",
  "Solo tenía la  planificacion en digital, pero no estaba completa.",
  "Únicamente las tenía en digital.",
  "Entrega únicamente digitales.",
  "Solo digitales",
  "Al solicitarlas, indicó que las manejan en forma digital.",
  "Indica que la manejan de forma digital.",
  "Al solicitarlas indicó que únicamente las tienen en digital.",
  "Al solicitarlas, no las tenía a la mano, ya que manejan las planificaciones en forma digital.",
  "Solo las tiene en digital",
  "Solo digital","Al solicitarlas, indicó que las manejan de forma digital.",
  "Solo tenía una planificación digital pero incompleta, no se puede evidenciar la competencia, únicamente el contenido.",
  "Solo está en formato digital",
  "Solo digital",
  "Solo entrega de manera digital",
  "Solo digital",
  "Solo la tenia en digital",
  "Solo la tenía digital",
  "Porque solo la tenia digital",
  "La docente tiene sus planificaciones en digital.",
  "Las cargaba en digital en su computadora",
  "Solamente la tenia en digital",
  "Solamente la tenia en digital"
)

### no tenia copia
valores_sin_copia <- c(
  "No las tenía",
  "Porque no contaba con ellas en el momento.",
  "No las tenía a la mano, se solicitó a Dirección y no hicieron entrega de la planificación correspondiente.",
  "Al solicitarla, no la tenía a la mano.",
  "No las tenía en el momento",
  "No tenía copia",
  "No tenía copia",
  "No tenía copia",
  "No tiene copia",
  "No tenía copia",
  "No tenía copia",
  "No tenía copia",
  "No tenía copia",
  "No tenía copia",
  "No tenía copia",
  "No tenía copia",
  "No tenía copia",
  "No tenía copia",
  "No tenía copia",
  "No tenía copia","Al solicitar la planificación indicó que no tenía copia extra.",
  "No las tenía a la mano.",
  "No tenía copia a la mano",
  "No tenía copia y le sirven",
  "No tenía para si mismo",
  "No tiene copia",
  "No tiene copias",
  "No tenía copia",
  "No tenía copia.",
  "No tenía una copia",
  "No tiene una copia",
  "No tenía copia extra.",
  "No tenía copia extra.",
  "No tenía copia extra, se tomó fotografía.",
  "No tenía copia extra para entregar.",
  "Indicó que no tenía una copia extra.",
  "No tenía copia de su planificación.",
  "No tenía copia de las planificaciones, las tenía en cuaderno.", "No tenía copia",
  "No tenía copia", "No tenía copia",
  "No tenía copia","No tenía copia",
  "No tenía copia", "Solamente contaba con sus impresiones, en el establecimiento no hay impresora o fotocopiadora.",
  "La docente únicamente  cuenta con el libro planificador, no tiene acceso a impresora o fotocopiadora en el CE.",
  "Indicó que no las ha podido imprimir, ya que la impresora siempre está ocupada.",
  "Indicó que no había una fotocopiadora cerca",
  "No las puso imprimir",
  "No se pudo sacar copiar",
  "No se pudo sacar copia",
  "No se pudo sacar copias",
  "No se puso sacar copia",
  "No se pudo tener copia"
)

### solo tenia una copia/original
valores_solo_original <- c(
  "Solo tiene 1",
  "Solamente tiene las que utiliza",
  "La docente tiene copia de si misma.",
  "Solo contaba con la planificación original",
  "Porque solo tenia una copia para sí.",
  "Las planificaciones las tiene para si mismo.",
  "Solo contaba con el libro planificador de Telesecundaria",
  "Solo contaba con su libro planificador",
  "Porque solo esa tenía para su clase",
  "Solo tenía su planificación",
  "Solo cuenta con la original",
  "Solo tiene el original",
  "Solo tiene original",
  "Solo tenía la original",
  "Solamente tenía la original","Solo tiene una",
  "Solo tiene una copia","Les sirven y mañana me da una copia",
  "Solo tiene una","Sólo tenía un juego de cada area",
  "Solo tenía la personal",
  "Solo tiene la propia","Solo tenía para si misma",
  "Solo tenía para sí",
  "Solamente tiene la propia","Solo tenía la propia",
  "Solo tenía para si", "Solo tenía para si mismo",
  "Solamente tenía para si mismo",
  "Solo tenía para si mismo","Solo tiene la personal",
  "Solo tenía la planificación propia"
)

### para uso personal
valores_uso_personal <- c(
  "La planificación la tiene la docente para sí misma.",
  "Tiene una copia para si misma.",
  "La docente indicó que sus planificaciones las tiene para si misma.",
  "El docente tiene para si mismo sus planificaciones.",
  "El docente tiene su planificación para si",
  "El docente tiene pasa si mismo las planificaciones.",
  "El docente tiene sus planificaciones impresas para si mismo.",
  "Solamente tiene para si mismo","El docente presentó y prestó su planificación para el momento de la observación, las impresiones indicó que son para si mismo.",
  "La docente tiene para si mismo las planificaciones.",
  "La docente tiene sus planificaciones para si misma",
  "La docente tiene sus planificaciones para si mismo",
  "Las tiene para si y dosifica en su cuaderno.",
  "Son solo de uso personal",
  "Únicamente las tenia para si mismo.",
  "Únicamente tenía para si mismo.",
  "Unicamente las tiene para si mismo.",
  "El docente indicó que la planificación la tiene para si mismo.",
  "La docente tiene sus planificaciones para si misma.",
  "El docente indicó que tiene sus planificaciones para sí mismo.",
  "Las tiene para si misma.",
  "La docente tiene las planificaciones para si misma.",
  "El docente tiene sus planificaciones para si mismo",
  "El docente tiene las planificaciones para si mismo.",
  "Las tiene para si misma.",
  "El docente tiene su planificación para si mismo.",
  "El docente tiene una copia para el mismo.",
  "La docente tiene una planificación para si misma",
  "Las planificaciones las tiene para si misma.",
  "El docente las tiene para si mismo",
  "El docente las tiene para si mismo.",
  "El docente tiene su planificación para si mismo.",
  "La planificación está impresa, sin embargo el docente las tiene para si mismo.",
  "El docente tiene sus planificaciones impresas para el mismo.",
  "El docente tiene sus planificaciones para si mismo.",
  "La docente tiene sus planificaciones para si misma.",
  "El docente cuenta con una copia de sus planificaciones para si mismo",
  "La docente tiene sus planificaciones impresas para si misma.",
  "El docente tiene impresas sus planificaciones para si mismo.",
  "La docente tiene su planificación para si misma.",
  "El docente tiene para si mismo las planificaciones",
  "El docente tiene para si mismo sus planificaciones.",
  "La docente tiene planificaciones para si misma."
)

### resguardo en dirección
valores_resguardo_direccion <- c(
  "Indicó que las copias las entregan en Dirección.",
  "Indicó que las entregan en Dirección.",
  "No las tenía, indicó que en Dirección las tienen.",
  "Indico´que no tiene copia, que las entregan en Dirección.",
  "Al solicitarle, indicó que no tenía copia a la mano, las dan en Dirección.",
  "Al solicitarlas, indicó que las entregarían en Dirección.",
  "Al solicitarle, indicó que las entregan en Dirección.",
  "Indicó que la entregarán en Dirección.",
  "Las planificaciones las entregarán en Dirección.",
  "Las planificaciones las entregarán en Dirección.",
  "Indicó que las entregarían en Dirección, pero no las entregaron.",
  "Indicó que el Director las estará entregando.",
  "Se indicó que el Director estará entregando las planificaciones.",
  "En Dirección estarán entregando copia de las planificaciones.",
  "Las estarán entregando en Dirección.",
  "La Sra. Directora estará entregando copia de las planificaciones.",
  "La Sra. Directora entregó copia de planificaciones."
)

### tenia solo fotografía
valores_foto_plani <-c(
  "El docente consintió en que se le tomara fotografía a su planificación, mas no tenia otra copia para compartir.",
  "Solo se le toma una fotografía.",
  "No tenía otra copia, se tomó fotografía.",
  "No tenía copia extra, se tomó fotografía.",
  "Sólo dejó que se le tomará la foto.",
  "Únicamente se le toma la fotografía",
  "Sólo se le toma una fotografía","Únicamente tenía las del centro educativo, y se le tomó foto.",
  "Sólo se le tomaron fotografías.",
  "Sólo presentó planificación para tomarle fotos.",
  "Sólo presentó planificación para tomarle fotos.",
  "Sólo tenía su planificación, únicamente para tomarle fotos.",
  "Sólo tenía su planificación. Se le tomarán fotos .",
  "El docente indicó que las planificaciones las tiene para si y que se le tomen fotografías."
)

### utiliza el libro planificador
valores_libro_planificador <- c(
  "La docente utiliza un planificador de telesecundaria del MINEDUC, no contaba con copias",
  "Solamente contaba con el ejemplar del libro planificador de Telesecundaria",
  "Solo contaba con el libro planificador, se tomaron fotografias.",
  "Las tenía en un planificador, no tenía copia.",
  "Solamente contaba con su libro planificador",
  "Utiliza un libro planificador",
  "Solamente tenia la personal",
  "Solamente tiene la original",
  "Solo cuenta con la planificación original",
  "Solo tenía la original e indico que entregaría una copia el día siguiente",
  "Tiene solo una copia/original",
  "Porque utiliza la guía de aprendizaje, está trabajando en la página 222"

)

## Recode con case match ----

obs02 <- obs02 |>
  mutate(
    planificaciones_no_entrega_copias = case_match(
      planificaciones_no_entrega_copias,
      all_of(valores_libro_planificador) ~ "Utiliza un libro planificador",
      all_of(valores_foto_plani) ~ "Tiene solo fotografía",
      all_of(valores_resguardo_direccion) ~ "Dirección resguarda las planificaciones",
      all_of(valores_uso_personal) ~ "Son solo de uso personal",
      all_of(valores_sin_copia) ~ "No tiene copia",
      all_of(valores_plani_digital) ~ "Solo está en formato digital",
      all_of(valores_solo_original) ~ "Tiene solo una copia/original",
      .default = planificaciones_no_entrega_copias
      
    )
  )

## volver a ver errores

valores_planificaciones_no_entrega_copias <- obs02 |>
  filter(planificaciones_copia_impresa == "No") |>	
  filter(!(is.na(planificaciones_no_entrega_copias))) |>	
  select(planificaciones_no_entrega_copias)

#View(valores_planificaciones_no_entrega_copias)

# materiales ----

## ver errores

valores_materiales <- obs02 |>
  filter(!(is.na(materiales))) |>	
  select(materiales) 

dput(valores_materiales)

## Define los vectores ----

### pizzaron y marcadores
valores_pizarron <- c(
  "Pizarrón",
  "Pizarra","Únicamente el pizarrón",
  "Únicamente el pizarrón.",
  "Pizarrón, marcador.","Bolígrafo para calificar las pruebas, pizarrón y marcador.",
  "Cartel, pizarra, marcadores, hojas de trabajo",
  "Carteles, marcadores y pizarrón",
  "Carteles, masking tape, pizarrón, libro de texto.",
  "Carteles, pizarrón, hojas con el mapa conceptual.",
  "Celular, bocina. Pizarrón, marcador.",
  "Computadora,  marcador y pizarrón",
  "Computadora, pizarrón, marcadores.",
  "Cuaderno  y pizarra",
  "Cuaderno, pizarra y marcadores",
  "Cuadernos, lapicero, marcadores y pizarrón",
  "Cuadernos, pizarra y CNB.",
  "El pizarrón y marcadores.",
  "El pizarrón, marcadores, hojas de trabajo.",
  "El pizarrón.",
  "Enseñarle a las niñas en el pizarrón con se hace la conversion  de pulgadas a centímetros",
  "La pizarra, marcador.",
  "Lap top personal, pizarrón, marcadores.",
  "Pizarra y marcadores",
  "Pizarra, marcadores",
  "Pizarrón y marcador.",
  "Pizarrón y marcadores.",
  "Pizarrón, marcador",
  "Pizarrón, marcador negro y rojo.",
  "Pizarrón, marcadores de diferente color para marcar diferencia en conceptos y señalar aspectos importantes.",
  "Únicamente utilizó pizarrón y marcadores",
  "Pizarra y marcador",
  "Pizarrón y marcadores",
  "Pizarrón, marcadores.",
  "Pizarrón, marcador y libro",
  "Pizarrón, marcador, libro.",
  "Pizarrón, marcador, cuaderno de apuntes.",
  "Pizarrón, marcador, hoja de trabajo.",
  "Pizarrón, marcadores y hoja de trabajo",
  "Pizarrón, marcadores, hojas de trabajo.",
  "Pizarrón, marcadores, libro y hoja de trabajo",
  "Pizarrón, marcador y tabla de ajedrez.",
  "Pizarrón, marcador, celular (en donde estaba anotado el contenido).",
  "Pizarrón, marcadores y guía",
  "Pizarrón, marcadores y guía de los estudiantes",
  "Pizarrón, marcadores, regla, escuadra, libro.",
  "Pizarrón, regla, transportador, yeso para Pizarrón, libro de Guatemática, tablet.",
  "Únicamente el pizarrón",
  "Solo el pizarrón.","El pizarrón y marcadores",
  "Pizarrón y marcador",
  "Pizarrón.",
  "Pizarra y marcador.",
  "Marcador y pizarrón",
  "Cuaderno y pizarra",
  "Pizarrón únicamente.",
  "Pizarrón y tablet",
  "Pizarrón, yeso.","Pizarra.",
  "Pizarrón, yeso, hojas.", "Bolígrafo para calificar las pruebas, pizarrón y marcador.",
  "Cartel con sopa de letras, pizarrón, marcador, folleto de lectura.",
  "Cartel, pizarra, marcadores, hojas de trabajo",
  "Carteles, marcadores y pizarrón",
  "Carteles, masking tape, pizarrón, libro de texto.",
  "Carteles, pizarrón, hojas con el mapa conceptual.",
  "Computadora,  marcador y pizarrón",
  "Computadora, pizarrón, marcadores.",
  "Cuaderno  y pizarra",
  "Cuaderno, pizarra y marcadores",
  "Cuadernos, lapicero, marcadores y pizarrón",
  "Cuadernos, pizarra y CNB.",
  "El pizarrón y marcadores.",
  "El pizarrón.",
  "El pizarrón, marcadores, hojas de trabajo.",
  "Folleto y pizarrón",
  "Folleto, pizarrón, marcador.",
  "Fotocopias, pizarrón y marcadores",
  "La biblia, el pizarrón y marcador",
  "La pizarra, marcador.",
  "Lap top personal, pizarrón, marcadores.",
  "Libro de texto, pizarrón y marcador.",
  "Libro, marcador y pizarra.",
  "Libro, pizarrón y marcadores",
  "Marcador y pizarra",
  "Marcador y pizarrón.",
  "Pizarra y cuadernos",
  "Pizarra y marcadores",
  "Pizarra, cuaderno",
  "Pizarra, cuadernos",
  "Pizarra, cuadernos y marcadores",
  "Pizarra, marcador y libro",
  "Pizarra, marcadores",
  "Pizarrón y marcador.",
  "Pizarrón y marcadores.",
  "Pizarrón, marcador",
  "Pizarrón, marcador negro y rojo.",
  "Pizarrón, marcadores",
  "Solo pizarrón.",
  "Sólo el pizarrón",
  "Únicamente el Pizarron",
  "Únicamente la pizarra.", "El pizarrón, marcadores, hojas con la gerarquía de las organizaciones.",
  "En la primera parte utilizó el cuadro de notas.  \nEn la segunda parte utilizó pizarrón, marcador, libro.",
  "La guía de contenidos, pizarrón y marcadores",
  "La guía de los estudiantes, pizarrón y marcadores",
  "Pizarra y marcador, teléfono celular donde leyó las definiciones de los conceptos del día",
  "Pizarra y marcadores",
  "Pizarra y resumen realizado por ella",
  "Pizarra, cuaderno y libros",
  "Pizarra, cuadernos y hoja de trabajo",
  "Pizarra, hoja de trabajo",
  "Pizarra, libro",
  "Pizarrón y folleto",
  "Pizarrón y hojas de trabajo",
  "Pizarrón y libro",
  "Solo utilizo el  pizarrón.", "Imágenes, cuadernos, pizarra y marcadores",
  "Marcador y pizarra.",
  "Marcador, pizarra y hojas de trabajo.",
  "Marcador, pizarra y libro.",
  "Marcador, pizarra y material de apoyo.",
  "Marcador, pizarra y transportador.",
  "Marcador, pizarrón, libro de texto.",
  "Marcador.","Pizarrón, mapa de San Lucas Toliman, yeso para pizarrón.",
  "Marcadores, pizarrón, regla y libro.",
  "Pizarra y marcadores","Pizarra y marcadores",
  "Pizarra, cuadernos, crayones",
  "Pizarra, cuadernos, marcadores y crayones",
  "Pizarra, libro de matemáticas",
  "Pizarra, marcador y flauta.",
  "Pizarra, marcador y libro de matemática",
  "Pizarra, marcador, cuaderno de apuntes.",
  "Pizarra, marcadores de colores, catapulta realizada con ligas, gancho de ropa y cuchara o tenedor.",
  "Pizarra, marcadores de varios colores, documento impreso para uso propio con el tema del día.",
  "Pizarra, marcadores y cuadernos",
  "Pizarra, marcadores y cuadernos.",
  "Pizarra, marcadores y teléfono celular donde leyó y copió la biografía",
  "Pizarra, marcadores y una hoja de trabajo.",
  "Pizarra, marcadores, libros de docente y estudiantes.",
  "Pizarrón y marcador, aunque no hubo desarrollo de una clase propiamente.",
  "Pizarrón y marcador, cuadro de notas.",
  "Pizarrón y marcador, fichas de papel.",
  "Pizarrón y marcador, libro de texto.",
  "Pizarrón y marcadores. Sello y almohadilla.",
  "Pizarrón y marcadores. Tarjetas con oraciones.",
  "Pizarrón y una hoja de trabajo.",
  "Pizarrón, cartulinas y marcadores",
  "Pizarrón, marcador y libros",
  "Pizarrón, marcador, calculadora.",
  "Pizarrón, marcador, cronograma de actividades, hoja de contenidos.",
  "Pizarrón, marcador, cuadro de notas, bolígrafo.",
  "Pizarrón, marcador, fragmento de noticia.",
  "Pizarrón, marcador, hoja con información.",
  "Pizarrón, marcador, hoja de contenido.",
  "Pizarrón, marcador, libro",
  "Pizarrón, marcador, libro de cuentos.",
  "Pizarrón, marcador, libro de texto.",
  "Pizarrón, marcador, libro, regla.",
  "Pizarrón, marcador, papel, celular.",
  "Pizarrón, marcadores e imágenes",
  "Pizarrón, marcadores y contenido",
  "Pizarrón, marcadores y flauta",
  "Pizarrón, marcadores y guía metodológica del area",
  "Pizarrón, marcadores y hojas de trabajo.",
  "Pizarrón, marcadores y la guía de los estudiantes",
  "Pizarrón, marcadores y los contenidos",
  "Pizarrón, marcadores y notas",
  "Pizarrón, marcadores y teléfono",
  "Pizarrón, marcadores y álgebra de Baldor",
  "Pizarrón, marcadores, almohadilla.",
  "Pizarrón, marcadores, hoja de contenido para el dictado, cuadro de notas.",
  "Pizarrón, marcadores, hojas de definiciones.",
  "Pizarrón, marcadores, hojas de sopa de letra.",
  "Pizarrón, marcadores, hojas de trabajo sobre la argumentación.",
  "Pizarrón, marcadores, hojas.",
  "Pizzaron, marcadores y cuadernos"
)

### libros
valores_material_libros <- c(
  "Libro","Pizarrón, libro de lectura de NUFED del MINEDUC.",
  "Pizarrón, libro del grado.",
  "Pizarrón, libro que se llama mam como l2 y cuaderno",
  "Recursos audiovisuales y libro",
  "Libros",
  "Libro de texto",
  "Libro de Santillana, celular.",
  "Libro Español 3, Santillana",
  "Libro de matemáticas de tercero básico",
  "Libro de Guatemática",
  "Libro de ciencias sociales",
  "Libro del popul wuj, hojas de colores.",
  "Libro Matemática Progresiva 3",
  "Libro de lectura",
  "Libro Zaculeu","Algebra de baldor",
  "Biblia", "Cuadernos , libros",
  "Libro.","Comunicación y Lenguaje L1, Editora Delmmi",
  "Diccionario",
  "El libro.",
  "El libro de matemáticas  de segundo básico",
  "Libro Ciencias Sociales, SendaSociales",
  "Libro Comunicación y Lenguaje. SendaExpresiva",
  "Libro Contabilidad General de tercero básico",
  "Libro Matemática 3 grado MINEDUC 2019",
  "Libro de matemáticas de segundo grado",
  "Libro de texto",
  "Libro de texto y pizarrón",
  "Libro del popol wuj, hojas de colores.",
  "Libros",
  "Libros de telesecundaria",
  "Libros del mineduc",
  "Libros y cuadernos",
  "Libro santillana",
  "Libro susaeta","Libros",
  "Libro de matemáticas",
  "Libro de texto.",
  "Libro de inglés Side by Side",
  "Libro del docente.",
  "Libro de telesecundaria.",
  "Libros de telesecundaria.",
  "Libro de comunicación y lenguaje",
  "Libro de ciencias naturales",
  "Libro del MINEDUC",
  "Libro de artes plásticas de segundo básico de Indegua S.A.",
  "Comunicación y Lenguaje L1, Editora Delmmi",
  "Copias del libro principito, pizarrón, marcadores.",
  "El libro.",
  "El libro Live Escalate Ascend, pizarra y marcador.",
  "El libro de comunicación y lenguaje NUFED.",
  "El libro de matemáticas  de segundo básico",
  "El libro de texto de ciencias sociales",
  "El álgebra  de valdor",
  "Libro Ciencias Sociales, SendaSociales",
  "Libro Comunicación y Lenguaje. SendaExpresiva",
  "Libro Contabilidad General de tercero básico",
  "Libro Matemática 3 grado MINEDUC 2019",
  "Libro Proyectos,, productividad y desarrollo 8",
  "Libro aprenda inglés de primer año lic. Francisco Abel Matul",
  "Libro de Ciencias naturales de segundo básico de Norma",
  "Libro de Santillana, marcador y pizarra",
  "Libro de matemáticas de tercero básico.",
  "Libro de santillana",
  "Libro de susaeta de matemáticas de segundo básico",
  "Libro de texto para dictar el contenido.",
  "Libro del popul wuj, hojas de colores.",
  "Libro digital, tablet, pizarrón.",
  "Libros","Dado, libro.",
  "Fotocopias de un libro de física fundamental editorial Zaculeu",
  "La docente utiliza el texto Taller de Lectura No. 1 de José Roderico Tello.",
  "Libro Español 3, Santillana y libro Poema de Mío Cid",
  "Libro Matemática Progresiva 3 y Matemáticas 3 grado del MINEDUC 2019",
  "Libro Nivel Básico Integrado 9 Comunicación y Lenguaje/Matemáticas, susaeta.",
  "Libro con el contenido del día",
  "Libro de Achi.",
  "Libro de Ciencias sociales de norma",
  "Libro de Ciencias sociales de segundo básico",
  "Libro de Guatemática, hojas de trabajo.",
  "Libro de IGER.",
  "Libro de Santillana. Google.",
  "Libro de comunicación y lenguaje de susaeta",
  "Libro de educación para el hogar de tercero básico, educación para la vida productiva",
  "Libro de elaboración propia y pizarrón",
  "Libro de idioma español inegua s.a.",
  "Libro de inglés de  Editorial Educativa de segundo básico",
  "Libro de la academia de las lenguas mayas",
  "Libro de mam",
  "Libro de matemática de tercero básico.",
  "Libro de matemáticas 9 grado",
  "Libro de matemáticas de segundo básico del autor. Mario S. Fernández",
  "Libro de matemáticas de tercero básico, pizarrón y marcadores",
  "Libro de matemáticas en la página 48",
  "Libro de productividad y desarrollo de segundo básico",
  "Libro de segundo básico de Editora Educativa",
  "Libro de segundo básico de Indegua S.A.",
  "Libro de susaeta de ciencias  sociales",
  "Libro de tercero básico del Ministerio de Educación.",
  "Libro de texto, de donde dictó los contenidos.",
  "Libro de texto, pizarrón, marcador.",
  "Libro de valdor",
  "Libro del docente y el estudiante.",
  "Libro del segundo básico del mimeduc",
  "Libro el principito",
  "Libro en digital, lap top, pizarrón.",
  "Libro guía",
  "Libro true Colors y diccionario español-inglés",
  "Libros",
  "Libros.",
  "Los libros de telesecundaria.",
  "Un libro de -IGER- Zaculeu",
  "Libros de texto",
  "Libros del mineduc",
  "Libros y cuadernos","Libros y pizarrón.","Li ro de física de Odraude Sogeirazam",
  "Libro y bocina para cantar",
  "Libro y cuadernos",
  "Libro y marcador.",
  "Libro y material de apoyo.",
  "Libro y pizarra",
  "Libro, CNB y pizarra.",
  "Libro, Proyectos. Productividad y Desarrollo",
  "Libro, cuaderno, televisión, pizarra",
  "Libro, hojas de trabajo para la niña con NEE, el pizarrón y marcadores.",
  "Libro, hojas en blanco y de colores",
  "Libro, hojas.",
  "Libro, marcadores, pizarrón.",
  "Libro, piedras de diferentes características, dado.",
  "Libro, pizarra y marcador.",
  "Libro, pizarrón, marcador.",
  "Libros","Libros New English de Editora Educativa y libro de English as a Fureing language pre-intermeate 2",
  "Libros de IGER. Pizarrón y marcadores.",
  "Libros de susaeta, pearson de física, indegua s.a.",
  "Libros de telesecundaria y el pizarrón.",
  "Libros del mineduc, cuaderno guia y su planificación",
  "Libros que según el docente los ha actualizado. Le sirven de guia.",
  "Libros y planificador de telesecundaria del MINEDUC. También los libros utilizados por los estudiantes (proporcionados por el MINEDUC).",
  "Libros, cartulinas y marcadores",
  "Libros, cuadernos y lapiceros",
  "Libros, pizarra y marcadores",
  "Libros, pizarrón, celular y marcadores.",
  "Marcador y el libro de texto",
  "Un libro de ciclo básico que se llama ciencias sociales y ciudadanía 9 de edición Julio César Barillas, el pizarrón y marcadores.",
  "Un libro de texto, de donde dictó los contenidos.",
  "Utiliza el libro de tercero básico del MINEDUC y un dado elaborado con caja de cartón.",
  "Utilizo el libro de Ciencias Naturales de INDEGUA S.A.",
  "Utilizó el libro de Conceptos Básicos de Telesecundaria. Área Académica del MINEDUC del año 2012.",
  "Utilizó el libro de emprendimiento.",
  "Vocabulario  de neologismos pedagógicos  de DIGEBI, Tzijob'elil k'aslenal, eternamente xuquje' komon taq chak",
  "Álgebra de Baldor, pizarrón y marcadores"
)

### guias y folletos
valores_material_impresos <- c(
  "Hojas de trabajo",
  "Hoja de trabajo","Fotocopia con información relacionada al tema.",
  "Hojas de trabajo.","Guía de aprendizaje, pizarrón, teléfono para buscar más información sobre el tema",
  "Hoja con vocabulario","El tiene su propio folleto de artes visuales",
  "La guía de los estudiantes",
  "La guía de trabajo de los estudiantes",
  "La guía del alumno, unidad tres, sección 2.",
  "La guía del estudiante, pizarrón y marcadores",
  "La guía, pizarrón, marcadores",
  "Hoja de evaluación final",
  "Hoja de la evaluación de los Aprendizajes.",
  "Hoja de los cuentos",
  "Hoja de sopa de letra, pizarrón, marcadores.",
  "Hoja de trabajo digital",
  "Hoja de trabajo, cuadernos y lapiz",
  "Hoja de trabajo, libro Santillana de secundaria y marcador",
  "Hoja de trabajo, libro de segundo básico",
  "Hoja de trabajo, ruleta",
  "Hoja impresa de vegetales sus características y clasificación",
  "Hoja o dibujo que debían llevar los estudiantes conteniendo la ventana o entorno de excel",
  "Hoja que contiene conversacion en inglés y la tradución, recorte que contiene oraciones para completar con el verbo has y have.",
  "Hojas  media carta, marcadores, pegamento",
  "Hojas 120 gramos, papel construcción, goma, tijeras, pizarra, marcadores",
  "Hojas bond, regla, lápiz, marcadores y pizarra",
  "Hojas bond, reglas y lapiz",
  "Hojas con elementos tecnológicos.",
  "Hojas con imágenes impresas, para mostrar a los estudiantes.",
  "Hojas con los acrósticos",
  "Hojas con líneas, cuadernos",
  "Hojas de colores, hojas bond, recortes, goma y tijeras",
  "Hojas de contenido",
  "Hojas de contenidos.",
  "Hojas de investigación que la docente prepara para su clase.",
  "Hojas de investigación, cuaderno.",
  "Hojas de lectura.",
  "Hojas de magnitudes, pizarrón, marcadores.",
  "Hojas de papel bond en cada grupo de trabajo.",
  "Hojas de papel con conceptos claves e imagenes",
  "Hojas de papel con la definición de cada palabra a a tratar en el día",
  "Hojas de papel de colores",
  "Hojas de trabajo con las profesiones en inglés",
  "Hojas de trabajo impresas, pizarrón, marcadores, libro de matemáticas de tercero del MINEDUC",
  "Hojas de trabajo para exposiciones",
  "Hojas de trabajo y marcadores",
  "Hojas de trabajo, marcador y pizarra",
  "Hojas de trabajo, marcador y pizarra.",
  "Hojas impresas (copias) para las evaluaciones finales",
  "Hojas impresas con dichos, refranes y significado de los mismos",
  "Hojas impresas con las notas musicales",
  "Hojas impresas y hojas escritas a mano por El",
  "Imprimió unas hojas de la guía de se básico",
  "Una fotocopia de la hoja de actividades",
  "Una guía que ella tiene impresa",
  "Una hoja con contenido .",
  "Una hoja con contenido.",
  "Una hoja de cronograma de actividades para el desarrollo del bimestre",
  "Una hoja de trabajo con cuatro dibujos que los estudiantes debían observar para responder una serie de preguntas.",
  "Una hoja de trabajo que es la síntesis del Decreto 42-2001",
  "Una hoja de trabajo, libro, pizarrón, marcador.",
  "Una hoja donde aparecen las definiciones.",
  "Una hoja media carta que contiene serie de ejercicios para resolver conversaciones de fracciones a decimales",
  "Únicamente un folleto",
  "Hojas impresas del contenido",
  "Una hoja de trabajo",
  "Una fotocopia de un libro.",
  "Hojas","Guía de trabajo",
  "Guía de aprendizaje de Ciencias Naturales de tercero básico del año 2019",
  "Folleto de inglés.",
  "Hojas impresas",
  "Hojas impresas con los ismos.",
  "Hojas impresas de evaluación final",
  "Hojas impresas para la evaluación final",
  "Guía de aprendizaje",
  "Guía de los estudiantes",
  "Guía metodológica del area",
  "Folleto","Copia del Decreto 42-2001",
  "Copia del cronograma de actividades del área de música",
  "Copiar del libro de Ciencias  naturales  guías metódicas",
  "Copias de los pasos de la investigación.",
  "Copias, marcadores fosforescente.",
  "Folleto, pizarrón",
  "Copias de trabajo",
  "Documento de lectura, pizarrón, marcador.",
  "Una hoja de trabajo.",
  "Una hoja impresa",
  "Material impreso","Caja, cartel con cuadros, folleto",
  "Cartel con imagen y recorte de una imagen, hoja de trabajo, hoja con la lectura.",
  "Copias de una leyenda.",
  "Copias entregadas a los estudiantes",
  "Fotocopias, pizarrón y marcadores",
  "Hoja de trabajo.",
  "Hoja impresa de trabajo que deben copiar en word",
  "Hoja impresa de un presupuesto familiar",
  "Hoja impresa para evaluación final",
  "Hojas  impresas con la sopa de letras sobre valores",
  "Hojas de evaluación final",
  "Hojas de lectura",
  "Hojas de trabajo revisadas y cuaderno",
  "Hojas impresas (copias) de evaluación final",
  "Hojas impresas con la lectura del día.",
  "Hojas impresas y lápiceros",
  "Hojas.","Ficha que el docente preparó, le sirve como guía para dictarle a los alumnos los ejercicios.",
  "Fichas que elabora para el tema del día, libros de inglés.",
  "Folleto de apoyo sobre los derechos y obligaciones de las personas.",
  "Material impreso",
  "Material impreso", "Apuntes de su cuaderno.",
  "Cartel con imagen y recorte de una imagen, hoja de trabajo, hoja con la lectura.",
  "Copias entregadas a los estudiantes",
  "Copias de una leyenda.",
  "Cuadernos y folletos de lectura",
  "Folleto de derechos y obligaciones.",
  "Guía de aprendizaje de comunicación y lenguaje",
  "Hoja de evaluación.",
  "Hoja de trabajo.",
  "Hoja de trabajo, computadora",
  "Hoja impresa de trabajo que deben copiar en word",
  "Hojas de lectura",
  "Hojas de trabajo y lápices",
  "Hojas impresas (copias) de evaluación final",
  "Una hoja de trabajo que entregó a los estudiantes.",
  "Una hoja impresa con el mapa de Guatemala",
  "Únicamente hojas."
)

### equipo tecnológico
valores_material_tecnologia <- c(
  "Computadora",
  "Computadoras",
  "Laptop","Cañonera y pamtalla.",
  "Cañonera, computadora y cuadernos",
  "Cañonera, computadoras, Internet.",
  "Cañonera, lap top, pizarrón.",
  "Cañonera.","Cañonera, lap top, pizarrón, hojas de papel bond.",
  "Computadora y pantalla para proyectar.",
  "Computadora, pantalla e internet",
  "Computadora, pantalla digital.",
  "Dispositivos tecnológicos",
  "Equipo tecnológico",
  "La computadora",
  "Laptop,  proyector, pantalla , pizarra, marcador.",
  "Pantalla, computadora, pizarrón y marcadores",
  "Proyector, laptop, pantalla (pizarrón).",
  "Tablet, pantalla, pizarrón, marcadores rojo y negro, para diferenciar los procesos.",
  "Una laptop",
  "Una tablet que contenía el contenido a desarrollar.",
  "Celular, bocina, pizarrón, marcador.",
  "Computadora.",
  "Computadoras.",
  "Computadoras portátiles",
  "Pantalla, computadora.",
  "Laptop, proyector, pantalla.",
  "Tablet personal",
  "Su teléfono celular",
  "Su computadora",
  "Utilizó computadora.",
  "Recursos tecnológicos.",
  "Equipo tecnológico","Pantalla",
  "Proyector","Cañonera",
  "Tablet", "Celular", "Internet",
  "Computadora, pantalla",
  "Laptop, proyector, pantalla",
  "Computadora y pantalla",
  "Pantalla y computadora",
  "Tablet, cañonera, pizarrón.",
  "Video tutorial, celular y pantalla",
  "Aplicación de kahoot.it",
  "YouTube","Audio con una bocina.",
  "Bocina\nPito \nPelotas de fútbol\nConos",
  "Bocina inalambrica y teléfono celular",
  "Bocina para reproducir audio",
  "Bocina recargable y teléfono celular",
  "Bocina y celular","Bocina y música",
  "Bocina y teléfono", "Bocina, celular",
  "Bocina, celular y micrófono",
  "Bocina, celular y micrófono.",
  "Bocinas pequeñas, celular.",
  "Recursos tecnológicos",
  "Recurso tecnológico",
  "Recursos Tecnológicos", "Cañonera, lap top, pizarrón, hojas de papel bond.",
  "Cañonera, laptop y guía de los estudiantes",
  "Celular, bocina. Pizarrón, marcador.",
  "Computadora y marcador",
  "Computadora y pantalla para proyectar.",
  "Computadora, cañonera",
  "Computadora, pantalla digital.",
  "Dispositivos tecnológicos",
  "El celular de los estudiantes",
  "Internet, para que los niños buscarán el concepto, características, ventajas y desventajas de los presentadores multimedia",
  "La computadora",
  "Laptop, proyector, pantalla (pizarra).",
  "Pantalla, computadora y la demostración",
  "Tablet, libro de Guatemática.",
  "Teléfono", "Cañonera, laptop, hojas impresas, papel de colores, video",
  "Computadora,  lapicero",
  "Computadora,  lapicero, marcadores",
  "Computadora, bocina y cañonera.",
  "Computadora, bocina, proyector, micrófono",
  "Computadora, cañonera.",
  "Computadora, dibujo y contenido",
  "Computadora, lapicero",
  "Computadora, marcador",
  "Computadora, marcadores y lapicero",
  "Computadora, pantalla para proyectar",
  "Computadora, pantalla, lista de cotejo",
  "Computadora, pantalla, pizarrón",
  "Computadora, proyector, pantalla",
  "Computadoras y cañonera",
  "Computadoras y el libro de texto TAC I",
  "Computadoras, hoja de trabajo",
  "Diapositivas, organizador gráfico",
  "El en el celular tiene sus folletos de inglés, pero solo son hojas,no tiene libros",
  "En su computadora tiene el contenido",
  "Equipo tecnológico",
  "Hojas de coevaluación, USB y bocinas",
  "La computadora y sus conectores",
  "Laptop, bocinas, para proyectar el audio. Pizarrón, marcador.",
  "Laptop, cuadro de notas.",
  "Laptop, en donde consultó el cronograma de actividades. Pizarrón, marcador.",
  "Laptop, pantalla para proyectar, hoja de trabajo.",
  "Laptop, pantalla para proyectar, pizarrón, marcador, hojas de trabajo.",
  "Laptop, pantalla, pizarra, marcador, el libro de La Constitución de la República.",
  "Laptop, pizarrón, proyector",
  "Laptop, pizarrón, proyector, pantalla.",
  "Laptop, proyector, pantalla (pizarra)",
  "Laptop, proyector, pantalla (pizarra), marcador.",
  "Laptop, proyector, pantalla, bocinas del proyector (para música de fondo mientras trabajan)",
  "Pantalla y computadora. Los conectores de las computadoras",
  "Pantalla, celular, libro, pizarrón y marcadores",
  "Pantalla, celular, pizarrón y marcadores",
  "Pantalla, diapositivas",
  "Proyector, pantalla, laptop",
  "Proyector, pantalla, pizarra, marcador.",
  "Recurso tecnologicico",
  "Recurso tecnológico, computadora.",
  "Recursos audiovisual y libro",
  "Recursos tecnológico.",
  "Recursos tecnológicos y cuaderno",
  "Recursos tecnológicos, Ya que utilizo su computadora.",
  "Recursos tecnológicos, computadora",
  "Recursos tecnológicos, folletos.",
  "Televisión, audio",
  "Televisión, hojas doble oficio, hojas de trabajo",
  "Tecnológicos trabajaron los alumnos.", "Cuaderno y televisión",
  "Equipo tecnológico",
  "Los documentos los tenía  en la computadora",
  "Medios audiovisuales (televisión), libro, cuaderno, caja de preguntas.",
  "No utilizo, más que los recursos tecnológicos que tienen en el aula, siendo éstas 16 computadoras.",
  "Pizarra, cuaderno, cables",
  "Pizarra, hojas, cuaderno y televisión, materiales impresos",
  "Pizarra, televisión",
  "Pizarrón y recursos tecnológicos.",
  "Pizarrón y teléfono",
  "Pizarrón, cañonera",
  "Pizarrón, computadora, pantalla",
  "Pizarrón, computadora.",
  "Pizarrón, folleto, computadoras.",
  "Pizarrón, laptop y marcadores",
  "Pizarrón, marcador (las computadoras las utilizaron los estudiantes).",
  "Pizarrón, marcador, celular para consultar el contenido.",
  "Pizarrón, marcador, laptop en donde se encontraba el contenido",
  "Pizarrón, marcador, laptop.",
  "Pizarrón, marcadores, laptop.",
  "Pizarrón, marcadores, pantalla y computadora",
  "Pizarrón, pantalla , computadora.",
  "Pizarrón, proyector, laptop, pantalla",
  "Pizarrón, televisión",
  "Su celular para dictar las definiciones.",
  "Su celular para explicar las proyectos productivos.",
  "Su celular para poner la canción del saludo en español",
  "Su cuaderno, el CNB en el teléfono",
  "Su teléfono celular para leer unas palabras de agradecimiento",
  "Teléfono, pizarrón"
)
 
### cuaderno y utiles escolares
valores_material_utiles <- c(
  "Cuaderno", "Cuadernos , libros",
  "Cuadernos","Apuntes de su cuaderno.",
  "Cuaderno de tareas anteriores",
  "Cuaderno y lapiceros","Cuadernos, legos, manual y set de construcción 9686",
  "Cuadernos, pizarra y flauta",
  "Cuaderno y útiles escolares",
  "Cuaderno y pizarra","Cuaderno de la docente, hojas y recorte.",
  "Cuaderno y guía de los estudiantes",
  "Cuaderno y útiles escolares",
  "Cuaderno, marcadores y creyones",
  "Cuadernos y lapiceros",
  "Cuadernos, pizarra y marcadores",
  "Cuaderno de notas",
  "Lapicero","El docente solo observó las presentaciones y anotó lo observado en un cuaderno",
  "Lápiz","Sello y lapicero",
  "Solo su propio cuaderno donde tiene todos los contenidos",
  "Su cuaderno, donde tiene escrito todo lo que va hacer en la clase",
  "Tiene su cuaderno guía",
  "Marcador", "Solamente marcadores",
  "Marcadores",  "Cuaderno de tareas anteriores",
  "Cuaderno y lapiceros",
  "Cuaderno y útiles escolares",
  "Cuadernos, Lapiceros",
  "Cuadernos, diálogos",
  "Lápicero y cuadro de notas",
  "Lapicero para calificar.", "Imágenes, cuadernos, pizarra y marcadores",
  "Marcador y pizarra.",
  "Marcador, pizarra y hojas de trabajo.",
  "Marcador, pizarra y libro.",
  "Marcador, pizarra y material de apoyo.",
  "Marcador, pizarra y transportador.",
  "Marcador, pizarrón, libro de texto.",
  "Marcador.",
  "Marcadores, pizarrón, regla y libro.",
  "Pizarra y marcadores",
  "Pizarra, cuadernos, crayones",
  "Pizarra, cuadernos, marcadores y crayones",
  "Pizarra, libro de matemáticas",
  "Pizarra, marcador y flauta.",
  "Pizarra, marcador y libro de matemática",
  "Pizarra, marcador, cuaderno de apuntes.",
  "Pizarra, marcadores de colores, catapulta realizada con ligas, gancho de ropa y cuchara o tenedor.",
  "Pizarra, marcadores de varios colores, documento impreso para uso propio con el tema del día.",
  "Pizarra, marcadores y cuadernos",
  "Pizarra, marcadores y cuadernos.",
  "Pizarra, marcadores y teléfono celular donde leyó y copió la biografía",
  "Pizarra, marcadores y una hoja de trabajo.",
  "Pizarra, marcadores, libros de docente y estudiantes.",
  "Pizarrón y marcador, aunque no hubo desarrollo de una clase propiamente.",
  "Pizarrón y marcador, cuadro de notas.",
  "Pizarrón y marcador, fichas de papel.",
  "Pizarrón y marcador, libro de texto.",
  "Pizarrón y marcadores. Sello y almohadilla.",
  "Pizarrón y marcadores. Tarjetas con oraciones.",
  "Pizarrón y una hoja de trabajo.",
  "Pizarrón, cartulinas y marcadores",
  "Pizarrón, marcador y libros",
  "Pizarrón, marcador, calculadora.",
  "Pizarrón, marcador, cronograma de actividades, hoja de contenidos.",
  "Pizarrón, marcador, cuadro de notas, bolígrafo.",
  "Pizarrón, marcador, fragmento de noticia.",
  "Pizarrón, marcador, hoja con información.",
  "Pizarrón, marcador, hoja de contenido.",
  "Pizarrón, marcador, libro",
  "Pizarrón, marcador, libro de cuentos.",
  "Pizarrón, marcador, libro de texto.",
  "Pizarrón, marcador, libro, regla.",
  "Pizarrón, marcador, papel, celular.",
  "Pizarrón, marcadores e imágenes",
  "Pizarrón, marcadores y contenido",
  "Pizarrón, marcadores y flauta",
  "Pizarrón, marcadores y guía metodológica del area",
  "Pizarrón, marcadores y hojas de trabajo.",
  "Pizarrón, marcadores y la guía de los estudiantes",
  "Pizarrón, marcadores y los contenidos",
  "Pizarrón, marcadores y notas",
  "Pizarrón, marcadores y teléfono",
  "Pizarrón, marcadores y álgebra de Baldor",
  "Pizarrón, marcadores, almohadilla.",
  "Pizarrón, marcadores, hoja de contenido para el dictado, cuadro de notas.",
  "Pizarrón, marcadores, hojas de definiciones.",
  "Pizarrón, marcadores, hojas de sopa de letra.",
  "Pizarrón, marcadores, hojas de trabajo sobre la argumentación.",
  "Pizarrón, marcadores, hojas.",
  "Pizzaron, marcadores y cuadernos"
)

### material didactivo
valores_material_didactico <- c(
  "Carteles","Carteles.",
  "Cartulinas, marcadores, tijeras, pegamento e imágenes",
  "Tarjetas de colores","Cajas de cartón, tijeras, pegamento, papel reciclado",
  "Cartones de lotería hechos por el docente",
  "Material Didáctico","Mapamundi","Tabla periódica de elementos, carteles, masking tape, pizarrón y marcadores, cuadro de notas.",
  "Tablas de madera, tachuelitas, lana, marcador, regla y hules.",
  "Tarjetas para el rompecabezas, las computadoras.",
  "Utilizó cartas con distintas figuras identificadas con el nombre esscrito en q'eqchi'",
  "Material didáctico","Carteles, maskin  tape.",
  "Carteles, maskin tape.","Escobas y recogedor de basura",
  "Investigaciones que realizó el docente para el desarrollo de la clase.",
  "La docente entregó fotocopias a los alumnos para que desarrollen su aprendizaje en inglés.",
  "La docente se auxilió de la investigación que los alumnos realizaron.",
  "La docente utiliza material de apoyo con información que desarrolla en el aula.",
  "La docente utiliza material investigado por ella para desarrollar su clase.",
  "Material didáctica proporcionado por Ministerio de Educación",
  "Material didáctico",
  "Material investigado por la docente y libro de Ciencias Sociales y Ciudadania.",
  "Material investigado por la docente.",
  "Material investigado por la docente.n",
  "Material de investigación como apoyo al tema, miel, ajo, cuchilla, brocha, hoja de sábila, botes reciclados, medidor para la miel, rosales.",
  "Se basa del Internet y de una guía para docente que tiene por parte de los libros que usan los estudiantes, que son de Norma",
  "Utilizó un trabajo investigado por un alumno.",
  "Carteles, recortes de letra, marcadores, pegamento y tijeras.",
  "Carteles, recortes, pegamento y tijeras.",
  "Carteles, rompecabezas, pizarra, marcadores y hojas de trabajo",
  "Carteles, sopa de letra, hojas de definiciones.",
  "Cartones de lotería de los animales en idioma xinka",
  "Cartulinas, folleto",
  "Material didáctico (una hoja impresa)",
  "Material didáctico: Hoja de trabajo.",
  "Tarjetas con imágenes, hoja de trabajo.",
  "Tangram, hojas de ejercicios.",
  "Una bolsita con diferentes figuras de la fauna de Guatemala",
  "Infografía impresa.",
  "Material didáctico",
  "Material didáctico como cartel.",
  "Láminas, pizarrón, marcadores y hojas de contenido",
  "Cartulinas",
  "Imágenes impresas",
  "Rompecabezas",
  "Tarjetas",
  "Flash cards",
  "Tangram","Reproductor, hoja con un poema en achi' y planchas de cartón que tienen el himno nacional de Guatemala en achi'.",
  "Dado",
  "Maquetas",
  "Material didáctico",
  "Material de apoyo",
  "Material investigado por la docente","Botellas plásticas de 3 litros rellenas con arena.",
  "Cajas de cartón, tijeras, pegamento, papel reciclado",
  "Material Didáctico",
  "Material de apoyo que refieren al tema.",
  "Tangram, hojas de ejercicios.",
  "Tarjetas con imágenes, hoja de trabajo."
)

### material de educación fisica
valores_material_deportes <- c(
  "Balón", "Balones", "Pelotas de voleibol",
  "Net de voleibol",
  "Conos","Cinta métrica.",
  "Conos de plástico con los que marcó ciertos puntos en el área para la realización de los ejercicios.",
  "En la parte teórica cañonera y en la práctica: pelotas de voleibol, net o malla, conos y un pito.",
  "Evaluaciones físicas, lapiceros, lápiz, borrador",
  "Evaluación física","Prueba física",
  "Escaleras","Cancha polideportiva",
  "Gorgorito","Pito", "Balones de voleibol",
  "Balones de voleibol, Net, conos, inflados de balón.",
  "Balones y elementos de deporte",
  "Balón de baloncesto y conos",
  "Balón de mano.","Balón, conos.",
  "Balón, recurso humano.","Botes con arena\nPelotas medicinales \nPelotas de futbol",
  "Cancha polideportiva, como espacio de trabajo, net de voleibol, pelotas.",
  "Conos pequeños, cuerdas",
  "Conos y gorgorito",
  "Conos y pelotas de fut bol",
  "Conos, balones.",
  "Conos, escaleras.",
  "Conos, gorgorito y balones",
  "Conos, gorgorito, hoja de asistencia.",
  "Conos, lazo y balones de voleibol",
  "Conos, pelota","Balones y elementos de deporte",
  "Gorgorito, bocina, música",
  "Cuerda, gorgorito, pelotas de voleibol",
  "Gorgorito y balones de futbol",
  "Gorgorito y balón de voleibol",
  "Guantes de boxeo y protección de boxeo",
  "Net de voleibol, pelota, conos",
  "Net y pelotas de voleibol",
  "Net, balón,  cancha polideportiva.",
  "Pelota de baloncesto, escalera, conos, pelotas de plástico pequeñas",
  "Pelota, escaleras, etc.",
  "Pelotas de fútbol, reloj y gorgorito",
  "Pelotas de vóleibol, aros",
  "Pelotas, net de voleibol.",
  "Recursos deportivos como conitos de colores."
)

### instrumentos musicales
valores_material_instrumento <- c(
  "Flauta","Instrumentos musicales",
  "Televisión, hojas, marimba, ukeleles",
  "Guitarra",
  "Marimba", "Melódicas y cuadernos",
  "Bocina", "Flauta y guitarra",
  "Flauta y libro","Pentagramas de la canción carnaval y del himno a la alegría impresas en hojas.",
  "Audio","Teclado, aplicación de piano y pizarrón",
  "Utilizo un teclado digital para enseñar las notas musicales",
  "Teclado","Sonido",
  "flauta","Piano","Melódicas",
  "Pentagramas impresos"
)

###  evaluacion y registro
valores_material_evaluacion <- c(
  "Prueba escrita","Cuadros de control de calificaciones",
  "Cuadros de registro y cuadro de nota por unidad",
  "Prueba diagnóstica","Lapicero y su cuadro de notas",
  "Evaluación","Evaluaciones","Gorgorito, cuaderno de asistencia.",
  "Evaluación diagnóstica",
  "Prueba escrita y lapiceros",
  "Pruebas impresas","Evaluaciones, lapiceros",
  "Evaluaciones, lapiceros, lápiz, borrador y crayones",
  "Evaluaciones, lápiz, borrador, regla",
  "Evaluación diagnóstico del MINEDUC",
  "La hoja de calificacion",
  "Cuadros de evaluación nada más",
  "La prueba objetiva","El examen",
  "Evaluación impresa",
  "Evaluación escrita y lápices",
  "Prueba bimestral impresa",
  "Prueba diagnóstica MINEDUC",
  "Pruebas escritas y lápices",
  "Evaluación bimestral",
  "Hoja de evaluación",
  "Cuadro de notas",
  "Cuadros de calificaciones",
  "Lista de cotejo", "Cuadros de control de calificaciones",
  "Cuadros de registro y cuadro de nota por unidad",
  "Evaluación escrita y lápices",
  "Evaluación impresa",
  "Prueba bimestral impresa",
  "Prueba diagnóstica MINEDUC", "Utilizó bolígrafo y cuadro de notas para anotar la calificación del trabajo de los estudiantes."

)

### ninguno
valores_material_ninguno <- c(
  "Ninguno","No se observa más que el pizarrón.",
  "Solo pizarrón.","No trae el libro con que trabaja",
  "Nada", "No utilizó.", "No utilizó ningún material.",
  "No se observó.","No se observaron",
  "No aplica","No hubo clase.", ".",
  "N/A", "Nada.","Nada", "Ninguna", "Ninguno",
  "Ninguno.", "Ningún material",
  "Nada extraordinario, solo explicación oral",
  "Solo dialogó","Ningún material, aunque sí tiene libro para la materia observada.\nEl desarrollo de la clase fue realizada por los alumnos.",
  "No se observa, ya que únicamente dicta a los alumnos.",
  "No se observó porque no había energía eléctrica",
  "No utizo ningun material.",
  "La docente no desarrolló la clase, porque los estudiantes expusieron.",
  "La docente no utilizó material alguno, la clase fue práctica con la participación de los estudiantes.",
  "Solo explicó","Ninguno",
  "No se observa","Ninguno",
  "No utilizo ningún tipo de material.",
  "No utilizó, no asistió.",
  "No utilizó, no se presentó.",
  "No se observa.",
  "No utilizo ningún material",
  "No utilizo ningún material.",
  "No utilizo ningún recurso.",
  "No utilizó materiales, ya que el desarrollo de la clase no estuvo a su cargo.",
  "Solo estaba dando instrucciones",
  "El docente no tenía más que su planificación.",
  "Workshop",
  "Explicar la importancia de tener un proyecto de vida para el futuro",
  "Lo que los alumnos realizaron fue una práctica de mesa redonda.",
  "La docente no utilizo ningún material.",
  "No desarrolló la clase.",
  "No asistió al aula.",
  "No se observó","Ninguno",
  "No utilizo ningún tipo de material.",
  "No se observa más que el pizarrón.",
  "No utilizó materiales, pero hizo uso de las características de su voz para desarrollar el contenido y lograr la atención de los estudiantes.",
  "No utilizó, no asistió.",
  "No utilizó, no se presentó.",
  "No se observó.",
  "No se observan",
  "No utiliza ningún material",
  "No utilizo materiales.",
  "No utilizo ningun material.",
  "No utilizó materiales.",
  "No utilizó, fue exposición verbal.",
  "Nada, solo calificó las presentaciones de los estudiantes",
  "Nada, solo las instrucciones"
)

### artistico manualidades
valores_material_artistico_manualidades <- c(
  "Acuarelas, dibujos, hojas, pinceles.",
  "Artes plásticas 2",
  "Carteles, dibujos, marcadores, pegamento.",
  "Carteles, marcadores, dibujos.",
  "Lienzo, acuarela en líquido, pinceles.",
  "Crayones, hojas, pegamento, cuadernos y pizarra",
  "Hojas, crayones, marcadores",
  "Hojas media carta, pegamento, marcadores, crayones",
  "Lana, aguja, tijeras.",
  "Materiales para decoración del mes patrio",
  "Paletas de madera",
  "Globos",
  "Platillos típicos, mesa, adornos",
  "Los alumnos portaban trajes elaborados por ellos mismos.",
  "Bocina, audio, escenografía y disfraces",
  "Bocina, trajes típicos",
  "Elementos de la gastronomía local."
)

### material de laboratorio
valores_material_tecnico_oficios <- c(
  "Desarrollo de laboratorio",
  "Microscopio","Sillas exclusivo para el salón de  belleza, planchadores de cabello, agua.",
  "Herramientas para el trabajo de robótica: destornillador, tornillos, entre otros.",
  "Lijas, lijadora, pegamento, rotomartillo, escuadra.",
  "Madera, lija, cierras, martillo y clavos.",
  "Martillo, tablas, clavos, pegamento .",
  "Máquinas de escribir y folletos de mecanografía",
  "Recursos de materiales de campo (machetes, azadón)",
  "Maquinas, tijeras, telas, hilos.",
  "Moldes, masa, arina, leche, tablas, cuchillos.",
  "Plantilla de clavos\nLana\nGalletas\nMantequilla\nQueso\nMoldes\nHorno\n",
  "Utilizo los recursos que tienen en erea de cocina como mesas, utensilios de cocina, horno, etc."
)

### planificador
valores_material_planificador <- c(
  "Planificación semanal",
  "Planificador", "Planificador",
  "Planificador\nLibros cuadernos",
  "El cronograma de actividades.", "Planificador",
  "Planificador y guía del docente, ambos de telesecundaria proporcionados por el MINEDUC.  También libros de los estudiantes y materiales para la resolución de los ejercicios.",
  "Planificador, guía del docente y libros de los estudiantes.",
  "Planificador, guía del docente, libros de los estudiantes, tabla periódica de los elementos, pizarra, marcadores, televisión y laptop."
)

### papeleria
valores_material_estudiantes <- c(
  "Al ser un día de exposiciones, los materiales fueron creados y utilizados por los estudiantes (carteles)",
  "En esta oportunidad fueron los estudiantes los que llevaron carteles para exponer sus temas.",
  "En este caso los alumnos utilizaron carteles y entregaron un trabajo escrito.",
  "Fueron los alumnos los que hicieron carteles, cada grupo hace su material para exponer.",
  "Los alumnos utilizaron papelográfo.",
  "Los alumnos utilizaron sus carteles.",
  "Nada, los estudiantes presentaron carteles",
  "Nada, los estudiantes son quienes trajeron algún tipo de material que a su juicio consideraban necesario para las macetas",
  "Nada, son los estudiantes quienes presentaron las respuestas y los procedimientos en el pizarrón",
  "Nada. Los estudiantes en su presentación usaron titeres",
  "Los alumnos se auxiliaron de una bocina para reproducir la música de la presentación.",
  "Los alumnos tenian su reproductor de música.",
  "Los alumnos utilizar recursos tecnológico.",
  "Circulos hechos de papel iris, impresiones con la actividad a realizar, lana, masking tape.",
  "De evaluación y registro",
  "El docente utiliza hojas que ha recolectado a través de inrernet.",
  "Ella les proporciona un documento que es por unidad, porque no tienen libros de sociales para los jóvenes. El documento  les cuesta Q. 25 a cada estudiante, son hojas impresas con faldero y gancho",
  "Folletos y guías",
  "Hoja de papel con contenido a mano.\nHojas impresas evaluación escrita\nMaracas para la tercera fase.\nLapicero, marcador.",
  "Hojas, goma, papel de colores",
  "Hojas, lápiz y cuaderno",
  "Hojas, mapas, lapiceros, marcadores, borrador",
  "Hojas, marcadores y pizarra",
  "Imágenes",
  "Imágenes impresas de fenómenos físicos, químicos y fisicoquímicos. La hojita con la pregunta la deben pegar en el cuaderno",
  "Información acerca de Rendición de Cuentas",
  "Lectura impresa y cuaderno",
  "Lecturas varias.",
  "Logo, tarjetas, hojas.",
  "Los carteles, recortes, tijeras y pegamento.",
  "Material de papelería",
  "Papel de china, tijeras y pegamento, pizarrón.",
  "Pizarra, hojas en blanco",
  "Pizarrón y hojas con ejercicios",
  "Pizarrón, copias con definiciones, hojas de papel bond.",
  "Pizarrón, hojas de contenidos.",
  "Pizarrón, hojas de trabajo.",
  "Pizarrón, una hoja donde tenia la dinámica y marcadores",
  "Pizarrón, yeso, copias de contenidos.",
  "Resumen realizado por la docente",
  "Sopa de letras, libro, pizarrón y cuaderno, tijera y pegamento.",
  "Tarjetas con tipos de conectores",
  "Tarjetas de figuras de animales, pizarrón, marcadores.",
  "Tarjetas, pizarrón, marcadores.",
  "Tiras de papel con oraciones, guía de contenidos del estudiante, pizarrón y marcadores",
  "Un antifaz impreso en una hoja la cual los estudiantes deben pegar en el cuaderno",
  "Utilizó hojas con las definiciones del tema, pizarrón y marcadores.",
  "Utilizó una hoja de trabajo resuelta por alumnos de tercero sección B",
  "No utilizó, los estudiantes utilizaron cartulinas, papel de colores, marcadores, para realizar la actividad."
)

## Recode con case match ----
obs02 <- obs02 |>
  mutate(
    materiales = case_match(
      materiales,
      all_of(valores_material_ninguno) ~ "Ninguno",
      all_of(valores_material_estudiantes) ~ "Material de papelería",
      all_of(valores_material_evaluacion) ~ "De evaluación y registro",
      all_of(valores_material_instrumento) ~ "Instrumentos musicales",
      all_of(valores_material_deportes) ~ "Balones y elementos de deporte",
      all_of(valores_material_didactico) ~ "Material didáctico",
      all_of(valores_material_utiles) ~ "Cuaderno y útiles escolares",
      all_of(valores_material_tecnologia) ~ "Equipo tecnológico",
      all_of(valores_material_impresos) ~ "Folletos y guías",
      all_of(valores_material_libros) ~ "Libros",
      all_of(valores_pizarron) ~ "Pizarra y marcadores",
      all_of(valores_material_planificador) ~ "Planificador",
      all_of(valores_material_tecnico_oficios) ~ "Equipo de laboratorio y oficios",
      all_of(valores_material_artistico_manualidades) ~ "Artísticos y manualidades",
      .default = materiales
    )
  )

## volver a ver errores


valores_materiales <- obs02 |>
  filter(!(is.na(materiales))) |>	
  select(materiales) 

#View(valores_materiales)

# espacio_fuera_aula ----

## ver errores

valores_espacio_fuera_aula <- obs02 |>
  filter(actividad_fuera == "Sí") |>	
  filter(!(is.na(espacio_fuera_aula))) |>	
  select(espacio_fuera_aula) 

dput(valores_espacio_fuera_aula)

## Define los vectores ----

### cancha deportiva y actividad física
valores_cancha <- c(
  "A la cancha de la comunidad. Evaluación de Educación Física",
  "En la cancha de basquet bol.",
  "A una pequeña cancha techada para presentar las danzas de parte de los estudiantes",
  "A una cancha techada de preprimaria. Aquí se hizo la presentación de los estudiantes",
  "A la cancha polideportiva para aplicar la prueba",
  "La evaluación se hizo en la cancha techada del establecimiento educativo",
  "La cancha, estiramientos",
  "Trabajaron en la cancha polideportiva del patio",
  "En la cancha polideportiva.",
  "Cancha deportiva ahí realizaron una obra",
  "Cancha de baloncesto, realizar practicas de coordinación de manejo de balón de baloncesto",
  "Ala cancha",
  "Las actividades de calentamiento, estiramiento y ejercicios de carrera fueron realizados en un campo de futbol a unos 5 minutos del establecimiento.",
  "A la cancha de baloncesto",
  "A la cancha de baloncesto a practicar la gimnasia rítmica que tienen que hacer, la siguiente semana tienen concurso  en el establecimiento",
  "Cancha polideportiva.",
  "Cancha",
  "Cancha",
  "Cancha",
  "Cancha del Colegio",
  "En la cancha polideportiva.",
  "Cancha polideportiva. Práctica del área en observación.",
  "A la cancha",
  "Canchas",
  "Cancha deportiva",
  "Fueron a la cancha polideportiva y realizaron una actividad del semáforo tomándose de las manos todos los alumnos y formando un círculo y desarrollando esa dinámica. En todo tiempo dirigida por la maestra.",
  "Por ser clase de Educación Física, se indicó que la reciben en las canchas que están justo afuera del establecimiento; pero en esa ocasión, se indicó que fueron a correr por los alrededores del sector.",
  "Educación física la reciben en la cancha de básquet",
  "A la cancha d básquet y la docente les hizo una prueba de personalidad",
  "La cancha de futbol",
  "Cancha deportiva",
  "A la cancha municipal de básquet ball",
  "Salieron a la cancha y allí desarrollaron la primera parte de la clase",
  "La actividad se realizó desde el inicio en la cancha polideportiva",
  "A la cancha para realizar la dinámica",
  "En la cancha realizo una evaluación",
  "En la cancha, realizaron una presentación de bailes",
  "Los alumnos formaron filas con sus escritorios, en el lugar que ocupa la cancha polideportiva, para realizar su evaluación final",
  "A la cancha polideportiva",
  "Ensayo en la cancha",
  "A la cancha polideportiva",
  "A la cancha polideportiva",
  "La actividad se realizó en la cancha de baskebol",
  "A la cancha de Básquet",
  "A la cancha de básquet y repasaron en grupo el baile",
  "Algunos ocuparon la cancha de basquet bol, otros el salón de usos múltiples, otro grupo el área de jardin para repasar.",
  "En la cancha polideportiva",
  "Cancha de baloncesto.",
  "Realizó la clase en la cancha polideportiva",
  "A la cancha polideportiva",
  "A la cancha",
  "A la cancha","A la cancha del barrio cerca del establecimiento\n",
  "A la cancha polideportiva",
  "A la cancha polideportiva",
  "A la cancha polieportiva a resolver su evaluación final",
  "A la cancha polideportiva para realizar el examen final"
)

### patio y actividad física
valores_patio <- c(
  "Al patio del centro educativo a practicar vóleibol",
  "En el patio, realizaron una dinámica que se llama la máquina de escribir, la docente leía un párrafo y Ellis se formaban en filas le escribían imaginariamente al compañero en la espalda como si fueran una máquina de escribir, si era punto el hacían la cabeza para delante al compañero y si era coma le hacían la cabeza así un lado.",
  "Al patio de la escuela",
  "Patio",
  "Al patio de la escuela",
  "Patio del establecimiento",
  "En el patio del establecimiento ensayaron coreografía",
  "Toda la clase se realizó en el patio dónde tienen las macetas",
  "Al patio a realizar actividad de acuerdo al t3ma calidad de vida",
  "Al patio, desarrollaron juegos",
  "Patio del colegio",
  "Al patio del establecimiento a práctica de baile",
  "Se ubicaron en el patio y en ese espacio ensayaron, organizados en dos grupos",
  "Fueron al patio para ensayar el Acto Cívico.",
  "Al patio para ensayar el Acto Cívico.",
  "Salieron al patio para practicar Gimnasia",
  "Al patio a practicar bailes",
  "Al patio de la escuela",
  "Al patio de el establecimiento",
  "Al patio del establecimiento.",
  "Patio del establecimiento y trabajaron gimnasia rítmica.",
  "Frente al salón de clases y patio del instituto.",
  "Al patio",
  "En el patio",
  "En el patio  alrededor de las aulas y la cocina.",
  "Al patio y al corredor",
  "Al patio y trabajaron dramatización",
  "Patio, piñata",
  "Al patio, para presentar el baile grupal."
)

### corredores para realizar actividad de aula
valores_corredor <- c(
  "Algunos alumnos salieron a los corredores para realizar los lanzamientos de sus catapultas, aunque sin supervisión del docente.",
  "En el corredor, realizar sus carteles.",
  "En el corredor del establecimiento.",
  "En el corredor del establecimiento.",
  "Al corredor del establecimiento.",
  "Se ubicaron en el corredor porque es el espacio que utilizan para realizar las evaluaciones bimestrales",
  "Al corredor",
  "Salieron al corredor porque ese es el espacio que utilizan para evaluar",
  "Al corredor",
  "Se ubicaron en el corredor porque es el espacio que utilizan para realizar las evaluaciones bimestrales",
  "Luego del dictado de la teoría, salieron al corredor a elaborar una receta.",
  "A los corredores del establecimiento porque en diferentes espacios colocó los ejercicios que debían resolver en el rally, los estudiantes se colocaban en el lugar en donde estaba el ejercicio, tomaban nota y lo resolvían.",
  "Al corredor a realizar las actividades que asignó en grupo.",
  "Al corredor para resolver su parcial",
  "Al corredor del establecimiento",
  "En el corredor de la clase."
)

### salón para actividades artísticas
valores_salon <- c(
  "En el salón del auditorio,  para realizar los dibujos en mesas.",
  "Fueron al salón de usos múltiples del establecimiento,  los alumnos hicieron exposiciones de las Civilizaciones Mayas, Imperio Azteca e Inca",
  "La actividad se realiza en el salón de usos múltiples de la comunidad",
  "A una galera que utilizan como salón de actividades, a practicar bailes que les corresponde por equipos",
  "Salón especialidad de cocina y repostería.",
  "Salón de belleza.",
  "En el salón en donde se ubica las marimbas.",
  "El docente no desarrolló contenidos porque le pidieron que llevara a los estudiantes al Salón de Actos para participar en Acto Cívico.",
  "Al salón de audiovisuales",
  "En el espacio que tiene el establecimiento como salón de usos múltiples.",
  "Los alumnos fueron al salón donde tienen los instrumentos, el cual está compartido con el laboratorio de computación",
  "Al salón de primero, para impartir la clase en conjunto los tres grados.",
  "Al salón de actividades varias, realizaron su examen final de artes visuales."
)

### laboratorio para recibir clase
valores_laboratorio <- c(
  "En el laboratorio de computación","En el laboratorio de computación.",
  "En el laboratorio de computación","Laboratorio de ciencias naturales",
  "Dividió a los estudiantes en los dos laboratorios"
)

### taller para área ocupacional
valores_taller <- c(
  "En el laboratorio de carpintería.",
  "Taller de corte y confección."
)

### área fuera del establecimiento para actividades
valores_campo_comunidad <- c(
  "Campos ciudad de los niños y llevaron a cabo el examen bimestral",
  "Al campo de la comunidad, contiguo al centro educativo.",
  "Al campo de la comunidad. Realizaron ejercicios de voleibol",
  "Al campo de la comunidad.",
  "Salieron a la calle para limpiar el muro perimetral",
  "Al área de campo a limpiar el espacio en donde sembraran semillas",
  "Fueron a desfilar a Génova Costa Cuca, por inauguración de la feria",
  "Al espacio frente al colegio al aire libre que utilizan para educación física (área del atrio de la iglesia)",
  "Al campo de fútbol",
  "Los alumnos se trasladaron al área asignada para educación física.",
  "Junto con los alumnos visitaron la escuela preprimaria y la escuela primaria.",
  "A los jardines del establecimiento para enseñar a los estudiantes las diferentes plantas",
  "Realizaron la práctica del transplante de esquejes de un rosal.",
  "Al Estadio Municipal Roquelino Escobar"
)

### no especifica
valores_sin_actividad <- c(
  "No realizo ninguna actividad pero si trabajaron fuera del aula","Salieron al acto cívico", 
  "Los alumnos se ubicaron en el área de actividades de la escuela, todos los alumnos son evaluados en un mismo espacio (primero, segundo y tercero).", 
  "Algunos estudiantes tenían sus pollos afuera y salió para calificarlos", 
  "Afuera del aula para practicar en grupo la canción relacionada con los colores en kaqchikel."
)

### cafeteria a comprar alimentos
valores_cafeteria <- c(
  "Se dirigen a la cafetería a traer productos: golosinas, gaseosas y agua pura. Para utilizarlos en la dramatización y enseguida los devolvieron"
)
## Recode con case match ----

obs02 <- obs02 |>
  mutate(
    espacio_fuera_aula = case_match(
      espacio_fuera_aula,
      all_of(valores_sin_actividad) ~ "No especifica",
      all_of(valores_cafeteria) ~ "Cafetería para obtener alimentos",
      all_of(valores_campo_comunidad) ~ "Área fuera del establecimiento para actividad educativa",
      all_of(valores_taller) ~ "Taller para área ocupacional",
      all_of(valores_laboratorio) ~ "Laboratorio para recibir computación",
      all_of(valores_salon) ~ "Salón para actividades artísticas",
      all_of(valores_corredor) ~ "Corredor para actividad de clase",
      all_of(valores_patio) ~ "Patio para hacer actividad física",
      all_of(valores_cancha) ~ "Cancha deportiva para hacer actividad física",
      .default = espacio_fuera_aula
    )
  )

## volver a ver errores

valores_espacio_fuera_aula <- obs02 |>
  filter(actividad_fuera == "Sí") |>	
  filter(!(is.na(espacio_fuera_aula))) |>	
  select(espacio_fuera_aula) 

#View(valores_espacio_fuera_aula)

# tipo_instrumento ----

## ver errores

valores_tipo_instrumento <- obs02 |>
  filter(!(is.na(tipo_instrumento))) |>	
  select(tipo_instrumento) 

dput(valores_tipo_instrumento)

## Define los vectores ----

### no utilizo
valores_instrumento_ninguno <- c(
  "Ninguno", "Ninguno", "Ninguno", "Ninguno", "Ninguno", "Ninguno",
  "No utilizó.", "No utilizó.", "No utilizó", "No utilizó.", "No utilizó.",
  "No utilizó.", "No utilizó", "No utilizó", "No utilizó.",
  "No hubo evaluaciones", "No hubieron evaluaciones", "No hubo evaluaciones.",
  "No evaluó.", "No trabajo ninguna evaluación",
  "No aplica", "No aplica", "No aplica", "No aplica", "No aplica",
  "N/a", "N/A", "Nada", "Ninguna", "Ninguna", "Ningún instrumento",
  "No utilizó ningún instrumento",
  "No utilizó instrumento, únicamente colocó la nota.",
  "No utilizó ni instrumento de evaluación.",
  "No hizo evaluación",
  "No hay evidencia","No se observó.", "No se observó", "No se observa", "No se observa.",
  "No se observaron", "No se observaron",
  "No se visibilizó.", "No se visibilizó alguno.",
  "No se visibilizó ningún instrumento.",
  "A la vista no tenia ningún instrumento.",
  "A la vista ninguno. Al preguntarle indican que es sumativa y de observación",
  "No se observó ningún instrumento.",
  "No se observó ningún instrumento, únicamente al final del curso pidió los cuadernos par verificar si tenían las tareas y firmar cada cuaderno, indicando que después pasaría las notas a su cuadro de evaluación.",
  "No se observó instrumento alguno.",
  "No se visibilizó ningún instrumento, unicamente tenia sus libros y hojas de lectura en el escritorio",
  "Visible no traia nada.",
  "A la vista la docente no tiene ningún instrumento de evaluación.",
  "Al docente no se le observó ningun instrumento de evaluación.",
  "En el aula la docente no utilizó ningún instrumento de evaluación.",
  "En el momento no utilizó ninguno.",
  "En la sesión ningun instrumento de evaluación.",
  "La docente no evaluó.",
  "La docente no hizo ninguna evaluación a los estudiantes.",
  "Ningun instrumento.",
  "Ninguno",
  "Ninguno observado.",
  "Ninguno, no de observó algún instrumento.",
  "Ninguno, solo anotó en un cuaderno",
  "Ninguno, solo califica y coloca el sello de revisado.",
  "Ninguno, solo firma los trabajos realizados",
  "Ninguno, únicamente selló los  cuadernos de los alumnos que habían finalizado su poema, ese sello tiene ponderación que registra al final de bimestre.",
  "Ninguno.",
  "Ninguno. Al final de la semana utilizará la lista de cotejo.",
  "Ninguno. El docente solo marca el cumplimiento de la tarea.",
  "Ningunom",
  "No fue visible.",
  "No hubo",
  "No se observan.",
  "No se observaron instrumentos",
  "No se observo",
  "No se observó alguno.",
  "No se observó algún instrumento.",
  "No se observó ningún que utilizará ningún instrumento.",
  "No se observó que tuviera algún instrumento, solo fue de observación.",
  "No se visibiliza.",
  "No se visibilizó",
  "No se visibilizó ninguno.",
  "No se visualizó ningún instrumento de evaluación.",
  "No tuvo ningún instrumento de calificación, solo tenia su computadora pero sin usar.",
  "No utilizo",
  "No utilizo ni instrumento de evaluación.",
  "No utilizo ningún instrumento",
  "No utilizó nada",
  "No utilizó ninguno.",
  "No utillizó.","Pregunto si estaba claro lo que impartió",
  "No utizo ningun instrumento",
  "No utizo ningun instrumento.",
  "El docente observó la presentación de los estudiantes, sin embargo no se visibilizó algun instrumento de evaluación.",
  "En el salón no se observó registro de la ponderación de la actividad.",
  "Nada, solo anotó las calificaciones en la hoja de control",
  "No se visibilizó algún instrumento de evaluación durante el desarrollo de la clase, se le consultó e indicó que fue de observación.",
  "No utilizo ningún instrumento, argumentando que los había dejado en su casa, pero que firmaría los cuadernos para evidenciar qué cada alumno había concluido con lo indicado en clase.",
  "No utilizó en el momento de la observación, al preguntarle si utilizaría algún instrumento para calificar, indicó que una rúbrica, pero que no la tenía aún, no se pudo evidenciar.",
  "No utilizó en el momento de la observación; sin embargo, al preguntarle, indicó que para calificar la actividad estaría utilizando una Escala de rango.|",
  "No utilizó instrumentos, únicamente anotó una calificación a la exposición de los estudiantes.",
  "No utilizó un instrumento, únicamente colocó la nota en su cuadro.",
  "No utilizó, se le preguntó si está utilizando rúbrica, indicó que no.",
  "No utilizó, solo su hoja de control de notas",
  "No utilizó, únicamente anotó la nota en su cuadro.",
  "No utilizó, únicamente anotó una calificación en su cuadro.",
  "Visible no trajo nada, su escritorio en ningún momemto tuvo algún instrumento."
)

### instrumento de observación
valores_observacion <- c(
  "De observación",
  "De observación.",
  "Observación",
  "Observación directa, supervisión del ingreso a plataformas a cada alumno",
  "La docente indicó que solo fue observación.",
  "El docente observó a cada grupo.",
  "Observación y tecnica ludica",
  "Observación y lista de cotejo",
  "De observación  y de desempeño",
  "Guia de observación."
)

### lista de cotejo
valores_lista <- c(
  "Lista de cotejo",
  "Lista de cotejo.",
  "Checklist","La docente utilizó una lista de cotejo",
  "Lista de cotejo",
  "Check  list","El docente utilizo lista de cotejo",
  "Listado se cotejo",
  "Lista de cotejo y rúbrica",
  "El docente utilizó una lista de cotejo",
  "Lista de cotejo y rúbrica"
  
)

### escala de rango
valores_escalarango <- c(
  "Escala de rango","El instrumento fue una rúbrica.",
  "Escala de rango.", "Coevaluación","Rúbrica",
  "Rúbrica con nota en la revisión de ejercicio",
  "Cuadro de control de calificaciones.",
  "Cuadro de identificación de intensidad sonora y coevaluación",
  "El docente indicó que para la sesión del día su instrumento de evaluación sería una rúbrica.",
  "El docente utiliza una escala de rango, que elabora en su cuaderno."
)

### evaluacion escrita
valores_evaluacion_escrita <- c(
  "Prueba escrita",
  "Prueba escrita",
  "Prueba objetiva","Cuestionario",
  "Cuestionario dictado a los estudiantes.  Indicó la ponderación para cada respuesta correcta.",
  "Prueba objetiva.","Hoja de calificación",
  "Prueba bimestral impresa",
  "Evaluación bimestral",
  "Evaluación corta","Imágenes impresas, pregunta impresa, para responder la dinámica detectives",
  "Evaluación escrita impresa\nCuadro de notas para calificar las exposiciones\nMaracas para representar la música en los diferentes cantos",
  "El docente utilizó un exámen escrito con preguntas que debían responder los estudiantes.",
  "Una hoja de evaluación final"
)

### hoja de trabajo y ejercicios
valores_hoja_trabajo <- c(
  "Hoja de trabajo","Flauta y hojas impresas",
  "Hoja de trabajo impresa","Folleto",
  "Hojas (copias)","Hola de trabajo",
  "Hojas impresas de los símbolos patrios",
  "Hojas de trabajo","Desarrollo de laboratorio",
  "Ejercicio de escribir el abecedario",
  "Ejercicio en clase",
  "Ejercicios","Material reciclado",
  "Ejercicios en clase","La guía de texto de los estudiantes, hojas impresas con dichos y refranes, entrego otra hoja de trabajo con dos columnas en las cuales los estudiantes deben escribir dichos en una columna y refranes en la otra columna",
  "Las figuras de la fauna de Guatemala",
  "Las hojas de trabajo que resolvieron los estudiantes en grupo, se usaron para realizar exposición de sus resultados, también de manera grupal.",
  "En el cuaderno los alumnos resolvieron ejercicios de fracciones",
  "Evaluación escrita",
  "Hoja impresa de sopa de letras",
  "Listado de palabras",
  "Mapa conceptual","Hoja de lectura y comprensión lectora",
  "Hoja de trabajo y cuadernos",
  "Hoja de trabajo y un cartel donde los estudiantes compartían diferentes recetas para mantener relaciones sociales efectivas.",
  "Hoja impresa de un presupuesto familiar y hoja elaborada por los estudiantes con columnas de ingresos y gastos en la familia",
  "Hojas con la definición de pluricultural, cultura",
  "Hojas de trabajo y ejercicios",
  "Hojas y colores para elaborar el mapa mental",
  "Mapa conceptual.",
  "Para la prueba pendiente había un test",
  "Preguntas de la lectura",
  "Práctica y ejercicio en la clase",
  "Texto paralelo",
  "Trabajos grupales y sintésis.",
  "Hoja impresa","Cartones de lotería",
  "Hoja impresa de evaluación final",
  "Hoja de actividades y evaluación.",
  "Hoja de registro de actividades",
  "Registro de actividades.",
  "Solo reviso los cuadernos y los firmo.",
  "El cuaderno de los estudiantes, les puso un ejercicio de 5 verbos para que cada estudiante coloquen el número, modo, tiempo y la persona",
  "Ejercicios en el cuaderno","Actividad de comentario personal del tema visto",
  "Actividad de un mapa mental",
  "Califico el dibujo en los cuadernos cuadernos",
  "Copiar de folleto",
  "Cuadro comparativo"
)


### instrumentos musicales
valores_instrumentos_musicales <- c(
  "Flauta","Flauta y hojas impresas",
  "Una flauta",
  "Ejecución de flauta y guitarra",
  "Los puso a practicar con aplicación de piano y con lira"
)

### tecnologia
valores_equipo_tecno <- c(
  "Computadora y el programa Mecanografía 10",
  "La computadora","Bocina y celular",
  "Canva, informe del proyecto",
  "Cañonera, hojas impresas, laptop, video",
  "Computadora",
  "Computadoras, cañonera, pantalla, televisión",
  "Tablet y celular de cada estudiante", "Cuaderno y celular","Globos, bocina, celular"
  
)

### maquina de escribir
valores_maquina_escribir <- c(
  "Máquina de escribir"
)

### elementos de educación física
valores_material_edu <- c(
  "Cuerdas, conos, pelotitas","Pelotas"
)

### no especifica 
valores_sinespecificar_instrumento <- c(
  "Evaluación", "Evaluación objetivo.",
  "El material solicitado para la tarea individual",
  "El pizarrón,  donde escribió una actividad para que los estudiantes la desarrollen",
  "En aula el docente únicamente tenia su computadora, pero apagada y cerrada."
)

### dinámicas y juegos
valores_dinamica_juego <- c(
  "Cartones de lotería", "Una dinámica,  pronunciando las consonantes, el docente explicó como realizar la pronunciación",
  "Juego interactivo",
  "Juego participativo",
  "Dinámica de secuencia revuelta",
  "Lluvia de ideas",
  "Resolver ejercicios",
  "Resolución de ejercicios en el pizarrón",
  "Pregunta a los alumnos",
  "Preguntas al azar",
  "Repaso", "Laboratorio",
  "Laboratorio en clase",
  "La prueba impresa por estudiante",
  "La lotería",
  "Pasaban al frente a practicar en el teclado",
  "Práctica individual de notas musicales en el piano",
  "Realización de carteles y exposiciones (la última parte ya no quedo tiempo de hacerlo)",
  "Retroalimentacion de manejo de balón de baloncesto",
  "Repaso",
  "Método de casos",
  "Realizo un debate para comprender las diferencias de la problemática ambiental"
  
)

### asistencia
valores_asistencia <- c(
  "Asistencia"
)

### control de calificaciones y actividades
valores_control <- c(
  "Calendario de ponderación de las actividades",
  "Control de califica por bloque",
  "Control de calificaciones","Tiene una hoja en dónde lleva el control del desempeño semanal de los estudiantes. Cada estudiante tiene su propio folder.",
  "El docente mostró la hoja de actividades que enlista los instrumentos a utilizar durante el bimestre."
)

### revision
valores_revision <- c(
  "Al no observar, se le preguntó a la docente acerca del instrumento de evaluación, indicó que utilizó el de observación",
  "El docente indica que para ponderar revisa las firmas que tienen en el cuaderno los estudiantes al final de bimestre.",
  "El docente indicó que al finalizar el bimestre revisan la totalidad de firmas con punteo que cada estudiante tiene en el cuaderno.",
  "En este espacio se podría indicar que el docente sella el cuaderno en dónde los estudiantes resolvieron los ejercicios. No califica pero deja evidencia del cumplimiento",
  "En su cuaderno de notas, obsevo el ejercicio y les coloco una calificación",
  "La docente no registró ponderaciones, únicamente selló y revisó los comentarios realizados por los alumnos.",
  "La docente únicamente observó. Se adjuntará un cuadro de notas, sin embargo no tiene registro de la exposición observada.",
  "No se observó, únicamente califica la lectura y su cuestionario. Firmando y sellando.",
  "No se observó.  La docente únicamente selló los cuadernos de algunos estudiantes.",
  "No se observó. Aunque la docente indicó que la práctica es para prepararlos para la certificación no sr observó que registrara algunos aspectos observados por ella.",
  "Solo marca el cumplimiento de la tarea, en este caso el separador de trimestre con una firma",
  "Solo revisó la tarea y anotó la calificación en su cuadro de control"
  
)

###Libros
valores_inst_libros <- c(
  "Libro",
  "Libro El principito",
  "Libro de matemáticas",
  "Libro de texto",
  "Libros", "Palabras del diccionario",
  "Solamente su libro y una copia del dibujo que los estudiantes deben realizar"
  
)

## Recode con case match ----

obs02 <- obs02 |>
  mutate(
    tipo_instrumento = case_match(
      tipo_instrumento,
      all_of(valores_dinamica_juego) ~ "Dinámicas y juegos",
      all_of(valores_material_edu) ~ "Material de educación física",
      all_of(valores_maquina_escribir) ~ "Máquina de escribir",
      all_of(valores_equipo_tecno) ~ "Equipo tecnológico",
      all_of(valores_instrumentos_musicales) ~ "Instrumentos musicales",
      all_of(valores_hoja_trabajo) ~ "Hojas de trabajo y ejercicios",
      all_of(valores_evaluacion_escrita) ~ "Evaluación escrita",
      all_of(valores_escalarango) ~ "Escala de rangos",
      all_of(valores_lista) ~ "Lista de cotejo",
      all_of(valores_observacion) ~ "Observación",
      all_of(valores_instrumento_ninguno) ~ "Ninguno",
      all_of(valores_inst_libros) ~ "Libros",
      all_of(valores_revision) ~ "Revisión de tareas",
      all_of(valores_sinespecificar_instrumento) ~ "No especifica",
      all_of(valores_control) ~ "Control de calificaciones y actividades",
      all_of(valores_asistencia) ~ "Toma de asistencia",
      .default = tipo_instrumento
    )
  )

## volver a ver errores

valores_tipo_instrumento <- obs02 |>
  filter(!(is.na(tipo_instrumento))) |>	
  select(tipo_instrumento) |>	
  tabyl(tipo_instrumento)

#View(valores_tipo_instrumento)

write_xlsx(obs02, here("Datos","Listos para análisis","Observación aulas", "obs02.xlsx"))