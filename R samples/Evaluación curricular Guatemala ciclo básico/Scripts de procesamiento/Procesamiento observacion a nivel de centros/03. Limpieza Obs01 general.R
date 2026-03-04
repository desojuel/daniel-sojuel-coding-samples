# agua_razon_sin_agua_visita ----

## ver errores
valores_agua_razon_sin_agua_visita <- obs01 |>
  filter(agua_centro == "No") |>	
  filter(!(is.na(agua_razon_sin_agua_visita))) |>	
  select(agua_razon_sin_agua_visita) 

#dput(valores_agua_razon_sin_agua_visita)

# agua_razon ----

## ver errores
valores_agua_razon <- obs01 |>
  filter(agua_centro == "No") |>	
  filter(!(is.na(agua_razon))) |>	
  select(agua_razon) 

#dput(valores_agua_razon)

## Define los vectores ----

### falta de servicio en la comunidad
valores_falta_comunidad <- c(
  "En la comunidad no hay servicio de agua potable.",
  "No hay fuentes de abastecimiento. Únicamente fuente de lluvia.",
  "No hay ninguna fuente que les permita tener agua, únicamente de lluvia.",
  "Por la situación económica de los padres de familia. La comunidad no tiene agua potable."
)

### limitaciones de infraestructura
valores_limite_infra <- c(
  "Porque el predio no es del Ministerio de Educación.",
  "Porque están en las instalaciones de una bodega, la cual no cuenta con agua"
)

### falta de colaboracion padres
valores_colaboracion_padres <- c(
  "Según indica la directora, el agua deberá ser instalado con la ayuda de los padres de familia, hasta el momento no han accedido a colaborar."
)

## Recode con case match ----

obs01 <- obs01 |>
  mutate(
    agua_razon = case_match(
      agua_razon,
      all_of(valores_colaboracion_padres) ~ "Falta colaboración de padres",
      all_of(valores_limite_infra) ~ "Limitaciones de infraestructura",
      all_of(valores_falta_comunidad) ~ "Sin fuente de abastecimiento",
      .default = agua_razon
    )
  )

## volver a ver errores

valores_agua_razon <- obs01 |>
  filter(agua_centro == "No") |>	
  filter(!(is.na(agua_razon))) |>	
  select(agua_razon) 

#View(valores_agua_razon)

# agua_gestiones_director ----

## ver errores
valores_agua_gestiones_director <- obs01 |>
  filter(agua_centro == "No" | agua_centro_dia_visita == "No") |>
  filter(!(is.na(agua_gestiones_director))) |>
  select(agua_gestiones_director) 

#dput(valores_agua_gestiones_director)

### Ninguna
valores_singestiones <- c(
  "Ninguna, así es en el sector","Se hace a nivel de cantones. Es un problema del municipio",
  "Ninguna gestión","Ninguna", "Ninguno",
  "Ninguna gestión", "No ha podido gestionar, debido a que el  predio es de la primaria.",
  "No se puede gestionar por ser cooperativa.",
  "Indica la señorita directora que es un trámite largo y de muchos papeles, por distancia y tiempo no se ha podido hacer más."
)

### informar autoridades
valores_autoridades <- c(
  "Informar a las autoridades educativas pero no ha tenido respuesta"
)

### construcción de un pozo
valores_pozo <- c(
  "Si construcción  de pozo mecánico  en el establecimiento",
  "Han intentado la gestión e instalación de un pozo.", "Para el director agua tienen en el establecimiento, debido a que tienen un pozo que funciona con bomba, a falta de energía eléctrica deja de funcionar también la bomba."
)

## Recode con case match ----

obs01 <- obs01 |>
  mutate(
    agua_gestiones_director = case_match(
      agua_gestiones_director,
      all_of(valores_pozo) ~ "Pozo",
      all_of(valores_autoridades) ~ "Informar a autoridades",
      all_of(valores_singestiones) ~ "Ninguna",
      .default = agua_gestiones_director
    )
  )

## volver a ver errores

valores_agua_gestiones_director <- obs01 |>
  filter(agua_centro == "No" | agua_centro_dia_visita == "No") |>
  filter(!(is.na(agua_gestiones_director))) |>
  select(agua_gestiones_director) 

#View(valores_agua_gestiones_director)

# laboratorio_area_cat_encargado ----

## ver errores
valores_laboratorio_area_cat_encargado <- obs01 |>
  filter(laboratorio_cat_encargado_estudiantes == "Sí") |>	
  filter(!(is.na(laboratorio_area_cat_encargado))) |>	
  select(laboratorio_area_cat_encargado) 

#dput(valores_laboratorio_area_cat_encargado)

## Define los vectores ----
### centro de computación en la comunidad
valores_cat_comunidad <-c(
  "Los alumnos acuden a un Centro de Computación los días sábados de 9:00 a 12:00.",
  "En un Centro Privado que funciona en la comunidad",
  "En la misma comunidad hay un centro de computación a donde acuden los alumnos una vez por semana."
)

### entorno virtual de aprendizaje
valores_virtual_aprendizaje <- c(
  "Entorno Virtual de Aprendizaje, ubicado en el municipio, los estudiantes de trasladan a dicha ubicación los días jueves, contra jornada."
)

### cat institucional identificado por nombre propio
valores_cat_nombre <-c(
  "CAT RENACER",
  "Academia de Computación No. 3 San Cristóbal Frontera",
  "ACADEMIA Y CENTRO DE APRENDIZAJE DE TECNOLOGÍAS DE LA INFORMACIÓN Y COMUNICACIÓN \"JIREH\"",
  "\"SISTEC\" AYARZA CASILLAS SANTA \n\n\"SISTEC\" JUTIAPA, JUTIAPA"
)

## Recode con case match ----
obs01 <- obs01 |>
  mutate(
    laboratorio_area_cat_encargado = case_match(
      laboratorio_area_cat_encargado,
      all_of(valores_cat_nombre) ~ "CAT identificado por nombre propio",
      all_of(valores_virtual_aprendizaje) ~ "Entorno Virtual de Aprendizaje",
      all_of(valores_cat_comunidad) ~ "Centro de computación de la comunidad",
      .default = laboratorio_area_cat_encargado
    )
  )

## volver a ver errores

valores_laboratorio_area_cat_encargado <- obs01 |>
  filter(laboratorio_cat_encargado_estudiantes == "Sí") |>	
  filter(!(is.na(laboratorio_area_cat_encargado))) |>	
  select(laboratorio_area_cat_encargado) 

#View(valores_laboratorio_area_cat_encargado)

# laboratorio_otros_recursos_tac ----

## ver errores
valores_laboratorio_otros_recursos_tac <- obs01 |>
  filter(laboratorio_otros_recursos == "Sí") |>	
  filter(!(is.na(laboratorio_otros_recursos_tac))) |>	
  select(laboratorio_otros_recursos_tac) 

#dput(valores_laboratorio_otros_recursos_tac)

## Define los vectores ----

### proyeccion y visualizacion
valores_proyeccion <-c(
  "Televisores", "Televisión", "Televisor.\n",
  "Cañonera, impresora, pantalla, pizarrón",
  "Cañoneras, televisor","4 laptos.",
  "2 proyectores \n5 pantallas","5 tabletas que utilizan algunos docentes para que los alumnos jueguen en grupo, como parte de las actividades en inglés.",
  "6 pantallas digitales, se adquirió por medio presupuesto para las experimentales.",
  "Aire acondicionado, pantalla, Internet."
)

### impresora y escaner
valores_impresora <- c(
  "Impresora y bocinas.", "Impresora",
  "Impresora multifuncional EPSON L3250, comprado con fondos de gratuidad en dónde pueden hacer impresiones los estudiantes.",
  "5 Escáner y 3 computadoras portátiles"
)

### equipo de sonido
valores_sonido_tac <-c(
  "Impresora y bocinas.",
  "Barra de sonido, impresora, ventiladores."
)

### conectividad y red
valores_red <-c(
  "Router donado por INNOVA pero no está instalado para las computadoras, no se pudo obtener mayor información por ausencia del docente de TAC",
  "Aire acondicionado, pantalla, Internet.", "Sistema de red, aire acondicionado, pizarra, extintor.",
  "Sistema de red, aire acondicionado, pizarra, extintor."
)

###mobiliario
valores_mobiliario_tac <-c(
  "Mesas, sillas, bancos.",
  "Mobiliario para las computadoras",
  "Pizarrón.", "Cañonera, impresora, pantalla, pizarrón"
)

### climatización y ventilación
valores_climatizacion <-c(
  "Hay ventiladores en el salón", "Sistema de red, aire acondicionado, pizarra, extintor.",
  "Ventiladores, otra pantalla, 32 cámaras  de seguridad en todo el establecimiento",
  "Aire acondicionado, pantalla, Internet.","Ventiladores, otra pantalla, 32 cámaras  de seguridad en todo el establecimiento",
  "Sistema de red, aire acondicionado, pizarra, extintor."
)

### equipo de mantenimiento tecnico
valores_tecnico_tac <- c(
  "Equipo de mantenimiento y reparación."
  )

## Recode con case match ----
obs01 <- obs01 |>
  mutate(
    laboratorio_otros_recursos_tac = case_match(
      laboratorio_otros_recursos_tac,
      all_of(valores_tecnico_tac) ~ "Equipo de mantenimiento técnico",
      all_of(valores_climatizacion) ~ "Climatización y ventilación",
      all_of(valores_mobiliario_tac) ~ "Mobiliario",
      all_of(valores_red) ~ "Conectividad y red",
      all_of(valores_sonido_tac) ~ "Equipo de sonido",
      all_of(valores_impresora) ~ "Impresora y escaner",
      all_of(valores_proyeccion) ~ "Equipo de proyección y visualización",
      .default = laboratorio_otros_recursos_tac
    )
  )

## volver a ver errores

valores_laboratorio_otros_recursos_tac <- obs01 |>
  filter(laboratorio_otros_recursos == "Sí") |>	
  filter(!(is.na(laboratorio_otros_recursos_tac))) |>	
  select(laboratorio_otros_recursos_tac) 

#View(valores_laboratorio_otros_recursos_tac)

# laboratorio_plataforma_general ----

## ver errores

valores_laboratorio_plataforma_general <- obs01 |>
  filter(!(is.na(laboratorio_plataforma_general))) |>	
  select(laboratorio_plataforma_general) 

#dput(valores_laboratorio_plataforma_general)

## Define los vectores ----


### canvas
valores_canvas <- c(
  "Classroom, Canvas",
  "Canvas",
  "Canvas, Google Forms.",
  "Canvas,  el docente de computación indica que utilizan únicamente correo electrónico, no utilizan las otras plataformas, ni WhatsApp.",
  "Canvas. Los estudiantes utilizan unas tablets de la primaria que solicitan cuando las necesitan.",
  "Canvas,  el docente de computación indica que utilizan únicamente correo electrónico, no utilizan las otras plataformas, ni WhatsApp."
)

### herramientas de google (Classroom, Meet, Drive, Gmail)
valores_google <- c(
  "Google clasroom y teams",
  "Classroom","Escholae",
  "Algunos profesores utilizan Classroom",
  "Las docentes comentan que han utilizado Google Classroom y Teams, pero que utilizan más WhatsApp para enviar información de tareas.",
  "Drive", "Google", "Google Forms",
  "Se indicó que utilizan Gmail.",
  "Gmail", "Google meet"
)

### geducar
valores_geducar <- plataforma_institucional <- c(
  "Geducar ( agenda digital)",
  "Geducar", "Geducar ( agenda digital)",
  "Geducar, sistema operativo de registro de estudiantes, tareas, asistencia, notas, cuadros de docentes"
)

### Whatsapp
valores_whatsapp <- mensajeria <- c(
  "Grupos de WhatsApp",
  "Tienen un grupo de WhatsApp para comunicarse con los estudiantes y padres de familia para brindar información.",
  "Utilizan más WhatsApp para enviar información de tareas."
)

### paginas web
valores_web <-plataforma_especializada <- c(
  "Www.Tecnologíaaranista.com",
  "Mecanet.", "Plataforma del Ministerio de la defensa"
)

### no especifica
valores_sinespecificar_plat <- c(
  "Office 2016.\nGoogle  \nWindows 7, 10 y 11."
)

### go clases
valores_goclasses <- c(
  "GoClasses"
)

### colplex 
valores_colpex <- c(
  "Colplex"
)

### runachay
valores_runachay <- c(
  "Runachay"
)

### idukay
valores_idukay <- c(
  "Idukay, es una plataforma ecuatoriana que utilizan, donde los estudiantes suben sus tareas, se pueden comunicar con los docentes, se llevan fichas anecdóticas; los padres de familia pueden entrar y revisar cómo están trabajando sus hijos."
)

## Recode con case match ----

obs01 <- obs01 |>
  mutate(
    laboratorio_plataforma_general = case_match(
      laboratorio_plataforma_general,
      all_of(valores_idukay) ~ "Idukay",
      all_of(valores_runachay) ~ "Runachay",
      all_of(valores_colpex) ~ "Colpex",
      all_of(valores_goclasses) ~ "GoClasses",
      all_of(valores_sinespecificar_plat) ~ "No especifica",
      all_of(valores_web) ~ "Páginas Web",
      all_of(valores_whatsapp) ~ "WhatsApp",
      all_of(valores_geducar) ~ "GEducar",
      all_of(valores_google) ~ "Herramientas de Google (Meet, Classroom, Drive",
      all_of(valores_canvas) ~ "Canvas",
      .default = laboratorio_plataforma_general
    )
  )

## volver a ver errores

valores_laboratorio_plataforma_general <- obs01 |>
  filter(!(is.na(laboratorio_plataforma_general))) |>	
  select(laboratorio_plataforma_general) 

#View(valores_laboratorio_plataforma_general)

# donacion_instrumentos_musicales ----

## ver errores

valores_donacion_instrumentos_musicales <- obs01 |>
  filter(adquisicion_instrumentos_musicales == "Fueron donados") |>	
  filter(!(is.na(donacion_instrumentos_musicales))) |>	
  select(donacion_instrumentos_musicales) 

#dput(valores_donacion_instrumentos_musicales)

## Define los vectores ----
### padres de familia
valores_padres_familia <- c(
  "Padres de familia",
  "Por padres de familia","Por auto gestión de padres  de familia y estudiantes.",
  "Por auto gestión de padres de familia y estudiantes.",
  "Ex alumnos y algunos padres de familia",
  "Padres de familia, alumnos y exalumnos; también instrumentos entregados al establecimiento como premios ganados en certámenes de bandas musicales"
)

### iglesia
valores_iglesia <-c(
  "La iglesia Centroamericana Bethel que es la dueña del Colegio",
  "Organización iglesia presbiteriana de Princeton"
)

### municipalidad
valores_muni <- c(
  "La municipalidad del municipio",
  "Alcaldes municipales anteriores"
)

### organización y fundación
valores_orga_funda <- c(
  "Proyecto Fe USA",
  "Fundación Rigoberta Menchú Tum y Club Quetzal"
)

### comunidad
valores_comunidad <-c(
  "Personas voluntarias.","Los emigrantes de la comunidad.\n",
  "Los emigrantes de la comunidad."
)

### festival de banda
valores_evento <- c(
  "En los festivales de bandas","En los festivales  de bandas"
)

## Recode con case match ----

obs01 <- obs01 |>
  mutate(
    donacion_instrumentos_musicales = case_match(
      donacion_instrumentos_musicales,
      all_of(valores_evento) ~ "En festival de bandas",
      all_of(valores_comunidad) ~ "La comunidad",
      all_of(valores_orga_funda) ~ "Organizaciones y Fundaciones",
      all_of(valores_muni) ~ "Municipalidad",
      all_of(valores_iglesia) ~ "Iglesias",
      all_of(valores_padres_familia) ~ "Padres de familia",
      .default = donacion_instrumentos_musicales
    )
  )

## volver a ver errores

valores_donacion_instrumentos_musicales <- obs01 |>
  filter(adquisicion_instrumentos_musicales == "Fueron donados") |>	
  filter(!(is.na(donacion_instrumentos_musicales))) |>	
  select(donacion_instrumentos_musicales) 

#View(valores_donacion_instrumentos_musicales)


# donaciones_tipo ----

## ver errores

valores_donaciones_tipo <- obs01 |>
  filter(donaciones_centro == "Sí") |>	
  filter(!(is.na(donaciones_tipo))) |>	
  select(donaciones_tipo) 

#dput(valores_donaciones_tipo)

## Define los vectores ----

### mejoras en infraestructura
valores_infraesctructura_donacion <- c(
  "Apoyo económico para la construcción del edificio",
  "3 aulas, 8 baños, 100 pupitres y 1 Tinaco.",
  "Materiales de construcción para aulas","Piso del salón de actividades múltiples fue donado por promoción bodas de plata 2008 -2010\n\nConstrucción aula D segundo B fue donada por la promoción bodas de plata 2008.\n\nEquipo de sonido fue donado por  orden INMEBOA profesora María Emma Sandoval.\n\nDonaciones por personas del lugar y promociones del INMEBOA:\n\nAlto parlantes.\nMicrófonos inalámbricos.\nCátedras de profesores.\nBombos, triple, cuádruple.\nCongelador\nRefrigeradora\nEstufa industrial\nToldo",
  "El establecimiento educativo fue construido por la comunidad con aporte de materiales especiales de parte de FUNDAP PEVI",
  "Materiales de construcción donados por padres de familia",
  "Piso del salón de actividades múltiples fue donado por promoción bodas de plata 2008 -2010\n\nConstrucción aula D segundo B fue donada por la promoción bodas de plata 2008."
)

### equipo de computacion y tecnologia
valores_compu_tecno <-c(
  "Laptop donadas por Funsepa",
  "Las mesas de las elecciones han sido donadas, las computadoras  las han donado los padres de familia",
  "Computadoras y muebles para las Computadoras",
  "Cañoneras, equipo de computo",
  "Un aula virtual","Instrumentos y computadoras",
  "Pizarrón electrónico.","Donación por parte del Tribunal Supremo Electoral:\n8 Computadoras, 100 sillas plásticas, 24 sillas de metal, 14 mesas, 80 cajas para archivo, 3 scaner.\n\nDonación por parte de la Municipalidad de Pasaco:\n150 Escritorios hace varios años.",
  "Computadoras, mesas y sillas",
  "El equipo de computo","Las pantallas de las aulas se adquirieron con aporte del 50 por ciento de los padres de familia y el otro 50 por ciento lo puso el Colegio",
  "Anteriormente laa instalaciones del instituto fueron ocupadas por un instituto de Telesecundaria, razón por la cual tienen 3 televisores, una cañonera con el lente quemado, 6 computadoras inservibles, casetes.",
  "Instrumentos y computadoras",
  "Piso del salón de actividades múltiples fue donado por promoción bodas de plata 2008 -2010\n\nConstrucción aula D segundo B fue donada por la promoción bodas de plata 2008.\n\nEquipo de sonido fue donado por  orden INMEBOA profesora María Emma Sandoval.\n\nDonaciones por personas del lugar y promociones del INMEBOA:\n\nAlto parlantes.\nMicrófonos inalámbricos.\nCátedras de profesores.\nBombos, triple, cuádruple.\nCongelador\nRefrigeradora\nEstufa industrial\nToldo",
  "10 Pantallas, 7 computadoras portátiles, sillas de plástico",
  "El TSE y la CGC donaron los escáneres y 5 laptop",
  "Mobiliario  y equipo de computación, scaner y router",
  "Escritorios, archivos, cañonera, televisores, computadora, estantes",
  "Escritorios, computadoras y cátedras",
  "Computadoras donadas por FUNSEPA",
  "Las computadoras y algunos instrumentos.",
  "Tablets para tecnología",
  "Donación por parte del Tribunal Supremo Electoral:\n8 Computadoras, 100 sillas plásticas, 24 sillas de metal, 14 mesas, 80 cajas para archivo, 3 scaner."
)

### mobiliario
valores_mobiliario_donacion <- c(
  "Pupitres de los estudiantes y 2 escritorios.",
  "Mobiliario ( escritorios)",
  "Escritorios","Donación por parte del Tribunal Supremo Electoral:\n8 Computadoras, 100 sillas plásticas, 24 sillas de metal, 14 mesas, 80 cajas para archivo, 3 scaner.\n\nDonación por parte de la Municipalidad de Pasaco:\n150 Escritorios hace varios años.",
  "Estanterías","El mobiliario del laboratorio de computación también fue donado por Plan Internacional, al igual que los instrumentos del laboratorio de ciencias y televisiones con reproductores de DVD para cada aula.",
  "Una refrigeradora\nUna estufa",
  "Remozamientos, pizarras",
  "Escritorios, pizzarones y cátedras para docentes",
  "Mobiliario (pupitres)",
  "5 mesas de madera por parte de una Epesista",
  "Escritorios ejecutivos que están siendo comprados para docentes, escritorios para niños de primaria y preprimaria comprados por estudiantes como proyecto de final de año, por alumnos graduandos",
  "La municipalidad, docentes, padres de familia y ex alumnos residentes en USA han donado escritorios y recurso económico para la construcción del centro educativo"
)

### libros
valores_libros_donacion <- c(
  "Donaciones de unos Libros desde el año 2016",
  "Libros", "\nAliados estratégicos con donaciones:\n\nEDIFY DE GUATEMALA\n\nCapacitación para docentes\n\nCertificación con el modelo CORE\n\nCoach educativo\n\n100 libros de School Bible\n\nPlataforma de lectura ONE TEST\n\nGuatemala próspera:\n\n50 Libros de cada programa: YO LIDERO, YO ACTÚO, LA TRANSFORMACIÓN ESTÁ EN MÍ\n\nSalva a los niños\n\n3 Pizarrones, 6 botiquines de primeros auxilios, 12 balones de diferentes deportes, hojas de papel bond tamaño carta, 6 cajas de marcadores de pizarra y permanentes 10 cajas, lapiceros azul, rojo y negro 3 cajas de cada uno, 12 juegos para actividades lúdicas, 50 pelotas grandes y 50 pequeñas.\n\nEmbajada de Israel: 80 libros de \"El diario de Ana Frank\"\n\nCooperativa \"El Recuerdo\":\n\nBiblioteca infantil\n\nBiblioteca juvenil\n\nAproximadamente 130 folletos de Idioma XINCA\n\nDiplomados del Idioma Xinca y material de apoyo para cada docente, más una certificación por la JUSAC\n\nBanrural:\n\n120 libros de Emprendimiento y productividad\n\nMINERÍA DE AURA\n\nPizarrones móviles\n\nSillas para el laboratorio de computación"
)

### material didactico
valores_didactico_donacion <- c(
  "Recurso didáctico, medicamentos, alimentación.",
  "Material Didáctico"
)

### comida y medicina
valores_medicina_comida <- c(
  "Recurso didáctico, medicamentos, alimentación.",
  "Material Didáctico"
)

### maquinas y herramientas
valores_maquina_donacion <- c(
  "Donación de equipo para las áreas ocupacionales",
  "Maquinas para soldar, erramienta para áreas"
)

###  no especifica
valores_noespecifica_donacion <- c(
  "Ministerio de Educación","La Iglesia de Jesucristo de los Santos de los Últimos días."
)

### instrumentos
valores_instrumentos_donacion <- c(
  "Instrumentos y computadoras"
)

## Recode con case match ----

obs01 <- obs01 |>
  mutate(
    donaciones_tipo = case_match(
      donaciones_tipo,
      all_of(valores_instrumentos_donacion) ~ "Instrumentos",
      all_of(valores_noespecifica_donacion) ~ "No especifica",
      all_of(valores_maquina_donacion) ~ "Máquinas y herramientas",
      all_of(valores_medicina_comida) ~ "Medicina y comida",
      all_of(valores_didactico_donacion) ~ "Material didáctico",
      all_of(valores_libros_donacion) ~ "Libros",
      all_of(valores_mobiliario_donacion) ~ "Mobiliario",
      all_of(valores_compu_tecno) ~ "Equipo de computación y tecnología",
      all_of(valores_infraesctructura_donacion) ~ "Mejoras en infraestructura",
      .default = donaciones_tipo
    )
  )

## volver a ver errores

valores_donaciones_tipo <- obs01 |>
  filter(donaciones_centro == "Sí") |>	
  filter(!(is.na(donaciones_tipo))) |>	
  select(donaciones_tipo) 

#View(valores_donaciones_tipo)

# otros_docentes_extra ----

## ver errores

valores_otros_docentes_extra <- obs01 |>
  filter(docentes_extra == "Sí") |>	
  filter(!(is.na(otros_docentes_extra))) |>	
  select(otros_docentes_extra) 

#dput(valores_otros_docentes_extra)

## Define los vectores ----

### no hay más
valores_no_profesionales <-  c(
  "No hay mas profesionales.",
  "No hay",
  "Instructores de informática, no llegan al centro educativo, los alumnos se movilizan los días jueves al municipio, donde en un espacio municipal reciben el curso de computación, en contra jornada.",
  "Tienen un acuerdo con intecap para que un profesional en computación pueda llegar a dar clases"
)

### docente auxiliares
valores_docente_auxiliar <- c(
  "Un maestro auxiliar en caso de ausencias.",
  "Orientación técnica, maestra auxiliar, maestro de la banda musical",
  "Auxiliares.",
  "Dos maestros",
  "Maestra auxiliar",
  "Auxiliares",   "Hay una maestra municipal que imparte varios cursos en todos los grados. No está registrada en el SIRE.",
  "Maestros sustitutos, voluntarios, exalumnos becados.",
  "Un docente voluntario",
  "Secretaria \nAuxiliares"
)

### psicopedagogia y orientación psicologia
valores_psicopedagogia <- c(
  "En el centro educativo cuentan profesional de apoyo psicopedagógico. Tambien cuentan con 2 profesores de música.",
  "Profesional de Robótica, Contabilidad, área de psicología.",
  "Psicóloga, secretarias",
  "En el centro educativo cuentan profesional de apoyo psicopedagógico. Tambien cuentan con 2 profesores de música.",
  "Orientadora vocacional \nPsicóloga \nSecretarias \nDocentes auxiliares"
)

### de robotica
valores_robotica_profesional <- c(
  "Profesional de Robótica, Contabilidad, área de psicología."
)

### musica
valores_musica_profesional <- c(
  "Orientación técnica, maestra auxiliar, maestro de la banda musical",
  "En el centro educativo cuentan profesional de apoyo psicopedagógico. Tambien cuentan con 2 profesores de música."
)

### tecnicos e industriales
valores_tecnicos_industriales <-  c(
  "1 Catedrático especializado tiempo completo área industrial electricidad; 1 Técnico Operativo III orientación industrial estructuras metálicas; 1 docente pagado por padres de familia  área industrial Carpintería. \n1 catedrática especializada tiempo completo de Cocina y Repostería; 1 catedrática 021 Técnico Operativo III Orientación economía doméstica Corte y Confección; 1 catedrática pagada por padres de familia de Orientación economía doméstica, Belleza.",
  "Herreros, carpintero, electricista",  "Alianzas con intecap y la cooperativa",
  "Instructores de Carpintería, corte de confección, repostería y mecanica general",
  "Ingeniera en alimentos, Licenciada en tecnología."
)

## administrativos y personal de apoyo
valores_administrativo <- c(
  "Orientador, coordinador, secretarias, administradora, personal de limpieza,seguridad",
  "El secretario-contador  del establecimiento",
  "Contador, encargada de audiovisuales, bibliotecaria, secretaria, subdirector",
  "Secretario-contador y directora",
  "Secretaria, director administrativo y director técnico pedagógico",
  "Secretaria \nAuxiliares"
)

## Recode con case match ----

obs01 <- obs01 |>
  mutate(
    otros_docentes_extra = case_match(
      otros_docentes_extra,
      all_of(valores_administrativo) ~ "Administrativos y personal de apoyo",
      all_of(valores_tecnicos_industriales) ~ "Técnicos e industriales",
      all_of(valores_musica_profesional) ~ "Músicos",
      all_of(valores_robotica_profesional) ~ "Profesional en robótica",
      all_of(valores_psicopedagogia) ~ "Psicopedagogo",
      all_of(valores_docente_auxiliar) ~ "Docentes auxiliares",
      all_of(valores_no_profesionales) ~ "No hay",
      .default = otros_docentes_extra
    )
  )

## volver a ver errores

valores_otros_docentes_extra <- obs01 |>
  filter(docentes_extra == "Sí") |>	
  filter(!(is.na(otros_docentes_extra))) |>	
  select(otros_docentes_extra) 

##dput(valores_otros_docentes_extra)

# acciones_docentes_extra ----

## ver errores

valores_acciones_docentes_extra <- obs01 |>
  filter(docentes_extra == "Sí") |>	
  filter(!(is.na(acciones_docentes_extra))) |>	
  select(acciones_docentes_extra) 

#dput(valores_acciones_docentes_extra)

## Definir los vectores ----

### suplencias y apoyo docentes
valores_apoyo_docente <- c(
  "1. Suplencias por maestros ausentes.\n\n2. Apoya actividades extracurriculares.",
  "Sustituir a un docente ausente, auxiliar a docentes en el aula.",
  "Orientadora vocacional es la encargada de observar a los estudiantes\n\nPsicóloga se encarga de los problemas de conducta de los estudiantes \n\nSecretarias son las encargadas de realizar todos los trámites administrativos \n\nDocente auxiliar es el encargado cubrir algun Docente si llegara a faltar",
  "Los auxiliares desarrollan los contenidos y actividades en caso de ausencia de un docente. Cuando un docente va a solicitar permiso por alguna necesidad o emergencia, indica al catedrático auxiliar, los temas y actividades correspondientes a ese día.",
  "1. Impartir clases de contabilidad  ad honorem\n\n2. Maestra de grado contratada por los padres de familia",
  "Cubrir y organizar auxiliaturas en caso de ausencia de los docentes \n\nVelar por la disciplina de los estudiantes \n\nSupervisar pasillos y diferentes espacios en el estacionamiento",
  "Las mismas que la docente", "La profesora de orientación se encarga de apoyar a los docentes en la aplicación del CNB y la elaboración de las planificaciones, el maestro de la banda es el encargado de enseñar a los estudiantes la interpretación musical con los instrumentos. La profesora auxiliar se encarga de apoyar a la dirección y a los docentes en todo lo necesario para que la educación y formación sea integral",
  "Docente auxiliar es el encargado cubrir algun Docente si llegara a faltar",
  "La secretaria funge funciones para todo el establecimiento\n\nAuxiliar uno: alumnos de primero básico.\n\nAuxiliar dos: alumnos de segundo básico.\n\nAuxiliar tres: alumnos de tercero básico."
)

### orientación y acompañamiento
valores_orientacion <- c(
  "Acompañamiento psicopedagógico, que inicia realizándoles a los estudiantes un diagnóstico si en caso detecta algun cuadro distinto a los demás distinto a los demas, inicia con el apoyo, que  puede ser apoyo emocional, psicológico ó pedagógico. En relación a los profesores de música el Colegio se ha caracterizado por siempre tener alumnos que les gusta la música, por eso se auxilian de dos músicos mas que llegan eventualmente para brindarles orientación a los estudiantes en educación musical.",
  "Robótica y contabilidad imparten clases y el área de psicología apoyaba tercero básica orientación vocacional en el último bimestre.",
  "Observar el comportamiento de los estudiantes fuera del aula.", "Asesores  pedagógico",
  "La Psicóloga atiende a estudiantes con problemas familiares, prevención de suicidio y otros que se presentan",
  "Orientadora vocacional es la encargada de observar a los estudiantes\n\nPsicóloga se encarga de los problemas de conducta de los estudiantes \n\nSecretarias son las encargadas de realizar todos los trámites administrativos"
)

### impartir clases
valores_clases <-c(
  "Imparten áreas académicas",
  "Impartir algunos de los cursos del CNB en todos los grados.",
  "Dan cursos como las demás áreas.",
  "Curso de computación",
  "Imparte el curso de computación"
)

### formación técnica u ocupacional
valores_ocupacional_tecnica <-c(
  "Trabajan en las áreas ocupacionales",
  "Imparten talleres de carpintería, corte y confección, repostería y mecánica general de autos y motocicletas."
)

### supervision y coordinacion
valores_supervision <- c(
  "Supervision, administracion, coordinación de todas las actividades planificadas tanto externas como internas.",
  "Apoyar en la coordinación y administración educativa",
  "Orientadora vocacional es la encargada de observar a los estudiantes\n\nPsicóloga se encarga de los problemas de conducta de los estudiantes \n\nSecretarias son las encargadas de realizar todos los trámites administrativos \n\nDocente auxiliar es el encargado cubrir algun Docente si llegara a faltar",
  "Se encarga de todo aspecto administrativo del establecimiento",
  "El contador se encarga de todo lo financiero-administrativo, la encargada de audiovisuales es la responsable del resguardo de cañoneras, pantallas,",
  "El secretario hace la función de contador"
)

### no especifica
valores_sinespecificar_accion <- c(
  "---"
)

## Recode con case match ----

obs01 <- obs01 |>
  mutate(
    acciones_docentes_extra = case_match(
      acciones_docentes_extra,
      all_of(valores_sinespecificar_accion) ~ "No especifica",
      all_of(valores_supervision) ~ "Supervisar y coordinar",
      all_of(valores_ocupacional_tecnica) ~ "Formación técnica u ocupacional",
      all_of(valores_clases) ~ "Impartir clases",
      all_of(valores_orientacion) ~ "Orientar y brindar acompañamiento psicopedagógico",
      all_of(valores_apoyo_docente) ~ "Cubrir y dar apoyo docente",
      .default = acciones_docentes_extra
    )
  )

## volver a ver errores


valores_acciones_docentes_extra <- obs01 |>
  filter(docentes_extra == "Sí") |>	
  filter(!(is.na(acciones_docentes_extra))) |>	
  select(acciones_docentes_extra) 

#View(valores_acciones_docentes_extra)

# ultima_vez_remozamiento----

## ver errores

valores_ultima_vez_remozamiento <- obs01 |>
  filter(remozamiento == "Sí") |>
  filter(!(is.na(ultima_vez_remozamiento))) |>	
  select(ultima_vez_remozamiento) 

#View(valores_ultima_vez_remozamiento)

# otra_area ----

## ver errores

valores_donaciones_tipo <- obs01 |>
  filter(donaciones_centro == "Sí") |>	
  filter(!(is.na(donaciones_tipo))) |>	
  select(donaciones_tipo) 

#dput(valores_donaciones_tipo)