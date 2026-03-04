df_dyd <- read_xlsx(here("Datos","Encuesta docente","df_dyd.xlsx"))

# numero_hijos ----

## ver errores

valores_numero_hijos <- df_dyd |>
  filter(hijos == "Sí") |> 
  filter(!(is.na(numero_hijos))) |>
  select(numero_hijos) 


## Define los vectores ----

### Un hijo

valores_uno <- c(
  "Una hija", "Una", "Uno", "1 hija", "1", "01", "1 hijo",
  "Uno", "1|", "1u", "Hija 1", "Uno (1)", "1 una", "UNO",
  "Un hijo", "1  Hija", "una hija"
)

### Dos hijos

valores_dos <- c(
  "2 hijas", "1 hijo y 1 hija", "dos", "2 hijas", "2", "2 HIJOS",
  "Dos", "DOS", "1 hijo 1 hija", "2 hijos", "1 H 1 M", "2 hijos",
  "Un hijo y una hija", "Dos hijos", "1 niña 1 niño", "2 una niña y un niño",
  "1 hijo 1 hija", "02 HIJOS", "2y"
)

### Tres hijos

valores_tres <- c(
  "3 hijos", "un hijo y dos hijas", "2 Varones y 1 Mujer",
  "Tres", "3", "03", "3 hijas", "2 hijos 1 hija", "2 varones 1 mujer",
  "2 muejeres y un varon", "2 hijas y 1 hijo", "2 hijas y 1 varón",
  "Yres", "1 hombre, 2 mujeres", "3 hijas", "tres", "2 hijos 1 hija",
  "2 hijas y 1 hijo", "2 hijos y una hija", "Tres hijos", "2 hijos  1 hija",
  "3 dos vanos y una niña"
)

### Cuatro hijos

valores_cuatro <- c(
  "tres hijas, un hijo", "3 Hijas, 1 Hijo", "Tres Hijas, un Hijo",
  "Tres Mujeres un Varón", "4", "3 HIJAS 1 HIJO", "3 HIJAS Y 1 HIJO",
  "3 hijas y 1 hombre", "Tres hijos, una hija", "Cuatro", "3 hijas 1 hijo",
  "4 hijos", "4 hijas", "3 varones 1 niña", "2 varones y 2 mujeres"
)

### Cinco hijos

valores_cinco <- c(
  "5 niños", "5"
)

### Seis hijos

valores_seis <- c(
  "6", "3 y 3", "4 hijos y 2 jijas"
)

### Siete hijos

valores_siete <- c(
  "7"
)

### Ocho hijos

valores_ocho <- c(
  "8"
)

### Once hijos

valores_once <- c(
  "11"
)

### Catorce hijos

valores_catorce <- c(
  "14"
)

## Recode case_match ----

df_dyd <- df_dyd |>
  mutate(
    numero_hijos = case_match(
      numero_hijos,
      all_of(valores_uno) ~ "Un hijo",
      all_of(valores_dos) ~ "Dos hijos",
      all_of(valores_tres) ~ "Tres hijos",
      all_of(valores_cuatro) ~ "Cuatro hijos",
      all_of(valores_cinco) ~ "Cinco hijos",
      all_of(valores_seis) ~ "Seis hijos",
      all_of(valores_siete) ~ "Siete hijos",
      all_of(valores_ocho) ~ "Ocho hijos",
      all_of(valores_once) ~ "Once hijos",
      all_of(valores_catorce) ~ "Catorce hijos",
      .default = numero_hijos
    )
  )

## volver a ver errores

valores_numero_hijos <- df_dyd |>
  filter(hijos == "Sí") |> 
  filter(!(is.na(numero_hijos))) |>
  select(numero_hijos) 

# otro autoidentificacion ----

## ver errores
valores_otro_autoidentificacion  <- df_dyd |>
  filter(autoidentificacion == "Otro") |>
  filter(!(is.na(otro_autoidentificacion))) |>
  select(otro_autoidentificacion) 

# View(valores_otro_autoidentificacion)

## Define los vectores ----

### Guatemalteca
valores_guatemalteca  <- c(
  "Guatemalteca", "Guatemalteca con padre asiático"
)

### Xinquin
valores_xinquin  <- c(
  "Xinquin"
)

## Recode case_match ----

df_dyd  <- df_dyd |>
  mutate(
    otro_autoidentificacion = case_match(
      otro_autoidentificacion,
      all_of(valores_guatemalteca) ~ "Guatemalteca",
      all_of(valores_xinquin) ~ "Xinquin",
      .default = otro_autoidentificacion
    )
  )

## volver a ver errores

valores_otro_autoidentificacion  <- df_dyd |>
  filter(autoidentificacion == "Otro") |>
  filter(!(is.na(otro_autoidentificacion))) |>
  select(otro_autoidentificacion) 

# otro transporte ----

## ver errores

valores_otro_transporte <- df_dyd |>
  filter(transporte == "Otro") |>
  filter(!(is.na(otro_transporte))) |>
  select(otro_transporte) |>
  tabyl(otro_transporte)

# View(valores_otro_transporte)

## Define los vectores ----

### autobus

valores_autobus <- c(
  "Bus  publico", "EN BUS"
)

### lacustre

valores_lacustre <- c(
  "Lacustre"
)

### vehiculo particular

valores_vehiculo <- c(
  "Particular", "Vehículo privado"
)

### varios
valores_varios <- c(
  "Taxi, mucrobus, camioneta y lancha"
)

## Recode case_match ----

df_dyd  <- df_dyd |>
  mutate(
    otro_transporte = case_match(
      otro_transporte,
      all_of(valores_autobus) ~ "Autobus",
      all_of(valores_lacustre) ~ "Lacustre",
      all_of(valores_vehiculo) ~ "Vehículo particular",
      all_of(valores_varios) ~ "Varios transportes",
      .default = otro_transporte
    )
  )

## volver a ver errores

valores_otro_transporte <- df_dyd |>
  filter(transporte == "Otro") |>
  filter(!(is.na(otro_transporte))) |>
  select(otro_transporte) |>
  tabyl(otro_transporte)



# otro_tiempo_llegada ----

## ver errores

valores_otro_tiempo_llegada <- df_dyd |>
  filter(tiempo_llegada == "Otro") |>
  filter(!(is.na(otro_tiempo_llegada))) |>
  select(otro_tiempo_llegada) |>
  tabyl(otro_tiempo_llegada)

# View(valores_otro_tiempo_llegada)

## Define los vectores ----

### Menos de 5 minutos

valores_menos_cinco <- c(
  "3 minutos", "4", "5 minutos"
)

### De 5 a 10 minutos

valores_cinco_diez <- c(
  "10", "10 minutos", "Menos de 10 minutos", "6"
)

### De 15 a 20 minutos

valores_quince_veinte <- c(
  "15", "15 a 20 minutos", "15 minutos",
  "20 minutos"
)

### De 20 a 30 minutos

valores_veinte_treinta <- c(
  "30 minutos", "Menos de 30 minutos"
)

### 1 hora

valores_una_hora <- c(
  "1:00"
)

### 5 a 6 horas

valores_cinco_seis <- c(
  "5 horas", "6 horas"
)

## Recode case_match ----

df_dyd  <- df_dyd |>
  mutate(
    otro_tiempo_llegada = case_match(
      otro_tiempo_llegada,
      all_of(valores_menos_cinco) ~ "Menos de 5 minutos",
      all_of(valores_cinco_diez) ~ "5 a 10 minutos",
      all_of(valores_quince_veinte) ~ "15 a 20 minutos",
      all_of(valores_veinte_treinta) ~ "20 a 30 minutos",
      all_of(valores_una_hora) ~ "1 hora",
      all_of(valores_cinco_seis) ~ "5 a 6 horas",
      .default = otro_tiempo_llegada
    )
  )

## Volver a ver errores

valores_otro_tiempo_llegada <- df_dyd |>
  filter(tiempo_llegada == "Otro") |>
  filter(!(is.na(otro_tiempo_llegada))) |>
  select(otro_tiempo_llegada) |>
  tabyl(otro_tiempo_llegada)


# Tipo_especialización diversificado ----

## Ver errores

valores_tipo_especializacion_diversificado <- df_dyd |>
  filter(especializacion_diversificado == "Sí") |>
  filter(!(is.na(tipo_especializacion_diversificado))) |>
  select(tipo_especializacion_diversificado) |>
  tabyl(tipo_especializacion_diversificado)

# View(valores_tipo_especializacion_diversificado)
#dput

## Define los vectores ----

### Administracion

valores_administracion <- c(
  "Administración Publica", "Administración Turística y Hotelera",
  "Administración de Centros Educativos", "Técnico en Admon",
  "Administración de Empresas", "Administración de Empresas y Computación",
  "Administración de Hoteles", "Administración de empresas",
  "En Administración", "En Administración de Empresas y Computación",
  "En Administración de empresas", "Perito Administración Pública",
  "Perito en Administracion de empresas con Orientación en computación",
  "Perito en Administraciòn de Empresas", "Administración Pública",
  "Perito en Administración Turística y Hotelera",
  "Perito en Administración de Empresas",
  "Perito en Administración de Empresas con Orientación en Computación",
  "Perito en administracion de empresas",
  "Perito en administración pública", "Turismo en Administración en Hoteleria",
  "Turismo en Administración en el Hoteleria."
)

### Computación

valores_computacion <- c(
  "BACHILLERATO EN COMPUTACION", "Bach. Industrial con especialidad en Computación",
  "Bach. Industrial y Perito en Computación",
  "Bachiller Industrial y Perito con Especialidad en Computación",
  "Bachiller con orientación en computación",
  "Bachiller en Ciencia y Letras con Orientación en Computación",
  "Bachiller en Ciencia y Letras con Orientación en Computación.",
  "Bachiller en Ciencia y Letras con Orientación en Computación",
  "Bachiller en Ciencia y Letras con Orientación en Computación.",
  "Bachiller en Ciencias y Letras con Orientación en Computación",
  "Bachiller en Ciencias y Letras con Orientación en computación",
  "Bachiller en Computacion Con Orientación Comercial",
  "Bachiller en Computacion con Orientacion Cientifica",
  "Bachiller en Computación con Orientación Científica",
  "Bachiller en Computación con Orientación Comercial",
  "Bachiller en ciencias y letras con orientación en computación",
  "Bachiller en computación con orientación Científica",
  "Bachiller en computación con orientación científica",
  "Bachiller en computación con orientación comercial",
  "Bachiller y Perito con Especialidad en Computación",
  "Bachillerato En Computación Con Orientación Comercial",
  "Bachillerato en Ciencia y Letras Con Orientación en Computación.",
  "Bachillerato en Ciencias y Letras Con orientación en computación",
  "Bachillerato en Ciencias y Letras con Orientación en Computación",
  "Bachillerato en Ciencias y Letras con orientación en Computación",
  "Bachillerato en Ciencias y letras con especialización en Computación",
  "Bachillerato en Computación",
  "Bachillerato en Computación con Orientación Científica",
  "Bachillerato en Computación con Orientación Comercial",
  "Bachillerato en Computación con Orientación científica",
  "Bachillerato en Computación y Comercial",
  "Bachillerato en ciencias y letras con orientación en computación",
  "Bachillerato en computación", "Bachillerato en computación comercial",
  "Bachillerato en computación con orientación científica",
  "Bachillerato en computación en especialización en calidad",
  "Ciencias y letras con Orientación en computación",
  "Computacion", "Computación", "Computación con Orientación Comercial",
  "Computación con Orientación Comercial.",
  "Computación con orientación comercial", "Con Orientación en Computacion",
  "Con Orientación en Computación", "Con especialización en Computación",
  "Con orientacion en computación", "Con orientación en compuración",
  "Contador con Orientación en Computación", "En Computación",
  "En Computación con Orientación Científica",
  "En Computación con Orientación Comercial",
  "En Computación con Orientación científica", "En computación",
  "En Computación con orientación comercial", "En computacion",
  "En computación y soporte técnico", "En computación.",
  "Especialidad en computación", "Inglés y computación",
  "Orientación en Computación", "Orientación en computación",
  "Perito Con Especialidad en Computacion", "Perito en computacion",
  "Perito Contador Con Orientación En Computación",
  "Perito Contador con Especialidad en Computación",
  "Perito Contador con Orientación en computación",
  "Perito Contador con orientación en Computación",
  "Perito Contador con orientación en computación",
  "Perito contador con Orientación en Computación",
  "Perito contador con Orientación en computación",
  "Perito contador con orientaciók en computacion comercial",
  "Perito contador con orientación en computación",
  "Perito en computación", "Perito en informática y computación",
  "Secretaria Bilingue con especialidad en Computación",
  "Secretaria Bilingüe con orientación en Computación",
  "Secretariado Bilingüe en computación", "Secretariado bilingue en computación",
  "Secretariado bilingüe con orientación en computación", "Secretariado en Computación",
  "bachillerato en ciencias y letras con orientacion en computacion",
  "computacion", "en computacion", "en computación con orientación científica",
  "perito contador con orientación en computación",
  "orientacion en conputacion",
  "Bachiller en Ciencia y Letras con  Orientación en Computación",
  "Bachiller en Ciencia y Letras con  Orientación en Computación.",
  "Con orientación en computación",
  "Perito Contador con Orientación en Computación",
  "bachillerato en ciencias y letras con  orientacion en computacion"
)

### Educación

valores_educacion <- c(
  "en Educación", "maestro de educacion fisica", "BACHILLER EN CIENCIAS Y LETRAS CON ORIENTACIÓN EN EDUCACIÓN",
  "Bachiller en Ciencias y Letras con Orientación En Educacion",
  "Bachiller en Ciencias y Letras con Orientación en Educación",
  "Bachiller en Ciencias y Letras con Orientación en Educación Musical",
  "Bachiller en Ciencias y letras con Orientación en Educación",
  "Bachiller en ciencias letras con oriencion en educacion",
  "Bachiller en ciencias y letras con orientación en Educación Física",
  "Bachiller en ciencias y letras con orientación en educación",
  "Bachiller en ciencias y letras con orientación en educación musical",
  "Bachiller en ciencias y letras con orientación en la educación",
  "Bachiller en educación", "Bachiller en educación física",
  "Bachillerato en Educación", "Bachillerato ciencias y letras con orientación en Educación Física",
  "Bachillerato con Orientación en Educación Física",
  "Bachillerato con educación", "Bachillerato en Cianciaby Letras con orientación en Educación Física",
  "Bachillerato en Ciencias y Letras con Orientación en Educación",
  "Bachillerato en Ciencias y Letras con Orientación en Educación Musical",
  "Bachillerato en Ciencias y Letras con Orientación en Educación Músical",
  "Bachillerato en Ciencias y Letras con Orientación en Educación en Productividad y Desarrollo",
  "Bachillerato en Educación con Orientación en Productividad y Desarrollo",
  "Bachillerato en Educación", "Bachillerato en educación",
  "Bachillerato en ciencias letras con orientacion en educacion",
  "Bachillerato en ciencias y letras con orientación en educación",
  "Bachillerato en ciencias y letras en orientación en educación",
  "Con Orientación en Educación", "EN EDUCACION", "Educación",
  "Educación Bilingüe Español - Inglés", "Educación Bilingüe Español-Inglés",
  "Educación Física", "Educación Musical", "Educación física",
  "Educación musical", "En Educación", "En Educación Física",
  "En Educación de Productividad y Desarrollo", "En Educación física",
  "En ciencias de la educación", "En educación",
  "En orientación en Educación", "Maestro de educación primaria con orientación bilingüe",
  "Orientación en Educación", "Orientación en Educación Física",
  "Orientación en educación", "Perito contador com orientación en Educación",
  "con especialización en Educación", "Pedagogia",
  "Preparación para la enseñanza de los estudiantes.",
  "Bachillerato  en Educación"
)

### Diseño grafico
valores_diseno <- c(
  "Diseño Gráfico", "Diseño gráfico", "En Diseño Gráfico",
  "En diseño gráfico"
)

### perito contador
valores_contador <- c(
  "CONTADOR", "Contador", "Económico contable", "Orientacion en contabilidad",
  "Perito Contador", "Perito contador", "Perito contador Con orientacion en empresas agricolas",
  "CONTABILIDAD"
)

### perito agronomo
valores_agronomia <- c(
  "Perito Agronomo", "Perito agronomo con especialidad en apicultura",
  "Perito agronomos.", "agronomo", "AGRONOMO", "Agronomo",
  "Perito Agrónomo"
)

### bachiller industrial y perito
valores_industrial_perito <- c(
  "Bach. Industrial y perito en electrónica digital y microprocesadores",
  "Bachiller Industrial", "Bachiller Industrial y Perito en Artes Grafícas",
  "Bachiller Industrial y Perito en Diseño de Modas",
  "Bachiller Industrial y Perito en Electricidad",
  "Bachiller Industrial y Perito en Electricidad.",
  "Bachiller Industrial y Perito en Electrónica", "Bachiller insdustrial y perito en carpintería",
  "Bachiller Industrial y Perito en Mecánica Automotriz",
  "Bachiller Industrial y Perito en refrigeración y Aire Acondicionado",
  "Bachiller Industrial y Périto en Electricidad",
  "Bachiller Industrial y perito en carpintería",
  "Bachiller industrial y Perito en Electricidad",
  "Bachiller industrial y perito en construcción carpintería",
  "Bachiller industrial y perito en electricidad",
  "Bachiller industrial y perito en mecánica automotriz",
  "Bachillerato Industrial y Perito en Dibujo de Construcción",
  "Bachillerato Industrial y Perito en Electricidad",
  "Bachillerato industrial y perito en carpintería", "Costura Industrial",
  "Industrial", "bachiller industrial y perito en mecanica general tornos",
  "industrial y perito en Electrónica digital y micro procesadores",
  "Bach. Industrial y perito en electrónica  digital y microprocesadores"
)

### mecanica
valores_mecanica <- c(
  "Bachiller en Mecanica General", "Bachiller en Mecánica General",
  "Mecanica tornos", "Perito en Metal Mecanica",
  "Perito en Mecánica Automotriz", "Bachillerato en mecánica general",
  "Mecánica Automotriz", "Mecánica General",
  "Mecánica General con Especialización en Soldadura",
  "Mecánica Industrial", "Mecánica automotriz",
  "Perito en mecánica automotriz", "Con Orientación en Mecánica Automotriz"
)

### mercadotecnia
valores_mercadotecnia <- c(
  "Perito en mercadotecnia y publicidad", 
  "En Mercadotecnia y Publicidad", "En mercadotecnia y publicidad",
  "Mercadotecnia", "Mercadotecnia y Publicidad",
  "Mercadotecnia y publicidad", "Perito en Mercadotecnia",
  "Perito en Mercadotecnia", "Perito en Mercadotecnia Y publicidad",
  "Perito en Mercadotecnia y Publicidad", "Licenciatura en Mercadeo"
)

### electricidad
valores_electricidad <- c(
  "Perito en Electricidad", "Perito en electricidad",
  "Périto en electricidad", "electricidad", 
  "Con orientacion en electricidad y soldadura",
  "Electricidad", "Electricidad industrial", "Electrónico",
  "En Electricidad", "En electricidad", "PERITO EN ELECTRICIDAD",
  "Electronica", "Electrónica", "Electrónica industrial",
  "Perito en electrónica con especializacion enElectrónica industrial"
)

### construccion
valores_construccion <- c(
  "PERITO EN CONSTRUCCIÓN CARPINTERIA", "Perito en Construcción",
  "Perito en Construcción Carpintería", "Dibujo en Construcción",
  "Perito en Construcción de Albañilería",
  "Perito en Dibujo en construcción", "Bachiller en Dibujo de Construcción",
  "Bachiller en dibujo construccion", "En ingenieria y Arquitectura",
  "Bachillerato en Construcción", "Dibujo de construccion",
  "Bachillerato en Dibujo de Construcción", 
  "Bachillerato y Dibujo Técnico en Construcción",
  "Dibujo en construccion", "En dibujo técnico en construcción",
  "Perito en costruccion carpintería.", "En Ingenieria y arquitectura"
)

### secretariado y/o bilingue
valores_bilingue <- c(
  "Bilingue", "Bilingue español ingles", "Bilingüe",
  "Bilingüe (inglés -castellano)", "Bilingüe Español inglés",
  "Bilingüe Español-Inglés", "Bilingüe Ingles- Español. Español- Ingles",
  "Bilingüe Inglés - Español", "Bilingüe con orientación en Turismo",
  "Con orientacion primaria bilingüe intercultural",
  "Ejecutiva Bilingüe", "Ejecutivo Bilingüe", "Secretaria Bilingüe",
  "SECRETARIADO BILINGUE", "Secretaria Bilingue", 
  "Secretaria Bilingüe (Español-Inglés) con Orientación en Idiomas (Alemán-Francés), Diplomacia y Relaciones Internacionales",
  "Secretaria Bilingüe (español-inglés)",
  "Secretaria Bilingüe con Orientación en Turismo, Locución y Aviación",
  "Secretaria Bilingüe y Bachiller en CL",
  "Licenciatura en educación primaria  intercultural  con énfasis en educación bilingüe",
  "Secretaria Ejecutiva Bilingue", "Secretaria Ejecutiva Bilingüe",
  "Secretariado Bilingue", "Secretariado Bilingüe",
  "Secretariado Ejecutivo Bilingüe", "Secretariado bilingue",
  "Secretariado bilingüe", "Secretariado comercial",
  "Secretariado y Oficinista", "Secretaria Comercial y Oficinista",
  "Secretaria ejecutiva en español", "Secretaria y Oficinista",
  "Secretaria y oficinista", "Oficinista", "Inglés/Español",
  "Idioma Inglés", "Ingles", "Inglés", "Orientación en inglés",
  "Orientación en inglés", "Diplomado en Ingles"
)

### turismo
valores_turismo <- c(
  "Turismo", "Turismo y admon hoteles", 
  "Con Orientación en Turismo", "En Hoteleria y Turismo",
  "En Turismo y Líneas Aéreas", 
  "Especialidad en Turismo y Líneas Aéreas",
  "ORIENTACION EN TURISMO", "Orientación en Turismo y Ecoturismo",
  "Perito en agroecoturismo"
)

### orientacion ciencias exactas
valores_cientifica <- c(
  "Orientación Científica", "Orientación en Ciencias Biológicas",
  "Orientación en ciencias biológicas", "Científica",
  "Bachiller en Ciencias y Letras con orientación en ciencias biológicas",
  "En ciencias biologícas", "matemática", "Pem en Matematica",
  "Matemática y Física", "Matemáticas",
  "En Matemática y Física", "En Matemstica"
)

### ciencias comunicacion
valores_comunicacion <- c(
  "Bach, en ciencias de la comunicación",
  "CIENCIAS DE LA COMUNICACIÓN",
  "Perito en ciencias de la comunicación social, locución y periodismo"
)

### dibujo
valores_dibujo <- c(
  "Perito en dibujo de arquitectura e ingeniería",
  "dibujo en conscrucción con orientación en dibujo de planos por computadora"
)

### tecnicas y artisticas
valores_tecnicas <- c(
  "perito en carpintería", "Tecnico artístico en artes plasticas",
  "Productividad y Desarrollo", "Perito en Soldadura",
  "Orientación en Textiles", "Música", "Músico", 
  "En música", "En Productividad y Desarrollo", "En Música",
  "Cultora de Belleza", "Artes plásticas con especialidad en pintura",
  "Orientación en musica", "Procesamiento de alimentos"
)

### juridica
valores_juridico <- c(
  "Especialidad Juridica", "Orientacion juridica"
)

### ambiental
valores_ambiental <- c(
  "Con Orientación Ambiental", "En Medio Ambiente",
  "Con Orientacion en Medio Ambiente"
)

### comercial
valores_comercial <- c(
  "Ciencias Comerciales", "Comercial",
  "Con orientación comercial", "Orientación Comercial",
  "Orientación comercial"
)

### enfermeria y medicina
valores_medicina <- c(
  "Con orientación en Medicina", "En Enfermería"
)

### ciencias y letras
valores_letras <- c(
  "BACHILLERATO EN CIENCIA Y LETRAS", 
  "Bachiller en Ciencias", "Bachiller en Ciencias y Letras",
  "Bachillerado en ciencias y letras", "Bachillerato en Ciencias y Letras",
  "Bachillerato en ciencias y letras", "En Ciencias y Letras",
  "Ciencias y Letras", "Ciencias y letras", "En ciencias y letras"
)

### ciencias sociales
valores_sociales <- c(
  "Diplomado en relaciones internacionales", "Psicología"
)

### sin especificar
valores_sin_especificar <- c(
  "No", "Por Madurez"
)

## Recode case_match ----

df_dyd  <- df_dyd |>
  mutate(
    tipo_especializacion_diversificado = case_match(
      tipo_especializacion_diversificado,
      all_of(valores_sin_especificar) ~ "Sin especificar",
      all_of(valores_sociales) ~ "Ciencias sociales",
      all_of(valores_letras) ~ "Ciencias y Letras",
      all_of(valores_medicina) ~ "Enfermería y Medicina",
      all_of(valores_comercial) ~ "Orientación Comercial",
      all_of(valores_ambiental) ~ "Orientación Ambiental",
      all_of(valores_juridico) ~ "Orientación Jurídica",
      all_of(valores_tecnicas) ~ "Técnica y artística",
      all_of(valores_dibujo) ~ "Orientación en dibujo",
      all_of(valores_comunicacion) ~ "Ciencias de la comunicación",
      all_of(valores_cientifica) ~ "Ciencias exactas",
      all_of(valores_turismo) ~ "Turismo",
      all_of(valores_bilingue) ~ "Secretariado y/o bilingüe",
      all_of(valores_construccion) ~ "Perito en construcción",
      all_of(valores_electricidad) ~ "Perito en electricidad",
      all_of(valores_mercadotecnia) ~ "Perito en mercadotecnia",
      all_of(valores_mecanica) ~ "Mecánica general o automotriz",
      all_of(valores_contador) ~ "Perito contador",
      all_of(valores_industrial_perito) ~ "Industrial y perito",
      all_of(valores_diseno) ~ "Diseño gráfico",
      all_of(valores_agronomia) ~ "Perito agrónomo",
      all_of(valores_educacion) ~ "Orientación en Educación",
      all_of(valores_computacion) ~ "Orientación en Computación",
      all_of(valores_administracion) ~ "Perito en administración",
      .default = tipo_especializacion_diversificado
    )
  )

## volver a ver errores

valores_tipo_especializacion_diversificado <- df_dyd |>
  filter(especializacion_diversificado == "Sí") |>
  filter(!(is.na(tipo_especializacion_diversificado))) |>
  select(tipo_especializacion_diversificado) |>
  tabyl(tipo_especializacion_diversificado)

# View(valores_tipo_especializacion_diversificado)


# Que_estudia_actualmente ----

## Ver errores 
valores_que_estudia_actualmente <- df_dyd |>
  filter(estudios_actuales == "Sí") |>
  filter(!(is.na(que_estudia_actualmente))) |>
  select(que_estudia_actualmente) 

# View(valores_que_estudia_actualmente)

## Define los vectores ----

### pedagogia
valores_pedagogia <- c(
  "1. Licenciatura en Pedagogía y Derechos Humanos. 2. Licenciatura en Educación Primaria Intercultural",
  "Cierre de la carrera de Licenciatura en Pedagogía y Ciencias de la Educación",
  "Cierre de la carrera de la Licenciatura en Pedagogía y Ciencias de la Educación",
  "EPS Licenciatura en Pedagogía", "LICENCIATURA EN PEDAGOGIA",
  "Examen privado de licenciatura en Pedagogía",
  "LICENCIATURA EN PEDAGOGÍA Y CIENCIAS DE LA EDUCACIÓN",
  "Licenciatura  en pedadogia y administración educativa",
  "Licenciatura en  pedagogía y Administración educativa",
  "Pedagogía,  licenciatura deportes",
  "1. Licenciatura en Pedagogía y Derechos Humanos. 2.  Licenciatura en Educación Primaria  Intercultural",
  "LICENCIATURA EN PEDAGOGIA Y ADMINIDTRACION EDUCATIVA",
  "LICENCIATURA EN PEDAGOGÍA Y ADMINISTRACIÓN EDUCATIVA",
  "Licencia en pedagogía y administración educativa",
  "Licenciada en pedagogía y Administración Educativa.",
  "Licenciatura en pedadogia y administración educativa",
  "Licenciatura En Pedagogía y Administración Educativa",
  "Licenciatura en pedagogía y Administración educativa",
  "Licenciatura en Pedagogia y Administracion Educativa",
  "Licenciatura en Pedagogia y Derechos Humanos", "Licenciatura en Psicopedagogía",
  "Licenciatura en Pedagogía", "Licenciatura en Pedagogía y Administración Educativa",
  "Licenciatura en Pedagogía Y Administración Educativa",
  "Licenciatura en Pedagogía con Especialidad en Administración Educativa",
  "Licenciatura en Pedagogía con especialidad en Ciencias Naturales",
  "Licenciatura en Pedagogía con Énfasis en Administración Educativa",
  "Licenciatura en Pedagogía con Énfasis en Diseño Curricular",
  "Licenciatura en Pedagogía con Énfasis en Idioma e Historia Maya Kaqchikel",
  "Licenciatura en Pedagogía de Derechos Humanos",
  "Licenciatura en Pedagogía e Investigación Educativa",
  "Licenciatura en Pedagogía y Administración Educativa",
  "Licenciatura en Pedagogía y Administración Educativa Y Técnico en Administración de empresas",
  "Licenciatura en Pedagogía y Administración Educativa con Orientación en Medio Ambiente",
  "Licenciatura en Pedagogía y Administración Educativa con orientación en Medio Ambiente",
  "Licenciatura en Pedagogía y Ciencias de la Educación ducación",
  "Licenciatura en Pedagogía y Derechos Humanos",
  "Licenciatura en Pedagogía y Derechos humanos",
  "Licenciatura en Pedagogía y Planificación Curricular",
  "Licenciatura en Pedagogía y Técnico en Administración Educativa",
  "Licenciatura en Pedagogía y Técnico en Administración Educativa.",
  "Licenciatura en Pedagogía y Técnico en Admón. Educativa",
  "Licenciatura en Pedagogía y Técnico en Ciencias de la Educación",
  "Licenciatura en Pedagogía y administración Educativa",
  "Licenciatura en Pedagogía y administración educativa con orientación en medio ambiente",
  "Licenciatura en Pedagogía y en Administración Educativa",
  "Licenciatura en Pedagogía y profesorado de educación media con especialidad en Comunicación y lenguaje",
  "Licenciatura en Profesorado en Pedagogía de Enseñanza Media y Técnico en Administración Educativa",
  "Licenciatura en pedagogia", "Licenciatura en pedagogía",
  "Licenciatura en pedagogia,especialidad gastronomica", 
  "Licenciatura en pedagogía en especialidad administración educativa",
  "Licenciatura en pedagogía y Administración Educativa",
  "Licenciatura en pedagogía y Administración educativa",
  "Licenciatura en pedagogía y administración educativa",
  "Licenciatura en pedagogía y ciencias de la educación",
  "Licenciatura en pedagogía y derechos humanos",
  "PEM EN PEDAGOGÍA Y ADMINISTRACIÓN EDUCATIVA",
  "PEM en Pedagogía y Administración Educativa",
  "PEM en Pedagogía y Ciencias Naturales con Orientación Ambiental",
  "PEM en Pedagogía y Técnico en Administración Educativa",
  "PEM en pedagogia comunicación y lenguaje",
  "PEM en pedagogía y administración educativa",
  "PEM en pedagogía y tecnico en administracion educativa",
  "PEM. En Pedagogía Administrativa",
  "PEM. En pedagogía y Técnico de administración educativa con orientación en medio ambiente",
  "PEM. En pedagogía y Técnico en Administración Educativa",
  "PROFESORADO DE ENSEÑANZA MEDIA EN PEDAGOGÍA Y TÉCNICO EN ADMINISTRACIÓN EDUCATIVA",
  "PSE EN PEDAGOGÍA CON ORIENTACIÓN EN DIRECCIÓN Y ADMINISTRACIÓN DE CENTROS EDUCATIVOS",
  "PSE en Pedagogía y CC SS", "Pedagogía",
  "Pedagogía y TAE", "Pedagogía, licenciatura deportes",
  "Pem en pedagogía y tic.", "Pem pedagogía y administración educativa",
  "Pen en pedagogía", "Privado de licenciatura en Pedagogía",
  "Profesado en enseñanza media, en pedagogía, especialización en comunicación y lenguaje.",
  "Profesor de Enseñanza Media en Pedagogía con Especialidad en Culturas e Idiomas Maya, Garifuna o Xinka",
  "Profesorado de Enseñanza Media en Pedagogia con especialidad en Productividad y Desrrollo",
  "Profesorado de Enseñanza Media en Pedagogía con Especialización en Educación Artística",
  "Profesorado de Enseñanza Media en Pedagogía con Técnico en Administración Educativa con Orientación en Medio Ambiente",
  "Profesorado de Enseñanza Media en Pedagogía y Ciencias Naturales con Orientación Ambiental",
  "Profesorado de Enseñanza Media en Pedagogía y Técnico en Administración Educativa",
  "Profesorado de Enseñanza Media en Pedagogía y Técnico en Administración Educativa con Orientación en Medio Ambiente",
  "Profesorado de Enseñanza Media en Pedagogía y Técnico en Administración Educativa j",
  "Profesorado de Enseñanza Media en Pedagogía y Técnico en Administración Educativa. (Tengo el cierre)",
  "Profesorado de Enseñanza Media en Pedagogía y Técnico en Administración Educativa. Licenciatura en Pedagogía y Administración Educativa",
  "Profesorado de enseñanza media en pedagogia y ciencias de la comunicación",
  "Profesorado de enseñanza media en pedagogia y técnico en administración educativa",
  "Profesorado de enseñanza media en pedagogía y técnico administración educativa",
  "Profesorado en Lengua y literatura (P.C) y licenciatura en Pedagogía yAdmon. Educativa",
  "Profesorado en Pedagogía, Ciencias de la Educación",
  "Profesorado en Segunda Enseñanza en Pedagogía y Ciencias Sociales",
  "Profesorado en pedagogía",
  "Profesorado en pedagogía y técnico en Administración educativa",
  "Profesorado en pedagogía, física y matemática",
  "Profesorado en segunda enseñanza en pedagogía y ciencias sociales Educación y Desarrollo",
  "Ya cerré mi licenciatura en Pedagogía",
  "licenciatura en pedagogia y administracion educativa",
  "licenciatura en pedagogia y administraciónn educativa",
  "licenciatutra en pedagogia",
  "Licenciatura en Pedagogía  y Administración Educativa",
  "Licenciatura en Pedagogía con Énfasis en Administración  Educativa"
)

### ciencias juridicas y sociales
valores_juridica <- c(
  "Ciencias Juridicas y Sociales", "Licenciatura en Ciencias Jurídicas Abogado y Notario",
  "Licenciatura en Ciencias Juridicas y Sociales, Abogado y Notario",
  "Licenciatura en Ciencias Jurídicas y Sociales",
  "Licenciatura en Ciencias Jurídicas y Sociales Abogado y Notario",
  "Licenciatura en Ciencias jurídicas y sociales",
  "Licenciatura en ciencias Jurídicas y Sociales",
  "Licenciatura en ciencias jurídicas y sociales",
  "Licenciatura en ciencias jurídicas y sociales, abogacía y notariado",
  "Licenciatura en ciencias jurídicas y sociales.",
  "Ciencias Jurídicas y Sociales", "Lic. Ciencias Jurídicas y Sociales"
)

### abogacia y notariado
valores_abogacia <- c(
  "Abogacía y Notariado", "Abogado y Notario Licenciado en Ciencias Jurídicas y Sociales",
  "Abogado y notario", "DERECHO", "Derecho", "Maestria en Dere ho Penal"
)

### administracion educativa
valores_administracion_educativa <- c(
  "Administración Educativa", "Administración educativa",
  "Cerré Administracion educativa",
  "Ciencias Sociales y Licenciatura en Administración Educativa",
  "Estoy haciendo la Tesis de la licenciatura en Administración Educativa",
  "LIC. en Administración Educativa", "Licenciatura en Administración Educativo",
  "LICENCIATURA EN ADMINISTRACION EDUCATIVA", 
  "LICENCIATURA EN ADMINISTRACIÓN EDUCATIVA",
  "Licencenciatura en Administración Educativa", 
  "Licenciatura en Administracion Educativa", "PEM con técnico en educación administrativa",
  "Licenciatura en Administración Educativa",
  "Licenciatura en Administración Educativa con gerencia de Calidad",
  "Licenciatura en Administración Educativa, PEM en Matemáticas",
  "Licenciatura en administracion educativ", "Pensum cerrado del PEM con Técnico Administrativo",
  "Licenciatura en administración Educativa",
  "Licenciatura en administración educativa",
  "Locenciada en administración educativa",
  "Maestría en Administración Educativa con Orientación en Currículo",
  "Profesorado de enseñanza media y técnico en Administración Educativa",
  "Profesorado de enseñanza media y técnico en administración educativa",
  "Maistría en educación bilingue con orientación en Administración Educativa",
  "Profesorado en Enseñanza Media y Técnico en Administración Educativa"
)

#### psicologia
valores_psicologia <- c(
  "Cierre en licenciatura en psicología y maestría en Neuropsicología Educativa",
  "Licenciatura en Psicología", "Sicología en la Educación",
  "Licenciatura en Psicología Clínica",
  "Licenciatura en psicología clínica y maestría en psicología clinica",
  "Licenciatura en psicología educativa clinica",
  "Licenciatura en psicología general",
  "Psicología Clínica"
)

### ingenieria en sistemas
valores_sistemas <- c(
  "Ing en sistemas", "Ingenieria en sistemas", "Licenciatura en grado academico en sistemas",
  "Ingeniería en Sistemas", "Ingeniería en Sistemas de Información y Comunicación",
  "Ingeniería en Sistemas y ciencias de la computación",
  "Licenciatura en grado academico  en sistemas",
  "Ingeniería en sistemas de información y ciencias de la computación"
)

### ingenieria civil
valores_civil <- c(
  "Ingeniería Civil y Licenciatura en Ciencias de la Comunicación con Especialidad en Publicidad y Relaciones Públicas",
  "Ingeniería civil"
)

### ingenieria industrial
valores_industrial <-c(
  "Ingeniería Industrial" 
)

### admin. de empresas
valores_empresas <- c(
  "Administración de empresas con especialización en transformación digital",
  "Doctorado en Administración de Empresas", "LICENCIATURA EN ADMINISTRACIÓN DE EMPRESA",
  "Licenciatura en Administración de Empresas"
)

### ciencias naturales y educacion ambiental
valores_naturales_ambiental <- c(
  "Lic. En educación ambiental", "Licenciatura en enseñanza en ciencias naturales y educación ambiental",
  "Licenciatura en educación ambiental",
  "PEM en ciencias Naturales y educación ambiental",
  "PEM en ciencias naturales con orientacion ambiental",
  "Profesorado ciencias Naturales con Orientación Ambiental",
  "Cultura ambiental y desarrollo de las ciencias naturales",
  "LICENCIATURA EN EDUCACIÓN AMBIENTAL", "En Medio Ambiente",
  "Licenciatura en orientación en medio ambiente",
  "Profesorado en Ciencias Naturales",
  "Licenciatura  en enseñanza en ciencias naturales y educación ambiental"
)

### docencia universitaria

valores_docencia <- c(
  "Docencia Universitaria", "Docencia superior, coaching en liderazgo",
  "Maestria en Docencia Universitaria", 
  "Maestría en Docencia Universitaria",
  "Maestría en Docencia Universitaria con Orientación en Estrategias de Aprendizaje",
  "Maestría en Docencia universitaria con énfasis en tennologia educativa",
  "Maestría en Educación con especialidad en Docencia Universitaria",
  "Maestría en docencia universitaria"
)

### licenciatura en arte
valores_arte <- c(
  "Licenciatura en Arte", "Licenciatura en Arte y Licenciatura en Teologia Sistematica",
  "Licenciatura en Artes visuales", "Licenciatura en Historia del Arte",
  "Tesis licenciatura en arte" 
)

### profesorado artes
valores_artes <- c(
  "Profesorado en artes plásticas",
  "Profesorado en Artes Plásticas",
  "PEM en Artes Plásticas e Historia del Arte y Licenciatura en Arte"
)

### profesorado enseñanza media
valores_media <- c(
  "Profesorado en educación media con especialidad en Inglés",
  "Profesorado en en enseñanza media",
  "Profesorado en enseñanza media", "Profesorado en señanza media y ciencias de la comunicación",
  "Enseñanza Media", "Licenciatura en la Enseñanza Media en Ciencias Económico Contables",
  "PROFESORADO EN ENSÑANZAMEDIA CON ESPECIALIDAD EN MATICAS PDEP-CB EMATICS",
  "PROFESORADO en Enseñanza Media con Especialidad en Matemáticas",
  "PROOFESORADO EEN LAA ENSEÑANZA MEDIA CON ESPECIALIDADCB EN MATEMATICAS PADEP",
  "Prfesorado de Enseñanza Media con especialidad en Emprendimiento para la Productividad",
  "Profesor de Enseñanza Media en Comunicación y Lenguaje",
  "Profesor de Enseñanza Media especializado en Productividad y Trabajo",
  "Profesor en Enseñanza Media en Ciencias Sociales y Formación Ciudadana",
  "Profesora de Enseñanza Media con Especialidad en Física y Matemáticas",
  "Profesorado Enseñanza Media en Matemáticas",
  "Profesorado de Educación Media en Química y Biología",
  "Profesorado de Enseñanza Media", "Profesorado de Enseñanza Media Con Especialización en el Idioma Inglés.",
  "Profesorado de Enseñanza Media Con Especialización en el Idioma Inglés.",
  "Profesorado de Enseñanza Media con Especialización en Matemáticas",
  "Profesorado de Enseñanza Media con especilizacion en Producctividad y Trabajo",
  "Profesorado de Enseñanza Media en Educación con orientación en ciencias Naturales y Medio ambiente",
  "Profesorado de Enseñanza Media en Inglés",
  "Profesorado de Enseñanza Media en la especialidad de Matemática y Fisica Y Maestría en Innovación Educativa",
  "Profesorado de Enseñanza Media y Técnico en Administración",
  "Profesorado de educación media con especialidad en comunicación y lenguaje",
  "Profesorado de enseñanza media con especialidad en Comunicación y Lenguaje",
  "Profesorado de enseñanza media con especialidad en inglés",
  "Profesorado de enseñanza media con especialización en matemáticas",
  "Profesorado de enseñanza media en historia y ciencias sociales",
  "Profesorado de enseñanza media en matemática",
  "Profesorado de enseñanza media.",
  "Profesorado de la enseñanza media con especialidad en Comunicación y Lenguaje",
  "Profesorado en Educacion Media con especialidad en Productividad y Trabajo",
  "Profesorado en Enseñanza Media con Especialidad en Ciencias Sociales y Formación Ciudadana",
  "Profesorado en Enseñanza Media con Especialización en Ciencias Sociales y Formación Ciudadana",
  "Profesorado en Enseñanza Media de la Informática y Ciencias de la Computación",
  "Profesorado en administración enseñanza media"
)

### profesorado en física y/o matemática
valores_fisica_mate <- c(
  "Profesorado en Matemática", "Profesorado en Matemática y Física",
  "Profesorado en Matemáticas", "Profesorado en Física y Matemática",
  "Profesorado en Fisica Matematica",
  "Profesorado en especialidad en Matemáticas",
  "Profesorado en matemática", "Profesorado en matemáticas",
  "Pénsum cerrado de Licenciatura en Física y Matemática",
  "Tengo cerrado el pensum de licenciatura en CPA y ya estoy por cerrar en primer año de PEM en Matemática y Física",
  "En matemática", "Especialidad en Matemática",
  "Especialidad en Matemáticas del ciclo básico", "Especialidad en matemática",
  "Matematicas", "Matemáticas", "PADEP CON ESPECIALIDAD EN MATEMÁTICA",
  "PADEP MATEMÁTICA", "PEM EN MATEMÁTICAS", "PEM en Ciencias Especializado en Matemática",
  "PEM en Matemática", "PEM en Matemática y Física",
  "PEM en matemática y física", "PEM en matemáticas",
  "PEM en matemáticas y física", "PEM especialidad en Matemática",
  "PEM física y Matemáticas", "PEM. Con especialidad en Matemáticas",
  "PEM. En Matemáticas", "PEM. Especialidad en Matemáticas",
  "PROFESORADO EN MATEMÁTICA", "Padep Profesorado en matemática y Maestría en Investigación",
  "Pem en matemática y física", "Profesora en Matemáticas",
  "Profesorado Física/Mate", "Profesorado con Especialidad en Matemática.",
  "Profesorado con Especialidad en Matemáticas",
  "Padep \"Profesorado en matemática\" y Maestría en Investigación",
  "Profesorado con Especialización en Matemática",
  "Profesorado con especialidad en Matemáticas, ciclo básico",
  "Profesorado con especialidad en ciencias de las matemáticas",
  "Profesorado con especialidad en matemáticas",
  "PROFESORADO  en Enseñanza  Media con Especialidad  en Matemáticas",
  "PROFESORADO  EN ENSÑANZAMEDIA  CON ESPECIALIDAD EN MATICAS PDEP-CB    EMATICS",
  "Profesorado en Enseñanza Media en Física y Matemáticas",
  "Profesorado de Enseñanza Media en la especialidad de Matemática y Fisica  Y Maestría en Innovación Educativa",
  "Licenciatura en la Enseñanza  de la Matemática y Física",
  "Profesor de Enseñanza Media en Pedagogía con Especialidad en Culturas e Idiomas Maya,  Garifuna o Xinka",
  "Profesorado de matemáticas", "Profesorado en ciencia especialidad Matemáticas"
)

### licenciatura en enseñanza matematica y/o fisica
valores_enseñanza_mate <- c(
  "Licenciatura en Enseñanza de la Matemática y Física",
  "Licenciatura en Enseñanza de la Física y la Matemática",
  "Licenciatura en Matemática Física", "Licenciatura en Matemática y Física",
  "Licenciatura en Matemática y Física.",
  "Licenciatura en Matemáticas", "Licenciatura en educación de la matemática y física",
  "Licenciatura en la Educación de la Física y la Matemática",
  "Licenciatura en la Enseñanza de la Matemática y Física",
  "Licenciatura en la Enseñanza de Matemática y Física",
  "Licenciatura en la Enseñanza de la Física Matemática, Ingenieria Civil.",
  "Licenciatura en la enseñanza de la Física y la Matemática",
  "Licenciatura en la enseñanza de la física y matemática",
  "Licenciatura en la enseñanza de la matemática y física",
  "Licenciatura en la enseñanza de matemática y física",
  "Licenciatura en la enseñanza en Física - Matemática",
  "Licenciatura en la física y matemática",
  "Licenciatura en matemática", "LICENCIATURA EN LA ENSEÑANZA DE LA MATEMÁTICA Y FISICA",
  "Didáctica de la matemática"
)

### contaduria y/o auditoria
valores_auditoria <- c(
  "Auditoria", "Contaduría Pública y Auditoría",
  "Licenciatura de Contaduría Pública y Auditoría",
  "Licenciatura en Ciencias Económico Contables"
)

### licenciatura en letras
valores_let <- c(
  "LICENCIATURA EN LETRA", "LICENCIATURA EN LETRAS"
)

### gestion educativa
valores_gestion_educativa <-c(
  "LICENCIATURA EN GESTIÓN EDUCATIVA INTERCULTURAL",
  "Maestría en Dirección y Gestión de Instituciones Educativas, fase de egreso/investigación",
  "Licenciatura en Gestión Educativa", "Maestría en Administración Escolar", "Dirección de centros educativos",
  "Profesorado de segunda enseñanza con orientación en administración y dirección de centros educativos",
  "Maestría en gerencia educativa"
)

### licenciatura en educacion
valores_lic_educacion <- c(
  "LICENCIATURA CON ORIENTACIÓN EN EDUCACIÓN PREPRIMARIA.",
  "Licenciatura en educación primaria  intercultural  con énfasis en educación bilingüe",
  "LICENCIATURA EN EDUCACION", "Licenciad en educación primari Bilingüe Intercultural",
  "Licenciatura en Educación", "Licenciatura en Educación Física",
  "Licenciatura en Educación Preprimaria Intercumtural con Énfasis en Educación Bilingüe",
  "Licenciatura en Educación Primaria Bilingüe Intercultural",
  "Licenciatura en educación primaria  intercultural  con énfasis en educación bilingüe",
  "Licenciatura en Educación de la Comunicación y el Lenguaje",
  "Licenciatura en Educación de la Química y la Biología. Licenciatura en Educación de la Matemática y la Física. Maestría en Planeamiento y Gerencia Educativa.",
  "Licenciatura en ciencias de la educación", "Licenciatura en educación",
  "Licenciatura en educación primaria intercultural con énfasis en educación bilingüe",
  "Licenciatura en educación primaria con énfasis en educación bilingüe",
  "Licenciatura en educación primaria intercultural con énfasis en educación bilingüe",
  "Licenciatura en la educación", "PROFESORADO EN TECNOLOGÍA Y LICENCIATURA DE EDUCACIÓN BILINGÜE INTERCULTURAL PRIMARIA",
  "Licenciatura en profesorado y ciencias de la educación",
  "Preprimaria intercultural", "Doctorado en Educación y ecotransformacion",
  "Licenciaturas en Educación Primaria intercultural con énfasis en educación bilingüe",
  "Profesorado Primaria intercultural", "Licenciatura en Formación  Intercultural"
)

### licenciatura ddhh
valores_ddhh <- c(
  "Licenciatura en Derechos Humanos y Cultura de Paz",
  "Licenciatura en derechos humanos"
)

### administracion rrhh
valores_rrhh <- c(
  "Maestria en Administración de Recursos Humanos",
  "Maestría en Gestión de Recursos Humanos",
  "Gerencia Administrativa de Recursos Humanos"
)

### profesorado en educacion
valores_prof_educacion <- c(
  "PEM en Educación Física", "PEM en Educación Física",
  "PEM en ciencias de la educación",
  "PROFESORADO DE EDUCACIÓN PRIMARIA BILINGÜE INTERCULTURAL",
  "PROFESORADO EN EDUCACIÓN PRIMARIA BILINGÜE INTERCULTURAL",
  "Profesorado De Educación Primaria Intercultural Bilingüe",
  "Profesorado","Profesorado Especializado en educación física deporte y recreación",
  "Profesorado de Educación PrePrimaria", "PEM  en Educación Física",
  "Profesorado de Educación Primaria Bilingue Intercultual",
  "Profesorado de educación física", "Licenciaturas en primaria Intercultural",
  "Profesorado en Educación Inicial y Preprimaria",
  "Profesorado en Educación Primaria Bilingüe Intercultural",
  "Profesorado en Educación Primaria Intercultural",
  "Profesorado en Primaria intercultural", "Licenciatura en Formación Intercultural"
)

### comunicacion y lenguaje
valores_lenguaje <- c(
  "Especial en comunicación y lenguaje",
  "Especialidad en comunicación y lenguaje", "Licenciatura en Comunicación",
  "Licenciatura en Comunicación y Lenguaje",
  "Licenciatura en lenguaje y comunicación",
  "PEM en Comunicación y Lenguaje", "Maestría, Ciencias de la Educación, La comunicación"
)

### ciencias sociales
valores_ciencias_sociales <- c(
  "Licenciatura en Ciencias Sociales y Formación Ciudadana",
  "Licenciatura en Enseñanza de las Ciencias Sociales y Formación Ciudadana",
  "Licenciatura en Estudios Sociales", "Licenciatura en ciencias sociales",
  "PEM en Ciencias Sociales", "PEM en Ciencias Sociales y Formación Ciudadana",
  "Profesorado En Ciencias Sociales y Formación Ciudadana",
  "PEM en Ciencias  Sociales",
  "Profesorado en Ciencias sociales", "Profesorado en ciencias sociales",
  "Profesorado en primaria intercultural y un profesorado rn cirncias sociales en la universidad regional de guatemala sede ipala",
  "Ciencias sociales", "Estudios Sociales", "Licenciatura en Historia"
)

### trabajo social
valores_trabajo_social <- c(
  "Licenciatura en Trabajo Social"
)


### enseñanza en ingles
valores_ingles <- c(
  "Profesorado en idioma inglés", "Profesorado en inglés",
  "En inglés", "Licenciatura en Enseñanza del Idioma Inglés con Especialización en Tecnología Educativa",
  "PEM en inglés", "Profesorado en Inglés",
  "Profesorado de Enseñanza Media Con  Especialización en el Idioma Inglés.",
  "Licenciatura Especializada en la enseñanza del idioma inlgés"
)

### deportes
valores_deportes <-c (
  "Licenciatura en Deportes", "Licenciatura en deporte",
  "Licenciatura en deportes", "Licenciatura en educación física deporte y recreación"
)

### enseñanza quimica y biologia
valores_quimica <- c(
  "Licenciatura en la Enseñanza de la Química y Biología. Además, Licenciatura en Ciencias Jurídicas y Sociales Abogacía y Notariado",
  "Licenciatura en la enseñanza de química y biología",
  "PEM Química y Biología", "PEM en Biología y Química",
  "PEM. Química y biología, Licenciatura en Ciencias Jurídicas y sociales",
  "Terminando otro profesorado el de Química y biología"
)

### maestria en curriculum
valores_curriculum <- c(
  "Maestría en Currículo", "Maestría en Currículum",
  "Maestría en diseño curricular"
)

### pem productividad
valores_productividad <- c(
  "Profesorado en emprendimiento para la productividad",
  "PEM con especializacion en emprendimiento para la Productividad.",
  "PSE En Productividad y Desarrollo",
  "Profesorado en  emprendimiento  para la productividad"
)

### pem musica
valores_musica <- c(
  "Profesorado en expresión artística con especialidad en música",
  "P.E.M. en Música", "PEM EN MÚSICA", "Técnico en ejecución musical"
)

### maestria educacion superior
valores_educacion_superior <- c(
  "Maestría en educación superior",
  "Maestría en educación superior e innovación",
  "Maestria en educación superior"
)

### sin especificar
valores_estudios <- c(
  "Beca en Inglés en la Universidad del Valle de Guatemala , y el EPS en l USAC",
  "ACTUALMENTE IMPARTIENDO EN DOCENCIA",
  "Doctorado", "E-learning", "Ejercicio Profesional Supervisado",
  "Haciendo ejercicio profesional supervisado de licenciatura",
  "LICENCIATURA", "La licenciatura", "Licenciatura",
  "PROCESO EPS", "PSE", "Proyecto de graduación. Para cerrar la licenciatura",
  "Realizando Tesis de la licenciatura",
  "Proyecto de graduación.  Para cerrar la licenciatura",
  "Realizando tesis de graduación en licenciatura.",
  "Si", "Sierre de licenciatura", "Tesis de licenciatura",
  "Ya he terminado la licenciatura", "POR EL MOMENTO IMPARTIENDO EL PAN DEL SABER EN LA U"
)

### teologia
valores_teologia <- c(
  "licenciatura en teologia", "Diplomado Universitario en Teología",
  "Lic. En Teología Ministerial", "Maestría en Ministerios"
)

### idioma español
valores_espanol <- c(
  "Idioma español", "Licenciatura del idioma español y literatura",
  "Licenciatura en Idioma Español y Literatura",
  "Licenciatura en el Idioma Español y Literatura",
  "Licenciatura en la enseñanza del idioma español y la literatura",
  "Licenciatura en la enseñanza del idioma español y literatura",
  "licenciatura en idioma español y literatura", "PEM.en Lengua y Literatura"
)

### en ciencias economicas y contables
valores_contable <- c(
  "PEM CIENCIA ECONÓMICO CONTABLE", "PEM en Ciencias Económicas y Contables",
  "Licenciatura en la enseñanza de las ciencias económico contables",
  "Licenciatura para la Enseñanza en las Ciencias Económico Contables"
)

### criminalistica
valores_criminalista <- c(
  "Criminalistica", "Licenciatura en criminología y ciencias Forenses"
)

### investigacion
valores_investigacion <- c(
  "Doctorado en Investigación para el desarrollo Social",
  "Investigación Educativa", "Maestría en Investigación"
)

### fisioterapia
valores_fisioterapia <- c(
  "Fisioterapia"
)

### relaciones internacionales
valores_internacional <- c(
  "Haciendo tesis en Relaciones Internacionales especializado en Seguridad Internacional"
)

### ingenieria geológica
valores_geologica <- c(
  "Ingeniero geólogo"
)

### licenciatura diseño curricular
valores_diseno_curricular <- c(
  "Licenciatura con énfasis en diseño curricular",
  "Profesorado en Ciencias de la Información Documental, con especialidad en el aprendizaje al currículum Nacional base",
  "profesorado en Ciencias de la información documental con especialidad en centros de recursos para el aprendizaje integrados al currículo modalidad B-learning"
)

### tics
valores_tics <- c(
  "Licenciatura en Tecnología de la Información y la Comunicación",
  "PEM EN TICS",
  "Licenciatura en  Tecnología de la Información y la Comunicación"
)

### licenciatura computación e informática
valores_informatica <- c(
  "Licenciatura en Computación e informática",
  "Licenciatura en Informática y Ciencias de la computación.",
  "Licenciatura en informática"
)

### licenciatura en supervisión eléctrica y electrónica
valores_electronica <- c(
  "Licenciatura en supervision eléctrica y electrónica"
)

### maestria proyectos educativos
valores_proyecto <- c(
  "Maestra en Elaboración de Proyectos Educativos"
)

### maestria educacion para el desarrollo
valores_desarrollo <- c(
  "Maestria en Educación para el desarrollo con Enfasis en Emprendimiento."
)

### maestria en gestión cultural
valores_cultural <- c(
  "Maestria en Gestión Cultural"
)

### maestria en aprendizaje, cognición y desarrollo educativo
valores_aprendizaje <- c(
  "Maestría en Aprendizaje, Cognición y Desarrollo Educativo"
)

### maestria didáctica de la matemática
valores_didactica <- c(
  "Maestría en Didáctica de la Matemática"
)

### maestria seguridad informatica
valores_seguridad <- c(
  "Maestría en Seguridad Informática"
)

### maestria en sexología
valores_sexologia <- c(
  "Maestría en Sexología"
)

### maestria en tecnología educativa
valores_tecnologia <- c(
  "Maestría en Tecnología Educativa con Énfasis en Entornos Virtuales",
  "Maestría en e-learning."
)

### maestria en andragogia
valores_andragogia <- c(
  "Maestría en andragogia"
)

### maestria en educación bilingüe
valores_maestria_bilingue <- c(
  "Maestría en educación bilingue con orientación en Administración"
)

### master en innovacion educativa
valores_innovacion <- c(
  "Máster Universitario en Innovación Educativa"
)

### no están estudiando 
valores_no_estudia <- c(
  "Nada", "Ninguna", "Ninguna carrera", "Ninguno", "No",
  "No esty estudiando"
)

### posgrado en formador de formadores
valores_formador <- c(
  "Posgrado en formador de formadoresy"
)

### profesorado en problemas del aprendizaje
valores_problema_aprendizaje <- c(
  "Profesorado Especializado en Problemas del Aprendizaje"
)

## Recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    que_estudia_actualmente = case_match(
      que_estudia_actualmente,
      all_of(valores_problema_aprendizaje) ~ "Profesorado en Problemas del Aprendizaje",
      all_of(valores_formador) ~ "Posgrado en Formador de formadores",
      all_of(valores_no_estudia) ~ "No estudia actualmente",
      all_of(valores_innovacion) ~ "Master en Innovación Educativa",
      all_of(valores_maestria_bilingue) ~ "Maestría en Educación Bilingüe",
      all_of(valores_andragogia) ~ "Licenciatura en Andragogia",
      all_of(valores_tecnologia) ~ "Maestría en Tecnología Educativa",
      all_of(valores_sexologia) ~ "Licenciatura en Sexología",
      all_of(valores_seguridad) ~ "Maestría en Seguridad Informática",
      all_of(valores_didactica) ~ "Maestría en Didáctica de la Matemática",
      all_of(valores_aprendizaje) ~ "Maestría en Aprendizaje, Cognición y Desarrollo Educativo",
      all_of(valores_cultural) ~ "Maestría en Gestión cultural",
      all_of(valores_desarrollo) ~ "Maestría en Educación para el Desarrollo",
      all_of(valores_proyecto) ~ "Maestría en Elaboración de Proyectos educativos",
      all_of(valores_electronica) ~ "Licenciatura en Supervisión Electrónica",
      all_of(valores_informatica) ~ "Licenciatura en Computación e Informática",
      all_of(valores_tics) ~ "Tecnología de la Información y Comunicación",
      all_of(valores_diseno_curricular) ~ "Diseño Curricular e Información documental",
      all_of(valores_geologica) ~ "Ingeniería Geológica",
      all_of(valores_internacional) ~ "Relaciones Internacionales",
      all_of(valores_fisioterapia) ~ "Fisioterapia",
      all_of(valores_investigacion) ~ "Investigación",
      all_of(valores_criminalista) ~ "Criminología",
      all_of(valores_contable) ~  "Ciencias Económicas y Contables",
      all_of(valores_espanol) ~ "Enseñanza del Idioma Español y Literatura",
      all_of(valores_teologia) ~ "Teología",
      all_of(valores_estudios) ~ "Sin especificar",
      all_of(valores_educacion_superior) ~ "Maestría en Educación Superior",
      all_of(valores_musica) ~ "Profesorado en Música",
      all_of(valores_productividad) ~ "Profesorado en Productividad",
      all_of(valores_curriculum) ~ "Maestría en Curriculum",
      all_of(valores_quimica) ~ "Enseñanza en Química y Biología",
      all_of(valores_deportes) ~ "Licenciatura en Deportes",
      all_of(valores_ingles) ~ "Enseñanza del Idioma Inglés",
      all_of(valores_trabajo_social) ~ "Licenciatura en Trabajo Social",
      all_of(valores_ciencias_sociales) ~ "Ciencias Sociales",
      all_of(valores_lenguaje) ~ "Comunicación y Lenguaje",
      all_of(valores_prof_educacion) ~ "Profesorado en Educación",
      all_of(valores_rrhh) ~ "Administración de Recursos Humanos",
      all_of(valores_ddhh) ~ "Licenciatura en Derechos Humanos",
      all_of(valores_lic_educacion) ~ "Licenciatura en Educación",
      all_of(valores_gestion_educativa) ~ "Gestión Educativa",
      all_of(valores_auditoria) ~ "Contaduría y Auditoría",
      all_of(valores_let) ~ "Licenciatura en Letras",
      all_of(valores_enseñanza_mate) ~ "Licenciatura en la Enseñanza de Matemática y/o Física",
      all_of(valores_fisica_mate) ~ "Profesorado en Matemática y/o Física",
      all_of(valores_media) ~ "Profesorado en Enseñanza Media",
      all_of(valores_artes) ~ "Profesorado en Artes Plásticas",
      all_of(valores_arte) ~ "Licenciatura en Arte",
      all_of(valores_docencia) ~ "Docencia Universitaria",
      all_of(valores_empresas) ~ "Administración de Empresas",
      all_of(valores_industrial) ~ "Ingeniería Industrial",
      all_of(valores_naturales_ambiental) ~ "Ciencias Naturales y Educación Ambiental",
      all_of(valores_civil) ~ "Ingeniería Civil",
      all_of(valores_sistemas) ~ "Ingeniería en Sistemas",
      all_of(valores_psicologia) ~ "Psicología",
      all_of(valores_administracion_educativa) ~  "Administración Educativa",
      all_of(valores_abogacia) ~ "Abogacía y Notariado",
      all_of(valores_juridica) ~ "Licenciatura en Ciencias Jurídicas y Sociales",
      all_of(valores_pedagogia) ~ "Pedagogía",
      .default = que_estudia_actualmente
    )
  )

## volver a ver errores
valores_que_estudia_actualmente <- df_dyd |>
  filter(estudios_actuales == "Sí") |>
  filter(!(is.na(que_estudia_actualmente))) |>
  select(que_estudia_actualmente) 

# capacitaciones_otro_proveedor ----

## Ver errores

valores_capacitaciones_otro_proveedor <- df_dyd |>
  filter(capacitaciones_proveedor == "Otro") |>
  filter(!(is.na(capacitaciones_otro_proveedor))) |>
  select(capacitaciones_otro_proveedor) 

# View(valores_capacitaciones_otro_proveedor)

## Define los vectores ----

### Sin especificar
valores_no_especifica <- c(
  "(otros, especifique)", "No me acuerdo", "No recuerdo", "Otros"
)

### amistades
valores_amistades <- c(
  "Amigos"
)

### propios medios
valores_propio <- c(
  "Autoaprendizaje y cursos en línea de otros colegas", "Autodidacta", "Con fondos propios.",
  "Cursos libres", "Egrafía", "En linea, a través de Internet, tutoriales", "Tecnología",
  "autodidacta"
)

### dirección departamental
valores_departamental <- c(
  "Años anteriores profesionales y capacitadores coordinados por Dirección Departamental y Supervisores",
  "Coordinacion Distrital"
)

### cta
valores_cta <- c(
  "CTA"
)

### en capacitación docente
valores_capacitacion_docente <- c(
  "Capacitacion docente", "Capacitación Docente Guatemala",
  "Capacitación docente", "Capacitador contratado por el colegio",
  "Capacitadores particulares", "Colegio Privado", "Compañeros maestros",
  "Coordinador técnico administrativo", "Director del Establecimiento",
  "Epesista", "Formación para docentes en Línea", "Instituciones que ofrecen capacitación",
  "La Institución Educativa al que laboro", "La asesora pedagógica del colegio",
  "Lic Angel Dionel Barrios López", "Licenciada con especialización y conocimiento del CNB en trabajo de colegio Privado.",
  "Maestros", "Particulares", "docentes de mi ex colegio"
)

### ciprevica
valores_ciprevica <- c(
  "Cipravica"
)

### programa PEMEM
valores_pemem <- c(
  "Comisión Nacional de Evaluación de los Aprendizajes del Programa PEMEM.",
  "DIRECTIVA PEMEN", "Estoy estudiando el PEM ahí es donde más lo.eno me explicaron un poco sobre todo lo relacionado al CNB"
)

### DIGEEX
valores_digeex <- c(
  "DIGEEX"
)

### digecur
valores_digecur <- c(
  "Digeduc"
)

### universidad
valores_universidad <- c(
  "EFPEM-USAC", "En la universidad", "En la universidad, USAC",
  "Experto externo de una universidad", "Experto externo universitario",
  "La universidad", "La universidad san Carlos de Guatemala, llevo los curso para el desarrollo de los aprendizaje con los estudiantes",
  "Los estudiantes de la universidad de San Carlos de Guatemala", "UVG",
  "Por medio Actividades académicas, en la universidad en", "USAC- EFPEN",
  "Profesional universitario", "Profesional universitario externo", "USAC",
  "Universidad", "Universidad Del Valle, de Sololá", "Universidad Galileo",
  "Universidad Mariano Galvez", "Universidad Mariano Gálvez",
  "Universidad UPNA y DVC de Guatemala", "Universidad de San Carlos de Guatemala",
  "la universidad del balle"
)

### editorial
valores_editorial <- c(
  "Editorial", "Editorial santillana"
)

### entidad privada
valores_entidad_privada <- c(
  "Entidades Privadas"
)

### no capacitación
valores_sin_capacitacion <- c(
  "Nadie", "Ninguno", "No", "No he recibido durante el 2025", "No tengo este año ese tipo de capacitación",
  "Nonguno"
)

### padep
valores_padep <- c(
  "PADEP", "Padep", "Padep/cb", "USAC Y PADEP"
)

## Recode case match ----

df_dyd <- df_dyd |>
  mutate(capacitaciones_otro_proveedor = case_match(
    capacitaciones_otro_proveedor,
    all_of(valores_padep) ~ "Programa Académico de Desarrollo Profesional Docente",
    all_of(valores_sin_capacitacion) ~ "No recibió capacitación",
    all_of(valores_entidad_privada) ~ "Entidad privada",
    all_of(valores_editorial) ~ "Editorial",
    all_of(valores_universidad) ~ "Universidad",
    all_of(valores_digecur) ~ "Dirección General de Curriculo",
    all_of(valores_digeex) ~ "Dirección General de Educación Extraescolar",
    all_of(valores_pemem) ~ "PEMEM",
    all_of(valores_ciprevica) ~ "Ciprevica",
    all_of(valores_capacitacion_docente) ~ "Por capacitación docente",
    all_of(valores_cta) ~ "CTA",
    all_of(valores_departamental) ~ "Dirección Departamental",
    all_of(valores_propio) ~ "Por propios medios",
    all_of(valores_amistades) ~ "Colegas y/o amistades",
    all_of(valores_no_especifica) ~ "Sin especificar"
  )
    
  )

## volver a ver errores
valores_capacitaciones_otro_proveedor <- df_dyd |>
  filter(capacitaciones_proveedor == "Otro") |>
  filter(!(is.na(capacitaciones_otro_proveedor))) |>
  select(capacitaciones_otro_proveedor) 

# capacitaciones_temas ----

## ver errores

valores_capacitaciones_temas <- df_dyd |>
  filter(capacitaciones_cnb != "No he recibido capacitaciones sobre los temas mencionados anteriormente") |> 
  filter(capacitaciones_numero != "0") |>
  filter(!(is.na(capacitaciones_temas))) |>
  select(capacitaciones_temas) 


## Define los vectores ----

### estrategias metodologicas y planificacion
valores_estrategias <- c(
  "\"Innovar para Transformar: Estrategias Metodológicas y Planificación Integrada al CNB\"",
  "\"Innovar para Transformar: Estrategias Metodológicas y Planificación Integrada al CNB\"",
  "Competencias para la vida, técnicas y estrategias, pilares educativo, modelos educativos, metodologías,",
  "Componentes del CNB, Pilares de la Educación, Estrategias /Metodología",
  "Componentes del CNB, Pilares de la Educación, Estrategias/Metodologías",
  "\"Innovar para Transformar: Estrategias Metodológicas y Planificación Integrada al CNB"
)

### contenidos del CNB
valores_contenidos <- c(
  ", áreas de CNb, Contenidos del CNB, actividades que se puede realizar con los contenidos del CNB",
  "- Componentes del CNB, Pilares de la Educación y Estrategias/Metodologías",
  "- Planificación con el CNB ciclo básico - Dosificación de aprendizajes - Actividades que promueven el desarrollo del aprendizaje",
  "-Estructura del CNB.       -Procedimientales, Actitudinales, Declarativos.    - Planificar pausas activas",
  "1) Como planificar de manera bimestral con el CNB 2) Como llevar a cabo las competencias como objetivo principal  para la enseñanza de los estudiantes 3) Como seleccionar temas acorde al nivel educativo que se desea enseñar",
  "1. Análisis de los componentes del CNB, 2. Planificación didáctica basada en competencias e indicadores de logro, 3. Integración de materiales concretos y audiovisuales en el proceso de enseñanza.",
  "1. CNB y su fundamento.  2. Herramientas de Evaluación   3. Planificar con el CNB",
  "1. Competencias en el aula.  2. Dosificación y secuencia de Contenidos.  3. Desarrollo de momentos de clase.",
  "1. Contenidos declarativos y procedimentales. 2. Competencias 3 evaluacion de contenidos",
  "1. Desarrollo de ejes del CNB. 2. Cómo planificar con el CNB. 3 Opinión sobre el CNB",
  "1. Estructura y componentes del currículo Nacional Base del CNB 2. Planificación educativa basadas en competencias del CNB 3. Evaluación de aprendizaje según el CNB.",
  "1. PLANIFICACIÓN CURRICULAR., 2. ELEMENTOS DEL CNB., 3. COMPETENCIAS.",
  "1. Que es el CNB. 2. Malla curricular de Matemáticas. 3. Competencias de Matemáticas",
  "1. Uso del CNB 2. Contextualización del CNB 3. Como adquirir el CNB",
  "Areas de aprendizaje, competencias e indicadores de logro",
  "Areás curriculares, tipos de Evaluación,modelos de aprendizaje",
  "CNB COMO ERAMIENTA, LAS TICS, PAUSAS ACTIVAS", "CNB, estructura y ejes",
  "CNB, estructura y planificación", "CNB, los contenidos y Indicadores de Logro",
  "COMPETENCIAS DE ÁREA, MARCO Y EJE - CONTENIDOS (DECLARATIVOS, PROCEDIMENTAES Y ACTITUDINALES) CARACTERÍSTICAS.",
  "COMPETENCIAS EN LA ESPECIALIDAD QUE TRABAJO. Malla curricular!!",
  "competencias, indicadores de logro",
  "Cnb , competencias, indicadores de logro",
  "Estructura del CNB, Aplicación de CNB para planificar, Evaluación",
  "Estructura del CNB, Adecuaciones Curriculares, Bases para realizar material educativo.",
  "Como esta estructurado, aplicación y metodología",
  "Como realizar las planificaciones, el uso correcto del CNB, Que es el CNB",
  "Como usar la Didáctica en los contenidos. Contenido actitudinales,",
  "Competencia de área, competencias de grado, unidades didácticas",
  "Competencia indicadores de logro evaluación",
  "Competencia, Curriculum y saberes", "Competencia, currículum y saberes",
  "Competencias", "Competencias   Herramientas de evaluación    planificación curricular",
  "Competencias contenidos y estrategias de evaluación",
  "Competencias del CNB, Forma de Planificación del CNB, Características del CNB",
  "Competencias indicadores de logro y contenidos",
  "indicador de logro, competencias y  contenidos",
  "Competencias, Contenidos, Estrategias  de Evaluación",
  "Competencias, Indicadores de Logros Los contenidos",
  "Competencias, Indicadores de logro, Contenidos declarativos",
  "Competencias, Indicadores de logro,contenidos: Declarativos, Procedimentales y Actitudinales",
  "Competencias, Temas y uso rubricas.", "Competencias, contenidos, evaluación",
  "Competencias, contenidos actitudinales y procedimentales",
  "Competencias, contenidos e indicadores de logro.",
  "Competencias, contenidos, estrategias de evaluación",
  "Competencias, indicador de logró, actividades",
  "Competencias, indicadores de logro y criterios de evaluación",
  "Competencias, indicadores de logro y estructura",
  "Competencias, indicadores de logro y evaluación",
  "Competencias, indicadores de logro, contenidos",
  "Competencias, indicadores de. Logro y evaluación",
  "Competencias, indicadores logro. Contenidos",
  "Competencias, logros, evaluaciones, contendio,maya curricular valores,",
  "Competencias. Evaluación. Perfil de egreso.",
  "Componentes del CNB",
  "Uso Curriculares, Las Competencias, Contenidos al entorno", 
  "Componentes del CNB, Pilares de la Educación y Estrategias",
  "Componentes del CNB, promoción de valores, uso de la tecnología",
  "Concreción del currículo, estructura y planificación",
  "Conocer la estructura del CNB", "Conocimiento, Curriculum y Saberes",
  "Contenidos 1, sistematización, objetivos.",
  "Contenidos, competencias y evaluación",
  "Contenidos, criterios de evaluación y actividades",
  "Contenidos, ejes, competencias e indicador de logros",
  "Curriculum como eje central en la educacion",
  "Curriculum, competencia y saberes", "Currículum, competencia, saberes",
  "Cómo está divido el Cnb, Como utilizar el cnb, Que temas aborda el cnb",
  "Cómo está divido el cnb, participando integral buscar la flexibilidad para adaptarse",
  "De competencias, indicador logros, planificación",
  "Dosificación- Competencias, contenidos y Actividades.",
  "ESTRUCTURA DEL CNB, Estrategias de enseñanza,  plataformas digitales",
  "ESTRUCTURA Y FORMA DEL CNB", "Ejes", "Ejes del CNB",
  "Ejes del CNB, Flexibilidad e Integración de áreas", 
  "Estructura del CNB, tipos de contenidos, utilización de malla curricular para planificar",
  "Ejes, competencias e indicadores de logro", "El CNB ES FLEXIBLE",
  "El CNB, CONSTRUCCION DE COMPETENCIAS Y COMO EVALUAR",
  "El Currículo y elementos claves para su aplicación en el aula, importancia del CNB, Cómo planificar con el CNB",
  "Elaboracion de Indicador de logros,competencias,estructura de contenidos...",
  "Estructura de CNB", "Estructura de CNB, estrategias de Evaluación E innovación tecnológica",
  "Estructura del CNB, Actualización del CNB",
  "Filosofía, estructura y Fin del CNB",
  "Procidimentales, actitudinales, Fortalecimiento del CNB, aplicaciones de herramientas de evaluación", 
  "Perfil de Ingeso y egreso - Competencias - Contenidos", 
  "Plan de mejoramiento de los a prendizajes, ilustraciones con material de apoyo. Dosificación de los contenidos por bloque", 
  "Planeación de los aprendizajes por competencias, herramientas de evaluación, Uso de materiales audiovisuales.",
  "Estructura del CNB. Tipos de contenidos. Utilización de malla curricular para planificar",
  "Estructura del CNB, uso correcto del CNB, importancia del CNB.",
  "Estructura del CNB, Contenidos del CNB y Competencias del CNB",
  "Estructura del CNB, Creación del CNB, Competencia e indicadores de logros del CNB",
  "Estructura del CNB, Cómo planificar, cómo desarrollar la clase.",
  "Estructura del CNB, Ejes del CNB, Como planificar de manera correcta con el CNB.",
  "Estructura del CNB, Estrategia de Enseñanza, Plataformas digitales",
  "Estructura del CNB, Estrategias de enseñanza, plataformas digitales",
  "Estructura del CNB, Formas de planificar, Instrumentos de evaluación",
  "Estructura del CNB, Herramientas de evaluación, Herramientas de enseñanza-aprendizaje, Pérdida de atencion en clases, clases dinámicas.",
  "Estructura del CNB, Planificaciones, Evaluaciones", "Estructura del CNB, Planificación",
  "Estructura del CNB, Tipos de contenido, Utilización de Malla Curricular para Planificar",
  "Estructura del CNB, actualización del CNB y Correcta Aplicación del CNB",
  "Estructura del CNB, actualización del CNB, aplicación del CNB en el aula.",
  "Estructura del CNB, conocimiento sobre el CNB, cómo utilizarlos",
  "Estructura del CNB, créditos académicos latinoamericanos, malla curricular universitaria",
  "Estructura del CNB, diseño de planificación, diseño de evaluaciones",
  "Estructura del CNB, estrategias de enseñanza, plataformas digitales",
  "Estructura y Diseño del CNB", "Estructura y aplicación del CNB",
  "Estructura,  como implementarlo , como usar las herramientas de evaluacion",
  "Estructura, aplicación, metodología", "Estructura, evaluación y mallas curriculares",
  "Estructura, metodología", "INDICADORES DE LOGRO",
  "Implementación del CNB, Componentes del CNB, Como hacer planes de mejora", 
  "Implemtacion del CNB", "Importancia del CNB",
  "Sobre que es el CNB, sobre la estructura del CNB y como obtener los contenidos y competencias", 
  "Taxonomía de Bloom, uso de temas, dosificación",
  "Tipos de Evaluación, tipos de Aprendizaje y las Áreas curriculares", 
  "Tipos de evaluación,  tipos de aprendizajes y los objetivos", 
  "Tipos de evaluación, tipos de aprendizaje y áreas curriculares", 
  "Transformación curricular, adecuación curricular, currículo nacional base", 
  "Sobre competencias del área, dé grado e indicador de logros", 
  "Sobre graduandos, seminarios y CNB",
  "¿Qué es el CNB?; Cómo implementarlo en nuestra planificación y recursos de clase",
  "Metodología, competencia, Indicadores de logroadores de logro",
  "Herramientas de evaluación, planificación, conocimiento del cnb",
  "Función del CNB", "Herramientas de evaluación, indicadores de logro, ejes transversales",
  "Herramientas de evaluación, competencias e indicador de logros", 
  "Herramientas de evaluación, competencias e indicadores de logro",
  "Evaluación, Estructura, Como funcionan los indicadores de logro, Competencias",
  "Flexibilidad del CNB, Principal objetivo del CNB, Aplicación del CNB",
  "Indicador de logros, de areas y competencias de grado", "Indicadores de logro", 
  "Indicadores de logro, competencia, contenidos",
  "Relación entre competencias, indicadores de logro y contenidos. Enfoque constructivista y aprendizaje significativo. Adaptación Curricular",
  "Indicadores de logro, competencias y metodología de planificación", 
  "Indicadores de logro, competencias, evaluación", "Indicadores de logro.", 
  "Indicadores de logro. actividades. Herramientas de evaluación.", 
  "Indicadores de logros, Contenidos, Competencias",
  "Indicadores de logros, metodologías y competencias del CNB", 
  "Induccion al CNB, TÉCNICAS Y HERRAMIENTAS",
  "Áreas curriculares, Competencias, indicadores de logros", "Áreas. Curriculares, tipos de evaluación, tipos de aprendizaje", 
  "área curricular, los valores  Competencias, evaluación, metodología,contenido", 
  "Número de periodos a la semana, las competencias, indicadores de logro", 
  "Objetivos específicos y recursos", "Objetivos, como trabajar el actitudinal, procedimental y declarativo y como trabajar los indicadores de logro. También como utilizar los verbos que utilizan para realizar una planificación.", 
  "Orden de temas, Base de datos, bibliografias",
  "Metodología de evaluación,  componentes del CNB, promoción de valores",
  "Integración de contenidos. Diferencia entre indicares de logro y competencia. Como planificar.", 
  "Instrumentos de evaluación, indicador de logros, competencias",
  "Introducción al CNB, Metrología y la Evaluación,", "Introducción al CNB, Planificación de Unidades didácticas, Análisis de competencia", 
  "Introducción al Cnb, Planificación de unidades didácticas y Análisis de competencia", 
  "La Planificación Administrativo, Planificación de planes, Areas Curriculares",
  "La cantidad de contenidos.   Las competencias.   Herramientas de evaluación", 
  "La evaluación, las competencias, indicadores de logro",
  "La plannificación. Competencias. Indicadores de logro.", 
  "La redacción de competencias La Integración de los temas con otros cursos  Cono redactar indicadores de logros.", 
  "La transformación curricular, elementos de la evaluación, planificación por comñetencias", 
  "Laboratorio de formación docente, Didáctica, Planificación Curricular", 
  "Las Matemáticas, actividades y planes.",
  "Las competencias, Inicio, desarrollo y fin de cada tema y Diferentes estratégias de dar clases", 
  "Las competencias, fines y objetivos del CNB, Perfil de ingreso y egreso de los estudiantes",
  "Logros, indicadores, temas, estrategias,valores ,contenidos,", 
  "Los componentes de CNB, aplicación de las competencias el el proceso de la planificación, aplicación de las mallas curriculares.", 
  "Los contenidos curriculares. Ejes curriculares. Planificación Curricular", 
  "Los ejes, contenidos, ciclos",
  "Que es el CNB", 
  "competencias, marco teórico y indicadores de logro", "competencias, planificacion, evaluacion", 
  "contenido, ejes, competencias", "desarrollo de las áreas, desarrollo de las competencias, evaluación", 
  "desarrollo, como usarlo, adecuarlo", "ejes, areas y saberes", 
  "estructura del C N B,", 
  "Que es la malla curricular, indicadores de logros , reforma educativa", 
  "Que es una competencia. Partes en que se compone el CNB. Que es planificación curricular", 
  "Qué es el CNB. Componentes del CNB. Implementación del CNB.", 
  "Ramificacionbde temas, competencias, indicadores de logros. Adecuación curricular", 
  "Maya curricular, competencias y criterios de evaluación", "Maya curricular, contexto,", 
  "Mejoramiento de los aprendizajes, Evaluación Formativa, Competencias", 
  "Los retos de la nueva escuela, El CNB Aplicado a los diferentes niveles socio económicos de Guatemala",
  "MALLAS CURRICULARES, COMPETENCIAS Y EVALUACIÓN", 
  "MANEJO DE HERRAMIENTAS, CONTENIDOS DEL CNB Y USO DE HERRAMIENTAS EN EL SALON DE CLASE.", 
  "Malla Curricular, Competencias e indicadores de logro, Evaluación", 
  "Malla curricular, actividades de aprendizaje, actividades de evaluación.", 
  "Malla curricular, competencias, desarrollo de contenidos utiizando las mismas", 
  "Malla currículum, acuerdos, funciones", "Mallas curriculares, plan de mejoramiento, competencias", 
  "Manejo de Herramientas digitales, Aplicación de las herramientas, Conocimiento de las herramientas digitales para el CNB.",
  "Modelos de Aprendizaje. Modelos Educativos. Introduccion al CNB",
  "Principios del diseño curricular, didáctica y renovación curricular", 
  "Principios, ejes y competencias del CNB",
  "Uso del CNB", "Uso del CNB,", "Uso del CNB, Competencias y resultados, Ejes del CNB", 
  "Uso del CNB, IMPORTANCIA, HERRAMIENTAS DE LA EVALUACIÓN",
  "Uso del CNB, aplicación del mismo en la planificación y evaluación", 
  "Uso del CNB, cómo planificar las clases integrando materias usando el CNB, cambios del CNB", 
  "Uso del CNB, herramientas", "Uso del cnb  aplicación del cnb", 
  "Uso del cnb  metodologias, herramientas", "Uso esteategia, uso guias curriculares  como dosificar, el uso corrwcto de las competencias"
)

### planificacion y evaluacion
valores_plani_evaluacion <- c(
  "1-.Sobre evaluación de los aprendizajes 2-. Sobre planificación 3-. Sobre contenidos a desarrollar en los diferentes cursos",
  "1. Como alcázar mi COMPETENCIAS 2. CONTENIDOS 3. Actividades de aprendizaje",
  "Estructura del CNB. Tipos de contenidos. Utilización de malla curricular para planificar",
  "1. Como estructurar mi planificación anual de acuerdo al CNB 2. Técnicas de observación y desempeño que se pueden aplicar en el proceso de enseñanza aprendizaje 3.  Ítems de como diseñar una evaluación de conocimientos.",
  "1. El Cnb y la evaluación. 2. Como contextualizar el currículo 3. Estructura del CNB",
  "1. Evaluación de los Aprendizajes 2. Adaptación de los Actividades: Declarativas, Procedimental y Actitud inalcanzable 3. Utilización del Odec o en su caso investigar o adaptarse al contexto educativo",
  "1. Herramientas de evaluación del CNB. 2. Técnicas y estrategias de Evaluación del CNB. 3.Evaluación de los aprendizajes del CNB.",
  "1. Planificación asertiva. 2. Planificación anual utilizando CNB. 3. Ampliando temas del CNB",
  "1. Taxonomía de Bloom en el desarrollo de actividades. 2. Planificación de bloque y quincenal.  3. Proceso de mejora",
  "1. Uso correcto de las tic's en nivel básico. 2. Implementación del CNB para el proceso de mejoramiento en el nivel básico. 3. Implementación del CNB para la planificación curricular.",
  "1. Uso de las herramientas de evaluación, 2. Integración de las TIC's, 3. Desarrollo de competencias",
  "Actividades a desarrollar según los contenidos. Planificar según el CNB y ser innovador en cada contenido a impartir.",
  "Actividades del docente, Planificación Curricular, Formato de la Planificación, Competencia, Indicador de Logro, Actividades",
  "Actividades. Indicadores de logro.  Herramientas de evaluación.",
  "Adecuaciones Curriculares, Planificación con el CNB, Evaluación con el CNB",
  "Adecuaciones curriculares, estrategias del uso del CNB, como planificar con el CNB",
  "Adecuación curricular, Planificación y Estrategias de Enseñanza.",
  "Adecuación curricular, Planificación y competencias",
  "Evaluación, ejes curricular, competencias",
  "Proceso de Mejoramiento de los aprendizajes, Metodologías Activas en el aula, Planificación,",
  "PLANIFICACION  PLAN DE MEJORA", 
  "Uso del CNB, IMPORTANCIA, HERRAMIENTAS DE LA EVALUACIÓN",
  "Uso del CNB, aplicación del mismo en la planificación y evaluación", 
  "Uso del CNB, cómo planificar las clases integrando materias usando el CNB, cambios del CNB", 
  "Uso del CNB para planificar, Competencias por área, Indicadores de Logro", 
  "Uso del CMB como planificar competencias", 
  "Tipos de Evaluación, tipos de Aprendizaje y las Áreas curriculares", 
  "Tipos de evaluación,  tipos de aprendizajes y los objetivos", 
  "Tipos de evaluación, tipos de aprendizaje y áreas curriculares",
  "PLANIFICACION CURRICULAR, PLANEAMIENTO EDUCATIVO, ESTRATEGIA 360", 
  "PLANIFICACIÓN, EJECUCIÓN Y EVALUACIÓN",
  "PLANIFICACIÓN, METODOLOGÍA Y EVALUACIÓN", 
  "competencias, planificacion, evaluacion", 
  "desarrollo de las áreas, desarrollo de las competencias, evaluación", 
  "Técnicas de Aprendizaje. Formas de evaluación Lúdica, planificación en cnb", 
  "Técnicas de aprendizaje, técnicas de evaluación contenidos del currículo en el área ocupacional", 
  "Técnicas de evaluación, técnicas de enseñanza de matemáticas, técnicas de lectoescritura", 
  "Técnicas y herramientas de evaluación Planificación curricular estrategias curriculares", 
  "Panificación y evaluación de aprendizaajes, Ruta de aprendizajes",
  "Objetivos específicos y recursos", "Objetivos, como trabajar el actitudinal, procedimental y declarativo y como trabajar los indicadores de logro. También como utilizar los verbos que utilizan para realizar una planificación.",
  "Metodologías de enseñanza y estrategias didácticas para el desarrollo del CNB, Evaluación del aprendizaje alineada al CNB, Inclusión, diversidad cultural y lingüística en el currículo",
  "Manejo del CNB, planificación docente y Habilidades y contenidos específicos", 
  "Manejo y estructura del CNB.  Registro de evaluación y competencias.",
  "MALLAS CURRICULARES, COMPETENCIAS Y EVALUACIÓN", 
  "Malla Curricular, Competencias e indicadores de logro, Evaluación", 
  "Malla curricular, actividades de aprendizaje, actividades de evaluación.",
  "Los componentes de CNB, aplicación de las competencias el el proceso de la planificación, aplicación de las mallas curriculares.", 
  "Los contenidos curriculares. Ejes curriculares. Planificación Curricular",
  "Indicadores de logro. actividades. Herramientas de evaluación.", 
  "Indicadores de logro, competencias y metodología de planificación", 
  "Indicadores de logro, competencias, evaluación",
  "Herramientas  de evaluación, implementar  el CNB",
  "Sobre la planificación", 
  "evaluaciones de aprendizaje, contenidos instrumentos de evaluación, planificacion", 
  "herramientas de enseñanza, herramientas de evaluación y herramientas de aprendizaje en clase.", 
  "herramientas digitales, herramientas tecnológicas, planificación de clase", 
  "herramientas digitales, herramientas tecnológicas, planificación de clase,", 
  "Utilización de las herramientas de evaluación, necesidades educativas especiales, elaboración de evaluaciones", 
  "Sobre planificación, Alcanzar la competencia, las técnicas de AP", 
  "Gestión de las clases y manejo de entorno digital, clase modelo y planificación",
  "Fines de la educación, evaluación de competencias  y perfiles de egreso e ingreso",
  "Evaluación, planificación y actividades pedagogicas",
  "la ley pina, CNB aplicación en el aula, evaluación", 
  "Evaluación, mejoramiento de aprendizajes, planificación, destrezas de aprendizaje.",
  "Evaluación, Indicarles de logro, Herramientas de evaluación",
  "Evaluación, Estructura, Como funcionan los indicadores de logro, Competencias",
  "Evaluacion ,planificacion", "Evaluacion etereoevaluacion coevaluacion",
  "Evaluacion de competencias", "Evaluaciones, técnicas de aprens",
  "Evaluaciones  y mejoramiento", "Evaluación, desarrollo y apoyo",
  "Evaluación de los aprendizajes. Las competencias en el CNB. Estrategias de Aprendizaje",
  "Evaluación de los aprendizajes, resistencia, habilidades blandas.",
  "Evaluacion enseñanza aprendizaje",
  "Áreas. Curriculares, tipos de evaluación, tipos de aprendizaje", 
  "área curricular, los valores  Competencias, evaluación, metodología,contenido", 
  "Que es una competencia. Partes en que se compone el CNB. Que es planificación curricular", 
  "Evaluación de los aprendizajes, habilidades blandas y evaluación de los aprendizajes",
  "Evaluacion y CNB EN GENERAL", "Evaluaciones de acuerdo al CNB actualizaciones del CNB formas de aprendizaje y",
  "Evaluacion, planificacion y estructura de instrumentos de evaluacion",
  "Evaluacion de los aprendizajes por competencias, uso de material de apoyo, planificación",
  "Estructura,  como implementarlo , como usar las herramientas de evaluacion",
  "Estructura del CNB, diseño de planificación, diseño de evaluaciones",
  "Estructura del CNB, Planificación",
  "Estructura, evaluación y mallas curriculares",
  "Estructura del CNB, tipos de contenidos, utilización de malla curricular para planificar",
  "Estructura del CNB, Tipos de contenido, Utilización de Malla Curricular para Planificar",
  "Estructura del CNB, Herramientas de evaluación, Herramientas de enseñanza-aprendizaje, Pérdida de atencion en clases, clases dinámicas.",
  "Estructura del CNB, Formas de planificar, Instrumentos de evaluación",
  "Estructura del CNB, Ejes del CNB, Como planificar de manera correcta con el CNB.",
  "Estructura del CNB, Cómo planificar, cómo desarrollar la clase.",
  "Estructura del CNB, Aplicación de CNB para planificar, Evaluación",
  "Estructura de CNB, estrategias de Evaluación E innovación tecnológica",
  "Adecuaciónes curriculares, Planeamiento y planeación, Estrategias de aprendizaje con relaión al CNB",
  "Aplicación correcta del uso de Instrumentos de Evaluación,  aplicación correcta de Plan de mejora, mejorar las competencias en el estudiante",
  "Areás curriculares, tipos de Evaluación,modelos de aprendizaje",
  "Aserca de la lista de cotejo planificación de clase",
  "Autoevaluacion, cooevalucion, eteroevalucion",
  "Bases para planificar, estrategias para la motivación, estrategias para la lecto escritura.",
  "CNB en la planificación Básica, Evaluaciones según el CNB, Desarrollo de competencias en base al CNB",
  "CNB evaluacion, planificacon , materiales", "CNB y las Evaluaciones",
  "CNB, Actualización Docente, Herramientas de Evaluación",
  "CNB, Adaptación del Curriculo y Técnicas de Evaluación Constante",
  "CNB, Adaptación del curriculo, Tecnicas de evaluación constante",
  "CNB, Herramientas de Evaluación y Malla curricular",
  "CNB, Maya curricular, planificación", "CNB, competencias, evaluación",
  "CNB, estructura y planificación", "EVALUACIÓN",
  "CNB, planificación por competencias, uso del CNB",
  "COMO PLANIFICAR EL CNB, ESTRATEGIAS Y METODOLOGÍAR CONO USAR EL CNB",
  "Capacitación del CNB, planificaciones",
  "Como Identificar las Necesidades Especiales, Como Planificar Diario, Bimestral y Anual.",
  "Como alcanzar las competencias, indicadores de logro, criterios de evaluación",
  "Como planificar con ayuda del CNB, Como desarrollar competencias y como aplicarlas Como adaptar la planificación si un joven o señorita es un caso especial",
  "Como planificar según el CNB. Etapas de la evaluación.  Desarrollo de competencias e indicadores de logro",
  "Como planificar, criterios de una planificación, importancia de la planificación",
  "Como realizar las planificaciones, el uso correcto del CNB, Que es el CNB",
  "Como usar el CNB para planificar, Como usar el CNB y Uso de material de apoyo curricular",
  "Como utilizar el CNB, Planificación, concondancia entre indicador de logro y actividad,",
  "Competencia indicadores de logro evaluación",
  "Competencias   Herramientas de evaluación    planificación curricular",
  "Competencias contenidos y estrategias de evaluación",
  "Competencias de CNB Metodología al planificar en el CNB Tendencia Curricular",
  "Competencias del CNB, Forma de Planificación del CNB, Características del CNB",
  "Competencias, Contenidos, Estrategias  de Evaluación",
  "Competencias, actividades, evaluación", 
  "Evaluación de proyectos, ejecución de proyectos y adecuación curricular",
  "Competencias, contenido y evaluación", "Competencias, contenidos, estrategias de evaluación",
  "Competencias, contenidos, evaluación",
  "Evaluación del aprendizaje, competencia en los contextos de los estudiantes, planificación curricular, Modelos de los aprenzajes y evaluación etc.",
  "Competencias, criterios de evaluación, fomentaron de cnb",
  "Competencias, estrategias de evaluación, actividades de aprendizaje",
  "Competencias, formas de evaluar y planificación.",
  "Competencias, indicadores de logro y criterios de evaluación",
  "Competencias, indicadores de logro y evaluación",
  "Implementación de la planificación, planificación y currículum",
  "Evaluación por competencias,  laboratorio para la investigación",
  "Evaluación por aprendizajes", "Evaluación por competencias",
  "Competencias, indicadores de. Logro y evaluación",
  "Competencias, logros, evaluaciones, contendio,maya curricular valores,",
  "Competencias, planificación, materiales didácticos,",
  "Competencias. Evaluación. Perfil de egreso.",
  "Maya curricular, competencias y criterios de evaluación", "Maya curricular, contexto,", 
  "Mejoramiento de los aprendizajes, Evaluación Formativa, Competencias", 
  "Estructura del CNB, Planificaciones, Evaluaciones",
  "Concreción del currículo, estructura y planificación",
  "Conocimiento  del CNB, Aplicación del CNB y Herramientas de Evaluación",
  "Conocimiento de Malla Curricular, Planificaciones, programas y Herramientas de Evaluacion",
  "Conocimiento del CNB, aplicación del mismo y evaluación",
  "Contenidos, competencias y evaluación",
  "Modelo de la aplicación de los aprendizajes, planificación y currículum",
  "Contenidos, criterios de evaluación y actividades",
  "Contextualización del CNB, Instrumentos de Evaluación, Actualización del CNB",
  "Contextualizar, planificar. Comparativa", 
  "Introducción al CNB, Metrología y la Evaluación,", "Introducción al CNB, Planificación de Unidades didácticas, Análisis de competencia", 
  "Introducción al Cnb, Planificación de unidades didácticas y Análisis de competencia", 
  "La Planificación Administrativo, Planificación de planes, Areas Curriculares",
  "Contextualizar. Planificar conocimientos previos",
  "Criterios de Evaluacion, Aplicacion de Competencias, Curriculum,",
  "Currículo Nacional Base, Planificación con cnb, el cnb en las aulas",
  "Cómo aplicar CNB en las planificaciones",
  "Cómo evaluar la progresión de los contenidos. Uso de herramientas de evaluación",
  "Cómo planificar a base de CNB, sobre competencias",
  "Cómo planificar con el CNB de Nivel Medio, Metodologías para la Evaluación y Uso de la IA en la educación.",
  "Cómo planificar con el CNB, como implementar los trabajos de mejoramiento, adecuación curricular",
  "Cómo planificar, contextualizar contenidos, sobre competencias",
  "Cómo realizar una planificación bimestral, planificación cómo elaborar una de dosificación cómo elaborar un cuadro de registro",
  "Cómo usar laPlanificación, el uso adecuado del cnb",
  "Evaluación de los aprendizajes, Educación Bilingue",
  "Cómo utilizar el CNB, Herramientas de Evaluación según el CNB, Estrategias de Aprendizaje según el CNB",
  "De competencias, indicador logros, planificación",
  "Evaluación de los aprendizajes, Tecnología para el Aprendizaje y la Comunicación. Residuos y Desechos Sólidos desde el CNB de Ciencias Naturales",
  "Desarrollo de competencias en el aula. El marco teórico.  Estrategias de evaluación",
  "Desarrollo de competencias, Aplicación de contenidos, Implementación del CNB de educación física, metodología de educación física, evaluación de los aprendizajes",
  "Desarrollo de habilidades, como planificar el cnb",
  "Diario pedagógico, evaluación, adecuación curricular",
  "Didáctica en la educación de nivel medio. Soy un docente didáctico. Herramientas digitales para la educación de nivel medio.",
  "Didáctica, Evaluación, Constructivismo", "Didáctica/ Técnicas de evaluación/ Constructivismo",
  "Diseño curricular, planificación y didáctica, evaluación de aprendizaje",
  "Diseño curricular, rutas de aprendízaje, elaboración de planificaciones",
  "Diseño y Planificación del currículum.     Estrategias de evaluación en base al CNB.      Herramientas e indicadores de logro.",
  "Docificacion, evaluación, distribución de tiempo.", "Dosificación de temas.",
  "Dosificación- Competencias, contenidos y Actividades.",
  "Dwfinicion del CNB APLICACION Y EVALUACION",
  "Formas de Evaluara, Formulación de Items, Formulación de series en relación a competenicas CNBV",
  "EL CNB, PLANIFICACIÓN Y EVALUACIÓN", "EL USO DEL CNB, COMO PLANIFICAR EL CNB",
  "ENA Evaluación y capacidades psicomotricez",
  "Evaluación y aprendizaje, habilidades psicomotrices y comprensión lectora",
  "Evaluación por competencias,  planificacion por competencias",
  "ESTRUCTURA DEL CNB, PLANIFICACION Y EVALUACIÓN",
  "Metodología de evaluación,  componentes del CNB, promoción de valores",
  "Educación activa, formas creativas para evaluar, apoyo al estudiante",
  "Educación física adaptada. Planificación. Actividad recreación",
  "Educación y valores. Planificación.",
  "El CNB, CONSTRUCCION DE COMPETENCIAS Y COMO EVALUAR",
  "El Currículo y elementos claves para su aplicación en el aula, importancia del CNB, Cómo planificar con el CNB",
  "El plan de mejoramiento, cómo elaborar las planificaciones y cómo elaborar los proyectos de evaluación",
  "El uso del CNB para planificar. La planificación",
  "Elaboración de objetivos estructura de la clase, clases con chicos con discapacidad",
  "Elaboración de planes,",
  "Integración de contenidos. Diferencia entre indicares de logro y competencia. Como planificar.", 
  "Instrumentos de evaluación, indicador de logros, competencias",
  "Emplear contenidos del CNB y la cultura maya,Instrumentos de evaluación según el CNB y planificación según el CNB",
  "Erramientas de Evaluación",
  "¿Qué es el CNB?; Cómo implementarlo en nuestra planificación y recursos de clase",
  "planificación con actividades activas,", "sobre planificar el curso", 
  "metodologías del aprendizaje, evaluación del aprendizaje y áreas curriculares", 
  "Estrategias de Evaluación, Diseño de Planificación, Evaluación",
  "Estrategias de aprendizaje, evaluación de aprendizajes, pensamiento critico ,",
  "Estrategias de evaluación basado en el CNB, Planificación de clases según el contexto, metodología para estudiantes de adecuación curricular",
  "Estrategias de evaluación, Aprendizaje Significativo y Desarrollo de habilidades",
  "Estrategias de evaluación, estructura del corriculum, diseño de planificación",
  "Estrategias de planificación según el CNB",
  "Técnicas de evaluación, técnicas de enseñanza de matemáticas, técnicas de lectoescritura", 
  "Técnicas y herramientas de evaluación Planificación curricular estrategias curriculares",
  "Evaluación curricular y planificar", "Evaluación de aprendizajes. Rol del educador, competencias",
  "Evaluación de los aprendizaje, adaptación de contenidos del CNB, Malla curricular",
  "Evaluación a través del CNB, uso de competencias de acuerdo al CNB y el uso de indicadores de logro",
  "Evaluación", "Evaluación de los aprendizajes y planificación por competencias.",
  "Evaluación de los aprendizajes, Desarrollo de competencias, ¿Para que sirven los Indicadores de Logros?",
  "Evaluación, planificación y metodología",
  "Evaluación, planificación, Malla curricular",
  "Evaluación, tipos de evaluación, retroalimentación.",
  "Evaluación. Didáctica. Constructivismo",
  "La plannificación. Competencias. Indicadores de logro.", 
  "La redacción de competencias La Integración de los temas con otros cursos  Cono redactar indicadores de logros.", 
  "La transformación curricular, elementos de la evaluación, planificación por comñetencias", 
  "Laboratorio de formación docente, Didáctica, Planificación Curricular", 
  "Las Matemáticas, actividades y planes.",
  "Indicador de logros, Evaluación , desarrollo de contenido",
  "Herramientas de Evaluacion. Uso e implementacion del CNB.", 
  "Herramientas de Evaluación, Aprendizaje Basado en Proyectos, Feedback", 
  "Herramientas de Evaluación, Implementaron del CNB. s",
  "Herramientas de Evaluación, Transformación Digital, Herramientas educativas digitales", 
  "Herramientas de Evaluación. Implementación de herramientas digitales en contenidos del CNB y planificación en base al CNB", 
  "Herramientas de evaluacion", "Herramientas de evaluacion, indicadores de logro  y comprtencias", 
  "Herramientas de evaluación implementación del cnb", "Herramientas de evaluación y aplicación del CNB", 
  "Herramientas de evaluación, aplicación del CNB, introducción al CNB.", 
  "Herramientas de evaluación, competencias e indicador de logros", 
  "Herramientas de evaluación, competencias e indicadores de logro", 
  "Herramientas de evaluación, implementaron del CNB,",
  "Herramientas de evaluación, indicadores de logro, ejes transversales", 
  "Herramientas de evaluación, planificación", "Herramientas de evaluación, planificación, conocimiento del cnb", 
  "Herramientas de evaluación, planificación. Mejoramiento", 
  "Herramientas de evaluación. Contextos culturales",
  "Herramientas para evaluar,planificar","La cantidad de contenidos.   Las competencias.   Herramientas de evaluación", 
  "La evaluación, las competencias, indicadores de logro", "La forma de planificar",
  "Manejo del CNB, Estrategias de aprendizajez, Planificación curricular",
  "Planeación de los aprendizajes por competencias, herramientas de evaluación, Uso de materiales audiovisuales.", 
  "Planificacion", "Planificacion Herramientas el aprendizaje Lector de Evaluacion", 
  "Planificacion curricular. Curso del CNB. Metodologías y herramientas del CNB", 
  "Planificacion por competencia. Diseño Curricular. Herramientas de Evaluación.", 
  "Planificacion por competencia. Herramientas de Evaluación. CNB y el libro de Matemática", 
  "Planificacion por competencia. Herramientas de evaluación. Evaluacion por competencia", 
  "Planificacion, estrategias de aprendiza, Hettamientas de Evaluacion", 
  "Planificaciones Generales por bimestre",
  "Planificaciones de los aprendizajes, herramientas de evaluación, uso de material audiovisual", 
  "Planificaciones, Adaptaciones curriculartes y unificacion de las artes para Expresión Artística", 
  "Planificaciones, adecuación curricular", "Planificació",
  "Planificació, evaluación de los aprendizajes , adecuación  curricular", 
  "Planificación", "Planificación / Herramientas de Evaluación", 
  "Planificación Bimestral , Dosificación y Plan Semanal",
  "Planificación CURRICULAR. Tipos de evaluaciones.  Desarrollo CURRICULAR", 
  "Planificación Curricular,  Uso del CNB y Herramientas de Evaluación", 
  "Planificación Curricular, Competencias y contenidos de un plan de Clases", 
  "Planificación Curricular, Herramientas de evaluación, Competencias", 
  "Planificación bimestral", "Planificación con CNB",
  "Planificación con base al CNB, Evaluación con Criterios del CNB, Uso de herramientas tecnológicas", 
  "Planificación con el CNB, Estrategias docentes, Didáctica en el nivel medio,", 
  "Planificación con el apoyo del CNB, Instrumentos y técnicas de evaluación, indicadores de logro que contempla el CNB", 
  "Planificación con problemas de aprendizaje, Aprendamos a planificar con inteligencia artificial.", 
  "Planificación curricular", "Planificación curricular , 2. Adecuación curricular,", 
  "Planificación curricular, Curso de CNB y Metodología", "Planificación curricular, elementos del curriculum, Herramientas de evaluación", 
  "Planificación curricular, elementos del currículo y herramientas de evaluación del currículo", 
  "Planificación curricular, métodos de enseñanza y adecuaciones curriculares", 
  "Planificación curricular, uso de CNB, metodología y herramientas de evaluación", 
  "Planificación curricular, uso del CNB y como usar CNB", "Planificación curricular,elementos del curriculo", 
  "Planificación curricular. Uso del Cnb. Herramientas",
  "Planificación de actividades lúdicas y recreativas basadas en el CNB", 
  "Planificación de acuerdo a los logros, materiales de apoyo para impartir las clases,", 
  "Planificación de contenidos, Estructura de CNB, Implementación del CNB", 
  "Planificación de cursos, evaluación de cursos, elaboración de evaluaciones", 
  "Planificación de cursos, evaluación, metodologías de enseñanza", 
  "Planificación de evaluación, Competencias.",
  "como planificar con el CNB, como planificar actividades de mejoramiento , adecuación curricular", 
  "Planificación de los Aprendizajes, técnicas de evaluación, evaluación por competencias", 
  "Planificación de los aprendizajes en contextos interculturales", 
  "Planificación de los aprendizajes, evaluación de los aprendizajes y solución de casos", 
  "Planificación de telesecundaria", "Planificación del CNB", 
  "Planificación del área curricular de xinka, malla curricular, evaluación.", 
  "Planificación desde el enfoque del CNB. Instrumentos de Evaluación de los aprendizajes. Tecnología y cnb", 
  "Planificación docente, el desarrollo.de competencias, formación  en la creación y uso de recursos educativos incluyendo materiales digitales, y otros recursos didácticos.", 
  "Planificación educativa, competencias y técnicas",
  "Planificación en Base al CNB, Sistematización de Contenidos, La Dosificación de Contenidos cómo herramienta fundamental.idos,", 
  "Planificación en CNB y Evaluación",
  "Planificación por Competencia. Herramientas de Evaluación. Innovación Curricular", 
  "Planificación por Competencia. Innovación Curricular. Herramientas de Evaluación.", 
  "Planificación uso cnb planes", "Planificación y Evaluación de aprendizaje/ Herramientas de Evaluación s", 
  "Planificación y agenda pedagógica, basado en el CNB y aplicación de tecnicas de evaluación en el CNB", 
  "Planificación y curriculum, etapas de la planificación, modelos de planeación de los aprendizajes", 
  "Planificación y docencia, estructura de dosificación y competencia, capacidades para la vida.", 
  "Planificación y evaluación , comprensión lectora,  herramientas de aprendizaje", 
  "Planificación y evaluación, herramientas de aprendizaje, comprensión lectora", 
  "Planificación y herramientas de evaluación",
  "Planificación y lectura,  evaluación del aprendizaje y habilidades psicomotrices", 
  "Planificación y lectura, Desarrollo spcomotriz del alumno y capacidad de aprendizaje", 
  "Planificación,  competencias", "Planificación,  metodología e indicadores de logro", 
  "Planificación, Derechos del niño y adolescente, evaluación", 
  "Planificación, Ejes transversales del CNB, Planificación y dosificación de clases", 
  "Planificación, Estructura CNB, Plan de Mejora.", "Planificación, Evaluacion, Practica Supervisada", 
  "Planificación, Evaluación de los aprendizajes. Malla curricular", 
  "Planificación, Evaluación, Inlución", "Planificación, Metodos y Estrategia pedagogicas", 
  "Planificación, NEE Estructura del CNB", "Planificación, Plan de mejora, Evaluación", 
  "Planificación, competencia y habilidades lingüísticas", "Planificación, competencia, material didáctico", 
  "Planificación, competencias y", "Planificación, competencias y currículo.", 
  "Planificación, conocimiento del CNB, evaluación", "Planificación, contenidos, estructura.", 
  "Planificación, diario de clases técnicas para impartir nuestras clases", 
  "Planificación, evaluación y contenido", "Planificación, forma de evaluar y cómo alcanzar objetivos.", 
  "Planificación, función del CNB",
  "Planificación, herramientas de evaluación, relación de indicadores con las actividades", 
  "Planificación, malla curricular, los objetivos", "Planificación, mallas curriculares estrategias de uso", 
  "Planificación, objetividad, CNB",
  "Uso de competencias.  Aplicacion de instrumentos y herramientas de evaluación", 
  "Uso de competencias, uso de indicadores de logro y como evaluar usando cnb",
  "Uso correcto del CNB", "Uso de CNB, Planificar con CNB, Evaluación.", 
  "Planificación, pasos de la planificación, conocer las áreas del CNB", 
  "Planificación-Evaluación-Competencia",
  "Planificación.evaluacion de los aprendizajes y uso de erramientas para el aprendizaje", 
  "PlanificaciónCNB, Criterios de evaluaciónCNB y educación integral con CNB", 
  "Planificando curricular, uso del CNB, metodología del CNB", 
  "Planificar por unidad, herramientas para evaluar el aprendizaje, competencias", 
  "Planificar,  competencias y herramientas de evaluación",
  "Planificar,  herramientas de evaluación,  competencias", 
  "Planificar,  herramientas de evaluación, vaciado de notas a cuadros de registros y fórmulas para porcentajes",
  "Preparación de clases, desarrollo cognitivo y estructura de contenido"
)

### uso de tics
valores_uso_tics <- c(
  "1. uso de las Tic 2. Gesto Administrativa 3.Adecuaciones Curriculares",
  "CNB COMO ERAMIENTA, LAS TICS, PAUSAS ACTIVAS",
  "herramientas digitales, herramientas tecnológicas, planificación de clase", 
  "herramientas digitales, herramientas tecnológicas, planificación de clase,",
  "La Tecnología y el CNB, Importancia de los juegos Lúdicos en clase, El contexto en el CNB",
  "Inteligencia Artificial,  Brecha Digital, Cerviacoso",
  "Inteligencia artificial utilizada en la educación , estrategias de enseñanza y aprendizaje, metodologías de aprendizaje", 
  "Inteligencia artificial, casa común y naturaleza",
  "Uso correcto de las tic's en el nivel básico, Implementación del CNB para el proceso de mejoramiento en nivel básico, Implementación del CNB para la planificación curricular", 
  "Uso correcto de las tic's en la educación media.  Implantación del CNB para el proceso de mejora en nivel medio.  Implantación del CNB para la planificación curricular.",
  "Plataformas Digitales para la enseñanza, Planificación Curricular en base del CNB de Guatemala y Estrategias de Enseñanza del CNB.",
  "Planificación con base al CNB, Evaluación con Criterios del CNB, Uso de herramientas tecnológicas", 
  "Herramientas de Aprendizaje,Ideas y estrategias en el Aula, Estrategias en Tecnología",
  "Cnb como herramienta, Pausas Activas, Estratégias de Enseñanza, El uso de las Tics",
  "Componentes del CNB, promoción de valores, uso de la tecnología",
  "Cómo planificar con el CNB de Nivel Medio, Metodologías para la Evaluación y Uso de la IA en la educación.",
  "Discapacidad - coordinación motora - herramientas tecnológicas",
  "Estructura del CNB, Estrategia de Enseñanza, Plataformas digitales",
  "Estructura del CNB, Estrategias de enseñanza, plataformas digitales",
  "Estructura del CNB, estrategias de enseñanza, plataformas digitales",
  "Evaluación de los aprendizajes por competencias, tecnologías del aprendizaje y sobre los desechos y residuos sólidos desde la perspectiva del CNB.",
  "Evaluación de los aprendizajes, Tecnología para el Aprendizaje y la Comunicación. Residuos y Desechos Sólidos desde el CNB de Ciencias Naturales",
  "Integración curricular, aprendizaje significativo, tics",
  "Uso de las TIC, Gestión Administrativa y Adecuaciones Curriculares", 
  "Uso de las tics en el nivel secundario. Implementación del cnb para el proceso de evaluación, Uso correcto del cnb para la planificación curricular", 
  "Manejo de Herramientas digitales, Aplicación de las herramientas, Conocimiento de las herramientas digitales para el CNB."
  
)

### material didactico o estrategias
valores_material_actividad <- c(
  "1.-Enfoque curricular por competencias desde una perspectiva integral- Materiales didacticos",
  "Actividades lúdicas para una mejora en el uso del cnb, actividades para trabajar contenidos, como utilizar los contenidos",
  "Aprendizaje a través de neurociencias. Didáctica de la matemática,",
  "Aprendizaje experiencial, materiales para usar, herramientas digitales",
  "Aprendizaje significativo (de acuerdo a herramientas actuales), uso del CNB para la evaluación de aprendizajes, uso de herramientas tecnológicas para el desarrollo de actividades y material didáctico, aplicación y realización basado en proyectos",
  "Bases para planificar, estrategias para la motivación, estrategias para la lecto escritura.",
  "CNB Estrategias para enseñar matemáticas unidas a Comunicación y Sociales",
  "CNB evaluacion, planificacon , materiales",
  "Pausa activa",
  "“La educación a distancia, un futuro que se hizo presente”, \"Planificando con el CNB Nivel Medio\", \"Didactica para el desarrollo del CNB\"",
  "Técnicas de Aprendizaje. Formas de evaluación Lúdica, planificación en cnb", 
  "Técnicas de aprendizaje, técnicas de evaluación contenidos del currículo en el área ocupacional", 
  "Recreación, nueva metodología de enseñanza y actividades aeróbicas y anacrónicas", 
  "Procidimentales, actitudinales, Fortalecimiento del CNB, aplicaciones de herramientas de evaluación", 
  "Proceso de mejoramiento, malla curricular, aprendizaje montesori",
  "Proceso de Mejoramiento de los aprendizajes, Metodologías Activas en el aula, Planificación,",
  "Planificacion, estrategias de aprendiza, Hettamientas de Evaluacion",
  "Planeación de los aprendizajes por competencias, herramientas de evaluación, Uso de materiales audiovisuales.", "Planificacion Herramientas el aprendizaje Lector de Evaluacion", 
  "Planificacion curricular. Curso del CNB. Metodologías y herramientas del CNB",
  "Material didactico, la importancia del idioma materno y Enfoque curricular por competencia.",
  "Herramientas de Aprendizaje,Ideas y estrategias en el Aula, Estrategias en Tecnología",
  "Evaluación. Didáctica. Constructivismo",
  "Metodología del aprendizaje, estrategias y didácticas del aprendizaje, plan de formación de los saberes", 
  "Metodología, competencia, Indicadores de logroadores de logro", 
  "Metodología, la participación ciudadana, diversidad cultural", 
  "Metodología, recursos, estructura", "Metodologías de enseñanza y estrategias didácticas para el desarrollo del CNB, Evaluación del aprendizaje alineada al CNB, Inclusión, diversidad cultural y lingüística en el currículo",
  "Manejo del CNB, Estrategias de aprendizajez, Planificación curricular", 
  "Manejo del CNB, Metodología, Estrategias",
  "metodologías del aprendizaje, evaluación del aprendizaje y áreas curriculares", 
  "logros, contenidos, estrategias, valores,competencias, material",
  "La Tecnología y el CNB, Importancia de los juegos Lúdicos en clase, El contexto en el CNB", 
  "Inteligencia artificial utilizada en la educación , estrategias de enseñanza y aprendizaje, metodologías de aprendizaje",
  "Herramientas didácticas. L1. Kiche.  Lector escritura", 
  "Evaluación, planificación y actividades pedagogicas",
  "Evaluación de los aprendizajes. Las competencias en el CNB. Estrategias de Aprendizaje",
  "Estructura del CNB, estrategias de enseñanza, plataformas digitales",
  "Estructura del CNB, Herramientas de evaluación, Herramientas de enseñanza-aprendizaje, Pérdida de atencion en clases, clases dinámicas.",
  "Estructura del CNB, Estrategia de Enseñanza, Plataformas digitales",
  "Estructura del CNB, Estrategias de enseñanza, plataformas digitales",
  "Estructura del CNB, Adecuaciones Curriculares, Bases para realizar material educativo.",
  "Estrategias de aprendizaje. Metodologías de investigación",
  "Estrategias de aprendizaje, evaluación de aprendizajes, pensamiento critico ,",
  "Cnb como herramienta, Pausas Activas, Estratégias de Enseñanza, El uso de las Tics",
  "Competencias para la vida, técnicas y estrategias, pilares educativo, modelos educativos, metodologías,",
  "Competencias, actividades, evaluación", "Competencias, contenido y evaluación",
  "Competencias, estrategias de evaluación, actividades de aprendizaje",
  "Competencias, planificación, materiales didácticos,",
  "Componentes del CNB, Pilares de la Educación y Estrategias",
  "Contenidos, criterios de evaluación y actividades",
  "Evaluaciones, técnicas de aprens",
  "Planificación con el CNB, Estrategias docentes, Didáctica en el nivel medio,",
  "Currículo base estrategias técnicas de investigación",
  "Currículum base estrategias de aprendizaje en los cursos",
  "Cómo utilizar el CNB, Herramientas de Evaluación según el CNB, Estrategias de Aprendizaje según el CNB",
  "Dificultad de aprendizaje, técnicas de aprendizaje, recursos de aprendizaje",
  "Diseño curricular, planificación y didáctica, evaluación de aprendizaje",
  "Diseño curricular, rutas de aprendízaje, elaboración de planificaciones",
  "Dosificación- Competencias, contenidos y Actividades.",
  "ESTRUCTURA DEL CNB, Estrategias de enseñanza,  plataformas digitales",
  "Educación activa, formas creativas para evaluar, apoyo al estudiante",
  "El constructivismo como base para el CNB, Modelos de clase en base al Nuevo Currículo, Técnicas de enseñanza-aprendizaje basadas en el CNB.",
  "Entorno, destrezas y habilidades de aprendizaje",
  "Estrategia para el aprendizaje", "Estrategias",
  "Estrategias de Aprendizaje", "Estrategias de Aprendizaje, Implementación del CNB, Evaluación de Proyectos",
  "Estrategias de enseñanza", "Estrategias de evaluación, Aprendizaje Significativo y Desarrollo de habilidades",
  "Estrategias para dar clases",
  "Estrategias para la enseñanza y aprendizaje basado en el CNB",
  "cnb clases dinamicas pandemia"
  
)

### actualizacion cnb
valores_actualizacion_cnb <- c(
  "Actualización constante de CNB", "Actualización curricular, propuestas CNB bachillerato por madurez.",
  "Actualización del CNB",
  "como planificar con el CNB, como planificar actividades de mejoramiento , adecuación curricular", 
  "Actualización docente con base al CNB, criterios de evaluación según el CNB, implementación de clases integrales",
  "Actualización docente en base al CNB, nuevos modelos de evaluación según el CNB, planificación con CNB",
  "CNB, Actualización Docente, Herramientas de Evaluación",
  "Contextualización del CNB, Instrumentos de Evaluación, Actualización del CNB",
  "El constructivismo como base para el CNB, Modelos de clase en base al Nuevo Currículo, Técnicas de enseñanza-aprendizaje basadas en el CNB.",
  "El nuevo CNB",
  "Estructura del CNB, actualización del CNB, aplicación del CNB en el aula.",
  "El nuevo CNB DE PREPRIMARIA, EL ABORDAJE SOBRE EL USO DE LAS HERRAMIENTAS PEDAGÓGICAS Y LA CONTEXTUALIZACIÓN DEL CNB.",
  "Estructura del CNB, Actualización del CNB",
  "Estructura del CNB, actualización del CNB y Correcta Aplicación del CNB",
  "Innovación Curricular, Desarrollo de competencias fundamentales para estudiantes y docentes, Estrategias Pedagógicas", 
  "Innovación educativa, CNB en el aula y la conformación del CNB"
  
)

### adecuacion curricular
valores_adecuacion <- c(
  "Adecuacion Curricular, Desarrollo de Competencias, Habilidades y competencias digitales",
  "Adecuaciones Curriculares, Planificación con el CNB, Evaluación con el CNB",
  "Adecuaciones curriculares, estrategias del uso del CNB, como planificar con el CNB",
  "Adecuación curricular, Planificación y Estrategias de Enseñanza.",
  "Adecuación curricular, Planificación y competencias",
  "desarrollo, como usarlo, adecuarlo",
  "Uso de las TIC, Gestión Administrativa y Adecuaciones Curriculares",
  "Relación entre competencias, indicadores de logro y contenidos. Enfoque constructivista y aprendizaje significativo. Adaptación Curricular",
  "Ramificacionbde temas, competencias, indicadores de logros. Adecuación curricular", 
  "Planificaciones, Adaptaciones curriculartes y unificacion de las artes para Expresión Artística", 
  "Planificaciones, adecuación curricular", "Planificació, evaluación de los aprendizajes , adecuación  curricular",
  "Niveles de concresión curricular, contenidos, Adecuación curricular",
  "Implementación de ajustes para tener una participación efectiva de cada estudiante.",
  "Evaluación de proyectos, ejecución de proyectos y adecuación curricular",
  "Adecuaciónes curriculares, Planeamiento y planeación, Estrategias de aprendizaje con relaión al CNB",
  "Aplicaciones de CNB Adecuación curricular Evaluación",
  "Transformación curricular, adecuación curricular, currículo nacional base", 
  "Inclusión, Adaptación de actividades acopladas a los temas del CNB y ejes del CNB", 
  "Inclusión, adaptación de contenidos y actividades,  Ejes del CNB",
  "CNB, Adaptación del Curriculo y Técnicas de Evaluación Constante",
  "CNB, Adaptación del curriculo, Tecnicas de evaluación constante",
  "Conociendo el area, contextualiando los temas, flexibilidad",
  "Contextualización del CNB, Instrumentos de Evaluación, Actualización del CNB",
  "Contextualizar. Planificar conocimientos previos",
  "Uso adecuado del CNB, Adecuación Curricular",
  "Evaluación de los aprendizaje, adaptación de contenidos del CNB, Malla curricular",
  "Cómo está divido el cnb, participando integral buscar la flexibilidad para adaptarse",
  "Cómo planificar con el CNB, como implementar los trabajos de mejoramiento, adecuación curricular",
  "Cómo planificar en base al CNB, contextualizar contenidos, utilizar competencias de acuerdo al contenido.",
  "Cómo planificar, contextualizar contenidos, sobre competencias",
  "Diario pedagógico, evaluación, adecuación curricular",
  "Discapacidad, autismo adecuación curricular",
  "Reforma curricular",
  "Rendimiento academico, actualización educativa, motivación al alumno.",
  "Relación entre competencias, indicadores de logro y contenidos. Enfoque constructivista y aprendizaje significativo. Adaptación Curricular",
  "Planificación curricular", "Planificación curricular , 2. Adecuación curricular,",
  "Estrategias de evaluación basado en el CNB, Planificación de clases según el contexto, metodología para estudiantes de adecuación curricular",
  "Estructura del CNB, Adecuaciones Curriculares, Bases para realizar material educativo.",
  "Planificación curricular, métodos de enseñanza y adecuaciones curriculares"
)

### aplicacion del cnb
valores_aplicacion_cnb <- c(
  "Aplicación de CNB general aplicación específica Comunicación y lenguaje",
  "Aplicación de Contenidos del CNB", "Estructura, aplicación, metodología",
  "Aplicación de la tecnología en el CNB. Aplicación de las competencias del CNB. Los retos del CNB",
  "Aplicación del CNB, cómo integrar áreas o subareas y cómo evaluar los contenidos de CNB",
  "Aplicación del Curriculum Nacional Base",
  "desarrollo, como usarlo, adecuarlo",
  "Uso de material educativo, Aplicación de Curriculum Nacional Base, Uso de CNB para planificacion,",
  "Uso adecuado del CNB, Adecuación Curricular",
  "Uso de las tics en el nivel secundario. Implementación del cnb para el proceso de evaluación, Uso correcto del cnb para la planificación curricular",
  "Uso correcto de las tic's en el nivel básico, Implementación del CNB para el proceso de mejoramiento en nivel básico, Implementación del CNB para la planificación curricular", 
  "Uso correcto de las tic's en la educación media.  Implantación del CNB para el proceso de mejora en nivel medio.  Implantación del CNB para la planificación curricular.", 
  "Uso correcto del CNB", "Uso de CNB, Planificar con CNB, Evaluación.", 
  "Uso de competencias de grado, aplicación de indicadores de logro", 
  "Uso de competencias, uso de indicadores de logro y como evaluar usando cnb", 
  "Uso de competencias.  Aplicacion de instrumentos y herramientas de evaluación", 
  "Qué es el CNB. Componentes del CNB. Implementación del CNB.",
  "Planificación Curricular,  Uso del CNB y Herramientas de Evaluación",
  "Implementacion del CNB, Políticas Educativas, ODEC, Hacia la Reforma Educativa.",
  "Herramientas de evaluación, implementaron del CNB,",
  "Herramientas de evaluación y aplicación del CNB",
  "Planificación de contenidos, Estructura de CNB, Implementación del CNB",
  "Planificación curricular, uso de CNB, metodología y herramientas de evaluación", 
  "Planificación curricular, uso del CNB y como usar CNB",
  "La importacia del uso del CNB en el proceso educativo.",
  "Herramientas de evaluación, aplicación del CNB, introducción al CNB.", 
  "Herramientas de evaluación implementación del cnb",
  "Herramientas de Evaluación, Implementaron del CNB. s",
  "Manejo del CNB, planificación docente y Habilidades y contenidos específicos", 
  "Manejo y estructura del CNB.  Registro de evaluación y competencias.",
  "Herramientas de Evaluacion. Uso e implementacion del CNB.",
  "Experiencias de aplicación del CNB, Desafíos del CNB y Importancia del desarrollo de competencias.",
  "Estructura,  como implementarlo , como usar las herramientas de evaluacion",
  "CNB - nuevos temas del CNB - cómo usar el CNB en mis clases",
  "Utilización y aplicación de los indicadores de logro. Temas acorde al nivel educativo asignado. Actividades acorde al nivel educativo asignado.", 
  "CNB, análisis, aplicación", "Estructura y aplicación del CNB",
  "CNB, planificación por competencias, uso del CNB",
  "COMO PLANIFICAR EL CNB, ESTRATEGIAS Y METODOLOGÍAR CONO USAR EL CNB",
  "Cnb como herramienta, Pausas Activas, Estratégias de Enseñanza, El uso de las Tics",
  "Como alcanzar las competencias, indicadores de logro, criterios de evaluación",
  "Como aplicar competencias",
  "Herramientas  de evaluación, implementar  el CNB",
  "Flexibilidad del CNB, Principal objetivo del CNB, Aplicación del CNB",
  "Evaluación a través del CNB, uso de competencias de acuerdo al CNB y el uso de indicadores de logro",
  "Estructura del CNB, actualización del CNB, aplicación del CNB en el aula.",
  "Estructura del CNB, actualización del CNB y Correcta Aplicación del CNB",
  "Como aplicar el CNB, La competencias y los indicadores de logro.",
  "Como esta estructurado, aplicación y metodología",
  "Como planificar con ayuda del CNB, Como desarrollar competencias y como aplicarlas Como adaptar la planificación si un joven o señorita es un caso especial",
  "Como planificar con el CNB, tipos de planificación",
  "Como planificar según el CNB. Etapas de la evaluación.  Desarrollo de competencias e indicadores de logro",
  "Como usar las herramientas del CNB como apoyo para el desarrollo del curso",
  "Como utilizar adecuadamente el CNB, Currículo nacional base, como utilizar adecuadamente las herramientas de evaluación",
  "Como utilizar el cnb, competencias y microciclos de trabajo, mays currícular de educación física.",
  "Conocimiento  del CNB, Aplicación del CNB y Herramientas de Evaluación",
  "Conocimiento del CNB, aplicación del mismo y evaluación",
  "Conocimiento e implementación del CMB, Aplicación de herramientas de Evaluación con base del CNB",
  "Criterios de Evaluacion, Aplicacion de Competencias, Curriculum,",
  "Cómo usar el CNB, la estructura y cómo saber si lo utilizamos bien según la evaluación del estudiante",
  "Cómo usar laPlanificación, el uso adecuado del cnb",
  "Desarrollo de competencias, Aplicación de contenidos, Implementación del CNB de educación física, metodología de educación física, evaluación de los aprendizajes",
  "Didactica de las matematicas y uso de cnb",
  "Dwfinicion del CNB APLICACION Y EVALUACION", "El uso de erramientas",
  "El uso del CNB", "Estrategias de Aprendizaje, Implementación del CNB, Evaluación de Proyectos",
  "Emplear contenidos del CNB y la cultura maya,Instrumentos de evaluación según el CNB y planificación según el CNB",
  "Enfoques, áreas, temas generadores",
  "Implementación de la planificación, planificación y currículum", 
  "Implementación de temas del CNB correctamente,  buena expresión de normas de ejecución de ejercicio, dar clases de acuerdo a los emas del CNB", 
  "Implementación del CNB, Componentes del CNB, Como hacer planes de mejora", 
  "Implemtacion del CNB", "la ley pina, CNB aplicación en el aula, evaluación"

)

### enfoque curricular por competencias
valores_enfoque_competencias <- c(
  "Enfoque Curricular por Competencias desde un Perspectiva Integral, Material didáctico y La importancias del Idioma k'iche'",
  "Enfoque Curricular por Competencias desde una Perspectiva  Integral, materiales didácticos, la importancia del idioma Kiche´.",
  "Enfoque Curricular por Competencias desde una perspectiva Integral, Materiales didácticos y La importancia del idioma K'iche'",
  "Enfoque Curricular por Competencias desde una perspectiva integral, Materiales didácticos y La Importancia del idioma K'iche'",
  "Enfoque Curricular por competencias desde una perspectiva integral, Materiales didácticos y La Importancia del idioma K'iche'",
  "Enfoque Curricular por competencias desde una perspectiva integral, Materiales didácticos y la importancia del idioma kiche´.",
  "Enfoque curricular por competencia, desde una perspectiva integral. Materiales didácticos y la importancia del idioma K'iche'.y la importancia",
  "Enfoque curricular por competencias desde una perspectiva integral, Materiales didácticos y La importancia del idioma K'iche'."
)

### educacion intercultural bilingue
valores_edu_inter_bilingue <- c(
  "Educacion Intercultural Bilingue",
  "Educación Intercultural Bilingue para una ciudadanía Activa",
  "Educación Intercultural Bilingüe para una Ciudadanía Activa",
  "Evaluación de los aprendizajes, Educación Bilingue",
  "Rutas de aprendizaje, Secuencias didacticas y Educación Intercultural y Bilingue", 
  "Metodologías de enseñanza y estrategias didácticas para el desarrollo del CNB, Evaluación del aprendizaje alineada al CNB, Inclusión, diversidad cultural y lingüística en el currículo",
  "Interculturalidad, La Equidad de Género, Desarrollo Sostenible",
  "Herramientas de evaluación. Contextos culturales",
  "Multiculturalidad e interculturalidad,  desarrollo de ejes transversales, aprendizajes significativos, docentes y sus practicas innovadoras."
  
)

### integracion curricular
valores_integracion <- c(
  "Integración curricular, aprendizaje significativo, tics",
  "Integración curricular, malla curricular, plan de mejora", 
  "Integración curricular. Malla curricular. Plan de mejora.", 
  "Integración de contenidos. Diferencia entre indicares de logro y competencia. Como planificar."
  
)

### modelos de aprendizaje o educativos
valores_modelos <- c(
  "Modelos de Aprendizaje. Modelos Educativos. Introduccion al CNB",
  "Métodos y técnicas pedagógica para el desarrollo educativo",
  "Programa de Sensibilización DIGECADE"
)
### desarrollo de competencias
valores_competencias <- c(
  "Desarrollo de competencias",
  "desarrollo de las áreas, desarrollo de las competencias, evaluación", 
  "Desarrollo de competencias en el aula. El marco teórico.  Estrategias de evaluación",
  "Desarrollo de competencias, Aplicación de contenidos, Implementación del CNB de educación física, metodología de educación física, evaluación de los aprendizajes",
  "Desarrollo de competencias, indicador de logro, áreas curriculares",
  "Desarrollo de habilidades, como planificar el cnb",
  "Evaluación de los aprendizajes, Desarrollo de competencias, ¿Para que sirven los Indicadores de Logros?",
  "Experiencias de aplicación del CNB, Desafíos del CNB y Importancia del desarrollo de competencias."
  
)

### aprendizaje experiencia o significativo
valores_aprendizaje_cnb <- c(
  "Aprendizaje", "Aprendizaje a través de neurociencias. Didáctica de la matemática,",
  "Aprendizaje experiencial, materiales para usar, herramientas digitales",
  "Aprendizaje significativo (de acuerdo a herramientas actuales), uso del CNB para la evaluación de aprendizajes, uso de herramientas tecnológicas para el desarrollo de actividades y material didáctico, aplicación y realización basado en proyectos"
  
)
### no especifica capacitaciones
valores_noespecifica_capa <- c(
  "2", "CNB.", "CnB", "Cnb", "Conpetencia", "Docente", "El cnb",
  "Fueron inducciones las que recibí por haber ingresado a laborar por contrato en el Mineduc",
  "Meta  Audaz", "Ninguna", "No re", "No recuerdo", "No se", "Si",
  "no", "xxxxxxx"
)


### metodologia de investigacion
valores_metodo_inves <- c(
  "Estrategias de aprendizaje. Metodologías de investigación",
  "Evaluación por competencias,  laboratorio para la investigación"
)

### educacion ambiental y en valores
valores_edu_ambiental_valores <- c(
  "Ambientales, seguridad social, educación en valores",
  "Competencias, logros, evaluaciones, contendio,maya curricular valores,",
  "Componentes del CNB, promoción de valores, uso de la tecnología",
  "Educación y valores. Planificación.",
  "Metodología de evaluación,  componentes del CNB, promoción de valores",
  "Evaluación de los aprendizajes por competencias, tecnologías del aprendizaje y sobre los desechos y residuos sólidos desde la perspectiva del CNB.",
  "Evaluación de los aprendizajes, Tecnología para el Aprendizaje y la Comunicación. Residuos y Desechos Sólidos desde el CNB de Ciencias Naturales",
  "Interculturalidad, La Equidad de Género, Desarrollo Sostenible"
)

### primeros auxilios
valores_primeros_auxilios <- c(
  "la realidad de las drogas,  primeros auxilios con los bomberos",
  "Uso de las Drogas, Primeros auxilios, uso de la IA", 
  "Uso de las Drogas. Primeros auxilios, Administración",
  "Uso de las Drogas, Primeros Auxilios y uso de la IA"
)
### capacitacion en áreas
valores_areas <- c(
  "Capacitación en Matemáticas, Uso de la Calculadora Científica, Idioma Maya",
  "Como utilizar el cnb, competencias y microciclos de trabajo, mays currícular de educación física.",
  "Competencias de áreas numéricas, competencias de ciencias naturales y química",
  "Contabilidad, sociales", "Didactica de las matematicas y uso de cnb",
  "Didáctica en la educación de nivel medio. Soy un docente didáctico. Herramientas digitales para la educación de nivel medio.",
  "Educación física adaptada. Planificación. Actividad recreación",
  "Educación física y recreación, deporte adaptado, deporte individual.",
  "Evaluación y aprendizaje, habilidades psicomotrices y comprensión lectora",
  "Fonologia Xinka, componentes del curriculo, Gramatica Xinka",
  "Formación de Lideres brillantes, Estructura del CNB, Educación Artística y el CNB integrados",
  "Las Matemáticas, actividades y planes.",
  "Álgebra, lógica, sistema de numeración", "Álgebra.  Fijugirss geométricas  sumas", 
  "Técnicas de evaluación, técnicas de enseñanza de matemáticas, técnicas de lectoescritura", 
  "Recursos musicales adaptados al entorno, adaptación de obras musicales, Alfabeto Maya Mam.", 
  "Lectura, Escritura y comprensión lectora",
  "Utilización de herramientas matematicas",
  "Manual de Crecimiento docente,comités escolares, elaboración del pan escolar",
  "Lectura, escritura y comprensión lectora", 
  "Lectura, redacción y comprensión lectora",
  "Los signos linguisticos.", "Nornas Básicas del Idioma Mam",
  "Modelo de Telesecundaria, matemática maya, Matemática",
  "Liderazgo Educación Inclusiva Derechos Humanos"
  
)

### identificacion de necesidades especiales
valores_necesidades_especiales <- c(
  "Como Identificar las Necesidades Especiales, Como Planificar Diario, Bimestral y Anual.",
  "Como planificar con ayuda del CNB, Como desarrollar competencias y como aplicarlas Como adaptar la planificación si un joven o señorita es un caso especial",
  "Dificultad de aprendizaje, técnicas de aprendizaje, recursos de aprendizaje"
)

### psicologia educativa
valores_psico_educativa <- c(
  "Psicología educativa, psicología social, neuroprendizaje",
  "Psicología en adolescencia, psicología en la niñez,",
  "Recursos humanos, Escuelas seguras y motivacional",
  "Uso de las Drogas, Primeros Auxilios y uso de la IA",
  "Uso de las Drogas, Primeros auxilios, uso de la IA", 
  "Uso de las Drogas. Primeros auxilios, Administración", "Uso de las TIC, Gestión Administrativa y Adecuaciones Curriculares", 
  "Rendimiento academico, actualización educativa, motivación al alumno."
)

### discapacidad
valores_discapacidad <- c(
  "Discapacidad - coordinación motora - herramientas tecnológicas",
  "Discapacidad, autismo adecuación curricular",
  "Discapacidad, recreación,",
  "Educaciòn inclusiva, educaciòn lùdica, planificaciòn y retroalimentaciòn con estudiantes con capacidades diferentes.",
  "Elaboración de objetivos estructura de la clase, clases con chicos con discapacidad"
  
)

### foda
valores_foda <- c(
  "FODA"
)

### modelo telesecundaria
valores_telesecundaria <- c(
  "Modelo de Telesecundaria, matemática maya, Matemática"
)

### gestion de clase y manejo del entorno digital
valores_gestion_entorno <- c(
  "Gestión de clase y manejo del enterno digital",
  "Gestión de clase y manejo del entorno digital",
  "manejo del sire",
  "“La educación a distancia, un futuro que se hizo presente”, \"Planificando con el CNB Nivel Medio\", \"Didactica para el desarrollo del CNB\"",
  "Gestión de clase y manejo del entorno digital. Clase modelo.", 
  "Gestión de las clases y manejo de entorno digital, clase modelo y planificación",
  "Herramientas de Evaluación, Transformación Digital, Herramientas educativas digitales"
)

### politicas educativas
valores_politica_edu <- c(
  "Implementacion del CNB, Políticas Educativas, ODEC, Hacia la Reforma Educativa."
  
)

### equidad de genero
valores_equidad <- c(
  "Interculturalidad, La Equidad de Género, Desarrollo Sostenible",
  "la sostenibilidad , la equidad"
)


## Recode case match ----

df_dyd  <- df_dyd |>
  mutate(
    capacitaciones_temas = case_match(
      capacitaciones_temas,
      all_of(valores_estrategias) ~ "Estrategias metodológicas y planificación integrada",
      all_of(valores_contenidos) ~ "Contenido y estructura del CNB",
      all_of(valores_plani_evaluacion) ~ "Planificación y evaluación con el CNB",
      all_of(valores_uso_tics) ~ "Uso de TICS",
      all_of(valores_material_actividad) ~ "Actividades didácticas, estrategias y/o material lúdico",
      all_of(valores_actualizacion_cnb) ~ "Actualización del CNB",
      all_of(valores_adecuacion) ~ "Adecuación curricular",
      all_of(valores_aplicacion_cnb) ~ "Uso y aplicación del CNB",
      all_of(valores_enfoque_competencias) ~ "Enfoque curricular por competencias",
      all_of(valores_edu_inter_bilingue) ~ "Educación intercultural bilingüe",
      all_of(valores_integracion) ~ "Integración curricular",
      all_of(valores_modelos) ~ "Modelos de aprendizaje o educativos",
      all_of(valores_competencias) ~ "Desarrollo de competencias",
      all_of(valores_aprendizaje_cnb) ~ "Aprendizaje significativo o experiencial",
      all_of(valores_noespecifica_capa) ~ "Sin especificar",
      all_of(valores_metodo_inves) ~ "Metodología de investigación",
      all_of(valores_edu_ambiental_valores) ~ "Educación ambiental y en valores",
      all_of(valores_primeros_auxilios) ~ "Primeros auxilios",
      all_of(valores_areas) ~ "Capacitación en área (matemática, lectoescritura)",
      all_of(valores_necesidades_especiales) ~ "Identiticación de necesidades especiales",
      all_of(valores_psico_educativa) ~ "Psicología y educación",
      all_of(valores_discapacidad) ~ "Discapacidad",
      all_of(valores_foda) ~ "FODA",
      all_of(valores_telesecundaria) ~ "Modelo de telesecundaria",
      all_of(valores_gestion_entorno) ~ "Gestión de clase y manejo de entorno digital",
      all_of(valores_politica_edu) ~ "Políticas educativas",
      all_of(valores_equidad) ~ "Equidad de género",
      .default = capacitaciones_temas
    )
  )

## volver a ver errores 

valores_capacitaciones_temas <- df_dyd |>
  filter(capacitaciones_cnb != "No he recibido capacitaciones sobre los temas mencionados anteriormente") |> 
  filter(capacitaciones_numero != "0") |>
  filter(!(is.na(capacitaciones_temas))) |>
  select(capacitaciones_temas) 

# otro_contratante ----

## ver errores

valores_otro_contratante <- df_dyd |>
  filter(contratante == "Otro") |>
  filter(!(is.na(otro_contratante))) |>
  select(otro_contratante) 



## Define los vectores ----

### administración del establecimiento
valores_admin_establecimiento <- c(
  "Administración del Establecimiento", "El Comité o Cooperativa del instituto.",
  "administracion del establecimiento",
  "Dirección Administrativa del Colegio", "El Centro Educativo",
  "La dueña", "La institución donde laboro",
  "La propietaria del establecimiento", "Propietario",
  "Una maestra interina"
)

### Asociacion para el desarrollo integral
valores_adisa <- c(
  "Asociación de Desarrollo Intengral Adisa"
)

### junta directiva
valores_junta_directiva <- c(
  "Jubta directiva", "Junta Directiva", "Junta directiva",
  "La Junta Directiva", "La junta directiva", "Los dirigentes de la junta directiva"
)

### junta directiva del establecimiento
valores_junta_establecimiento <- c(
  "Junta DIrectiva de Cooperativas", "Junta Directiva de la Cooperativa",
  "Junta Directiva del Instituto", "Junta cooperativa",
  "Junta directiva del establecimiento", "Junta por Cooperativa", "La Cooperativa",
  "La Junta Diretiva del Establecimiento", "La cooperativa",
  "La junta Cooperativa", "La junta cooperativa", "cooperrativa"
  
)

### junta directiva padres de familia
valores_junta_padres <- c(
  "Asociación de padres de familia",
  "Contratada por junta directiva por padres de familia",
  "Contrato por actas de junta directivas y por padres de familia",
  "Directiva de Padres de Familia.",
  "Padres de Familia del centro educativo",
  "Directiva de padres de familia.",
  "La junta de padres de familia",
  "Padres de familia", "Padres de familia y el director",
  "Junta Directiva de padres de familia.",
  "Junta Directiva padres de familia",
  "Junta Directiva padres de familia.",
  "Junta de Padres de Familia", "La JUnta Directiva de Padres de Familia",
  "Junta de padres de familia", "Organización de padres de familia",
  "La Directiva de Padres de Familia",
  "Junta directiva de padres de familia",
  "junta Directiva de Padres de Familia",
  "Padres de familia.", "Presidente padres de familia del establecimiento.",
  "Por actas y padres de familia",
  "JUNTA DE PADRES DE FAMILIA", "Junta Directiva de padres de familia",
  "Junta Directiva de Padres de Familia",
  "Junta Directiva de Padres de Familia del Centro Educativo"
)

### directiva de la cooperativa del plantel
valores_directiva_plantel <- c(
  "Directiva de la Cooperativa del plantel"
)

### Director o directora del establecimiento
valores_director <- c(
  "Director", "Director del establecimiento", "Directora, junta cooperativa",
  "Padres de familia y el director", "Profe Nery Ralda Maldonado",
  "El Director del Establecimiento"
)

### EduRed
valores_edured <- c(
  "Edured S.A"
)

### INDE
valores_inde <- c(
  "Instituto Nacional de Electrificación", "Instituto Nacional de Electrificación \"INDE\"",
  "Instituto Nacional de Electrificación INDE", "Instituto Nacional de Electrificación.",
  "Instituto Nacional de Eletrificación INDE.", "Instituto Nacional de electrificación",
  "Instituto de Electrificación Nacional", "Instituto de electrificación INDE",
  "instituto nacional de electrificación INDE"
)

### MINEDUC
valores_mineduc <- c(
  "Mineduc"
)

### MINDEF
valores_mindef <- c(
  "Ministerio de la Defensa"
)

### no especifica
valores_noespecifica_contratante <- c(
  "No recuerdo", "Por medio de actas", "Renovación Anual",
  "Soy representante legal"
)

### Recode case match ----

df_dyd <- df_dyd |>
  mutate(
    otro_contratante = case_match(
      otro_contratante,
      all_of(valores_noespecifica_contratante) ~ "Sin especificar",
      all_of(valores_mindef) ~ "Ministerio de la Defensa",
      all_of(valores_mineduc) ~ "Ministerio de Educación",
      all_of(valores_inde) ~ "Instituto Nacional de Electrificación",
      all_of(valores_edured) ~ "EduRed",
      all_of(valores_director) ~ "Director o Directora del establecimiento",
      all_of(valores_directiva_plantel) ~ "Junta Directiva de la Cooperativa del Plantel",
      all_of(valores_junta_padres) ~ "Junta Directiva de los padres de familia",
      all_of(valores_junta_establecimiento) ~ "Junta Directiva del establecimiento",
      all_of(valores_junta_directiva) ~ "Junta Directiva",
      all_of(valores_adisa) ~ "Asociación para el Desarrollo Integral de San Antonio Ilotenango",
      all_of(valores_admin_establecimiento) ~ "Administración del establecimiento",
      .default = otro_contratante
    )
  )

### volver a ver errores

valores_otro_contratante <- df_dyd |>
  filter(contratante == "Otro") |>
  filter(!(is.na(otro_contratante))) |>
  select(otro_contratante) 
# View(valores_otro_contratante)


# otro_situacion_laboral ----

## ver errores
valores_otro_situacion_laboral <- df_dyd |>
  filter(situacion_laboral == "Otro") |>
  filter(!(is.na(otro_situacion_laboral))) |>
  select(otro_situacion_laboral)

# View(valores_otro_situacion_laboral)

## Define los vectores ----

### padres de familia
valores_padres_familia <- c(
  "Padres de familia", "comité padres de familia"
)

### contratacion interna
valores_interna <- c(
  "Contratación interna."
)

## Recode case match ----

df_dyd <- df_dyd |>
  mutate(
    otro_situacion_laboral = case_match(
      otro_situacion_laboral,
      all_of(valores_interna) ~ "Contratación interna",
      all_of(valores_padres_familia) ~ "Comité de padres de familia",
      .default = otro_situacion_laboral
    )
  )

## Volver a ver errores
valores_otro_situacion_laboral <- df_dyd |>
  filter(situacion_laboral == "Otro") |>
  filter(!(is.na(otro_situacion_laboral))) |>
  select(otro_situacion_laboral) 



# otro_situacion_laboral_no_oficial ----

## ver errores
valores_otro_situacion_laboral_no_oficial <- df_dyd |>
  filter(otro_situacion_laboral == "Otro") |>
  filter(!(is.na(otro_situacion_laboral_no_oficial))) |>
  select(otro_situacion_laboral_no_oficial) 

# View((valores_otro_situacion_laboral_no_oficial))


# otro_cnb_frecuencia_consulta ----
## ver errores

valores_otro_cnb_frecuencia_consulta <- df_dyd |>
  filter(cnb_frecuencia_consulta == "Otro") |>
  filter(!(is.na(otro_cnb_frecuencia_consulta))) |>
  select(otro_cnb_frecuencia_consulta) 

# View(valores_otro_cnb_frecuencia_consulta)

## Define los vectores ----

### al realizar la planificacion anual
valores_plani_anual <- c(
  "1 año", "1 ves al año", "ANUAL", "Al REALIZAR PLANIFICACION ANUAL",
  "Al realizar la planificacion anual",
  "Al realizar planificacion anual", "Al realizar planificacion anual.",
  "Anual", "Anualmente", "Año y medio",
  "Cada año", "Cada inicio de año", "Cuando se hace la planificación. Este no ha sufrido cambios últimamente.",
  "Cuando se elabora la planificación de curso", "Cuando se elabora el plan anual de curso",
  "En planificacion anual", "Planificador de Telesecundaria",
  "SOLO A INICIO DE AÑO PARA ELABORAR LA PLANIFICACION ANUAL", "al realizar planificacion anual.",
  "anual", "anualmente", "cada año", "cada año al planificar", "cada inicio de ciclo"
)

### cada bimestre
valores_bimestre <- c(
  "2", "2 veces al año", "2 veces pero lamento no contar con libro de Ingles y expresión artística",
  "Bimensualmente y cada vez que replanteo contenido y actividades",
  "Bimestral", "Cada bimestre", "Dos veces al año", "bimestral",
  "Bimensualmente  y cada vez que replanteo contenido y actividades"
)

### cada trimestre
valores_trimestre <- c(
  "3", "3 meses", "3 o 4 meses", "Cada 3 meses", "Cada tres meses",
  "Cada Trimestre","Cada trimestre", "Para la elaboración de la planificación trimestral, también para resolver alguna duda.",
  "Semestral", "Semestralmente", "TRIMESTRAL", "TRIMESTRE",
  "Trimestral", "Trimestrales", "Trimestralmente",
  "Trimestramente", "Trimestre", "Trinestral", "Cada tres  meses"
)

### cada cuatrimestre
valores_cuatrimestre <- c(
  "4", "CUATRIMESTRAL"
)

### a la semana
valores_semana <- c(
  "Cada semana", "Quincenal", "Cuando las actividades en mi programación no concuerdan o por falta de clases nos atrasamos en los temas programados esto es una vez cada semestre"
)

### al revisar contenido
valores_contenido <- c(
  "Revisión de contenido de unidad", "Cuando me surgen dudas o al momento de realizar las planificaciones.",
  "A la hora de revisar los planes y verificar contenido",
  "A cada unidad"
)

### cada seis meses
valores_seis_mes <- c(
  "Cada seis meses"
)

### cuando lo requiero
valores_requiero <- c(
  "Cuando es necesario", "Cuando lo necesite",
  "Cuando lo necesito", "Cuando lo requiero", "Cuando se necesita",
  "Cuando sea necesario", "Cuando surgen dudas sobre temas planificados",
  "Cuando tengo dudas", "Cuándo es necesario", "CUANDO SE CONSIDERA NECESARIO.",
  "Al inicio del año y cada vez que puedo"
)

### con regularidad
valores_regularidad <- c(
  "A veces", "Con regularidad", "De vez en cuando",
  "Frecuentemente"
)

### poco frecuente
valores_poco_frecuente <- c(
  "No frecuente", "Poco frecuente", "Rara ves"
)

### sin especificar
valores_sinespecifcar_cnb <- c(
  "Pag. Web", "Santillana o internet"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    otro_cnb_frecuencia_consulta = case_match(
      otro_cnb_frecuencia_consulta,
      all_of(valores_sinespecifcar_cnb) ~ "Sin especificar",
      all_of(valores_poco_frecuente) ~ "Poco frecuente",
      all_of(valores_regularidad) ~ "Con reguralidad",
      all_of(valores_requiero) ~ "Cuando lo requiere",
      all_of(valores_seis_mes) ~ "Cada seis meses",
      all_of(valores_contenido) ~ "Cuando revisa el contenido de unidad",
      all_of(valores_semana) ~ "Cada semana",
      all_of(valores_cuatrimestre) ~ "Cada cuatrimestre",
      all_of(valores_trimestre) ~ "Cada trimestre",
      all_of(valores_bimestre) ~ "Cada bimestre",
      all_of(valores_plani_anual) ~ "Al realizar la planificación anual",
      .default = otro_cnb_frecuencia_consulta
    )
  )

## Volver a ver errores

valores_otro_cnb_frecuencia_consulta <- df_dyd |>
  filter(cnb_frecuencia_consulta == "Otro") |>
  filter(!(is.na(otro_cnb_frecuencia_consulta))) |>
  select(otro_cnb_frecuencia_consulta) 

# otro_cantidad_centros_educativos ----

## ver errores

valores_otro_cantidad_centros_educativos <- df_dyd |>
  filter(cantidad_centros_educativos == "Otro") |>
  filter(!(is.na(otro_cantidad_centros_educativos))) |>
  select(otro_cantidad_centros_educativos) |>
  tabyl(otro_cantidad_centros_educativos)

# View(valores_otro_cantidad_centros_educativos)

## Define los vectores ----

### uno
valores_un_centro <- c(
  "1", "Solo en uno"
)

### dos
valores_dos_centro <- c(
  "2", "Trabajo como personal administrativo y docente auxiliar"
)

### ninguno
valores_ninguno <- c(
  "Ninguno", "ninguno"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    otro_cantidad_centros_educativos = case_match(
      otro_cantidad_centros_educativos,
      all_of(valores_ninguno) ~ "Ninguno",
      all_of(valores_dos_centro) ~ "Dos centros",
      all_of(valores_un_centro) ~ "Un centro",
      .default = otro_cantidad_centros_educativos
    )
  )

## Volver a ver errores

valores_otro_cantidad_centros_educativos <- df_dyd |>
  filter(cantidad_centros_educativos == "Otro") |>
  filter(!(is.na(otro_cantidad_centros_educativos))) |>
  select(otro_cantidad_centros_educativos) |>
  tabyl(otro_cantidad_centros_educativos)

# View(valores_otro_cantidad_centros_educativos)

# cn_libros_primero ----

## ver errores

valores_cn_libros_primero <- df_dyd |>
  filter(cn_uso_libros == "Sí") |>
  filter(cn_grados == "Primer grado") |>
  filter(!(is.na(cn_libros_primero))) |>
  select(cn_libros_primero) |>
  tabyl(cn_libros_primero)

# View(valores_cn_libros_primero)

## Define los vectores ----

### Ciencias Naturales 1
valores_naturales_1 <- c(
  "Ciencias Naturales", "Ciencias Naturales 1",
  "Ciencias Naturales 1 INDEGUA. S.A",
  "Ciencias Naturales 1 Santillana",
  "Ciencias naturales 1ro basico",
  "Ciencias Naturales 1 Santillana. Ciencias Naturales EDESSA 1",
  "Ciencias Naturales 1, Editorial Norma",
  "Guía ciencias naturales 1",
  "Guía ciencias naturales  1",
  "Libro de Santillana 1ro de ciencias naturales",
  "Ciencias naturales 1", "Ciencias naturales 1 Editorial Santillana",
  "Ciencias naturales Suseta ciencias naturales 1", "Cuencias Naturales"
)

### ciencias naturales 7
valores_naturales_7 <- c(
  "Ciencias Naturales 7", "Ciencias Naturales 7 Susaeta",
  "Ciencias naturales 7. Edit. Santillana",
  "Ciencias Naturales 7 de Santillana, Biología de Audersik, y muchos mas"
)

### biologia de audersik
valores_audersik <- c(
  "Ciencias Naturales 7 de Santillana, Biología de Audersik, y muchos mas"
)

### ciencias naturales mineduc
valores_naturales_mineduc <- c(
  "Ciencias Naturales MINEDU, Ciencias Naturales Grupo Quirigua",
  "Ciencias Naturales MINEDUC", "Ciencias narurales mineduc",
  "Ciencias naturales, Ministerio de Educación", "LIBRO DEL MINIEDUC Y SANTILLANA"
)

### ciencias naturales educativa
valores_naturales_educativa <- c(
  "Ciencias Naturales de Primero Básico de Editorial Educativa",
  "Ciencias Naturales editora educativa  y la web",
  "Ciencias Naturales editora educativa y la web", "Editora Educativa"
)


### ciencias naturales
valores_ciencias_naturales <- c(
  "Ciencias Naturales para primero basico",
  "Ciencias Naturales!! Santillana Primero y segundo básico libros de biología.",
  "Ciencias naturales Santillan", "Ciencias naturales de santillana y la guía metodológica del CNB",
  "Editorial Santillana", "El libro de santillana de Básico",
  "Libro de Santillana de nivel Básico", "Santillana",
  "Ciencias naturales de santillana y la guía metodológica del  CNB",
  "Ciencias Naturales!! Santillana Primero y segundo básico  libros de biología.",
  "Guía Metodológica de Ciencias Naturales, Guardianes Ecológicos de Guatemala, Amenazas y Desastres Naturales Unicef. Ciencias Naturales primero básico editorial Santillana.",
  "Ciencias Naturales, descubrir los secretos del entorno, Santillana",
  "Ciencias Naturales, editorial Santillana. Guardianes Ecológicos Guatemala. Fenómenos y Desastres Naturales y Amenazas Naturales Unicef. Guía Metodología de Ciencias Naturales"
)

### ciencias naturales guias de aprendizaje
valores_guia_aprendizaje <- c(
 "Ciencias Naturales, Guía de aprendizaje",  
 "Cinecias Naturales Guia de aprendeizje de Telesecundaria",
 "GUIA DE APRENDIZAJE", "GUIA DE APRENDIZAJE. LIBROS EDITORIAL SANTILLANA, ENCICLOPEDIAS.",
 "GUÍA DE APRENDIZAJE Y PLANIFICADOR", "Guia de Aprendizaje",
 "Guia de aprendizaje telesecundaria,", "Guia y planificador",
 "Guía de CN primero básico TELESECUNDARIA,", "Guía de aprendizaje",
 "Guía de aprendizaje primero basico, Ciencias Naturales - Grupo Quirigua, Ciencias Naturales Editorial ASEC",
 "Guía de aprendizaje y planificador", "Guía de ciencias Naturales y conceptos basicos",
 "Guías de aprendizaje y olanificador", "Guías de aprendizaje y planificador docente",
 "Las guías de ciencias Naturales", "Libro guía de aprender de primer grado"
)

### ciencias naturales y experimentos
valores_naturales_experimentos <- c(
  "Ciencias naturales y experimentos naturales",
  "Ciencias naturales, experimentos de las ciencias",
  "Ciencias naturales, experimentos naturales"
)

### integrado 7
valores_integrado_naturales <- c(
  "Integrado 7 Ciencias Naturales"
)

### ciencias naturales planificador del facilitador
valores_naturales_planificador <- c(
  "Planificador del facilitador", "Planificador del facilitador de ciencias naturales",
 "Planificador del facilitador y Guia de Aprendizaje Ciencias Naturales 1" 
)


### proyecto saber 7
valores_proyecto_saber <- c(
  "Proyecto Saber7 Ciencias Naturales, Santillana"
)

### quimica analitica
valores_quimica_analitica <- c(
  "Química analítica"
)

### experiencias del Mundo natural
valores_mundo_natural <- c(
  "Santillana mundo natural"
)


### sin especificar nombre
valores_noespecifica_naturales <- c(
  "Conceptos básicos", "Enciclopedicos", "Investigacion digital",
  "Investigacion  digital",
  "Libro de primero basico", "Libro de primero básico Santillana.",
  "Libro de texto de Ciencias Naturales", "Norma, Santillana, Editora Educativa",
  "Santillana y norma", "Santillana, Ciencias Naturales Norma y conceptos básicos de Telesecundaria",
  "Santillana, Mario Samuel Fernández", "Santillana. Editora educativa"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cn_libros_primero = case_match(
      cn_libros_primero,
      all_of(valores_noespecifica_naturales) ~ "No especifica el nombre",
      all_of(valores_mundo_natural) ~ "Experiencias del Mundo Natural",
      all_of(valores_quimica_analitica) ~ "Química Analítica",
      all_of(valores_proyecto_saber) ~ "Proyecto Saber 7",
      all_of(valores_naturales_planificador) ~ "Ciencias Naturales: Planificador del facilitador",
      all_of(valores_integrado_naturales) ~ "Integrado 7",
      all_of(valores_naturales_experimentos) ~ "Ciencias Naturales y Experimentos",
      all_of(valores_guia_aprendizaje) ~ "Ciencias Naturales: Guía de aprendizaje",
      all_of(valores_ciencias_naturales) ~ "Ciencias Naturales",
      all_of(valores_naturales_educativa) ~ "Ciencias Naturales: Editoria Educativa",
      all_of(valores_naturales_mineduc) ~ "Ciencias Naturales de MINEDUC",
      all_of(valores_audersik) ~ "Biología de Audersik",
      all_of(valores_naturales_7) ~ "Ciencias Naturales 7",
      all_of(valores_naturales_1) ~ "Ciencias Naturales 1",
      .default = cn_libros_primero
    )
  )

## volver a ver errores

valores_cn_libros_primero <- df_dyd |>
  filter(cn_uso_libros == "Sí") |>
  filter(cn_grados == "Primer grado") |>
  filter(!(is.na(cn_libros_primero))) |>
  select(cn_libros_primero) |>
  tabyl(cn_libros_primero)

# View(valores_cn_libros_primero)




# cn_libros_segundo ----

valores_cn_libros_segundo <- df_dyd |>
  filter(cn_uso_libros == "Sí") |>
  filter(cn_grados == "Segundo grado") |>
  filter(!(is.na(cn_libros_segundo))) |>
  select(cn_libros_segundo) |>
  tabyl(cn_libros_segundo)

# View(valores_cn_libros_segundo)

##  Define los vectores ----

#### ciencias naturales 9
valores_naturales_9 <- c(
  "Caleidoscopio Ciencias Naturales 9, Santillana. Ciencias Naturales 1 Guía de aprendizaje, Educación Básica, Ministerio de Educación."
)

### ciencias naturales guía de aprendizaje y planificador
valores_naturales_telesecundaria <- c(
  "Ciencias Naturales de Telesecundaria guía y planificador",
  "Ciencias Naturales Guía de aprendizaje",
  "Guia de aprendizaje de ciencias naturales",
  "Guias de aprendizaje.", "Guía de Ciencias Naturales y Planificador",
  "Guía de aprendizaje 2", "Guía de aprendizaje segundo básico 2019",
  "Guía de aprendizaje y Editora Educativa", 
  "Ciencias Naturales  de Telesecundaria guía y planificador",
  "Guía de aprendizaje y Planificador docente",
  "Iger, planificados y Guías de  Telesecundaria",
  "Guía de aprendizaje, Enciclopedia de la Educación, Editora Educativa. Otros",
  "Iger, planificados y Guías de Telesecundaria",
  "Planificados del facilitados y guía del aprendizaje  de Telesecundaria",
  "Planificados del facilitados y guía del aprendizaje de Telesecundaria"
)

### biologia de audersick
valores_biologia <- c(
  "Biología Audersick, libros de ciencias de editorial Norma, Santillana, páginas de Internet"
)

### ciencias naturales 2
valores_naturales_2 <- c(
  "Ciencias Naturales 2 y Modulo de Aprendizaje MINEDUC",
  "Ciencias naturales 2", "Guía - Ciencias Naturales 2",
  "Guía de aprendizaje Ciencias Naturales 2 del Ministerio de Educación y Libro Santillana de Ciencias Naturales 2"
)

### ciencias naturales 
valores_ciencias_naturales2 <- c(
  "Ciencias Naturales Santillana", "Ciencias Naturales, libro de texto para segundo básico de Telesecundaria. CNB de Ciencias Naturales. Ciencias Naturales ciclo Básico",
  "Ciencias Naturales. Editorial Santillana", "Santillana Ciencias Naturales"
)

### ciencas naturales 8
valores_naturales_8 <- c(
  "Ciencias Naturales 8", "Ciencias naturales 8 Susaeta"
)

### ciencia integrada
valores_ciencia_integrada <- c(
  "Ciencia integrada"
)

### biologia de claude a villee
valores_biologia_claude <- c(
  "Editorial santillana,  Biología de Claud Ville"
  )

### libro mineduc
valores_naturales_mineduc2 <- c(
  "Libro MINEDUC", "Ciencias Naturales II del gobierno de  Guatemala"
)

### no especifica nombre
valores_noespecifica_naturales2 <- c(
  "Editorial santilla y susaeta", "Google",
  "Internet", "Libro de ciencias", "Santillana",
  "Santillana , susaeta", "Santillana Secundaria",
  "Santillana secundaria", "Santillana,editora educativa y guía del docente.",
  "Textos de Telesecundaria",
  "Editorial  santilla  y susaeta"
)

### diversidad natural
valores_diversidad_natural <- c(
  "Diversidad natural"
)

### el origen de las especies
valores_origen <- c(
  "El origen de las especies"
)

### Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cn_libros_segundo = case_match(
      cn_libros_segundo,
      all_of(valores_diversidad_natural) ~ "Diversidad Natural",
      all_of(valores_noespecifica_naturales2) ~ "No especifica nombre",
      all_of(valores_naturales_mineduc2) ~ "Ciencias Naturales de MINEDUC",
      all_of(valores_biologia_claude) ~ "Biología de Claude A. Villee",
      all_of(valores_ciencia_integrada) ~ "Ciencia Integrada",
      all_of(valores_naturales_8) ~ "Ciencias Naturales 8",
      all_of(valores_ciencias_naturales2) ~ "Ciencias Naturales",
      all_of(valores_naturales_2) ~ "Ciencias Naturales 2",
      all_of(valores_biologia) ~ "Biología de Audersick",
      all_of(valores_naturales_telesecundaria) ~ "Ciencias Naturales: Guía de aprendizaje y Planificador de facilitador",
      all_of(valores_naturales_9) ~ "Ciencias Naturales 9",
      all_of(valores_origen) ~ "El Origen de las Especies",
      .default = cn_libros_segundo
    )
  )

### volver a ver errores

valores_cn_libros_segundo <- df_dyd |>
  filter(cn_uso_libros == "Sí") |>
  filter(cn_grados == "Segundo grado") |>
  filter(!(is.na(cn_libros_segundo))) |>
  select(cn_libros_segundo) |>
  tabyl(cn_libros_segundo)




# cn_Libros_tercero ----

valores_cn_libros_tercero <- df_dyd |>
  filter(cn_uso_libros == "Sí") |>
  filter(cn_grados == "Tercer grado") |>
  filter(!(is.na(cn_libros_tercero))) |>
  select(cn_libros_tercero) |>
  tabyl(cn_libros_tercero)

# View(valores_cn_libros_tercero)

## Define los vectores ----

### fundamentos de la física
valores_fund_fisica <- c(
  "1. Fundamendos de la física (Serway y Vuille). 2. Ciencias Naturales 9 Fisica (Santillana). 3. Ciencias Naturales (Iger))",
  "Fundamentos básicos de la física, Química básica", "Fisiva universitaria, fundamentos de fisica, física fundamental galileo etc."
)

### fisica general
valores_fisica_general <- c(
  "Física Fundamental Mario S. Fernández",
  "1. Fundamendos de la física (Serway y Vuille).  2. Ciencias Naturales 9 Fisica (Santillana).  3. Ciencias Naturales (Iger))",
  "Física General básico", "Física General, Ciencias Naturales Santillana",
  "Física General, Mario Samuel Fernández y guía Ciencias Naturales 3",
  "Física fundamental, mario samuel", "Ciencias santillaba, Física Samuel F otros",
  "Ciencias Naturales III,Samuel Fernández",
  "FÍSICA GENERAL, MARIO SAMUEL FERNÁNDEZ R.",
  "Ediciones LS, Física General",
  "Libro de Física Fundamental de Mario Samuel Hernández"
)

### fisica conceptos y aplicaciones
valores_fisica_tippens <- c(
  "Física de Tipens", "Física de Tipoens, Física para básicos de Mario Fernández y Física",
  "Santillana, Física de Tippens.", "Ciencias Naturales Santillana Edición 2004 y Tipens",
  "Tippens, Sánchez fernandez", "Física de Tipoens,   Física para básicos de Mario Fernández y Física"
)

### guias de aprendizaje ciencias naturales
valores_guias_naturales <- c(
  "Guía de Aprendizaje de Ciencias naturales de Telesecundaria, Ciencias Naturales III Guia para docentes, Ciencias Naturales III, Santillana",
  "Guía de aprendizaje de tercero Básico Ciencias Naturales",
  "Guía de aprendizaje ciencias naturales",
  "Guía de aprendizaje  de tercero Básico  Ciencias  Naturales",
  "Guía de aprendizaje y ciencias Naturales 3",
  "Guía de telesecundaria, concepto del libro anterior",
  "Guía para el curso de Ciencias Naturales",
  "Guías de telesecundaria y Santillana"
)

### planificador del facilitador ciencias
valores_planificador_naturales <- c(
  "Planificador Docente y guías de Aprendizaje",
  "Planificador del facilitador, guia de aprendizaje",
  "Planificador, Guias de Aprendizaje del estudiante, Guias de Aprendizaje y Conceptos Básicos Vol I, II, III, IV"
)

### ciencias naturales 3
valores_naturales_3 <- c(
  "Ciencias Naturales 3", "Guía de aprendizaje y ciencias Naturales 3",
  "Ciencias Naturales 3, guía de aprendizaje",
  "Guía Ciencias naturales 3", "Guía de Aprendizaje de Ciencias naturales de Telesecundaria, Ciencias Naturales III Guia para docentes, Ciencias Naturales III, Santillana"
)

### FISICA 1
valores_fisica_1 <- c(
  "Física 1, Física fundamental básica"
)

### ciencias naturales y tecnologia
valores_naturales_tecno <- c(
  "Ciencias Naturales y Tecnología, Física Fundamental",
  "Libro de ciencia naturales y tecnologia"
)

### ciencia para vivir en comunidad
valores_ciencia_comuni <- c(
  "Ciencia para Vivir en Comunidad, Ciencias Naturales 3 Aprender Para Progresar"
)

### ciencias naturales
valores_naturales3 <- c(
  "Ciencias Naturales", "Ciencias Naturales Santillana Edición 2004 y Tipens",
  "Ciencias Naturales, libros de textos", "Ciencias Santillana"
)

### ciencias naturales mineduc
valores_naturales_mineduc3 <- c(
  "Ciencias naturales 3ro básico mineduc", "El del ministerio de educación",
  "El libro del Ministerio de educación", "Texto del ministerio de educación",
  "Susaeta y los libros proporcionados por el Ministerio de educación"
)

### ciencias naturales 9
valores_naturales9 <- c(
  "Ciencias naturales 9, fundamentos de la fisica volumen 1",
  "1. Fundamendos de la física (Serway y Vuille).  2. Ciencias Naturales 9 Fisica (Santillana).  3. Ciencias Naturales (Iger))"
)

### fisica
valores_fisica <- c(
  "Fisica", "Física", "fisica para basicos"
)

### naturaleza y vida
valores_natu_vida <- c(
  "Vida y naturaleza"
)

### integrado 9
valores_integrado_9 <- c(
  "susaeta integrado 9"
)

### experiencias del mundo natural para curiosos
valores_experiencias <- c(
  "Curiosos santillana"
)

### sin especificar
valores_noespecifica_naturales3 <- c(
  "Editora Educativa, Mario Samuel Fernández, Google Académico, otros",
  "1", "Audesirk, Chang, Física Fundamental básica.",
  "Santillana  e IGER", "Web, química,  biologia, física",
  "Libros digitales", "M", "Páginas web", "Santillana", "Prodesa", "Comces",
  "Santillana e IGER", "Santillana 2025", "Texto para el estudiante", "ALGEBRA DE BALDOR, CIENCIAS SOCILEA 3 EU, FOLLETOS IMPRESOS",
  "Apuntes de Padep", "Web, química, biologia, física", "Biología de Billy, libro de la editorial Zatmaro y Santillana,"
)

## Recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    cn_libros_tercero = case_match(
      cn_libros_tercero,
      all_of(valores_noespecifica_naturales3) ~ "No especifica nombre",
      all_of(valores_experiencias) ~ "Experiencias del Mundo Natural para curiosos",
      all_of(valores_integrado_9) ~ "Integrado 9",
      all_of(valores_natu_vida) ~ "Naturaleza y Vida",
      all_of(valores_fisica) ~ "Física",
      all_of(valores_naturales9) ~ "Ciencias Naturales 9",
      all_of(valores_naturales_mineduc3) ~ "Ciencias Naturales de MINEDUC",
      all_of(valores_naturales3) ~ "Ciencias Naturales",
      all_of(valores_ciencia_comuni) ~ "Ciencia para Vivir en Comunidad",
      all_of(valores_naturales_tecno) ~ "Ciencias Naturales y Tecnología",
      all_of(valores_fisica_1) ~ "Física 1",
      all_of(valores_naturales_3) ~ "Ciencias Naturales 3",
      all_of(valores_planificador_naturales) ~ "Ciencias Naturales: Planificador para el facilitador",
      all_of(valores_guias_naturales) ~ "Ciencias Naturales: guías para aprendizaje",
      all_of(valores_fisica_tippens) ~ "Física: conceptos y aplicaciones",
      all_of(valores_fisica_general) ~ "Física General",
      all_of(valores_fund_fisica) ~ "Fundamentos de la Física",
      .default = cn_libros_tercero
    )
  )

## volver a ver errores

valores_cn_libros_tercero <- df_dyd |>
  filter(cn_uso_libros == "Sí") |>
  filter(cn_grados == "Tercer grado") |>
  filter(!(is.na(cn_libros_tercero))) |>
  select(cn_libros_tercero) |>
  tabyl(cn_libros_tercero)



# cn_recursos_centro_educativo ----

## ver errores
valores_cn_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(cn_recursos_centro_educativo))) |>
  select(cn_recursos_centro_educativo) |>
  tabyl(cn_recursos_centro_educativo)

# View(valores_cn_recursos_centro_educativo)
dput(valores_cn_recursos_centro_educativo)

## Define los vectores ----

### Libros
valores_libros <- c(
  "A veces un libro", "Algunos libros", "Algunos textos",
  "El libro de guía para los alumnos",
  "Solo tenemos 5 libros, sacamos copias para trabajar", "Solo textos, guía y planificador", 
  "Santillana", "Solo el libro de ciencias naturales",
  "libro", "Únicamente libros",
  "libros de texto", "libros internet",
  "Libro", "Libro  ciencias del MIneduc", "Libro Mineduc Segundo Básico", 
  "Libro de texto", "Libro de texto Libro integrado 7 y 8", "Libro de texto de CCNN del Mineduc.", 
  "Libro de texto, hojas, valija didáctica", "Libro de texto, material didáctico", 
  "Libro de textos", "Libro digital", "Libro guía de Ciencias Naturales", 
  "Libro para el maestro del ministerio de educación", "Libro planificador  y guías", 
  "Libro y tv", "Libro, computadora, marcadores, pizarra", "Libro, copias y materiales didácticos", 
  "Libro, cuadernos", "Libros", "Libros de Texto", "Libros de biblioteca", 
  "Libros de telesecundaria y material audiovisual.", "Libros de texto", 
  "Libros de texto de guías y planificador", "Libros de texto, impresora, hojas, marcadores", 
  "Libros de texto, material repetitivo (marcadores, almohadillas, hojas, lapiceros, lápiz.", 
  "Libros de texto, pantallas, proyectores, experimentos.", "Libros de texto, plataforma digital, Dispositivo electrónico (Tablet)", 
  "Libros digitales", "Libros e Internet", "Libros e internet", 
  "Libros editora Educativa", "Libros proporcionados por el ministerio de educación", 
  "Libros y entorno", "Libros y material didáctico.", "Libros y material didácticos", 
  "Libros y materiales", "Libros y páginas Web", "Libros, Audiovisuales", 
  "Libros, Cañonera, Televisores, marcadores y pizarrones", "Libros, Cnb", 
  "Libros, Internet, copias", "Libros, cañoneras, pizarras y marcadores", 
  "Libros, cuadernos, láminas, enciclopedias.", "Libros, elaboración de materiales didácticas, Marcadores, revistas y otros", 
  "Libros, hojas de trabajo", "Libros, hojas, marcadores, papelografos.", 
  "Libros, internet, marcadores, etc", "Libros, marcadores, pizarra,", 
  "Libros, material didáctico", "LIBRO DE TEXTO DEL MINEDUC",
  "Libros, nateria prima, espacio, instrumentos", 
  "Libros, nateria prima, espacio, instrumentos",
  "Texto", "Texto para estudiantes", "Un libro para el docente", 
  "Textos", "Textos, Laboratorio Físico de Ciencias Naturales, Computadora, Cañonera", 
  "Libros, pizarrón...", "Mario Samuel Fernández, guía de Mineduc, Física de Serway", 
  "Libros, pantalla, pizarrón", "Libros, papelería, internet, computadora, pizarra, marcadores, cañonera", 
  "Libros, pizarrón...", "Libros, revistas", "Libros, textos, impresora, hojas, marcadores, pizarra, almohadilla", 
  "Libros, área", "Libros.", "Libros. Y Materiales de laboratorio",
  "LIBROS DE SUSAETA", "LIBROS DE TEXTO", "LIBROS DE TEXTO, COMPUTADORA, PROYECTOR, PIZARRA", 
  "Los libros", "Los libros de Telesecundaria y material audio visual", 
  "Los textos", "Ciencias naturales 3", "Ciencias naturales, Santillana"
)

### Equipo tecnológico
valores_equipo_tecnologia <- c(
  "Cañonera", "Cañonera internet y libros computadora pizarrón marcadores", 
  "Cañonera y computadora", "Cañonera, impresiones, hojas, marcadores", 
  "Cañonera, televisión, área verde", "Centro de computación", 
  "Computadora", "Didácticos , tecnológicos", "Hojas, pizarrón, marcadores, tinta, internet, computadora, hojas.", 
  "Impresora, cañonera y pizarra", "Impresora, pantalla",
  "Internet", "Internet, Impresiones, libros, etc",
  "Libros de texto, pantallas, proyectores, experimentos.", "Libros de texto, plataforma digital, Dispositivo electrónico (Tablet)", 
  "Libros digitales", "Libros e Internet", "Libros e internet", 
  "Libros, Cañonera, Televisores, marcadores y pizarrones", "Libros, Internet, copias", "Libros, cañoneras, pizarras y marcadores",
  "Proyector", "Proyector y la valija didáctica", "Proyectores y áreas para realizar actividades prácticas", 
  "Pizarra inteligente, laboratorio, Internet, sonido,",
  "Pantallas, cable HDMI", "Pantallas, cañorera, television", 
  "Pantalla", "Pantalla, Impresora", "Pantalla, laboratorio de Ciencias Naturales,  libro de texto.", 
  "LIBROS DE TEXTO, COMPUTADORA, PROYECTOR, PIZARRA", 
  "Pizarra  Cañonera", "cañonera", "computadoras pizarron almohadilla hojas", 
  "Tecnología", "Tecnología, televisiones, espacio", 
  "Tecnológicos y físicos", "Tecnológicos: Tablet, cañoneras e internet. Materiales: Pizarrones, marcadores, etc.", 
  "Televisión, computadoras y libros de texto", "Televisión, tablet, plataforma digital, libros de texto", 
  "Televisores e internet", "Fotocopias, televisión y proyector a veces", 
  "pizarrón, marcadores, proyector, bocinas, papel de distintos colores y televisor..", 
  "Material Didáctico, Libros, proyectores, computadora",
  "Didácticos y tecnológica.", "Laptop, proyector e impresiones",
  "Internet, impresiones y copias", "Internet,marcadores,papel,hojas,étc.", 
  "Investigaciones e impresiones de los temas, entre otros", "LIBROS DE TEXTO, COMPUTADORA, PROYECTOR, PIZARRA",
  "Computadora e internet, libros, material de laboratorio, material didáctico.", 
  "Computadora y cañonera", "Computadora, Proyector.", "Computadora, cañonera, libros de texto", 
  "Computadora, internet, teléfono", "Computadora, proyector, libros de texto y laboratorio."
)

### materiales papelería
valores_papeleria <- c(
  "Carteles fichas de trabajo", 
  "Carteles hojas computadora materiales didáctico", "Cartulinas y otras del entorno", 
  "Cañonera, impresiones, hojas, marcadores", "libros, hojas y otros",
  "Pizarrón,marcadores,almohadilla lapiceros lápices,silicon, y otros",
  "Computadora e internet, libros, material de laboratorio, material didáctico.", 
  "Computadora, proyector, libros de texto y laboratorio.", 
  "Copias", "Copias.", "Pizarrón, marcadores, papel, reglas, etc",
  "No hay específicos, únicamente hojas, papelógrafos, ejemplar de un libro.", 
  "Fichas carteles marcadores", "Folletos", "Fomi, hojas , cartulinas, marcadores, televisión", 
  "Fotocopias y materiales didácticos", "Fotocopias, impresiones", 
  "Fotocopias, marcadores, hojas y tinta",
  "Libros, pantalla, pizarrón", "Libros, papelería, internet, computadora, pizarra, marcadores, cañonera",
  "Hoja, marcadores, sellador, área verde", "Hojas algunas veces y alguna que otra fotocopia", 
  "Hojas bond", "Hojas bond. Pizarro. Planificador. Libro Guía.", 
  "Hojas de trabajo", "Hojas de trabajo, libros", "Hojas iris, papel Bond, marcadores de colores", 
  "Hojas marcadores", "Hojas y marcadores rmadores", "Hojas, carteles, marcadores y proyector.", 
  "Hojas, impresiones, recurso tecnológico y recurso pedagógico", 
  "Hojas, impresiones.", "Hojas, pizarrón, marcadores, tinta, internet, computadora, hojas.", 
  "Impresos", "Libro, computadora, marcadores, pizarra",
  "Imágenes y catulina", "Internet,marcadores,papel,hojas,étc.", 
  "Investigaciones e impresiones de los temas, entre otros",
  "Libros de texto, impresora, hojas, marcadores", 
  "Mis hijas de trabajo impreso", "Biblioteca, Hemeroteca, Ludoteca", 
  "pizarrón, marcadores, proyector, bocinas, papel de distintos colores y televisor..", 
  "fotocopias, lapicero, libros de texto, marcadores", "hojas de papel bond, marcadores, lapiceros y lápices", 
  "Libros de texto, material repetitivo (marcadores, almohadillas, hojas, lapiceros, lápiz.", 
  "Libros, cuadernos, láminas, enciclopedias.", "Libros, elaboración de materiales didácticas, Marcadores, revistas y otros", 
  "Libros, hojas de trabajo", "Libros, hojas, marcadores, papelografos.", 
  "Libros, internet, marcadores, etc", "Libros, marcadores, pizarra,", 
  "Libros, textos, impresora, hojas, marcadores, pizarra, almohadilla", 
  "Lo disponible para la valija didáctica, hojas, marcadores, tinta,etc.", 
  "Láminas", "Marcadores", "Marcadores de Pizarra; almohadias,", 
  "Material impreso", "reproducción de hojas de trabajo o ejercicios",
  "Tecnológicos: Tablet, cañoneras e internet. Materiales: Pizarrones, marcadores, etc.", 
  "Marcadores y pizarrón", "Marcadores, almohadillas, cartulina, pizarra", 
  "Marcadores, cartulina, proyector, impresiones.", "Marcadores, cartulinas, goma, etc.", 
  "Marcadores, hojas, formatos, libros", "Marcadores, lapicero, copias, hojas, almohadilla", 
  "Marcadores, libros.", "Marcadores, papel, almohadilla, pizarrón, tinta"
)

### material audiovisual y didáctico
valores_audiovisual_didactico  <- c(
  "Audiovisual, material de apoyo", "Audiovisual, y libros", "Audiovisuales", 
  "Diapositiva", "Material Didáctico, Libros, proyectores, computadora", "Material concreto", 
  "Material de apoyo", "Material didáctico", "Material visual y auditivo", 
  "Materiales didácticos", "Materiales didácticos, tecnología",
  "Libro de texto, material didáctico", "La mochila didáctica",
  "Murales maquetas esqueleto humano órganos de los distintos sistemas",
  "Balija didactica",  "Recursos visuales, materiales de laboratorio (microscopio), libros",
  "Reciclable y Materiales de aprendizaje", "Recursos didácticos y digitales pero lamentablemente no hay acceso a Internet", 
  "Libro de texto, hojas, valija didáctica", "Libro, copias y materiales didácticos", 
  "Libros de telesecundaria y material audiovisual.", 
  "Libros, material didáctico", "Videos", "Únicamente la valija didáctica", 
  "Valija didáctica", "Valija didáctica, proyector", "Varios materiales",
  "Materiales didácticos/valija escolar", "Materiales ilustrativos  y audiovisuales", 
  "Materiales pedagogicos", "Recursos visuales, materiales de laboratorio (microscopio), libros"
)

### mobiliario
valores_mobiliario <- c(
  "Aula pura, pizarrón", "La pizarra", "Mesas, pizarra",
  "Pizarra y almohadia de Pizarra", 
  "Pizarra, marcadores, lapiceros y hojas de papel bond", "Pizarras", 
  "Pizarras, cátedras, CNB en digital", "Pizarras, material didáctico", 
  "Pizarrón", "Pizarrón,  marcador", "Pizarrón, marcadores, cajonera, audio, material adicional impreso"
)

### equipo de laboratorio
valores_laboratorio <- c(
  "Laboratorio con  probetas, vasos medidores, extintor, microscopios, ollas, pinzas, tubos de ensayo,   buretas, cuenta gotas, termòmetros, embudos, pipetas,  balanzas,", 
  "Laboratorio de ciencias", "Laboratorio de ciencias y libros", 
  "Laboratorio, audiovisuales", "Laboratorio, maecadores, pizarra otros",
  "Material y equipo de laboratorio", "Microscopio, macroscopio ,libros",
  "Recursos visuales, materiales de laboratorio (microscopio), libros", 
  "laboratorios, espacio y facilidades tecnologicas"
)

### guías
valores_guias <- c(
  "GUIA", "GUIAS DE APRENDIZAJE", "GUÍA DE APRENDIZAJE", 
  "GUÍA DE APRENDIZAJE Y PLANIFICADOR", "Guia de Aprendizaje", 
  "Guia de aprendIzaje, marcadores pi", "Guia de aprendizaje", 
  "Un folleto de trabajo", 
  "Guia de aprendizaje del gobierno", "Guia de ciencias naturales", 
  "Guia del docente", "Guia para docente y libro para estudiantes", 
  "Guias", "Guias de aprendizaje", "Guía", "Guía de aprendizaje", 
  "Guía de aprendizaje y Planificador de Ciencias Naturales", 
  "Guía de ciencias Naturales", "Guía de ciencias naturales educación básica", 
  "Guía de ciencias naturales y planificador", "Guía de telesecundaria", 
  "Guía del docente", "Guía impresa", "Guía y Planificador", 
  "Guías", "Guías  hojas de trabajo", "Guías de aprendizaje.", 
  "La guía del docente", "Solo la guía", 
  "Solo una guía", "Las guías de aprendizaje y planificadores"
)

### recursos digitales
valores_digitales  <-c(
  "CONTENIDOS DE LA WEB", 
  "Cajonera  aula virtual, internet", "Medio Digital"
)

### CNB
valores_recurso_cnb  <- c(
  "CNB", "CNB Y libro Ciencias Naturales 8"
)

### planificador
valores_planificador <- c(
  "Planificación", "Planificador", "Planificador de CN", "Planificador y guia", 
  "Planificador, CNB y Guías de aprendizaje", "Planificadores y Guías.",
  "un planificador y una guía"
)

### formacion docente
valores_formacion  <- c(
  "Participar en en talleres de actualización docente."
)

### No especifica
valores_sinespecificar_recurso <- c(
  "0","1", "Físicos,", "Recursos naturales", "Todos", "Área verde, azadón, machetes"
)

### ninguno
valores_recurso_ninguno  <- c(
  "NINGUNO", "Nada", "Nada...", "Ninguna", "Ninguno", "Ninguno,por medios y fondos propios se realizan las actividades", 
  "Ningún", "Ningún material", "No", "No hay", "No tiene", "No.", 
  "ninguno", "propios"
)

## Recode con case match ----

df_dyd  <- df_dyd |>
  mutate(
    cn_recursos_centro_educativo = case_match(
      cn_recursos_centro_educativo,
      all_of(valores_recurso_ninguno) ~ "Ninguno",
      all_of(valores_sinespecificar_recurso) ~ "No especifica el recurso",
      all_of(valores_formacion) ~ "Formación docente",
      all_of(valores_planificador) ~ "Planificador",
      all_of(valores_recurso_cnb) ~ "El CNB",
      all_of(valores_digitales) ~ "Recursos digitales",
      all_of(valores_guias) ~ "Guías",
      all_of(valores_laboratorio) ~ "Equipo de laboratorio",
      all_of(valores_mobiliario) ~ "Mobiliario",
      all_of(valores_audiovisual_didactico) ~ "Material audiovisual y didáctico",
      all_of(valores_papeleria) ~ "Material de papeleria",
      all_of(valores_equipo_tecnologia) ~ "Equipo tecnológico",
      all_of(valores_libros) ~ "Libros",
      .default = cn_recursos_centro_educativo
    )
  )

## volver a ver errores

valores_cn_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(cn_recursos_centro_educativo))) |>
  select(cn_recursos_centro_educativo) |>
  tabyl(cn_recursos_centro_educativo)

# View(valores_cn_recursos_centro_educativo)


# cn_recursos_propios ----

## ver errores

valores_cn_recursos_propios <- df_dyd |>
  filter(!(is.na(cn_recursos_propios))) |>
  select(cn_recursos_propios) |>
  tabyl(cn_recursos_propios)

# View(valores_cn_recursos_propios)

## Define los vectores ----

### audiovisual y didáctivo

valores_audiovisual_didactico_propio <- c(
  "Audiovisual, diversos", "Maquetas y otros", 
  "Maquetas, papel, colores etc.", "Marcadoras,  cartulinas,  juego geométrico", 
  "Material  de apoyó", "Material audio visual, libros de telesecundaria", 
  "Material concreto", "Material de laboratorio, material didáctico",
  "Material didáctico", "Material didáctico , recursos tecnológicos", 
  "Material didáctico e investigación", "Material didáctico impreso", 
  "Material didáctico variado", "Material didáctico, folletos, hojas de trabajo, cuadros de texto.", 
  "Material didáctico, hojas de trabajo, investigación", 
  "Material didáctico, libro, marcadores, el medioambiente", 
  "Material didáctico", "Material didáctico , recursos tecnológicos", 
  "Material didáctico e investigación", "Material didáctico impreso", 
  "Material didáctico variado",
  "Materiales impresos (libros, fotocopias), materiales reciclados (botellas, cartones) recursos audiovisuales (videos), juegos interactivos y análisis de datos.  Materiales del medio natural: Observación directa del entorno para realizar investigaciones y exploraciones. Experimentos y actividades prácticas",
  
  "Materiales impresos (libros, fotocopias), materiales reciclados (botellas, cartones) recursos audiovisuales (videos), juegos interactivos y análisis de datos.  Materiales del medio natural: Observación directa del entorno para realizar investigaciones y exploraciones. Experimentos y actividades prácticas",
  "Material didáctico, hojas de trabajo, investigación", 
  "hoja de trabajo , carteles,exposiciones,mapas conceptuales, cuadros comparativos", 
  "hojas de ejercicios o resumen de contenido", "impresiones, fotocopias, marcadores, almohadillas", 
  "impresora, hojas, libro, materiales varios.",
  "Textos, recursos didácticos, computadora", 
  "Libros, investigaciones, carteles didácticos, materiales reciclados", 
  "Libros, láminas, imágenes etc",
  "Material impreso y material reciclado", "Material impreso, material reciclado", 
  "Material mediado", "Materiales impresos (libros, fotocopias), materiales reciclados (botellas, cartones) recursos audiovisuales (videos), juegos interactivos y análisis de datos.  Materiales del medio natural: Observación directa del entorno para realizar investigaciones y exploraciones. Experimentos y actividades prácticas",
  "Material impreso", "Material impreso o digital sobre esta área", 
  "Distintos materiales utilizados en docencia superior en la Usac", 
  "Material didáctico. Investigación", "videos, documentos de apoyo",
  "Carles , experimentos y gráficas", "Carteles", "Carteles , folletos", 
  "Carteles impresos, maquetas", "Carteles imágenes descriptivas libros láminas videos significativos", 
  "Carteles, computador, hojas de trabajo", "Carteles, copias, hojas, crayones", 
  "Carteles, diapositivas, presentaciones, uso de herramientas tecnologicas", 
  "Carteles, fotocopias, investigaciones", "Carteles, imágenes, videos, libros, entre otros", 
  "Carteles, marcadores, audio visual", "Carteles, marcadores, internet", 
  "Carteles, material didáctico, internet", "Carteles, materiales impresos, termometros.", 
  "Cartulinas con imagenes y otros",
  "Reciclaje", "Recopilación  de material bibliografico de varios autores/ Laptop, Reproductor  de imágenes  equipo de Audio./ Herramientas  agricolas", 
  "Recursos didácticos",  "Recursos humanos, naturales hojas de tareas", "Recursos naturales", 
  "Recursos pedagógicos", "Recursos reutilizables",
  "Carteles, material didáctico, internet", "Carteles, materiales impresos, termometros.", 
  "Cartulinas con imagenes y otros",  "Figuras",
  "Diagrama, fotografía videos mapas conceptuales", 
  "Diversidad de material didáctico",  "Naturales y reciclado",
  "Carteles, imágenes, videos, libros, entre otros", 
  "Diapositivas, experimentos, material didáctico.", "Diapositivas, material didáctico", 
  "Materiales Didácticos, de ensayo, visuales", 
  "Esquelo en tamaño real, Material Didáctico, Sistemas del cuerpo humanos en tamaño real",
  "El entorno, material audio visual",
  "Videos educativos y folletos", "Videos, copias, recortes, revistas", 
  "Videos, diapositivas, folletos", "Videos, imágenes, maquetas, carteles, música", 
  "Videos, libros de texto, pantalla, pliegos de papel..", 
  "WEB", "audio visuales",  "Maquetas y otros",
  "Pizarrón, proyector, libro de texto, CNB, salón de clases, videos y presentaciones digitales", 
  "Libros, afiches", "Libros, carteles, guías,  consolidados de tareas, etc.", 
  "Libros, computadora y materiales didácticos", "Libros, copias y material visual", 
  "Libros, cuadernos, hojas de trabajo y actividades de campo", 
  "Libros, hojas de trabajo, láminas", "Libros, instrumentos de medición", 
  "Libros de texto, naturaleza misma, cuadernos, lapiceros , crayones, materiales didacticos", 
  "Materiales de apoyo,  videos.", "Materiales didácticos , juegos.", 
  "Materiales impresos (libros, fotocopias), materiales reciclados (botellas, cartones) recursos audiovisuales (videos), juegos interactivos y análisis de datos.  Materiales del medio natural: Observación directa del entorno para realizar investigaciones y exploraciones. Experimentos y actividades prácticas", 
  "Materiales impresos (libros, fotocopias), materiales reciclados (botellas, cartones) recursos audiovisuales (videos), juegos interactivos y análisis de datos.  Materiales del medio natural: Observación directa del entorno para realizar investigaciones y exploraciones. Experimentos y actividades prácticas", 
  "Materiales impresos o juegos", "Materiales para experimentos de uso diario.", 
  "Materila didáctico", "Medio Digital", "Minicomputadora, tablet, internet", 
  "Naturales y reciclado", "Libros y Material Didáctico.",
  "Presentaciones visuales.", 
  "Simuladores, maquetas y prototipos",     "Tablas periódicas para los estudiantes, libros", 
  "Propios", "Proyector", "Proyector , carteles , marcadores ,", 
  "Proyector, láminas"
)

### libros

valores_libros_propio <- c(
  "Libro Santillana,  Materiales didácticos", "Libro de texto", 
  "Libro",
  "Libro de texto de CCNN del mineduc.", "Libro de texto, copias  y carteles", 
  "Libro de texto, internet, recortes, cuadernos, hojas.", 
  "Libro de textos y folleto", "Libro extra,  información de internet", 
  "Libro o internet", "Libro, Transportador ,Regla, Pita, Hojas  de papel bond,Tijera, resistol", 
  "Libro, carteles, infografias, materiales de medición.",  "Internet, libros, folletos.",
  "Libro, computadora, otros", "Libro, folleto, cañonera  y imágenes", 
  "Libro, hojas, Internet marcadores", "Libro, presentaciones didácticas , instrumento de ensayo", 
  "Libros", "Libros Marcadores", "Libros afichaa", "Libros de Texto", 
  "Libros de ciencia", "Libros de ciencias", "Libros de ciencias naturales, fotocopias, guías de trabajo, proyecciones, audio", 
  "Libros de texto", "Libros de texto  de trabajo y recursos naturales", 
  "Libros de texto propios", "Libros de texto y planificadores", 
  "Libros de texto, computadora", "Libros de texto, naturaleza misma, cuadernos, lapiceros , crayones, materiales didacticos", 
  "Libros de textos", "Libros de textos propios e instrumentos de laboratorio propios", 
  "Libros e Internet", "Libros e investigaciones", "Libros e investigación", 
  "Libros en línea", "Libros extra, internet, celular, computadora", 
  "Libros textos internet", "Libros y Material Didáctico.", 
  "Otros libros",  "Documentos",
  "Manuales y libros utilizados en enseñanza superior",
  "Los planificadores que envía Mineduc y ciencias naturales santillana", 
  "Libros y computadora", "Libros y computadora, fotocopias", 
  "Libros y hojas", "Libros y juego geometrico", "Libros, Computadoras, fotocopias, juegos", 
  "Libros, Folletos, CNB", "Libros, Folletos, Computadora, Proyector, material didáctico, etc.", 
  "Libros, Internet, computadora, cañonera.", "Libros, Videos, Juegos de mesa, hojas, fotocopias, Carteles, calculadoras", 
  "Libros, afiches", "Libros, carteles, guías,  consolidados de tareas, etc.", 
  "Libros, computadora y materiales didácticos", "Libros, copias y material visual", 
  "Libros, cuadernos, hojas de trabajo y actividades de campo", 
  "Libros, folletos", "Libros, herramientas, materiales", "Libros, hojas de trabajo y materiales educativos", 
  "Libros, hojas de trabajo, láminas", "Libros, instrumentos de medición", 
  "Libros, internet, fotocopias, computadora, cuadernos, grapadora, etc.", 
  "Libros, investigaciones, carteles didácticos, materiales reciclados", 
  "Libros, láminas, imágenes etc", "Libros, paginas web, documentos pdf", 
  "Libros, papel bond pliegos, tabla periódica, computadora etc.", 
  "Libros, recursos tecnológicos", "Libros, reglas, calculadoras, tablas", 
  "Libros, revistas y material didácticos propios", "Libros, sitios web, hojas de trabajo", 
  "Libros, videos folletos, calculadora, metros, reglas", "Libros, y lo necesario para experimentar", 
  "Libros,marcadores,hojas,pliegos de papel bond", "Libros,teléfono", 
  "Libros,teléfono,crayones,tijeras,lapiceros,libretas,cuadernos", 
  "Lo del con texto,impreso y palpables", "LIBRO DE TEXTO", "LIBRO DE TEXTO DEL MINEDUC",
  "Ciencias Naturales 2  y Modulo de Aprendizaje 2 MINEDUC", 
  "Ciencias Naturales 3", "Únicamente libros",
  "Textos y cnb",
  "CNB y Libro Ciencias Naturales 8", 
  "libro", "libros", "libros, folletos", "libros, material de laboratorio, computador, internet,"
)

### equipo de laboratorio
valores_laboratorio_propio <- c(
  "Laboratorio", "Laboratorio equipo y material", "Laboratorios, espacio y recursposo tecnologicos",
  "Algunos reactivos, bata propia",  "Flexómetro, Reglas, Simuladores digitales y modelos de movimiento",
  "Experimentó, guías", "Computadora. Y algunos instrumentos de laboratorio",
  "Libros de textos propios e instrumentos de laboratorio propios",
  "Instrumentos de precisión",
  "Libros, y lo necesario para experimentar", 
  "Juegos de Geometría tamaño Grande, Materiales de Laboratorio", 
  "carteles, libros, instrumentos de laboratorio, pizarra inteligente, Internet, materiales para experimentos"
)

### equipo tecnologico
valores_equipo_tecnologia_propio <- c(
  "COMPUTADORA", "COMPUTADORA, TELEFONO,", "Cajonera, pizarrón, lecturas",
  "Carteles, computador, hojas de trabajo", "Bocina inalámbrica, Fichas de textos",
  "Bocina Bluetooth", "Cañonera", "Cañonera, computadora, libros material didáctico", 
  "Cañonera, hojas de actividades, materiales lúdicos", "Celular y cartelera", 
  "Celular,  marcadores, computadora, ejercicio escritos y resueltos", 
  "Celular,  marcadores, ejercicios escritos resueltos", "Centro de computación", 
  "Comoutadora", "Comoutadora, impresiones", 
  "Reglas y transportador para pizarra, computadora y cañonera.",
  "Libros, recursos tecnológicos",  "Recursos didácticos",
  "Recopilación  de material bibliografico de varios autores/ Laptop, Reproductor  de imágenes  equipo de Audio./ Herramientas  agricolas",
  "Libro, computadora, otros", "Libro, folleto, cañonera  y imágenes",
  "Recurso Tecnológico y Libros de Texto", "Recursos audiovisuales , tecnología (computadora ).", 
  "Recursos de la Web y herramientas virtuales",
  "Minicomputadora, tablet, internet",  "Pantalla, computadora",
  "Libros y computadora", "Libros y computadora, fotocopias", 
  "Libros, Computadoras, fotocopias, juegos", 
  "Libros de texto, computadora",  "Pc, impresiones", 
  "Investigaciones,  impresiones,  cañonera, hojas de trabajo entre otros",
  "Libros de ciencias naturales, fotocopias, guías de trabajo, proyecciones, audio",
  "Libros extra, internet, celular, computadora", 
  "Libros, Folletos, Computadora, Proyector, material didáctico, etc.", 
  "Libros, Internet, computadora, cañonera.", "Libros, Videos, Juegos de mesa, hojas, fotocopias, Carteles, calculadoras", 
  "Computadora", "Computadora  telefono", "Computadora e impresora", 
  "Computadora fichas de trabajo", "Computadora portatil", 
  "Computadora portátil", "Computadora y bocinas", "Computadora y materiales didácticos", 
  "Computadora y otros", "Computadora!!", "Computadora,  textos", 
  "Computadora, Internet", "Computadora, canonera. Recortes.", 
  "Computadora, cañonera, juegos didácticos, etc", "Computadora, celular", 
  "Computadora, celular, internet", "Computadora, crayones,  hojas", 
  "Computadora, diapositiva", "Computadora, fotocopias", "Computadora, hojas de trabajo computadora", 
  "Computadora, impresiones y material digital.", "Computadora, impresora", 
  "Computadora, lapiceros, marcadores", "Computadora, libro de texto, IA", 
  "Computadora, libro, copias, marcadores", "Computadora, libros", 
  "Computadora, libros, hoja de trabajos", "Computadora, marcadores", 
  "Computadora, material de reciclaje, hojas, memoria USB, marcadores, sellos, lapiceros", 
  "Computadora, teléfono,", "Computadora, teléfono, hojas de trabajo, carteles", 
  "Computadora,cañonera, bocina, internet,plataformas digitales gratuitas.", 
  "Computadora. Videos", "Computadora. Y algunos instrumentos de laboratorio", 
  "Computatora, cañonera, marcadores, páginas web, YouTube, entre otros",
  "El internet", "Impresiones, computadora, impresión, imágenes, fotocopias, crayones, otros", 
  "Internet", "Internet  y otros libros de apoyo", "Internet AI", 
  "Internet, computadora, cañonera,  impresora", "Internet, computadora, hojas", 
  "Internet, computadora, impresora, fotocopia", "Internet, folletos, otros libros", 
  "Internet, hojas, marcadores, lapiceros", "Internet, impresora y computadora", 
  "LAPTOP E INTERNET",        "Laptop e internet", "Laptop, celular para decarga de internet", 
  "Laptop, cuaderno de asistencia, material desactivo",
  "Tablet, Teléfono Inteligente, Material Impreso", 
  "Tableta", "Tecnologicos", "Tecnología", "Tecnológico y Libros de Texto", 
  "Tecnológicos y físicos", "Televisor , reproductor de  audio etc.", 
  "Teléfono celular y computadora", "Teléfono e impresiones", 
  "Teléfono, audios, imágenes, libros.", "Teléfono, internet", 
  "Teléfono, lapicero, cuaderno, asistencia, lista de cotejo.", 
  "Textos, recursos didácticos, computadora", "tablet",
  "carteles, teléfono, computadora", "computadora", "computadora, material de reciclaje"
)

### Material de papeleria
valores_papeleria_propio <- c(
  "Carteles, fotocopias, investigaciones",     "Carteles, marcadores, audio visual", "Carteles, marcadores, internet", 
  "Computadora, material de reciclaje, hojas, memoria USB, marcadores, sellos, lapiceros", 
  "Carteles, copias, hojas, crayones",
  "Libros, paginas web, documentos pdf",
  "Económicos: impresiones, hojas, ….", "Ejercicios escritos resuelto, celular, pizarrón. Marcadores", 
  "Elaboración de folletos", 
  "Lo del con texto,impreso y palpables",
  "Libros, videos folletos, calculadora, metros, reglas",
  "Copias", "Copias ,hojas de trabajo, telefono con internet", 
  "Copias.", "Cuaderno de apuntes, fotocopias, marcadores.", 
  "Cuaderno, hojas, lápiz, colores",
  "Impresiones, hojas, teléfono, audios.",     "Impresiones, revistas, laptop, internet.", "Impresos", "Imágenes, hojas de trabajo, crayones, marcadores, tijera, etc", 
  "Folletos, marcadores, cartulinas, hojas etc.", "Folletos, periódico murales hojas de trabajo.", 
  "Folletos, períodicos murales, hojas de trabajo", "Fotocopias, servicio de Internet, computadora, impresora, plataformas digitales.", 
  "Hoja de trabajo, carteles", "Hojas", 
  "Libro de texto, internet, recortes, cuadernos, hojas.", 
  "Libro de textos y folleto",
  "Libro, hojas, Internet marcadores",
  "marcadores, almohadillas, lapiz, borrador, sacapuntas", 
  "Hojas .papel bon", "Hojas de trabajo", "Hojas de trabajo y computadora", 
  "Hojas de trabajo y guias", "Hojas de trabajo y marcadores", 
  "Hojas de trabajo y organizadores graficas", "Hojas de trabajo,  diapositivas, esquemas gráficos", 
  "Hojas de trabajo, carteles, material de apoyo", "Hojas de trabajo, impresiones.", 
  "Hojas de trabajo, libros, investigaciones", "Hojas de trabajo, pizarra marcador, almohadilla", 
  "Hojas de trabajos", "Hojas de trabajos, Lista de Cotejos, carteles, Marcadores", 
  "Hojas y cartulinas.", "Hojas, cartulinas borrador libro,tinta,hojas doble oficio estuche juego geometricohos", 
  "Pizarrón, marcadores, hojas con actividades",
  "material impreso, material reciclable,",
  "Investigación de los temas pinturas crayones y hojas", 
  "materiales de reciclaje como botellas pet, cajas de cartòn, tubos de cartòn,papel reciclado,",
  "Papelografos, marcadores, pizarra, hojas reglas, compás, cuaderno, impresiones, links  libro me lo se todo de la ciencia entre otros.", 
  "Papelografos, marcadores, sellador", "Papelogragos hojas marcadores hojas entre otros", 
  "Papelógrafos, cartulinas, hojas, impresora, computadora.", 
  "Maquetas, papel, colores etc.", "Marcadoras,  cartulinas,  juego geométrico", 
  "Marcadores crayones pliegos de papel", "Marcadores para pizarra, hojas de colores y silicón en líquido", 
  "Marcadores pizarra etc", "Marcadores, almohadillas, guía del docente", 
  "Marcadores, folletos, hojas de trabajo, otros", "Marcadores, hojas de trabajo, pizarrón", 
  "Marcadores, sellos", "Marcadores, textos, papelogragos, computadora", 
  "Marcadores,almohadilla, reglas", "Marcadores,hojas ,almohadilla , lapiceros, cuaderno", 
  "Libros,marcadores,hojas,pliegos de papel bond", "Libros,teléfono", 
  "Libros,teléfono,crayones,tijeras,lapiceros,libretas,cuadernos", 
  "Libros, internet, fotocopias, computadora, cuadernos, grapadora, etc.", 
  "Hojas, cartulinas,plastilina, bolas de duropor, marcafores de colores etc", 
  "Hojas, libro, internet, computadora", "Hojas, libro, marcadores", 
  "Hojas, libros, marcadores de colores para pizarra.", "Hojas, marcadores, crayones, material de reciclaje", 
  "Hojas,papel Bond, hojas", "INTERNET", "Imagenes, videos carteles, cartulina plastilina temperas y una computadora", 
  "Impresiones, ilustraciones, celular tijeras, laptop,   internet,", 
  "Libro, Transportador ,Regla, Pita, Hojas  de papel bond,Tijera, resistol", 
  "Pizzaron, marcadores, afiches e impresiones", "FOLLETOS IMPRESOS",
  "Impresiones", "Impresiones, computadora, impresión, imágenes, fotocopias, crayones, otros"
  
)

### Guias
valores_guias_propio <- c(
  "Guia de aprendizaje", "Guia de aprendizaje del gobierno", 
  "Guia de autoaprendizaje que se realizo durante la Pandemia", 
  "Guia de ciencias Naturales", "Guia y planificador", "Guias, material didáctico", 
  "Guía de ciencias naturales de telesecundaria", "Guías", 
  "Guías de trabajo"
  
)

### Recursos digitales
valores_digitales_propio <- c(
  "Computadora,cañonera, bocina, internet,plataformas digitales gratuitas.", 
  "De páginas web",  "Digitales",  "Documentos PDF",
  "Material digital de Internet",
  "Páginas web y libros", "Páginas web, libros de consulta", 
  "Páginas web, textos",  "Recursos digitales elaborados de acuerdo a los temas. Fotocopias, presentaciones, videos.", 
  "Material de páginas Web", "Medio Digital", "Paginas web, libros", 
  "Inteligencia artificial, tutoriales de YouTube, libros te texto", 
  "investigar en la web los temas a impartir", "TELEFONO CON INTERNET Y LA COMPUTADORA CON INTERNET", 
  "Tuturiales", "Revistas, enciclopedias impresas,  material video gráfico, audio, internet"
) 

### no especifica 
valores_noespecifica_recurso <- c(
  "Biologia",  "Conceptos básicos", "Económicos",
  "Todas las posibles", "Todo", "Todo es mio", "Todos", 
  "Variados"
)

### folletos y hojas de trabajo 
valores_folletos <- c(
  "Folletos", "Folletos  periódicos  murales  hoja de trabajo", 
  "Folletos con ejercicios", "Folletos, internet, cuadernillos",
  "Investigaciones,  impresiones,  cañonera, hojas de trabajo entre otros"
)

### Investigaciones
valores_investigaciones <- c(
  "Investigaciones", "Investigaciones en el intwenet", 
  "Revistas,  investigations, entre otros",
  "Materiales impresos (libros, fotocopias), materiales reciclados 
  (botellas, cartones) recursos audiovisuales (videos), juegos interactivos y análisis de datos.  
  Materiales del medio natural: Observación directa del entorno para realizar investigaciones y exploraciones. 
  Experimentos y actividades prácticas",
  " Materiales impresos (libros, fotocopias), materiales reciclados (botellas, cartones) recursos audiovisuales (videos), juegos interactivos y análisis de datos.  Materiales del medio natural: Observación directa del entorno para realizar investigaciones y exploraciones. Experimentos y actividades prácticas",
  "Investigaciones propias", "Investigaciones,  impresiones,  cañonera, hojas de trabajo entre otros", 
  "Investigaciones, hojas de trabajo, libro", "Investigación de los temas pinturas crayones y hojas", 
  "Investigación por medio de internet", "Juegos de Geometría tamaño Grande, Materiales de Laboratorio"
)

### planificador
valores_planificador_propio <- c(
  "Planificador", 
  "Planificador y guías de aprendizaje", 
  "un planificador y una guiilla."
)

### ninguno
valores_ninguno_recurso <- c(
  "Ninguno"
)

## Recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    cn_recursos_propios = case_match(
      cn_recursos_propios,
      all_of(valores_ninguno_recurso) ~ "Ninguno",
      all_of(valores_planificador_propio) ~ "Planificador",
      all_of(valores_investigaciones) ~ "Investigaciones",
      all_of(valores_folletos) ~ "Folletos y hojas de trabajo",
      all_of(valores_noespecifica_recurso) ~ "No especifica recurso",
      all_of(valores_digitales_propio) ~ "Recursos digitales",
      all_of(valores_guias_propio) ~ "Guías",
      all_of(valores_papeleria_propio) ~ "Material de papeleria",
      all_of(valores_equipo_tecnologia_propio) ~ "Equipo tecnológico",
      all_of(valores_laboratorio_propio) ~ "Equipo de laboratorio",
      all_of(valores_libros_propio) ~ "Libros",
      all_of(valores_audiovisual_didactico_propio) ~ "Material audiovisual y didáctico",
      .default = cn_recursos_propios
    )
  )

## volver a ver errores

valores_cn_recursos_propios <- df_dyd |>
  filter(!(is.na(cn_recursos_propios))) |>
  select(cn_recursos_propios) |>
  tabyl(cn_recursos_propios)

# View(valores_cn_recursos_propios)



# cn_otro_ultima_vez_libros_mineduc ----

## ver errores
valores_cn_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(cn_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(cn_otro_ultima_vez_libros_mineduc))) |>
  select(cn_otro_ultima_vez_libros_mineduc) |>
  tabyl(cn_otro_ultima_vez_libros_mineduc)

# View(valores_cn_otro_ultima_vez_libros_mineduc)

## Define los vectores ----

### en el 2014
valores_2014 <- c(
  "2014"
)

### en el 2015
valores_2015 <- c(
  "2015", "Hace 10 años"
)

### en el 2016
valores_2016 <- c(
  "2016"
)

### en el 2017
valores_2017 <- c(
  "2017", "2017, pero se había recibido instrucciones en utilizar hasta el año 2024"
)

### en el 2018
valores_2018 <- c(
  "2018", "2018 o 2,020", "2018..", "Hace 7 años"
)

### en el 2020
valores_2020 <- c(
  "2020", "2018 o 2,020", "2020 aproximadamente", "En el 2020"
)
### en el 2019
valores_2019 <- c(
  "2019", "En 2019"
)

### en el 2021 
valores_2021 <- c(
  "2021"
)

### en el 2022
valores_2022 <- c(
  "2022"
)

### nunca
valores_nunca_libros <- c(
  "El instituto cuenta con los libros", "Ninguna vez",
  "Nunca"
)

### no recuerda
valores_norecuerda_libros <- c(
  "No recuerdo  el año", "No recuerdo"
)
## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cn_otro_ultima_vez_libros_mineduc = case_match(
      cn_otro_ultima_vez_libros_mineduc,
      all_of(valores_nunca_libros) ~ "Nunca",
      all_of(valores_2022) ~ "En el 2022",
      all_of(valores_2021) ~ "En el 2021",
      all_of(valores_2019) ~ "En el 2019",
      all_of(valores_2020) ~ "En el 2020",
      all_of(valores_2018) ~ "En el 2018",
      all_of(valores_2017) ~ "En el 2017",
      all_of(valores_2016) ~ "En el 2016",
      all_of(valores_2015) ~ "En el 2015",
      all_of(valores_2014) ~ "En el 2014",
      all_of(valores_norecuerda_libros) ~ "No recuerda",
      .default = cn_otro_ultima_vez_libros_mineduc
    )
  )

## volver a ver errores

valores_cn_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(cn_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(cn_otro_ultima_vez_libros_mineduc))) |>
  select(cn_otro_ultima_vez_libros_mineduc) |>
  tabyl(cn_otro_ultima_vez_libros_mineduc)



# cn_tipo_materiales_recibidos ----

## ver errores

valores_cn_tipo_materiales_recibidos <- df_dyd |>
  filter(cn_recepcion_materiales__mineduc == "Sí") |>
  filter(!(is.na(cn_tipo_materiales_recibidos))) |>
  select(cn_tipo_materiales_recibidos) |>
  tabyl(cn_tipo_materiales_recibidos)

dput(valores_cn_tipo_materiales_recibidos)

## Define los vectores ----

### guias de aprendizaje
valores_guia_aprendizaje_recibido <- c(
  "GUÍA DE APRENDIZAJE", "GUÍA DE APRENDIZAJE Y PLANIFICADOR", 
  "Guia de aprendizaje Primero Basico", "Guia y planificador", 
  "Guias", "Guía de aprendizaje", "Guía de conceptos basicos", 
  "Planoficadores y guías telesecundaria",
  "Guía del docente", "Guía y planificador", "CNB, Guías de Aprendizaje", 
  "Solamente la guia de aprendizaje", "Solamente la guía que vino para los estudiantes y de alli fue que yo tome 1 de cada grado"
)

### libro de texto
valores_libro_texto <- c(
  "Libro de Ciencias Naturales", "Libro de texto", "Libro de texto.", 
  "Libro de textos", "Libros", "Libros de texto", "Libros de textos",
  "Textos", "Documento Guardianes Ecológicos Guatemala"
)

### material de papeleria
valores_papeleria_recibido <- c(
  "Engrapadora, hojas reglas, tijera, marcadores, tinta perforador, corrector, grapas y USB.", 
  "Hojas,marcadores, lapiceros", 
  "Libros de texto, cartulinas, marcadores, lapiceros, almohadilla, lápices, crayones, temperas", 
  "MARCADORES, LAPICEROS, HOJAS, PLIEGO DE PAPEL BOND.", 
  "Marcadores cartulinas,goma", "Marcadores, hojas, cartulinas, etc.", 
  "Marcadores, papel bond y un block de papel de color , lapiceros, lápiz, papel de baño, utencilios de limpieza, almohadillas, dispensador de tape, rollo de masking tape, rollo de tape ancho, limpiadores.", 
  "Papel, marcadores."
)

### valija didáctica
valores_valija <- c(
  "Valija didáctica"
)

### equipo de laboratorio
valores_microscopio <- c(
  "microscopio"
)

### ninguno
valores_ningun_recibido <- c(
  "Ninguno", "Vale del Mineduc"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cn_tipo_materiales_recibidos = case_match(
    cn_tipo_materiales_recibidos,
    all_of(valores_ningun_recibido) ~ "Ninguno",
    all_of(valores_microscopio) ~ "Equipo de laboratorio",
    all_of(valores_valija) ~ "Valija didáctica",
    all_of(valores_papeleria_recibido) ~ "Material de papelería",
    all_of(valores_libro_texto) ~ "Libro de texto",
    all_of(valores_guia_aprendizaje_recibido) ~ "Guías de aprendizaje",
    .default = cn_tipo_materiales_recibidos
    )
  )

## volver a ver errores

valores_cn_tipo_materiales_recibidos <- df_dyd |>
  filter(cn_recepcion_materiales__mineduc == "Sí") |>
  filter(!(is.na(cn_tipo_materiales_recibidos))) |>
  select(cn_tipo_materiales_recibidos) |>
  tabyl(cn_tipo_materiales_recibidos)

# View(valores_cn_tipo_materiales_recibidos)

# cs_libros_primero ----

## ver errores

valores_cs_libros_primero <- df_dyd |>
  filter(cs_uso_libros == "Sí") |>
  filter(cs_grados == "Primer grado") |>
  filter(!(is.na(cs_libros_primero))) |>
  select(cs_libros_primero) |>
  tabyl(cs_libros_primero)

# View(valores_cs_libros_primero)

## Define los vectores ----

### ciencias sociales 1
valores_sociales_1 <- c(
  "1", "Ciencias sociales I Santillana", "Ciencias de Sociales de 1ro básico"
)

### ciencias sociales y formacion ciudadana
valores_sociales_formacion <- c(
  "Ciencias Sociales y Formación Ciudadana 1 de Editorial Santillana",
  "Ciencias Sociales y Formación Ciudadana 7 Grupo Quiriguá-IGER",
  "Ciencias Sociales y Formación Ciudadana Telesecundaria",
  "Ciencias Sociales y formación ciudadana",
  "Ciencias sociales y formación ciudadana",
  "Planificador ciencias sociales y ciudadanía 1 basico",
  "CIENCIAS Sociales y Ciudadania Susaeta",
  "Planificador del facilitador y Guia de Aprendizaje Ciencias Sociales, Formación Ciudadana e Interculturalidad 1"
)

### ciencias sociales
valores_sociales_ciencias <- c(
  "Ciencias sociales Santillana",
  "Guia y planificador telesecundaria ciencias sociales"
)


### ciencias sociales editora educativa
valores_sociales_educativa <- c(
  "Editora educativa y Santillana y constitución política",
  "Editora educativa y enciclopedia"
)

### educacion intercultural
valores_edu_intercultural <- c(
  "Educación intercultural, cultura gastronómica, Constitución Política de la República de Guatemala",
  "Planificador del facilitador y Guia de Aprendizaje Ciencias Sociales, Formación Ciudadana e Interculturalidad 1",
  "Educación intercultural,  cultura gastronómica,  Constitución Política de la República de Guatemala"
)

### constitución
valores_constituciones <- c(
  "Educación intercultural, cultura gastronómica, Constitución Política de la República de Guatemala",
  "Editora educativa y Santillana y constitución política",
  "Educación intercultural,  cultura gastronómica,  Constitución Política de la República de Guatemala"
)

### escenarios
valores_escenarios <- c(
  "Escenarios"
)

### Ciencias sociales planificador del facilitador
valores_sociales_planificador <- c(
  "PLANIFICADOR", "Planificador", "Planificador del facilitador y Guia de Aprendizaje Ciencias Sociales, Formación Ciudadana e Interculturalidad 1",
  "Planificador y guías en pdf", "Planificador,", "Planificador,"
)

### enciclopedia
valores_enciclopedia <- c(
  "Enciclopedicos", "LIBROS EDITORIAL SANTILLANA, ENCICLOPEDIAS"
)

### hechos 1
valores_hechos1 <- c(
  "Hechos 1. Delta"
)

### sin especificar
valores_noespecifica_sociales <- c(
  "Santillana", "Santillana, Norma, conceptos básicos de Telesecundaria",
  "Santillana,  Norma, conceptos básicos de Telesecundaria"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cs_libros_primero = case_match(
    cs_libros_primero,
    all_of(valores_noespecifica_sociales) ~ "No especifica el nombre",
    all_of(valores_hechos1) ~ "Hechos 1",
    all_of(valores_enciclopedia) ~ "Enciclopedia",
    all_of(valores_sociales_planificador) ~ "Ciencias Sociales: Planificador del facilitador",
    all_of(valores_escenarios) ~ "Escenarios",
    all_of(valores_constituciones) ~ "Constitución Política de la República de Guatemala",
    all_of(valores_edu_intercultural) ~ "Educación Intercultural",
    all_of(valores_sociales_educativa) ~ "Ciencias Sociales Editora Educativa",
    all_of(valores_sociales_ciencias) ~ "Ciencias Sociales",
    all_of(valores_sociales_formacion) ~ "Ciencias Sociales y Formación Ciudadana",
    all_of(valores_sociales_1) ~ "Ciencias Sociales 1",
    .default = cs_libros_primero
    )
  )

## volver a ver errores

valores_cs_libros_primero <- df_dyd |>
  filter(cs_uso_libros == "Sí") |>
  filter(cs_grados == "Primer grado") |>
  filter(!(is.na(cs_libros_primero))) |>
  select(cs_libros_primero) |>
  tabyl(cs_libros_primero)


# cs_libros_segundo ----

## ver errores

valores_cs_libros_segundo <- df_dyd |>
  filter(cs_uso_libros == "Sí") |>
  filter(cs_grados == "Segundo grado") |>
  filter(!(is.na(cs_libros_segundo ))) |>
  select(cs_libros_segundo) |>
  tabyl(cs_libros_segundo)

# View(valores_cs_libros_segundo)

## Define los vectores ----

### ciencias sociales editora educativa
valores_sociales_educativa2 <- c(
  "Ciencias Sociales de Editora Educativa",
  "Ciencias sociales editora educativa ciencias sociales santillana y la web",
  "Editora Educativa, Conceptos Básicos, Guía de Aprendizaje",
  "Ciencias sociales editora educativa  ciencias sociales santillana y la web"
)

### ciencias sociales
valores_sociales_ciencias2 <- c(
  "Ciencias Sociales, Historia de Guatemala, Ciencias Sociales para Segundo Básico.",
  "Libro de ciencias sociales", 
  "Ciencias sociales editora educativa  ciencias sociales santillana y la web",
  "Libro de ciencias sociales segundo basico",
  "Ciencias sociales para segundo basico"
)

### historia de guatemala
valores_historia_guate <- c(
  "Ciencias Sociales, Historia de Guatemala, Ciencias Sociales para Segundo Básico."
)

### ciencia sociales 2
valores_sociales2 <- c(
  "Ciencias sociales 2 Santillana"
)

### constitución politica
valores_constitucion <- c(
  "Constitución política de la república de Guatemala"
)

### guia metodológica de ciencias sociales
valores_guia_metodo_cs <- c(
  "Guía Metodologica de Ciencias Sociales. Ciencias Sociales 14 de Editorial Santillana",
  "Guía Metodológica de Ciencias Sociales. Ciencias Sociales segundo básico editorial Santillana."
)

### ciencias sociales guía de aprendizaje
valores_guia_sociales <- c(
  "Guía de aprendizaje", "Guías", "Guías de aprendizaje, Enciclopedia Educativa, otros.",
  "Planificados del docente y Guía de aprendizaje de Telesecundaria"
)

### Interculturalidad
valores_interculturalidad <- c(
  "Intercilturalidad"
)

### ciencias sociales planificador para el facilitador
valores_planificador_sociales <- c(
  "Planificador de Ciencias Sociales", "Planificador de telesecundaria.",
  "Planificador del docente", "Planificador y guia de Telesecundaria",
  "Planificados del docente y Guía de aprendizaje de Telesecundaria"
)

### proyecto saber
valores_proyecto_sociales <- c(
  "Proyecto saber Santillana"
)

### sin especificar
valores_noespecifica_sociales2 <- c(
  "Google", "Mata curricular", "Santillana"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cs_libros_segundo = case_match(
      cs_libros_segundo,
      all_of(valores_noespecifica_sociales2) ~ "No especifica el nombre",
      all_of(valores_proyecto_sociales) ~ "Proyecto saber",
      all_of(valores_planificador_sociales) ~ "Ciencias Sociales: Planificador para el facilitador",
      all_of(valores_interculturalidad) ~ "Interculturalidad",
      all_of(valores_guia_sociales) ~ "Ciencias Sociales: Guía de aprendizaje",
      all_of(valores_guia_metodo_cs) ~ "Guía Metodológica de Ciencias Sociales",
      all_of(valores_constitucion) ~ "Constitución Política de la República de Guatemala",
      all_of(valores_sociales2) ~ "Ciencias Sociales 2",
      all_of(valores_historia_guate) ~ "Historia de Guatemala",
      all_of(valores_sociales_ciencias2) ~ "Ciencias Sociales",
      all_of(valores_sociales_educativa2) ~ "Ciencias Sociales Editora Educativa",
      .default = cs_libros_segundo
    )
  )

## volver a ver errores 

valores_cs_libros_segundo <- df_dyd |>
  filter(cs_uso_libros == "Sí") |>
  filter(cs_grados == "Segundo grado") |>
  filter(!(is.na(cs_libros_segundo ))) |>
  select(cs_libros_segundo) |>
  tabyl(cs_libros_segundo)


# cs_libros_tercero ----

## ver errores

valores_cs_libros_tercero <- df_dyd |>
  filter(cs_uso_libros == "Sí") |>
  filter(cs_grados == "Tercer grado") |>
  filter(!(is.na(cs_libros_tercero ))) |>
  select(cs_libros_tercero) |>
  tabyl(cs_libros_tercero)

dput(valores_cs_libros_tercero)

## Define los vectores ----

### ciencias sociales 3
valores_sociales3 <- c(
  "CIENCIAS SOCIALES 3 EU"
)

### ciencias sociales
valores_sociales_ciencias3 <- c(
  "Ciencias Sociales de Tercero Santillana", "Libro de ciencias sociales"
)

### ciencias sociales y formación ciudadana
valores_formacion_ciudadana <- c(
  "Ciencias Sociales y Formación Ciudadana", "E-grafías,  susaeta",
  "Escenario  ciencias  Sociales  y Formación  Ciudadana"
)

### ciencias sociales guia de aprendizaje
valores_guia_aprendizaje_sociales <- c(
  "Guía de aprendizaje", "Guía de aprendizaje ciencias sociales",
  "Planificador, Guias de Aprendizaje y Conceptos Básicos Vol. I, II, III, IV, Santillana"
)

### ciencias sociales planificador para el facilitador
valores_planificador_sociales3 <-c (
  "Planificador del docente y guia de aprendizaje de Telesecundaria", 
  "Planificador docente", "Planifecador para el docente", 
  "Planificador, Guias de Aprendizaje y Conceptos Básicos Vol. I, II, III, IV, Santillana"
)

### no especifica
valores_noespecifica_sociales3 <- c(
  "SANTILLANA", "Santillana", "Santillana, Susaeta", "Susaeta",
  "Libros digitales", "Textos para estudiantes"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cs_libros_tercero = case_match(
    cs_libros_tercero,
    all_of(valores_noespecifica_sociales3) ~ "No especifica el nombre",
    all_of(valores_planificador_sociales3) ~ "Ciencias Sociales: Planificador para el facilitador",
    all_of(valores_guia_aprendizaje_sociales) ~ "Ciencias Sociales: Guía de aprendizaje",
    all_of(valores_formacion_ciudadana) ~ "Ciencias Sociales y Formación Ciudadana",
    all_of(valores_sociales_ciencias3) ~ "Ciencias Sociales",
    all_of(valores_sociales3) ~ "Ciencias Sociales 3",
    .default = cs_libros_tercero
    )
  )

## volver a ver errores

valores_cs_libros_tercero <- df_dyd |>
  filter(cs_uso_libros == "Sí") |>
  filter(cs_grados == "Tercer grado") |>
  filter(!(is.na(cs_libros_tercero ))) |>
  select(cs_libros_tercero) |>
  tabyl(cs_libros_tercero)


# cs_recursos_centro_educativo ----
valores_cs_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(cs_recursos_centro_educativo))) |>
  select(cs_recursos_centro_educativo) |>
  tabyl(cs_recursos_centro_educativo)

dput(valores_cs_recursos_centro_educativo)

## Define los vectores ----

### libro
valores_libro_cs <- c(
  "1 libro",  "Algunos libros", "BIBLIOGRAFÍAS DIVERSAS",
  "Canionera, libros, pizarra.", "Ciencias sociales y ciudadanía", 
  "Enciclopedias, libros, folletos, trifoliares",
  "Imágenes libros", "LIBROS, MANUALES",
  "LIBROS, FOLLETOS. DOCUMENTOS EN PDF",
  "libros y laminas", "libros y uso de la tecnología",
  "LIBRO DE TEXTO", "LIBROS DE SANTILLANA", "LIBROS DE TEXTO", 
  "LIBROS DE TEXTOS, CAÑONERA, BOCINAS, COPIAS, INTERNET, APOYO TÉCNICO", 
  "LIBROS, FOLLETOS. DOCUMENTOS EN PDF", "LIBROS, MANUALES",
  "Libro", "Libro Santillana de Ciencias Sociales", 
  "Santillana", "Si", "Sociales Santillana", "Solo el libro de texto del docente hay que sacar copia", 
  "Solo planificador", "Solo planificadores", "Susaeta", "TEXTO TERCERO BÁSICO Y BIBLIOGRAFIA ESPECIALIZADA DEL CURSO", 
  "TEXTOS DE CADA UNO DE LOS GRADOS", "Textos", 
  "Textos para estudiantes",
  "Compendium de historia de Guatemala", 
  "Módulos de aprendizaje No. 2",
  "Desafíos socioeconómicos: retos y oportunidades:Aracely Martínez, Nancy L.", 
  "Documentos de apoyo",
  "san tillana", "Únicamente libro del área",
  "Una biblioteca de aula con diversos libros, con diversas ediciones y autores, y libros de ediciones actualizadas.", 
  "libro", "libro, plataforma, laptop, marcadores, almohadilla, lapicero verde, hojas de control zonas y escritorio",
  "Libro de MINEDUC", "Libro de Santillana, hojas impresas, tecnología.", 
  "Libro de texto", "Libro de texto Susaeta Integrado 7 y 8", "Libro de texto, cañoneras, pliegos de papel", 
  "Libro de textos, acceso a Internet y medios audiovisuales", 
  "Libro del docente", "Libro te texto", "Libro y pizarrón", "Libro, Audiovisual", 
  "Libro, folleto", "Libros", "Libros  y  materiales", "Libros Digitales", 
  "Libros con temas claros", "Libros de Ciencias Sociales 1,2,3", 
  "Libros de apoyo, cañonera, internet", "Libros de contenidos, libros de lectura", 
  "Libros de texto", "Libros de texto computadora cañonera", "Libros de texto y algunas revistas", 
  "Libros de texto y material para la planificación.", "Libros de texto, dispositivos tecnológicos", 
  "Libros de texto, folletos.", "Libros de texto, guías, impresora", 
  "Libros de texto, material audiovisual y tecnología", "Libros de textos", 
  "Libros de textos.", "Libros digitales", "Libros hojas impresas reproductor", 
  "Libros marcadores pizarra papelografos", "Libros y material didáctico", 
  "Libros y materiales", "Libros, carteles, folletos  etc", "Libros, computadora, cañonera y bocina. También material de librería dependiendo las actividades con los estudiantes.", 
  "Libros, computadora, impresora, marcadores, hojas", "Libros, hojas, juegos", 
  "Libros, impresora", "Libros, mapas,", "Libros, material en audio y video", 
  "Libros, pizarra y Televisión", "Libros, pizarrón", "Libros, proyector", 
  "Libros, recursos materiales y el entorno", "Libros, retroproyector, yeso.", 
  "Libros, televisión", "Los libros de texto", "Los libros de texto únicamente"
)

### material audiovisual y didáctico
valores_audiovisual_didactico_cs <- c(
  "1 vez al año a veces la valija", "A veces la valija didáctica",
  "Balija didactica", "La valija didáctica.",
  "Libro de textos, acceso a Internet y medios audiovisuales",
  "Carteles", "Carteles, materiales lúdicas, y otros.",
  "Cartulina, hojas, fotocopias, regla, etc",
  "Valija didactica", "Valija didáctica", "Afiches",
  "Videos audios libros", "material propio.", 
  "Cuerdas, Pines y Aros",
  "mterial didáctico, libro de texto", "Geografía visualizada",
  "Únicamente valija didáctica", "Didáctico , fotocopias",
  "Todo lo relacionado con materiales didácticos.",
  "Recursos didácticos", "Recursos pedagógicos",
  "Videos. Textos", "internet, copias, libros etc",
  "Material didáctico y libros", "Material didáctico y material tecnológico", 
  "Material impreso", "Material mediado", "Materiales didácticos",
  "Libro, Audiovisual", "Libros de texto, material audiovisual y tecnología",
  "Libros Digitales", "Libros y material didáctico"
)

### material de papeleria
valores_papeleria_cs2 <- c(
  "Carteles", "Carteles, materiales lúdicas, y otros.",
  "Cartulina, hojas, fotocopias, regla, etc",
  "Cuaderno, hojas de colores, carteles, imágenes.",
  "Libros, carteles, folletos  etc", "Estuche de marcadores", 
  "Revistas, periódicos", "Tinta,hojas", 
  "Fotocopias", "Fotocopias, marcadores, hojas , tintas", 
  "Fotocopias, tinta, marcadores.",
  "Marcadores carteles",
  "Marcadores, libros y computadora",
  "Revistas, periódicos", "Tinta,hojas",
  "Papel, marcadores, crayones, lapiceros, lápices, folders, cuadernos.", 
  "Papelografos y los naturales del entorno", "Pdf",
  "Hojas e impresiones", "Humanos, hohoas de tareas, marcadores, pizarra", 
  "La malla curricular del área, fotocopias,marcadores,pizarra", 
  "Fotocopias", "Fotocopias, marcadores, hojas , tintas", 
  "Fotocopias, tinta, marcadores.",
  "Marcador, pizzara, almohadilla,cartulina.", "Marcadores carteles", 
  "Marcadores de Pizarra y almohadia", "Marcadores y almohadia de Pizarra", 
  "Marcadores y cartulinas", "Marcadores y cartulinas.", "Marcadores,  hojas", 
  "Marcadores, almohadilla. Hojas, marcadores de pizarrón. Pizarrón", 
  "Marcadores, cartulinas", "Marcadores, hojas de papel bond, tinta para marcadores, lapiceros, grabadora, tv, amplificación de sonido", 
  "Marcadores, hojas libros, cañonera", "Marcadores, hojas, tinta", 
  "Marcadores, libros y computadora", "Marcadores, libros, cañonera, televisores", 
  "Marcadores, libros, cañonera, televisores, pizarrones",
  "Marcadores, papelógrafos, computadora, bocinas, impresora, cañonera . Libros actualizados de diferentes autores",
  "Cuaderno, hojas de colores, carteles, imágenes.",
  "Papel, marcadores, crayones, lapiceros, lápices, folders, cuadernos.", 
  "Papelografos y los naturales del entorno", "Pdf", "Pizarra, marcadores, papel, cañonera", 
  "La malla curricular del área, fotocopias,marcadores,pizarra", 
  "Humanos, hohoas de tareas, marcadores, pizarra",
  "Proyector, hojas, cartulina, marcadores.",
  "Marcador, pizzara, almohadilla,cartulina.", "Estuche de marcadores",
  "Pizarrón, marcadores, hojas", "Pizarrón, pliegos de papel, cartulina, marcadores", 
  "libro, plataforma, laptop, marcadores, almohadilla, lapicero verde, hojas de control zonas y escritorio",
  "Marcadores carteles", "Marcadores de Pizarra y almohadia", "Marcadores y almohadia de Pizarra", 
  "Marcadores y cartulinas", "Marcadores y cartulinas.", "Marcadores,  hojas", 
  "Marcadores, almohadilla. Hojas, marcadores de pizarrón. Pizarrón", 
  "Marcadores, cartulinas", "Marcadores, hojas de papel bond, tinta para marcadores, lapiceros, grabadora, tv, amplificación de sonido", 
  "Marcadores, hojas libros, cañonera", "Marcadores, hojas, tinta", 
  "Marcadores, libros y computadora", "Marcadores, libros, cañonera, televisores", 
  "Marcadores, libros, cañonera, televisores, pizarrones",
  "Marcadores, papelógrafos, computadora, bocinas, impresora, cañonera . Libros actualizados de diferentes autores",
  "Libros, computadora, cañonera y bocina. También material de librería dependiendo las actividades con los estudiantes.",
  "Hojas de trabajo, imágenes, crayones, etc",
  "Hojas, marcadores, lapiceros, grapadora, corrector, almohadilla", 
  "Hojas, marcadores, mapas, televisión", "Hojas, marcadores, pizarrón, computadora e impresora",
  "Hojas de trabajo, pizarra, marcador almohadilla",
  "Cuaderno, hojas de colores, carteles, imágenes.",
  "Cuaderno, hojas de colores, carteles, imagenes."
)

### CNB
valores_cnb_cs <- c(
  "CNB", "CNB y Planificador", "Cnb" 
)

### equipo tecnológico
valores_equipo_tecnologia_cs <- c(
  "Canionera, libros, pizarra.",
  "Libros, computadora, impresora, marcadores, hojas",
  "Cañonera", "Cañonera, libros de texto.", "Cañonera, pizarrón.", 
  "Computadora, cañonera, textos, folletos, internet, plataforma YouTube,",
  "Cañonera, pliego de Bond", "Centro de computación",
  "Libros de texto, dispositivos tecnológicos",
  "Tecnologico", "Tecnología", 
  "Pantalla, Impresora", 
  "Pizarra, marcadores, papel, cañonera",
  "Pantalla, pizzara", "Pantallas, libros de texto",
  "Proyector,", "Proyector, hojas, cartulina, marcadores.",
  "Tecnología, carteles", "Televisión o computadora",
  "Libros, material en audio y video", "Materiales tecnológicos como cañonera y laptop",
  "Mapas, Computadora, libros y otros", "libros y uso de la tecnología",
  "Libros de texto, material audiovisual y tecnología",
  "Hojas, proyector", "Libros de apoyo, cañonera, internet",
  "LIBROS DE TEXTOS, CAÑONERA, BOCINAS, COPIAS, INTERNET, APOYO TÉCNICO",
  "Impresora, cañonera y pizarra", "Libros de texto computadora cañonera",
  "Hojas, marcadores, pizarrón, computadora e impresora"
)

### mobiliario
valores_mobiliario_cs <- c(
  "Canionera, libros, pizarra.",
  "Pizarrón, hojas bonf", "Pizarrón, marcadores, hojas", "Pizarrón, pliegos de papel, cartulina, marcadores", 
  "Pizarrón. Aula pura"
)

### folletos
valores_folletos_recursos <- c(
  "Folleto", "Folleto, libro", "Folletos", "Folletos diapositivas", 
  "Folletos impresos planificador", "Folletos relacionados", "Folletos y libros educativos"
)

### guias
valores_guias_sociales <- c(
  "GUIAS, CNB, LIBROS", "Guia de conceptos basicos", 
  "Guia del docente", "Guias", "Guía de Ciencias Naturales.", 
  "Guía de aprendizaje", "Guía y planificador", "Guías de aprendizaje, planificadores, valija didáctica", 
  "Guías y planificadores"
)

### hojas de trabajo e impresiones
valores_hojas_impresiones <- c(
  "Hoja de investigación", "Hojas de investigación", 
  "Hojas de trabajo", "Hojas de trabajo, imágenes, crayones, etc", 
  "Hojas de trabajo, pizarra, marcador almohadilla", "Hojas y papel bon", 
  "Hojas, marcadores, lapiceros, grapadora, corrector, almohadilla", 
  "Hojas, marcadores, mapas, televisión", "Hojas, marcadores, pizarrón, computadora e impresora", 
  "Hojas, papelografo", "Hojas, proyector",
  "Impresiones", "Impresiones, marcadores.", "Impresora, cañonera y pizarra", 
  "Impresora, hojas de papel bond, hojas de colores", "Impresos y digitales"
)

### recursos digitales
valores_recursos_digitales  <- c(
  "Impresos y digitales", "Plataforma",
  "Internet", "Internet y aplicaciones Google Workspace, docs, sheets, slides, forms, mail, drawings, Google Drive entre otros.", 
  "Internet, libros, marcadores, almohadilla", "Internet, libros.",
  "Páginas Web y libros", "DOCUMENTOS DE LA WEB",
  "Digitales",
  "Libro de textos, acceso a Internet y medios audiovisuales"
)

### no especifica
valores_sinespecificar_recurso2  <- c(
  "Recurso ambiental o áreas verdes es el único", 
  "Todo material", "Todos", 
  "Varios", "fotocopias",
  "interación en museo y contextos fuera del salon de clase y relevantes para su desarrollo comunitario"
)

### ninguna
valores_ninguno_recurso2  <- c(
  "Por cuenta propia",
  "nada", "ni un recurso", "ninguno",
  "CONCEPTOS BASICOS", "Con recursos propios", "DIGEDUCA", 
  "Ninguno", "N/A", "NINGUNO", "Nada", "Ningun", "Ninguna",
  "Ninguna, los materiale son elaborados por mi cuenta", 
  "Ninguno", "Ninguno,se realizan las actividades con fondos y recursos propios", 
  "Ningún", "Ningúno", "No", "No existe recursos", "No me da ningún recurso", 
  "No proporciona", "Nunguno",
  "El centro Educativo no me facilita ningun material todo es por cuenta propia",
  "Información investigado por el docente"
)

### planificador
valores_planificador_recurso  <- c(
  "Planifiador del facilitador", "Planificado", 
  "Planificador", "Planificador de Ciencias Sociales",
  "Planificador del facilitador, guia de aprendizaje telesecundaria", 
  "Planificador docente", "Planificador y libro camino sin fronteras secundari Santillana", 
  "Planificadores, Hojas, copias, marcadores.", "PLANIFICADOR",
  "Planificados y guías de Telesecundaria"
)

### Recode con case match ----

df_dyd  <- df_dyd |>
mutate(
  cs_recursos_centro_educativo = case_match(
    cs_recursos_centro_educativo,
    all_of(valores_planificador_recurso) ~ "Planificador del facilitador",
    all_of(valores_ninguno_recurso2) ~ "Ninguno",
    all_of(valores_sinespecificar_recurso2) ~ "No especifica el recurso",
    all_of(valores_recursos_digitales) ~ "Recursos digitales",
    all_of(valores_hojas_impresiones) ~ "Hojas e impresiones",
    all_of(valores_guias_sociales) ~ "Guias de ciencias sociales",
    all_of(valores_folletos_recursos) ~ "Folletos",
    all_of(valores_papeleria_cs2) ~ "Material de papelería",
    all_of(valores_mobiliario_cs) ~ "Mobiliario",
    all_of(valores_equipo_tecnologia_cs) ~ "Equipo de tecnología",
    all_of(valores_cnb_cs) ~ "CNB",
    all_of(valores_audiovisual_didactico_cs) ~ "Material audiovisual y didáctico",
    all_of(valores_libro_cs) ~ "Libro",
    .default = cs_recursos_centro_educativo
    )
  )

### volver a ver errores

valores_cs_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(cs_recursos_centro_educativo))) |>
  select(cs_recursos_centro_educativo) |>
  tabyl(cs_recursos_centro_educativo)


# cs_recursos_propios ----

## ver errores

valores_cs_recursos_propios <- df_dyd |>
  filter(!(is.na(cs_recursos_propios))) |>
  select(cs_recursos_propios) |>
  tabyl(cs_recursos_propios)

dput(valores_cs_recursos_propios)

## Define los vectores ----

### equipo teconologico

valores_cs_tecnologia <- c(
  "Bocina, laptop, materiales manipulativos", "Bocinas, teléfono, Internet, videos",
  "COMPUTADORA", "COMPUTADORA, IMPRESORA, INTERNET, LIBROS, MANUALES PROPIOS PARA ELABORACIÓN DE TRAJES, EXPOSICIONES, ETC.",
  "Computador, fotocopias", "Computadora", "Computadora cañonera", 
  "Computadora de escritorio, celular, internet", "Computadora e impresora", 
  "Computadora e impresora.", "Computadora laptop , canonera", 
  "Libro, computadora", "Libros ,Lapton", 
  "Libros, computadora", "Libros,Lapton", 
  "Libros, computadora, teléfono,",
  "Copias, telefono con internet",
  "Hojas de trabajo y recursos tecnológicos", 
  "Fotocopias, Internet, computadora, impresora, celular.",
  "Proyector hojas de trabajo libros celular", 
  "Proyector, láminas", "computadora, impresora",
  "cañonera, computadora, impresora folletos y recolección de fuentes de informaciones que acorde a las competencias del CNB", 
  "computadora y hojas de trabajo", "computadora, celular, material didactico", 
  "TELEFONO", "Tablet, libros, internet, proyector", 
  "Tecnología", "Tecnológico", "Tecnológicos audiovisuales", 
  "Teléfono e internet", "Teléfono móvil, libros de texto, Constitución Política de la República de Guatemala, diccionario, marcadores,", 
  "Teléfono, computadora y libro digital", "Teléfono, computadora, marcadores, hojas Bond, otros.", 
  "Teléfono, folletos, libros, computadora",
  "Libros de texto, computadora, internet, hojas de trabajo", 
  "Computadora libros", "Computadora y bocinas", "Computadora y libros", 
  "Computadora y material de reciclaje", "Computadora y otros dispositivos móviles", 
  "Computadora y textos varios", "Computadora, Cañonera, Grabadora, Memoria usb, Cds, Indumentaria típica, ceramica, cesteria, material reciclado (vidrio, plastico, papel)", 
  "Computadora, Cañonera, internet, teléfono, plataformas digitales gratuitas, impresiones, copias.", 
  "Computadora, Internet", "Computadora, Internet e impresora", 
  "Computadora, Páginas educativas online", "Computadora, impresora", 
  "Computadora, impresora e internet", "Computadora, impresora, hojas, material didáctico, libros", 
  "Computadora, internet, libros físicos y digitales , folletos,", 
  "Computadora, internet, textos.", "Computadora, libros, hojas, marcadores, etc.", 
  "Computadora, mapas, geografía, libros de historia, revistas, CDs, Mapamundi y otros", 
  "Computadora, marcadores", "Computadora, material reciclado, hojas, marcadores lapiceros, sellos", 
  "Computadora, pizarrón, marcadores", "Computadora, tablet", 
  "Computadora, telèfono", "Computadora,teléfono y libro de texto", 
  "Computadora.. Libtos",  "Laptop, hojas de trabajo.",
  "Libros, portátil, Internet, cañonera etc.", 
  "Laptop", "Laptop e interbet", "Laptop y recursos páginas web", 
  "Equipo multimedia", "Equipó de compútación personal, teléfono celular.",
  "Herramientas tecnológicas adecuadas, técnicas de investigación y libros de texto", 
  "Minicomputadora, tablet, internet, libro", "Didácticos y Tecnológicos",
  "Pantalla, computadora", "Pc, impresiones, textos", 
  "Libros físico y digital, portátil, bocina, cañonera, internet etc.",
  "Internet, biblioteca, otros.", "Internet, computadora, impresora, fotocopia", 
  "Internet, computadora, proyector", "Internet, impresiones, revistas, prensa, laminas educativas, laptop.", 
  "Internet, libro de santillana", "Internet, páginas web", 
  "Internet, páginas web, textos,obras literarias, diccionario. Guías visuales de geografía, mapas, globo terráqueo", 
  "Internet. Impresora. Celular. Impresiones",   "Libros, Pantalla, Internet",
  "Cañonera e internet",  "Celular, bocina y globo terraqueo", "Celular, computadora otros"
  )

### material audiovisual y didáctico
valores_cs_visual_didactico_ <- c(
  "Audiovisuales  tecnológicos",  "Libro, medios auduivisuales",
  "Carteles", "Carteles hojas de trabajo", "Carteles, folletos", 
  "Carteles, folletos, imágenes, libros, CNB", "Carteles, fotocopias, recortes", 
  "Carteles, hojas de trabajo", "Carteles, lámina", "Cartulinas, Pliegos bond, Marcadores, permanentes, Laptop, impresora, Internet", 
  "Cartulinas, ilustraciones, etc.", "Cartulinas, plastilina, hojas", 
  "Celular y material didáctico", "Libro y medios audiovisuales",
  "CNB, folletos medios audiovisuales", 
  "Libros, cartulinas, hojas de trabajo, imágenes, juegos, dinámicas",
  "Libros, imágenes, audios, videos.", 
  "Copias, carteles. Libro de texto",
  "Copias, proyección de videos",
  "Hoja de investigación y libro", 
  "Hojas con los temas investigados de acuerdo al cnb  marcadores papelogragos entre otros", 
  "Hojas de actividades", "Hojas de ejercicios", "Hojas de investigaciones, proyector", 
  "Hojas de investigación", "Hojas de trabajo", "Hojas de trabajo y computadora", 
  "Hojas de trabajo y recursos tecnológicos", "Hojas de trabajo, constitución, cuaderno, carteles, Imágenes", 
  "Hojas de trabajo, marcadores", "Hojas de trabajo, pizarra, marcador, almohadilla", 
  "Hojas de trabajo, videos interactivos, dinamicas y realaciones sociales", 
  "Hojas de trabajo, videos, libros entre otros", "Hojas, fotocopias, cartulinas, videos y cartulinas", 
  "Hojas, impresiones", "Hojas, listas de Cotejo, Marcadores, Carteles", 
  "Hojas, papel, marcadores, teléfono", "INTERNET Y LIBROS DE CIENCIAS SOCIALES", 
  "Impresiones", "Impresiones de hojas de trabajo, imágenes etc", 
  "Impresiones, imágenes libros propios", "Impresora,visuales y audiovisuales", 
  "Impresos", "Recursos naturales", "Revistas hojas marcadores y otros", 
  "Revistas, videos, investigaciones",  "Jos de trabajos y recursos naturales de la comunidad",
  "MATERIAL DE RECICLAJE, LIBROS, REVISTAS, PERIÓDICOS, COMPUTADORA, CELULAR, MEDIOS ESCRITOS, MEDIOS UDIOVISUALES, MARCADORES, HOJAS DE COLRES, PAPELÓGRAFOS, COPIAS, RESÚMENES, HOJAS DE TRABAJO, AGENDA DE TRABAJO.", 
  "Mapas , libros etc.", "Mapas folletos libros", "Mapas y afiches", 
  "Mapas, dibujos, impresiones, hojas de trabajo, carteles,", 
  "Mapas, revistas, libro", "computadora, celular, material didactico", 
  "Material de capacitaciones", "Material didáctico", "Material didáctico variado", 
  "Material didáctico, computadora, libros etc", "Material didáctico, mapas y carteles", 
  "Material didáctico.", "lapto, materiales didácticos entre otros", 
  "Periodico, folletos, cartulina, hojas, lapicero. Etcétera", 
  "Pines, Cuerdas", "Valija didáctica", "Valija dodactica",
  "Material escrito y carteles", "Didácticos y Tecnológicos",
  "Dictarles y la malla curricular", "Folletos, Hojas de Trabajo, Láminas, medios digitales.",
  "Material impreso", "Material mediado", "Materiales didácticos", 
  "Materiales escritos. Audiovisuales.",  "Impresos",
  "Folletos, imágenes, videos",  "Impresora,visuales y audiovisuales", 
  "Hojas de trabajo, videos interactivos, dinamicas y realaciones sociales", 
  "Hojas de trabajo, videos, libros entre otros", "Hojas, fotocopias, cartulinas, videos y cartulinas", 
  "Folletos, libros, videos", "Folletos, presentación, juegos", 
  "Materiales reciclables y otro tipo de materiales para hacer arriates  para hacer macetas  y recursos naturales como siembra de árboles y otro tipo de plantas", 
  "Libros, recursos visuales", "Libros, revistas, etc", "Libros, revistas, guías y material didáctico propio", 
  "Libros, televisión, material didáctico", 
  "Libros, vídeos, documentales", "Otros textos, computadora, situaciones cotidianas y material didáctico", 
  "Libros, internet, cartel didáctico,",
  "Pizarra, documentos de apoyo y otros", 
  "Pizarra, periódico, documentos de apoyo",
  "Libros, material didáctico, copias, hojas de trabajo, otros", 
  "Libros, plataformas digitales y cañonera",
  "Libros, Guía visualizada, mapas, fotocopias, hojas de trabajo, cuaderno de trabajo, compendio de mapas y fichas informativas", 
  "Libros, Internet, impresiones, imágenes, mapas, bocina, folletos.", 
  "Diapositivas y folletos", "Diapositivas, material didáctico",
  "Computadora, mapas, geografía, libros de historia, revistas, CDs, Mapamundi y otros"
)

### recursos digitales
valores_digitales_cs <- c(
  "Computadora, Cañonera, internet, teléfono, plataformas digitales gratuitas, impresiones, copias.", 
  "Computadora, Páginas educativas online", "Computadora, internet, libros físicos y digitales , folletos,", 
  "Constitución, mapas, medios digitales", "Digitales",
  "PAGINAS  WEB", "Pdf", "Material digital",
  "Imvestigaciones del tema a desarrollar", "Internet", 
  "Investigacion", 
  "Investigaciones", "Investigaciones de Google", "Investigaciones en el internet", 
  "Investigaciones impresos y digitales", "Investigaciones propias", 
  "Investigaciones, audios, videos", "Investigación,  videos entre otros", 
  "Investigación, hojas impresas", "Investigo en la web y creo guías conforme al cnb", 
  "Presentación Pawer point", "Página w", "Páginas web", "Páginas web, hojas de trabajo. Videos", 
  "Páginas web, libros",  "Folletos, Hojas de Trabajo, Láminas, medios digitales.",
  "Videos , pantalla.", "Videos, audios, imágenes y textos", 
  "Viedeos cañonera", "Web", "Web, computadora, folletos, YouTube, entre otros", 
  "Webs, Libros de Ciencias Sociales, Libros de historia, Libros de Filosofía.", 
  "Recursos de la Web y herramientas virtuales"
)

### material de papeleria
valores_papeleria_cs_propio <- c(
  "Computadora, libros, hojas, marcadores, etc.", 
  "Cartulinas, Pliegos bond, Marcadores, permanentes, Laptop, impresora, Internet", 
  "Computadora, marcadores", "Computadora, material reciclado, hojas, marcadores lapiceros, sellos", 
  "Computadora, pizarrón, marcadores",
  "Maradores, temas de soporte", 
  "Hojas de trabajo, constitución, cuaderno, carteles, Imágenes", 
  "Hojas de trabajo, marcadores", "Hojas de trabajo, pizarra, marcador, almohadilla", 
  "Flash cards para mis clases.",
  "Folletos libros papel de colores pliegos de cartulinas crayones y marcadores", 
  "Folletos y hojas de trabajo",
  "folletos.", "fotocopias", "hoja de trabajo ,carteles, mapas ,mapas conceptuales dinamicas actividades dentro y fura del aula", 
  "impresiones y computadora",
  "Crayones, hojas de trabajo, etc", 
  "Cuaderno, hojas impresas",
  "Hojas, listas de Cotejo, Marcadores, Carteles", 
  "Hojas, papel, marcadores, teléfono",
  "Fotocopias, impreciones, hojas bond, recortes, planificación, asistencia, cronogramas de actividades, e internet", 
  "Asistencia, hojas de trabajo, lapicero, computadora", 
  "Reciclaje", "Recortes, hojas, internet",
  "Pizarrón, Marcadores y libro de texto", 
  "Pizzaron, marcadores, afiches e impresiones", "Plan del área, CNB del área, carteles, computadora, celular.", 
  "Marcador, almohadilla, pizzara, cartulina, videos.", "Marcadores .lapicero .cuaderno", 
  "Marcadores libros papelogragos computadora", "Marcadores, almohadilla, computadora.", 
  "Marcadores, hojas de trabajos. etc.", "Marcadores, lápices", 
  "Marcadores, material de apoyo en actividades, libros, internet.", 
  "Marcadores,cartulinas, aplicaciones tecnológicas, teléfono.etc", 
  "Libros Marcadores pizarras", "Libros de texto, videos, carteles, organizadores", 
  "Libros de texto. internet, computadora portátil lecturas de superación y valores, almohadilla, marcadores, hojas de trabajo, sello para revisar tareas.",
  "Libro de texto, celular, hojas, marcadores, etc"
)

### libros
valores_libros_propios <- c(
  "LIBRO DE CIENCIASS SOCIALES PARA TERCERO, LATPOP E INTERNET", 
  "LIBRO DE TEXTO, PAGINAS  WEB", "LIBROS DE TEXTO, TEXTOS ESPECIALIZADOS EN CIENCIAS SOCIALES", 
  "LIBROS, FOLLETOS.", "La Constitución de la República de Guatemala", 
  "La investigación", "Lecturas de otros textos", 
  "Mis libros", "Webs, Libros de Ciencias Sociales, Libros de historia, Libros de Filosofía.", 
  "Santillana, internet, folletos, cuadernillos", 
  "Susaeta", "Texto personal",  "El Internet y los libros",
  "libros", "libros y carteles", "libros y usos de tecnología", 
  "san tillana", "1 libro", "Historia", "Historia de Guatemala",
  "Texto personal, folleto a estudiantes", "Texto y folleto", 
  "Texto. Copias.", "Textos de Santillana", "Textos. Copias", 
  "Textos. Paginas de Internet.",
  "BIBLIOGRAFIA ESPECIALIZADA DE CIENCIAS SOCIALES", 
  "CIENCIAS SOCIALES",
  "Ciencias sociales", "Ciencias sociales  y ciudadanía", 
  "Copias de libro",
  "Desafíos socioeconómicos: retos y oportunidades:Aracely Martínez, Nancy L.", 
  "Libro", "Libro ,impresiones e internet", "Libro Santillana No. 8", 
  "Libro Santillana y la web", "Libro de Santillana, hojas de trabajo.", 
  "Libro de ciencias sociales y formación ciudadana", "Libro de ciencias sociales, computadora, cañonera, impresora internet", 
  "Libro de texto", "Libro de texto e Internet", "Libro de texto,  Internet, recortes,cuaderno, hojas", 
  "Libro de texto, PDF, web", "Libro de texto, celular, hojas, marcadores, etc", 
  "Libro de texto, fuentes confiables", "Libro de texto, investigación de temas en páginas web, computadora, impresiones, copias y otros.", 
  "Libro e Internet", "Libro y medios audiovisuales", "Libro, computadora", 
  "Libro, hoja de tarabajo", "Libro, hojas y mas", "Libro, información impresa del sitio web.", 
  "Libro, medios auduivisuales", "Libro.marcador", "Libros", 
  "Libros ,Lapton", "Libros Marcadores pizarras", "Libros antiguos, Internet y CNB", 
  "Libros carteles investigaciónes", "Libros de Texto, Guías.", 
  "Libros de apoyo", "Libros de ciencias sociales, guías de trabajo, hojas de actividades", 
  "Libros de distintas editoriales", "Libros de texto", "Libros de texto internet", 
  "Libros de texto variados y cnb", "Libros de texto, computadora, internet, hojas de trabajo", 
  "Libros de texto, impresiones, fotocopias, juegos.", "Libros de texto, información de fuentes confiables", 
  "Libros de texto, internet", "Libros de texto, mapas, leyes de Guatemala, impresiones y pdf de la web", 
  "Libros de texto, videos, carteles, organizadores", "Libros de texto. internet, computadora portátil lecturas de superación y valores, almohadilla, marcadores, hojas de trabajo, sello para revisar tareas.", 
  "Libros de textos", "Libros de textos, afiches", "Libros digitales, IA", 
  "Libros e internet", "Libros e investigaciones", "Libros en PDF", 
  "Libros específicos de ciencias sociales, edición santillana", 
  "Libros físico y digital, portátil, bocina, cañonera, internet etc.", 
  "Libros impresos", "Libros impresos de lectura y otros", 
  "DOCUMENTOS O VIDEOS", "De texto",
  "Distintos textos y algunos folletos recibidos en capacitaciones del ministerio de educación",
  "Enciclopedia, libros Virtuales, plataformas, material didáctico", 
  "Enciclopedias", "Enciclopedias virtuales y físicas, editorial Oceano, computadora.", 
  "Libros impresos y  digitales", "Libros imágenes ilustrativas Internet carteles", 
  "Libros internet", "Libros propios", "Libros propios, folletos", 
  "Libros que utiliza el ministerio de educación", "Libros tecnología", 
  "Libros y Computadora", "Libros y folletos", "Libros y material didáctico.", 
  "Libros, Guía visualizada, mapas, fotocopias, hojas de trabajo, cuaderno de trabajo, compendio de mapas y fichas informativas", 
  "Libros, Internet", "Libros, Internet, impresiones, imágenes, mapas, bocina, folletos.", 
  "Libros, Pantalla, Internet", "Libros, carteles y mapamundi entre otros", 
  "Libros, cartulinas, hojas de trabajo, imágenes, juegos, dinámicas", 
  "Libros, computadora", "Libros, computadora, copias, folletos", 
  "Libros, computadora, teléfono,", "Libros, diccionario, Constitución de la República de Guatemala  e internet", 
  "Libros, dispositivo móvil e Internet", "Libros, documentos en pdf, videos,", 
  "Libros, documentos pdf, paginas web", "Libros, epub, internet.", 
  "Libros, folletos impresos y entorno", "Libros, folletos, trifoliares", 
  "Libros, fotocopias, computadora", "Libros, fotocopias, internet", 
  "Libros, hojas de trabajo", "Libros, hojas y más", "Libros, hojas, marcadores, etc", 
  "Libros, imágenes, audios, videos.", "Libros, internet", 
  "Libros, internet, cartel didáctico,", "Libros, internet, hojas", 
  "Libros, links de actividades", "Libros, mapas, carteles entre otros", 
  "Libros, material didáctico, copias, hojas de trabajo, otros", 
  "Libros, plataformas digitales y cañonera", "Libros, portátil, Internet, cañonera etc.", 
  "Libros, recursos visuales", "Libros, revistas, etc", "Libros, revistas, guías y material didáctico propio", 
  "Libros, revistas, sitios web", "Libros, televisión, material didáctico", 
  "Libros, vídeos, documentales", "Libros,Lapton", "Libró, computadora, Cnb"
)


### folletos o guias
valores_folletos_guia  <-c(
  "Folleto, libros", "Folletos", "Folletos Internet", "Folletos descargados de internet.", 
  "Folletos impresos", "Folletos libros papel de colores pliegos de cartulinas crayones y marcadores", 
  "Folletos y hojas de trabajo", "Folletos, CNB,", "Folletos, Hojas de Trabajo, Láminas, medios digitales.", 
  "Folletos, hojas de trabajo", "Folletos, hojas de trabajo,", 
  "Folletos, hojas de trabajo.", "Folletos, imágenes, videos",
  "Folleto cuaderno", "Folleto y videos", "HOJAS DE TRABAJO IMPRESAS Y RESUMENES IMPRESOS DE TEMAS", 
  "GUIAS DE APRENDIZAJE", "GUIAS DE TRABAJO", "Geografías", 
  "Guías docentes", "Elaboración de folletos",
  "Folletos, libros, videos", "Folletos, presentación, juegos"
)
### mobilioarios
valores_mobiliario_propio  <-c (
  "Pizarra", "Pizarra, documentos de apoyo y otros", 
  "Pizarra, periódico, documentos de apoyo", "Pizarrón, Marcadores y libro de texto", 
  "Pizzaron, marcadores, afiches e impresiones"
)

### planificador
valores_planificador_cs  <-c(
  "Planificador", "Planificador del docente y guia de aprendizaje de Telesecundaria", 
  "Planoficador y guía cc. Sociales"
)

### ninguno
valores_ningun_recurso  <-c(
  "Ninguno", "No proporciona",  "investigo los temas",
  "."
)

### cnb
valores_cnb_propio  <- c(
  "CNB", "CNB y del internet",     "CNB, folletos, imágenes medios audiovisuales",
  "CNB, investigaciones propias",  "Folletos, CNB,",
  "Currículo Nacional Base CNB de Guatema y libros digitales PDF"
)

### no especifica
valores_noespecifica_recurso_cs  <-c(
  "Muchos",  "Propios", "Propios santillana",
  "Recursos humanos de la comunidad",  "Varios",  "Económicos", 
  "Diferentes"
)

### Recode con case match ----

df_dyd  <- df_dyd |>
  mutate(cs_recursos_propios = case_match(
    cs_recursos_propios,
    all_of(valores_noespecifica_recurso_cs) ~ "No especifica el recurso",
    all_of(valores_cnb_propio) ~ "CNB",
    all_of(valores_ningun_recurso) ~ "Ninguno",
    all_of(valores_planificador_cs) ~ "Planificador",
    all_of(valores_mobiliario_propio) ~ "Mobiliario",
    all_of(valores_folletos_guia) ~ "Guías y folletos",
    all_of(valores_libros_propios) ~ "Libros",
    all_of(valores_papeleria_cs_propio) ~ "Material de papelería",
    all_of(valores_digitales_cs) ~ "Recursos digitales",
    all_of(valores_cs_visual_didactico_) ~ "Material audiviosual y didáctico",
    all_of(valores_cs_tecnologia) ~ "Equipo tecnológico",
    .default = cs_recursos_propios
  )
    
  )

### volver a ver errores

valores_cs_recursos_propios <- df_dyd |>
  filter(!(is.na(cs_recursos_propios))) |>
  select(cs_recursos_propios) |>
  tabyl(cs_recursos_propios)


# cs_otro_ultima_vez_libros_mineduc ----

## ver errores

valores_cs_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(cs_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(cs_otro_ultima_vez_libros_mineduc ))) |>
  select(cs_otro_ultima_vez_libros_mineduc) |>
  tabyl(cs_otro_ultima_vez_libros_mineduc)

# View(valores_cs_otro_ultima_vez_libros_mineduc)

## Define los vectores ----

### nunca
valores_nunca_cs <-c(
  "0","N/A", "NO HE RECIBIDO", "Ninguno", "No",
  "No he recibido", "No he recibido libro",
  "No hemos recibido", "No hemos rwcibido libros",
  "No se ha recibido ningún material sobre esta área",
  "No se ha recibido.", "No se ha recido", "No se han recibido libros",
  "Nunca"
) 

### 2010
valores_cs_2010 <-c(
  "2010"
)

### 2014
valores_cs_2014 <-c(
  "2014"
)

### 2016
valores_cs_2016 <-c(
  "2016"
)

### 2017
valores_cs_2017 <-c(
  "2017"
)

### 2018
valores_cs_2018 <-c(
  "2018", "Planificador 2018"
)

### valores 2019
valores_cs_2019 <-c(
  "2019"
)

### valores 2020
valores_cs_2020 <-c(
  "2020", "2020 aproximadamente"
)

### no recuerda
valores_cs_norecuerda <-c(
  "No recuerdo"
)

## Recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    cs_otro_ultima_vez_libros_mineduc = case_match(
      cs_otro_ultima_vez_libros_mineduc,
      all_of(valores_cs_norecuerda) ~ "No recuerda",
      all_of(valores_cs_2020) ~ "2020",
      all_of(valores_cs_2019) ~ "2019",
      all_of(valores_cs_2018) ~ "2018",
      all_of(valores_cs_2017) ~ "2017",
      all_of(valores_cs_2016) ~ "2016",
      all_of(valores_cs_2014) ~ "2014",
      all_of(valores_cs_2010) ~ "2010",
      all_of(valores_nunca_cs) ~ "Nunca ha recibido",
      .default = cs_otro_ultima_vez_libros_mineduc
    )
  )

## volver a ver errores

valores_cs_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(cs_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(cs_otro_ultima_vez_libros_mineduc ))) |>
  select(cs_otro_ultima_vez_libros_mineduc) |>
  tabyl(cs_otro_ultima_vez_libros_mineduc)

# cs_tipo_materiales_recibidos ----

## ver errrores

valores_cs_tipo_materiales_recibidos <- df_dyd |>
  filter(cs_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(cs_tipo_materiales_recibidos))) |>
  select(cs_tipo_materiales_recibidos) |>
  tabyl(cs_tipo_materiales_recibidos)

# View(valores_cs_tipo_materiales_recibidos)

## Define los vectores ----

### libro de ciencias sociales
valores_libros_cs <- c(
  "Ciencias Sociales", "Libros"
)

### guia y planificador
valores_guia_plani_cs <-c(
  "Guia y planificador  cc. Sociales",
  "Guias", "Guias y palnificador"
)
## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cs_tipo_materiales_recibidos = case_match(
      cs_tipo_materiales_recibidos,
      all_of(valores_guia_plani_cs) ~ "Guía y planificador para el facilitador",
      all_of(valores_libros_cs) ~ "Libro de Ciencias Sociales",
      .default = cs_tipo_materiales_recibidos
    )
  )

## volver a ver errores

valores_cs_tipo_materiales_recibidos <- df_dyd |>
  filter(cs_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(cs_tipo_materiales_recibidos))) |>
  select(cs_tipo_materiales_recibidos) |>
  tabyl(cs_tipo_materiales_recibidos)


# cl_libros_primero ----
## ver errores
valores_cl_libros_primero <- df_dyd |>
  filter(cl_uso_libros == "Sí") |>
  filter(cl_grados == "Primer grado") |>
  filter(!(is.na(cl_libros_primero))) |>
  select(cl_libros_primero) |>
  tabyl(cl_libros_primero)

dput(valores_cl_libros_primero)

## Define los vectores ----

### comunicación y lenguaje
valores_comu_lenguaje <- c(
  "Comunicación y Lenguaje", "Susaeta comunicación y lenguaje"
)

### comunicación y lenguaje 1
valores_comu_lenguaje_1 <- c(
  "Comunicación y Lenguaje 1\" de la Editorial Santillana.",
  "Comunicación y lenguaje 1, IGER"
)

### guia de aprendizaje y planificador
valores_guia_plani_cl <- c(
  "GUIA DE APRENDIZAJE Y PLANIFICADOR", "GUIAS DE APRENDIZAJE, LIBROS EDITORIAL SANTILLANA", 
  "Guia de Aprendizaje y Planificador", "Guia de aprendizaje", 
  "Guia y planificador", "Guia y planificador del curso", "Guía comunicación  y lenguaje 1", 
  "Guía de aprendizaje", "Guía de comunicación y Lenguaje", 
  "Guía de comuniciación y lenguaje de primero básico", 
  "Guía del docente. Primero Básico. Plan Internacional",
  "Guía metodológica de comunicación y lenguaje y libros de santillana de primero basico", 
  "Guía y planificadores", "Guías de aprendizaje y planificador docente",
  "Planificador", "Planificador del facilitador y Guia de Aprendizaje 1", 
  "Planificador docente"
)

### integrado 7
valores_integrado_7  <-c(
  "Integrado 7 Comunicación y Lenguaje"
)

### comunicación y lenguaje conceptos básicos
valores_conceptos_cl <- c(
  "Conceptos", "Conceptos básicos"
)

### no especifica
valores_noespecifica_lenguaje <-c(
  "DIGEDUCA", "Libros de texto de susaeta", 
  "Mentor interactivo", "Santillana,  Norma, Salguero", "Susaeta"
)
 
## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cl_libros_primero = case_match(
      cl_libros_primero,
      all_of(valores_noespecifica_lenguaje) ~ "No especifica el nombre",
      all_of(valores_conceptos_cl) ~ "Comunicación y Lenguaje, conceptos básicos",
      all_of(valores_integrado_7) ~ "Integrado 7",
      all_of(valores_guia_plani_cl) ~ "Guía y planificador de facilitador",
      all_of(valores_comu_lenguaje_1) ~ "Comunicación y Lenguaje 1",
      all_of(valores_comu_lenguaje) ~ "Comunicación y Lenguaje",
      .default = cl_libros_primero
    )
  )

## volver a ver errores

valores_cl_libros_primero <- df_dyd |>
  filter(cl_uso_libros == "Sí") |>
  filter(cl_grados == "Primer grado") |>
  filter(!(is.na(cl_libros_primero))) |>
  select(cl_libros_primero) |>
  tabyl(cl_libros_primero)

# View(valores_cl_libros_primero)

# cl_libros_segundo ----

## ver errores

valores_cl_libros_segundo <- df_dyd |>
  filter(cl_uso_libros == "Sí") |>
  filter(cl_grados == "Segundo grado") |>
  filter(!(is.na(cl_libros_segundo))) |>
  select(cl_libros_segundo) |>
  tabyl(cl_libros_segundo)

dput(valores_cl_libros_segundo)

## Define los vectores ----

### comunicación y lenguaje 2do basico
valores_segundo_cl <- c(
  "Comunicación y Lenguaje segundo básico. Libros de textos adicionales y externos."
)

### comunicación y lenguaje
valores_comu_lenguaje2 <- c(
  "Comunicación y lenguaje idioma español"
)

### comunicación l1 2
valores_l1 <- c(
  "Comunicación. L1 2"
)

### páginas web
valores_web_cl <- c(
  "Google", "Páginas web"
)

### conceptos basicos
valores_conceptos_cl2 <- c(
  "Conceptos Básicos."
)

### guia de aprendizaje
valores_guia_aprendizaje_cl <- c(
  "Guia de aprendizaje comunicación y lenguaje", 
  "Guía - Comunicación y Lenguaje 2 Idioma Español", "Guía de Comunicación y Lenguaje y Planificador", 
  "Guía de aprendizaje 2 y Planificador del docente", "Guía de aprendizaje 2019", 
  "Guía de aprendizaje y Enciclopedia de la Educación", "Guía de aprendizaje, Editora Educativa",
  "Planificador y guias de aprendizaje", 
  "Planificador y guías de aprendizaje"
)

### no especifica nombre
valores_noespecifica_lenguaje2 <- c(
  "Libro de IGER y los de Telesecundaria"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cl_libros_segundo = case_match(
      cl_libros_segundo,
      all_of(valores_noespecifica_lenguaje2) ~ "No especifica el nombre",
      all_of(valores_guia_aprendizaje_cl) ~ "Guía de aprendizaje y planificador",
      all_of(valores_conceptos_cl2) ~ "Comunicación y Lenguaje, conceptos básicos",
      all_of(valores_web_cl) ~ "Usa páginas web",
      all_of(valores_l1) ~ "Comunicación y Lenguaje L1 2",
      all_of(valores_comu_lenguaje2) ~ "Comunicación y Lenguaje",
      all_of(valores_segundo_cl) ~ "Comunicación y Lenguaje. Segundo básico",
      .default = cl_libros_segundo
    )
  )

## volver a ver errores

valores_cl_libros_segundo <- df_dyd |>
  filter(cl_uso_libros == "Sí") |>
  filter(cl_grados == "Segundo grado") |>
  filter(!(is.na(cl_libros_segundo))) |>
  select(cl_libros_segundo) |>
  tabyl(cl_libros_segundo)

# View(valores_cl_libros_segundo)


# cl_libros_tercero ----

## ver errores

valores_cl_libros_tercero <- df_dyd |>
  filter(cl_uso_libros == "Sí") |>
  filter(cl_grados == "Tercer grado") |>
  filter(!(is.na(cl_libros_tercero))) |>
  select(cl_libros_tercero) |>
  tabyl(cl_libros_tercero)

dput(valores_cl_libros_tercero)

## Define los vectores ----

###  comunicacion y lenguaje

valores_comu_lenguaje3 <- c(
  "Comunicación y Lenguaje (Ejercicios Unificados)",
  "Libro de Comunicación y Lenguaje"
)

### guia de aprendizaje
valores_guia_aprendizaje_cl2 <- c(
  "Guía de Aprendizaje de Comunicación y Lenguaje, Idioma Español", 
  "Guía de aprendizaje", "Guía de aprendizaje comunicación y lenguaje", 
  "Guía de comunicación y lenguaje 3",
  "Planificador del facilitador,guia de aprendizaje", 
  "Planificador docente y guías de aprendizaje", "Planificador, Guias de Aprendizaje del estudiante",
  "Libro guía y planificador"
)

### mineduc cyl
valores_Lenguaje_mineduc <- c(
  "Idioma Español por parte del MINEDUC"
)

### integrado 9
valores_integrado_9_cl <- c(
  "susaeta integrado 9"
) 

### no especifica
valores_noespecifica_lenguaje3 <- c(
  "Libros digitales", 
  "Santilla", "Santillana", "Susaeta, libros de texto", 
  "Textos para estudiantes"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cl_libros_tercero = case_match(
      cl_libros_tercero,
      all_of(valores_noespecifica_lenguaje3) ~ "No especifica el nombre",
      all_of(valores_integrado_9_cl) ~ "Integrado 9",
      all_of(valores_Lenguaje_mineduc) ~ "Comunicación y Lenguaje de MINEDUC",
      all_of(valores_guia_aprendizaje_cl2) ~ "Guía de aprendizaje y planificador",
      all_of(valores_comu_lenguaje3) ~ "Comunicación y Lenguaje",
      .default = cl_libros_tercero
    )
  )

## volver a ver errores

valores_cl_libros_tercero <- df_dyd |>
  filter(cl_uso_libros == "Sí") |>
  filter(cl_grados == "Tercer grado") |>
  filter(!(is.na(cl_libros_tercero))) |>
  select(cl_libros_tercero) |>
  tabyl(cl_libros_tercero)

# View(valores_cl_libros_tercero)

# cl_recursos_centro_educativo ----

## ver errores

valores_cl_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(cl_recursos_centro_educativo))) |>
  select(cl_recursos_centro_educativo) |>
  tabyl(cl_recursos_centro_educativo)

dput(valores_cl_recursos_centro_educativo)

## Define los vectores ----

### libros
valores_libros_cl <- c(
  "1 libro", "AULA, LIBROS", "Algunos textos",
  "Algunos libros",  "Comunicación y Lenguaje",
  "De lectura",  "Ejemplar de un libro",
  "Santillana", "Solo libro",
  "Texto de Comunicación y Lenguaje", 
  "Textos", "Textos, libros", "Un libro",
  "libro", "libro de texto", "textos", 
  "libros y guías para docentes", "libros, cnb, internet", 
  "libros, internet, cnb", "literatura",
  "Integrado susaeta", "Recueros materiales como libros", 
  "Libras guias ynplanificador", "Libre de Comunicación y Lenguaje", 
  "Libro", "Libro Comunicación y Lenguaje Idioma Español", 
  "Libro Mineduc", "Libro Sabtillanana y páginas del MINEDUC", 
  "Libro de Comunicación y Lenguaje", "Libro de Comunicación y Lenguaje 7, fotocopias e impresiones", 
  "Libro de Texto", "Libro de Texto Comunicación y Lenguaje 2", 
  "Libro de Texto brindado por el MINEDUC", "Libro de comunicacion y lenguaje", 
  "Libro de comunicación y lenguaje", "Libro de docente comunicación y lenguaje 1", 
  "Libro de la Gramatica, tres libros de cada nivel", "Libro de texto", 
  "Libro de texto de susaeta 7", "Libro de texto internet", 
  "Libro de texto y de lectura", "Libro de texto y equipo audiovisual, herramientas tecnológicas", 
  "Libro de texto, Plataforma Digital", "Libro de texto, cañonera, televisores y bocina.", 
  "Libro de texto, fotocopias, valija didáctica, cañonera", 
  "Libro de texto, valija didáctica", "Libro de textos y valija didáctica.", 
  "Libro guía y planificador", "Libro y Planificador", "Libro, Internet, equipo de computación.", 
  "Libro, televisión, plataforma", "Libro: Guía de Comunicación y Lenguaje, Idioma Español 3.", 
  "Libros", "Libros anteriores y guías", "Libros de COED", 
  "Pdf", 
  "Libros de Docente", "Libros de Lectura", "Libros de Texto. Plataforma Digital", 
  "Libros de Textos", "Libros de actividades prácticas y libros de ortocaligrafía.", 
  "Libros de cuentos, de los cuales ya solo se seleccionan temas para contextualizar, además de libros de comunicación, los cuales no son contextualizar, por lo que como Docente selecciono el contenido apto y contextualizar la información", 
  "Libros de diversas lecturas", "Libros de lectirura", "Libros de lectura", 
  "Libros de lenguaje y comunicación mas actualizada", "Libros de texto", 
  "Libros de texto de Editora Educativa", "Libros de texto lecturas carteles", 
  "Libros de texto y documentos para consultar.", "Libros de texto, Santillana", 
  "Libros de texto, en físico o impresos", "Libros de texto, folletos", 
  "Libros de texto, guias de obras, hojas, marcadores, cañoneras y internet.", 
  "Libros de texto, internet, televisión", "Libros de texto, televisores y computadoras", 
  "Libros de textos", "Libros de textos para estudiantes, libros de cuentos", 
  "Libros de textos, revisas, etc", "Libros de textto", "Libros del Mineduc", 
  "Libros del Ministerio de Educación  Antiguos", "Libros guia", 
  "Libros pero reutilizado, porque el Ministerio no ha enviado nuevos", 
  "Libros propios y folletos", "Libros proporcionados por el ministerio", 
  "Libros que dió el Ministerio de Educación", "Libros se texto de otras editoriales", 
  "Libros y CNB", "Libros y computadora", "Libros y guías", 
  "Libros y material didáctico", "Libros y materiales", "Libros y plataforma", 
  "Libros y todo lo necesario", "Libros, CNB, guía del doc., hojas de papel, impresiones, fotocopias, material de oficina, entre otros.", 
  "Libros, copias", "Libros, folletos, digital", "Libros, folletos, fotocopias, impresiones, marcadores", 
  "Libros, hojas de papel, marcadores, material de oficina, fotocopias, impresiones, entre otros.", 
  "Libros, hojas de trabajo", "Libros, internet para poder verificar el CNB cuando lo necesite", 
  "Libros, pizarrón", "Libros, páginas webs, folletos, internet, material audiovisual", 
  "Libros, temas gramaticales", "Libros, valija didáctica", 
  "Libros, videos, imágenes, fotos.",
  "Jtextos", "LIBRO DE TEXTO", 
  "LIBROS, FOLLETOS", "LIBROS, FOLLETOS, DOCUMENTOS EN PDF", 
  "Hojas de Lectura, Obras de autores de Guatemala, hispanoamericananas y universales, hojas impresas de lecturas, libros de texto y otros", 
  "El libro que envió el ministerio de educación.",
  "Cañonera , internet, libros de texto variados",
  "COMUNICACIÓN Y LENGUAJE L-2", "CONCEPTOS BASICOS"
)

### materiales audiviosuales y didácticos
valores_audiovisual_didactico_cl <- c(
  "Algunos materiales didácticos",
  "Cocina usb  material didáctico",
  "Copias,material didáctico",
  "Libros, valija didáctica",
  "Material didactico", "Valija Didáctica", "Valija didáctica",
  "Material didáctico", "Material didáctico.", "Material impreso", 
  "Materiales", "Materiales Escolares, papeles, marcadores", 
  "Mochila didactica",  "Perinola, cañonera, Audio, ruleta, dado",
  "Libros, páginas webs, folletos, internet, material audiovisual",
  "Libro de texto, valija didáctica", "Libro de textos y valija didáctica.",
  "Libro de texto, fotocopias, valija didáctica, cañonera", 
  "Lecturas, juegos, libros, Internet, otros",
  "material Didáctico", 
  "material didáctico", "Únicamente valija didáctica",
  "La valija didáctica, laboratorio de computación", 
  "Guía de aprendizaje, planificadores, valija didáctica", 
  "Cañonera, computadora, pizarrón, material didáctico", 
  "Audiovisuales", "Balija didactica", "Balija didáctica"
)

### cnb
valores_cnb_cl <- c(
  "CNB", "CNB, PLANIFICADOR y GUÍAS DE TEXTO", 
  "CNB, libros y paginas web", "Cmb", "Currículo digital libros de texto"
)

### equipo tecnológico
valores_tecnologia_cl <- c(
  "Cañonera", "Cañonera , internet, libros de texto variados", 
  "Cañonera, computadora de escritorio, rincón de lectura con libros varios como novelas, cuentos, biografías, dados de lectura, fichas", 
  "Cañonera, computadora, pizarrón, material didáctico", 
  "Cañonera, folleto de trabajo, libro de lectura", "Cañonera, pliego de bond", 
  "Centro de computación",  "La valija didáctica, laboratorio de computación", 
  "La Tecnologia y libros",
  "cañonera", 
  "pizarrón, cañonera, televisor, bocinas, computadoras y libros de texto.", 
  "proyector, bocina, pizarrón, marcadores, papel de colores, televisor.", 
  "computadoras pizarron papel almohadilla",
  "Televisor, libros, internet",
  "Proyector, audios", "Proyector, pantalla digital.", "Proyector, pantallas digitales, valija didáctica.", 
  "Proyectores., videos.", 
  "Pantalla, Impresora", "Pantalla, pizarra", 
  "Marcadores, pizarrón, computadora e impresora", 
  "Libros de texto, internet, televisión", "Libros de texto, televisores y computadoras", 
  "Libros de Texto. Plataforma Digital",
  "Libro, Internet, equipo de computación.", 
  "Libro, televisión, plataforma",
  "Libro de texto, fotocopias, valija didáctica, cañonera", 
  "Libro de texto, cañonera, televisores y bocina.", 
  "Libro de texto y equipo audiovisual, herramientas tecnológicas", 
  "Impresora, cañonera y pizarra", "Impresora, hojas.",
  "Computadora, cañonera y plataformas digitales.", "Computadora, laptop, cañonera, libro de curso", 
  "Computadora, proyector y libros de textos."
  
)

### recursos digitales
valores_recurso_digital_cl <- c(
  "DOCUMENTOS DE LA WEB",  "Digital, pozarron", "Impresos y digitales",
  "Digitales y en fisico", "El aula virtual y guías pedagógicas",
  "Internet",  "Libros, videos, imágenes, fotos.",
  "Libro de texto, Plataforma Digital",
  "Internet, libros disponibles", "Investigaciones", "Investigaciones  e impresiones  entre otros", 
  "Investigación en Internet",  "Página w"
)

### folletos y guias de aprendizaje
valores_folleto_guia_cl <- c(
  "Folleto", 
  "Folletos impresos", "Folletos y obras literararias", "Fotocopias", 
  "GUIAS DE APRENDIZAJE", "GUÍA DE APRENDIZAJE Y PLANIFICADOR",
  "Solo 5 guias de aprendizaje mineduc",
  "Guia", "Guia de aprendizaje comunicación y lenguaje", "Guia de ciencias.", 
  "Guia de comunicación y lenguaje, libros de lectura para estudiantes, marcadores, tijera, papel, pegamento, hojas Bond, impresiones", 
  "Guia y planificador", "Guía de Aprendizaje", "Guía de aprendizaje", 
  "Guía de aprendizaje, planificadores, valija didáctica", 
  "Hojas de trabajo", "Hojas de trabajo, imágenes, etc", "Hojas impresas", 
  "Guía y planificador", "Guía y planificadores", "Guías de aprendizaje", 
  "Guías de aprendizajes", "Guías de comunicación y lenguaje", 
  "Guías metodológicas.", "Guías, planificadores"
  
)

### material de papeleria
valores_papeleria_cl <- c(
  "Guia de comunicación y lenguaje, libros de lectura para estudiantes, marcadores, tijera, papel, pegamento, hojas Bond, impresiones",
  "Hojas bond, pliego manila, marcadores permanentes, maskintape, selladores, tinta de marcadores, tijera, grapadora, impresiones, entre otros.", 
  "Hojas papel bon", "Hojas, libros,  marcadores.", "Hojas,libro, pizarrón,", 
  "Humanos. Hojas, marcadores, pizarron", "Impresiones", "Impresiones en hoja de trabajo",
  "Cañonera, pliego de bond", "Impresos", 
  "proyector, bocina, pizarrón, marcadores, papel de colores, televisor.",
  "Pizarra, marcadores, almohadilla, impresiones, tinta, lapiceros", 
  "Pizarrón, almohadilla, marcadores, material impreso, goma, tijeras, crayones, pliegos de papel de colores, grabadora, hojas bond.", 
  "Libros, folletos, fotocopias, impresiones, marcadores", 
  "Libros, hojas de papel, marcadores, material de oficina, fotocopias, impresiones, entre otros.",
  "Libros de texto, guias de obras, hojas, marcadores, cañoneras y internet.", 
  "Lapiceros, marcadores y otros",  "Ojas ,marcadores",
  "Marcador, almohadilla, pizzara, cartulina, videos.", "Marcadores tinta papel", 
  "Marcadores y cartulinas", "Marcadores y cañonera", "Marcadores, pizarrón, computadora e impresora", 
  "Marcadores, tinta, papel", "Marcadores, tinta, papel, cañonenra", 
  "Impresos y digitales",
  "Un cuaderno, lápices,lapiceros,marcadores,almohadilla,tinta,cañonera"
  
)

### ninguno
valores_ninguno_cl <- c(
  "Nada", "Ninguna", "Ninguno", "Ninguno, yo lo buscamos de forma virtual", 
  "Ningún", "No hay, todo es propio", "No imparto comunicación y lenguaje", 
  "No recibo",   "Recursos propios", "ninguno", "0"
)

### mobiligario
valores_mobiliario_cl <- c(
  "Pizarra, marcadores, almohadilla, impresiones, tinta, lapiceros", 
  "Pizarra, marcadores, almohadillas, copias, impresiones, tinta para marcadores, lapiceros", 
  "Pizarrón marcador, almohadilla", "Pizarrón, almohadilla, marcadores, material impreso, goma, tijeras, crayones, pliegos de papel de colores, grabadora, hojas bond.", 
  "Pizarrón, hojas de trabajo propias"
)

### planificador del facilitador
valores_planificador_cl <- c(
  "Planificador", "Planificador de comunicación y lenguaje 2", 
    "Planificador docente y guías de aprendizaje", "Planificador y guia", 
    "Planificador y guia de aprendizaje", "Planificador y guias de aprendizaje de Telesecundaria", 
    "Planificador y guía", "Planificadores y guías.", "Planificados del docente y guías de aprendizaje de Talesecundaria"
)

### no especifica
valores_noespecifica_recurso_cl <- c(
  "Lo indispensable",  "Atenea", "COMCESP", 
  "Material de apoyo de la DIGECUR y recursos propios"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cl_recursos_centro_educativo = case_match(
      cl_recursos_centro_educativo,
      all_of(valores_noespecifica_recurso_cl) ~ "No especifica el recurso",
      all_of(valores_planificador_cl) ~ "Planificador para el facilitador",
      all_of(valores_mobiliario_cl) ~ "Mobiliario",
      all_of(valores_ninguno_cl) ~ "Ninguno",
      all_of(valores_papeleria_cl) ~ "Material de papelería",
      all_of(valores_folleto_guia_cl) ~ "Guías y folletos",
      all_of(valores_recurso_digital_cl) ~ "Recursos digitales",
      all_of(valores_tecnologia_cl) ~ "Equipo tecnológico",
      all_of(valores_cnb_cl) ~ "CNB",
      all_of(valores_audiovisual_didactico_cl) ~ "Material audiovisual y didáctico",
      all_of(valores_libros_cl) ~ "Libros",
      .default = cl_recursos_centro_educativo
    )
  )

## volver a ver errores

valores_cl_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(cl_recursos_centro_educativo))) |>
  select(cl_recursos_centro_educativo) |>
  tabyl(cl_recursos_centro_educativo)

# View(valores_cl_recursos_centro_educativo)

# cl_recursos_propios ----

## ver errores

valores_cl_recursos_propios <- df_dyd |>
  filter(!(is.na(cl_recursos_propios))) |>
  select(cl_recursos_propios) |>
  tabyl(cl_recursos_propios)

dput(valores_cl_recursos_propios)

## Define los vectores ----

### material audiovisual y didáctico
valores_audio_didactico_cl <- c(
  "Audiovisual o Magistral",
  "Computadora, cañonera, material didáctico, hojas impresas.", 
  "Computadora, cañonera, plataformas digitales, material didáctico", 
  "Computadora, hojas de trabajo", "E-grafias.",
  "Computadora y material didáctico",
  "Libros y materiales",
  "Manualidades", "material didáctico .",
  "Libros, tiras didácticas, dinámicas interactivas.", 
  "Libros, internet, cartel didáctico", "Libros, internet, hojas, carteles",
  "Libros de lectura y textos literarios, cañonera, internet, fichas , hojas variadas",
  "Hojas de trabajo, libros de texto,recursos didácticos",
  "Folletos CNB imagenes medios audivisuales",
  "Didáctico,", "Documentos impresos",
  "Juegos de mesa, Scrabble, diccionarios, rompecabezas, tarjetas, Jenga, palillos chinos, computadora, libros de lectura, marcadores, cartulinas, pliegos de papel y cartulina, dados, domino, juego adivina quien soy", 
  "Impresora, hojas de colores, hojas de trabajos", "Impresos", 
  "Imágenes, hojas de trabajo, etc", 
  "Libros, marcadores, computadora, material didáctico",
  "Libros de texto, impresiones, material didáctico elaboración propia", 
  "Libros de texto, internet, material propio, aplicaciones y plataformas",
  "Lenguaje mímico papelografo", "Libro de textos, tiras didácticas",
  "Impresiones", "Impresiones, dibujos", "Revistas, periódicos, Internet", 
  "Recortes libros de texto y el internet", 
  "Folletos material didáctico marcadores crayones",
  "Cuadernillos, libros de lectura", "Cuaderno de ejercicios facilitado por el MINEDUC e información recabada en la red", 
  "Diapositivas, comprensión lectora, folletos",
  "Copias", "Cuadernillo unificado, hojas", 
  "Libros de texto, herramientas digitales, actividades lúdicas, proyectos de aula, aprendizaje Colaborativo, dramatizaciones, poesías corales entre otros.", 
  "Folletos, CNB imágenes, marcadores, medios audiovisuales", 
  "Carteles, caja de palabras,  tarjetas de personajes, de lectura, libros de poemas", 
  "Carteles, hojas de trabajo, lista de cotejo", 
  "Folletos. Presentaciones de PowerPoint",
  "lapto, cuaderno de asistencia, materiales didácticos entre otros",
  "Material audiovisual", "Material didactico", 
  "Material didáctico", "Material didáctico variado", "Material didáctico y recursos tecnológicos", 
  "Material didáctico, fotocopias, hojas de trabajo- ejercicios", 
  "Material didáctico, hojas e impresiones", "Material didáctico,marcadores, libros", 
  "Material fisico y audio visual", "Material mediado", "Materiales concretos CNB impreso y otras bibliografías", 
  "Materiales propios",
  "Cartulinas, papelografos, videos,  entre otros", 
  "CNB, folletos imágenes medio audiovisuales"
)

### equipo tecnológico
valores_tecnologico_cl <- c(
  "Bocinas, proyector, láminas",
  "COMPUTADORA , LIBROS Y GUIAS DE CONSULTA",
  "Folletos y recursos tecnologicos",
  "INTERNET, LIBROS DE COMUNICACIÓN", 
  "Impresora, cañonera y pizarra", 
  "tablet", "triptico",
  "Videos y pantalla.", "Videos, música, lectura, exposiciones, ejemplificaciones, dramatizaciones,", 
  "Libros, impresiones, fotocopias, cel., laptop, material de oficina, entre otros.", 
  "Libros, computador", "Libros, computadora, cañonera, impresora.", 
  "Libros, computadora, impresora, marcadores, pliegos de papel, impresiones, carteles, infografias", 
  "Libros, computadora, impresora, tinta", 
  "Libros de lectura y textos literarios, cañonera, internet, fichas , hojas variadas",
  "Libro, computadora, Internet e impresora",
  "Internet", "Internet  e impresiones",
  "Libro de texto, teléfono, aplicaciones tecnológicas", 
  "Internet, celular, computadora, impresora. Hojas de actividades y actividades en línea", 
  "Internet, computadoras, impresoras, libros de texto", "Internet, libros", 
  "Internet, libros de texto, computadora", "Internet, libros digitales, CNB", 
  "Internet, láptop, hojas impresas", "Internet, páginas web", 
  "Internet.", "LATOP E INTERNET",
  "Libros de textos, computadora", 
  "Libros. Computadora, impresiones, tinta. Impresora", 
  "Libros de textos, computadora, celular, rompecabezas, hojas de papel bond, marcadores, impresora.",
  "Laptop y celular", "Laptop, internet, impresiones.", 
  "Laptop, plataforma, internet, folletos de ortografía y caligrafía, obras literarias y manuales de comprensión lectora.", 
  "Impresora, hojas de colores, hojas de trabajos", 
  "Herramientas tecnológicas, carteles libros de texto",
  "Diccionario, computadora. Canonera, impresiones, ¹",
  "Libros, folletos, televisión, internet",
  "Libros, marcadores, computadora, material didáctico",
  "Hojas bond, libro de texto, computadora, cañonera, bocina, materiales de impresiones, entre otros.", 
  "Computadora", "Computadora , impresora", "Computadora e impresora", 
  "Computadora libros digitales y de texto", "Computadora texto", 
  "Computadora y libro de texto", "Computadora y libros de texto para presentaciones", 
  "Computadora y material didáctico", "Computadora y otros", "Computadora,", 
  "Computadora, Internet", "Computadora, Internet personal, libros disponibles en mi biblioteca", 
  "Computadora, Internet.", "Computadora, cañonera, material didáctico, hojas impresas.", 
  "Computadora, cañonera, plataformas digitales, material didáctico", 
  "Computadora, hojas de trabajo", "Computadora, impresiones.", 
  "Mi internet", "Minicomputadora, tablet, internet",
  "Pantalla, computadoras.", 
  "Pc, hojas de trabajo",
  "impresora, impresiones,  internet, carteles, hojas de trabajo, videos, hojas y libros.", 
  "internet, computadora. hojas impresas", "lapto, cuaderno de asistencia, materiales didácticos entre otros", 
  "computadora", "computadora, impresora, papel bond y materiales repetitivos y otros", 
  "TELEFONO CELULAR", "Tecnología, textos literarios, diccionarios en físico, carteles, obra literaria del Rinoceronte de Alexander Scott", 
  "Tecnológicos de gamificación", "Teléfono, internet libros digitales", 
  "Teléfono, tablet, internet",
  "Computadora, impresora", "Computadora, impresora, bocina, celular, laptop, impresiones, material de oficina, entre otros.", 
  "Computadora, impresora, lapicero, diccionario", "Computadora, impresora, libros de textos, hojas de papel bond, marcadores.", 
  "Computadora, internet, cañonera, impresora, marcadores", "Computadora, libro de texto, USB, hojas, lapiceros, material reciclado, sellos", 
  "Computadora, libros de lectura, diccionarios.", "Computadora, libros de texto", 
  "Computadora, libros de texto, libros de lectura, proyector", 
  "Computadora, libros, cnb, celular", "Computadora, libros, hojas de trabajo", 
  "Computadora, libros, hojas, recursos naturales.", "Computadora, obras de autores, cartulinas y otros", 
  "Computadora, papelógrafo, marcadores", "Computadora, telefono,Internet, libros digitales de Santillana y algunos del ministerio", 
  "Computadora, televisión, internet", "Equipo de Computo",
  "CAÑONERA, COMPUTADORA, IMPRESORA Y LIBROS"
)

### cnb
valores_cnb_cl_propio <- c(
  "CNB", "CNB Y libro Comunicación y Lenguaje Idioma Español", 
  "Cnb", "Temas de cnb investigarlos e imprimir", 
  "CNB de Guatemala y libros digitales", "CNB en digital", "CNB, folletos imágenes medio audiovisuales", 
  "CNB, folletos, imágenes, carteles y medios audiovisuales"
  
)

### material de papeleria
valores_papeleria_cl_propio <- c(
  "Computadora, libro de texto, USB, hojas, lapiceros, material reciclado, sellos",
  "Computadora, papelógrafo, marcadores", 
  "Marcador, almohadilla, pizzara, cartulina, videos.", 
  "Marcador, cuaderno de asistencia  hoja impreso de lecturas, folletos, hojas de papel bond, mi plan.", 
  "Marcadores y libros", "Marcadores y planificadores", "Marcadores, lapiceros y hojas bond", 
  "Marcadores,investigaciones,",
  "computadora, impresora, papel bond y materiales repetitivos y otros", 
  "Sellos, almohadilla con tinta, lapiz, lapicero, hojas bond, papel de colores, dulces para premiar participación, dibujos, plastico para forrar libros, libros que he comprado para ampliar contenido, computadora, USB.", 
  "Libros, copias, hojas, carteles, otros.", 
  "Libros,guía metodologica impresa,marcadores,hojas,y otros materiales a ocupar", 
  "Libros, impresiones, fotocopias, cel., laptop, material de oficina, entre otros.", 
  "Libros, hojas de papel impresora, tinta, marcadores, revistas D.", 
  "Libros, hojas de papel, impresiones, entre otros.",
  "Libros, computadora, impresora, marcadores, pliegos de papel, impresiones, carteles, infografias", 
  "Hojas de actividades", "Hojas de lectura", "Hojas de trabajo", 
  "Hojas de trabajo según cnb", "Hojas de trabajo,", "Hojas de trabajo, lectura, vinculación con la comunidad", 
  "Hojas de trabajo, libros de texto,recursos didácticos", "Hojas de trabajo, papel bond, cuaderno crayones , papelografos", 
  "Hojas de trabajo, presentaciones en proyector", "Hojas impresas", 
  "Hojas impresas, documentos, carteles", "Hojas iris, ruletas.", 
  "Hojas, libros y copias", "Hojas, msrcadores, papel craf", "INTERNET, LIBROS DE COMUNICACIÓN", 
  "Hojas", "Hojas bond, libro de texto, computadora, cañonera, bocina, materiales de impresiones, entre otros."
)

### recursos digitales
valores_recurso_digital_propio <- c(
  "Diapositivas, comprensión lectora, folletos", 
  "Diccionario. Libros de lectura. Plataforma", 
  "Google, Libros de texto", "Páginas web",
  "Plataforma y libro", "Sitios web y hojas de trabajos", 
  "Plataformas ditigales.", "Plataformas virtuales, hojas de trabajo, herramientas digitales, computadora", 
  "Presentaciones", "Presentaciones digitales.", "Presentaciones, hojas de trabajo, recursos digitales elaborados, etc.", 
  "Presentaciones, videos, plataformas interactivas.",
  "Libros, pdf documentos web", 
  "Videos y pantalla.", "Videos, música, lectura, exposiciones, ejemplificaciones, dramatizaciones,", 
  "Libros, redes sociales y todo lo que me ayude en mi curso", 
  "Libros, sitios web", "Medios digitales",
  "Libros, libros digitales, páginas web", 
  "Libros, folleto, sitio web",
  "Recursos de la Web y herramientas virtuales", 
  "Libros y uso de Internet", "Libros, PDF", "Libros, cnb virtual, y publicaciones y videos", 
  "Libros en PDF. Investigación de Google",
  "Libros digitales, plantillas creadas para diversas actividades, material didáctico", 
  "Libros de texto, herramientas digitales, actividades lúdicas, proyectos de aula, aprendizaje Colaborativo, dramatizaciones, poesías corales entre otros.", 
  "Laptop, plataforma, internet, folletos de ortografía y caligrafía, obras literarias y manuales de comprensión lectora.",
  "Internet, páginas web", "La web", 
  "Investigaciones", "Investigaciones en diferentes páginas web, de las cuales realizo impresiones y trabajo con los estudiantes", 
  "Investigaciones propias", "Investigaciones propias, libro de santillana, y otros textos", 
  "Investigaciones, impresiones, hojas de trabajo, cajonera entre otros", 
  "Investigación", "Investigación en internet",
  "Libros y material digital", "Páginas web y libros", 
  "Libros y pag. Web", "Libros y páginas de Internet", 
  "Libros y páginas web", "Libros y sitio web",
  "Digital y textos varios", "Digitales e impresos"
)

### Libros
valores_libros_propio_cl <- c(
  "Computadora, libros de texto, libros de lectura, proyector", 
  "Computadora, libros, cnb, celular", "Computadora, libros, hojas de trabajo", 
  "Computadora, libros, hojas, recursos naturales.","Comunicación y Lenguaje", 
  "Contenido de otros libros, revistas, periódicos y anexos", 
  "Contenidos con temas impresos y ejercicios",
  "Enciclopedia practica de comunicación", 
  "Enciclopedicos", "Bitácora",
  "Lectura y ejercicios base", "Lecturas, carteles, diccionario pizarrón etc.",
  "LIBRO DE TEXTO", "LIBROS, FOLLETOS, EDUCAPLAY, MATERIAL EN PDF", 
  "Investigaciones propias, libro de santillana, y otros textos", 
  "Internet, libros", 
  "libro", "libro de lecturas, ortografía , caligrafía y contenidos", 
  "libros", "libros, cnb, internet", 
  "Santillana", "Santillana computadora", 
  "Santillana comunicación y lenguaje segundo básico", 
  "Internet, libros de texto, computadora", "Internet, libros digitales, CNB",
  "INTERNET, LIBROS DE COMUNICACIÓN", 
  "Herramienta de evaluación, CNB y libros de texto",
  "HOJAS DE TRABAJO,  DICCIONARIOS,  LIBROS",
  "Diccionario, computadora. Canonera, impresiones, ¹", 
  "Diccionario, inciclopedia de comunicación ,libros de lenguaje, guías propias", 
  "Diccionario. Libros de lectura. Plataforma", "Diccionarios libros", 
  "Libro", "Libro Gramatical", 
  "Libro Santillana y otros", "Libro Santillana, tecnología.", 
  "Libro de Comunicación y Lenguaje No. 9, Editorial Susaeta. Libros de Comprensión de lectura de Susaeta. Ortografía Básica. Análisis y aplicación de ortografía moderna. Lectopolis.", 
  "Libro de Literatura y otros de acuerdo a los temas", "Libro de Telesecundaria", 
  "Libro de santillana", "Libro de texto Santillana Guatemala", 
  "Libro de texto cuaderno, hojas de trabajo, cuadernillo, internet", 
  "Libro de texto y actividades", "Libro de texto, teléfono, aplicaciones tecnológicas", 
  "Libro de textos, tiras didácticas", "Libro lenguaje de Básico", 
  "Libro ortografía", "Libro personal e información digital", 
  "Libro, computadora, Internet e impresora", "Libro, periódico mural", 
  "Libros", "Libros  prensa marcadores hojas etc.", "Libros ,cartulinas, laminas y otros", 
  "Libros de Texto", "Libros de Texto, Guías", "Libros de Texto, Santillana  Susaeta, Iger, entre otros.", 
  "Libros de comunicación y lenguaje", "Libros de ejercicios", 
  "Libros de lectura", "Libros de lectura y textos literarios, cañonera, internet, fichas , hojas variadas", 
  "Libros de lectura, cuadernos de caligrafía, hojas de lecturas y su comprobación", 
  "Libros de lectura, ortografía, redacción y caligrafía", "Libros de literatura y otros", 
  "Libros de ortografía", "Libros de texto", "Libros de texto Idioma Español", 
  "Libros de texto Internet  impresiones", "Libros de texto de diferentes editores y el CNB", 
  "Libros de texto de distintas editoriales.", "Libros de texto impresionante  Internet", 
  "Libros de texto universitarios, folletos de capacitaciones del ministerio de educación", 
  "Libros de texto y de lectura", "Libros de texto y fotocopias", 
  "Libros de texto y material didáctico", "Libros de texto y obras literarias", 
  "Libros de texto, Hojas de trabajo", "Libros de texto, folletos, fotocopias,celular", 
  "Libros de texto, herramientas digitales, actividades lúdicas, proyectos de aula, aprendizaje Colaborativo, dramatizaciones, poesías corales entre otros.", 
  "Libros de texto, impresiones, material didáctico elaboración propia", 
  "Libros de texto, internet, material propio, aplicaciones y plataformas", 
  "Libros de texto, obras literarias, periòdico, revistas, pàginas de internet", 
  "Libros de textos", "Libros de textos e impresiones", "Libros de textos, computadora", 
  "Libros de textos, computadora, celular, rompecabezas, hojas de papel bond, marcadores, impresora.", 
  "Libros digitales", "Libros digitales, plantillas creadas para diversas actividades, material didáctico", 
  "Libros e Internet", "Libros e internet", "Libros en PDF. Investigación de Google", 
  "Libros planificación marcadores cronogramas", "Libros propios, Internet.", 
  "Libros te texto, de lectura", "Libros videos hojas impresora etc", 
  "Libros y CNB", "Libros y actividades creativas, internet", "Libros y cuadernos", 
  "Libros y cuadernos y cokpitsdora", "Libros y documentos en linea", 
  "Libros y furriculo digital", "Libros y hojas de trabajos", "Libros y material digital", 
  "Libros y materiales", "Libros y pag. Web", "Libros y páginas de Internet", 
  "Libros y páginas web", "Libros y sitio web", "Libros y texnologia", 
  "Libros y uso de Internet", "Libros, PDF", "Libros, cnb virtual, y publicaciones y videos", 
  "Libros, computador", "Libros, computadora, cañonera, impresora.", 
  "Libros, computadora, impresora, marcadores, pliegos de papel, impresiones, carteles, infografias", 
  "Libros, computadora, impresora, tinta", "Libros, copias, hojas, carteles, otros.", 
  "Libros, diccionario fotocopias", "Libros, diccionarios y obras", 
  "Libros, enciclopedia", "Libros, enciplopedias, páginas web", 
  "Texto/santillana", 
  "Textos", "Textos, para reforzar los aprendizajes, internet.", 
  "Libros, folleto, sitio web", "Libros, folletos", "Libros, folletos, documentos impresos de la universidad", 
  "Libros, folletos, fotocopias de lecturas", "Libros, folletos, marcadores, fotocopias", 
  "Libros, folletos, televisión, internet", "Libros, fotocopias", 
  "Libros, hojas de papel impresora, tinta, marcadores, revistas D.", 
  "Libros, hojas de papel, impresiones, entre otros.", "Libros, hojas de trabajo.", 
  "Libros, hojas y más", "Libros, impresiones", "Libros, impresiones,", 
  "Libros, impresiones, fotocopias, cel., laptop, material de oficina, entre otros.", 
  "Libros, internet, cartel didáctico", "Libros, internet, hojas, carteles", 
  "Libros, libros digitales, páginas web", "Libros, lista de cotejo, hojas de trabajo", 
  "Libros, marcadores, computadora, material didáctico", "Libros, pdf documentos web", 
  "Libros, redes sociales y todo lo que me ayude en mi curso", 
  "ODEC impresa del MINEDUC, Investigaciones en Internet, Libros de reglas ortogradicas", 
  "Otros libros", "Otros libros de texto, juegos", 
  "Libros, sitios web", "Libros, tiras didácticas, dinámicas interactivas.", 
  "Libros,guía metodologica impresa,marcadores,hojas,y otros materiales a ocupar", 
  "Libros,internet", "Libros. Computadora, impresiones, tinta. Impresora"
)

### no especifica
valores_noespecifica_recurso_cl2 <- c(
  "Económicos", "0", "1", "Mentor ineractivo", "Varios", 
  "Teoria y practico", "Teoricas y practicas","Todo", "Voz, experiencias"
)

### folletos y guias
valores_folletos_guia_propio <- c(
  "Folleto y videos", "Folletos", 
  "Folletos CNB imagenes medios audivisuales", "Folletos impresos", 
  "Hoja de ejercicios, resumen de contenidos", 
  "Planificador y hojas de trabajo actividad",
  "folleto hojas de trabajo  diccionario", "hojas de trabajo, libros, revistas", 
  "Una guia sacando copias un celular con internet",
  "Libros, folletos", "Libros, folletos, documentos impresos de la universidad", 
  "Libros, folletos, fotocopias de lecturas", "Libros, folletos, marcadores, fotocopias", 
  "Libros, folletos, televisión, internet",
  "Laptop, plataforma, internet, folletos de ortografía y caligrafía, obras literarias y manuales de comprensión lectora.",
  "Folletos investigados pdf", "Folletos material didáctico marcadores crayones", 
  "Folletos y recursos tecnologicos", "Folletos, CNB imágenes, marcadores, medios audiovisuales", 
  "Folletos, computadora, entre otros", "Folletos, hoja de trabajo, diccionario", 
  "Folletos, hojas de trabajo y diccionario.", "Folletos, internet, Santillana", 
  "Folletos. Presentaciones de PowerPoint", "Google, Libros de texto", 
  "Guia de aprendizaje", "Guia de aprendizaje y su planificador", 
  "Guia telesecundaria y comunicación lenguaje piedrasanta", "Guias", 
  "Guias estás mandan a los libros anteriores", "Guía metodológica de comunicación y lenguaje virtual e investigaciones", 
  "Guía y planificador de telesecundaria", "Guías de aprendizaje"
)

### ninguno
valores_ningun_recurso_cl <- c(
  "Ninguno", "Recursos propios", "Práctica en clase",
  "No imparto comunicación y lenguaje"
)

### mobiliario
valores_mobiliario_cl_propio <- c(
  "Pizarron y marcador", "Pizzaron, marcadores, afiches e impresiones"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cl_recursos_propios = case_match(
      cl_recursos_propios,
      all_of(valores_mobiliario_cl_propio) ~ "Mobiliario",
      all_of(valores_ningun_recurso_cl) ~ "Ninguno",
      all_of(valores_folletos_guia_propio) ~ "Folletos y guías",
      all_of(valores_noespecifica_recurso_cl2) ~ "No especifica el recurso",
      all_of(valores_libros_propio_cl) ~ "Libros",
      all_of(valores_recurso_digital_propio) ~ "Recursos digitales",
      all_of(valores_papeleria_cl_propio) ~ "Material de papeleria",
      all_of(valores_cnb_cl_propio) ~ "CNB",
      all_of(valores_tecnologico_cl) ~ "Equipo tecnológico",
      all_of(valores_audio_didactico_cl) ~ "Material audiovisual y didáctico",
      .default = cl_recursos_propios
    )
  )

## Volver a ver errores

valores_cl_recursos_propios <- df_dyd |>
  filter(!(is.na(cl_recursos_propios))) |>
  select(cl_recursos_propios) |>
  tabyl(cl_recursos_propios)

# View(valores_cl_recursos_propios)

#

# cl_otro_ultima_vez_libros_mineduc ----
## ver errores
valores_cl_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(cl_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(cl_otro_ultima_vez_libros_mineduc ))) |>
  select(cl_otro_ultima_vez_libros_mineduc) |>
  tabyl(cl_otro_ultima_vez_libros_mineduc)

# View(valores_cl_otro_ultima_vez_libros_mineduc)

## Define los vectores ----

### 2014
valores_cl_2014 <- c(
  "2014"
)

### 2016

valores_cl_2016 <- c(
  "2016"
)

### 2017
valores_cl_2017 <- c(
  "2017"
)

### 2018
valores_cl_2018 <- c(
  "2018", "2018.", "Año 2018"
)

### 2019
valores_cl_2019 <- c(
  "2019"
)

### 2020 
valores_cl_2020 <- c(
  "2020", "2020.", "2020 aproximadamente"
)

### 2024
valores_cl_2024 <-c (
  "2024"
)

### no recuerda
valores_norecuerda_cl <- c(
  "No recuerdo", "Norecuerdo"
)

### nunca ha recibido
valores_nunca_libro_cl <- c(
  "Ninguna vez", "No he recibido", "No se a recibido"
)

## Recode con case match ----

df_dyd <- df_dyd  |>
  mutate(
    cl_otro_ultima_vez_libros_mineduc = case_match(
      cl_otro_ultima_vez_libros_mineduc,
      all_of(valores_nunca_libro_cl) ~ "Nunca ha recibido",
      all_of(valores_norecuerda_cl) ~ "No recuerda",
      all_of(valores_cl_2024) ~ "2024",
      all_of(valores_cl_2020) ~ "2020",
      all_of(valores_cl_2019) ~ "2019",
      all_of(valores_cl_2018) ~ "2018",
      all_of(valores_cl_2017) ~ "2017",
      all_of(valores_cl_2016) ~ "2016",
      all_of(valores_cl_2014) ~ "2014",
      .default = cl_otro_ultima_vez_libros_mineduc
    )
  )

## volver a ver errores

valores_cl_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(cl_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(cl_otro_ultima_vez_libros_mineduc ))) |>
  select(cl_otro_ultima_vez_libros_mineduc) |>
  tabyl(cl_otro_ultima_vez_libros_mineduc)

# View(valores_cl_otro_ultima_vez_libros_mineduc)

# cl_tipo_materiales_recibidos ----
## ver errores
valores_cl_tipo_materiales_recibidos <- df_dyd |>
  filter(cl_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(cl_tipo_materiales_recibidos))) |>
  select(cl_tipo_materiales_recibidos) |>
  tabyl(cl_tipo_materiales_recibidos)

# View(valores_cl_tipo_materiales_recibidos)

## Define los vectores ----

### Guia de aprendizaje y planificador
valores_guia_planificador_cl <- c(
  "GUÍA DE APRENDIZAJE Y PLANIFICADOR",
  "Guia de Aprendizaje","Guias y conceptos",
  "Guia y planicicador del curso", "Guia y planificador",
  "Guia y planificafores", "Guias", 
  "Guías y planificadores",
  "Planificador del docente"
)

### libros
valores_libro_cl <- c(
  "Libro de texto", "Libros de texto", "Textos"
)

### material papeleria

valores_papeleria_cl2 <- c(
  "Hojas, lapiceros y marcadores"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cl_tipo_materiales_recibidos = case_match(
      cl_tipo_materiales_recibidos,
      all_of(valores_papeleria_cl2) ~ "Material de papelería",
      all_of(valores_libro_cl) ~ "Libros",
      all_of(valores_guia_planificador_cl) ~ "Guías y planificador para el facilitador",
      .default = cl_tipo_materiales_recibidos
    )
  )

## volver a ver errores

valores_cl_tipo_materiales_recibidos <- df_dyd |>
  filter(cl_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(cl_tipo_materiales_recibidos))) |>
  select(cl_tipo_materiales_recibidos) |>
  tabyl(cl_tipo_materiales_recibidos)

# View(valores_cl_tipo_materiales_recibidos)

#in_libros_primero ----

## ver errores

valores_in_libros_primero <- df_dyd |>
  filter(in_uso_libros == "Sí") |>
  filter(in_grados == "Primer grado") |>
  filter(!(is.na(in_libros_primero))) |>
  select(in_libros_primero) |>
  tabyl(in_libros_primero)

dput(valores_in_libros_primero)

## Define los vectores ----

### ciav
valores_ciav <- c(
  "Ciav"
)

### english for everyone
valores_english_everyone <- c(
  "English for everyone, nivel"
)

### future 1
valores_future1 <- c(
  "Future 1"
)

### guia de aprendizaje y planificador
valores_guia_plani_in <- c(
  "Guas y planificadores", "Guia  de aprendizaje y  planificador",
  "Libros de lecturas guías de aprendizaje y planificador",
  "Guia y planificador", "Guias", "Guía  comunicación  y lenguaje  idioma  extranjero  1", 
  "Guías de aprendizaje y Planificador Docente", "Guías del idioma inglés",
  "Planificador", "Planificador del facilitador", 
  "Planificador del facilitador y Guia de Aprendizaje INGLÉS -ENGLISH 1", 
  "Planificador y guía, inglés básico"
)

### idioma extranjero 1
valores_extranjero <- c(
  "Idioma Extranjero 1"
)

### idioma ingles
valores_idioma_in <- c(
  "Idioma Inglés de Editorial Santillana"
)

### english 7
valores_english7 <- c(
  "Inglesh 7 susaeta"
)

### no especifica
valores_noespecifica_in <- c(
  "Inglés 1ro básico", "Practico y teorico",
  "Libros de lingles", "Mentor ibteractivo"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    in_libros_primero = case_match(
      in_libros_primero,
      all_of(valores_noespecifica_in) ~ "No especifica el nombre",
      all_of(valores_english7) ~ "English 7",
      all_of(valores_idioma_in) ~ "Idioma Inglés",
      all_of(valores_extranjero) ~ "Idioma Extranjero 1",
      all_of(valores_guia_plani_in) ~ "Guía de aprendizaje y planificador",
      all_of(valores_future1) ~ "Future 1",
      all_of(valores_english_everyone) ~ "English for Everyone",
      all_of(valores_ciav) ~ "Libro de CIAV",
      .default = in_libros_primero
    )
  )

## volver a ver errores

valores_in_libros_primero <- df_dyd |>
  filter(in_uso_libros == "Sí") |>
  filter(in_grados == "Primer grado") |>
  filter(!(is.na(in_libros_primero))) |>
  select(in_libros_primero) |>
  tabyl(in_libros_primero)

# View(valores_in_libros_primero)

# in_libros_segundo ----

## ver errores

valores_in_libros_segundo <- df_dyd |>
  filter(in_uso_libros == "Sí") |>
  filter(in_grados == "Segundo grado") |>
  filter(!(is.na(in_libros_segundo))) |>
  select(in_libros_segundo) |>
  tabyl(in_libros_segundo)

dput(valores_in_libros_segundo)

## Define los vectores ----
###ciapp
valores_ciapp <- c(
  "CIAPP"
)

### editora educativa ingles
valores_editora_edu_in <- c(
  "Editora Educativa New Básic"
)

### recursos de internet

valores_internet_in <- c(
  "Folletos de la web", "Google"
)

### guia de aprendizaje cyl idioma extranjero
valores_guia_ingles <- c(
  "Guia de aprendizaje comunicación y lenguaje idioma extranjero", 
  "Guía - Comunicación y Lenguaje Idioma Extranjero",
  "Guía de Comunicación y Lenguaje Idioma Extranjero Inglés"
)

### guia de aprendizaje
valores_guia_in <- c(
  "Guía 2019", 
  "Guías", "Guía de aprendizaje, diccionario",
  "Planificados y guías de Telesecundaria", "Planifixador, guias de aprendizaje dizaje,diccionario, revistas",
  "Guía de aprendizaje, Enciclopedia de la Educación",
  "Guía de aprendizaje 2 y Planificador del docente 2"
)

### ingles para segundo grado
valores_ingles_segundo <- c(
  "Inglés para segundo grado. Inglés Práctico."
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    in_libros_segundo = case_match(
      in_libros_segundo,
      all_of(valores_ingles_segundo) ~ "Inglés para Segundo Básico",
      all_of(valores_guia_in) ~ "Guía de aprendizaje",
      all_of(valores_guia_ingles) ~ "Comunicación y Lenguaje idioma extranjero",
      all_of(valores_internet_in) ~ "Recursos de internet",
      all_of(valores_editora_edu_in) ~ "Editora Educativa Inglés",
      all_of(valores_ciapp) ~ "CIAPP",
      .default = in_libros_segundo
    )
  )

## volver a ver errores

valores_in_libros_segundo <- df_dyd |>
  filter(in_uso_libros == "Sí") |>
  filter(in_grados == "Segundo grado") |>
  filter(!(is.na(in_libros_segundo))) |>
  select(in_libros_segundo) |>
  tabyl(in_libros_segundo)

# View(valores_in_libros_segundo)



# in_libros_tercero ----

## ver errores

valores_in_libros_tercero <- df_dyd |>
  filter(in_uso_libros == "Sí") |>
  filter(in_grados == "Tercer grado") |>
  filter(!(is.na(in_libros_tercero))) |>
  select(in_libros_tercero) |>
  tabyl(in_libros_tercero)

dput(valores_in_libros_tercero)

## Define los vectores ----

### guia de aprendizaje idioma extranjero
valores_guia_aprendizaje_in <- c(
  "Guia de Aprendizaje y otros materiales didacticos", "Guía de Aprendizaje de comunicación y lenguaje idioma extranjero ingles", 
  "Guía de aprendizaje", "Guía de aprendizaje idioma inglés", 
  "Guía de aprendizaje y libro de Ciav", "Guía de aprendízaje idioma exteanjero", 
  "Guía de comunicación y lenguaje, idioma estrangero."
)

### comunicacion y lenguaje

valores_comu_lenguaje_in <- c(
  "Comunicación y Lenguaje (Ejercicios Unificados)"
)

### guia y planificador
valores_guia_planificador_in <- c(
  "Guía y planificador", 
  "Guía y planificador, Asociación para el Desarrollo Integral",
  "Planificador, Guias de Aprendizaje del estudiante, Diccionario"
)

### no especifica el nombre 
valores_noespecifica_in2 <- c(
  "Comprensión Lectora", 
  "Folletos",  
  "Libros digitales", "No recuerdo"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    in_libros_tercero = case_match(
      in_libros_tercero,
      all_of(valores_noespecifica_in2) ~ "No especifica el nombre",
      all_of(valores_guia_planificador_in) ~ "Guía y planificador",
      all_of(valores_comu_lenguaje_in) ~ "Comunicación y Lenguaje",
      all_of(valores_guia_aprendizaje_in) ~ "Comunicación y Lenguaje idioma extranjero",
      .default = in_libros_tercero
    )
    )

## volver a ver errores

valores_in_libros_tercero <- df_dyd |>
  filter(in_uso_libros == "Sí") |>
  filter(in_grados == "Tercer grado") |>
  filter(!(is.na(in_libros_tercero))) |>
  select(in_libros_tercero) |>
  tabyl(in_libros_tercero)

# View(valores_in_libros_tercero)

# in_recursos_centro_educativo ----

## ver errores

valores_in_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(in_recursos_centro_educativo))) |>
  select(in_recursos_centro_educativo) |>
  tabyl(in_recursos_centro_educativo)

dput(valores_in_recursos_centro_educativo)

## Define los vectores ----

### libros
valores_libros_in <- c(
  "Algunos libros",
  "LIBRO", "Libr Guía de aprendizaje", "Libro", 
  "Enciclopedicos", "Un libro comprado", 
  "Texto", "Textos", "Textos, bocinas, Proyector, computadora",
  "Libro Comseps", "Libro de Ciav e internet", "Libro de inglés básico", 
  "Libro de texto", "Libro de texto, guía del maestro, TV para utilizar material auditivo", 
  "Libro de texto, planificadores, material didáctico.", "Libro del programa AMCO", 
  "Libro docente richmond Santillana", "Libro para docente y diccionarios", 
  "Libro y hojas de investigación", "Libro y material didáctico", 
  "Libro, Internet, plataforma de recursos", "Libro, pizarra almohadilla", 
  "Libro, pizarrón, computadora e impresora, internet", "Libros", 
  "Libros Cd's pantalla, Internet", "Libros comprados e investigaciones propias", 
  "Libros de  texto", "Libros de Texto", "Libros de apoyo, acompañamiento por parte de la editorial", 
  "Libros de texto", "Libros de texto que están escritos en inglés y no trae traducción", 
  "Libros de texto y materiales impresos, tecnología y recursos en línea, material audiovisual, visual y gráfico, juegos y actividades interactivas, material de escritura y ejercicios prácticos, material de referencia, gramática y fonética, evaluaciones y pruebas.", 
  "Libros de texto, consultas en internet", "Libros de texto, impresiones, fotocopias, hojas bond, papel de colores, pegamento, tijeras, bocina.", 
  "Libros de texto, malla curricular", "Libros de texto, material audio visual, valija didáctica, computadoras, material permanente de trabajo", 
  "Libros de texto, recursos tecnológicos", "Libros de texto, tecnología, material didáctico", 
  "Libros impresos y Textos", "Libros y guías", "Libros y pantalla", 
  "Easy english", "El libro que mando el ministerio de educación,  y el de concepción basicos",
  "Libros, diapositivas, material didáctico", "Libros, hojas impresora", 
  "Libros, marcadores, pizarra, impresiones etc.", "Libros, pizarrones, pantallas, proyectores.", 
  "Libros, pizarrón", "Libros, plataforma virtual, cañonera, computadora, cuadernos", 
  "Libros, tecnología", "Librosndentexto",
  "libro de texto y guía del maestro", "libros", "libros de texto", 
  "libros, internet", 
  "Únicamente el libro del área", 
  "Revistas otros libros de ingles", "Santillana",
  "Me dieron tres libros de Inglés en el año 2021 que envió el área de DIGEBI de la Dirección Departamental"
)

### materiales audiovisual y didactico
valores_audio_didactico_in <- c(
  "Algunos materiales didácticos","Libro y material didáctico", 
  "Libros de texto, material audio visual, valija didáctica, computadoras, material permanente de trabajo",
  "Material audio visual y material didáctico", 
  "Balija didáctica","Didáctico", "Valija didáctica", "Valija didáctica, libro New Basic English one, diccionario",
  "Libros, diapositivas, material didáctico",
  "Libros de texto, tecnología, material didáctico", 
  "Libros de texto y materiales impresos, tecnología y recursos en línea, material audiovisual, visual y gráfico, juegos y actividades interactivas, material de escritura y ejercicios prácticos, material de referencia, gramática y fonética, evaluaciones y pruebas.",
  "Libro de texto, guía del maestro, TV para utilizar material auditivo", 
  "Libro de texto, planificadores, material didáctico.",
  "Investigaciones e impresiones  entre otros", 
  "Investigación", "Investigación de contenidos de Idioma Ingles", 
  "Guía de aprendizaje, planificador docente, valija didáctica",
  "Elaboración de materiales, manualidades,",
  "plataformas, copias, audiovisuales",
  "textos, material permanente de trabajo, audio visuales computadores.",
  "Material didáctico", "Material didáctico, uso de la tecnología, entre otros", 
  "Material impreso", "Material impresos", "Materiales didácticos, pizarra, marcadores, bocinas, micrófonos y otros.", 
  "Materiales impresos, audiovisuales, hojas de trabajo",
  "Únicamente lo de la valija didáctica"
)

### recursos digitales
valores_recurso_digital_in <- c(
  "Aplicaciones", "Copias y videos", "Diccionario, aplicaciones web",
  "Digitales", "Digitales e impresos",  "Impresiones y página w",
  "Libro, Internet, plataforma de recursos",
  "Recursos digitales y folletos impreos",
  "plataformas, copias, audiovisuales",
  "Servicio de internet", "Sitios web y libros",
  "Sitios web y libros.", "Videos", "impresos y digitales",
  "Libros, plataforma virtual, cañonera, computadora, cuadernos"
)

### mobiliario
valores_mobiliario_in <- c(
  "Aula, pizarrón, marcadores, cuadernos, hojas de papel, lapiceros, lápices",
  "Copias", "Copias y videos", "La pizarra",
  "Hoja de trabajo, pizarrón, audios",
  "Libro, pizarra almohadilla", "Salón de clases, pizarra",
  "Pizarra", "Pizarra, proyector", "Solo el aula", 
  "Pizarron", "Pizarrón", "Pizarrón y Pantalla", "Pizarrón, bocina, marcadores", 
  "Pizarrón, televisión, cañonera, bocina, marcadores y libros de texto.", 
  "Libros, marcadores, pizarra, impresiones etc.", "Libros, pizarrones, pantallas, proyectores.",
  "Libro, pizarrón, computadora e impresora, internet",
  "Copias, marcador, lapicero", "Copias, marcadores", 
  "Diapositivas, hojas de trabajo"
  
)

### material de papeleria
valores_papeleria_in <- c(
  "Aula, pizarrón, marcadores, cuadernos, hojas de papel, lapiceros, lápices",
  "Estuche de marcadores", "Hoja de trabajo, pizarrón, audios",
  "Hojas bond", "Hojas de investigación e innovación", 
  "Marcador, almohadilla, pizzara, cartulina, videos.", "Marcadores para pizarra, almohadillas, lapiceros cuadernos para planificar, cartulinas, papel, Iris, hojas de todo tipo", 
  "Marcadores y borrador de pizarra, lapiceros y lápiz", "Marcadores y cartulinas", 
  "Marcadores y cartulinas.", "Marcadores, hojas, resaltadores, folder", 
  "Marcadores, hojas,pizarrones y mas", "Marcadores, pizarrones, hojas, papel bond,", 
  "Marcadores. Impresiones", "computadoras marcadores pizarron almohadilla",
  "Pliegos de papel bond, hojas bond, cuaderno,celular,cañonera", 
  "Libros, marcadores, pizarra, impresiones etc.", "Libros, pizarrones, pantallas, proyectores.", 
  "Libros de texto, impresiones, fotocopias, hojas bond, papel de colores, pegamento, tijeras, bocina.", 
  "Hojas de trabajo", "Hojas, impresiones, CNB digital, marcadores", 
  "Hojas, lapiceros, etc.", "Hojas, marcadores, cartulinas", "Ilustraciones, cartulinas, ficheros, diccionarios de inglés", 
  "Impresiones y hojas", "Impresiones y página w", "Impresiones, fotocopias,libros de texto", 
  "Impresiones, proyector, cartulina, marcadores."
  
)

### folletos y guias
valores_guia_folleto_in <- c(
  "Algunos folletos", 
  "Folleto", "Folleto, libros, tecnología", 
  "Folletos", "Fotocopia, marcadores, tinta, hojas", "Fotocopias", 
  "GUIA DE APRENDIZAJE", "GUIAS DE APR4NDIZAJE", "GUÍA DE APRENDIZAJE Y PLANIFICADOR", 
  "Guia de aprendizaje comunicación y lenguaje idioma extranjero", 
  "Guia de aprendizaje y planificador", "Guia de ingles", "Guia y planificador", 
  "Guias", "Guias y planificador", "Guía de aprendizaje", "Guía de aprendizaje, planificador docente, valija didáctica", 
  "Guía de estudios para", "Guías de aprendizaje hojas de lecturas  planificador", 
  "Guías de aprendizaje y planificador docente", "Guías de comunicación inglés", 
  "Guías y planificador", "Guías y planificadores", "Guías."
)

### equipo tecnológico
valores_tecnologia_in <- c(
  "Bocina, USB, material didáctico, canciones", 
  "Bocina, hojas de trabajo, juegos,",
  "Computadora, cañonera, televisión.","Recursos Didácticos",
  "Libros, pizarrón", "Libros, plataforma virtual, cañonera, computadora, cuadernos", 
  "Libros, tecnología", "Proyector",
  "Tecnológico", "Tecnológicos, textos, impresiones etc", 
  "Televisiones, computadora, internet, bocina", "Teléfono, internet", 
  "Pizarra, proyector", "Pizarrón y Pantalla", "Pizarrón, bocina, marcadores", 
  "Pizarrón, televisión, cañonera, bocina, marcadores y libros de texto.", 
  "Pantalla plana", "Pantalla y en algunos salones computadora y la plataforma del libro.", 
  "Pantalla, Impresora", "Pantalla, Impresora.",
  "Internet", "Internet, equipo de sonido, libros, computadora", 
  "Internet, libros digitales","Libros y pantalla", 
  "Libros de texto, recursos tecnológicos", "Libros de texto, tecnología, material didáctico", 
  "Libros Cd's pantalla, Internet",
  "Textos, bocinas, Proyector, computadora", 
  "Tv", "Tv.", "computadoras marcadores pizarron almohadilla",
  "Vía Internet", "cañonera", "cañonera, bocinas y una tv", 
  "Libros de texto y materiales impresos, tecnología y recursos en línea, material audiovisual, visual y gráfico, juegos y actividades interactivas, material de escritura y ejercicios prácticos, material de referencia, gramática y fonética, evaluaciones y pruebas.",
  "Libro, pizarrón, computadora e impresora, internet",
  "Impresiones, proyector, cartulina, marcadores.", "Impresora, cañonera y pizarra", 
  "COMPUTADORA", "Cañonera", "Cañonera y Pantalla", "Cañonera, televisores y bocina.", 
  "Cañonera, televisores, bocina, pizarrón, marcadores.", 
  "Cañonera. Fotocopiadora  impresora computadora"
)

### cnb
valores_cnb_in <- c(
  "CNB", "CNB, Libros con vocabulario y gramática, ejercicios en PDF", 
  "CNB, Planificador guías de aprendizaje", 
  "Cnb", "CNB, libros entre otros", "cnb"
)

### sin especificar
valores_noespecifica_recurso_in <- c(
  "Los disponibles", "Muchos"
)

### ninguno
valores_ninguno_in <- c(
  "Mi conocimiento y experiencia",
  "NINGUNO", "NO", "Nada", "0",
  "Ninguna", "Ninguno", "No", "No facilita", "No imparto comunicación y lenguaje", 
  "Nunguno", "ni uno", "ni uno recurso", "ninguno"
)

### beca
valores_beca_in <- c(
  "Beca de estudios de inglés."
)

### planificador
valores_planificador_in <- c(
  "Planificador", "Planificador  idioma extranjero ingles 2", "Planificador del docente", 
  "Planificador del facilitador, guia de aprendizaje", "Planificador y guía del curso", 
  "Planificador y guías de aprendizaje", "Planoficador y guia"
)

## Recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    in_recursos_centro_educativo = case_match(
      in_recursos_centro_educativo,
      all_of(valores_planificador_in) ~ "Planificador del facilitador",
      all_of(valores_ninguno_in) ~ "Ninguno",
      all_of(valores_noespecifica_recurso_in) ~ "No especifica el recurso",
      all_of(valores_cnb_in) ~ "CNB",
      all_of(valores_tecnologia_in) ~ "Recursos tecnológicos",
      all_of(valores_guia_folleto_in) ~ "Guías y folletos",
      all_of(valores_papeleria_in) ~ "Material de papelería",
      all_of(valores_mobiliario_in) ~ "Mobiliario",
      all_of(valores_recurso_digital_in) ~ "Recursos digitales",
      all_of(valores_libros_in) ~ "Libros",
      all_of(valores_beca_in) ~ "Beca de inglés",
      all_of(valores_audio_didactico_in) ~ "Material audiovisual y didáctico",
      .default = in_recursos_centro_educativo
    )
  )

## volver a ver errores

valores_in_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(in_recursos_centro_educativo))) |>
  select(in_recursos_centro_educativo) |>
  tabyl(in_recursos_centro_educativo)

# View(valores_in_recursos_centro_educativo)

# in_recursos_propios ----

## ver errores

valores_in_recursos_propios <- df_dyd |>
  filter(!(is.na(in_recursos_propios))) |>
  select(in_recursos_propios) |>
  tabyl(in_recursos_propios)

dput(valores_in_recursos_propios)

## Define los vectores ----

### material audiovisual y didáctica
valores_audio_didactico_propio <- c(
  "Actividades lúdica",
  "Audio", "Audios hojas de trabajo, diálogos", 
  "Audios, Marcadores, Diccionario, hojas de trabajo, ilustraciones,", 
  "Audios, hojas de trabajos, diálogos y cantos entre otros", 
  "Audios, imágenes, rompecabezas", "Bocina, audios",
  "Bocina, carteles, dibujos, objetos reales", 
  "Marcador, almohadilla, pizzara, cartulina, videos.", "Marcadores papeles cartulinas y otros", 
  "Marcadores, internét, librillos",
  "hoja de trabajo, cartulinas,  oraciones ,actividades dentro y fura del aula", 
  "hojas de trabajo, carteles y videos",
  "Presentaciones, aplicaciones, hojas de trabajo", "Presentaciones, hojas de trabajo, folletos , pruebas cortas",
  "HOjas de trabajo, material didáctico, libros computadora", 
  "Bocina, folletos","Ejercicios impresos",
  "Libro, fotocopias, materiales digitales elaborados, post, presentaciones, documentos.",
  "Copias", "Copias y diccionario", "Copias, audiovisuales", "Crayones, papel bond, libros", 
  "Crusigramas ,sopa de letras ,hojas de trabajo ,instrumento de evaluación",
  "Cañonera, computadora y material didáctico", 
  "Recortes, hojas, Internet", "Recortes. Videos",
  "Recurso económico impresiones", 
  "Libro de texto, computadora, material didactico", "Libro de texto, hojas de trabajo", 
  "Computadora, bocinas , presentaciones y proyectores.", "Computadora, bocinas, pesentaciones, proyector.", 
  "Computadora, hojas, lapicero, marcadores, material reciclado", 
  "Computadora, impresora, libros de texto, loterías, rompecabezas, hojas de trabajo", 
  "Computadora, impresora, libros, hojas de trabajo, rompecabezas, loterías.", 
  "Computadora, impresora, teléfono, bocina, carteles, hojas de trabajo.", 
  "Computadora, impresoras, libros, internet, loterías, rompecabezas.", 
  "COPIAS,JUEGOS, LIBRO ,ACTIVIDADES  ,",
  "Libros y material didáctico", 
  "Material impreso de Páginas Web", "Material impreso, material de descarga, discos con charlas y diálogos", 
  "Materiales didacticos", "Materiales didácticos,", 
  "Materiales impresos", 
  "textos, audio visuales, material permanente de trabajo, computadoras, valija didactica",
  "Planificación propia, laptop, otros.", "Planificación, libro,", 
  "Planificador","libros, tecnología, material elaborado por mi", 
  "Utilizo material elaborado para el aprendizaje del inglés como lengua extranjera.", 
  "Videos", "Videos, calonera, pantalla, ibros folletos e márgenes", 
  "Vocabularios y libros de Ingles", 
  "Libros, hojas de trabajo, videos, audios", "Libros, impresiones, internet", 
  "Libros, infografias, carteles, memorias, afiches, impresora, hojas, marcadores", 
  "Libros y material digital audivisual",
  "Libros digitales, marcadores, hojas, computadora.", 
  "Hojas bond. Bocina", "Libro, material didáctico", 
  "Lenguaje mimico, papelografos",
  "Libros de texto, planificaciones del CNB impresas, materiales impresos(fotocopias)", 
  "Impresora, hojas de trabajo y actividades",
  "Hojas con dibujos y escritura en inglés y pronunciación", 
  "Hojas de Trabajo, Diccionario Español-Inglès,", "Hojas de actividades", 
  "Hojas de apoyo", "Hojas de trabajo", "Hojas de trabajo impresas", 
  "Hojas de trabajo, libro de gramática más complementaria", 
  "Hojas de trabajo, lista de cotejo", "Hojas de trabajo, lista de cotejos", 
  "Hojas de trabajo, tarjetas, audios,", "Hojas y libros", "Hojas, dibujos, cañonera, internet", 
  "Festivales, competencias, videos, actividades especiales.", 
  "Flashcards posters, TV set", "Libros, audios, videos, imágenes.",
  "Juegos, bocinas hojas de trabajo", 
  "Material didáctico", "Material didáctico variado", 
  "Libros, material audio-visual","Lista de cotejo y material didáctico", 
  "Cables, libros, audios, computadora, flash cards, posters, ilustraciones,", 
  "Canciones, ejercicios, teoría, actividades didácticas propias, juegos", 
  "Carteles, audios, libros", "Carteles, juegos de mesa.", 
  "Libro, videos, carteles, imágenes", "Libros , afiches, material audio visual."
)

### material de papeleria
valores_papeleria_propio_in <- c(
  "Folletos, marcadores, pizarra, bocinas, videos, etc.",
  "Afiches, pizzaron, marcadores e impresiones",
  "Impresiones", "Impresiones, laptop, Internet.", 
  "Libro,  marcadores, hojas de trabajo",
  "Libros, pizarra, marcadores.", 
  "Papelografo y marcadores", 
  "marcadores, lapiceros , fotocopias",
  "Tableta, bocina, hojas, marcadores, post it, libros", 
  "Marcador, almohadilla, pizzara, cartulina, videos.", "Marcadores papeles cartulinas y otros", 
  "Marcadores, internét, librillos",
  "Libros, marcadores, hojas de trabajo entre otras",
  "Libros, dispositivos electrónicos, cartulinas, hojas  impresas, marcadores, lapiceros, borrador.",
  "Libros de Inglés (Book), Internet, hojas de trabajo (diálogos), marcadores, almohadilla, sello para revisar tareas.", 
  "Copias", "Copias y diccionario", "Copias, audiovisuales", "Crayones, papel bond, libros", 
  "Crusigramas ,sopa de letras ,hojas de trabajo ,instrumento de evaluación"
)

### recurso digital
valores_digital_in <- c(
  "Aplicaciones web  y libros",  "Búsquedas en Google",
  "CD, libros, diccionarios", "Computadora, libros digitales",
  "CNB de Guatemala, y libros digitales pdf.", "digitales", 
  "Folleto, sitios web", "INTERNET Y LIBROS DE INGLÉS",
  "Herramientas de redes o plataformas", "Libros y plataformas",
  "Libros de texto y libros digitales", "Libros, libros digitales", 
  "Internet, libros y cursos virtuales", "WEB",
  "Página w, impresiones", "Páginas  web libros digitales videos y recursos", 
  "Plataformas en linea como liveworksheets, libro de texto de una academía, juegos y ejercicios muy básicos.", 
  "Material digital","PDF's que he logrado obtener a través del internet",
  "Libro, impresiones e internet", "Libro, internet.",
  "Internet", "Internet, celular, carteles", "Internet, celular, computadora", 
  "Internet, folletos, libros de texto", "Internet, impresora", 
  "Internet, impresora y computadora", "Internet, libros y cursos virtuales", 
  "Investigacion", "Investigacion digital e impreso", "Investigaciones", 
  "Investigaciones propias, y libros comprados", "Investigaciones,  impresiones, hojas de trabajo, cajonera entre otros", 
  "Investigación digitales", "Investigación, hojas impresas, libros, pizarrón", 
  "Investigo en Internet y elaboro textos y actividades de aprendizaje para mis estudiantes.", 
  "Folletos, aplicaciones de internet, diccionario, otros",
  "Estudiar cursos virtuales de Ingles  y utilizacion de internet y redes sociales.", 
  "Digitales", "Digitales audios", "Digitales y auditivos",
  "Recursos de internet, Libros de texto", "Recursos de la Web y herramientas virtuales", 
  "Recursos digitales", "Suscripciones a clases en línea"
)

### equipo tecnologico
valores_tecnologico_in <- c(
  "Bocina, audios", "Bocina, carteles, dibujos, objetos reales", 
  "Bocina, folletos", "Bocina, fotocopias,computadora, celular",
  "COMPUTADORA Y GUIAS IMPRESAS","Minicomputadora, tablet, internet", 
  "Libros, dispositivos electrónicos, cartulinas, hojas  impresas, marcadores, lapiceros, borrador.", 
  "Libros, audio, computadora y cañonera", 
  "Hojas bond. Bocina", "Libro, mi computadura, fotocopias,",
  "Libro side by side, telefono con internet", 
  "Libro, Diccionario, teléfono marcadores y celular si es necesario para traductor", 
  "Libro, bocinas, hojas","Libros. Computadores y celular.",
  "Libros, bocinas, internet", "laptop, bocina, copias", 
  "Videos, calonera, pantalla, ibros folletos e márgenes", 
  "Pc, equipo de audio, impresiones", "folletos, computadora y otros",
  "Libros, computadora, bocinas, cañonera",
  "Libros,  libretas, bocina,  pantalla, computadora,  carteles, audio, fichas entre otros", 
  "Libros digitales, marcadores, hojas, computadora.", 
  "Laptop e internet", "Laptop, Guías de aprendizaje, libros", 
  "Laptop, textos, folletos", "Libros digitales, computadora, celular",
  "Libro de texto, computadora, material didactico",
  "Internet, celular, computadora", 
  "Pantalla, computadora", "computadora", 
  "computadora - bocina", "computadora - bocinas",
  "Pantalla, tv, Guías y libro práctico.",
  "Internet, impresora", "LAPTOP E INTERNET", 
  "Juegos, bocinas hojas de trabajo", 
  "Internet, impresora y computadora", 
  "Libros, web , computadoras",
  "Impresora, hojas de trabajo y actividades",
  "Hojas, dibujos, cañonera, internet", 
  "Folletos, marcadores, pizarra, bocinas, videos, etc.",
  "Cables, libros, audios, computadora, flash cards, posters, ilustraciones,",
  "Cañonera y pantalla", "Folleto, bocina,",
  "Cañonera, compu, bocina, marcadores de colores", "Cañonera, computadora y material didáctico", 
  "Celular", "Celular, google, bocina", "Computadora", "Computadora e impresora", 
  "Computadora portátil, folletos", "Computadora teléfono y textos", 
  "Computadora y bocinas", "Computadora, Internet", "Computadora, Ipad, proyector, libro y cuaderno", 
  "Computadora, aplicaciones", "Computadora, bocina, cañonera, impresora", 
  "Computadora, bocinas , presentaciones y proyectores.", "Computadora, bocinas, pesentaciones, proyector.", 
  "Computadora, cañonera, IMPRESORA, marcadores", "Computadora, hojas, lapicero, marcadores, material reciclado", 
  "Computadora, impresora, audio", "Computadora, impresora, entre otros", 
  "Computadora, impresora, libros de texto, loterías, rompecabezas, hojas de trabajo", 
  "Computadora, impresora, libros, hojas de trabajo, rompecabezas, loterías.", 
  "Computadora, impresora, libros, textos, tablet y celular", "Computadora, impresora, teléfono, bocina, carteles, hojas de trabajo.", 
  "Computadora, impresoras, libros, internet, loterías, rompecabezas.", 
  "Computadora, internet, hojas de trabajo, impresiones", "Computadora, libros digitales", 
  "Computadora, libros, audios, videos", "Computadora, televisión", 
  "TELEFONO CELULAR", "libros, tecnología, material elaborado por mi", 
  "Tablet", "Tablet, internet, proyector libros", "Tableta, bocina, hojas, marcadores, post it, libros", 
  "Tecnológico y libro de texto", "Tecnológicos", "Teléfono internet",
  "Computadora, teléfono, cuaderno", "Computadora, teléfono, hojas e impresora", 
  "Computadora,diccionario y mas", "Computadoras"
)

### cnb
valores_cnb_in_propio <- c(
  "CNB", "CNB de Guatemala, y libros digitales pdf.", "cnb"
)

### libros
valores_libros_propios_in <- c(
  "Computadora, libros, audios, videos", "Easy english",
  "Folletos, hojas de trabajo y libros de texto",
  "Google, Libros de texto","INTERNET Y LIBROS DE INGLÉS",
  "Guias, libros o folletos","LIBRO DE TEXTO", 
  "LIBRO: BASIC COMERCIAL ENGLISH", "Manual de inglés básico", 
  "Investigación, hojas impresas, libros, pizarrón",
  "Internet, libros y cursos virtuales", 
  "Internet, folletos, libros de texto", 
  "Hojas de trabajo, libro de gramática más complementaria", 
  "Diferentes versiones de Editorial Santillana y Editorial Norma",
  "Libro Compseps", "Libro de inglés basica", "Libro de texto, PDF de inglés, Google.", 
  "Libro de texto, computadora, material didactico", "Libro de texto, hojas de trabajo", 
  "Libro de texto, tarjetas", "Libro e internet", "Libro llamado Four Corners", 
  "Libro side by side, telefono con internet", "Libro y hojas de investigación", 
  "Libro,  marcadores, hojas de trabajo", "Libro, Diccionario, teléfono marcadores y celular si es necesario para traductor", 
  "Libro, bocinas, hojas", "Libro, fotocopias, materiales digitales elaborados, post, presentaciones, documentos.", 
  "Libro, impresiones e internet", "Libro, internet.", "Libro, material didáctico", 
  "Libro, mi computadura, fotocopias,", "Libro, revistas peliculas", 
  "Libro, videos, carteles, imágenes", "Libros", "Libros , afiches, material audio visual.", 
  "Libros de Inglés (Book), Internet, hojas de trabajo (diálogos), marcadores, almohadilla, sello para revisar tareas.", 
  "Libros de Oxford", "Libros de Texto", "Libros de ingles", "Libros de otras editoriales", 
  "Libros de texto y libros digitales", "Libros de texto y manuales", 
  "Libros de texto, Libros de trabajo, diccionarios, lap top, flash cards y posters.", 
  "Libros de texto, hojas de trabajo", "Libros de texto, internet", 
  "Libros de texto, planificaciones del CNB impresas, materiales impresos(fotocopias)", 
  "Libros de textos, folletos", "Libros digitales, computadora, celular", 
  "Libros digitales, marcadores, hojas, computadora.", "Libros videos etc.", 
  "Libros y carteles", "Libros y copias", "Libros y material didáctico", 
  "Libros y material digital audivisual", "Libros y plataformas", 
  "Recursos de internet, Libros de texto", 
  "libros de texto", "libros e internet vídeos", "libros, tecnología, material elaborado por mi", 
  "Tableta, bocina, hojas, marcadores, post it, libros", 
  "Tecnológico y libro de texto",
  "Libros,  libretas, bocina,  pantalla, computadora,  carteles, audio, fichas entre otros", 
  "Libros, Textos, guías.", "Libros, audio, computadora y cañonera", 
  "Libros, audios, videos, imágenes.", "Libros, bocinas, internet", 
  "Libros, computadora, bocinas, cañonera", "Libros, diccionario, redes móviles", 
  "Libros, diccionarios", "Libros, dispositivos electrónicos, cartulinas, hojas  impresas, marcadores, lapiceros, borrador.", 
  "Libros, documentos impresos", "Libros, guias", "Libros, guías, videos, entre otros", 
  "Libros, hojas de trabajo, videos, audios", "Libros, impresiones, internet", 
  "Libros, infografias, carteles, memorias, afiches, impresora, hojas, marcadores", 
  "Libros, información de Internet.", "Libros, internet, diccionarios", 
  "Libros, internet, folleto", "Libros, libros digitales", "Libros, manual de inglés básico", 
  "Libros, marcadores, hojas de trabajo entre otras", "Libros, material audio-visual", 
  "Libros, pizarra, marcadores.", "Libros, videos hojas de trabajo", 
  "Texto . Folleto.", "Texto y folleto.", 
  "Textos", "Textos  y videos", "Textos de mi propiedad, y algunos libros auxiliares de mi propiedad,", 
  "Textos guías...", "Textos, audios", "Textos, enciclopedia, diccionario, programa en linéa.", 
  "Textos, hojas de trabajo, marcadores, grupos de WhatsApp,", 
  "Libros, web , computadoras", "Libros,internet, guías", "Libros. Computadores y celular."
)

### mobiliario
valores_mobiliario_in_propio <- c(
  "Contexto social, salon , materiales y jardin", 
  "Investigación, hojas impresas, libros, pizarrón",
  "Libros, pizarra, marcadores.", "Pizarra", "Pizarra,marcador,hojas de papel bondad,bocina", 
  "Pizarrón"
)

### diccionario
valores_diccionario_in <- c(
  "Diccionario", "Diccionario ingles español", "Diccionario, enciclopedia, Internet, computadora", 
  "Diccionario, revisas.", "Diccionarios", 
  "Libros, diccionario, redes móviles", 
  "Textos, enciclopedia, diccionario, programa en linéa.", 
  "Libros, diccionarios","Libros, internet, diccionarios", 
  "Libros de texto, Libros de trabajo, diccionarios, lap top, flash cards y posters.",
  "HOJAS DE TRABAJO, DICCIONARIOS, OTROS LIBROS",
  "Folletos, hojas de trabajo y diccionario de ingles.", 
  "Folletos, aplicaciones de internet, diccionario, otros",
  "Diccionarios, imagenes, vocabularios, etc.", 
  "Hojas de Trabajo, Diccionario Español-Inglès,",
  "Folletos, hojas de trabajo, diccionario Ingles-Español",
  "Folletos  hojas de trabajo  diccionario español  e inglés"
)

### no especifica 
valores_noespecifica_in_propio <- c(
  "Económicos", "Actualización,", "Ingles basico",
  "Terioria y practico", "experiencia vividas"
)

### folletos y guias
valores_folleto_guias_in <- c(
  "Folleto, bocina,", "Folleto, sitios web", 
  "Texto . Folleto.", "Texto y folleto.",
  "folletos, computadora y otros",
  "Internet, folletos, libros de texto", "Libros,internet, guías",
  "Folletos", "Folletos  hojas de trabajo  diccionario español  e inglés", 
  "Folletos de recopilación temática personal", "Folletos y tros libros", 
  "Folletos,", "Folletos, aplicaciones de internet, diccionario, otros", 
  "Folletos, hojas de trabajo", "Folletos, hojas de trabajo y diccionario de ingles.", 
  "Folletos, hojas de trabajo y libros de texto", "Folletos, hojas de trabajo, diccionario Ingles-Español", 
  "Folletos, marcadores, pizarra, bocinas, videos, etc.",
  "Guia de aprendizaje", "Guia y pñanificador", "Guias digitales", 
  "Guias, libros o folletos", "Guía a de aprendizaje", "Guía de aprendizaje", 
  "Guía y planificador", "Guías", "Libros, internet, folleto", 
  "Laptop, Guías de aprendizaje, libros", 
  "me apoyo con con una guia del gobierno", 
  "Laptop, textos, folletos", "Libros de textos, folletos",
  "Guías de enseñanza del Idioma Inglés y hojas impresas"
)

### ninguno
valores_ninguno_recurso_in <- c(
  "Ninguna", "Ninguno", "No imparto comunicación y lenguaje",
  "ninguno"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    in_recursos_propios = case_match(
      in_recursos_propios,
      all_of(valores_ninguno_recurso_in) ~ "Ninguno",
      all_of(valores_folleto_guias_in) ~ "Folletos y guías",
      all_of(valores_noespecifica_in_propio) ~ "No especifica el recurso",
      all_of(valores_diccionario_in) ~ "Diccionario",
      all_of(valores_mobiliario_in_propio) ~ "Mobiliario",
      all_of(valores_libros_propios_in) ~ "Libros",
      all_of(valores_cnb_in_propio) ~ "CNB",
      all_of(valores_tecnologico_in) ~ "Equipo tecnológico",
      all_of(valores_digital_in) ~ "Recursos digitales",
      all_of(valores_papeleria_propio_in) ~ "Material de papeleria",
      all_of(valores_audio_didactico_propio) ~ "Material audiovisual y didáctico",
      .default = in_recursos_propios
    )
  )

## volver a ver errores

valores_in_recursos_propios <- df_dyd |>
  filter(!(is.na(in_recursos_propios))) |>
  select(in_recursos_propios) |>
  tabyl(in_recursos_propios)

# View(valores_in_recursos_propios)

#in_otro_ultima_vez_libros_mineduc ----
## ver errores
valores_in_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(in_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(in_otro_ultima_vez_libros_mineduc ))) |>
  select(in_otro_ultima_vez_libros_mineduc) |>
  tabyl(in_otro_ultima_vez_libros_mineduc)

# View(valores_in_otro_ultima_vez_libros_mineduc)

## Define los vectores ----
### 2014
valores_in_2014 <- c(
  "2014"
)

### 2015
valores_in_2015 <- c(
  "2015"
)

### 2016
valores_in_2016 <- c(
  "2016"
)

### 2017
valores_in_2017 <- c(
  "2017"
)

### 2018
valores_in_2018 <- c(
  "2018"
)

### 2019
valores_in_2019 <- c(
  "2019", "Año 2019"
)

### 2020

valores_in_2020 <- c(
  "2020", "2020."
)

### 2024
valores_in_2024 <- c(
  "2024"
)

### no recibió
valores_nunca_in <- c(
  "Ninguno", "No he recibido", "No he recibido nunca",
  "No se a recibido", "No se ha recibido material en esta área",
  "No se ha recibido ningún material de esta área"
)

### no recuerda
valores_norecuerda_in <- c(
  "No recuerdo"
)

## Recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    in_otro_ultima_vez_libros_mineduc = case_match(
      in_otro_ultima_vez_libros_mineduc,
      all_of(valores_norecuerda_in) ~ "No recuerda",
      all_of(valores_nunca_in) ~ "No ha recibido",
      all_of(valores_in_2024) ~ "2024",
      all_of(valores_in_2020) ~ "2020",
      all_of(valores_in_2019) ~ "2019",
      all_of(valores_in_2018) ~ "2018",
      all_of(valores_in_2017) ~ "2017",
      all_of(valores_in_2016) ~ "2016",
      all_of(valores_in_2015) ~ "2015",
      all_of(valores_in_2014) ~ "2014",
      .default = in_otro_ultima_vez_libros_mineduc
    )
  )

## volver a ver errores

valores_in_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(in_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(in_otro_ultima_vez_libros_mineduc ))) |>
  select(in_otro_ultima_vez_libros_mineduc) |>
  tabyl(in_otro_ultima_vez_libros_mineduc)

# View(valores_in_otro_ultima_vez_libros_mineduc)

# in_tipo_materiales_recibidos ----

## ver errores

valores_in_tipo_materiales_recibidos <- df_dyd |>
  filter(in_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(in_tipo_materiales_recibidos))) |>
  select(in_tipo_materiales_recibidos) |>
  tabyl(in_tipo_materiales_recibidos)

dput(valores_in_tipo_materiales_recibidos)

## Define los vectores ----

### beca
valores_beca_ingles <- c(
  "Beca de inglés sobre metodología y contenidos"
)

### guia de aprendizaje y planificador
valores_guia_planificador_ingles <- c(
  "GUÍA DE APRENDIZAJE YPLANIFICADOR", "Guia y planifiicador", 
  "Planificador del docente", 
  "Guias", "Guias de aprendizaje", "Guias y planificadores", "Guías y planificadores"
)

### libro
valores_libros_ingles <- c(
  "Libro de comunicación y lenguaje idioma extranjero",
  "Textos"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    in_tipo_materiales_recibidos = case_match(
      in_tipo_materiales_recibidos,
      all_of(valores_libros_ingles) ~ "Libros",
      all_of(valores_guia_planificador_ingles) ~ "Guía y planificador del facilitador",
      all_of(valores_beca_ingles) ~ "Beca de inglés",
      .default = in_tipo_materiales_recibidos
    )
  )

## volver a ver errores

valores_in_tipo_materiales_recibidos <- df_dyd |>
  filter(in_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(in_tipo_materiales_recibidos))) |>
  select(in_tipo_materiales_recibidos) |>
  tabyl(in_tipo_materiales_recibidos)

# View(valores_in_tipo_materiales_recibidos)

#cm_libros_primero ----

## ver errores

valores_cm_libros_primero <- df_dyd |>
  filter(cm_uso_libros == "Sí") |>
  filter(cm_grados == "Primer grado") |>
  filter(!(is.na(cm_libros_primero))) |>
  select(cm_libros_primero) |>
  tabyl(cm_libros_primero)

dput(valores_cm_libros_primero)

## Define los vectores ----

### valores culturas
valores_cultura_guate <- c(
  "Culturas de Guatemala", "Culturas e idiomas  Mayas  de Guatemala"
)

### el rostro y el ser de los 4 pueblos
valores_ser4_pueblos <- c(
  "El Ser de las cuatro culturas", "El ser de los cuatro pueblos"
)

### gramatica qeqchi

valores_qeqchi <- c(
  "Gramática q'eqchi'"
)

### guia y planificador
valores_guia_planificador_cm <- c(
  "Guia y planificador", "Guias y planificadores",
  "Planificador y guías de aprendizaje"
)

### guia metodologia idioma maya
valores_idioma_maya <- c(
  "Guía Metodológica de Idiomas Mayas 1°. Básico"
)

### guia pedagogia lenguas mayas
valores_guia_lengua_maya <- c(
  "Guías pedagógicas de la academia de lenguas mayas."
)

### idiomas
valores_idiomas <- c(
  "Idiomas"
)

### interculturalidad para jovenes
valores_inter_jovenes <- c(
  "LIBRO DE INTERCULTURALIDAD PARA JOVENES",
  "LIBRO DE INTERCULTURALIDAD PARA JOVENES",
  "Texto de educación  Intercultural para jóvenes"
)

### la cultura gastronomica
valores_cultura_gastronomica <- c(
  "La Cultura Gastronómica de los cuatro pueblos: Maya, Gsrífuna Xinca y Ladino"
)

### lenguas cultura maya
valores_lengua_cultura_maya <- c(
  "Lengua de cultura maya"
)

### libro poqomam
valores_poqomam <- c(
  "Libro Poqomam"
)

### descolonización y educación bilingüe
valores_tijonik <- c(
  "Tijonik"
)

### sin especificar
valores_sinespecificar_cm <- c(
  "Modulo 1"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cm_libros_primero = case_match(
      cm_libros_primero,
      all_of(valores_sinespecificar_cm) ~ "No especifica el nombre",
      all_of(valores_tijonik) ~ "Descolonización y educación bilingüe",
      all_of(valores_poqomam) ~ "Libro Poqomam",
      all_of(valores_lengua_cultura_maya) ~ "Lengua de Cultura Maya",
      all_of(valores_cultura_gastronomica) ~ "La Cultura Gastronómica de los cuatro pueblos",
      all_of(valores_inter_jovenes) ~ "Educación  Intercultural para jóvenes",
      all_of(valores_idiomas) ~ "Idiomas",
      all_of(valores_guia_lengua_maya) ~ "Guía pedagógica de la academia de lenguas mayas",
      all_of(valores_idioma_maya) ~ "Guía Metodológica de Idiomas Mayas",
      all_of(valores_guia_planificador_cm) ~ "Guía y Planificador del facilitador",
      all_of(valores_qeqchi) ~ "Gramática q'eqchi'",
      all_of(valores_ser4_pueblos) ~ "El rostro y el ser de los cuatro pueblos de Guatemala",
      all_of(valores_cultura_guate) ~ "Culturas de Guatemala",
      .default = cm_libros_primero
    )
  )

## volver a ver errores

valores_cm_libros_primero <- df_dyd |>
  filter(cm_uso_libros == "Sí") |>
  filter(cm_grados == "Primer grado") |>
  filter(!(is.na(cm_libros_primero))) |>
  select(cm_libros_primero) |>
  tabyl(cm_libros_primero)

# View(valores_cm_libros_primero)

# cm_libros_segundo ----
valores_cm_libros_segundo <- df_dyd |>
  filter(cm_uso_libros == "Sí") |>
  filter(cm_grados == "Segundo grado") |>
  filter(!(is.na(cm_libros_segundo))) |>
  select(cm_libros_segundo) |>
  tabyl(cm_libros_segundo)

dput(valores_cm_libros_segundo)


## Define los vectores ----

### aprender mam
valores_aprender_mam <- c(
  "Aprender Mam"
)

### cnb
valores_cnb_cm <- c(
  "CnB"
)

### gramatica q'eqchi'
valores_gramatica_qeqchi <- c(
  "Gramática del idioma Q'eqchi'"
)

### gramatica pedagogica itza
valores_gramatica_itza <- c(
  "Gramática pedagógica Itza"
)

### guia de aprendizaje
valores_guia_cm <- c(
  "Guía de aprendizaje", "Guías."
)

### interculturalidad para jóvenes
valores_inter_jovenes_2 <- c(
  "Interculturalidad para Jóvenes", 
  "Libro de Interculturalidad para jóvenes. Libro de Kaqchikel."
)

### academia de lengua mayas
valores_academia_maya <- c(
  "Libros de la academia  de lengua mayas"
)

### ri nutinamit guatemala

valores_rinutinamit <- c(
  "Ri nutinamit Guatemala"
)

### diccionario de kiche
valores_diccionario_kiche <- c(
  "Un diccionario de kiche"
)

### usak' q'at
valores_usakqat <- c(
  "Usak' q'at"
)

### páginas web
valores_web_cm <- c(
  "Google","Páginas web", "Recursos en la página web del MINEDUC"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cm_libros_segundo = case_match(
      cm_libros_segundo,
      all_of(valores_web_cm) ~ "Páginas web",
      all_of(valores_usakqat) ~ "Usak' q'at",
      all_of(valores_diccionario_kiche) ~ "Diccionario K'iche'",
      all_of(valores_rinutinamit) ~ "Ri nutinamit Guatemala",
      all_of(valores_academia_maya) ~ "Libro de la Academia de Lenguas Mayas",
      all_of(valores_inter_jovenes_2) ~ "Educación Intercultural para Jóvenes",
      all_of(valores_guia_cm) ~ "Guía de aprendizaje",
      all_of(valores_gramatica_itza) ~ "Gramática pedagógica Itza",
      all_of(valores_gramatica_qeqchi) ~ "Gramática Q'eqchi'",
      all_of(valores_cnb_cm) ~ "CNB",
      all_of(valores_aprender_mam) ~ "Aprender mam",
      .default = cm_libros_segundo
  )
)

## volver a ver errores

valores_cm_libros_segundo <- df_dyd |>
  filter(cm_uso_libros == "Sí") |>
  filter(cm_grados == "Segundo grado") |>
  filter(!(is.na(cm_libros_segundo))) |>
  select(cm_libros_segundo) |>
  tabyl(cm_libros_segundo)

# View(valores_cm_libros_segundo)

#cm_libros_tercero ----


valores_cm_libros_tercero <- df_dyd |>
  filter(cm_uso_libros == "Sí") |>
  filter(cm_grados == "Tercer grado") |>
  filter(!(is.na(cm_libros_tercero))) |>
  select(cm_libros_tercero) |>
  tabyl(cm_libros_tercero)

dput(valores_cm_libros_tercero)

## Define los vectores ----

### asociacion para el desarrollo integral
valores_desarrollo_integral <- c(
  "Asociacion para el Desarrollo Integral"
)

### CNB
valores_cnb_cm_2 <- c(
  "CNB tradicional"
)

### conversemos en xinka
valores_conversa_xinka <- c(
  "Conversemos en Xinka"
)

### culturas e idiomas xincas
valores_cultura_idioma <- c(
  "Culturas e Idiomas xincas"
)

### el rostro y ser de los cuatro pueblos
valores_rostro_ser <- c(
  "El Rostro y el ser de los cuatro Pueblos"
)

### no utiliza libros
valores_nolibros_cm <- c(
  "FOLLETOS IMPRESOS", 
  "Folletos", "Folletos de Kaqchikel",
  "No hay libros"
)

### guias de aprendizaje
valores_guia_aprendizaje_cm <- c(
  "Guía de aprendizaje", 
  "Guías metodológicas de cultura maya"
)

### no especifica el nombre
valores_noespecifica_cm_2 <- c(
  "Libros digitales", 
  "Libros proporcionados como proyecto, por estudiantes universitarios, y libros propios comprados"
)

### páginas web
valores_web_cm2 <- c(
  "Recursos en la pagina web del Mineduc"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cm_libros_tercero = case_match(
      cm_libros_tercero,
      all_of(valores_web_cm2) ~ "Páginas web",
      all_of(valores_noespecifica_cm_2) ~ "No especifica el nombre",
      all_of(valores_nolibros_cm) ~ "No utiliza libros",
      all_of(valores_rostro_ser) ~ "El rostro y el ser de los cuatro pueblos de Guatemala",
      all_of(valores_cultura_idioma) ~ "Culturas e idiomas xincas",
      all_of(valores_guia_aprendizaje_cm) ~ "Guía de aprendizaje",
      all_of(valores_conversa_xinka) ~ "Conversemos en xinca",
      all_of(valores_cnb_cm_2) ~ "CNB",
      all_of(valores_desarrollo_integral) ~ "Asociación para el Desarrollo Integral",
      .default = cm_libros_tercero
    )
    )

## volver a ver errores

valores_cm_libros_tercero <- df_dyd |>
  filter(cm_uso_libros == "Sí") |>
  filter(cm_grados == "Tercer grado") |>
  filter(!(is.na(cm_libros_tercero))) |>
  select(cm_libros_tercero) |>
  tabyl(cm_libros_tercero)

# View(valores_cm_libros_tercero)

# cm_recursos_centro_educativo ----

## ver errrores
valores_cm_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(cm_recursos_centro_educativo))) |>
  select(cm_recursos_centro_educativo) |>
  tabyl(cm_recursos_centro_educativo)

dput(valores_cm_recursos_centro_educativo)

## Define los vectores ----

### ninguno

valores_ninguno_cm <- c(
  "0",  "N/A", "NINGUNO", "Nada", "Nada, uno mismo tiene investigar", 
  "Ninguna", "Ninguno", "Ningún", "No", "No hay", "No lo imparto",
  "nada", "ninguno", 
  "No tiene",   "Todos los materiales es por propia cuenta ya que el centro Educativo no me ha facilitado ningún material"
)

### no especifica
valores_noespecifica_cm <- c(
  "1", "Del ministerio de cultura",  "Diferentes fuentes",
  "Módulos",  "Practico y teorico", "Recurrir diferentes medios"
)

### libros
valores_libros_cm <- c(
  "1 libro", "1 libro de texto", 
  "1 libro metodológico proporcionado como proyecto por estudiantes universitarios en años anteriores.",
  "Libro", "Libro de Garifuna", 
  "El colegio da los libros",
  "Ĺibros de texto,  Google",
  "El diccionario, libros, CNB",
  "libro", "libro de texto", "libros de cooperativa el recuerdo", 
  "libros, carteles,  diapositivas,  conversacines en pareja", 
  "Libro de Interculturalidad para jóvenes", "Libro de Q'anjob'al", 
  "Libro de lectura sobre historias culturales", "Libro de mam", 
  "Libro de texto", "Libro de texto internet", "Libro de texto y revistas", 
  "Libro de vocabulario básico", "Libro y material impreso, videos, imágenes entre otros", 
  "Libro, entregado como proyecto por estudiantes universitarios", 
  "Libro, hojas de trabajo, humano", "Libro, impresora", "Libros", 
  "Libros de Textos, televisión y tablet.", "Libros de texto", 
  "Algunos libros que ha mandado el MINEDUC", 
  "Aprendamos Kaqchikel 3, lengua Maya 3",
  "Cultura e Idiomas Xinca", 
  "Libro", "Libro de Garifuna", 
  "Libro de Interculturalidad para jóvenes", "Libro de Q'anjob'al", 
  "Libro de lectura sobre historias culturales", "Libro de mam", 
  "Libro de texto", "Libro de texto internet", "Libro de texto y revistas", 
  "Libro de vocabulario básico", "Libro y material impreso, videos, imágenes entre otros", 
  "Libro, entregado como proyecto por estudiantes universitarios", 
  "Libro, hojas de trabajo, humano", "Libro, impresora", "Libros", 
  "Libros de Textos, televisión y tablet.", "Libros de texto", 
  "Libros de texto y materiales de apoyo de la DIGEBI", "Libros de texto, televisión y tablet.", 
  "Libros de textos", "Libros de textos adicionales, copias, impresiones.", 
  "Libros diversos", "Libros fisicos", "Libros y Textos", "Libros y material didáctico", 
  "Libros y materiales", "Libros y por fuera una docente del sector oficial me brindó un módulo de dicha área. También busco información en una página del museo  de Cuilpa", 
  "Libros,", "Libros, diccionario.", "Libros, internet, computadora cañonera, marcadores", 
  "Libros.  Practica","Ri nutinamit Guatemala",
  "La comunicación en el Idioma, práctica", "Un libro de cultura",
  "Texto y copias.", "Textos de Poqomchi´, Didácticos y Tecnologicos", 
  "Textos en poqomam", "Textos y material Digital", 
  "Vocabularios y Libros", 
  "MODULO DE EDUCACION XINKA", "Tecnogia, diccionario , libros", "Texto", 
  "Kemon Chabal Lecto-escritura DIGEBI", "L2 cómo idioma Mam", 
  "LIBRO", "LIBRO DE INTERCULTURALIDAD PARA JOVENES", "LIBRO DE TEXTO",
  "Diccionario, Libro de Aritmética, Constitución Política",
  "Libros de texto y materiales de apoyo de la DIGEBI", "Libros de texto, televisión y tablet.", 
  "Libros de textos", "Libros de textos adicionales, copias, impresiones.", 
  "Libros diversos", "Libros fisicos", "Libros y Textos", "Libros y material didáctico", 
  "Libros y materiales", "Libros y por fuera una docente del sector oficial me brindó un módulo de dicha área. También busco información en una página del museo  de Cuilpa", 
  "Libros,", "Libros, diccionario.", "Libros, internet, computadora cañonera, marcadores", 
  "Libros.  Practica", "CNB y un libro de texto", "Cnb, libro"
)

### material audiviosual y didáctico

valores_audio_didactico_cm <- c(
  "Alguien materiales didácticos",
  "Aterial Mediado", "Audiovisuales",
  "Copias, marcador, hojas", 
  "Textos de Poqomchi´, Didácticos y Tecnologicos",  "Textos y material Digital",
  "Libros y material didáctico", "valija dicactica",
  "Libros y materiales","Recursos audiovisual",
  "Los necesarios para materiales didácticos", 
  "Folletos audios videos etc","Revistas, folletos.",
  "La guía metodológica y lotería Xinka",
  "Hojas de trabajo", "Hojas de trabajo e investigaciones", 
  "Hojas de trabajo en internet", "Hojas de trabajo tintas impresoras", 
  "Hojas de trabajo.","Valija didáctica", "Videos y carteles", 
  "Material impreso", "Materiales didácticos", "Memoria, la utilización del dado, rompecabeza, sopa de letra, hojas, dibujos", 
  "Material didactico", 
  "Material didáctica, y otros", "Material didáctico", 
  "Libro y material impreso, videos, imágenes entre otros", 
  "Didácticos y tecnológicos","Fotocopias, Valija Didáctica.",
  "Carteles", "Carteles loterías",
  "Computadora e impresora, pizarrón, marcadores y hojas",
  "Contenidos", "Copias", "Copias, audiovisuales",
  "Cañonera, pizarra, marcadores, hojas, laboratorio de computación, uso de biblioteca"
  
)

### cnb
valores_cnb_recurso <- c(
  "CNB y un libro de texto", "CNB, libros y páginas web",
  "Cnb", "Cnb, libro",  "El diccionario, libros, CNB",
  "solo el CNB"
)

### mobiliario
valores_mobiliario_cm <- c(
  "Cajonera. Laptoop. Pizarra y marcadores",
  "Pizarra", "Pizarra y marcadores.", "Pizarra, pantalla", 
  "Pizarrón", "Pizarrón, marcadores, hojas tamaño carta y oficio etc", 
  "Pizarrón, marcadores, hojas.", "Pizarrón,almohadilla,libros"
)

### equipo tecnologico
valores_tecnologia_cm <- c(
  "Cajonera. Laptoop. Pizarra y marcadores",
  "Cañonera", "Cañonera y computadora", "Cañonera, laptop, pizarra.", 
  "Cañonera, pizarra, marcadores, hojas, laboratorio de computación, uso de biblioteca", 
  "Centro de computación", "Libros de Textos, televisión y tablet.",
  "Laboratorio de computación", "Tecnogia, diccionario , libros", "Texto", 
  "Pizarra, pantalla", "Proyector",  "Tv",
  "televisión",
  "Textos de Poqomchi´, Didácticos y Tecnologicos",
  "Pantalla plana", "Pantalla, Impresora", 
  "Libros, internet, computadora cañonera, marcadores", 
  "Libros de texto, televisión y tablet.", 
  "Computadora", "Hojas, impresiones, proyector","Impresora, cañonera y pizarra",
  "Equipo de cómputo, libros, equipo de sonido, equipo audio visual",
  "Computadora, cañonera, fotocopias",
  "Computadora e impresora, pizarrón, marcadores y hojas", 
  "Computadora, Hojas, tinta impresara", "Computadora, cañonera, fotocopias"
)

### diccionario
valores_diccionario_cm <- c(
  "Diccionario de idioma kiche español", 
  "Diccionario, Libro de Aritmética, Constitución Política", 
  "Diccionario, dosificación","diccionario de traduccion", 
  "Libros, diccionario.",  "Tecnogia, diccionario , libros", "Texto", 
  "Folletos en PDF, páginas webs, diccionario de palabras en kiche",
  "El diccionario, libros, CNB"
  
)

### recursos digitales
valores_digital_cm <- c(
  "Digitales",  "Internet",  "Material digital", 
  "Malla curricular, y páginas web", 
  "Investigaciones", "Investigaciones  e impresiones  entre otros", 
  "Investigaciones, vocabularios ilustrados", "Investigación de Historias",
  "Folletos en PDF, páginas webs, diccionario de palabras en kiche",
  "Enlaces que me dirigen a la pagina del mineduc", 
  "PDF o páginas web", "Pdf",  "Web",
  "Enlaces que me dirigen a la página del MINEDUC"
)

### folletos y guias
valores_folletos_guias_cm <- c(
  "Folleto", "Folleto, pizarra, marcadores entre otros.", "Folletos", 
  "Folletos audios videos etc", "Folletos de vocabulario", 
  "Guias", "Guía Metodológica para el aprendizaje del Idioma Achi", 
  "Guía de aprendizaje", "Guía metodológica y loteria Xinka", 
  "Guías",
  "Folletos en PDF, páginas webs, diccionario de palabras en kiche"
)

### material de papeleria
valores_papeleria_cm <- c(
  "Fotocopias, marcadores, tinta",
  "Hojas, impresiones, proyector", "Impresiones", 
  "Impresiones, fotocopias",  "Sacar copias",
  "impresiones, fotocopias",
  "impresiones, fotocopias, hojas de colores, crayones, hojas bond,",
  "Pizarrón, marcadores, hojas tamaño carta y oficio etc", 
  "Pizarrón, marcadores, hojas.", "Pizarrón,almohadilla,libros", 
  "Marcador y almohadilla", "Marcador, almohadilla, pizzara, cartulina, videos.", 
  "Marcadores y cartulinas", "Marcadores y papel", "Marcadores, almohadilla, lapiceros, hojas, fotocopias, engrapadora, tijeras", 
  "Marcadores, hojas, almuhadia, cuaderno,tinta, lápiz y lapicero", 
  "Marcadores, proyector, hojas, cartulina.",
  "Hojas", "Hojas bon", "Hojas de colores cuadernor, piedra de moler."
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cm_recursos_centro_educativo = case_match(
      cm_recursos_centro_educativo,
      all_of(valores_papeleria_cm) ~ "Material de papelería",
      all_of(valores_folletos_guias_cm) ~ "Guías y folletos",
      all_of(valores_digital_cm) ~ "Recursos digitales",
      all_of(valores_diccionario_cm) ~ "Diccionario",
      all_of(valores_tecnologia_cm) ~"Equipo tecnológico",
      all_of(valores_mobiliario_cm) ~ "Mobiliario",
      all_of(valores_cnb_recurso) ~ "CNB",
      all_of(valores_audio_didactico_cm) ~ "Material audiovisual y didáctico",
      all_of(valores_libros_cm) ~ "Libros",
      all_of(valores_noespecifica_cm) ~ "No especifica el recurso",
      all_of(valores_ninguno_cm) ~ "Ninguno",
      .default = cm_recursos_centro_educativo
    )
  )

## volver a ver errores
valores_cm_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(cm_recursos_centro_educativo))) |>
  select(cm_recursos_centro_educativo) |>
  tabyl(cm_recursos_centro_educativo)

# View(valores_cm_recursos_centro_educativo)

# cm_recursos_propios ----

## ver errrores
valores_cm_recursos_propios <- df_dyd |>
  filter(!(is.na(cm_recursos_propios))) |>
  select(cm_recursos_propios) |>
  tabyl(cm_recursos_propios)

dput(valores_cm_recursos_propios)

## Define los vectores ----

### ninguno
valores_ninguno_cm2 <- c(
  "0",   "Ninguno", 
  "No", "No se cuenta con material"
)

### no especifica 
valores_noespecifica_cm_3 <- c(
  "1",  "Cooperativa el recuerdo", "Económicos",
  "La mayoría",   "La sociedad",  "MODULO DE EDUCACION", 
  "Módulos", "Neologismos",   "Todo", "Todos",
  "Poco", "Practico y teorico"
)

### material audiovisual y didáctico
valores_audio_didactico_cm2 <- c(
  "Afiches", "Afiches e impresiones",
  "Audios, imágenes y textos",  "Didáctico", 
  "Libros y material didáctico",
  "Vestuario, para identificar, materiales",
  "Videos", "Videos y carteles", 
  "internet, libros de texto, discos, impresiones, material didáctico", 
  "Videos y pantalla, tv..", "Revistas, hojas de trabajo",
  "material didactico. folletos,", "Tecnológicos y material didáctico",
  "Recortes, internet, marcadores", "Recursos audiovisual", 
  "Libros, audios, imágenes.",  "Maqueta",
  "Libros, impresiones. carteles.", "Textos, loterías, domino fichas bingo",
  "Presentaciones digitales", "Presentaciones, cantos", "Presentaciones.",
  "Material impartido en capacitaciones recibidas", "Material reciclado", 
  "Materiales de Idioma Kiche y otros recursos audiovisuales", 
  "Materiales impresos", "Maya curricular", "Maya curricular, planificación", 
  "Material audiovisual, material didáctico físico", "Material didáctico", 
  "Material didáctico digital y físico", "Material didáctico variado", 
  "Material didáctico, marcadores, hojas bond y libros propios.", 
  "Material elaborado como cards y otros", 
  "Vocabulario Maya Achi, Folletos de ALMG, Gramática del idioma Achi", 
  "Vocabulario de mi autoría", "Vocabularios y libros extras", 
  "Libros, juegos", "Libros, lotería, dados, bingo, bocinas, computadora, hojas marcadores", 
  "Libros, materiales didácticos, internet, copias.", "Libros, materiales didácticos, juegos", 
  "Libros, carteles, imágenes, videos", "Libros, copias audiovisuales", 
  "Libros y materiales", "Libros,  marcadores ,  papelografos", 
  "Libros, CNB, impresos, cartulinas", "Libros, Carteles e ihnternet", 
  "Computadora, libros, recursos ,naturales, caridade de elementos del entorno natural", 
  "COPIAS, JUEGOS, ACTIVIDADES, LIBRO", "Exposición", 
  "Dados, mapas, comidas bailes", "Dados, mapas, comidas, revistas", 
  "Audios, videos, imágenes", "Cartelome",
  "Hojas . Libros  . Carteles", "Hojas de trabajo", "Hojas de trabajos", 
  "Hojas marcadores papelografos", "Libro de texto y audios", "Libro extra imágenes e internet", 
  "Juegos de mesa, libros, folletos", 
  "Impresiones y el celular", "Impresiones, internet, hojas, luz, computadora, tiempo en contra jornada rnada",
  "Folletos libros hojas videos audios etc", 
  "Carteles", "Carteles copias impresiones", 
  "Investigación  en Internet, diccionario, afiches , libros de historia y cultura", 
  "Investigación hojas de trabajo", 
  "Carteles marcadores imágenes", "Carteles, cuaderno, material lúdico", 
  "Carteles, imágenes impresas, videos, audios, libros de o guías de lingüística Xinka, otros.", 
  "Carteles, imágenes, videos, audios, libros lingüisticos", 
  "Carteles, juegos de mesa", "Carteles, marcadores, escala de rango, material didáctico", 
  "Bocina, juegos interactivos"
)

### guias y folletos
valores_guia_folleto_cm <- c(
  "Algunos folletos y el Internet", "Audios, folletos",
  "Computadora, folletos,",
  "Elaboración de folletos",
  "foleltos de investigacion", 
  "Pc, guías de aprendizaje", 
  "Libros, folletos y página web", "Libros, folletos, diccionarios Linros de lectura en idioma Kaqchikel", 
  "Libros, guia metodologica del area cultura e idiomas maya, garifuna o xinkaimpresiones, carteles, computadora,  marcadores, hojas bond, periodico, pegamento, tijeras, etc.", 
  "Libros, guías, cuaderno etc", "Láminas con imágenes, folletos", 
  "Libros folleto", "Libros folletos", 
  "Investigo y creo guías según cnb",
  "Folleto", "Folleto elaborado de acuerdo a la malla curricular", 
  "Folletos", "Folletos  hojas de trabajo", "Folletos adicionales", 
  "Folletos de capacitación recibida", "Folletos de investigaciones", 
  "Folletos de vocabulario", "Folletos de vocabularios", "Folletos e investigaciones", 
  "Folletos libros", "Folletos libros de xinca", "Folletos libros hojas videos audios etc", 
  "Folletos y guias", "Folletos y hojas de trabajo", "Folletos y libros", 
  "Folletos y páginas web", "Folletos, ODEC, Internet, un libro del MINEDUC,", 
  "Folletos, Pizarra, Libro",
  "Guias", "Guía de aprendizaje de la cooperativa el recuerdo de san Pedro Pinula jalapa", 
  "Guías", "Guías de trabajo", "Guías, hojas de trabajo", 
  "Diapositivas, folletos de la academia de Lenguas Mayas", 
  "Cuaderno donde he formado de manera clara los contenidos", 
  "CNB digital, internet, folletos", "Computadora portátil, folletos"
)

### equipo tecnologico
valores_tecnologia_cm2 <- c(
  "Bocina, juegos interactivos","COMPUTADORA",
  "Cañonera, teléfono,papelografo, folletos", 
  "Libros computadoras",   "Pantalla", 
  "Videos y pantalla, tv..", 
  "Teléfono, internet libros digitales",
  "Tecnologicos", "Tecnología",
  "Tecnológicos y Libros de Texto", 
  "Tecnológicos y material didáctico",
  "computadora, internet. hojas de colores, impresiones", 
  "Libros, usb", "Libros. Cañonera, bocina.", "Libros. Cnb, mayas, impresiones. Videos.", 
  "Libros, portátil, Internet, cañonera etc.", 
  "Libros, revistas y computadora.", 
  "Papel iris, impresiones, computadora, bocina, teléfono", 
  "Libros, Internet, redes sociales, telefono, computadora etc.", 
  "Libros varios sobre Cultura e Idiomas Mayas, libros de la gramatica del idioma Kaqchikel, dicionarios en kaqchikel, PDFs descargables, uso del internet, celular, computdora, impresora.", 
  "Libros digitales y físico, portátil, cañonera, internet , bocina etc.", 
  "Google, computadora, entre otros","Laptop e internet",
  "Investigaciones, impresiones hojas de trabajo, cañonera entre otros", 
  "Impresiones y el celular", "Impresiones, internet, hojas, luz, computadora, tiempo en contra jornada rnada", 
  "Cañoneras y computadora", "Celular", "Computadora", "Computadora . Uso internet", 
  "Computadora con internet", "Computadora e impresora", "Computadora e impresora, asistencia, teléfono", 
  "Computadora impresora entre otros", "Computadora portátil, folletos", 
  "Computadora y teléfono", "Computadora, Internet", "Computadora, Internet e impresora", 
  "Computadora, bocinas", "Computadora, cañonera, impresora, marcadores", 
  "Computadora, folletos,", "Computadora, fotocopias", "Computadora, impresora, carteles, juegos", 
  "Computadora, impresora, tablet, celular, libros y guías", 
  "Computadora, internet, impresiones, etc.", "Computadora, investigaciones", 
  "Computadora, libros, recursos ,naturales, caridade de elementos del entorno natural", 
  "Computadora, libros, textos, diccionario y documentos", 
  "Computadora, teléfono, internet, hojas etc", "Computadora,teléfono, Internet para investigar"
)

### Libros
valores_libros_cm2 <- c(
  "Capacitación, libros",
  "El libro de Popol Vhu y libros de culturas.", 
  "Copias, libros, videos",  "Gramática q'e qchi'", 
  "Idioma chorti y Cultitas de Guatemala", 
  "LIBROS ADICIONALES E INTERNET", 
  "Material en digital y libros", 
  "Trifoliares", "Ukab' q'at",
  "pronabi Guatemala 1995 numerales mayas",
  "enciclopedias de cultura guatemalteca.", 
  "Temas investigados ,imternet hojas ,libros", 
  "Páginas Web, equió de computo. Libro para enseñar el idioma kaqchikel", 
  "Páginas web y libros de kaqchikel de la editora maya wuj", 
  "RECUERSOS DE LA ACADEMIA DE LENGUAS MAYAS DE GUATEMALA", 
  "LIBROS DE LA MATERIA, LAPTOP E INTERNET", "LIBROS DE TEXTO, HOJAS DE TRBAJO E IMAGENES IMPRESAS DE VOCABULARIO", 
  "LIBROS DE TEXTO,PERIÓDICO, COMPUTADORA", 
  "Google y libros que compro", "Google, Libros de texto",
  "Texto y copias.", "Texto y folleto.", "Textos del poqomchi´, Didácticos y Tecnológicos", 
  "Textos digitales", "Textos, computadora,", "Textos, loterías, domino fichas bingo", 
  "Folletos, ODEC, Internet, un libro del MINEDUC,", 
  "Computadora, libros, recursos ,naturales, caridade de elementos del entorno natural", 
  "Lecto-escritura, Gramática Normativa proporcionado por la Comunidad Lingüística Tektiteka", 
  "Lengua y cultura maya", "Libro", "Libro  e Internet", "Libro L2 cómo idioma Mam", 
  "Libro de Academia Maya  y otros.", "Libro de cooperativa el recuerdo", 
  "Libro de gramática Q'eqchi' y hojas de trabajo", "Libro de idioma al maya qeqchi", 
  "Libro de información cultura xinca", "Libro de intercuralidad.", 
  "Libro de kiche, hojas de trabajo de plataformas virtuales", 
  "Libro de texto", "Libro de texto presentación digital", 
  "Libro de texto y audios", "Libro e internet", "Libro extra imágenes e internet", 
  "Libro, Internet y folletos", "Libro, diccionario, internet", 
  "Libro, módulo, investigo en Google.", "Libros", "Libros computadoras", 
  "Libros de Texto", "Libros de Texto, Diccionario. Teléfono", 
  "internet, libros de texto, discos, impresiones, material didáctico", 
  "libro", "libro diapositivas", "libros y planificador de Xinka", 
  "libros, copias",
  "Libros de cultura, hojas de trabajo", "Libros de la cooperativa el recuerdo", 
  "Libros de lecto escritura de diferentes niveles", "Libros de números, y diccionario propio la cual compre en la ALMG del área Ixil", 
  "Libros de texto", "Libros de texto Cultura Maya", "Libros de texto e internet", 
  "Libros de texto, internet", "Libros de textos", "Libros de textos y computadora", 
  "Libros de textos, número y diccionario del idioma Ixil, comprado en la ALMG, del área ixil", 
  "Libros de textos, proyector", "Libros del textos", "Libros digitales", 
  "Libros digitales y físico, portátil, cañonera, internet , bocina etc.", 
  "Libros folleto", "Libros folletos", "Libros marcadores y pizarra", 
  "Libros otorgados por cooperativa Tonantel", "Libros propios del idioma y páginas web verificadas.", 
  "Libros propios, diccionarios", "Libros relacionado con los 4 pueblos", 
  "Libros varios sobre Cultura e Idiomas Mayas, libros de la gramatica del idioma Kaqchikel, dicionarios en kaqchikel, PDFs descargables, uso del internet, celular, computdora, impresora.", 
  "Libros y diccionario", "Libros y diccionarios", "Libros y documentos en linea", 
  "Libros y folletos", "Libros y material didáctico", "Libros y material digital", 
  "Libros y materiales", "Libros,  marcadores ,  papelografos", 
  "Libros, CNB, impresos, cartulinas", "Libros, Carteles e ihnternet", 
  "Libros, Internet, cuaderno etcétera.", "Libros, Internet, redes sociales, telefono, computadora etc.", 
  "Libros, afiches, investigaciones etc.", "Libros, audios, imágenes.", 
  "Libros, carteles, imágenes, videos", "Libros, copias audiovisuales", 
  "Libros, diccionarios y links proporcionados por La Academia de alergias Mayas de Guatemala", 
  "Libros, documento", "Libros, folletos y página web", "Libros, folletos, diccionarios Linros de lectura en idioma Kaqchikel", 
  "Libros, guia metodologica del area cultura e idiomas maya, garifuna o xinkaimpresiones, carteles, computadora,  marcadores, hojas bond, periodico, pegamento, tijeras, etc.", 
  "Libros, guías, cuaderno etc", "Libros, impresiones. carteles.", 
  "Libros, interne, capacitaciones", "Libros, internet", "Libros, internet, folletos", 
  "Libros, juegos", "Libros, lotería, dados, bingo, bocinas, computadora, hojas marcadores", 
  "Libros, materiales didácticos, internet, copias.", "Libros, materiales didácticos, juegos", 
  "Libros, portátil, Internet, cañonera etc.", "Libros, revistas y computadora.", 
  "Libros, sitios web, pdf y hojas de trabajos", "Libros, traductores", 
  "Libros, usb", "Libros. Cañonera, bocina.", "Libros. Cnb, mayas, impresiones. Videos."
)

### cnb
valores_cnb_cm <- c(
  "CNB digital, internet, folletos", "CNB y del Internet",
  "Currículo Nacional Base CNB de Guatemala libros digitales PDF", 
  "En tercero solo cnb","Investigaciones, CNB"
  
)

### recurso digital
valores_digital_cm2 <- c(
  "Canal de Youtube",   "Digitales", "Documentos digitales y diccionario", 
  "Diapositivas, folletos de la academia de Lenguas Mayas", 
  "Folletos y páginas web", "Libros y documentos en linea", 
  "Libro de kiche, hojas de trabajo de plataformas virtuales", 
  "Libro de texto presentación digital",  "Web", 
  "Temas investigados ,imternet hojas ,libros", 
  "Páginas Web, equió de computo. Libro para enseñar el idioma kaqchikel", 
  "Páginas web", "Páginas web y libros de kaqchikel de la editora maya wuj", 
  "Libros y material digital",  "Material en digital y libros", 
  "Libros, sitios web, pdf y hojas de trabajos",
  "Libros, interne, capacitaciones", "Libros, internet", "Libros, internet, folletos", 
  "Libros, diccionarios y links proporcionados por La Academia de alergias Mayas de Guatemala", 
  "Información de internet", "Información web", "Internet", 
  "Internet,", "Internet, libro de texto", "Internet, libros redes sociales", 
  "Internet,hojas computadora y teléfono", "Investigaciones", 
  "Investigaciones de páginas web", "Investigaciones del internet", 
  "Investigaciones en digital y hojas de trabajo", "Investigaciones en la www según cnb", 
  "Investigaciones web", "Investigaciones, CNB", "Investigaciones, impresiones hojas de trabajo, cañonera entre otros", 
  "Investigación  en Internet, diccionario, afiches , libros de historia y cultura", 
  "Investigación hojas de trabajo", "Investigación propia",
  "PDF", "Paginas web","Pdf",  "Recursos de la Web", 
  "Investigar en internet", "Investigo y creo guías según cnb", 
  "Google y libros que compro", "Google, Libros de texto", 
  "Google, computadora, entre otros", "Medios digitales",
  "Consulto algunas páginas confiables en internet, me asesoro de algunos amigos que hablan el idioma, consulto en libros de mi propiedad, me asesoro con amigos que dan el mismo curso en otros establecimientos."
)

### material papeleria
valores_papeleria_cm2 <- c(
  "Dibujos, marcadores, crayones, cartulinas, etc",
  "Fotocopias, impresiones, marcadores, almohadilla",
  "Marcador, almohadilla, pizzara, cartulina, videos.", 
  "Marcadores, temas de investigación", "Marcadores,hojas, investigación", 
  "Libros, guia metodologica del area cultura e idiomas maya, garifuna o xinkaimpresiones, carteles, computadora,  marcadores, hojas bond, periodico, pegamento, tijeras, etc."
  
)

### diccionario
valores_diccionario_cm2 <- c(
  "Diccionario de la Academia de Lenguas Mayas", 
  "Documentos digitales y diccionario", 
  "Un diccionario",
  "Libros y diccionario", "Libros y diccionarios",
  "Diccionarios", "Diccionarios folletos", "Diccionarios, Libros digitales"
)


## Recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    cm_recursos_propios = case_match(
      cm_recursos_propios,
      all_of(valores_diccionario_cm2) ~ "Diccionario",
      all_of(valores_papeleria_cm2) ~ "Material de papelería",
      all_of(valores_digital_cm2) ~ "Recursos digitales",
      all_of(valores_cnb_cm) ~ "CNB",
      all_of(valores_libros_cm2) ~ "Libros",
      all_of(valores_tecnologia_cm2) ~ "Equipo tecnológico",
      all_of(valores_guia_folleto_cm) ~ "Guías y folletos",
      all_of(valores_audio_didactico_cm2) ~ "Material audiovisual y didáctico",
      all_of(valores_ninguno_cm2) ~ "Ninguno",
      all_of(valores_noespecifica_cm_3) ~ "No especifica el recurso",
      .default = cm_recursos_propios
    )
  )

## volver a ver errores

valores_cm_recursos_propios <- df_dyd |>
  filter(!(is.na(cm_recursos_propios))) |>
  select(cm_recursos_propios) |>
  tabyl(cm_recursos_propios)

# View(valores_cm_recursos_propios)

# cm_otro_ultima_vez_libros_mineduc ----

## ver errores

valores_cm_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(cm_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(cm_otro_ultima_vez_libros_mineduc ))) |>
  select(cm_otro_ultima_vez_libros_mineduc) |>
  tabyl(cm_otro_ultima_vez_libros_mineduc)

dput(valores_cm_otro_ultima_vez_libros_mineduc)

## Define los vectores ----

### nunca
valores_nunca_cm <- c(
  "0", "Ninguna", "Ninguna vez", "Ninguno", "No", "No existe un libro", 
  "No existen", "No he recibido", "No he recibido nada", "No hemos recibido", 
  "No hemos recibido libros asignados a tercero básico", "No se a recibido", 
  "No se ha recibido", "No se ha recibido a la fecha", "No se ha recibido material en esta área", 
  "No se ha recibido ningún material de esta área", "No sé ha recibido", 
  "Nuca", "Nunca", "Nunguno", "nunca", "N/A"
)

### 2018
valores_cm_2018 <- c(
  "2018"
)

### 2019
valores_cm_2019 <- c(
  "2019"
)

### 2020
valores_cm_2020 <- c(
  "2020"
)

### 2022
valores_cm_2022 <- c(
  "2022"
)

### no especifica
valores_cm_noespecifica <- c(
  "MATERIAL DIDACTICO"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    cm_otro_ultima_vez_libros_mineduc = case_match(
      cm_otro_ultima_vez_libros_mineduc,
      all_of(valores_cm_noespecifica) ~ "No especifica",
      all_of(valores_cm_2022) ~ "2022",
      all_of(valores_cm_2020) ~ "2020",
      all_of(valores_cm_2019) ~ "2019",
      all_of(valores_cm_2018) ~ "2018",
      all_of(valores_nunca_cm) ~ "Nunca ha recibido",
      .default = cm_otro_ultima_vez_libros_mineduc
    )
  )

## volver a ver errores

valores_cm_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(cm_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(cm_otro_ultima_vez_libros_mineduc ))) |>
  select(cm_otro_ultima_vez_libros_mineduc) |>
  tabyl(cm_otro_ultima_vez_libros_mineduc)

# View(valores_cm_otro_ultima_vez_libros_mineduc)

# teatro_libros_primero ----

## ver errrores
valores_teatro_libros_primero <- df_dyd |>
  filter(teatro_uso_libros == "Sí") |>
  filter(teatro_grados == "Primer grado") |>
  filter(!(is.na(teatro_libros_primero))) |>
  select(teatro_libros_primero) |>
  tabyl(teatro_libros_primero)

dput(valores_teatro_libros_primero)

## Define los vectores ----

### comunicacion y lenguaje
valores_comu_lenguaje_teatro <- c(
  "Comunicacion y lenguje"
)

### educacion artistica
valores_edu_artistica <- c(
  "Educación Artística", "Educación artística"
)

### expresion artistica
valores_expre_artistica <- c(
  "Expresión artística"
)

### ninguno
valores_ninguno_teatro <- c(
  "GUIAS DE INVESTIGACION PROPIA"
)

### guia de aprendizaje y planificador
valores_guia_plani_teatro <- c(
  "GUÍA DE APRENDIZAJE Y PLANIFICADOR", "Guia de Educación Artistica", 
  "Guia expresión artistica", "Guia y planificador", 
  "Panificador y guia", "Planificador", 
  "Planificador del facilitador y Guia de Aprendizaje Expresión Artística 1",
  "Guía de Aprendizaje", 
  "Guías de aprendizaje y Planificador docente", "Guías y planificadores"
)

### multimateria mineduc
valores_multimateria <- c(
  "Libro Multimateria del MINEDUC"
)

### no especifica el nombre
valores_noespecifica_teatro <- c(
  "Santillana  Editora Educativa"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    teatro_libros_primero = case_match(
      teatro_libros_primero,
      all_of(valores_noespecifica_teatro) ~ "No especifica el nombre",
      all_of(valores_multimateria) ~ "Multimateria de MINEDUC",
      all_of(valores_guia_plani_teatro) ~ "Expresión Artística: Guía y planificador del facilitador",
      all_of(valores_ninguno_teatro) ~ "Ninguno",
      all_of(valores_expre_artistica) ~ "Expresión Artístíca",
      all_of(valores_edu_artistica) ~ "Educación Artística",
      all_of(valores_comu_lenguaje_teatro) ~ "Comunicación y Lenguaje",
      .default = teatro_libros_primero
    )
  )

## volver a ver errores

valores_teatro_libros_primero <- df_dyd |>
  filter(teatro_uso_libros == "Sí") |>
  filter(teatro_grados == "Primer grado") |>
  filter(!(is.na(teatro_libros_primero))) |>
  select(teatro_libros_primero) |>
  tabyl(teatro_libros_primero)

# View(valores_teatro_libros_primero)

# teatro_libros_segundo ----

## ver errores

valores_teatro_libros_segundo <- df_dyd |>
  filter(teatro_uso_libros == "Sí") |>
  filter(teatro_grados == "Segundo grado") |>
  filter(!(is.na(teatro_libros_segundo))) |>
  select(teatro_libros_segundo) |>
  tabyl(teatro_libros_segundo)

dput(valores_teatro_libros_segundo)

## Define los vectores ----

### educación artística
valores_edu_artistica2 <- c(
  "Educación Artística", "Libro de Teatro, Educación Artística."
)

### ninguno
valores_ninguno_teatro2 <- c(
  "Folletos,", "Google", "Materiales impresos"
)

### Expresión Artística guia y planificador
valores_guia_plani_teatro2 <- c(
  "Guia de aprendizaje expresión artística", "Guía de Expresión Artística y Planificador", 
  "Guía de aprendizaje y Planificador del docente", "Guía de educación artística",
  "Planificador. Guias de aprendizaje.", 
  "Planificados y guías  de Telesecundaria"
)

### no especifica
valores_noespecifica_teatro2 <- c(
  "Libros de santillana"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    teatro_libros_segundo = case_match(
      teatro_libros_segundo,
      all_of(valores_noespecifica_teatro2) ~ "No especifica el nombre",
      all_of(valores_guia_plani_teatro2) ~ "Expresión Artística: Guía y planificador del facilitador",
      all_of(valores_ninguno_teatro2) ~ "Ninguno",
      all_of(valores_edu_artistica2) ~ "Educación Artística",
      .default = teatro_libros_segundo
    )
  )

## volver a ver errores

valores_teatro_libros_segundo <- df_dyd |>
  filter(teatro_uso_libros == "Sí") |>
  filter(teatro_grados == "Segundo grado") |>
  filter(!(is.na(teatro_libros_segundo))) |>
  select(teatro_libros_segundo) |>
  tabyl(teatro_libros_segundo)

# View(valores_teatro_libros_segundo)

#teatro_libros_tercero ----

valores_teatro_libros_tercero <- df_dyd |>
  filter(teatro_uso_libros == "Sí") |>
  filter(teatro_grados == "Tercer grado") |>
  filter(!(is.na(teatro_libros_tercero))) |>
  select(teatro_libros_tercero) |>
  tabyl(teatro_libros_tercero)

dput(valores_teatro_libros_tercero)

## define los vectores ----

### danza y expresión artistica
valores_danza_expresion <- c(
  "Danza y Expresión artititica"
)

### guia de aprendizaje
valores_guia_aprendizaje_teatro <- c(
  "Guia de aprendizaje",
  "Guía de aprendizaje de expresión artística"
)

### guia de expresión y apreciación artística
valores_expre_apre <- c(
  "Guía de Expresión y Apresiación artística"
)

### no especifica
valores_noespecifica_teatro3 <- c(
  "Libros de texto proporcionados por el ministerio", 
  "Libros digitales"
)

### ningno
valores_ninguno_teatro3 <- c(
  "Mo hay", "No hay"
)

### paginas web
valores_web_teatro <- c(
  "Paginas web"
)

### planificador para el facilitador
valores_planificador_teatro <- c(
  "Planificador",  "Planificador docente"
)

## Recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    teatro_libros_tercero = case_match(
      teatro_libros_tercero,
      all_of(valores_planificador_teatro) ~ "Planificador para el facilitador",
      all_of(valores_web_teatro) ~ "Páginas web",
      all_of(valores_ninguno_teatro3) ~ "Ninguno",
      all_of(valores_noespecifica_teatro3) ~ "No especifica el nombre",
      all_of(valores_expre_apre) ~ "Guía de Expresión y Apreciación Artística",
      all_of(valores_guia_aprendizaje_teatro) ~ "Guía de aprendizaje",
      all_of(valores_danza_expresion) ~ "Danza y Expresión Artística",
      .default = teatro_libros_tercero
    )
  )

## volver a ver errores

valores_teatro_libros_tercero <- df_dyd |>
  filter(teatro_uso_libros == "Sí") |>
  filter(teatro_grados == "Tercer grado") |>
  filter(!(is.na(teatro_libros_tercero))) |>
  select(teatro_libros_tercero) |>
  tabyl(teatro_libros_tercero)

# View(valores_teatro_libros_tercero)

# teatro_recursos_centro_educativo ----
## ver errores
valores_teatro_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(teatro_recursos_centro_educativo))) |>
  select(teatro_recursos_centro_educativo) |>
  tabyl(teatro_recursos_centro_educativo)

dput(valores_teatro_recursos_centro_educativo)

## Define los vectores ----

### equipo tecnológico
valores_tecnologia_teatro <- c(
  "Ambiente agradable. Proyector",
  "Aparato auditivo","Tecnológicos y materiales", 
  "Reproductor de sonido, área de cancha sintetica, impresiones", 
  "Aula, cancha, escenario, equipo de sonido", 
  "Aula, pizarrón, marcadores, cañonera y folleto", 
  "Bocina y micrófono", "Bocina, hojas y pizarras", "Bocina, microfonos, folletos y talleres", 
  "Bocinas, cañonera","Libros, Smart tv", "Libros, marcadores entre otros.", 
  "Libros de texto, pantallas , plataforma educativa",
  "Impresora, cañonera y pizarra", "Impresora, hojas de papel.", 
  "Escenario, sonido, marcadores, pizarras, hojas", 
  "Espacios, tecnología", "Pantalla, Impresora",
  "Cañonera", "Cañonera, computadora",
  "Computadora", "Computadora, impresora, internet, bocinas",
  "Libros , smart tv, bocina."
)

### espacio educativo
valores_espacio <- c(
  "Auditorium", "Aula, cancha, escenario, equipo de sonido", 
  "Aula, pizarrón, marcadores, cañonera y folleto",
  "El aula pura, pizarrón","Pizarra, pantalla", 
  "Libros Santillana, Manual de Teatro, Recursos digitales y Espacio escénico",
  "Escenario, material impreso", 
  "Proyector", "Proyector, marcadores, hojas.",
  "Folletos digitales, páginas webs, internet, escenario, material audiovisual",
  "Escenario, sonido, marcadores, pizarras, hojas", "Espacio de área", 
  "Espacios, tecnología",
  "Salón de clases, medios audiovisuales", "Salón de clases, áreas verdes", 
  "Salón, pizarrón, áreas verdes", 
  "Reproductor de sonido, área de cancha sintetica, impresiones", 
  "Recurso de audiovisual, literatura, espacio físico, entre otros"
)

### cnb
valores_cnb_teatro <- c(
  "CNB", "CNB, PLANIFICADOR GUÍAS DE APRENDIZAJE",
  "Cnb de Guatemala,  libros digitales."
)

### material audiviosual y didáctico
valores_audio_didactico_teatro <- c(
  "Carteles", "Copias", "Copias e impresiones", "Copias y libros", "Didácticos", 
  "FOLLETOS, AUDIOVISUALES", "Libros y videos",
  "Recursos de actividades prácticas","danza",
  "Valija  didáctica", "Valija didáctica", 
  "Videos", "Videos,  música y carteles.", 
  "Videos, y recursos audiovisuales", 
  "RED WIFI, EQUIPO AUDIO VISUAL, COPIAS", "Recurso de audiovisual, literatura, espacio físico, entre otros", 
  "Libros de textos, planificadores, material didáctico.", "Libros de textos, trajes, materiales varios", 
  "Libros y materiales", "Libros y materiales para desarrollar las actividades",
  "Fotocopias, cartulinas","Investigación de temas", 
  "Material Didactico, uso de cañonera, utilería", "Material de Apollo como teatrinos, folletos", 
  "Material de apoyo", "Material de apoyo y folletos", "Material didactico", 
  "Material didáctico", "Solo material de apoyo", 
  "Salón de clases, medios audiovisuales", 
  "Material impreso", "Materiales de apoyo",
  "Folletos digitales, páginas webs, internet, escenario, material audiovisual"
)

### recursos digitales
valores_digitales_teatro <- c(
  "Digitales", "Digitales e impresos","medios virtuales",
  "Enlaces virtuales", "Material digital",
  "Material virtual", "Pdf", "Plataforma",
  "paginas de internet",
  "Libros de texto, pantallas , plataforma educativa",
  "Libros Santillana, Manual de Teatro, Recursos digitales y Espacio escénico"
)

### Libros
valores_libros_teatro <- c(
  "Editora educativa", "Educación Artística Editorial Santillana",
  "Expresión artística 1°, 2°, 3°",
  "Telones, libros", "Texto", "Textos", 
  "Un libro", "Un libro de educación artística", 
  "Libro", "Libro Guía.", "Libro de texto", "Libro para enseñar el idioma khikelaqc", 
  "Libros", "Libros  de texto", "Libros , smart tv, bocina.", "Libros Santillana, Manual de Teatro, Recursos digitales y Espacio escénico", 
  "Libros actualizados", "Libros de obras,investigación", "Libros de texto", 
  "Libros de texto, impresora hojas", "Libros de texto, pantallas , plataforma educativa", 
  "Libros de textos, planificadores, material didáctico.", "Libros de textos, trajes, materiales varios", 
  "Libros e impresiones", "Libros y materiales", "Libros y materiales para desarrollar las actividades", 
  "Libros y videos", "Libros, Smart tv", "Libros, marcadores entre otros.", 
  "Libros, marcadores, hojas, etc.", "Libros.", "Lo básico de Stanislavski y otros autores" 
  
)

### guias y folletos
valores_guia_folleto_teatro <- c(
  "FOLLETOS, AUDIOVISUALES", "Folletos", "La guia y planificador",
  "Folletos digitales, páginas webs, internet, escenario, material audiovisual", 
  "GUIA DE APRENDIZAJE", "GUÍA DE APRENDIZAJE DE EXPRESISÓN ARTISTICA", 
  "Guia de aprendizaje", "Guia de aprendizaje expresión artística", 
  "Guia de educacion artistica y copias", "Guia y planificador", 
  "Planificador del facilitador y guía del aprendizaje de Telesecundaria", 
  "Planificador del facilitador, guia de aprendizaje", "Planificador y guias", 
  "Planificadores y guías de Telesecundaria",
  "folleto de Expresión artística de IGER", 
  "Guias", "Guias  folletos", "Guias de aprendizaje, planificador Docente", 
  "Guía de Telesecundaria", "Guía de aprendizaje", "Guía de educación artística", 
  "Guía de telesecundaria", "Guía y planificador", "Guías de aprendizaje y planificador", 
  "Guías y planificadores"
  
)

### material de papelería
valores_papeleria_teatro <- c(
  "Impresiones", "Impresiones y fotocopias, bocinas", 
  "Papel, marcadores,",
  "Proyector, marcadores, hojas.",
  "Pizarrón de formica, marcadores de formica, papel tamaño carta, oficio y papel bond", 
  "Pizarra, marcadores, almohadillas, copias, impresiones, tinta para marcadores", 
  "Pizarra, marcadores, impresiones, tinta, lapiceros, u otro material requerido", 
  "Impresos", "Marcador y hojas", "Marcadores y cartulinas", 
  "Impresiones,  fotocopias", "Impresiones, hojas, marcadores", 
  "Libros, Smart tv", "Libros, marcadores entre otros.", "PAPEL BOND",
  "Libros, marcadores, hojas, etc."
)

### ninguno
valores_ninguno_teatro4 <- c(
  "Los libros son propios",
  "Propios santillana","ninguno", 
  "N/A", "Nada", "Ni se cuenta con libros o folletos otra la consulta solo investigaciones", 
  "Ninguna", "Ninguno", "Ninguno, es en una cooperativa", "Ninguno, me guío con libros de santillana u otros", 
  "Ningúno", "No", "No hay", "No hay guías", "No hay, lo que me queda es investigar sobre el tema y contextualizar al ámbito de los estudiantes", 
  "No me facilita ningún recurso en el área", 
  "No me facilitan pero yo hago mi material de apoyo. Compro mis herramientas para imprimir de la mejor manera posible en beneficio del estudiante"
)

### mobiliario
valores_mobiliario_teatro <- c(
  "Pizarra", "Pizarra marcador", "Pizarra, marcadores, almohadillas, copias, impresiones, tinta para marcadores", 
  "Pizarra, marcadores, impresiones, tinta, lapiceros, u otro material requerido", 
  "Pizarra, pantalla", "Pizarrón", "Pizarrón de formica, marcadores de formica, papel tamaño carta, oficio y papel bond"
)

### planificador
valores_planificador_teatro2 <- c(
  "Planificador", "Planificador de Educación artistica", "Planificador de Expresión Artística", 
  "Planificador del facilitador y guía del aprendizaje de Telesecundaria", 
  "Planificador del facilitador, guia de aprendizaje", "Planificador y guias", 
  "Planificadores y guías de Telesecundaria"
)

### no especifica
valores_noespecifica_teatro4 <- c(
  "1", "Ampliación", "Con recursos básicos", "Docente especializado",
  "Poco", "Practico y teorico","m"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    teatro_recursos_centro_educativo = case_match(
      teatro_recursos_centro_educativo,
      all_of(valores_noespecifica_teatro4) ~ "No especifica el recurso",
      all_of(valores_planificador_teatro2) ~ "Planificador",
      all_of(valores_mobiliario_teatro) ~ "Mobiliario",
      all_of(valores_ninguno_teatro4) ~ "Ninguno",
      all_of(valores_papeleria_teatro) ~ "Material de papelería",
      all_of(valores_guia_folleto_teatro) ~ "Guías y folletos",
      all_of(valores_libros_teatro) ~ "Libros",
      all_of(valores_digitales_teatro) ~ "Recursos digitales",
      all_of(valores_audio_didactico_teatro) ~ "Material audiovisual y didáctico",
      all_of(valores_cnb_teatro) ~ "CNB",
      all_of(valores_espacio) ~ "Espacio educativo",
      all_of(valores_tecnologia_teatro) ~ "Equipo tecnológico",
      .default = teatro_recursos_centro_educativo
    )
  )

## volver a ver errores

valores_teatro_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(teatro_recursos_centro_educativo))) |>
  select(teatro_recursos_centro_educativo) |>
  tabyl(teatro_recursos_centro_educativo)

# View(valores_teatro_recursos_centro_educativo)

# teatro_recursos_propios ----

## ver errores

valores_teatro_recursos_propios <- df_dyd |>
  filter(!(is.na(teatro_recursos_propios))) |>
  select(teatro_recursos_propios) |>
  tabyl(teatro_recursos_propios)

dput(valores_teatro_recursos_propios)

## Define los vectores ----

### material audiovisual y didáctico

valores_audio_didactico_teatro2 <- c(
  "Afiches","Guías virtuales y videos",
  "Audios, videos", "Audios, videos, imágenes.",
  "Copias", "Copias y guías propias", "Copias, material pedagógico", 
  "Crayones hojas de trabajo", "Dictarles y la malla curricular", 
  "Didácticos", "Fichas","Folletos videos etc", 
  "Material", "Páginas web,materiales didacticos",
  "Material impreso", 
  "Videos", "Videos,  carteles y música.", "Videos, Internet, audios.", 
  "Videos.","audivisuales, vestuario, maquillaje, entre otros", 
  "Textos, materiales reciclables. Mascaras, títeres, teatro para títeres.", 
  "Tiras didácticas e impresiones",
  "Temas impresos, material didáctico", 
  "Telones, material didáctico etc.", 
  "Recursos didacticos, libro de texto",
  "Recursos de actividades prácticas",
  "Material impreso acorde al curso", "Material impreso y libros propios del docente", 
  "Material impreso, consultas del internet",
  "Material Didáctico Variado.", "Material de apoyo", "Material de dibujo artístico y material de maquillaje", 
  "Material didactico y auditivo(bocina)", "Material didáctico", 
  "Material elaborado y recurso audiovisual",
  "Equipo audio visual , folletos , cartulinas.", 
  "Internet, copias, revistas.", "Libros, revistas, videos",
  "Libros y videos", "Libros, audios", "Libros, audiovisuales, textos", 
  "Libros, cnb, videos y publicaciones de Internet", 
  "Internet, materiales.", "Investigaciones, diapositivas y consultas con licenciados que trabajan con el área pero práctico", 
  "Computadora, celular, materiales diversos", "Computadora, impresiones, calcomanías, recortes", 
  "BOCINA, MAQUILLAJE, ESCENOGRAFÍA", "Bocina", "Bocina e internet", 
  "Bocina pequeña..", "Bocina, Pegamento  hojas, tijeras, etc", 
  "Bocina, teléfono, etc.", "Bocinas para generar sonidos", "Bocinas, micrófono, utilería"
)

### material papeleria
valores_papeleria_teatro2 <- c(
  "Bocina, Pegamento  hojas, tijeras, etc",
  "Hojas", "Impresos y digitales",
  "Material impreso", 
  "Material impreso acorde al curso", "Material impreso y libros propios del docente", 
  "Material impreso, consultas del internet",
  "Material personal", 
  "Materiales básicos", 
  "Marcador, almohadilla, pizzara, cartulina, videos.", 
  "Libros, marcadores, hojas de trabajo", 
  "Libros, reglas, marcadores, papel, bocina, témperas, etc", 
  "Impresiones  e Internet", "Impresiones y libros de expresión artística",
  "Hojas con guión", "Hojas de actividades", "Hojas de tareas, y juego de geometría, temperas", 
  "Hojas de trabajo", "Hojas de trabajo, materiales didácticos, tv, bocina", 
  "Hojas de trabajos marcadores entre otros colores", "Impresiones", 
  "Carteles, revistas, libros de texto", "Cartulina, dibujos y marcadores", 
  "Cartulinas temperas marcadores hojas de 80 gramos", "Cartulinas, laminas,  palos ojas o otros", 
  "CUADERNOS, COMPUTADORA, REPRODUCTOR DE MÚSICA, USB, MATERIALES IMPRESOS, RECURSOS EN LÍNEA, AGENDA DE TRABAJO, HOJAS DE TRABAJO, COPIAS, REVISTAS, PERIÓDICOS, LIBROS DIGITALES."
)

### tecnologia
valores_tecnologia_teatro2 <- c(
  "COMPUTADORA E IMPRESORA",
  "Cañonera", "Cañonera. Computadora y bocina", "Celular, computadora e impresora, hojas", 
  "Celular, pdf's,","Folletos, cañonera,  papelografo, teatrin.", 
  "Equipo audio visual , folletos , cartulinas.", 
  "Equipos de computación y libros de textos", 
  "Pantalla, computadora", "Pc, impresiones, videos", 
  "Habilidad actoral, bocina portátil, vídeos", 
  "Folletos, teatrales,  exposiciones, cañonera", 
  "Computadora", "Computadora e impresora",
  "Proyector", "TELEFONO CELULAR", 
  "Uso de Internet. Computadora","libros Computadora y otros",
  "bocinas, cortinas, escenografías, microfonos, cañonera",
  "Teléfono internet", "Teléfono, internet, libros digital s", 
  "Proyector, bocina, telón, Internet, telas de distintos colores, computadora, papel china, cartón y diversión tipos de papel", 
  "Pelucas, pintacaritas, micrifono, celular, bocina, etc",
  "Impresora, cañonera y pizarra", "Libros, compu", 
  "Libros de textos, bocina, proyector, telones",
  "Pizarra, marcadores, computadora. cañonera etc.", 
  "Laptop", "Laptop. Canonera, bocinas,", "Las Tics y Guiones.", 
  "Internet y computadora", "Internet, bocina",
  "Computadora impresora cañonera marcadores", "Computadora, Internet", 
  "Computadora, Reproductor y libro de teatro", "Computadora, audios, internet, hojas", 
  "Computadora, celular, materiales diversos", "Computadora, impresiones, calcomanías, recortes", 
  "Computadora, impresora", "Computadora, libros de dibujos o para colorear.", 
  "Computadora, libros de texto", "Computadora, libros,marcadores hojas de trabajo, teléfono.", 
  "Computadora, papel, otros", "Computadora, videos libros", "Computadora,cel, internet", 
  "Comñitadora con internet", "Con apoyo tecnológico",
  "CUADERNOS, COMPUTADORA, REPRODUCTOR DE MÚSICA, USB, MATERIALES IMPRESOS, RECURSOS EN LÍNEA, AGENDA DE TRABAJO, HOJAS DE TRABAJO, COPIAS, REVISTAS, PERIÓDICOS, LIBROS DIGITALES."
)

### cnb
valores_cnb_teatro2 <- c(
  "Cnb", "Cnb de Guatemala y libros digitales", 
  "Cnb...textos.....", "CNB,Paginas web.",
  "Libros y CNB", "Libros, cnb, videos y publicaciones de Internet"
)

### recurso digital 
valores_digital_teatro <- c(
  "Digitales", "Digitales y audiovisuales", 
  "Medios digitales", "Música", 
  "enlaces virtuales", 
  "Temas de investigación en la web", 
  "Recursos de la Web", "Sitios Web",
  "Pdf, páginas web", "Páginas web",
  "Páginas web,materiales didacticos",
  "Pra iniciar que conozcan de los 13 signos del teatro y lo pueda implementar en pequeñas obras infantiles es investigacion de internet",
  "Folletos y contenidos en Google", "Impresos y digitales", 
  "Documentos en PDF, libros","Consultas en la web", 
  "Internet", "Libros y redes","Lo que se tiene en la plataforma educativa porque es privada",
  "Internet y computadora", "Internet, bocina", "Internet, copias, revistas.", 
  "Internet, materiales.", "Investigaciones", "Investigaciones  propias y contextuañizacion de la información que se obtiene", 
  "Investigaciones de internet", "Investigaciones propias, y después contextualizar la misma.", 
  "Investigaciones, diapositivas y consultas con licenciados que trabajan con el área pero práctico", 
  "Investigaciónes", "Web"
)

### libros
valores_libros_teatro2 <- c(
  "Algunos libros","Guías, revistas, libros", 
  "Computadora, Reproductor y libro de teatro", "Computadora, libros de dibujos o para colorear.", 
  "Computadora, libros de texto", "Computadora, libros,marcadores hojas de trabajo, teléfono.", 
  "Computadora, videos libros", "Documentos en PDF, libros",
  "Equipos de computación y libros de textos", 
  "Recursos didacticos, libro de texto",
  "Soy arte", 
  "Texto", "Textos", "Textos dramático", "Textos, elementos escénicos entre otros.", 
  "Textos, folletos, internet", "Textos, materiales reciclables. Mascaras, títeres, teatro para títeres.", 
  "Teatrin, sonido y libros", "Teatrines, títeres, libros, etc", 
  "Historia del Arte guatemalteco,guiones e intternet",
  "Impresiones y libros de expresión artística", 
  "libro y  folletos", "libros Computadora y otros", 
  "Libro", "Libro Educación Artística Editorial Santillana", 
  "Libro de teatro", "Libro de texto", "Libro de texto Soy Arte", 
  "Libro digital e internet", "Libro e internet", "Libro guía de teatro", 
  "Libro, Internet", "Libros", "Libros carteles", "Libros de Expresión, libros de cuentos", 
  "Libros de texto, internet.", "Libros de texto, redes moviles", 
  "Libros de textos, bocina, proyector, telones", "Libros digitales", 
  "Libros e Internet", "Libros e impresiones", "Libros e internet", 
  "Libros propios", "Libros y CNB", "Libros y folletos", "Libros y redes", 
  "Libros y videos", "Libros, audios", "Libros, audiovisuales, textos", 
  "Libros, cnb, videos y publicaciones de Internet", "Libros, compu", 
  "Libros, folletos, pdf", "Libros, marcadores, hojas de trabajo", 
  "Libros, reglas, marcadores, papel, bocina, témperas, etc", 
  "Libros, revistas, videos", "Libros, teatrinos, etc.", "Libros."
  
)

### guias y folletos
valores_folleto_guias_teatro <- c(
  "Folletos", "Folletos e impresiones", "Folletos temáticos personales", 
  "Folletos videos etc", "Folletos y  hojas de trabajo", "Folletos y contenidos en Google", 
  "Folletos y hojas de trabajo", "Folletos, cañonera,  papelografo, teatrin.", 
  "Folletos, hojas de trabajo", "Folletos, teatrales,  exposiciones, cañonera", 
  "Foolletos e impresiones", "GUÍA DE APRENDIZAJE DE EXPRESIÓN",
  "Libros y folletos", "Libros, folletos, pdf","libro y  folletos",
  "Planificador","Textos, folletos, internet",
  "Guia", "Guia de aprendizaje", "Guia expresión artística telesecundaria", 
  "Guias folletos libros de cuentos", "Guía de Apresiación Artística", 
  "Guía de aprendizaje", "Guías", "Guías virtuales y videos", 
  "Guías, revistas, libros"
)

### mobiliario
valores_mobiliario_teatro2 <- c(
  "Impresora, cañonera y pizarra", 
  "Pizarra", "Pizarra, marcadores, computadora. cañonera etc."
)

### recursos escenicos
valores_escenicos <- c(
  "Mantas blancas, luces Led, bocinas. Maquillaje vestuarios.", 
  "Multimedia y guiones",
  "Trajes .bocina vestuario", 
  "Trajes, manualidades", "Títeres,  vestuarios", 
  "Teatriles y entorno", "Titeres",
  "Teatrin, sonido y libros", "Teatrines, títeres, libros, etc", 
  "Telones, Internet, etc", "Telones, material didáctico etc.", 
  "Obras de teatro, escenografías", 
  "Utilería y presentaciones", 
  "audivisuales, vestuario, maquillaje, entre otros", 
  "Vestuario, elementos escenograficos", "Vestuarios  escenarios espacios tiempo", 
  "Pelucas, pintacaritas, micrifono, celular, bocina, etc", "Pinturas papel carton"
)

### espacio educativo
valores_espacio_teatro <- c(
  "Recursos del entorno", "Recursos del entorno e internet", 
  "Salón de clases, áreas verdes, pizarrón", 
  "Vestuarios  escenarios espacios tiempo"
)

### no especifica
valores_noespecifica_teatro5 <- c(
  "1", "Económicos", "Económicos forocopias",
  "Todo", "Todos", "m", "todos los necesarios"
)

### ninguno
valores_ninguno_teatro5 <- c(
  "0", "Experiencia propia", 
  "Mi experiencia en muestras de teatro y talleres", 
  "Ninguno", "No",
  "Practico y teorico", "experiencia teatral"
)

## Recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    teatro_recursos_propios = case_match(
      teatro_recursos_propios,
      all_of(valores_ninguno_teatro5) ~ "Ninguno",
      all_of(valores_noespecifica_teatro5) ~ "No especifica el recurso",
      all_of(valores_espacio_teatro) ~ "Espacio educativo",
      all_of(valores_escenicos) ~ "Recursos escénicos",
      all_of(valores_mobiliario_teatro2) ~ "Mobiliario",
      all_of(valores_folleto_guias_teatro) ~ "Guías y folletos",
      all_of(valores_libros_teatro2) ~ "Libros",
      all_of(valores_digital_teatro) ~ "Recursos digitales",
      all_of(valores_cnb_teatro2) ~ "CNB",
      all_of(valores_tecnologia_teatro2) ~ "Equipo tecnológico",
      all_of(valores_papeleria_teatro2) ~ "Material de papelería",
      all_of(valores_audio_didactico_teatro2) ~ "Material audiovisual y didáctico",
      .default = teatro_recursos_propios
    )
  )

## volver a ver errores

valores_teatro_recursos_propios <- df_dyd |>
  filter(!(is.na(teatro_recursos_propios))) |>
  select(teatro_recursos_propios) |>
  tabyl(teatro_recursos_propios)

# View(valores_teatro_recursos_propios)

# teatro_otro_ultima_vez_libros_mineduc ----

## ver errores
valores_teatro_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(teatro_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(teatro_otro_ultima_vez_libros_mineduc ))) |>
  select(teatro_otro_ultima_vez_libros_mineduc) |>
  tabyl(teatro_otro_ultima_vez_libros_mineduc)

dput(valores_teatro_otro_ultima_vez_libros_mineduc)

## define los vectores ----

### 2016
valores_teatro_2016 <- c(
  "2016","Libros Guía en el año 2015 al 18"
)

### 2017
valores_teatro_2017 <- c(
  "Libros Guía en el año 2015 al 18","2017"
)

### 2018
valores_teatro_2018 <- c(
  "2018", "2018, Expresión Artística",
  "Libros Guía en el año 2015 al 18"
)

### 2019
valores_teatro_2019 <- c(
  "2019"
)

### 2020 
valores_teatro_2020 <- c(
  "2020"
)

### nunca
valores_nunca_teatro <- c(
  "N/A", "Ninguno", "No", 
  "No he recibido", "No hemos recibido un libro como tal", "No se a recibido", 
  "No se ha recibido información sobre esta área", "No se ha recibido ningún material de esta área", 
  "Nunca", "no hemos recibido", "nunca"
)

## recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    teatro_otro_ultima_vez_libros_mineduc = case_match(
      teatro_otro_ultima_vez_libros_mineduc,
      all_of(valores_nunca_teatro) ~ "Nunca ha recibido",
      all_of(valores_teatro_2020) ~ "2020",
      all_of(valores_teatro_2019) ~ "2019",
      all_of(valores_teatro_2018) ~ "2018",
      all_of(valores_teatro_2017) ~ "2017",
      all_of(valores_teatro_2016) ~ "2016",
      .default = teatro_otro_ultima_vez_libros_mineduc
    )
  )

## volver a ver errores

valores_teatro_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(teatro_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(teatro_otro_ultima_vez_libros_mineduc ))) |>
  select(teatro_otro_ultima_vez_libros_mineduc) |>
  tabyl(teatro_otro_ultima_vez_libros_mineduc)

# View(valores_teatro_otro_ultima_vez_libros_mineduc)

# teatro_tipo_materiales_recibidos ----

## ver errores
valores_teatro_tipo_materiales_recibidos <- df_dyd |>
  filter(teatro_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(teatro_tipo_materiales_recibidos ))) |>
  select(teatro_tipo_materiales_recibidos) |>
  tabyl(teatro_tipo_materiales_recibidos)

# View(valores_teatro_tipo_materiales_recibidos)


## Define los vectores ----
### guia de aprendizaje
valores_guia_aprendizaje_teatro2 <- c(
  "GUÍA DE APRENDIZAJE", "Guia", "Guia y planificador",
  "Guias", "Guias y planificador", "Guía de aprendizaje, planificador, valija didáctica",
  "Guías y planificadores", "Planificador y guia"
)

### planificador para el facilitador
valores_planificador_teatro3 <- c(
  "Guia y planificador", "Guias y planificador",
  "Guía de aprendizaje, planificador, valija didáctica",
  "Guías y planificadores", "Planificador y guia"
)

### valija didáctica
valores_valija_teatro <- c(
  "Guía de aprendizaje, planificador, valija didáctica"
)

### libro
valores_libro_teatro <- c(
  "Libros de texto", "Texto"
)

### material reciclable
valores_reciclable <- c(
  "Material reciclable"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    teatro_tipo_materiales_recibidos = case_match(
      teatro_tipo_materiales_recibidos,
      all_of(valores_reciclable) ~ "Material reciclable",
      all_of(valores_libro_teatro) ~ "Libros",
      all_of(valores_valija_teatro) ~ "Material didáctico",
      all_of(valores_planificador_teatro3) ~ "Planificador del facilitador",
      all_of(valores_guia_aprendizaje_teatro2) ~ "Guía de aprendizaje",
      .default = teatro_tipo_materiales_recibidos
    )
  )

## volver a ver errores 

valores_teatro_tipo_materiales_recibidos <- df_dyd |>
  filter(teatro_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(teatro_tipo_materiales_recibidos ))) |>
  select(teatro_tipo_materiales_recibidos) |>
  tabyl(teatro_tipo_materiales_recibidos)

# View(valores_teatro_tipo_materiales_recibidos)

# danza_libros_primero ----

## ver errores

valores_danza_libros_primero <- df_dyd |>
  filter(danza_uso_libros == "Sí") |>
  filter(danza_grados == "Primer grado") |>
  filter(!(is.na(danza_libros_primero))) |>
  select(danza_libros_primero) |>
  tabyl(danza_libros_primero)

dput(valores_danza_libros_primero)

## Define los vectores ----

### educación artística
valores_educacion_art <- c(
  "Educación artística"
)

### expresion artistica 
valores_expresion_art <- c(
  "Expresión Artística",
  "Expresión artística"
)

### guia de aprendizaje y planificador
valores_guia_aprendizaje_danza <- c(
  "Expresión artística planoficador y guia", 
  "GUÍA DE APRENDIZAJE Y PLANIFICADOR", "Guia de Educación Artistica", 
  "Guia y planificsdor", "Guias y planificador", "Guías de aprendizaje y Planificador docente", 
  "Guías y planificadores", "Planificador",
  "Planificador del facilitador y Guia de Aprendizaje Expresión Artística 1", 
  "Planificador docente y guías de aprendizaje", "Planificador y guia"
)

### no especifica 
valores_noespecifica_danza <- c(
  "Santillana", "Solamente el que envió el MINEDUC", "Web"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    danza_libros_primero = case_match(
      danza_libros_primero,
      all_of(valores_noespecifica_danza) ~ "No especifica el nombre",
      all_of(valores_guia_aprendizaje_danza) ~ "Guía de aprendizaje y Planificador",
      all_of(valores_expresion_art) ~ "Expresión Artística",
      all_of(valores_educacion_art) ~ "Educación Artística",
      .default = danza_libros_primero
    )
  )

## volver a ver errores

valores_danza_libros_primero <- df_dyd |>
  filter(danza_uso_libros == "Sí") |>
  filter(danza_grados == "Primer grado") |>
  filter(!(is.na(danza_libros_primero))) |>
  select(danza_libros_primero) |>
  tabyl(danza_libros_primero)

# View(valores_danza_libros_primero)

# danza_libros_segundo ----
## ver errores
valores_danza_libros_segundo <- df_dyd |>
  filter(danza_uso_libros == "Sí") |>
  filter(danza_grados == "Segundo grado") |>
  filter(!(is.na(danza_libros_segundo))) |>
  select(danza_libros_segundo) |>
  tabyl(danza_libros_segundo)

dput(valores_danza_libros_segundo)

## Define los vectores ----

### artes y expresion
valores_arte_expresion <- c(
  "Artes y expresión artística, páginas Web"
)

### educación artistica
valores_edu_artistica3 <- c(
  "Educación Artística"
)

### guia de aprendizaje expresion artistica
valores_guia_aprendizaj_danza2 <- c(
  "Guia de aprendizaje expresión artística", 
  "Guía de Expresión Artística y Planificador", 
  "Guía de Educación Artística", "Guía de aprendizaje y Planificador del docente"
)

### no especifica
valores_noespecifica_danza2 <- c(
  "Google", "Libros de santillana", 
  "Materiales impresos", "Páginas web,material didactico."
)

## Recode con case match ----

df_dyd <- df_dyd  |>
  mutate(
    danza_libros_segundo = case_match(
      danza_libros_segundo,
      all_of(valores_noespecifica_danza2) ~ "No especifica el nombre",
      all_of(valores_guia_aprendizaj_danza2) ~ "Expresión Artística: Guía de aprendizaje",
      all_of(valores_edu_artistica3) ~ "Educación Artística",
      all_of(valores_arte_expresion) ~ "Artes y Expresión artística",
      .default = danza_libros_segundo
    )
  )

## volver a ver errores

valores_danza_libros_segundo <- df_dyd |>
  filter(danza_uso_libros == "Sí") |>
  filter(danza_grados == "Segundo grado") |>
  filter(!(is.na(danza_libros_segundo))) |>
  select(danza_libros_segundo) |>
  tabyl(danza_libros_segundo)

# View(valores_danza_libros_segundo)

# danza_libros_tercero ----

## ver errores

valores_danza_libros_tercero <- df_dyd |>
  filter(danza_uso_libros == "Sí") |>
  filter(danza_grados == "Tercer grado") |>
  filter(!(is.na(danza_libros_tercero))) |>
  select(danza_libros_tercero) |>
  tabyl(danza_libros_tercero)

dput(valores_danza_libros_tercero)

## Define los vectores ----

### cnb
valores_cnb_danza <- c(
  "CNB y paginas web"
) 

### expresion artistica
valores_expre_art2 <- c(
  "Expresión Artistica"
)

### guia de aprendizaje
valores_guia_aprendizaje_danza2 <- c(
  "Guía", "Guía de aprendizaje", "Guía de aprendizaje de expresión artística", 
  "Planificador docente y guías de aprendizaje",
  "guía de aprendizaje de Expresión Artística"
)

### planificador
valores_planificador_danza <- c(
  "Planificador de Educación Artística", 
  "Planificador docente y guías de aprendizaje"
)

### no especifica
valores_noespecifica_danza3 <- c(
  "Libro digital", "Libros digitales",
  "Libros proporcionados por el ministerio"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    danza_libros_tercero = case_match(
      danza_libros_tercero,
      all_of(valores_noespecifica_danza3) ~ "No especifica el nombre",
      all_of(valores_planificador_danza) ~ "Planificador para el facilitador",
      all_of(valores_guia_aprendizaje_danza2) ~ "Guía de aprendizaje",
      all_of(valores_expre_art2) ~ "Expresión Artística",
      all_of(valores_cnb_danza) ~ "CNB",
      .default = danza_libros_tercero
    )
  )

## volver a ver errores

valores_danza_libros_tercero <- df_dyd |>
  filter(danza_uso_libros == "Sí") |>
  filter(danza_grados == "Tercer grado") |>
  filter(!(is.na(danza_libros_tercero))) |>
  select(danza_libros_tercero) |>
  tabyl(danza_libros_tercero)

# View(valores_danza_libros_tercero)

# danza_recursos_centro_educativo ----

## ver errores

valores_danza_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(danza_recursos_centro_educativo))) |>
  select(danza_recursos_centro_educativo) |>
  tabyl(danza_recursos_centro_educativo)

dput(valores_danza_recursos_centro_educativo)

## Define los vectores ----

### equipo tecnológico
valores_tecnologia_danza <- c(
  "Aparato para música y libros",
  "Audio", "Audio y cañonera",
  "Aula, Pizarrón, Cañonera, Marcadores", 
  "Aula, cancha, escenario, equipo de sonido",
  "Bocina", "Bocina y micrófono", "Computadora",
  "Bocina y un salón para realizar actividades", "Bocina únicamente", 
  "Bocina, escenario, hojas, lápices, pizarra", "Bocina, espacio adecuado", 
  "Bocinas", "Bocinas  para  audio", "Bocinas, internet, minicomputadora", 
  "Bocinas, pizarrón, tinta, lapiceros material pedagógico", 
  "Bocinas, ulas, aros, cintas.",  "Cañonera", 
  "Libros bocina micrófono","Radio audios",
  "Sonido, espacio físico.", 
  "Tecnologìa", "Tecnología, bocina, espacio", 
  "Teléfono celular","Tv y reproductor", "equipo de sonido",
  "Recursos audiovisuales, equipo de sonido, espacio físico, entre otros...", 
  "Reproductor", "Reproductor de sonido, área de cancha polideportiva, impresiones", 
  "Protector", "Proyector", "Proyector, marcadores, hojas, impresiones.", 
  "Pantalla", "Pantalla, Impresora", "Pantallas, libros de texto,", 
  "Parlantes, canciones, libros de texto", 
  "Salón de clases, bocina, áreas verdes", 
  "Marcadores, proyector, hojas, impresiones", 
  "Equipod de sonido", "Libro. Bocina,", "Libros , smart tv, bocina",
  "Impresiones, fotocopias, bocina, hojas", "Impresora, bocina.", 
  "Impresora, cañonera y pizarra", "Impresora, computadora, bocina", 
  "Internet", "Internet y bocina"
)

### espacio educativo
valores_espacio_danza <- c(
  "Aula, Pizarrón, Cañonera, Marcadores", 
  "Aula, cancha, escenario, equipo de sonido",
  "Bocina y un salón para realizar actividades", 
  "Tecnología, bocina, espacio", 
  "Bocina, escenario, hojas, lápices, pizarra", "Bocina, espacio adecuado", 
  "Bocina, espacio adecuado", "El aula pura , pizarrón",
  "Medios Audivisuales, salones","Sonido, espacio físico.", 
  "Recursos audiovisuales, equipo de sonido, espacio físico, entre otros...", 
  "Reproductor de sonido, área de cancha polideportiva, impresiones", 
  "Salón de clases, bocina, áreas verdes", "Salón de clases, áreas verdes", 
  "Salón y audio"
  
)

### materiales para expresión corporal
valores_expresion_corporal <- c(
  "Bocinas, ulas, aros, cintas.", "Danza",
  "Trajes .", "Tutoriales de danza"
)

### material audiovisual y didáctico
valores_audio_didactico_danza <- c(
  "Audios trabajos manuales", "Didácticos", 
  "Bocina, escenario, hojas, lápices, pizarra", "Bocinas, pizarrón, tinta, lapiceros material pedagógico", 
  "Documentos", "RED WIFI, EQUIPO AUDIO VISUAL, COPIAS.", 
  "Proyector, marcadores, hojas, impresiones.", 
  "Planificadores, guías aprendizaje, valija didáctica", 
  "Material físico", "Material impreso", "Material impreso creativo", 
  "Material impreso y libros propios del docente la escuela no ha proporcionado material", 
  "Materiales de apoyo", "Medios Audivisuales, salones", 
  "Música, videos, ejemplificaciones, ensayos, exposiciones, dramatizaciones", 
  "Marcadores  y cartulinas", "Marcadores e investigaciones, imágenes", 
  "Marcadores y Hojas", "Marcadores y cartulinas", "Marcadores y cartulinas.", 
  "Marcadores, proyector, hojas, impresiones", "Material",
  "Malla curricular", "Mapeo de contenidos, folletos en PDF, páginas webs, mobiliario adecuado", 
  "Investigaciónesby material didáctico", 
  "Valija didáctica", "Videos", "digital auduio videos",
  "Talleres de coreografos y documentales", 
  "Recursos audiovisuales, equipo de sonido, espacio físico, entre otros...", 
  "Libros y material audiovisual", "Libros y materiales", 
  "Libros y materiales pedagogicos", "Libros, audio"
)

### cnb
valores_cnb_danza2 <- c(
  "CNB PLANIFICADOR GUÍAS DE APRENDIZAJE", "Cnb"
  
)

### material papeleria
valores_papeleria_danza <- c(
  "Copias", 
  "Impresiones", "Impresiones ,fotocopias", 
  "Impresiones, fotocopias, bocina, hojas",
  "Copias y libros","Fotocopias", "Libros, hojas de trabajo, marcadores.", 
  "Pizarra, marcadores, almohadillas, copias, impresiones, tinta para marcadores", 
  "Pizarrones de formica, marcadores de formica, hojas de papel bond, etc"
)

### recursos digitales
valores_digitales_danza <- c(
  "Digitales", "Enlces","Lo que ya hay en laplqtaforma",
  "Material digital", "Paginas web","Pdfs",
  "Páginas w", "Páginas web", "digital auduio videos",
  "Virtuales",  "paginas de internet", "virtuales",
  "Mapeo de contenidos, folletos en PDF, páginas webs, mobiliario adecuado"
)

### Libros
valores_libros_danza <- c(
  "Editora Educativa", "Expresión artística, danza 1°, 2°, 3°", 
  "Libro", "Libro de Expresión Artística de IGER", 
  "Solo el libro que envió el MINEDUC", 
  "Texto", "Textos", "Un libro",
  "Libro de Telesecundaria", "Libro de texto", "Libro y planificadores.", 
  "Libro. Bocina,", "Libros", "Libros , smart tv, bocina", "Libros Santillana", 
  "Libros bocina micrófono", "Libros de texto", "Libros digitales", 
  "Libros entre otros.", "Libros y material audiovisual", "Libros y materiales", 
  "Libros y materiales pedagogicos", "Libros, audio", "Libros, hojas de trabajo, marcadores.", 
  "Pantallas, libros de texto,", "Planifocador. Libro guia", 
  "Parlantes, canciones, libros de texto"
)

### guias y folletos
valores_guia_folleto_danza <- c(
  "Folletos", "Folletos, audio.", 
  "GUIA DE APRENDIZAJE", "GUÍA DE APRENDIZAJE", 
  "Mapeo de contenidos, folletos en PDF, páginas webs, mobiliario adecuado", 
  "Guia", "Guia de aprendizaje expresión artística", "Guia y plsnificador", 
  "Guias de Expresión Artística integrado", "Guias de educacion artistica copias", 
  "Guias folletos", "Guias y planificador", "Guía de Educación Artística", 
  "Guía de Telesecundaria", "Guía de aprendizaje", "Guía de docente", 
  "Guía y planificado", "Guías de aprendizaje y Planificador docente", 
  "Guías de aprendizajes", "Investigo y creo guías según cnb",
  "Planificador del docente y Guía de Aprendizaje de Telesecundaria.", 
  "Planificador y guia", "Planificadores, guías aprendizaje, valija didáctica"
  
)

### mobiliario 
valores_mobiliario_danza <- c(
  "Mapeo de contenidos, folletos en PDF, páginas webs, mobiliario adecuado", 
  "Pizarra", "Pizarra marcador", 
  "Pizarra, marcadores, almohadillas, copias, impresiones, tinta para marcadores", 
  "Pizarrones de formica, marcadores de formica, hojas de papel bond, etc", 
  "Pizarrón", "Pizarrón."
)

### planificador
valores_planificador_danza2 <- c(
  "Planificador de Educación Artística", 
  "Planificador del docente y Guía de Aprendizaje de Telesecundaria.", 
  "Planificador y guia", "Planificadores, guías aprendizaje, valija didáctica", 
  "Planifocador. Libro guia"
)

### ninguno
valores_ninguno_danza <- c(
  "0", "N/A", "NINGUNO", "Ningumo", "Ninguna", "Ninguno", "Ninguno, trabajo por las tardes en una cooperativa", 
  "Ningúno", "No", "No hau", "No hay", "No hay guías", 
  "ninguno"
)

### no especifica
valores_noespecifica_danza4 <- c(
  "1", "Actividades prácticas", "Amplificación", 
  "Con recursos básico", "Recursos  básico", 
  "ijo"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    danza_recursos_centro_educativo = case_match(
      danza_recursos_centro_educativo,
      all_of(valores_noespecifica_danza4) ~ "No especifica el recurso",
      all_of(valores_ninguno_danza) ~ "Ninguno",
      all_of(valores_planificador_danza2) ~ "Planificador",
      all_of(valores_mobiliario_danza) ~ "Mobiliario",
      all_of(valores_guia_folleto_danza) ~ "Guías y folletos",
      all_of(valores_libros_danza) ~ "Libros",
      all_of(valores_digitales_danza) ~ "Recursos digitales",
      all_of(valores_papeleria_danza) ~ "Material de papelería",
      all_of(valores_cnb_danza2) ~ "CNB",
      all_of(valores_audio_didactico_danza) ~ "Material audiovisual y didáctico",
      all_of(valores_expresion_corporal) ~ "Recursos de expresión corporal",
      all_of(valores_espacio_danza) ~ "Espacio educativo",
      all_of(valores_tecnologia_danza) ~ "Equipo tecnológico",
      .default = danza_recursos_centro_educativo
    )
  )
  
## volver a ver errores

valores_danza_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(danza_recursos_centro_educativo))) |>
  select(danza_recursos_centro_educativo) |>
  tabyl(danza_recursos_centro_educativo)

# View(valores_danza_recursos_centro_educativo)

# danza_recursos_propios ----

## ver errores
valores_danza_recursos_propios <- df_dyd |>
  filter(!(is.na(danza_recursos_propios))) |>
  select(danza_recursos_propios) |>
  tabyl(danza_recursos_propios)

dput(valores_danza_recursos_propios)

## Define los vectores ----

### equipo tecnologico
valores_tecnologia_danza2 <- c(
  "3 bocinas, 2 aparatos telefónicos","Audios, bocinas, imágenes", 
  "Bocina", "Bocina ,espacio  escénico, música variada", 
  "Flauta, reproductor, internet libro","INTERNET",
  "Hojas de trabajo y reproductor de música", "USB", 
  "Tv, bocina, hojas de trabajo, investigación", 
  "Investigación bocina","Libros, compu",
  "Libro de texto, vestuarios, reproductor de sonido", 
  "Laptop", "Las Tics","Libros y computadora", "Libros y tecnología", 
  "Libros de textos, bocina, proyector", 
  "Libros, equipo de sonido, material didáctico",
  "Libro, bocina", "Libros de texto, computadora", 
  "Libro, bocina y salón para practicar",
  "equipo de computo", 
  "Música, bocina","Reproductores, USB y entorno",
  "Pantalla", "Pc, impresiones", "computadora", 
  "Texto", "Textos", "Textos y recurso auditivo", 
  "Textos y sistema de audio.","Videos,  cajonera, folletos",
  "TELEFONO CELULAR", "Teléfono internet", 
  "Teléfono móvil, bocina y otros enseres.", "Teléfono móvil, bocina y otros recursos.", 
  "Teléfono móvil, bocina y otros.", 
  "Textos y sistema de audio.",
  "Pc, reproductor, teléfono móvil, libros",
  "Marcadores, bocina, lapiceros, papel, etc", 
  "COMPUTADORA, MOVIL PERSONAL", "COMPUTADORA, REPRODUCTOR DE MÚSICA, USB, MATERIALES IMPRESOS, RECURSOS EN LÍNEA, AGENDA DE TRABAJO, HOJAS DE TRABAJO, COPIAS, REVISTAS, PERIÓDICOS, LIBROS DIGITALES.", 
  "Cañonera", "Celular, hojas", "Equipo audio visual , folleto",
  "Bocina e internet", "Bocina microfono celular internet", "Bocina y canciones", 
  "Bocina,", "Bocina, aparato reproductor de música  vestuario, maquillaje y peinados", 
  "Bocina, celular, libros, materiales didácticos.", "Bocina, etc", 
  "Bocina.", "Bocinas", "Bocinas  o audios", "Bocinas y videos de pasos", 
  "Bocinas, Internet, etc.", "Bocinas, coreografía, concursos de bailes", 
  "Bocinas, libros", "Bocinas, vestuarios, libros, cañonera", 
  "COMPUTADORA, MOVIL PERSONAL", "COMPUTADORA, REPRODUCTOR DE MÚSICA, USB, MATERIALES IMPRESOS, RECURSOS EN LÍNEA, AGENDA DE TRABAJO, HOJAS DE TRABAJO, COPIAS, REVISTAS, PERIÓDICOS, LIBROS DIGITALES.", 
  "Cañonera", "Celular, hojas","Folletos y bocina", 
  "Folletos, hoja de trabajo, bocina", "Folletos, hojas de trabajo y bocina",
  "Folletos", "Folletos  hojas de trabajo  bocina", 
  "Libro, pizarra, computadora cañonera etc.", 
  "Internet cañonera Computadora,marcadores bocina micrófono", 
  "Internet y bocina", "Internet, bocina etc"
)

### material audiovisual y didáctico
valores_audio_didactico_danza2 <- c(
  "Audio", "Audio, Videos", "Audio, música", 
  "Audios, bocinas, imágenes", "Audiovisual, folletos y otros aparatos para danzar", 
  "Audiovisual, trajes, libro", "Con enfoques didácticos",
  "Computadora, teléfono, internet, audios hojas",
  "Folletos audios videos","Folletos y videos.", 
  "Internet, videos, ciertos articulos", "Afiches",
  "Libros carteles loterías","Recursos para actividades prácticas",
  "Libros, recursos audiovisual,", "experiencia propia, videos", 
  "Libros, videos, revistas", "copias, información impresa, capturas",
  "Videos", "Videos de Internet, documentos, libros de Internet y diapositivas", 
  "Videos,  cajonera, folletos", "Videos, imágenes, Internet y audios", 
  "Videos, investigaciones, folletos.", "Videos, música, ejemplificaciones, exposiciones, ejemplificaciones,", 
  "audivisuales, literatura, vestuario, pistas, entre otras...", 
  "Sonido, audiovisual.", "Textos y recurso auditivo", 
  "Páginas web,materiales didacticos", "Recursos audiovisuales", 
  "Multimedia", "Música,  videos.", "Música, bocina", 
  "Material", "Material Didáctico Variado", 
  "Material audiovisual, libros, trajes", "Material audiovisual.", 
  "Material didáctico", "Material didáctico y investigación del tema", 
  "Material digital", "Material impreso y libros propios del docente", 
  "Material investigado", "Material pedagógico, copias, material  auditivo", 
  "Material propio", "Materiales básicos",
  "Libros, equipo de sonido, material didáctico",
  "Libros en digital, videos", "Malla curricular", 
  "Computadora", "Computadora  e impresora", "Cuerpo humano, Celular", 
  "Conocimientos propios, habilidad, videos, audios", 
  "Computadora con internet", "Computadora e Internet", "Computadora impresoras, carteles, entre otros", 
  "Computadora y bocinas", "Computadora, Internet", "Computadora, Libros, hojas de trabajo, celular, marcadores.", 
  "Computadora, libro, celular", "Computadora, libros.", "Computadora, música propia", 
  "Computadora, teléfono, internet, audios hojas", "Computadora.  Bocina. Uso de Internet", 
  "Bocinas, coreografía, concursos de bailes", 
  "Bocina, celular, libros, materiales didácticos.",
  "COMPUTADORA, REPRODUCTOR DE MÚSICA, USB, MATERIALES IMPRESOS, RECURSOS EN LÍNEA, AGENDA DE TRABAJO, HOJAS DE TRABAJO, COPIAS, REVISTAS, PERIÓDICOS, LIBROS DIGITALES."
)

### recursos de expresion corporal
valores_expre_corporal2 <- c(
  "Audiovisual, folletos y otros aparatos para danzar", 
  "Audiovisual, trajes, libro", "Cintas y banderas, Aros, mantones y mantos"
)

### libros
valores_libros_danza2 <- c(
  "Audiovisual, trajes, libro", "Bocina, celular, libros, materiales didácticos.",
  "Bocinas, libros", "Bocinas, vestuarios, libros, cañonera", 
  "Computadora, Libros, hojas de trabajo, celular, marcadores.", 
  "Computadora, libro, celular", "Computadora, libros.",
  "Enciclopedias", "Flauta, reproductor, internet libro",
  "Libro", "Principios de metodo Bapne", 
  "Redes móviles,  libros de texto","libros", 
  "Santillana y otros","enlaces y textos", 
  "Videos de Internet, documentos, libros de Internet y diapositivas", 
  "Material impreso y libros propios del docente", 
  "Libro de Telesecundaria", "Libro de expresión artística y copias", 
  "Libro de texto", "Libro de texto, vestuarios, reproductor de sonido", 
  "Libro de textos e impresiones", "Libro pdf", "Libro, bocina", 
  "Libro, bocina y salón para practicar", "Libro, folleto", "Libro, pizarra, computadora cañonera etc.", 
  "Libros", "Libros .guias etc", "Libros carteles loterías", "Libros de Editorial Educativa", 
  "Libros de texto", "Libros de texto, computadora", "Libros de texto, redes moviles", 
  "Libros de textos, bocina, proyector", "Libros digitales", "Libros e Internet", 
  "Libros e impresiones", "Libros e internet", "Libros en digital, videos", 
  "Libros en páginas webs", "Libros propios", "Libros propios de santillana", 
  "Libros y computadora", "Libros y folletos", "Libros y tecnología", 
  "Libros, CNB", "Libros, compu", "Libros, consultas e internet", 
  "Libros, equipo de sonido, material didáctico", "Libros, investigo Internet y el Cnb", 
  "Libros, marcadores, hojas de trabajo", "Libros, recursos audiovisual,", 
  "Libros, revistas", "Libros, videos, revistas",
  "Pc, reproductor, teléfono móvil, libros"
)

### guias y folletos
valores_guia_folleto_danza2 <- c(
  "Audiovisual, folletos y otros aparatos para danzar",
  "Folleto", "Folleto elaborado de acuerdo a la malla curricular, recursos guardados con anterioridad", 
  "Folletos", "Folletos  hojas de trabajo  bocina", "Folletos audios videos", 
  "Folletos e Internet", "Folletos e impresiones", "Folletos y bocina", 
  "Folletos y videos.", "Folletos, hoja de trabajo, bocina", 
  "Folletos, hojas de trabajo y bocina", 
  "Videos, investigaciones, folletos.",
  "Equipo audio visual , folleto","Videos,  cajonera, folletos",
  "Libros y folletos","Utilizo folletos", 
  "Solo folletos", "Son investigaciones de Internet y iniciamos con los criterios que integran la danza",
  "Libro, folleto","Libros .guias etc",
  "Guia", "Guia de aprendizaje", "Guias", "Guias de trabajo según cnb", 
  "Guias y folletos"
)

### espacio educativo
valores_espacio_danza2 <- c(
  "Bocina ,espacio  escénico, música variada", "La música, el espacio y el cuerpo",
  "Libro, bocina y salón para practicar","Vestuarios escenario etc" 
)

### recursos escenicos
valores_escenicos_danza <- c(
  "Bocina, aparato reproductor de música  vestuario, maquillaje y peinados", 
  "Bocinas, vestuarios, libros, cañonera",
  "Flauta, reproductor, internet libro","Alfombras",
  "audivisuales, literatura, vestuario, pistas, entre otras...", 
  "Marimba, instrumentos musicales", "Vestuarios escenario etc", 
  "Libro de texto, vestuarios, reproductor de sonido", 
  "Cintas y banderas, Aros, mantones y mantos"
)

### recursos digitales
valores_recurso_digital_danza <- c(
  "COMPUTADORA, REPRODUCTOR DE MÚSICA, USB, MATERIALES IMPRESOS, RECURSOS EN LÍNEA, AGENDA DE TRABAJO, HOJAS DE TRABAJO, COPIAS, REVISTAS, PERIÓDICOS, LIBROS DIGITALES.", 
  "Cnb", "Cnb Guatemala y libros digitales","Consultas en la web",
  "Digitales", "Dispositivos digitales",
  "Internet", "Internet  e impresión", "virtuales",
  "Medios digitales", "Sitios web","Trabajos de internet",
  "Páginas web", "Páginas web,materiales didacticos", 
  "Paginas web", "Pdfs, páginas web", 
  "Recursos de la Web y herramientas virtuales",
  "Lo q se tiene en la plataforma", "Material digital",
  "Investigacion", "Libros en páginas webs",
  "Investigaciones", "Investigaciones impresas o digital", "Investigaciones propias en páginas web y contextualizacion de las mismas", 
  "Investigaciones propias para el desarrollo de los diferentes contextos de la danza", 
  "Investigación bocina",  "Investigaciónes, Pizarron, marcadores", 
  "Internet cañonera Computadora,marcadores bocina micrófono", 
  "Internet y bocina", "Internet, bocina etc", "Internet, libros digitales", 
  "Internet, videos, ciertos articulos", "Internet."
)

### cnb
valores_cnb_danza3 <- c(
  "Cnb", "Cnb Guatemala y libros digitales","Libros, CNB", 
  "Libros, investigo Internet y el Cnb"
  
)


### material de papeleria
valores_papeleria_danza2 <- c(
  "Copias", "Cuaderno de actividades, cuaderno de notas.", 
  "Económicos fotocopias", "Impresiones", "Impresos y textos",
  "Hojas de actividades", "Hojas de bon de colores crayones",
  "Investigaciónes, Pizarron, marcadores", 
  "Libros, marcadores, hojas de trabajo",
  "Temperas hojas de colores", "Fotocopias",
  "Maradores", "Marcador, almohadilla, pizzara, cartulina, videos.", 
  "Marcadores papelogragos cartulinas entre otros", "Marcadores, bocina, lapiceros, papel, etc", 
  "Libros, revistas", "Libros, videos, revistas",
  "Hojas de trabajo", "Hojas de trabajo y carteles de apoyo", 
  "Hojas de trabajo y reproductor de música"
)

### mobiliario
valores_mobiliario_danza2 <- c(
  "Investigaciónes, Pizarron, marcadores", 
  "Libro, pizarra, computadora cañonera etc."
)

### planificador
valores_planificador_danza3 <- c(
  "Planificador docente y guías de aprendizajes"
)

### no especifica
valores_noespecifica_danza5 <- c(
  "1", "Económicos",
  "Lo que se pueda hacer", "Soy arte","Todo", "Todos"

)

### ninguno
valores_ninguno_danza2 <- c(
  "Desarrollo los temas investigando leyendo", "Experiencia propia", 
  "Habilidades", "Teoria y practico",
  "Ningu", "Ninguna", "Ninguno", "No",
  "mi hijo estudio y le pregunto", "ninguno"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    danza_recursos_propios = case_match(
      danza_recursos_propios,
      all_of(valores_ninguno_danza2) ~ "Ninguno",
      all_of(valores_noespecifica_danza5) ~ "No especifica el recurso",
      all_of(valores_planificador_danza3) ~ "Planificador",
      all_of(valores_mobiliario_danza2) ~ "Mobiliario",
      all_of(valores_papeleria_danza2) ~ "Material de papelería",
      all_of(valores_cnb_danza3) ~ "CNB",
      all_of(valores_recurso_digital_danza) ~ "Recursos digitales",
      all_of(valores_escenicos_danza) ~ "Recursos Escénicos",
      all_of(valores_espacio_danza2) ~ "Espacio educativo",
      all_of(valores_guia_folleto_danza2) ~ "Guías y folletos",
      all_of(valores_libros_danza2) ~ "Libros",
      all_of(valores_expre_corporal2) ~ "Recursos de expresión corporal",
      all_of(valores_audio_didactico_danza2) ~ "Material audiovisual y didáctico",
      all_of(valores_tecnologia_danza2) ~ "Equipo tecnológico",
      .default = danza_recursos_propios
    )
  )

## volver a ver errores

valores_danza_recursos_propios <- df_dyd |>
  filter(!(is.na(danza_recursos_propios))) |>
  select(danza_recursos_propios) |>
  tabyl(danza_recursos_propios)

# View(valores_danza_recursos_propios)

# danza_otro_ultima_vez_libros_mineduc ----

## ver errores
valores_danza_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(danza_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(danza_otro_ultima_vez_libros_mineduc ))) |>
  select(danza_otro_ultima_vez_libros_mineduc) |>
  tabyl(danza_otro_ultima_vez_libros_mineduc)

dput(valores_danza_otro_ultima_vez_libros_mineduc)


## Define los vectores ----


### no especifica
valores_sinespecificar_danza <- c(
  "1","Solo el planifocador"
)

### 2016
valores_danza_2016 <- c(
  "2016"
)

### 2017
valores_danza_2017 <- c(
  "2017"
)

### 2018
valores_danza_2018 <- c(
  "2018", "2018 libro de Expresión Artística"
)

### 2019
valores_danza_2019 <- c(
  "2019"
)

### 2020
valores_danza_2020 <- c(
  "2020"
)

### 2021
valores_danza_2021 <- c(
  "2021"
)

### no ha recibido
valores_nunca_danza <- c(
  "N/A", "Ninguno", "Ningún", "No", "No he recibido", 
  "No se a recibido", "No se ha recibido material propio sobre esta área", 
  "No se ha recibido ningún material de esta", "Nunca", "nunca"
)

### no recuerda
valores_norecuerda_danza <- c(
  "No recuerdo"
)

## Recode con vectores ----

df_dyd <- df_dyd |>
  mutate(
    danza_otro_ultima_vez_libros_mineduc = case_match(
      danza_otro_ultima_vez_libros_mineduc,
      all_of(valores_norecuerda_danza) ~ "No recuerda",
      all_of(valores_sinespecificar_danza) ~ "No especifica",
      all_of(valores_nunca_danza) ~ "Nunca ha recibido",
      all_of(valores_danza_2021) ~ "2021",
      all_of(valores_danza_2020) ~ "2020",
      all_of(valores_danza_2019) ~ "2019",
      all_of(valores_danza_2018) ~ "2018",
      all_of(valores_danza_2017) ~ "2017",
      all_of(valores_danza_2016) ~ "2016",
      .default = danza_otro_ultima_vez_libros_mineduc
    )
  )

## volver a ver errores

valores_danza_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(danza_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(danza_otro_ultima_vez_libros_mineduc ))) |>
  select(danza_otro_ultima_vez_libros_mineduc) |>
  tabyl(danza_otro_ultima_vez_libros_mineduc)

# View(valores_danza_otro_ultima_vez_libros_mineduc)

# danza_tipo_materiales_recibidos ----

## ver errores

valores_danza_tipo_materiales_recibidos <- df_dyd |>
  filter(danza_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(danza_tipo_materiales_recibidos ))) |>
  select(danza_tipo_materiales_recibidos) |>
  tabyl(danza_tipo_materiales_recibidos)

dput(valores_danza_tipo_materiales_recibidos)

## Define los vectores ----

### guia y planificador
valores_guia_planificador_danza <- c(
  "GUÍA DE APRENDIZAJE Y PLANIFICADOR DE EXPRESIÓN", 
  "Gua y planificador", "Guia", "Guias", "Guías y planificadores"
)

### libro
valores_libro_danza <- c(
  "Libro de musica", "Texto"
)

### no especifica
valores_sinespecificar_danza2 <- c(
  "Si"
)

### ninguno
valores_ninguno_danza3 <- c(
  "Ninguno"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    danza_tipo_materiales_recibidos = case_match(
      danza_tipo_materiales_recibidos,
      all_of(valores_ninguno_danza3) ~ "Ninguno",
      all_of(valores_sinespecificar_danza2) ~ "No especifica el material",
      all_of(valores_libro_danza) ~ "Libro",
      all_of(valores_guia_planificador_danza) ~ "Guía de aprendizaje y planificador",
      .default = danza_tipo_materiales_recibidos
    ) 
  )
 
## volver a ver errores

valores_danza_tipo_materiales_recibidos <- df_dyd |>
  filter(danza_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(danza_tipo_materiales_recibidos ))) |>
  select(danza_tipo_materiales_recibidos) |>
  tabyl(danza_tipo_materiales_recibidos)

# View(valores_danza_tipo_materiales_recibidos)

# musica_libros_primero ----
## ver errores
valores_musica_libros_primero <- df_dyd |>
  filter(musica_uso_libros == "Sí") |>
  filter(musica_grados == "Primer grado") |>
  filter(!(is.na(musica_libros_primero))) |>
  select(musica_libros_primero) |>
  tabyl(musica_libros_primero)

dput(valores_musica_libros_primero)

## Define los vectores ----

### expresion artistica
valores_expre_artistica_musica <- c(
  "EXpresión Artística", "Expresión Artística", 
  "Expresión artística"
)

### educación artistica
valores_edu_artistica_musica <- c(
  "Educación artística"
)

### guia y planificador
valores_guia_plani_musica <- c(
  "GUIA DE APRENDIZAJE , LIBROS DE CONSULTA", 
  "GUÍA DE APRENDIZAJE Y PLANIFICADOR DE EXPRESIÓN ARTISTICA", 
  "Guia de Educación Artistica", "Guia y planificador", "Guia y planificador expresión artística", 
  "Guias y planificadores", "Guías de aprendizaje y Planificador docente", 
  "Guías y planificadores", "Planificador del facilitador y Guia de Aprendizaje Expresión Artística 1", 
  "Planificador y guias", "Planificador y guías de aprendizaje",
  "Es la guia de educacion artistica integrada"
)

### no especifica
valores_noespecifica_musica <- c(
  "Editora Educativa", "Enciclopedias", 
  "Santillana", "Sólo el que envió el MINEDUC"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    musica_libros_primero = case_match(
      musica_libros_primero,
      all_of(valores_noespecifica_musica) ~ "No especifica el nombre",
      all_of(valores_guia_plani_musica) ~ "Guía de aprendizaje y planificador",
      all_of(valores_edu_artistica_musica) ~ "Educación Artística",
      all_of(valores_expre_artistica_musica) ~ "Expresión Artística",
      .default = musica_libros_primero
    )
  )

## volver a ver errores

valores_musica_libros_primero <- df_dyd |>
  filter(musica_uso_libros == "Sí") |>
  filter(musica_grados == "Primer grado") |>
  filter(!(is.na(musica_libros_primero))) |>
  select(musica_libros_primero) |>
  tabyl(musica_libros_primero)

# View(valores_musica_libros_primero)

# musica_libros_segundo ----

## ver errores

valores_musica_libros_segundo <- df_dyd |>
  filter(musica_uso_libros == "Sí") |>
  filter(musica_grados == "Segundo grado") |>
  filter(!(is.na(musica_libros_segundo))) |>
  select(musica_libros_segundo) |>
  tabyl(musica_libros_segundo)

dput(valores_musica_libros_segundo)

## Define los vectores ----

### educacion artistica
valores_edu_artistica_musica2 <- c(
  "Educación Artística", "Música,  Educación Artística."
)


### expresion artistica guia y planificador
valores_guia_plani_musica2 <- c(
  "Guia de aprendizaje expresión artística", "Guía de Educación Artística", 
  "Guía de Expresión Artística y Planificador", "Guía de aprendizaje", 
  "Guía de aprendizaje y Planificador del docente", "Guías de Telesecundaria"
)

### páginas web
valores_web_musica <- c(
  "Google",  "Páginas web y material didactico"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    musica_libros_segundo = case_match(
      musica_libros_segundo,
      all_of(valores_web_musica) ~ "Páginas web",
      all_of(valores_guia_plani_musica2) ~ "Expresión Artística: Guía de aprendizaje y planificador",
      all_of(valores_edu_artistica_musica2) ~ "Educación Artística",
      .default = musica_libros_segundo
    )
  )

## volver a ver errores

valores_musica_libros_segundo <- df_dyd |>
  filter(musica_uso_libros == "Sí") |>
  filter(musica_grados == "Segundo grado") |>
  filter(!(is.na(musica_libros_segundo))) |>
  select(musica_libros_segundo) |>
  tabyl(musica_libros_segundo)

# View(valores_musica_libros_segundo)

# musica_libros_tercero ----

## ver errores

valores_musica_libros_tercero <- df_dyd |>
  filter(musica_uso_libros == "Sí") |>
  filter(musica_grados == "Tercer grado") |>
  filter(!(is.na(musica_libros_tercero))) |>
  select(musica_libros_tercero) |>
  tabyl(musica_libros_tercero)

dput(valores_musica_libros_tercero)

## Define los vectores ----

### educación musical
valores_edu_musical <- c(
  "Educación Musical", "Guia de Educacion Musical"
)

### guia de aprendizaje
valores_guia_aprendizaje_musica <- c(
  "Guia", "Guía de aprendizaje", 
  "Guía de aprendizaje de expresión artística",
  "Guías y planificador", "Planificador docente y guías de aprendizajes"
)

### planificador del facilitador
valores_planificador_musica <- c(
  "Planificador docente y guías de aprendizajes",
  "Guías y planificador","Planificador de Educación Artística"
)

### manual de musica
valores_manual_musica <- c(
  "Manual de Musica"
)

### no especifica 
valores_noespecifica_musica2 <- c(
  "Libro digital", "Libros digitales", "Libros dogitales"
)

### paginas web
valores_web_musica2 <- c(
  "Paginas web"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    musica_libros_tercero = case_match(
      musica_libros_tercero,
      all_of(valores_web_musica2) ~ "Páginas web",
      all_of(valores_noespecifica_musica2) ~ "No especifica el nombre",
      all_of(valores_manual_musica) ~ "Manual de Música",
      all_of(valores_planificador_musica) ~ "Planificador del facilitador",
      all_of(valores_guia_aprendizaje_musica) ~ "Guía de aprendizaje",
      all_of(valores_edu_musical) ~ "Educación Musical",
      .default = musica_libros_tercero
    )
  )

## volver a ver errores

valores_musica_libros_tercero <- df_dyd |>
  filter(musica_uso_libros == "Sí") |>
  filter(musica_grados == "Tercer grado") |>
  filter(!(is.na(musica_libros_tercero))) |>
  select(musica_libros_tercero) |>
  tabyl(musica_libros_tercero)

# View(valores_musica_libros_tercero)

# musica_recursos_centro_educativo ----

## ver errores
valores_musica_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(musica_recursos_centro_educativo))) |>
  select(musica_recursos_centro_educativo) |>
  tabyl(musica_recursos_centro_educativo)

dput(valores_musica_recursos_centro_educativo)

## Define los vectores ----

### mobiliario
valores_mobiliario_musica <- c(
  "Alumnos pizarra y los chicos llevan sus instrumentos",
  "Pizarra, computadora, cañonera", "Pizarra, flauta, lapiceros impresiones", 
  "Pizarra, marcadores, almohadillas, copias, impresiones, tinta para marcadores", 
  "Pizarra, marcadores, televisión", "Pizarrón", 
  "Pizarrón de Formica, marcador de formica, hojas papel bond diferentes tamaños, pliegos papel bond", 
  "Pizarrón, bocina electrónica, salón de clases", 
  "Pizarrón, bocina, cañonera.", 
  "Televisor para reproducir música y visualizar videos, red de internet, pizarra pequeña", 
  "Instrumentos musicales, mobiliario, Equipo de Sonido, Pizarron, escritorios, materiales como marcadores, cuadernos, almohadillas, papel bond y otros ..", 
  "Aula pura , pizarrón",  "Hojas, libro, pizarrón y bocinas"
)

### audiovisual y didáctico
valores_audi_didactico_musica <- c(
  "Audios", "Audios o bocinas", "Audiovisual", 
  "Bocinas, impresiones, cartulinas.",
  "Libros y materiales pedagogicos","audiovisuales", 
  "Teclado. Flauta.material audiovisual", 
  "Marcadores  y cartulinas", "Música",   "Valija didáctica",
  "Recursos para actividades prácticas", 
  "Planificador, guía de aprendizaje, valija didáctica", 
  "Merodos del maestro Alvarado Coronado", "Métodos de los instrumentos", 
  "Marcadores y cartulinas", "Material", 
  "material didactico", 
  "recursos audivisuales, equipo de sonido, instrumentos musicales (viento, resonancia, percusión), entre otros", 
  "Material audio visual", "Material de apoyo", "Material digital", 
  "Material impreso", "Material impreso y físico", "Materiales de apoyo", 
  "Libros, equipo de sonido, piano, guitarra, impresiones de m ateriales didácticos, accesorios para pizarra, hojas para impresiones, laboratorio exclusivo para computo e impresión.",
  "Didácticos",  "FOLLETOS, AUDIOVISUALES",
  "Impresos creativo"
)

### tecnologico
valores_tecnologia_musica <- c(
  "Audios o bocinas", "Pantalla, Impresora",
  "CNB, Flauta, bocina", "Libros , smart tv, bocina", 
  "Instrumentos musicales y Audio,computadora", 
  "Instrumentos musicales, mobiliario, Equipo de Sonido, Pizarron, escritorios, materiales como marcadores, cuadernos, almohadillas, papel bond y otros ..", 
  "Instrumentos musicales. Audio", "Pizarra, marcadores, televisión",
  "Impresiones, copias, bocina,", 
  "recursos audivisuales, equipo de sonido, instrumentos musicales (viento, resonancia, percusión), entre otros",
  "Teclados, guitarras, computadora, bocina", "Tecnología, espacios, bocina", 
  "Tecnológico y pedagógico",   "Textos. Proyector", 
  "Televisor para reproducir música y visualizar videos, red de internet, pizarra pequeña", 
  "Pizarrón, bocina electrónica, salón de clases", "Pizarrón, bocina, cañonera.", 
  "Libros, Smart tv",  "Pizarra, computadora, cañonera", 
  "Libros, equipo de sonido, piano, guitarra, impresiones de m ateriales didácticos, accesorios para pizarra, hojas para impresiones, laboratorio exclusivo para computo e impresión.", 
  "Impresiones, marcadores, proyector.", "Impresora, cañonera y pizarra", 
  "Cañonera", "Cañonera, bocina", "Centro de computación", 
  "Bocina", "Bocina y Micrófono", "Recursos  tecnológico",
  "Electricidad",   "Hojas, libro, pizarrón y bocinas", 
  "Folletos, instrumentos musicales básicos, bocinas, amplificadores, cañonera, reproductores", 
  "Equipi de sonido y aula", "Proyector", 
  "Equipo audiovisual", "Equipo de sonidos y folletos",
  "Computadora", "Computadora, manual",
  "Bocina y un salón para realizar las actividades", "Bocina, cañonera", 
  "Bocina, instrumentos", "Bocina, libros micrófono", 
  "Bocinas, impresiones, cartulinas."
)

### espacio educativo
valores_espacio_musica <- c(
  "Aula pura , pizarrón",
  "Bocina y un salón para realizar las actividades",
  "Equipi de sonido y aula", "Salón", "Salón de clases, pizarra", 
  "Escenario, sonido, hojas, pizarras, lápices",
  "Pizarrón, bocina electrónica, salón de clases"
)

### instrumentos
valores_instrumentos <- c(
  "Bocina, instrumentos",
  "CNB, Flauta, bocina",  "Pizarra, flauta, lapiceros impresiones", 
  "Libros e instrumentos", "Libros, redoblantes, micrófonos", 
  "Libros propios y flauta",  "Un piano", 
  "recursos audivisuales, equipo de sonido, instrumentos musicales (viento, resonancia, percusión), entre otros",
  "Teclado y guitarra", "Teclado. Flauta.material audiovisual", 
  "Teclados, guitarras, computadora, bocina", 
  "Flauta , guitarra", "Flauta y guitarra", 
  "bombos, redoblantes, liras, triples y trompetas.", 
  "flauta, guitarra",
  "Flauta,guitarra", "Flautas",  "Redoblantes, güiras, triples.", 
  "Libros, equipo de sonido, piano, guitarra, impresiones de m ateriales didácticos, accesorios para pizarra, hojas para impresiones, laboratorio exclusivo para computo e impresión.",
  "Guitarra, teclado, libros de apoyo", 
  "Guitarras","INTRUMENTOS MUSICALES",
  "Folletos, instrumentos musicales básicos, bocinas, amplificadores, cañonera, reproductores", 
  "Folletos, instrumentos musicales.", 
  "Instrumentos", "Instrumentos de aire", 
  "Instrumentos musicales", "Instrumentos musicales y Audio,computadora", 
  "Instrumentos musicales, mobiliario, Equipo de Sonido, Pizarron, escritorios, materiales como marcadores, cuadernos, almohadillas, papel bond y otros ..", 
  "Instrumentos musicales. Audio", 
  "Brindaron guitarras pero es material de un programa implementado no por ser recursos del establecimiento"
)

### cnb
valores_cnb_musica <- c(
  "CNB", "CNB PLANIFICADOR GUIAS DE APRENDIZAJE",
  "Cnb", "Libros  Cnb", 
  "CNB, Flauta, bocina"
)

### material de papeleria
valores_papeleria_musica <- c(
  "Copias", "Copias impresiones",
  "Pizarra, flauta, lapiceros impresiones",
  "Pizarra, marcadores, almohadillas, copias, impresiones, tinta para marcadores", 
  "Copias y libros","Escenario, sonido, hojas, pizarras, lápices", 
  "Fotocopias", "Fotocopias, marcadores, pizarra",
  "Impresiones  fotocopias", "Impresiones, copias, bocina,", 
  "Impresiones, marcadores, proyector.",
  "Pizarrón de Formica, marcador de formica, hojas papel bond diferentes tamaños, pliegos papel bond", 
  "Marcador pizarra", "Marcadores  y cartulinas", 
  "Marcadores y cartulinas", "Marcadores y folletos",
  "Instrumentos musicales, mobiliario, Equipo de Sonido, Pizarron, escritorios, materiales como marcadores, cuadernos, almohadillas, papel bond y otros ..", 
  "Hojas y marcadores", "Hojas, libro, pizarrón y bocinas"
)

### recursos digitales
valores_digitales_musica <- c(
  "Digitales", "En plataforma esta todo lo u7", 
  "Pdf, páginas webs",  "Virtuales","enlaces", 
  "paginas de internet", 
  "Internet", "Investigación", "Links internet", "Paginas web",
  "Plataformas digitales y libros de otros países.",
  "Páginas web"
)

### libros
valores_libros_musica <- c(
  "Editora Educativa",
  "Expresión artística, educación musical 1°, 2°, 3°", 
  "Formación musical 3", 
  "LIBRO DE TEXTO", "Libro", "Libro de Expresión Artística de IGER", 
  "Libro de texto", "Libro guia", "Libros", "Libros  Cnb", 
  "Libros , smart tv, bocina", "Libros Santillana", "Libros de texto", 
  "Libros de textos", "Libros e impresiones", "Libros e instrumentos", 
  "Libros entre otros", "Libros propios y flauta", "Libros relacionados al curso", 
  "Libros y materiales pedagogicos", "Libros y planificador.", 
  "Libros, Smart tv", "Libros, equipo de sonido, piano, guitarra, impresiones de m ateriales didácticos, accesorios para pizarra, hojas para impresiones, laboratorio exclusivo para computo e impresión.", 
  "Libros, folletos", "Libros, redoblantes, micrófonos", "Links internet", 
  "Los libros de Educación musical en cada uno de los tres niveles", 
  "Los libros de los tres grados de educación musical pero desactualizados", 
  "Los textos",   "libros de textos", 
  "Teoría Musical 3", "Texto", "Textos", "Textos. Proyector", 
  "Hojas, libro, pizarrón y bocinas", 
  "Guitarra, teclado, libros de apoyo"
)

### guias y folletos
valores_guia_folleto_musica <- c(
  "FOLLETOS, AUDIOVISUALES",
  "Folleto", "Folletos", "Folletos, instrumentos musicales básicos, bocinas, amplificadores, cañonera, reproductores", 
  "Folletos, instrumentos musicales.",
  "GUIA DE APRENDIZAJE",   "guía de aprendizaje",
  "Planificador del facilitador y guía del facilitador en Telesecundaria", 
  "Planificador y guías", "Planificador, guía de aprendizaje, valija didáctica", 
  "Libros, folletos","Marcadores y folletos", 
  "Guía de Telesecundaria", "Guía de aprendizaje", 
  "Guía de educación artística", "Guía metodológico para docente", 
  "Guía metodológico para el docente", "Guía y planificador", 
  "Guías de aprendizaje y Planificador docente", "Guías de aprendizajes", 
  "Guías y planificadores",
  "GUÍA DE APRENDIZAJE Y PLANIFICADOR", "Guia", "Guia de aprendizaje expresión artística", 
  "Guia de educacion artistica integrada", "Guia y planificador", 
  "Guias", "Guias y folletos", "Guias y planificadores"
)

### ninguno
valores_ninguno_musica <- c(
  "N/A", "Nada", "Nimguno", "Ninguna", "Ninguno", 
  "Ninguno, trabajo por las tardes en una cooperativa", "Ningún", 
  "Ningúno", "No", "No hay", "ninguno","0",
  "El centro educativo no me ha proporcionado ningún material del curso de música", 
  "No se cuenta con materiales propios para el curso de música"
)

### planificador
valores_planificador_musica2 <- c(
  "Planificador", "Planificador de Educación Artística", 
  "Planificador del facilitador y guía del facilitador en Telesecundaria", 
  "Planificador y guías", "Planificador de Educación Artística",
  "Planificador, guía de aprendizaje, valija didáctica"
)

### no especifica
valores_noespecifica_musica3 <- c(
  "1","Ampliación",   "solfa",
  "Recursos básico", "Recursos propios de cada grado"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    musica_recursos_centro_educativo = case_match(
      musica_recursos_centro_educativo,
      all_of(valores_noespecifica_musica3) ~ "No especifica el recurso",
      all_of(valores_ninguno_musica) ~ "Ninguno",
      all_of(valores_guia_folleto_musica) ~ "Guías y folletos",
      all_of(valores_libros_musica) ~ "Libros",
      all_of(valores_digitales_musica) ~ "Recursos digitales",
      all_of(valores_papeleria_musica) ~ "Material de papelería",
      all_of(valores_cnb_musica) ~ "CNB",
      all_of(valores_planificador_musica2) ~ "Planificador del facilitador",      all_of(valores_instrumentos) ~ "Instrumentos musicales",
      all_of(valores_espacio_musica) ~ "Espacio educativo",
      all_of(valores_tecnologia_musica) ~ "Equipo tecnológico",
      all_of(valores_audi_didactico_musica) ~ "Material audiovisual y didáctico",
      all_of(valores_mobiliario_musica) ~ "Mobiliario",
      .default = musica_recursos_centro_educativo
    )
  )

## volver a ver errores

valores_musica_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(musica_recursos_centro_educativo))) |>
  select(musica_recursos_centro_educativo) |>
  tabyl(musica_recursos_centro_educativo)

# View(valores_musica_recursos_centro_educativo)

# musica_recursos_propios ----

## ver errores

valores_musica_recursos_propios <- df_dyd |>
  filter(!(is.na(musica_recursos_propios))) |>
  select(musica_recursos_propios) |>
  tabyl(musica_recursos_propios)

dput(valores_musica_recursos_propios)

## Define los vectores ----

### audiovisual y didáctico
valores_audio_didactico_musica2 <- c(
  "Audio, música, Internet", "Audios Videos", 
  "Audios, Internet, videos.", "Audios, bocina, computadora, internet", 
  "Audiovisual","Folletos, hojas de trabajo, bocina", 
  "Reutilizables e instrumental", 
  "Videos", "Videos y herramientas digitales", 
  "Videos y libros en línea", "Videos,  e investigaciones.", "Videos, ejemplificaciones, música, flauta, lecturas, investigaciones, exposiciones", 
  "Videos, revistas", "Videos..", "Visuales, sonoros e impreso", 
  "audivisual, sonido, entre otros....",
  "Folletos videos audios etc", "Letra de canciones, audios, otros", 
  "Investigaciónes de temas en la web y material didáctico", 
  "Flauta, guitarra, pandereta,triangulo,material didactico",
  "Fichas de trabajo impreso.", "Instrumentos y materiales básico", 
  "Hojas de actividades", "Instrumento musical, bocina, y otros", 
  "Hojas de trabajo", "Hojas de trabajo, investigación", "Hojas de trabajo, material didáctica, reproductor de audio, útiles sonoros", 
  "Hojas de trabajo, útiles escolares como crayón, marcadores y temperaturas", 
  "Hojas entre otros", "Hojas impresas", "Hojas instrumentos para la práctica", 
  "Hojas, material didáctico","Instrumentos y hojas de trabajo",
  "Flauta , equipo audiovisual , folleto.","Malla curricular",
  "Libro de texto, impresiones, material didactico", 
  "Sonido,  audiovisual.","Tiras didácticas e impresiones", 
  "Recurso reciclable, matarla pedagógico", "Recursos de actividades prácticas", 
  "Metodo orf, dalcroze y kodaly", "Proyector, impresiones, flauta, bocina, videos",
  "Libro y cuaderno Pedagógico","Libros y audios",
  "Material", "Material Didáctico Variado", "Material de apoyo", 
  "Material didactico,paginas web", "Material didactico,páginas web", 
  "Material didáctico", "Material impreso", "Materiales audiovisuales", 
  "Libros pedagógicos musicales, diapositivas, audios, documentos trabajados en sibelius y otros", 
  "Libros de lectura rítmica, material audiovisual", 
  "Instrumentos elaborados, material audiovisual", 
  "Instrumentos, hojas de trabajo y otros", 
  "Instrumentos musicales, material reciclable para hacer instrumentos musicales para realizar sonidos"
)

### cnb
valores_cnb_musica2 <- c(
  "CNB", "Cnb", "Cnb guatemala y libros digitales",
  "Cnb guatemala y libros digitales",
  "CNB, folleto elaborado de acuerdo a la malla curricular, libros de Ethel Batres, materiales guardados con anterioridad."
)


### guias y folletos
valores_guia_folleto_musica2 <- c(
  "CNB, folleto elaborado de acuerdo a la malla curricular, libros de Ethel Batres, materiales guardados con anterioridad.", 
  "Copias folleto","Flauta , equipo audiovisual , folleto.",
  "Guia de aprendizaje", "Guias folletos", "Planificador",
  "Guías de aprendizajes", "Guías de educación artística", 
  "Guías y planificadores", "Guías....textos.....",
  "Folletos", "Folletos e impresiones", "Folletos hojas de trabajo  bocina", 
  "Folletos videos audios etc", "Folletos y hojas de trabajo y bocina", 
  "Folletos y libros", "Folletos, flauta, etc.", "Folletos, hojas de trabajo, bocina"
)

### instrumentos
valores_instrumentos2 <- c(
  "Asistencia, flauta, celular","Proyector, impresiones, flauta, bocina, videos", 
  "BOCINA, FOLLETOS E INSTRUMENTOS MUSICALES",
  "Bocina, computadora, celular con datos, instrumentos o útiles sonoros de percusiones, ukulele", 
  "Bocina, computadora, flauta", "Bocina, flauta",
  "Bocina, hojas de actividades y flautas", 
  "Bocinas, micrófono, flauta, karaokes", "Textos, Instrumentos Musicales.",
  "Computadora,  celular, guitarra", "Computadora, flauta, marcadores, almohadilla, lapicero.",
  "Celulares, bocinas , instrumentos propios", 
  "Videos, ejemplificaciones, música, flauta, lecturas, investigaciones, exposiciones", 
  "Flauta, guitarra", "Libros de texto, flautas, partituras musicales, redes moviles", 
  "Libro, Instrumentos", "Marcadores, hojas de papel, lapiceros, instrumentos, platea etc", 
  "Marimba,", "Marimboide, cajón peruano, bocina, libros, partituras, baquetas.", 
  "Libros propios, investigaciones, instrumentos musicales y libros de pedagogos musicales vistas en talleres de fladem Guat", 
  "Libro, bocina, instrumentos musicales reciclables", "Libro, instrumentos musicales", 
  "Guitarra", "Intrumentos musicales,",
  "flauta, pandereta, guitarra, bocina", "flauta,guitarra,pandereta, triangulo,",
  "USB, internet, reproductor, computadora, instrumentos musicales", 
  "Piano portable, Guitarra, flauta, bocina, micrófono", 
  "Guitarra o libros", "Guitarra, flauta dulce, celular", "Guitarra, teclado, flautas", 
  "Guitarra. Trompeta flauta","Libros, teclado, guitarra", 
  "Flauta, guitarra, pandereta,triangulo,material didactico", "Flauta, guitarra, piano y saxofón", 
  "Flauta, guitarra, teclado libros de solfeo", "Flauta, internet, palitos, palanganas", 
  "Flauta, libros de texto, computadora, impresora, celular, hojas de papel bond.", 
  "Flauta, libros de textos, computadora, impresora, hojas de papel y marcadores.", 
  "Flauta, liras, redoblantes, bombos", "Flauta, manuales, folletos, audios.", 
  "Flauta, televisor, y otros", "Flauta,Guitarra,pandereta,tringulo", 
  "Flautas", "Libro de texto, flauta, reproductor de música, bocina.", 
  "Instrumento musical", "Instrumento musical, bocina, y otros",
  "RECURSO HUMANO, INSTRUMENTOS MUSICALES", 
  "instrumentos musicales", "instrumentos musicales, equipo de audio", 
  "intrumentos musicales, metodos de aprendizaje, bocinas, cañonera", 
  "Instrumentos", "Instrumentos elaborados, material audiovisual", 
  "Instrumentos musicales", "Instrumentos musicales, libros de texto, métodos de música", 
  "Instrumentos musicales, material reciclable para hacer instrumentos musicales para realizar sonidos", 
  "Instrumentos musicales, teléfono móvil, pc, libros de música, folletos, reproductor de música etc", 
  "Instrumentos y bocina", "Instrumentos y hojas de trabajo", "Instrumentos y materiales básico", 
  "Instrumentos, agrupaciones y lecciones", "Instrumentos, hojas de trabajo y otros", 
  "Instrumentos, libros.", "Libros, flauta", 
  "Teclado", "Teclado flauta guitarra.", 
  "Teclado, flauta, dispositivos digitales", "Teclado. Guitarra", 
  "Métodos de lectura, libros de texto, instrumentos musicales", 
  "Bocina, instrumentos autóctonos de material reciclado"
)

### Libros
valores_libros_musica2 <- c(
  "CNB, folleto elaborado de acuerdo a la malla curricular, libros de Ethel Batres, materiales guardados con anterioridad.", 
  "Cnb guatemala y libros digitales",
  "Computadora, libros, hojas de trabajo, celular.", 
  "Editorial educativa, Santillana, experiencia y otros", 
  "Enciclopedias","Folletos y libros","Internet, Santillana",
  "INTERNET Y OTROS LIBROS","Instrumentos, libros.",
  "LIBRO DE TEXTO","Las Tics y libros de música", 
  "Texto", "Textos e internet", "libro", "libro y folletos", "libros",
  "Textos, Instrumentos Musicales.",
  "Métodos de lectura, libros de texto, instrumentos musicales", 
  "Libro", "Libro , investigación en el internet", "Libro Utatlan de Formacion Musical", 
  "Libro de educación musical", "Libro de expresión artística y copias", 
  "Libro de texto", "Libro de texto, flauta, reproductor de música, bocina.", 
  "Libro de texto, impresiones, material didactico", "Libro folleto pdf", 
  "Libro y cuaderno Pedagógico", "Libro, Instrumentos", "Libro, bocina", 
  "Libro, bocina, instrumentos musicales reciclables", "Libro, instrumentos musicales", 
  "Libros", "Libros , audios", "Libros de formación musical, internet", 
  "Libros de formación musical, libros de cantos y otros", "Libros de guía se aprendizaje, computadora", 
  "Libros de lectura rítmica, material audiovisual", "Libros de texto", 
  "Libros de texto, flautas, partituras musicales, redes moviles", 
  "Libros digitales", "Libros e Internet", "Libros e impresiones", 
  "Libros e instrumentos", "Libros e internet", "Libros pedagógicos musicales, diapositivas, audios, documentos trabajados en sibelius y otros", 
  "Libros propios", "Libros propios, investigaciones, instrumentos musicales y libros de pedagogos musicales vistas en talleres de fladem Guat", 
  "Libros revistas cartón marcadores", "Libros texto", "Libros y CNB", 
  "Libros y audios", "Libros y tecnología", "Libros, Cnb investigsciones", 
  "Libros, computadora, reproductor de música", "Libros, flauta", 
  "Libros, hojas de trabajo", "Libros, marcadores, hojas de trabajo", 
  "Libros, marcadores, redes moviles", "Libros, teclado, guitarra", 
  "Instrumentos musicales, libros de texto, métodos de música", 
  "Flauta, libros de texto, computadora, impresora, celular, hojas de papel bond.", 
  "Flauta, libros de textos, computadora, impresora, hojas de papel y marcadores."
  
)

### material papeleria
valores_papeleria_musica2 <- c(
  "Copias", "Copias folleto",
  "Hojas de trabajo, material didáctica, reproductor de audio, útiles sonoros", 
  "Hojas de trabajo, útiles escolares como crayón, marcadores y temperaturas", 
  "Impresiones", "Libros revistas cartón marcadores",
  "Libros, hojas de trabajo", "Libros, marcadores, hojas de trabajo", 
  "Libros, marcadores, redes moviles", "fotocopias",
  "Marcador, almohadilla, pizzara, cartulina, videos.", 
  "Marcadores, hojas de papel, lapiceros, instrumentos, platea etc"
)

### mobiliario
valores_mobiliario_musica2 <- c(
  "Investigaciónes, Pizarron marcadores",
  "Marcador, almohadilla, pizzara, cartulina, videos.", 
  "Melodíca pizarra"
)

### ninguno
valores_ninguno_musica2 <- c(
  "NINGUNO", "Ninguno", "Ningún",
  "Desarrollo los temas investigando y leyendo",
  "Experiencia propia", "La voz el cuerpo instrumento de percusión"
  
)

### recursos digitales
valores_digitales_musica2 <- c(
  "Consultas en la web","Google","enlaces y textos", 
  "Internet", "Internet  e impresión", "recursos digitales", 
  "Internet, Santillana", "Internet, bocina", "Internet, conocimientos", 
  "Internet.","Medios digitales, instrumentos, bocina", 
  "Pág web", "Página w", "Sitios web", "Teléfono, internet y libros digitales",
  "Medios digitales", "Medios digitales, instrumentos, bocina", 
  "Investigacion  digital", "Pdf, libros descargados", 
  "Digitales", "Digitales e impresos", 
  "Recursos de la Web y herramientas virtuales", 
  "Discos con música acorde al contenido, internet en el dispositivo para utilización de pistas o canciones asignadas, transcripciones de notación musical adecuado a las melodías o canciones que se interpretan",
  "Investigaciones", "Investigaciones bocina celular internet", 
  "Investigaciones propias y el apoyo de un instructor por el mes de agosto y septiembre", 
  "Investigaciones y con el apoyo de un instructor, el cual se fortalece más en el mes de septiembre.", 
  "Investigación", "Investigación y bocina", "Investigaciónes de temas en la web y material didáctico", 
  "Investigaciónes, Pizarron marcadores"
)
### tecnologia
valores_tecnologia_musica2 <- c(
  "Audios, bocina, computadora, internet", 
  "BOCINA, FOLLETOS E INSTRUMENTOS MUSICALES", "Bocina", 
  "Bocina, computadora", "Bocina, computadora, celular con datos, instrumentos o útiles sonoros de percusiones, ukulele", 
  "Bocina, computadora, flauta", "Bocina, flauta", 
  "Bocina, hojas de actividades y flautas", 
  "Piano portable, Guitarra, flauta, bocina, micrófono", 
  "Bocina, instrumentos autóctonos de material reciclado", 
  "Bocina, teléfono celular, internet.",
  "Proyector, impresiones, flauta, bocina, videos", 
  "Pantalla, computadora", "Pc, impresiones", 
  "Folletos y hojas de trabajo y bocina", "Libros de guía se aprendizaje, computadora", 
  "Folletos, hojas de trabajo, bocina", "Libro, bocina", 
  "Investigaciones bocina celular internet", 
  "Libros y tecnología", "Micrófono,bocina, computadora, impresora.",
  "Libros, computadora, reproductor de música", 
  "USB, internet, reproductor, computadora, instrumentos musicales", 
  "Usb Internet  bocinas computadora","equipo de computo", 
  "Teléfono, internet", "Teléfono, internet y libros digitales", 
  "Investigación y bocina","Libro de texto, flauta, reproductor de música, bocina.", 
  "Instrumentos musicales, teléfono móvil, pc, libros de música, folletos, reproductor de música etc", 
  "Instrumentos y bocina","Internet, bocina","TELEFONO CELULAR",
  "Compu con inyernet", "Folletos hojas de trabajo  bocina",
  "instrumentos musicales, equipo de audio", 
  "intrumentos musicales, metodos de aprendizaje, bocinas, cañonera", 
  "Flauta, libros de texto, computadora, impresora, celular, hojas de papel bond.", 
  "Flauta, libros de textos, computadora, impresora, hojas de papel y marcadores.", 
  "Computadora", "Computadora cañonera, impresora, micrófono, bocina marcadores", 
  "Computadora e Internet", "Computadora e impresora", "Computadora usb", 
  "Computadora,  celular, guitarra", "Computadora, Internet", "Computadora, bocinas", 
  "Computadora, flauta, marcadores, almohadilla, lapicero.", "Computadora, impresoras", 
  "Computadora, libros, hojas de trabajo, celular.", "Computadora,USB, bocinas", 
  "Cañonera", "Celular, internet", "Celulares, bocinas , instrumentos propios", 
  "Bocina, teléfono, folletos.", "Bocinas", "Bocinas bluetooth,  telefono, Pc,  y otros", 
  "Bocinas, micrófono, flauta, karaokes", "Bosina. Computadora"
)

### no especifica

valores_noespecifica_musica4  <- c(
  "1", "Económicos", "Económicos fotocopias", 
  "Recursos propios",  "Si por mi cuenta", 
  "Teoria y practico", "Teoría y Práctica Musical (Innovaciones Educativas)", 
  "Teoría y práctica musical",
  "Todo", "Todos",  "solfa", "todos los necesarios"
)

## Recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    musica_recursos_propios = case_match(
      musica_recursos_propios,
      all_of(valores_noespecifica_musica4) ~ "No especifica el recurso",
      all_of(valores_tecnologia_musica2) ~ "Equipo tecnológico",
      all_of(valores_digitales_musica2) ~ "Recursos digitales",
      all_of(valores_ninguno_musica2) ~ "Ninguno",
      all_of(valores_mobiliario_musica2) ~ "Mobiliario",
      all_of(valores_papeleria_musica2) ~ "Material de papeleria",
      all_of(valores_libros_musica2) ~ "Libros",
      all_of(valores_instrumentos2) ~ "Instrumentos musicales",
      all_of(valores_guia_folleto_musica2) ~ "Guías y folletos",
      all_of(valores_cnb_musica2) ~ "CNB",
      all_of(valores_audio_didactico_musica2) ~ "Material audiovisual y didáctico",
      .default = musica_recursos_propios
    )
  )

## volver a ver errores

valores_musica_recursos_propios <- df_dyd |>
  filter(!(is.na(musica_recursos_propios))) |>
  select(musica_recursos_propios) |>
  tabyl(musica_recursos_propios)

# View(valores_musica_recursos_propios)

# musica_otro_ultima_vez_libros_mineduc ----

## ver errores
valores_musica_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(musica_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(musica_otro_ultima_vez_libros_mineduc ))) |>
  select(musica_otro_ultima_vez_libros_mineduc) |>
  tabyl(musica_otro_ultima_vez_libros_mineduc)

dput(valores_musica_otro_ultima_vez_libros_mineduc)


## Define los vectores ----

### 2014
valores_musica_2014 <- c(
  "2014"
)

### 2016 
valores_musica_2016 <- c(
  "2016"
)

### 2017
valores_musica_2017 <- c(
  "2017"
)

### 2018
valores_musica_2018 <- c(
  "2018"
)

### 2019
valores_musica_2019 <- c(
  "2019"
)

### 2020
valores_musica_2020 <- c(
  "2020"
)

### nunca
valores_nunca_musica <- c(
  "N/A", "Ninguno", "No", "No a recibido", "No recuerdo", "No se ha recibido material especifico sobre esta área", 
  "No se ha recibido ningún material de esta área", "Nunca", 
  "no he recibido", "nunca"
)

### no especifica cuando
valores_sinespecificar_musica <- c(
  "1",  "Libro digital", "Libro guia"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    musica_otro_ultima_vez_libros_mineduc = case_match(
      musica_otro_ultima_vez_libros_mineduc,
      all_of(valores_sinespecificar_musica) ~ "No especifica",
      all_of(valores_nunca_musica) ~ "No ha recibido",
      all_of(valores_musica_2020) ~ "2020",
      all_of(valores_musica_2019) ~ "2019",
      all_of(valores_musica_2018) ~ "2018",
      all_of(valores_musica_2017) ~ "2017",
      all_of(valores_musica_2016) ~ "2016",
      all_of(valores_musica_2014) ~ "2014",
      .default = musica_otro_ultima_vez_libros_mineduc
    )
    
  )

## volver a ver errores

valores_musica_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(musica_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(musica_otro_ultima_vez_libros_mineduc ))) |>
  select(musica_otro_ultima_vez_libros_mineduc) |>
  tabyl(musica_otro_ultima_vez_libros_mineduc)

# View(valores_musica_otro_ultima_vez_libros_mineduc)

# musica_tipo_materiales_recibidos ----

## ver errores

valores_musica_tipo_materiales_recibidos <- df_dyd |>
  filter(musica_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(musica_tipo_materiales_recibidos ))) |>
  select(musica_tipo_materiales_recibidos) |>
  tabyl(musica_tipo_materiales_recibidos)

dput(valores_musica_tipo_materiales_recibidos)


## Define los vectores ----

### guia de aprendizaj y planificador
valores_guia_planificador_musica <- c(
  "GUÍA DE APRENDIZAJE Y PLANIFICADOR", 
  "Guia y planificador", "Guias", "Guias y planificadores"
)

### libros
valores_libro_musica <- c(
  "Libro", 
  "Libro de textos de educación Artística", "Textos"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    musica_tipo_materiales_recibidos = case_match(
      musica_tipo_materiales_recibidos,
      all_of(valores_libro_musica) ~ "Libros",
      all_of(valores_guia_planificador_musica) ~ "Guía de aprendizaje y planificador",
      .default = musica_tipo_materiales_recibidos
    )
  )

## volver a ver errores
valores_musica_tipo_materiales_recibidos <- df_dyd |>
  filter(musica_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(musica_tipo_materiales_recibidos ))) |>
  select(musica_tipo_materiales_recibidos) |>
  tabyl(musica_tipo_materiales_recibidos)

# View(valores_musica_tipo_materiales_recibidos)

# visuales_libros_primero ----

## ver errores
valores_visuales_libros_primero <- df_dyd |>
  filter(visuales_uso_libros == "Sí") |>
  filter(visuales_grados == "Primer grado") |>
  filter(!(is.na(visuales_libros_primero))) |>
  select(visuales_libros_primero) |>
  tabyl(visuales_libros_primero)

dput(valores_visuales_libros_primero)

## Define los vectores ----

### artes plasticas
valores_artes_plasticas <- c(
  "Artes plásticas  Editora Educativa"
)

### educacion artistica
valores_edu_artistica_visual <- c(
  "Educación artística",  "Guia Educación Artistica"
)

### expresion artistica
valores_expre_artistica_visual <- c(
  "Expresión Artística", 
  "Expresión artística", "Expresión artística telesecundaria guia y planificador"
)

### guia de aprendizaje y planificador
valores_guia_plani_visual <- c(
  "GUÍA DE APRENDIZAJE Y PLANIFICADOR",    "Guia y planificador", "Guias y planificador", "Guias y planificadores", 
  "Guías de aprendizaje y Planificador docente", "Guías de aprendizaje y planificador docente", 
  "Guías y planificadores", 
  "Planifiador", 
  "Planificador del facilitador y Guia de Aprendizaje Expresión Artística 1"
)

### no especifica el nombre
valores_noespecifica_visual <- c(
  "Editora Educativa",    "Libro de texto",
  "Santillana", "Textos escolares"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    visuales_libros_primero = case_match(
      visuales_libros_primero,
      all_of(valores_noespecifica_visual) ~ "No especifica el nombre",
      all_of(valores_guia_plani_visual) ~ "Guía de aprendizaje y planificador",
      all_of(valores_expre_artistica_visual) ~ "Expresión Artística",
      all_of(valores_edu_artistica_visual) ~ "Educación Artística",
      all_of(valores_artes_plasticas) ~ "Artes Plástica",
      .default = visuales_libros_primero
     )
  )

## volver a ver errores

valores_visuales_libros_primero <- df_dyd |>
  filter(visuales_uso_libros == "Sí") |>
  filter(visuales_grados == "Primer grado") |>
  filter(!(is.na(visuales_libros_primero))) |>
  select(visuales_libros_primero) |>
  tabyl(visuales_libros_primero)

# View(valores_visuales_libros_primero)

# visuales_libros_segundo ----

valores_visuales_libros_segundo <- df_dyd |>
  filter(visuales_uso_libros == "Sí") |>
  filter(visuales_grados == "Segundo grado") |>
  filter(!(is.na(visuales_libros_segundo))) |>
  select(visuales_libros_segundo) |>
  tabyl(visuales_libros_segundo)

dput(valores_visuales_libros_segundo)

## Define los vectores ----

### educacion artistica

valores_edu_artistica_visuales <- c(
  "Educación Artística", 
  "Guía de Educación Artística"
)

### aprendizaje expresion artistica
valores_expre_artistica_visuales <- c(
  "Guia de aprendizaje expresión artística",
  "Guía de Expresión Artística y Planificador", 
  "Guía de aprendizaje y Planificador del docente"
)

### no especifica el nombre
valores_sinespecificar_visuales <- c(
  "Google", "Guias", 
  "Páginas web y material didactico",  "Texto"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    visuales_libros_segundo = case_match(
      visuales_libros_segundo,
      all_of(valores_sinespecificar_visuales) ~ "No especifica el nombre",
      all_of(valores_expre_artistica_visuales) ~ "Expresión Artística: Guía de aprendizaje y planificador",
      all_of(valores_edu_artistica_visuales) ~ "Educación Artística",
      .default = visuales_libros_segundo
    )
  )

## volver a ver errores

valores_visuales_libros_segundo <- df_dyd |>
  filter(visuales_uso_libros == "Sí") |>
  filter(visuales_grados == "Segundo grado") |>
  filter(!(is.na(visuales_libros_segundo))) |>
  select(visuales_libros_segundo) |>
  tabyl(visuales_libros_segundo)

# View(valores_visuales_libros_segundo)

# visuales_libros_tercero ----

## ver errores

valores_visuales_libros_tercero <- df_dyd |>
  filter(visuales_uso_libros == "Sí") |>
  filter(visuales_grados == "Tercer grado") |>
  filter(!(is.na(visuales_libros_tercero))) |>
  select(visuales_libros_tercero) |>
  tabyl(visuales_libros_tercero)

dput(valores_visuales_libros_tercero)

## Define los vectores ----

### guia de aprendizaje
valores_guia_aprendizaje_visuales <- c(
  "Guía de aprendizaje", 
  "Guía de aprendizaje de expresión artística", "Guía y planificador",
  "Planificador docente y guías de aprendizajes"
)

### planificador
valores_planificador_visuales <- c(
  "Guía y planificador", 
  "Planificador", "Planificador docente y guías de aprendizajes"
  
)

### educacion musical integrada
valores_edu_musical_visuales <- c(
  "Libro de Educación Musical integrada"
)

### no especifica
valores_sinespecificar_visuales2 <- c(
  "Libros digitales", "Paginas web  y otros"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    visuales_libros_tercero = case_match(
      visuales_libros_tercero,
      all_of(valores_sinespecificar_visuales2) ~ "No especifica el nombre",
      all_of(valores_edu_musical_visuales) ~ "Educación Musical Integrada",
      all_of(valores_planificador_visuales) ~ "Planificador para el facilitador",
      all_of(valores_guia_aprendizaje_visuales) ~ "Guía de aprendizaje",
      .default = visuales_libros_tercero
    )
  )


## volver a ver errores

valores_visuales_libros_tercero <- df_dyd |>
  filter(visuales_uso_libros == "Sí") |>
  filter(visuales_grados == "Tercer grado") |>
  filter(!(is.na(visuales_libros_tercero))) |>
  select(visuales_libros_tercero) |>
  tabyl(visuales_libros_tercero)

# View(valores_visuales_libros_tercero)

# visuales_recursos_centro_educativo ----

## ver errores
valores_visuales_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(visuales_recursos_centro_educativo))) |>
  select(visuales_recursos_centro_educativo) |>
  tabyl(visuales_recursos_centro_educativo)

dput(valores_visuales_recursos_centro_educativo)

## Define los vectores ----

### artisticos
valores_artisticos <- c(
  "Elementos de Pintura", "Hojas 120 gramos, algunas pinturas y pinceles.", 
  "Hojas de tareas pintura", "Hojas doble oficio, oficio, crayones, marcadores, témperas, pinceles reglas T, todos los tipos de papel que se requiera según las actividades, materiales para trabajos manuales.", 
  "Lienzos, dibujos.","Lápices, papeles, pintura",
  "Marcadores, papel , temperas", 
  "Marcadores, pinturas, hojas etc.",
  "Pintura, brochas, maskintape, utiles escolares", "Pinturas", 
  "Pinturas, pinceles, caballetes","Temperas. Pinceles.",
  "Medios audiovisuales, material para la didáctica, marcadores, hojas, pintura, plasticina, pegamento"
)

### audiovisual y didáctico
valores_audio_didactico_visuales <- c(
  "Audiovisuales","Didácticos","Fichas impresas.", 
  "Guía de aprendizaje, planificador docente, valija didáctica", 
  "Libro de texto. Material didáctico",
  "Virtuales", "Visuales e impresos", "audivisuales, sonido, espacio físico, entre otros...", 
  "Libros, videos, imágenes, navegador", 
  "Libros,revistas e impresiones","Videos, libros", 
  "Valija didáctica", "Valija didáctica, papel, marcadores, lapiceros, tinta, almohadilla, etc.", 
  "Material impreso", "Materiales de apoyo", "Materiales del internet", 
  "Materiales para arte","Tecnológico y pedagógico",
  "Proyector, equipo de sonido, juego geometrico y material didactico",
  "Medios audiovisuales, material para la didáctica, marcadores, hojas, pintura, plasticina, pegamento", 
  "Material didactico", "Material didáctico", "Material didáctico como formatos, hojas entre otros", 
  "Material didáctico como formatos, hojas, entre otros"
)

### cnb
valores_cnb_visuales <- c(
  "CNB", "CNB PLANIFICADOR GUÍAS DE APRENDIZAJE", 
  "CNB y libros","Cnb", "Investigo y creo guía segun cnb"
  
)

### espacio educativo
valores_espacio_visuales <- c(
  "Aula específica para impartir el curso", 
  "Aula pura , pizarrón .", "Aula, Pizarrón, Cañonera y Marcadores", 
  "Aula, regla y escuadras", "Espacios, tecnología", 
  "Salón de clases especial para Artes Visuales", 
  "audivisuales, sonido, espacio físico, entre otros...", 
  "Salón especial para el desarrollo del curso, caballetes", "Salón especial para impartir el curso"
)

### guia y folleto
valores_guia_folleto_visuales <- c(
  "Folletos", 
  "GUIA DE APRENDIZAJE", "GUÍA DE APRENDIZAJE Y PLANIFICADOR", 
  "Guia", "Guia de aprendizaje expresión artística", "Guias", 
  "Guias y diario de clase", "Guias y folletos", "Guias y planificador", 
  "Guias y planificadores", "Guía", "Guía de aprendizaje", "Guía de aprendizaje, planificador docente, valija didáctica", 
  "Guía de educación artística", "Guía y planificador", "Guías", 
  "Guías de aprendizaje", "Guías de aprendizaje y Planificador docente", 
  "Guías de aprendizajes", "Guías y planificadores", "Guías y planificadores."
  
  
)

### instrumentos de dibujo
valores_dibujo <- c(
  "Aula, regla y escuadras","Hojas de papel, televisores, compás y regla.", 
  "JUEGO DE GEOMETRÍA, REGLA T",
  "Pizarra reglas escuadras mesas", 
  "Proyector, equipo de sonido, juego geometrico y material didactico",
  "Marcadores de pizarra, juego geometrico", 
  "Regla, compás, transportador", 
  "Reglas, marcadores", 
  "Marcadores, pliegos de papel, crayones, reglas, hojas", 
  "Libro de texto, juego de geometría. Técnicas de pintura."
)


### libros
valores_libros_visuales <- c(
  "1 libro de artes plásticas o visuales", "1 libro sobre las artes plásticas o visuales",
  "CNB y libros","Cañoneras, libros", "Editora Educativa",
  "libro , pizarra marcadores,reglas,compas plantillas block de artes plasticas", 
  "Enciclopefdas","Impresora ,libro", "Pantalla,  libros de texto", 
  "Expresión artística, Artes Plásticas 1°, 2°, 3°","Videos, libros"
)

### mobiliario
valores_mobiliario_visuales <- c(
  "Aula pura , pizarrón .", "Aula, Pizarrón, Cañonera y Marcadores", 
  "Impresiones, libro, pizarra, recurso humano", 
  "Impresiones, marcadores, pizarra", "Solo pizarrón",
  "Pizarra", "Pizarra reglas escuadras mesas", "Texto", 
  "Pizarra,", "Pizarra, computadora, cañonera etc.", "Pizarra, marcadores, almohadillas, copias, impresiones, tinta para marcadores", 
  "Pizarrón", "Pizarrón de Formica, marcadores, hojas diferentes tamaños, pliegos papel bond etc", 
  "Marcador pizarra","Marcadores de pizarra, juego geometrico", 
  "Libro", "Libro de Expresión Artística de IGER", 
  "Libro de texto", "Libro de texto, juego de geometría. Técnicas de pintura.", 
  "Libro de texto. Material didáctico", "Libro folleto", "Libro guia", 
  "Libro virtual, computadoras, internet, plataformas virtuales", 
  "Libro y planificador.", "Libros", "Libros , smart tv, bocina", 
  "Libros Santillana", "Libros de artes plásticas", "Libros de texto", 
  "Libros de texto, manuales", "Libros de textos", "Libros entre otros", 
  "Libros y computadora", "Libros, Smart tv", "Libros, impresora, bocina", 
  "Libros, papel, plastilina, reglas, marcadores etc.", "Libros, videos, imágenes, navegador", 
  "Libros,revistas e impresiones", "Lubros",
  "Los libros de artes plásticas en los niveles existentes"
)

### ninguno
valores_ninguno_visuales <- c(
  "N/A", "Nada", "Ninguno", "Ningún", "Ningúno", "No", "No existe", 
  "No hay, Todo es el esfuerzo de uno, investigación  constante  el Internet", 
  "No, trabajo en una cooperativa","0", 
  "Conocimientos propios", "Consultoría a artistas visuales", 
  "Experiencia propia", 
  "ninguno", "perspectiva", 
  "Para el curso de formación musical ningún material o recurso"
)

### no especifica
valores_noespecifica_visuales <- c(
  "Poco","1","Recursos básicos", "Recursos de actividades prácticas",
  "Todo", "virtuales","Material", "Material de apoyo"
)

### papeleria
valores_papeleria_visuales <- c(
  "Computadora, impresora, hojas, marcadores", 
  "Copias", "Copias folletos", "Copias, hojas, libros", "Copias, impresiones", 
  "Fotocopias", "Papel y lápiz", "Papel, marcadores y enseres de oficina.", 
  "Hojas", "Hojas  e impresiones", "Hojas 120 gramos, algunas pinturas y pinceles.", 
  "Hojas de papel Bond para realizar formatos", "Hojas de papel, televisores, compás y regla.", 
  "Hojas de tareas pintura", "Hojas doble oficio, oficio, crayones, marcadores, témperas, pinceles reglas T, todos los tipos de papel que se requiera según las actividades, materiales para trabajos manuales.", 
  "Hojas, papel,", "Hojas, pintura, lápiz, crayones.", "Impresiones", 
  "Impresiones fotocopias", "Impresiones, libro, pizarra, recurso humano", 
  "Impresiones, marcadores, pizarra", "Impresiones, marcadores, proyector", 
  "Impresora ,libro", "Impresora, cañonera y pizarra", "Impresora, hojas de papel, tijeras.", 
  "Maradores temas acuerdo al grado", 
  "Valija didáctica, papel, marcadores, lapiceros, tinta, almohadilla, etc.",
  "Proyector, marcadores, impresiones.",
  "libro , pizarra marcadores,reglas,compas plantillas block de artes plasticas", 
  "Pizarrón de Formica, marcadores, hojas diferentes tamaños, pliegos papel bond etc", 
  "Pizarra, marcadores, almohadillas, copias, impresiones, tinta para marcadores", 
  "Marcador pizarra", "Marcadores", "Marcadores de pizarra, juego geometrico", 
  "Marcadores formatos", "Marcadores y cartulinas", "Marcadores y cartulinas.", 
  "Marcadores, hojas, lápiz, lapiceros", "Marcadores, papel , temperas", 
  "Marcadores, pinturas, hojas etc.", "Marcadores, pliegos de papel, crayones, reglas, hojas"
)

### Planificador
valores_planificador_visuales2 <- c(
  "Planificador", "Planificador de Educación Artística", "Planificador del facilitador y guía de aprendizaje Telesecundaria", 
  "Planificador y guia", "Planificador y guías"
)

### recursos digitales
valores_digitales_visuales <- c(
  "Aplicación","Digitales","Enlaces", 
  "Libro virtual, computadoras, internet, plataformas virtuales", 
  "Material digital", "Paginas web", "Pdf",
  "Plataforma", "Páginas web", "Material de apoyo y guías virtuales"
  
)

### tecnologia
valores_tecnologia_visuales <- c(
  "Aula, Pizarrón, Cañonera y Marcadores", 
  "Cañonera", "Cañonera, computadora", "Cañonera, tv", 
  "Internet", "Internet, articulos, etc", 
  "Cañoneras, compus etc", "Cañoneras, libros", "Centro de computación", 
  "Computadora", "Computadora matricidio Internet", "Computadora, cañonera", 
  "Computadora, cañonera, folletos", "Computadora, impresora, hojas, marcadores", 
  "Espacios, tecnología", "Impresiones, marcadores, proyector", 
  "Impresora, cañonera y pizarra", "Impresora, hojas de papel, tijeras.", 
  "Libro virtual, computadoras, internet, plataformas virtuales", 
  "Libros , smart tv, bocina", 
  "Pantalla,  libros de texto", 
  "Tecnológico y pedagógico", "Televisores, cañonera y hojas de papel.", 
  "Televisores, computadora, sonido", 
  "Pantalla, Impresora","RED WIFI, EQUIPO AUDIO VISUAL, COPIAS.",
  "Proyector", "Proyector, equipo de sonido, juego geometrico y material didactico", 
  "Proyector, marcadores, impresiones.",
  "Pizarra, computadora, cañonera etc.", 
  "Libros y computadora", "Libros, Smart tv", "Libros, impresora, bocina"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    visuales_recursos_centro_educativo = case_match(
      visuales_recursos_centro_educativo,
      all_of(valores_tecnologia_visuales) ~ "Equipo tecnológico",
      all_of(valores_digitales_visuales) ~ "Recursos digitales",
      all_of(valores_planificador_visuales2) ~ "Planificador",
      all_of(valores_papeleria_visuales) ~ "Material de papeleria",
      all_of(valores_noespecifica_visuales) ~ "No especifica el recurso",
      all_of(valores_ninguno_visuales) ~ "Ninguno",
      all_of(valores_mobiliario_visuales) ~ "Mobiliario",
      all_of(valores_libros_visuales) ~ "Libros",
      all_of(valores_dibujo) ~ "Materiales de dibujo",
      all_of(valores_guia_folleto_visuales) ~ "Guías y folletos",
      all_of(valores_espacio_visuales) ~ "Espacio educativo",
      all_of(valores_cnb_visuales) ~ "CNB",
      all_of(valores_audio_didactico_visuales) ~ "Material audiovisual y didáctico",
      all_of(valores_artisticos) ~ "Recursos artísticos",
      .default = visuales_recursos_centro_educativo
    )
  )

## volver a ver errores

valores_visuales_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(visuales_recursos_centro_educativo))) |>
  select(visuales_recursos_centro_educativo) |>
  tabyl(visuales_recursos_centro_educativo)

# View(valores_visuales_recursos_centro_educativo)

#  visuales_recursos_propios ----

## ver errores

valores_visuales_recursos_propios <- df_dyd |>
  filter(!(is.na(visuales_recursos_propios))) |>
  select(visuales_recursos_propios) |>
  tabyl(visuales_recursos_propios)

dput(valores_visuales_recursos_propios)

## Define los vectores ----

### artisticos
valores_artisticos2 <- c(
  "Acuarelas, crayones pastel, marcadores, impresiones, computadora.", 
  "Acuarelas, temperas, pinceles, lápices, hojas.",
  "COMPUTADORA, REPRODUCTOR DE MÚSICA, USB, MATERIALES IMPRESOS, RECURSOS EN LÍNEA, AGENDA DE TRABAJO, HOJAS DE TRABAJO, COPIAS, REVISTAS, PERIÓDICOS, LIBROS DIGITALES, PINTURAS SECAS, LÍQUIDA, SELLADOR DE MADERA, METRO, MADERA, TORNILOS, MOLDES, PAPEL DE COLES DE VARIADO, SILICÓN FRÍO Y BARRA, PISTOLA DE SILICÓN, PLANTILLAS, BOCETOS, MOLDES, ETC.",
  "Computadora, impresora, temperas, libros de textos y otros relacionado a las artes visuales.", 
  "Crayones temperas marcadores libros formatos", 
  "Hojas lino, cartulinas, acuerela, juegos de geometria", 
  "Elementos de Pintura", "Equipo audio visual , folleto.", 
  "Impresiones, foamy moldeable, arcilla para moldear,", 
  "Instrumentos y material de dibujo (pinturas, pinceles, etc)",
  "Libros, pinturas, pinceles, hojas bond", 
  "Reutilizables, tecnológico y artístico", 
  "Textos, marcadores, pintura", "Textos, pinturas, pinceles, marcadores, lápices.", 
  "Lienzos y dibujos", "Lienzos, pinceles, marcadores y lapices", 
  "Lienzos, plastilina, pinceles,  lápices, acuarelas",
  "Libros. Pinturas. Cañonera bocina hojas etc", 
  "Pedagógico, impresiones, pinturas", "Pinceles, acrílico, reglas,", 
  "Pinceles, pintura, reglas, marcadores, material impreso entre otros", 
  "Pintura, marcadores, crayones, hojas,", "Pinturas secas, húmedas y mojadas, crayones, reglas,papeles, tutoriales", 
  "Pinturas secas, húmedas y mojadas, reglas,", "Pinturas secas, húmedas, mojadas, crayones, reglas, etc.", 
  "Pinturas y lienzos", "Pinturas, lápices, crayones, impresiones, pinceles, libros de textos", 
  "Pinturas, pinceles, material de dibujo y pintura, lápiz", 
  "Pinturas, reglas, dibujos","Trabajar dibujo y pintura con temperas", 
  "Temperas, acuarelas, hilos tela", "Temperas, reglas, formatos, pinceles, computadora, impresora, libros de textos.", 
  "Marcadores, pinturas, herramientas e instrumentos de dibujo", 
  "Marcadores, témperas, reglas, acuarelas, hojas Bond, formatos, etc", 
  "Lápices crayones papel", "Lápiz pinturas crayones juegos geométricos", 
  "Juegos de geometría, pinceles, pinceles, diversidad de pintura,  cartón, cartulina,  caballetes , otros", 
  "Crayones. Témperas.crayones .pinturas acrílicas. Juego de geometría.", 
  "Cuadros, exposiciones, videos, música, pintura, pinceles, talleres," 
)

### audiovisual
valores_audio_didactico_visuales2 <- c(
  "Audios, videos e imágenes.", "Audiovisual.", "Audiovisuales", 
  "Carteles, impresiones",  "Dibujos, internet, carteles",
  "Computadora, material didáctico, videos y presentaciones artísticas.", 
  "Computadora,videos revisitas", "Libro de texto, material didactico",
  "Investigaciones de los temas y material didáctico",  
  "Investigaciones, documentos pdf y diapositivas",
  "Internet, laptop, material didáctico.",
  "Libros, folletos, imágenes e impresiones", 
  "Libros, hojas de trabajo", "Libros, hojas,tecnología", 
  "Libros, impresiones, dibujos", 
  "Técnicas de pintura. Instrumentos de trazo", 
  "Témperas, hojas",
  "Videos",   "presentaciones de power point", 
  "recursos audiovisuales, sonido, espacio físico, entre otros...", 
  "Videos folletos audios etc", "Videos,  investigación entre otros", 
  "Videos, libros", "Videos. Materiales impresos",
  "Pedagógico, impresiones, pinturas", 
  "Reutilizables, tecnológico y artístico", 
  "Libros, revistas",  "Paginas web, material didactico.", 
  "Material", "Material Didáctico Variado", "Material audio visual", 
  "Material de apoyo", "Material de apoyo y guías virtuales", 
  "Material didactico,páginas web", "Material didáctico", 
  "Material didáctico folletos de ejercicio de Google", "Material didáctico, artes, juegos visuales", 
  "Material escrito y carteles", "Material impreso", "Material oersonal", 
  "Material reciclable para hacer sonidos, instrumentos musicales, material con ilustraciones", 
  "Materiales Basicos", "Materiales Básicos", "Materiales didácticos", 
  "Materiales impresos", "Recursos de actividades prácticas",  
  "Marcador, almohadilla, pizzara, cartulina, videos.", 
  "Libros, revistas e impresiones", 
  "Libros de texto, pinceles, pinturas, lienzos", 
  "Hojas de trabajo, guias de aprestamiento , y videos", "Hojas de trabajo, recursos didácticos", 
  "Folletos, videos, imágenes.", "Formatos, cámara , fotos", 
  "Diapositivas, impresiones, investigaciones y otros recursos extras", 
  "Cuadros, exposiciones, videos, música, pintura, pinceles, talleres," 
)

### cnb
valores_cnb_visuales2 <- c(
  "Cnb", "Cnb de Guatemala y libros digitales",
  "Libros y CNB", "Libros, CNB", 
  "Libros, Cnb investigsciones de Internet"
  
)

### guias y folleto
valores_guia_folleto_visuales2 <- c(
  "Equipo audio visual , folleto.", 
  "Folleto", "Folletos",   "Videos folletos audios etc",
  "Folletos Internet", "Folletos y hojas de trabajo", "Folletos y hojas de trabajo.", 
  "Folletos y pdf's", "Folletos, hojas de trabajo", "Folletos, internet", 
  "Folletos, videos, imágenes.", 
  "Malla curricular",  "Textos....guias", 
  "Marcadores  folletos, Internet  algunos impresos, y la planificación, cuaderno de asistencia o registro", 
  "Libros, folletos, imágenes e impresiones", 
  "Guia de aprendizaje", "Guia y planificador", "Guias", "Guias y folletos", 
  "Guía de artes visuales", "Guías", "Guías de aprendizaje", 
  "Guías de aprendizajes", "Guías y planificadores"
)

### instrumentos dibujo
valores_dibujo_2 <- c(
  "Compás, libros", "Crayones. Témperas.crayones .pinturas acrílicas. Juego de geometría.",
  "Hojas lino, cartulinas, acuerela, juegos de geometria", 
  "Instrumentos de medición temperas", "Instrumentos técnicos y visuales.", 
  "Instrumentos técnicos, otros.", "Instrumentos técnicos.", 
  "Instrumentos y material de dibujo (pinturas, pinceles, etc)", 
  "Juegos de geometría, pinceles, pinceles, diversidad de pintura,  cartón, cartulina,  caballetes , otros", 
  "Libro de texto. Instrumentos de trazo",
  "Pinceles, acrílico, reglas,", 
  "reglas", "reglas, marcadores, libros folletos", 
  "Tablero, reglas, lápices, lapiceros, marcadores.",
  "Reglas propias", "Reglas, lápiz, crayones, compás, etc",
  "Pinturas secas, húmedas y mojadas, reglas,", "Pinturas, reglas, dibujos", 
  "Pinceles, pintura, reglas, marcadores, material impreso entre otros", 
  "Marcadores, pinturas, herramientas e instrumentos de dibujo", 
  "Libros, juego de geometría, compas", 
  "Lápiz pinturas crayones juegos geométricos", 
  "Libros de texto computadoras, material de dibujo",
  "Libros, compás, compu"
  
)

### libros
valores_libros_visuales2 <- c(
  "Artes Visuales edición Santillana", 
  "Computadora, impresora, temperas, libros de textos y otros relacionado a las artes visuales.",
  "Computadora, libros, hojas de trabajo, celular.", "Computadora, libros, internet", 
  "Computadora, libros.", "Santilla y otros", 
  "Santillana, internet,",
  "Temperas, reglas, formatos, pinceles, computadora, impresora, libros de textos.", 
  "Computadoras y libros", "Compás, libros",
  "Crayones temperas marcadores libros formatos", 
  "Las Tics y libros de artes mayas", 
  "Libro", "Libro de expresión artística y copias", "Libro de texto", 
  "Libro de texto, material didactico", "Libro de texto. Instrumentos de trazo", 
  "Libro de textos e impresiones", "Libro guía", "Libro propios", 
  "Libro y Folletos", "Libro y recursos como folletos que yo elaboro", 
  "Libro, hojas de trabajo", "Libros", "Libros de Artes Plásticas", 
  "Libros de música y consulta del internet", "Libros de texto", 
  "Libros de texto computadoras, material de dibujo", "Libros de texto, pinceles, pinturas, lienzos", 
  "Libros de texto,computadora,cel, internet", "Libros digitales", 
  "Libros e Internet", "Libros e internet", "Libros en PDF", 
  "Libros en digital y en físico", "Libros en pdf", "Libros entre otros.", 
  "Libros impresos, Tecnología, entre otros.", "Libros propios", 
  "Libros propios santillana", "Libros y CNB", "Libros, CNB", 
  "Libros, Cnb investigsciones de Internet", "Libros, Internet, impresisiones", 
  "Libros, compás, compu", "Libros, folletos, imágenes e impresiones", 
  "Libros, hojas de trabajo", "Libros, hojas,tecnología", 
  "Libros, impresiones, dibujos", "Libros, juego de geometría, compas", 
  "Libros, pinturas, pinceles, hojas bond", "Libros, revistas", 
  "Libros y CNB", "Libros, CNB",  "Los textos", 
  "Planificador","Texto", "Texto personal", "Texto y folleto", 
  "Textos, marcadores, pintura", "Textos, pinturas, pinceles, marcadores, lápices.", 
  "Textos....guias","enlaces y textos", 
  "libro y folletos", "libro, guia y folleto", "libros", "libros y materiales de referencia.",  "Videos, libros",
  "Marcadores, libros","Pinturas, lápices, crayones, impresiones, pinceles, libros de textos", 
  "Libros, Cnb investigsciones de Internet", "Libros, Internet, impresisiones", 
  "Libros, compás, compu", "Libros, folletos, imágenes e impresiones", 
  "Libros, hojas de trabajo", "Libros, hojas,tecnología", 
  "Libros, impresiones, dibujos", "Libros, juego de geometría, compas", 
  "Libros, pinturas, pinceles, hojas bond", "Libros, revistas", 
  "Libros, revistas e impresiones", "Libros. Pinturas. Cañonera bocina hojas etc", 
  "Libros, revistas e impresiones", "Libros. Pinturas. Cañonera bocina hojas etc", 
  "Hojas, libros", "INTERNET Y OTROS LIBROS", "Internet, libros"
)

### mobiliario
valores_mobiliario_visuales2 <- c(
  "Investigaciónes, Pizarron y marcadores",
  "Pizarra, computadora, cañonera",  "yeso pizarra",
  "Marcador, almohadilla, pizzara, cartulina, videos."
)

### ninguno
valores_ninguno_visuales2 <- c(
  "Motricidad y alumnos",   "Talento,", 
  "Ninguna", "Ninguno", "Ningún"
  
)

### no especifica
valores_noespecifica_visuales2 <- c(
  "Económicos", "Económicos fotocopias",
  "Soy arte",  "Teoria y practico", "Todos", 
  "todos los necesarios", 
  "Propios", "Recursos básicos",  "Siempre llevo materiales", 
  "Experiencia propia", "Mayoría", "Muchos"
)

### papeleria
valores_papeleria_visuales2 <- c(
  "Acuarelas, crayones pastel, marcadores, impresiones, computadora.", 
  "Acuarelas, temperas, pinceles, lápices, hojas.",
  "COMPUTADORA, REPRODUCTOR DE MÚSICA, USB, MATERIALES IMPRESOS, RECURSOS EN LÍNEA, AGENDA DE TRABAJO, HOJAS DE TRABAJO, COPIAS, REVISTAS, PERIÓDICOS, LIBROS DIGITALES, PINTURAS SECAS, LÍQUIDA, SELLADOR DE MADERA, METRO, MADERA, TORNILOS, MOLDES, PAPEL DE COLES DE VARIADO, SILICÓN FRÍO Y BARRA, PISTOLA DE SILICÓN, PLANTILLAS, BOCETOS, MOLDES, ETC.",
  "Copias", "Crayones temperas marcadores libros formatos", 
  "Crayones. Témperas.crayones .pinturas acrílicas. Juego de geometría.", 
  "Cuaderno de trabajo. Bocina. Microfono. Celular internet", 
  "Cuaderno, crayón, hojas, computadora e impresora", "Cuadros, exposiciones, videos, música, pintura, pinceles, talleres,", 
  "Folletos y hojas de trabajo", "Folletos y hojas de trabajo.",
  "Folletos, hojas de trabajo", "VOZ, MARCADORES, RÓTULOS", 
  "Pinturas, lápices, crayones, impresiones, pinceles, libros de textos", 
  "Pinturas, pinceles, material de dibujo y pintura, lápiz", 
  "Marcador, almohadilla, pizzara, cartulina, videos.", 
  "Marcadores  folletos, Internet  algunos impresos, y la planificación, cuaderno de asistencia o registro", 
  "Marcadores, investigación", "Marcadores, libros", "Marcadores, pinturas, herramientas e instrumentos de dibujo", 
  "Marcadores, témperas, reglas, acuarelas, hojas Bond, formatos, etc", 
  "Hojas",   "Internet, marcadores", 
  "Tablero, reglas, lápices, lapiceros, marcadores.",
  "Silicón, internet, material para actividades", 
  "Pintura, marcadores, crayones, hojas,", "Pinturas secas, húmedas y mojadas, crayones, reglas,papeles, tutoriales", 
  "Impresiones",   "Internet, reglas para pizarra", 
  "Impresiones fotocopias", "Impresiones, foamy moldeable, arcilla para moldear,", 
  "Hojas de actividades", "Hojas de las investigaciones", "Hojas de tareas", 
  "Hojas de trabajo, guias de aprestamiento , y videos", "Hojas de trabajo, recursos didácticos", 
  "Hojas de trabajo.", "Hojas lino, cartulinas, acuerela, juegos de geometria", 
  "Hojas, libros"
)

### Recursos digitales
valores_digitales_visuales2 <- c(
  "Dgitales",   "Digitales","Folletos y pdf's", 
  "Información de internet",  "Paginas web, material didactico.", 
  "Investigaciones", "Investigaciones de los temas y material didáctico", 
  "Investigaciones propios sobre las diferentes técnicas, y contextualizarlo según los recursos disponibles en la comunidad", 
  "Investigaciones sobre diferentes técnicas, y materiales a utilizar en artes plásticas", 
  "Investigaciones, documentos pdf y diapositivas", "Investigación", 
  "Libros digitales",   "Medios digitales",     "Pág web",
  "Pdf, libros descargados y páginas web", 
  "Sitios web",  "Trabajos de internet", 
  "Web", "enlaces y textos", 
  "Recursos de la Web y herramientas virtuales", 
  "Investigaciónes, Pizarron y marcadores", "Investigo, leo y practico"
  
)

### tecnologia
valores_tecnologia_visuales2 <- c(
  "Acuarelas, crayones pastel, marcadores, impresiones, computadora.", 
  "Bocina", "COMPUTADORA, REPRODUCTOR DE MÚSICA, USB, MATERIALES IMPRESOS, RECURSOS EN LÍNEA, AGENDA DE TRABAJO, HOJAS DE TRABAJO, COPIAS, REVISTAS, PERIÓDICOS, LIBROS DIGITALES, PINTURAS SECAS, LÍQUIDA, SELLADOR DE MADERA, METRO, MADERA, TORNILOS, MOLDES, PAPEL DE COLES DE VARIADO, SILICÓN FRÍO Y BARRA, PISTOLA DE SILICÓN, PLANTILLAS, BOCETOS, MOLDES, ETC.", 
  "Cañonera", "Cañoneras, computoadoras", 
  "Compu con internet",   "Proyector",
  "Computadora", "Computadora cañonera marcadores  impresora", 
  "Computadora e Internet", "Computadora e impresora", "Computadora internet", 
  "Computadora y teléfono.", "Computadora, Bocina, micrófono, cañonera", 
  "Computadora, Internet", "Computadora, bocinas", "Computadora, cañonera, bocina, micrófono", 
  "Computadora, hojas", "Computadora, impresora, temperas, libros de textos y otros relacionado a las artes visuales.", 
  "Computadora, impresoras", "Computadora, internet copias audios", 
  "Computadora, libros, hojas de trabajo, celular.", "Computadora, libros, internet", 
  "Computadora, libros.", "Computadora, material didáctico, videos y presentaciones artísticas.", 
  "Computadora,cañonera", "Computadora,videos revisitas", 
  "Computadoras y libros", "Equipo de computo",
  "Cuaderno de trabajo. Bocina. Microfono. Celular internet", 
  "Cuaderno, crayón, hojas, computadora e impresora", "Cuadros, exposiciones, videos, música, pintura, pinceles, talleres,", 
  "Imágenes, computadora, teléfono",
  "Pizarra, computadora, cañonera", 
  "TELEFONO CELULAR",  "Tv",  "metodos, técnicas, cañonera, compu", 
  "Temperas, reglas, formatos, pinceles, computadora, impresora, libros de textos.", 
  "Teléfono internet y libros digitales", "Teléfono, internet", 
  "Reutilizables, tecnológico y artístico", 
  "Libros, compás, compu",  "Libros, hojas,tecnología", 
  "Libros impresos, Tecnología, entre otros.",
  "Libros de texto,computadora,cel, internet", 
  "Pantalla, computadora", "Pc, impresiones",
  "Libros. Pinturas. Cañonera bocina hojas etc", 
  "LAPTOP E INTRNET", "Laptop", "Laptop e internet",
  "Internet", "Internet, computadora, etc", "Internet, etc", 
  "Internet, laptop, material didáctico.", "Internet, libros", 
  "Internet, marcadores", "Internet, reglas para pizarra"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    visuales_recursos_propios = case_match(
      visuales_recursos_propios,
      all_of(valores_tecnologia_visuales2) ~ "Equipo tecnológico",
      all_of(valores_digitales_visuales2) ~ "Recursos digitales",
      all_of(valores_papeleria_visuales2) ~ "Material de papelería",
      all_of(valores_noespecifica_visuales2) ~ "No especifica el recurso",
      all_of(valores_ninguno_visuales2) ~ "Ninguno",
      all_of(valores_mobiliario_visuales2) ~ "Mobiliario",
      all_of(valores_libros_visuales2) ~ "Libros",
      all_of(valores_dibujo_2) ~ "Instrumentos de dibujo",
      all_of(valores_guia_folleto_visuales2) ~ "Guías y folletos",
      all_of(valores_cnb_visuales2) ~ "CNB",
      all_of(valores_audio_didactico_visuales2) ~ "Material audiovisual y didáctico",
      all_of(valores_artisticos2) ~ "Materiales artísticos",
      .default = visuales_recursos_propios
    )
  )

## volver a ver errores

valores_visuales_recursos_propios <- df_dyd |>
  filter(!(is.na(visuales_recursos_propios))) |>
  select(visuales_recursos_propios) |>
  tabyl(visuales_recursos_propios)

# View(valores_visuales_recursos_propios)

# visuales_otro_ultima_vez_libros_mineduc ----

## ver errores 

valores_visuales_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(visuales_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(visuales_otro_ultima_vez_libros_mineduc ))) |>
  select(visuales_otro_ultima_vez_libros_mineduc) |>
  tabyl(visuales_otro_ultima_vez_libros_mineduc)

dput(valores_visuales_otro_ultima_vez_libros_mineduc)

## Define los vectores ----

### 2014
valores_visuales_2014 <- c(
  "2014"
)

### 2016
valores_visuales_2016 <- c(
  "2016"
)

### 2017
valores_visuales_2017 <- c(
  "2017"
)

### 2018
valores_visuales_2018 <- c(
  "2018", "Libro guias de aprendizaje 2015 al2018"
)

### 2019
valores_visuales_2019 <- c(
  "2019"
)

### 2020
valores_visuales_2020 <- c(
  "2020"
)

### nunca
valores_visuales_nunca <- c(
  "N/A", "Ninguno", "Ningún", 
  "No", "No he recibido", "No hemos recibido",
  "No se ha recibido material sobre esta área", 
  "No se ha recibido ningún material de esta área", "Nunca", 
  "nunca"
)

### no especifica
valores_noespecifica_visuales3 <- c(
  "Expresión Artística", 
  "No recuerdo"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    visuales_otro_ultima_vez_libros_mineduc = case_match(
      visuales_otro_ultima_vez_libros_mineduc,
      all_of(valores_noespecifica_visuales3) ~ "No especifica",
      all_of(valores_visuales_nunca) ~ "No ha recibido",
      all_of(valores_visuales_2020) ~ "2020",
      all_of(valores_visuales_2019) ~ "2019",
      all_of(valores_visuales_2018) ~ "2018",
      all_of(valores_visuales_2017) ~ "2017",
      all_of(valores_visuales_2016) ~ "2016",
      all_of(valores_visuales_2014) ~ "2014",
      .default = visuales_otro_ultima_vez_libros_mineduc
    )
  )

## volver a ver errores

valores_visuales_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(visuales_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(visuales_otro_ultima_vez_libros_mineduc ))) |>
  select(visuales_otro_ultima_vez_libros_mineduc) |>
  tabyl(visuales_otro_ultima_vez_libros_mineduc)

# View(valores_visuales_otro_ultima_vez_libros_mineduc)


# visuales_tipo_materiales_recibidos ----

## ver errores
valores_visuales_tipo_materiales_recibidos <- df_dyd |>
  filter(visuales_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(visuales_tipo_materiales_recibidos ))) |>
  select(visuales_tipo_materiales_recibidos) |>
  tabyl(visuales_tipo_materiales_recibidos)

dput(valores_visuales_tipo_materiales_recibidos)


## Define los vectores ----

### guia de aprendizaje y planificador
valores_guia_planificador_visuales <- c(
  "GUÍA DE APRENDIZAJE Y PLANIFICADOR DE EXPRESIÓN ARTISTICA", 
  "Guia y planificador", "Guias", "Guias y planificadores", "Guías y planificadores"
)

### libros
valores_libro_visuales <- c(
  "Libro de texto", "Libros"
)

### ninguno
valores_ninguno_visuales3 <- c(
  "Ninguno"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    visuales_tipo_materiales_recibidos = case_match(
      visuales_tipo_materiales_recibidos,
      all_of(valores_ninguno_visuales3) ~ "Ninguno",
      all_of(valores_libro_visuales) ~ "Libros",
      all_of(valores_guia_planificador_visuales) ~ "Guía y planificador",
      .default = visuales_tipo_materiales_recibidos
    )
  )

## volver a ver errores

valores_visuales_tipo_materiales_recibidos <- df_dyd |>
  filter(visuales_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(visuales_tipo_materiales_recibidos ))) |>
  select(visuales_tipo_materiales_recibidos) |>
  tabyl(visuales_tipo_materiales_recibidos)

# View(valores_visuales_tipo_materiales_recibidos)


# fisica_libros_primero ----

## ver errores
valores_fisica_libros_primero <- df_dyd |>
  filter(fisica_uso_libros == "Sí") |>
  filter(fisica_grados == "Primer grado") |>
  filter(!(is.na(fisica_libros_primero))) |>
  select(fisica_libros_primero) |>
  tabyl(fisica_libros_primero)

dput(valores_fisica_libros_primero)

## Define los vectores ----

### CNB
valores_cnb_fisica <- c(
  "CNB"
)

### educacion fisica
valores_edu_fisica <- c(
  "Educación física", 
  "Educación física 1ro. Básico iger", "Guías de educación física"
)

### no especifica
valores_noespecifica_fisica <- c(
  "Hojas de investigación en internet", "Malla curricular"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    fisica_libros_primero = case_match(
      fisica_libros_primero,
      all_of(valores_noespecifica_fisica) ~ "No especifica",
      all_of(valores_edu_fisica) ~ "Educación Física",
      all_of(valores_cnb_fisica) ~ "CNB",
      .default = fisica_libros_primero
    )
  )

## volver a ver errores

valores_fisica_libros_primero <- df_dyd |>
  filter(fisica_uso_libros == "Sí") |>
  filter(fisica_grados == "Primer grado") |>
  filter(!(is.na(fisica_libros_primero))) |>
  select(fisica_libros_primero) |>
  tabyl(fisica_libros_primero)

# View(valores_fisica_libros_primero)

# fisica_libros_segundo ----

## ver errores

valores_fisica_libros_segundo <- df_dyd |>
  filter(fisica_uso_libros == "Sí") |>
  filter(fisica_grados == "Segundo grado") |>
  filter(!(is.na(fisica_libros_segundo))) |>
  select(fisica_libros_segundo) |>
  tabyl(fisica_libros_segundo)

dput(valores_fisica_libros_segundo)

## Define los vectores ----

### cnb
valores_cnb_fisica2 <- c(
  "CNB", "CnB"
)

### disciplina deportiva
valores_disciplina <- c(
  "Disciplinas deportivas" 
)

### guia de aprendizaje
valores_guia_fisica <- c(
  "Guia de aprendizaje", "Guía metodológica para el docente"
)

### ninguno
valores_ninguno_fisica <- c(
  "Google", 
  "Paginas web", "Pelotas, redes, conos, escaleras."
)

## Recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    fisica_libros_segundo = case_match(
      fisica_libros_segundo,
      all_of(valores_ninguno_fisica) ~ "Ninguno",
      all_of(valores_guia_fisica) ~ "Guía de aprendizaje",
      all_of(valores_disciplina) ~ "Disciplinas deportivas",
      all_of(valores_cnb_fisica2) ~ "CNB",
      .default = fisica_libros_segundo
    )
  )

## volver a ver errores

valores_fisica_libros_segundo <- df_dyd |>
  filter(fisica_uso_libros == "Sí") |>
  filter(fisica_grados == "Segundo grado") |>
  filter(!(is.na(fisica_libros_segundo))) |>
  select(fisica_libros_segundo) |>
  tabyl(fisica_libros_segundo)

# View(valores_fisica_libros_segundo)

# fisica_libros_tercero ----

## ver errores

valores_fisica_libros_tercero <- df_dyd |>
  filter(fisica_uso_libros == "Sí") |>
  filter(fisica_grados == "Tercer grado") |>
  filter(!(is.na(fisica_libros_tercero))) |>
  select(fisica_libros_tercero) |>
  tabyl(fisica_libros_tercero)

dput(valores_fisica_libros_tercero)

## Define los vectores ----

### cnb
valores_cnb_fisica3 <- c(
  "CNB digital"
)

### educacion fisica
valores_edu_fisica2 <- c(
  "Libro de Educación Fisica"
)

### guia y planificador
valores_guia_planificador_fisica <- c(
  "Planificador docente y guías de aprendizajes"
)

### sin especificar
valores_noespecifica_fisica2 <- c(
  "Libros de texto", "Libros digitales", 
  "Malla curricular", "Malla curricular y paginas web"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    fisica_libros_tercero = case_match(
      fisica_libros_tercero,
      all_of(valores_noespecifica_fisica2) ~ "No especifica",
      all_of(valores_guia_planificador_fisica) ~ "Guía y planificador",
      all_of(valores_edu_fisica2) ~ "Educación Física",
      all_of(valores_cnb_fisica3) ~ "CNB",
      .default = fisica_libros_tercero
    )
  )

## volver a ver errores

valores_fisica_libros_tercero <- df_dyd |>
  filter(fisica_uso_libros == "Sí") |>
  filter(fisica_grados == "Tercer grado") |>
  filter(!(is.na(fisica_libros_tercero))) |>
  select(fisica_libros_tercero) |>
  tabyl(fisica_libros_tercero)

# View(valores_fisica_libros_tercero)

# fisica_recursos_centro_educativo ----

## volver a ver errores

valores_fisica_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(fisica_recursos_centro_educativo))) |>
  select(fisica_recursos_centro_educativo) |>
  tabyl(fisica_recursos_centro_educativo)

dput(valores_fisica_recursos_centro_educativo)


## Define los vectores ----

### actividades motrices

valores_actividad_motriz <- c(
  "Aros conos pelotas y otros", 
  "Aros cuerdas escaleras balones", "Aros cuerdas pelota conos entre otros", 
  "Aros, Cuerdas pines", "Balones", "Balones de futbol, baloncesto y voleibol, colchonetas, aros, cuerdas, conos , platos de colores", 
  "Balones de fútbol, baloncesto y voleybol conos net", "Balones de fútbol, baloncesto, aros, conos etc.", 
  "Balones de mala calidad", "Balones de muy mala calidad", 
  "Balones, cono, lazos",   "Cancha de basquet ball, campo de foot ball, pelotas, conos, hula-hulas.", 
  "CONOS, ESCALERAS, REDES",
  "Cancha de basquetbol, campo de futbol, pelotas, conos y hula-hulas", 
  "Cancha de basquetbol, campo de futbol, pelotas, hula-hulas, conos", 
  "Cancha,conos,balones,cuerdas",
  "Comos y uniformes",  "Escaleras, conos, pelotas, hula hula", "Escaleres, Balones, Conos, red Volibol", 
  "Cono y ula ula", "Cono, aros, balones, cuerdas", "Conos", 
  "Conos , cuerdas y hulas", "Conos ,pelotas, cuerdas", "Conos escaleras u otor", 
  "Conos mallas pelotas", "Conos y aros", "Conos y balones", 
  "Conos y pelotas", "Conos,  balones, aros", "Conos, ULA ULA, pelotas", 
  "Conos, algunos balones, algunos aros", "Conos, aros, cuerdas, pelotas, net, otros.", 
  "Conos, aros, pelotas, cuerdas, escaleras. Etc", "Conos, aros, pelotas, globos, escaleras,pelotas", 
  "Conos, aros, silbato y cronómetro", "Conos, balon, cuerdas y ulas", 
  "Conos, balones", "Conos, balones, bollas escaleras, etc.", 
  "Conos, balones, ulas, escaleras y cuerdas", "Conos, escaleras y hulas", 
  "Conos, escaleras, Pelotas", "Conos, escaleras, pelotas", 
  "Conos, hula hula, pelotas, pita", "Conos, hulas", "Conos, hulas hulas", 
  "Conos, material para boly bol", "Conos, pelotas", "Conos, pelotas, aros", 
  "Conos, pelotas, bollas, y escalera", "Conos, pelotas, hulas", 
  "Conos, ula-ula", "Conos, uñas, escaleras, balones de fútbol y básquet bol, etc", 
  "Conos,Aros,Cuerdas,Vallas,Balones, Canchas, Red, etc", "Conos,balones", 
  "Conos., cuerdas", "Copias", "Cuerdas Aros", "Cuerdas, Pines  aros", 
  "Cuerdas, Pines y aros", "DIGEF balón, aros, conos, señaladores, colchonetas, escaleras de agilidad, tableros de ajedrez.", 
  "De atletismo", "Instrumentos deportivos",
  "Hula Hula, balones, net conos", 
  "ODEC, conos, pelotas, aros etc.", 
  "Pelota, aros, hulas, conos", 
  "Pelota, conos y mas", "Pelotas", "Pelotas (varias) mesa de tenis de mesa y sus implementos. Ajedrez", 
  "Pelotas , conos", "Pelotas .cuerda .conos etc", "Pelotas conos", 
  "Pelotas, conos, cintas, hula-hula", "Pelotas, conos, cronometros, cuerdas, aros.", 
  "Pelotas, conos, cuerdas", "Pelotas, conos, escaleras, pita, hula hoop.", 
  "Pelotas, escaleras,", "Pelotas, escaleras, conos.", "Pelotas, silbatos, entre otros", 
  "Pelotas,aro, cono", "Pelotas,conos, escaleras,bollas", "Pelotazo arroz", 
  "Hula hulas, conos, pelotas, escaleras plásticas.", "IMPLEMENTACION DEPORTIVA", 
  "IMPLEMENTOS DEPORTIVOS", "Implementos deportivos", "Implementos depórtivo y área física", 
  "Implementos y espacios amplios", "Impresiones, cancha polideportiva, cancha basquetbol, balones, conos", 
  "Equipamiento deportivo",    "Impresoras, gorgorito, conos, escalera, bocinas.", 
  "Balones, conos", "Balones, conos,", 
  "aros, conos, cuerdas, pelotas, pizarra, marcadores, uniforme, otros", 
  "cono, balones", "conos, aros balones", "conos, pelotas. aros. y de mas material", 
  "Vestidores, Bodega para materiales, pelotas de futbol, volibol, basquetbol,  redes, conos , aros, colchonetas, paracaídas, silbato, gabachas para diferenciar a los equipos,", 
  "Materias para realizar los ejercicios", "Movilidad del cuerpo", 
  "Balones, conos, aros, escaleras, cuerdas.", "Balones, conos, balones de mala calidad", 
  "Balones, conos, campo de fútbol, cancha de voleibol, cancha de basquetbol", 
  "Balones, conos, cuerdas, aros, etc", "Balones, conos, cuerdas, cancha, etc.", 
  "Balones, conos, cuerdas, net , colchonetas, balas, jabalinas", 
  "Balones, conos, escaleras y cuerdas", "Balones, conos, red.", 
  "Tecnológicos: Cañoneras, bocina, tablet. Materiales: balones, conos, cuerdas etc.", 
  "Balones, conos, redes,", "Balones, conos,aros,colchonetas", 
  "escaleras, conos, pelotas, discos", 
  "Libros pelotas,conos etc",  "Material para deporte", "Materiales deportivos",
  "Balones, conos.", "Balones, escaleras, conos, platos, cuerdas, colchonetas", 
  "Balones, hulas, cuerdas, conos, colchonetas.", "CNB", "CNB módulos de aprendizaje", 
  "CONOS, ESCALERAS, REDES", "Cancha de basquet ball, campo de foot ball, pelotas, conos, hula-hulas."
)

### audiovisual y didáctico
valores_audio_didactico_fisica <- c(
  "Capacitaciones y material didáctico",
  "Folletos videos audios",  "Libro, PDF, VIDEOS",
  "Materiales",  "Materiales didácticos", 
  "Materiales de apoyo", "Valija didáctica", 
  "Materia técnico pedagógico para Educ. Física", "Material", 
  "Material dado por el ministerio de educación", "Material deportivo", 
  "Material deportivo, recursos tecnológicos y libros", "Material didactico, balones conos etc.", 
  "Material didáctico", "Material didáctico  y la tecnología", 
  "Material didáctico libros, recursos materiales, instalaciones", 
  "Material didáctico y materia para realizar ejercicios"
)

### balones
valores_balones <- c(
  "Aros conos pelotas y otros", 
  "Aros cuerdas escaleras balones", "Aros cuerdas pelota conos entre otros",
  "Balones", "Balones de futbol, baloncesto y voleibol, colchonetas, aros, cuerdas, conos , platos de colores", 
  "Balones de fútbol, baloncesto y voleybol conos net", "Balones de fútbol, baloncesto, aros, conos etc.", 
  "Balones de mala calidad", "Balones de muy mala calidad", 
  "Libros pelotas,conos etc", "Pocas pelotas balonesto", 
  "Pelota, aros, hulas, conos", 
  "aros, conos, cuerdas, pelotas, pizarra, marcadores, uniforme, otros", 
  "cono, balones", "conos, aros balones", "conos, pelotas. aros. y de mas material", 
  "Vestidores, Bodega para materiales, pelotas de futbol, volibol, basquetbol,  redes, conos , aros, colchonetas, paracaídas, silbato, gabachas para diferenciar a los equipos,", 
  "Pelota, conos y mas", "Pelotas", "Pelotas (varias) mesa de tenis de mesa y sus implementos. Ajedrez", 
  "Pelotas , conos", "Pelotas .cuerda .conos etc", "Pelotas conos", 
  "Pelotas, conos, cintas, hula-hula", "Pelotas, conos, cronometros, cuerdas, aros.", 
  "Pelotas, conos, cuerdas", "Pelotas, conos, escaleras, pita, hula hoop.", 
  "Pelotas, escaleras,", "Pelotas, escaleras, conos.", "Pelotas, silbatos, entre otros", 
  "Pelotas,aro, cono", "Pelotas,conos, escaleras,bollas", "Pelotazo arroz", 
  "Material didactico, balones conos etc.", 
  "Malla, balones conos", "Marcadores cartulinas pelota cilvatos conos", 
  "Marcadores, cartulinas, pelotas, conos.", "Marcadores, pelotas, cartulinas, conos", 
  "Balones, cono, lazos", "Balones, conos", "Balones, conos,", 
  "Balones, conos, aros, escaleras, cuerdas.", "Balones, conos, balones de mala calidad", 
  "Balones, conos, campo de fútbol, cancha de voleibol, cancha de basquetbol", 
  "Balones, conos, cuerdas, aros, etc", "Balones, conos, cuerdas, cancha, etc.", 
  "Balones, conos, cuerdas, net , colchonetas, balas, jabalinas", 
  "Balones, conos, escaleras y cuerdas", "Balones, conos, red.", 
  "Conos, balon, cuerdas y ulas", 
  "Hula Hula, balones, net conos",  "escaleras, conos, pelotas, discos", 
  "Tecnológicos: Cañoneras, bocina, tablet. Materiales: balones, conos, cuerdas etc.", 
  "Hula hulas, conos, pelotas, escaleras plásticas.", 
  "Escaleras, conos, pelotas, hula hula", "Escaleres, Balones, Conos, red Volibol", 
  "Conos, pelotas", "Conos, pelotas, aros", 
  "pelotas de toda clase o deporte", "pelotas,", 
  "pelotas, conos, escaleras",
  "Conos, pelotas, bollas, y escalera", "Conos, pelotas, hulas", 
  "Conos, escaleras, Pelotas", "Conos, escaleras, pelotas", 
  "Conos, hula hula, pelotas, pita", "La cancha conos conchas gabachas",
  "Conos, uñas, escaleras, balones de fútbol y básquet bol, etc", 
  "Conos,Aros,Cuerdas,Vallas,Balones, Canchas, Red, etc", "Conos,balones", 
  "Conos, balones", "Conos, balones, bollas escaleras, etc.", 
  "Conos, balones, ulas, escaleras y cuerdas",
  "DIGEF balón, aros, conos, señaladores, colchonetas, escaleras de agilidad, tableros de ajedrez.", 
  "Balones, conos, redes,", "Balones, conos,aros,colchonetas","Conos ,pelotas, cuerdas",
  "Conos mallas pelotas","Conos y balones", "Conos y pelotas", "Conos,  balones, aros", "Conos, ULA ULA, pelotas",
  "Conos, algunos balones, algunos aros", "Conos, aros, cuerdas, pelotas, net, otros.", 
  "Conos, aros, pelotas, cuerdas, escaleras. Etc", "Conos, aros, pelotas, globos, escaleras,pelotas",
  "Balones, conos.", "Balones, escaleras, conos, platos, cuerdas, colchonetas", 
  "Balones, hulas, cuerdas, conos, colchonetas."
)

### cancha deportiva
valores_cancha <- c(
  "Balones, conos, campo de fútbol, cancha de voleibol, cancha de basquetbol", 
  "Balones, conos, cuerdas, cancha, etc.",
  "Implementos depórtivo y área física",  "el espacio o patio del establecimiento",
  "Instalación deportiva",  "Solo una cancha techada", 
  "Material didáctico libros, recursos materiales, instalaciones", 
  "Impresiones, cancha polideportiva, cancha basquetbol, balones, conos", 
  "Cancha de basquet ball, campo de foot ball, pelotas, conos, hula-hulas.", 
  "Cancha de basquetbol, campo de futbol, pelotas, conos y hula-hulas", 
  "Cancha de basquetbol, campo de futbol, pelotas, hula-hulas, conos", 
  "Cancha deportiva", "Cancha,conos,balones,cuerdas", 
  "Conos,Aros,Cuerdas,Vallas,Balones, Canchas, Red, etc",
  "La cancha conos conchas gabachas"
)

### cnb
valores_fisica_cnb <- c(
  "CNB", "CNB módulos de aprendizaje", "Cnb",
  "El CNB", "El CNB, capacitaciones en mi área",
  "Hojas y guias CNB"
  
)

### guias y folletos
valores_guia_folleto_fisica <- c(
  "Folletos", "Folletos videos audios", 
  "Guia de aprendizaje",    "PLANIFICACIÓN", 
  "Guias", "Guías de educación física", "Guías educativas", 
  "Guías para el estudiante", "Planificador docente", "tabla de planificación"
)

### instrumentos control y medicion
valores_control_medicion <- c(
  "Conos, aros, silbato y cronómetro",
  "Impresoras, gorgorito, conos, escalera, bocinas.", 
  "Marcadores cartulinas pelota cilvatos conos", "Vos de mando"
)

### libro
valores_libros_fisica <- c(
  "Educación física",  "LIBRO DE TEXTO",
  "Libro", "Libro de conceptos", 
  "Material didáctico libros, recursos materiales, instalaciones", 
  "Libro de texto", "Libro, PDF, VIDEOS", "Libros", "Libros de Textos, documentos de apoyo", 
  "Libros de ejercicios", "Libros de texto", "Libros impresos", 
  "Libros pelotas,conos etc", "MANUAL DE EDUCACION FISICA"
)

### mobiliario
valores_mobiliario_fisica <- c(
  "Impresora, cañonera y pizarra"
)

### ninguno
valores_ninguno_fisica2 <- c(
  "N/A", "Nada", "Ninguna", "Ninguno", "Ninguno son materiales propios", 
  "No", "No hay", "nada", "ninguno"
)

### papeleria
valores_papeleria_fisica <- c(
  "Marcadores cartulinas pelota cilvatos conos",
  "Marcadores, cartulinas, pelotas, conos.",
  "Marcadores, pelotas, cartulinas, conos"
  
)

### recursos digitales
valores_digitales_fisica <- c(
  "Digitales",  "Investigación", "Investigación acorde el grado",
  "Material digital",  "Paginas web",
  "Páginas web", "Web",
  "Redes sociales"
)


### sin especificar
valores_sinespecificar_fisica <- c(
  "Sólo cuento con el material que tienen de años atrás, no mucho, por cierto, hace falta muchos tipos de balones.", 
  "Varios"
)
### tecnologia
valores_tecnologia_fisica <- c(
  "Cañonera", "Centro de computación",
  "Impresora, cañonera y pizarra",  "Internet", 
  "Pantalla, Impresora",
  "Tecnológicos: Cañoneras, bocina, tablet. Materiales: balones, conos, cuerdas etc.", 
  "Impresoras, gorgorito, conos, escalera, bocinas.", 
  "Material deportivo, recursos tecnológicos y libros", "Material didáctico  y la tecnología"
  
)

## Recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    fisica_recursos_centro_educativo = case_match(
      fisica_recursos_centro_educativo,
      all_of(valores_tecnologia_fisica) ~ "Equipo tecnológico",
      all_of(valores_sinespecificar_fisica) ~ "No especifica el recurso",
      all_of(valores_digitales_fisica) ~ "Recursos digitales",
      all_of(valores_papeleria_fisica) ~ "Material de papeleria",
      all_of(valores_ninguno_fisica2) ~ "Ninguno",
      all_of(valores_mobiliario_fisica) ~ "Mobiliario",
      all_of(valores_libros_fisica) ~ "Libros",
      all_of(valores_control_medicion) ~ "Instrumentos de control y medición",
      all_of(valores_guia_folleto_fisica) ~ "Guías y folletos",
      all_of(valores_fisica_cnb) ~ "CNB",
      all_of(valores_cancha) ~ "Canchas deportivas",
      all_of(valores_balones) ~ "Balones",
      all_of(valores_audio_didactico_fisica) ~ "Material audiovisual y didáctico",
      all_of(valores_actividad_motriz) ~ "Material para actividades motrices",
      .default = fisica_recursos_centro_educativo
)
  )

## volver a ver errores

valores_fisica_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(fisica_recursos_centro_educativo))) |>
  select(fisica_recursos_centro_educativo) |>
  tabyl(fisica_recursos_centro_educativo)

# View(valores_fisica_recursos_centro_educativo)

# fisica_recursos_propios ----

## ver errores

valores_fisica_recursos_propios <- df_dyd |>
  filter(!(is.na(fisica_recursos_propios))) |>
  select(fisica_recursos_propios) |>
  tabyl(fisica_recursos_propios)

dput(valores_fisica_recursos_propios)

## Define los vectores ----

### actividades motrices

valores_actividad_motriz2 <- c(
  "Aros, balones, inflador", 
  "Cintas","Cuerda, balones de basquetbol", 
  "Cuerdas", "Cuerdas y Pines", "Cuerdas, costales", "De la malla curricular", 
  "Deportivo", "Diplomados estudios pagados,material e indumentaria propia", 
  "Conos", "Conos deportivos de la dirección del establecimiento, e investigación de ejercios para mantener una buena salud física", 
  "Conos escaleras campo de baloncesto aros cuerdas", "Conos hulas escaleras", 
  "Conos pelota escalera ulaula pi", "Conos pelotas .cuerdas  gorgoritos", 
  "Conos silbato y escaleras", "Conos y boyas propios", "Conos y pelotas", 
  "Conos, aros, balones y otros", "Conos, balones, cuerdas", "Conos, balones, cuerdas entre otros", 
  "Conos, cuerdas", "Conos, cuerdas, pelotas.", "Conos, escaleras, pelotas, salta cuerdas", 
  "Conos, listado de alumnos, balones, globos, cuerdas, etc", "Conos, pelotas", 
  "Conos, pelotas, cuerdas", "Conos, pelotas, ulas ulas", "Conos, silbatos, reloj, pelotas", 
  "Conos,balones,cuerdas", "Conos,balones,cuerdas entre oras cosas mas", 
  "Conos,escaleras,cuerdas balones,",
  "conos, pelotas, cuerdas ,ulaulas", "conos, pelotas, unas, y otros", 
  "cuerdas pines,", "cuerdas, balones", "equipo de Computo", "lazos, cuerdas para saltar,  bocina", 
  "Una net, tablas de natación, flotadores, balones de diferentes deportes", 
  "Equipamiento deportivo", "Inflador,pito o gorgorito,", "Infladores, conos", 
  "Instrumentos deportivos", "Ligas", 
  "Gorgorito, balones, conos.", "Material digital e implementos deportivos", 
  "Gorgorito, cronómetro, balones, conos,", 
  "Gorgorito, cronómetro, net, estafetas, pelotas beisbol, bates, libros de textos", 
  "Gorgorito, cuerdas, discos", "Gorgoritos, cronómetro, Tableros, Cuerdas, Pelotas", 
  "Gorgoritos, platos escaleras aros pelotas conos",
  "Hula hula pelotas", "Lazos", "Material deportivo", 
  "Material de educación física: conos, hula hula, pelotas, escaleras, cuerdas entre otros", 
  "Hulas", "Hulas, pelotas", "Implementos deportivos", 
  "Materiales: pelotas y conos",
  "Tecnológicos , deportivos y libros",
  "Silbato, conos, inflador y tablero", 
  "Silbato, cuerdas", "Silbato, hojas,  escaleras, cuerdas", 
  "Palos de escoba, conos", "Pelotazo  conos",
  "Pelota,aros,cuerdas,conos", "Pelotas , conos, escaleras", 
  "Pelotas conos", "Pelotas de futbol, pelotas de voleibol, conos, gorgorito,guantes de portero",  "Pelotas estafeta conos aros y otros", 
  "Pelotas hulas", "Pelotas, aros, hulas", "Pelotas, conos, hojas de investigación", 
  "Pelotas, conos, platillos, cuerdas, aros, y materiales fabricados", 
  "Pelotas, cuerdas, sílbato, material impreso, material reciclado", 
  "Parachute redes de fut red voly","Pines",
  "Implementos.", "Implemntos deportivos", "Impresiones, conos, silbato", 
  "Estafetas, silbato pitas", "Felletos, silbato, conos y pelotas", 
  "Computadora, impresora, internet, libros, pelotas, cuerda para saltar, conos y gorgoritos.", 
  "Computadora, impresora, libros de texto, cuerdas para saltar, conos, pelotas y gorgoritos.", 
  "Computadora, impresora, libros de textos, cuerdas para saltar, pelotas, conos y gorgoritos.", 
  "Aros, conos, balones escaleras, colchonetas, cuerdas", "Aros, cuerdas, balones,picas, conos, escalerillas, colchonetas", 
  "Aros, pines y Cuerdas", "Balon, aros, conos, Salta cuerda,  pino,cronometro,", 
  "Balones silvatos conos etc", "Balones y conos", "Balones, Net o reses, silbatos", 
  "Balones, conos", "Balones, conos, lazos", "Balones, conos, net", 
  "CONOS, CUERDAS CANCHA", "Canchas, balones y otros materiales", 
  "Balones, silbato, conos, colchonetas...", "Balones, silbatos, cuerdas , libros"
)

### audiovisual y didáctico
valores_audio_didactico_fisica2 <- c(
  "Material", "Material audio visual", "Material audiovisual",
  "Material de reciclaje   ,costales,  pelotas,   libros ,",
  "Pelotas, cuerdas, sílbato, material impreso, material reciclado", 
  "Pelotas, inflador, materual reciclable",
  "Silbato, material reciclado",
  "Videos", "Videos audios etc", "Videos, pañuelos, net, tiza, dados.", 
  "Tableros de ajedrez, pelotas de básquet, voley y fut.", 
  "Material didactico,páginas", "Musica videos e investigaciones.", 
  "Material reciclable", "Materiales", "Materiales de reciclaje, hojas de trabajo y otros"
)
### balones

valores_balones2 <- c(
  "Aros, balones, inflador", "Materiales: pelotas y conos",
  "Material de educación física: conos, hula hula, pelotas, escaleras, cuerdas entre otros", 
  "Aros, conos, balones escaleras, colchonetas, cuerdas", "Aros, cuerdas, balones,picas, conos, escalerillas, colchonetas", 
  "Balon, aros, conos, Salta cuerda,  pino,cronometro,", 
  "Balones", "Balones de acuerdo al deporte que se practica", "Balones para futbol", 
  "Balones silvatos conos etc", "Balones y conos", "Balones, Net o reses, silbatos", 
  "Balones, conos", "Balones, conos, lazos", "Balones, conos, net", 
  "Balones, silbato, conos, colchonetas...", "Balones, silbatos, cuerdas , libros", 
  "Balones. Gorgorito", "Hula hula pelotas", 
  "cuerdas, balones",
  "conos, pelotas, cuerdas ,ulaulas", "conos, pelotas, unas, y otros", 
  "Una net, tablas de natación, flotadores, balones de diferentes deportes", 
  "Hulas, pelotas","Libros balones aros", 
  "Libros, pelotas y más", "Material de reciclaje   ,costales,  pelotas,   libros ,",
  "Gorgorito, balones, conos.", "Silbato, textos, balones", 
  "Pelota de  basket", "Pelota y mas", 
  "Pelota,aros,cuerdas,conos", "Pelotas", "Pelotas , conos, escaleras", 
  "Pelotas conos", "Pelotas de foot bol.", "Pelotas de futbol, pelotas de voleibol, conos, gorgorito,guantes de portero", 
  "Pelotas de fútbol y BASQUETBOL", "Pelotas estafeta conos aros y otros", 
  "Pelotas hulas", "Pelotas, aros, hulas", "Pelotas, conos, hojas de investigación", 
  "Pelotas, conos, platillos, cuerdas, aros, y materiales fabricados", 
  "Pelotas, cuerdas, sílbato, material impreso, material reciclado", 
  "Pelotas, inflador, materual reciclable", "Pelotas, net, silbato", 
  "Pelotas,escaleras y aros", "Silbato, balones etc.",
  "Net, tablas de notación, flotadores, balones", 
  "Gorgoritos, cronómetro, Tableros, Cuerdas, Pelotas", 
  "Gorgoritos, platos escaleras aros pelotas conos", 
  "Gorgorito, cronómetro, balones, conos,", 
  "Tableros de ajedrez, pelotas de básquet, voley y fut.", 
  "Gorgorito, cronómetro, net, estafetas, pelotas beisbol, bates, libros de textos", 
  "Conos y pelotas", "Cuerda, balones de basquetbol", 
  "Conos, aros, balones y otros", "Conos, balones, cuerdas", "Conos, balones, cuerdas entre otros", 
  "Conos, cuerdas, pelotas.", "Conos, escaleras, pelotas, salta cuerdas", 
  "Conos, listado de alumnos, balones, globos, cuerdas, etc", "Conos, pelotas", 
  "Conos, pelotas, cuerdas", "Conos, pelotas, ulas ulas", "Conos, silbatos, reloj, pelotas", 
  "Conos,balones,cuerdas", "Conos,balones,cuerdas entre oras cosas mas", 
  "Conos,escaleras,cuerdas balones,", "Felletos, silbato, conos y pelotas", 
  "Conos pelota escalera ulaula pi", "Conos pelotas .cuerdas  gorgoritos", 
  "Canchas, balones y otros materiales"
  
)


### cancha deportiva
valores_cancha2 <- c(
  "CONOS, CUERDAS CANCHA", "Canchas, balones y otros materiales", 
  "Conos escaleras campo de baloncesto aros cuerdas",
  "Contexto y areas del centro educativo"
  
)
### cnb
valores_fisica_cnb2 <- c(
  "Busco, practico según el CNB", "El CNB", 
  "CNB", "CNB digital, internet","Cnb","Libros y cnb"
  
)

### guias y folletos
valores_guia_folleto_fisica2 <- c(
  "Felletos, silbato, conos y pelotas", "Marcadores, folletos, copias", 
  "Folletos", "Folletos  cilvatos  conos  pelotas", "Folletos e impresiones", 
  "Folletos, cilbato, conos y pelotas", "GUIAS","Planificación, diario pedagógico etc.", 
  "Planificador docente", 
  "Silbato, planificaciones", "Silbato, tabla shanon, guías de aprendizaje", 
  "Guia de aprendizaje", "Malla curricular y actividades planificadas",
  "Guía del CNB", "Guía metodológica de Educación física virtual e investigaciones" 
  
)
### instrumento control y medicion

valores_control_medicion2 <- c(
  "Balon, aros, conos, Salta cuerda,  pino,cronometro,", 
  "Balones silvatos conos etc","Balones, Net o reses, silbatos", 
  "Balones, silbato, conos, colchonetas...", "Balones, silbatos, cuerdas , libros", 
  "Balones. Gorgorito","Cronómetro, gorgorito,tablilla",
  "Conos pelotas .cuerdas  gorgoritos", "Mi gorgorito,  inflador de pelotas", 
  "Libros, computadora, silbato, pecheras", 
  "silbato, libros, computadora",
  "Impresiones, conos, silbato", "Inflador,pito o gorgorito,", 
  "Gorgorito", "Gorgorito, balones, conos.", 
  "Pelotas, net, silbato", "Pito y cuerdas", 
  "Pitó, copias, agujas p/inflar", "Vallas, silbato", 
  "Un silbato, tabla y ropa deportiva",
  "Silbato", "Silbato y reloj", "Tablero,silbato,cronómetro", 
  "Silbato, Inflador", "Silbato, balones etc.", "Silbato, conos, inflador y tablero", 
  "Silbato, cronometro, reloj de mano", "Silbato, cronómetro", 
  "Silbato, cuerdas", "Silbato, hojas,  escaleras, cuerdas", "Silbato, humano", 
  "Silbato, material reciclado", "Silbato, planificaciones", "Silbato, tabla shanon, guías de aprendizaje", 
  "Silbato, teléfono para cronómetros y tiempo", "Silbato, tenis", 
  "Silbato, textos, balones", "Silbatos", "Silvato. Reloj,", "Silvatos, escaleras y ulas.",
  "Gorgorito, cronómetro", "Gorgorito, cronómetro, balones, conos,", 
  "Gorgorito, cronómetro, net, estafetas, pelotas beisbol, bates, libros de textos", 
  "Gorgorito, cuerdas, discos", "Gorgoritos, cronómetro, Tableros, Cuerdas, Pelotas", 
  "Gorgoritos, platos escaleras aros pelotas conos",
  "Conos silbato y escaleras", "GORGORITO, TABLA DE MADERA", 
  "Estafetas, silbato pitas", "Felletos, silbato, conos y pelotas",
  "Computadora, impresora, internet, libros, pelotas, cuerda para saltar, conos y gorgoritos.", 
  "Computadora, impresora, libros de texto, cuerdas para saltar, conos, pelotas y gorgoritos.", 
  "Computadora, impresora, libros de textos, cuerdas para saltar, pelotas, conos y gorgoritos."
)


### Libros
valores_libros_fisica2 <- c(
  "Computadora, impresora, internet, libros, pelotas, cuerda para saltar, conos y gorgoritos.", 
  "Computadora, impresora, libros de texto, cuerdas para saltar, conos, pelotas y gorgoritos.", 
  "Computadora, impresora, libros de textos, cuerdas para saltar, pelotas, conos y gorgoritos.", 
  "INTERNET, LIBROS","LAPTOP, INTERNET Y LIBROS PROPIOS DE EDUCACION FISICA", 
  "LIBRO DE TEXTO", "Material de reciclaje   ,costales,  pelotas,   libros ,",
  "Libro", "Libro de Telesecundaria", 
  "Textos para reforzamiento.", "Textos, implementos deportivos", 
  "Silbato, textos, balones", "Tecnológicos , deportivos y libros",
  "Libros", "Libros balones aros", "Libros de texto", "Libros de texto, materiales de apoyo", 
  "Libros digitales", "Libros planificación", "Libros que nos dió la departamental de escuintla", 
  "Libros y cnb", "Libros, PDF", "Libros, computadora, silbato, pecheras", 
  "Libros, guías del área", "Libros, paginas web", "Libros, pelotas y más"
)

### ninguno
valores_ninguno_fisica3 <- c(
  "Ejercicios,juegos calentamientos", "Google",
  "Ninguno","nada", "ninguno"
)

### papeleria
valores_papeleria_fisica2 <- c(
  "Copias", "Hojas", "Hojas de trabajo", "Hojas de trabajos",
  "Impresiones, conos, silbato", "Tabla de planificación", 
  "Impresión de hojas", "Marcadores, folletos, copias"
)

### recursos digitales
valores_digital_fisica2 <- c(
  "Contenidos de Google", "Digitales", 
  "Internet", "Internet, libro digital, musica de ejercicios funcionales", 
  "Internet.", "Investigaciones", "Investigación propia", 
  "Musica videos e investigaciones.", 
  "Página w", "Recursos de la Web", "Recursos virtuales, pants, etc", 
  "Libros, paginas web","Material digital e implementos deportivos"
)

### sin especificar
valores_noespecifica_fisica3 <- c(
  "Económicos","Elementos de juego, diferentes recursos","Kerebell", 
  "Recursos materiales", "Todo", "Todos",
  "VARIOS"
  
)
### tecnologia
valores_tecnologia_fisica2 <- c(
  "Bocina", "Bocinas, copias.", 
  "Compuadora y bocinas", "Computadora e impresora", 
  "Computadora, Internet", "Computadora, equipo de implementacion de fisica", 
  "Computadora, impresora, internet, libros, pelotas, cuerda para saltar, conos y gorgoritos.", 
  "Computadora, impresora, libros de texto, cuerdas para saltar, conos, pelotas y gorgoritos.", 
  "Computadora, impresora, libros de textos, cuerdas para saltar, pelotas, conos y gorgoritos.", 
  "Computadora, impresora.", "Dispositivos electrónicos",
  "Tecnologicos", 
  "equipo de Computo", "lazos, cuerdas para saltar,  bocina", 
  "silbato, libros, computadora",
  "Tecnológicos , deportivos y libros", "Teléfono, internet", 
  "LAPTOP, INTERNET Y LIBROS PROPIOS DE EDUCACION FISICA",
  "Minicomputadora, tablet, teléfono internet","TELEFONO CELULAR", 
  "Laptop e internet", "Libros, computadora, silbato, pecheras"
)

### uniforme
valores_uniforme_fisica <- c(
  "Traje de deporte, asistencia", "Un silbato, tabla y ropa deportiva"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    fisica_recursos_propios = case_match(
      fisica_recursos_propios,
      all_of(valores_uniforme_fisica) ~ "Uniforme",
      all_of(valores_noespecifica_fisica3) ~ "No especifica el recurso",
      all_of(valores_digital_fisica2) ~ "Recursos digitales",
      all_of(valores_papeleria_fisica2) ~ "Material de papeleria",
      all_of(valores_ninguno_fisica3) ~ "Ninguno",
      all_of(valores_libros_fisica2) ~ "Libros",
      all_of(valores_guia_folleto_fisica2) ~ "Guías y folletos",
      all_of(valores_fisica_cnb2) ~ "CNB",
      all_of(valores_cancha2) ~ "Canchas deportivas",
      all_of(valores_balones2) ~ "Balones",
      all_of(valores_audio_didactico_fisica2) ~ "Material audiovisual y didáctico",
      all_of(valores_actividad_motriz2) ~ "Material para actividades motrices",
      all_of(valores_tecnologia_fisica2) ~ "Equipo tecnológico",
      all_of(valores_control_medicion2) ~ "Instrumentos de control y medición",
      .default = fisica_recursos_propios
    )
  )

## volver a ver errores

valores_fisica_recursos_propios <- df_dyd |>
  filter(!(is.na(fisica_recursos_propios))) |>
  select(fisica_recursos_propios) |>
  tabyl(fisica_recursos_propios)

# View(valores_fisica_recursos_propios)

# fisica_otro_ultima_vez_libros_mineduc ----

## ver errores

valores_fisica_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(fisica_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(fisica_otro_ultima_vez_libros_mineduc ))) |>
  select(fisica_otro_ultima_vez_libros_mineduc) |>
  tabyl(fisica_otro_ultima_vez_libros_mineduc)

dput(valores_fisica_otro_ultima_vez_libros_mineduc)

## Define los vectores ----

### 2006
valores_fisica_2006 <- c(
  "2006"
)

### 2007
valores_fisica_2007 <- c(
  "2007"
)

### 2018
valores_fisica_2018 <- c(
  "2018"
)

### 2019
valores_fisica_2019 <- c(
  "2019"
)

### 2020
valores_fisica_2020 <- c(
  "2020"
)

### nunca
valores_nunca_fisica <- c(
  "En ningun momento", "En ningún año", 
  "N/A", "NINGUNO", "Ninguno", "Ningún", "No", "No existen", 
  "No he recibido", "No he recibido.", "No hemos recibido", 
  "0",  "No se a recibido", "No se ha recibido material sobre esta área", 
  "No se ha recibido ningún material de esta área", "No tenemos material específico.", 
  "Nunca", "Nunca he recibido", "nunca"
  
)

### no recuerda
valores_norecuerda_fisica <- c(
  "No recuerdo"
)

### no especifica
valores_sinespecificar_fisica2 <- c(
  "1", "Otro"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    fisica_otro_ultima_vez_libros_mineduc = case_match(
      fisica_otro_ultima_vez_libros_mineduc,
      all_of(valores_sinespecificar_fisica2) ~ "No especifica",
      all_of(valores_norecuerda_fisica) ~ "No recuerda",
      all_of(valores_nunca_fisica) ~ "Nunca ha recibido",
      all_of(valores_fisica_2020) ~ "2020",
      all_of(valores_fisica_2019) ~ "2019",
      all_of(valores_fisica_2018) ~ "2018",
      all_of(valores_fisica_2007) ~ "2007",
      all_of(valores_fisica_2006) ~ "2006",
      .default = fisica_otro_ultima_vez_libros_mineduc
    )
  )

## volver a ver errores

valores_fisica_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(fisica_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(fisica_otro_ultima_vez_libros_mineduc ))) |>
  select(fisica_otro_ultima_vez_libros_mineduc) |>
  tabyl(fisica_otro_ultima_vez_libros_mineduc)

# View(valores_fisica_otro_ultima_vez_libros_mineduc)

# fisica_tipo_materiales_recibidos ----

## ver errores
valores_fisica_tipo_materiales_recibidos <- df_dyd |>
  filter(fisica_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(fisica_tipo_materiales_recibidos ))) |>
  select(fisica_tipo_materiales_recibidos) |>
  tabyl(fisica_tipo_materiales_recibidos)

dput(valores_fisica_tipo_materiales_recibidos)

## Define los vectores ----

### actividad motriz
valores_actividad_motriz3 <- c(
  "Conos, escaleras, pelotas"
)

### balones
valores_balones3 <- c(
  "Conos, escaleras, pelotas"
)

### guias
valores_guias_fisica <- c(
  "Guias"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    fisica_tipo_materiales_recibidos = case_match(
      fisica_tipo_materiales_recibidos,
      all_of(valores_guias_fisica) ~ "Guías",
      all_of(valores_balones3) ~ "Balones",
      all_of(valores_actividad_motriz3) ~ "Materiales para actividades motrices",
      .default = fisica_tipo_materiales_recibidos
    )
  )

## volver a ver errores

valores_fisica_tipo_materiales_recibidos <- df_dyd |>
  filter(fisica_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(fisica_tipo_materiales_recibidos ))) |>
  select(fisica_tipo_materiales_recibidos) |>
  tabyl(fisica_tipo_materiales_recibidos)

# View(valores_fisica_tipo_materiales_recibidos)

# eemp_libros_primero ----

## ver errores

valores_emp_libros_primero <- df_dyd |>
  filter(emp_uso_libros == "Sí") |>
  filter(emp_grados == "Primer grado") |>
  filter(!(is.na(emp_libros_primero))) |>
  select(emp_libros_primero) |>
  tabyl(emp_libros_primero)

dput(valores_emp_libros_primero)

## Define los vectores ----

### agropecuaria
valores_agropecuaria <- c(
  "Agropecuaria"
)

### contabilidad 1
valores_conta1 <- c(
  "Contabilidad 1"
)

### emprendimiento para la productividad
valores_emp_produ <- c(
  "Emprendimiento", "Emprendimiento a la productividad Guillermo Pineda", 
  "Emprendimiento para la Productividad", "Emprendimiento para la productividad", 
  "Emprendimiento y productividad 1ro básico"
)

### guia
valores_guia_emp <- c(
  "GUIA DE ENTRENAMIENTO \"YO DECIDO\" DE JOHN MAXWELL", 
  "Guia", "Guías Docentes", "Guías Tommi", "Guías docentes", 
  "Guías y planificadores"
)

### productividad y desarrollo
valores_produ_desa <- c(
  "Productividad y desarrollo", "Proyectos, Productividad y Desarrollo 7"
)

### ninguno
valores_ninguno_emp <- c(
  "Ninguno"
)

### no especifica
valores_noespecifica_emp <- c(
  "Ededitora educativa y Susaeta", "Malla curricular", "Materiales impresos", 
  "Normas", "Sólo en digital el que utiliza el MINEDUC"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    emp_libros_primero = case_match(
      emp_libros_primero,
      all_of(valores_noespecifica_emp) ~ "No especifica",
      all_of(valores_ninguno_emp) ~ "Ninguno",
      all_of(valores_produ_desa) ~ "Productividad y Desarrollo",
      all_of(valores_guia_emp) ~ "Guías",
      all_of(valores_emp_produ) ~ "Emprendimiento para la productividad",
      all_of(valores_conta1) ~ "Contabilidad 1",
      all_of(valores_agropecuaria) ~ "Agropecuaria",
      .default = emp_libros_primero
    )
  )

## volver a ver errores

valores_emp_libros_primero <- df_dyd |>
  filter(emp_uso_libros == "Sí") |>
  filter(emp_grados == "Primer grado") |>
  filter(!(is.na(emp_libros_primero))) |>
  select(emp_libros_primero) |>
  tabyl(emp_libros_primero)

# View(valores_emp_libros_primero)

# emp_libros_segundo ----

## ver errores

valores_emp_libros_segundo <- df_dyd |>
  filter(emp_uso_libros == "Sí") |>
  filter(emp_grados == "Segundo grado") |>
  filter(!(is.na(emp_libros_segundo))) |>
  select(emp_libros_segundo) |>
  tabyl(emp_libros_segundo)

dput(valores_emp_libros_segundo)

## Define los vectores ----

### Aprendo
valores_aprendo <- c(
  "Aprendo muntimaterias"
)

### cnb
valores_cnb_emp <- c(
  "CnB"
)

### productividad y desarrollo
valores_produ_desa2 <- c(
  "Codigo comercio, productividad y desarrollo indegua"
)

### el emprendedor
valores_emprendedor <- c(
  "El emprendedor"
)

### emprendimiento 2
valores_emp2 <- c(
  "Emprendimiento 2"
)

### emprendimiento para jóvenes
valores_emp_jovenes <- c(
  "Emprendimiento para jóvenes. Contabilidad para Segundo Básico."
)

### contabilidad 
valores_contabilidad <- c(
  "Emprendimiento para jóvenes. Contabilidad para Segundo Básico."
)

### guia de emprendimiento para la productividad
valores_guia_emp2 <- c(
  "Guía de Emprendimiento y Planificador", 
  "Guias", 
  "Guía de emprendimiento para la productividad"
)

### enciclopedia de la educacion
valores_enciclopedia_edu <- c(
  "Enciclopedia de la Educación"
)

### no especifica
valores_noespecifica_emp2 <- c(
  "Editora Educativa, Manual de la Educación",
  "Google", "Páginas web", "Santillana", "Santillana y Susaeta"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    emp_libros_segundo = case_match(
      emp_libros_segundo,
      all_of(valores_noespecifica_emp2) ~ "No especifica",
      all_of(valores_enciclopedia_edu) ~ "Enciclopedia de la Educación",
      all_of(valores_guia_emp2) ~ "Guía de emprendimiento para la productividad",
      all_of(valores_contabilidad) ~ "Contabilidad para 2do básico",
      all_of(valores_emp_jovenes) ~ "Emprendimiento para jóvenes",
      all_of(valores_emp2) ~ "Emprendimiento 2",
      all_of(valores_emprendedor) ~ "El emprendedor",
      all_of(valores_produ_desa2) ~ "Productividad y desarrollo",
      all_of(valores_cnb_emp) ~ "CNB",
      all_of(valores_aprendo) ~ "Aprendo",
      .default = emp_libros_segundo
    )
  )

## volver a ver errores

valores_emp_libros_segundo <- df_dyd |>
  filter(emp_uso_libros == "Sí") |>
  filter(emp_grados == "Segundo grado") |>
  filter(!(is.na(emp_libros_segundo))) |>
  select(emp_libros_segundo) |>
  tabyl(emp_libros_segundo)

# View(valores_emp_libros_segundo)

# emp_libros_tercero ----

## ver errores

valores_emp_libros_tercero <- df_dyd |>
  filter(emp_uso_libros == "Sí") |>
  filter(emp_grados == "Tercer grado") |>
  filter(!(is.na(emp_libros_tercero))) |>
  select(emp_libros_tercero) |>
  tabyl(emp_libros_tercero)

dput(valores_emp_libros_tercero)

## Define los vectores ----

### contabilidad 3
valores_conta3 <- c(
  "Contabilidad 3, Emprendimiento y productividad"
)

### emprendimiento y productividad
valores_emp_productividad <- c(
  "Contabilidad 3, Emprendimiento y productividad"
)

### emprendimiento para la productividad
valores_emp_produ3 <- c(
  "Emprendimiento para la productividad"
)

### emprendimiento y gestion
valores_emp_ges <- c(
  "Emprendimiento y gestión"
  
)

### planificador para el facilitador
valores_planificador_emp <- c(
  "Planificador docente"
)

### no especifica
valores_noespecifica_emp3 <- c(
  "Del Mineduc", "Libro digital de emprendimiento", "Libros digitales", 
   "Paginas web y otros", "Variados"
)

### ninguno
valores_ninguno_emp2 <- c(
  "No hay"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    emp_libros_tercero = case_match(
      emp_libros_tercero,
      all_of(valores_ninguno_emp2) ~ "Ninguno",
      all_of(valores_noespecifica_emp3) ~ "No especifica",
      all_of(valores_planificador_emp) ~ "Planificador para el facilitador",
      all_of(valores_emp_ges) ~ "Emprendimiento y Gestión",
      all_of(valores_emp_produ3) ~ "Emprendimiento para la productividad",
      all_of(valores_emp_productividad) ~ "Emprendimiento y Productividad",
      all_of(valores_conta3) ~ "Contabilidad 3",
      .default = emp_libros_tercero
    )
  )

## volver a ver errores

valores_emp_libros_tercero <- df_dyd |>
  filter(emp_uso_libros == "Sí") |>
  filter(emp_grados == "Tercer grado") |>
  filter(!(is.na(emp_libros_tercero))) |>
  select(emp_libros_tercero) |>
  tabyl(emp_libros_tercero)

# View(valores_emp_libros_tercero)

# emp_recursos_centro_educativo ----

## ver errores
valores_emp_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(emp_recursos_centro_educativo))) |>
  select(emp_recursos_centro_educativo) |>
  tabyl(emp_recursos_centro_educativo)

dput(valores_emp_recursos_centro_educativo)

## Define los vectores ----

### audiovisuales y didáctico
valores_audio_didactico_emp <- c(
  "Algunos materiales","Libros de texto y audiovisuales",
  "Audio visuales,", "Audios, TV, bocina, libros", "Audiovisual, compudora, impresora, libros.",
  "Audios, TV, bocina, libros","Libros, audivisuales",
  "Carteles", "Cartulinas, hojas, crayones, fotocopias, etc.", 
  "Cartulinas,marcadores tinta", "Dada", "Didáctico", "Equipo",
  "Fotocopias, hojas de trabajo, material audiovisual", 
  "Material Didáctico", "Material didáctica", "Material didáctico", 
  "Material didáctico, instalaciones", "Material didáctico, instalaciónes", 
  "Material impreso", "Materiales didácticos", "Materiales impresos", 
  "Materiales o guias", "Materiales para el curso",
  "taller y materiales didacticos", 
  "Reproducción de materiales, talleres", "Revistas, libros", 
  "Revistas, videos","material didactico y equipo", "material y equipo", 
  "Valija Didáctica", "Valija didactica", "Valija didáctica", 
  "Viaja didactica", "Visual, material didactico", "Visuales y físicos (libros)", 
  "RED WIFI, EQUIPO AUDIO VISUAL, COPIAS, LIBROS DE TEXTO, HUERTO ESCOLAR.", 
  "Recursos Audiovisuales", "Recursos de actividades prácticas", 
  "Recursos didacticos", "Recursos didáctico", "Red de internet, valija didáctica, aula", 
  "Planes de negocio, fichas de trabajo, tarjetas de brain storming, monopoly", 
  "EQUIPO, ELECTRICIDAD, INSTALACIONES, MOBILIARIO, ALGUNAS VECES MATERIAL DIDADCTICO."
)

### cnb
valores_cnb_emp2 <- c(
  "CNB", "Cnb",
  "CNB de Guatemala y libros digitales", "CNB guías de aprendizaje", 
  "El CNB y Malla curricular"
)

### espacio educativo
valores_espacio_emp <- c(
  "Ambiente adecuado","Aula pura, pizarrón,etc",
  "Aula pura.", "Aula, cañonera, recurso humano.", 
  "Cocina, estufas, tratos etc.", "Cocina, estufas, utensilios de cocina, etc", 
  "Cocina, leña, impresora, hojas de papel.",
  "RED WIFI, EQUIPO AUDIO VISUAL, COPIAS, LIBROS DE TEXTO, HUERTO ESCOLAR.", 
  "Libros de texto e instalaciones físicas", 
  "Laboratorio y equipo de corte y confección, cañonera, computadora", 
  "EQUIPO, ELECTRICIDAD, INSTALACIONES, MOBILIARIO, ALGUNAS VECES MATERIAL DIDADCTICO.", 
  "El taller de artes y diversas herramientas de uso particular",
  "Salón de clases apropiado, escritorios", 
  "Salón de clases, pizarra",
  "instalaciones, pizarra, cañonera, marcadores", "instalaciones, pizarra, marcadores", 
  "cuento con salón, tenazas, secadoras, planchas, tijeras, pelucas, otros.", 
  "Taller de corte y confección", "Taller físico y libros del CNB", 
  "Talleres y equipo", "Talleres y equipo.", 
  "Laboratorio con equipo y mobiliario de cocina", "Laboratorio de cocina, utensilios y equipo de cocina y repostería", 
  "Impresiones, área de realización de actividades planificadas"
)

### equipo e insumo agricola
valores_agricola <- c(
  "HERRAMIENTA, EQUIPO E INSUMOS AGRICOLAS", 
  "Heramientas, equipo e insumos agricolas.","Herramientas, equipo e insumos agricolas.",
  "Herramienta, equipo e insumos agricolas.","Terreno, herramientas",
  "Implementos de Horticultura",
  "herramienta, equipo e insumos agricolas", "herramienta, equipo e insumos agricolas."
)

### equipo y material de corte y confeccion
valores_confeccion <- c(
  "Computadora, canonera, maquinas de coser y equipo básico de corte y confección", 
  "Computadora, cañonera, herramientas, maquinaria, equipo, área de práctica.",
  "EQUIPO, MAQUINARIA, REGLAS, CINTAS METRICAS, PIZARRA, ELECTRICIDAD, PLANCHAS, PLANCHADOR, MESAS, TIJERAS MAQUINAS DOMESTICA Y OVELOCK, PROYECTOR, COMPUTADORA.", 
  "Equipo necesario para Corte y Confección",
  "Maquinas de coser. Y el equipo minimo indispensable.",
  "Laboratorio y equipo de corte y confección, cañonera, computadora"
)

### equipo y material de estilismo
valores_estilismo <- c(
  "cuento con salón, tenazas, secadoras, planchas, tijeras, pelucas, otros."
  
)

### equipo de electricidad
valores_electrico <- c(
  "herramientas para la electricidad, materiales cajas focos apagadores cable",
  "herramienta equipo electrico y maquinaria industrial"
)

### equipo y herramientas de taller
valores_taller <- c(
  "Cañonera, máquinas de soldar, sierras, prensas de banco, botas, gabachas de cuero, caretas electrónicas, guantes de cuero, barrenos, amolador.",
  "EQUIPO, MAQUINARIA, REGLAS, CINTAS METRICAS, PIZARRA, ELECTRICIDAD, PLANCHAS, PLANCHADOR, MESAS, TIJERAS MAQUINAS DOMESTICA Y OVELOCK, PROYECTOR, COMPUTADORA.", 
  "El taller de artes y diversas herramientas de uso particular", 
  "Galpón y herramientas manuales y de motor", 
  "herramienta equipo electrico y maquinaria industrial",
  "Herramientas", "Herramientas en talleres y proyectores", "Herramientas y máquinaria", 
  "Herramienta electrica, manual, caretas electonicas televisor maquinas de soldar pulidoras", 
  "Herramienta y Equipo", "Herramienta y equipo", "maquinaria", 
  "MAQUINARIA Y HERRAMIENTAS", "Maquinaria, herramientas y equipo."
)

### folletos
valores_folletos_emp <- c(
  "Folleto", "Folletos", "Folletos impresos",
  "Libros, internet, folletos","folletos", 
  "folletos, libros computadora"
)

### guia de aprendizaje
valores_guia_emp3 <- c(
  "GUIA DE APRENDIZAJE",
  "Guia", "Guias", "Guías", "Guías de aprendizaje elaboradas en el centro educativo", 
  "Guías docentes", "Guías docentes  e impresiones"
  
)

### instrumentos de dibujo
valores_dibujo_emp <- c(
  "Juego de reglas geométricas, proyector."
)

### libros
valores_libros_emp <- c(
  "1 libro","textos y enlaces",
  "Audios, TV, bocina, libros", "Audiovisual, compudora, impresora, libros.",
  "Audios, TV, bocina, libros","Textos, pizarrón, marcadores,", 
  "Biblioteca de aula con libros actualizados para investigación.",
  "COMPUTADORA, CAÑONERA, IMPRESORA, LIBRONTES S DE DIFEREAUTORES", 
  "Emprendimiento para la Productividad",
  "LIBRO DE TEXTO", "Los libros","Un libro de Santillana de emprendimiento", "Un libro de emprendimiento", 
  "Productividad y desarrollo", "Santillana", "Si, tengo una biblioteca para elegir.",
  "Libro", "Libro (impreso)", "Libro de Contabilidad", "libro",
  "Libro de texto", "Libro de texto del maestro", "Libro digital", 
  "Libros", "Libros , marcadores", "Libros de Contabilidad", "Libros de actividades, libros de recetas", 
  "Libros de emprendimiento de los 3 grados", "Libros de iger", 
  "Libros de texto", "Libros de texto e instalaciones físicas", 
  "Libros de texto y audiovisuales", "Libros de texto, televisión y tablet.", 
  "Libros de textos sobre diferentes formas de emprender", "Libros de textos, hojas de trabajo,", 
  "Libros digitales", "Libros e impresiones", "Libros folletos Papel.maquinas", 
  "Libros físicos,  investigaciones en Internet, PDF.", "Libros pues se cuenta con biblioteca, utensilios de cocina y herramientas manuales en las diferentes áreas", 
  "Libros y cnb", "Libros y material de apoyo", "Libros y materiales", 
  "Libros,  materiales digitales", "Libros, audivisuales", "Libros, copias, internet, laptop.", 
  "Libros, impresora", "Libros, internet, folletos", "Libros, pizarra, marcadores, utensilios de cocina, herramientas manuales de las diferentes áreas", 
  "LIBRO YO DECIDO", "LIBROS", "LIBROS DE SANTILLANA", "LIBROS Y FOLLETOS", 
  "LIBROS, INTERNET PARA DETERMINADAS INVESTIGACIONES, MATERIALES DE APOYO PARA DIFERENTES PROYECTOS"
)


### mobiliario
valores_mobiliario_emp <- c(
  "Aula pura, pizarrón,etc","Computadora, impresora y pizarrón", 
  "Conos y escaleras", "Impresora, cañonera y pizarra", 
  "Pizarra, marcadores, almohadillas, copias, impresiones, tinta para marcadores", 
  "Pizarras, cátedras,  CNB Digital", "Pizarrón", "Pizarrón y evaluaciones finales", 
  "Pizarrón,marcadores,lápiz,almohadilla,tinta para marcadores,",
  "Salón de clases apropiado, escritorios", 
  "Salón de clases, pizarra",
  "instalaciones, pizarra, cañonera, marcadores", "instalaciones, pizarra, marcadores", 
  "EQUIPO, ELECTRICIDAD, INSTALACIONES, MOBILIARIO, ALGUNAS VECES MATERIAL DIDADCTICO."
)


### ninguno
valores_ninguno_emp3 <- c(
  "N/A", "NINGUNO", "Propios", "Temas de investigación", 
  "NO APLICA", "Nada", "Ninguna", "Ninguno", "Ninguno solo, la malla curricular", 
  "Ninguno,", "Ninguno, utilizo la tecnología.", "Ninguno, yo los busco todos", 
  "Ninguno. Se utiliza la tecnología y los materiales descargados en páginas web de confianza", 
  "Ninguno. Se utilizan recursos descargados de páginas de instituciones del Estado confiables, de autores nacionales relacionados con el tema.", 
  "Ninguno. Se utilizan recursos tecnológicos proporcionados por ka web.", 
  "Ningún", "Ningún material", "Ningún material o recurso", 
  "Yo misma imprimí mis materiales", "0", "experiencia",
  "nada", "ninguno", 
  "no hay ningun texto para impartir el curso. aparte fe tv que estan en cada aula para relizar presentaciones. se invistiga de acuerdo al contexto del centro educativo", 
  "Ningúno", "No", "No existe recursos, utilizo la tecnología.", 
  "No existen recursos", "No hay", "No imparto"
)


### papeleria y manualidades
valores_papeleria_emp <- c(
  "Algunas copias, marcadores, hojas, engraapadora.",
  "Cartulinas, hojas, crayones, fotocopias, etc.", 
  "Cartulinas,marcadores tinta","internet, marcadores, almohadilla de pizarra",
  "Copias", "Copias y folletos","Utensilios de cocina, pegamento, hojas",
  "Computadora, cañonera, marcadores, lapiceros, hojas",
  "Fotocopias", "Textos, pizarrón, marcadores,", 
  "marcadores, borrador hojas", "marcadores, hojas, masking, tinta, almohadilla.", 
  "Marcadores","Pistola de silicona, tijeras, alicates, extensiones eléctricas etc.",  
  "Marcadores ,Pizarrón y almohadilla", "Marcadores y cartulinas", 
  "Marcadores, computadoras, pizaron", "Marcadores, tinta y hojas", 
  "Impresiones", "Impresiones, área de realización de actividades planificadas",
  "Hojas bond", "Hojas, lapiceros, marcadores",
  "Fotocopias y hojas de trabajo", "Fotocopias, hojas de trabajo, material audiovisual"
)

### Planificador
valores_planificador_emp2 <- c(
  "Planificador de Matemáticas de Segundo Básico", 
  "Planificador docente"
  
)

### recursos digitales
valores_digitales_emp <- c(
  "Celular, internet y libros digitales","digitales",
  "Libros,  materiales digitales","Páginas web", 
  "Paginas web", "Presentación Power point, canva etc.",
  "Digital", "Digitales","Investigacion", "Investigación", 
  "Investigación hojas de trabajo ilustraciones"
)

### sin espescificar
valores_sinespecificar_emp <- c(
  "COMCESP", "Carpinterías, restaurantes", 
  "Fisico", "Insumos","equipos y acessorios", 
  "Lo necesario", "Lo que este al alcance", "Los existentes en la comunidad y en el centro educativo", 
  "Todo"
)

### tecnologia
valores_tecnologia_emp <- c(
  "Audios, TV, bocina, libros", "Audiovisual, compudora, impresora, libros.",
  "Audios, TV, bocina, libros", "Internet", "Internet y folletos", 
  "Aula, cañonera, recurso humano.",
  "COMPUTADORA, CAÑONERA, IMPRESORA, LIBRONTES S DE DIFEREAUTORES",
  "Cajoneras y pantallas digitales",
  "Proyector", "Proyector, equipo para decoración.", 
  "Proyector, pizarrón , valija didáctica","Un taller equipado,cañonera",
  "Cañonera", "Cañonera, Computadora y Bocinas", 
  "Cañonera, máquinas de soldar, sierras, prensas de banco, botas, gabachas de cuero, caretas electrónicas, guantes de cuero, barrenos, amolador.", 
  "Celular, internet y libros digitales",
  "Centro de computación", "Libros de texto, televisión y tablet.", 
  "Cocina, leña, impresora, hojas de papel.", "Computadora", "Computadora para investigar temas", 
  "Computadora y cañonera", "Computadora, canonera, maquinas de coser y equipo básico de corte y confección", 
  "Computadora, cañonera, herramientas, maquinaria, equipo, área de práctica.", 
  "Computadora, cañonera, marcadores, lapiceros, hojas", "Computadora, impresora y pizarrón", 
  "Computadora, impresora y pizarrón", 
  "EQUIPO, ELECTRICIDAD, INSTALACIONES, MOBILIARIO, ALGUNAS VECES MATERIAL DIDADCTICO.", 
  "EQUIPO, MAQUINARIA, REGLAS, CINTAS METRICAS, PIZARRA, ELECTRICIDAD, PLANCHAS, PLANCHADOR, MESAS, TIJERAS MAQUINAS DOMESTICA Y OVELOCK, PROYECTOR, COMPUTADORA.", 
  "Impresora, cañonera y pizarra", 
  "Laptop y cañonera.","Libros, copias, internet, laptop.",
  "Libros, impresora",
  "Tecnología", "Tecnología y libros, páginas web", 
  "Tecnológicos y libros de texto", "Televisor",
  "Pantalla", "Pantalla, Impresora", 
  "Laboratorio y equipo de corte y confección, cañonera, computadora", 
  "Juego de reglas geométricas, proyector."
)

### equipo y electrodomesticos de cocina
valores_utensilio <- c(
  "Algunos utensilios de cocina",
  "Cocina, estufas, tratos etc.", 
  "Utensilios de cocina, pegamento, hojas", 
  "Cocina, estufas, utensilios de cocina, etc", 
  "Equipo de cocina", "Equipo y herramientas de cocina y repostería",
  "Equipo y suministros", "Estufas. Hornos", "Filtro de agua", 
  "Laboratorio con equipo y mobiliario de cocina", "Laboratorio de cocina, utensilios y equipo de cocina y repostería", 
  "Libros pues se cuenta con biblioteca, utensilios de cocina y herramientas manuales en las diferentes áreas", 
  "Libros, pizarra, marcadores, utensilios de cocina, herramientas manuales de las diferentes áreas"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    emp_recursos_centro_educativo = case_match(
      emp_recursos_centro_educativo,
      all_of(valores_utensilio) ~ "Equipo y electrodomésticos de cocina",
      all_of(valores_tecnologia_emp) ~ "Equipo tecnológico",
      all_of(valores_sinespecificar_emp) ~ "Sin especificar el recurso",
      all_of(valores_digitales_emp) ~ "Recursos digitales",
      all_of(valores_planificador_emp2) ~ "Planificador para el facilitador",
      all_of(valores_papeleria_emp) ~ "Material de papeleria y manualidades",
      all_of(valores_ninguno_emp3) ~ "Ninguno",
      all_of(valores_mobiliario_emp) ~ "Mobiliario",
      all_of(valores_libros_emp) ~ "Libros",
      all_of(valores_dibujo_emp) ~ "Instrumentos de de dibujo",
      all_of(valores_guia_emp3) ~ "Guía de aprendizaje",
      all_of(valores_folletos_emp) ~ "Folletos",
      all_of(valores_taller) ~ "Equipo y herramientas de taller",
      all_of(valores_electrico) ~ "Equipo de electricidad",
      all_of(valores_estilismo) ~ "Equipo y material de estilismo",
      all_of(valores_confeccion) ~ "Equipo y material de corte y confección",
      all_of(valores_agricola) ~ "Equipo y material agrícola",
      all_of(valores_espacio_emp) ~ "Espacio educativo",
      all_of(valores_cnb_emp2) ~ "CNB",
      all_of(valores_audio_didactico_emp) ~ "Material audiovisual y didáctico",
      .default = emp_recursos_centro_educativo
    )
  )

## volver a ver errores
valores_emp_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(emp_recursos_centro_educativo))) |>
  select(emp_recursos_centro_educativo) |>
  tabyl(emp_recursos_centro_educativo)

#View(valores_emp_recursos_centro_educativo)

# emp_recursos_propios ----

## ver errores

valores_emp_recursos_propios <- df_dyd |>
  filter(!(is.na(emp_recursos_propios))) |>
  select(emp_recursos_propios) |>
  tabyl(emp_recursos_propios)

dput(valores_emp_recursos_propios)

## Define los vectores ----

### audiovisual y didáctico
valores_audio_didactico_emp2 <- c(
  "Afiches", "Audiovisual", "Ayuda audiovisual",
  "Cartel, marcadores, libros", "Carteles, afiches, presentaciones", 
  "Carteles, láminas y presentaciones", "Carteles, recortes, fotocopias", 
  "Computadora hojas, videos materiales didáctico", 
  "Computadora, hojas de trabajo libros, videos educativos", 
  "Computadora, impresiones, carteles",
  "Valija Didáctica", "libros, equipo, internet, material didáctico, manuales",
  "Video", "Videos", "Videos folletos etc", "Videos,  investigaciones,  cartulina entre otros.", 
  "Visuales, revistas o investigaciones particulares",
  "Impresiones, libros, material didáctico",
  "Escrito, humano, folletos de trabajo", "videos y fotocopias",
  "Tableta, material didáctico, libros, capacitación por cuenta propia...",
  "Folletos, marcadores, cartulinas etc", 
  "Recursos didacticos.", "TECNOLOGIÁ, MATERIAL DIDACTICO, LIBRO, FOLLETOS.", 
  "Recursos didácticos, herramientas y otros",
  "Libros, material didáctico", "Materiales didácticos",
  "Material Impresos Tecnología", "Material audiovisual", "Material de apoyo, dinámicas", 
  "Material de reciclaje", "Material didactico", "Material didactico,pagina web", 
  "Material didáctico", "Material escrito y carteles", "Material impreso, planificacion del curso, recetario  otros", 
  "Material impreso, visual y creativo", "Material reciclable, material impreso, gráficas y diversos materiales para realizar proyectos", 
  "Material reciclado", "Materiales con tutoriales", "Materiales de internet", 
  "Materiales de reciclaje","Páginas web,libros,material didactico", 
  "Libros, material didáctico, tecnología, páginas web", "Libros, material impreso", 
  "Libros y materiales", "Libros y materiales didacticos", 
  "Libros de texto, impresora,  computadora,  papel tinta y juegos creativos creados y elaborados por mi persona", 
  "Libro de texto, material didactico,malla curricular", "Libros , afiches paguinas wuf",
  "Invesgacion de los temas y material didáctico",
  "Folletos, material didactico, computadora",
  "Ejercicios", "Elaboración de folletos","Equipo ,ingrdientes,folletos", 
  "Diapositivas, diagramas, etc", "Didáctico, equipo, wtc",
  "Documentos, revistas electrónicas, informes, videos ...", 
  "Internet y materiales dependiendo la actividad.",
  "Documentos, videos"
)

### cnb
valores_cnb_emp3 <- c(
  "CNB de Guatemala y libros digitales", 
  "CNB digital, folletos, internet", "CNB guías de aprendizaje 0tros libros  internet", 
  "CNB, laptop, hojas de trabajo, marcadores",
  "CnB digital. Folletos.","Recursos del CNB"
)

### equipo de electricidad
valores_electricidad_emp <- c(
  "MATERIALES ELECTRONICOS", "herramientas, apagadores , cable"
)

### maquinaria y herramientas de taller
valores_taller2 <- c(
  "maquina de soldar, caladora, compresor, herramientas electricas", 
  "maquina de soldar, herramientas eléctricas, serrucho, etc."
)
### equipo y material agricola
valores_agricola_emp2 <- c(
  "Herramientas para el trabajo de campo","mi computadora, y el cnb",
  "Internet,  semillas, herramientas,  otros.",
  "COMPUTADORA, REPRODUCTOR DE MÚSICA, USB, MATERIALES IMPRESOS, RECURSOS EN LÍNEA, AGENDA DE TRABAJO, HOJAS DE TRABAJO, COPIAS, REVISTAS, PERIÓDICOS, LIBROS DIGITALES, HUERTO ESCOLAR, UTENSILIOS DE COCINA, HERRAMIENTAS DE SIEMBRA, CALCULADORA, TELÉFONO, MEDIOS DIGITALES, HOJAS DE COLUMNAS, PAQUETES CONTABLES, HOJAS DE COLORES VARIADOS, SILICÓN, ORGANIZADORES GRÁFICOS, DRAMATIZACIONES, PRÁCTICA DE COCINA, ETC.", 
  "Semillas, pilones, fertilizante, plaguicidas."
)


### equipo y electrodomestico de cocina
valores_utensilio2 <- c(
  "COMPUTADORA, REPRODUCTOR DE MÚSICA, USB, MATERIALES IMPRESOS, RECURSOS EN LÍNEA, AGENDA DE TRABAJO, HOJAS DE TRABAJO, COPIAS, REVISTAS, PERIÓDICOS, LIBROS DIGITALES, HUERTO ESCOLAR, UTENSILIOS DE COCINA, HERRAMIENTAS DE SIEMBRA, CALCULADORA, TELÉFONO, MEDIOS DIGITALES, HOJAS DE COLUMNAS, PAQUETES CONTABLES, HOJAS DE COLORES VARIADOS, SILICÓN, ORGANIZADORES GRÁFICOS, DRAMATIZACIONES, PRÁCTICA DE COCINA, ETC.", 
  "Estufas, libros mesas. maquinas de coser",
  "Herramientas de cocinas","Toda mí cosina",
  "Mis libros, algunos utensilios de cocina", 
  "Libros, recetarios,", "Libros, utensilios de cocina", 
  "Hilo, tela, preparación algunos alimentos",
  "Recetario", "Recetarios",
  "Revistas, y algunos utencilios necesarios",
  "Libros de texto y recetarios de cocina y repostería"
)



### equipo y material de corte confección
valores_confeccion2 <- c(
  "Computadora, moldes, cortadores, agujas, tijeras, materiales para demostraciones", 
  "Estufas, libros mesas. maquinas de coser",
  "Libros, materiales como lana, pegamento, tijeras, hojas iris, listón",
  "Hilo, tela, preparación algunos alimentos",
  "Tela, hilos, agujas, patrones, cinta métrica, otros.", "Telas  hilos botones carretes agujas tijeras mis libros.", 
  "Telas reglas hilos tijeras otros",
  "telas. hilos reglas maquinas", 
  "MATERIALES, TELAS, AGUJAS, HILOS, ALFILERES, PAPEL BOND,  HERRAMIENTAS, TIZA, BASTIDORES, CUTER.", 
  "Madera, agujas","TELAS, ALFILERES, PAPEL, CUTER, TIZA, MARCADOR DE TELA, TIJERA ZICZAC, PATRONES, MUESTRAS DE PRENDAS, DESCOSEDORES, BOTONES, ZÍPER, PRENSATELAS ESPECIALES, IMAN, TORULACIONES, PAPEL ESPECIAL,", 
  "Mi propio equipo de corte y confección, maniquí",
  "Maquina libros.folletos Internet computadora pizarras tijeras metros Papel", 
  "Maquinas de coser, reglas,  yeso, tijeras, mesas, cintas métricas, etc.", 
  "Libros de texto, internet, telefono,telas, conos de hilo, tizas",
  "Libros de apoyo, tijeras, metros, pistolas de Silicon, agujas."
)

### equipo y material de estilismo
valores_estilismo <- c(
  "Lámparas luz led, materiales para uñas acrílicas, soft gel, gelish, rizadoras, otros." ,
  "Lámparas luz led, materiales para uñas acrílicas, soft gel, gelish, rizadoras, otros."
)

### folletos y guias
valores_guia_folleto_emp <- c(
  "Folleto", "Folletos", "Folletos  hojas de trabajo", "Folletos computadora libro", 
  "Folletos hojas de trabajo y manualidades", "Folletos y guía del CNB", 
  "Folletos y hojas de trabajo.", "Folletos y libros", "Folletos, hojas de trabajo", 
  "Folletos, hojas de trabajo y elaboración de manualidades", 
  "Internet  folleto",
  "Internet, folleros",
  "Folletos, hojas, marcadores...", "Folletos, internet", "Folletos, investigaciones, cuaderno , internet", 
  "Folletos, libros, hojas, marcadores", "Folletos, libros, internet", 
  "Folletos, libros, proyectos", "Folletos, marcadores, cartulinas etc", 
  "Folletos, material didactico, computadora", "Folletos, página w", 
  "Folletos, revistas", "Folletos, textos, guías de trabajo", 
  "Guias", "Guias de aprendizajes del CNB", "Guias, libros,  manuales,", 
  "Guías", "Guías de trabajo, material audiovisual, libro de texto, material reciclable", 
  "Guías docentes", "Guías, fotocopias, recortes", "Guías, libro", 
  "Guías, material visual"
)


### herramientas de carpinteria
valores_carpinteria_emp <- c(
  "herramientas martillos, arcos con sierra , electrodo,metro", 
  "Carpintería, restaurante", "Computadora,  sierra de corte",
  "Laptop, cañonera, herramientas de carpintería", "Laptop, folletos, reglas etc."
)

### libros
valores_libros_emp2 <- c(
  "Diferentes Libros de texto como guía e Internet", 
  "Diferentes libros", "Diferentes libros material de reciclaje  marcadores , recursos obtenidos  d la comunidad", 
  "Emprendimiento para la productividad","LIBRO TEXRO",
  "hojas de trabajo y textos de contabilidad", 
  "libros", "libros de texto",
  "Susaeta","Tableta, material didáctico, libros, capacitación por cuenta propia...",
  "Estufas, libros mesas. maquinas de coser","Mata curricular",
  "Folletos computadora libro", "LAPTOP, INTERNET Y LIBROS PROPIOS DE PRODUCTIVIDAD", 
  "Internet, libros", "Internet, libros, hojas de trabajos y materiales", 
  "Google y libros comprados", "Historia de emprendimiento", 
  "Libro", "Libro de emprendimiento del autor Gabriel Alfredo piloña, alineado conforme a la malla curricular del cnb", 
  "Libro de emprendimiento para la productividad", "Libro de texto", 
  "Libro de texto, material didactico,malla curricular", "Libro digitales", 
  "Libro físico", "Libro proyectos de Santillana e internet", 
  "Libro tiempo imágenes Internet", "Libro y copias", "Libro y hojas de columnas", 
  "Libro y otros", "Libro, Guías, hojas de trabajo.", "Libros", 
  "Libros , afiches paguinas wuf", "Libros De Texto", "Libros algunas herramientas", 
  "Libros calculadora", "Libros computadora", "Libros contenidos bajados de Internet material reciclado", 
  "Libros de Administración", "Libros de Contabilidad, paginas Web", 
  "Libros de Texto, Guias, páginas de internet.", "Libros de apoyo, tijeras, metros, pistolas de Silicon, agujas.", 
  "Libros de emprendiendo, revistas y CNB", "Libros de emprendimiento", 
  "Libros de productividad y desarrollo y marcadores", "Libros de texto", 
  "Libros de texto y artículos", "Libros de texto y herramientas propias", 
  "Libros de texto y recetarios de cocina y repostería", "Libros de texto,", 
  "Libros de texto, cuadernillos , lapiceros, marcadores, pliegos de papel bondreglas, hojas, materiales,", 
  "Libros de texto, herramientas de trabajo entre otras", "Libros de texto, herramientas, recetarios, redes móviles", 
  "Libros de texto, impresora,  computadora,  papel tinta y juegos creativos creados y elaborados por mi persona", 
  "Libros de texto, información en línea. Videos tutoriales", 
  "Mis libros, algunos utensilios de cocina", 
  "Textos y hojas de trabajo", "Textos,", "Web, libros",
  "Textos, Paginas de Internet, incluso Tesis", 
  "Libros de texto, internet", "Libros de texto, internet, telefono,telas, conos de hilo, tizas", 
  "Libros de texto, pizarrón, marcadores, salón de clases", "Libros de texto, páginas web", 
  "Libros de texto, páginas web, videos, tutoreales", "Libros de textos e impresiones", 
  "Libros del ministerio de educación", "Libros digitales", "Libros diversos, calculadora, fotocopias", 
  "Libros e Internet", "Libros e impresiones", "Libros e investigaciones", 
  "Libros propios", "Libros propios y folletos", "Libros video copias folletos", 
  "Libros y  hojas de trabajo", "Libros y computadora portátil", 
  "Libros y libros en pdf", "Libros y materia prima para la elaboración de productos", 
  "Libros y material digital", "Libros y materiales", "Libros y materiales didacticos", 
  "Libros y materiales digitales", "Libros y recursos tecnológicos", 
  "Libros, Internet", "Libros, Internet, IA", "Libros, Marcadores, Hojas", 
  "Libros, calculadora,", "Libros, calculadora, almohadillas, marcadores", 
  "Libros, carteles y Marcadores", "Libros, computadora, impresora etc.", 
  "Libros, computadora, maquetas", "Libros, computadora, moldes, herramientas propias para demostracionesias", 
  "Libros, documentos web, pdf", "Libros, folletos, internet", 
  "Libros, hojas de papel factura, calculadora, etc.", "Libros, hojas de papel factura, calculadora, etcétera.", 
  "Libros, internet", "Libros, libros web", "Libros, manuales de herrería, Internet.", 
  "Libros, marcadores", "Libros, marcadores de pastel, imágenes", 
  "Libros, marcadores, hojas tabulares para contabilidad", "Libros, material didáctico", 
  "Libros, material didáctico, tecnología, páginas web", "Libros, material impreso", 
  "Libros, materiales como lana, pegamento, tijeras, hojas iris, listón", 
  "Libros, otras herramientas,  vehículo", "Libros, páginas web, entre otros.", 
  "Libros, recetarios,", "Libros, tecnología", "Libros, utensilios de cocina", 
  "Libros,hojas, cuadernos", "Libros,temas investigados ,internet", 
  "Internet y codigo de comercio y otros libros."
)


### mobiliario
valores_mobiliario_emp2 <- c(
  "Estufas, libros mesas. maquinas de coser",
  "Hojas de trabajo, pizarra, marcador, almohadilla",
  "Pizarra", "Pizarrón hojas cuaderno"
)

### ninguno
valores_ninguno_emp4 <- c(
  "Experiencia vividas", "NINGUNO", "NO APLICA", "Ninguno",
  "experiencia","ninguno", "ninguno (todo es del establecimiento)", 
  "Recursos del lugar.","Temas de Internet","cada alumno lleva material",
  "RECURSOS DEL NATURALES DEL LUGAR", "RECURSOS NATURALES DEL LUGAR"
)


### no especificar
valores_sinespecificar_emp2 <- c(
  "Económicos","Equipo basico","Herramienta", 
  "Los que están al alcance", "Los que estén al alcance", "Los que necesite para llevar a cabo cada una de las Actividades.", 
  "Los recursos de la comunidad.","Prácticos y teoricos",
  "Recurso", "Recursos de actividades prácticas",
  "Recursos propios", "Todo", "Todos","Variados",
  "recursos del area de trabajo","1", "3", 
  "BAC EMPRENDIMIENTO", "Consulta de textos referentes a los contenidos y",
  "Productos", "Propios y adecuados"
)

### papeleria y manualidades
valores_papeleria_emp2 <- c(
  "CNB, laptop, hojas de trabajo, marcadores","Cuaderno, asistencia, folletos y lapicero", 
  "Computadora, libros, marcadores, impresiones, papel bond pliegos", 
  "Copias", "Copias y folletos", "Corrector",
  "electrodos, disco, pintura", 
  "Libros de texto, cuadernillos , lapiceros, marcadores, pliegos de papel bondreglas, hojas, materiales,", 
  "Hojas de trabajo, pizarra, marcador, almohadilla",
  "Fotocopias", "Fotocopias. Hojas de trabajo",
  "Maradores y temas investigados", "Marcadores", "Marcadores cuadernillos", 
  "Marcadores lapiceros pizaron", "Marcadores, hojas bom etc", 
  "Marcadores, libros de texto, celular, hojas, etc.",
  "Lapiceros, marcadores, resaltado res, cuadernos y otros", 
  "Hojas marcadores, lapiceros, folder,", "Hojas, cartulinas, marcadores", 
  "Hojas, marcadores y otros.", "Impresiones", "Impresiones y materiales", 
  "Impresiones, libros, material didáctico",
  "TIJERAS,MARCADORES, HOJAS, MATRIALES VARIADOS", 
  "Ropa de práctica, hojas de trabajo, gasolina.",
  "Teléfono móvil, internet, libros, marcadores, almohadilla, hojas de papel.", 
  "Papel cartulina impresiones","Pinceles, inflador para globos, tijeras etc.", 
  "Materiales impresos", "Materiales reciclado para manualidades", 
  "Hoja de trabajo", "Hojas", "Hojas ,juego geometrico,tempera, cartulinas,duro por y otros", 
  "Hojas de contenido", "Hojas de ejercicios, Resumen de contenidos", 
  "Hojas de tareas", "Hojas de trabajo", "Hojas de trabajo, pizarra, marcador, almohadilla", 
  "Hojas de tres columnas", "Hojas impresas", "Hojas marcadores entre otros", 
  "Hojas marcadores, lapiceros, folder,", "Hojas, cartulinas, marcadores", 
  "Hojas, marcadores y otros."
)

### planificador
valores_planificador_emp3 <- c(
  "Planificador docente", 
  "Planificadores"
  
)

### recursos digitales
valores_digitales_emp2 <- c(
  "Aprendizaje expositivo y en línea",
  "Contenidos de Google de productividad", 
  "Digital y fusico", "Digitales","textos y material digital",
  "Trello,  plantillas para pitch", "Utilizi la tecnología, recursos confiables de Web.", 
  "Investigacion", "Paginas web", "Recursos de la Web", 
  "Páginas de Internet", "Páginas web,libros,material didactico", 
  "Materiales descargados de páginas confiables de instituciones del Estado, autores guatemaltecos relacionados con el tema.", 
  "Los materiales descargados", "Los materiales descargados de páginas institucionales del Estado, de páginas web de autores nacionales confiables y relacionados con el tema.",
  "Libros y material digital","Libros y materiales digitales",
  "Libros de texto, páginas web", "Sitios web",
  "Libros de texto, páginas web, videos, tutoreales",
  "Investigacion  impreso e integracion", "Investigaciones", "Investigaciones del internet", 
  "Investigaciones propias y contextualizacion del contenido", 
  "Investigación propia", "Investigar en el internet",
  "Internet", "Internet  e impresión", 
  "Documentos descargados de internet, Guías elaboradas por el mismo docente."
)

### tecnologia
valores_tecnologia_emp2 <- c(
  "CNB, laptop, hojas de trabajo, marcadores", 
  "Cañonera", "Celular.","Pantalla, computadora", 
  "Compu con internet", "Libros, tecnología", 
  "computadora", "computadora, folletos", "mi computadora, y el cnb",
  "computadora, libros.", "computadora, marcadores, hojas bond, papelografos", 
  "VIDEOS, COMPUTADORA, CAÑONERA, PDF Y DOCUMENTOS WEB",
  "Pc, textos, impresiones","TECNOLOGIÁ, MATERIAL DIDACTICO, LIBRO, FOLLETOS.", 
  "Compudora, impresora, libros etc", "Computador", "Computadora", 
  "Computadora e impresoras", "Computadora hojas, videos materiales didáctico", 
  "Computadora,  sierra de corte", "Computadora, Internet", "Computadora, bocinas", 
  "Computadora, folletos", "Computadora, hojas de trabajo libros, videos educativos", 
  "Computadora, impresiones, carteles", "Computadora, impresora, entre otros", 
  "Computadora, libros", "Computadora, libros, marcadores, impresiones, papel bond pliegos", 
  "Computadora, metros, bayas, entre otros", "Computadora, moldes, cortadores, agujas, tijeras, materiales para demostraciones", 
  "Computadora, teléfono celular para proyectar, entre otros", 
  "COMPUTADORA, BOCINA,", "Equipo de computo",
  "Tableta, celular, internet", 
  "Teléfono", "Teléfono internet libros digitales", 
  "Tecnología", "Tecnología para investigación gación, hojas de trabajo", 
  "Tecnología para investigar.", "Tecnología y libros", "Tecnológicos y libros de texto", 
  "Tecnológicos, material didáctico", "Tecnológicos, materiales reciclados", 
  "Documentos personales, equipo de cómputo y celular.",
  "Dispositivos móviles,  bocina, uso tablet", 
  "Folletos computadora libro", "Proyector",
  "Libros y recursos tecnológicos", "TELEFONO CELULAR",
  "Libros y computadora portátil", 
  "Libros, computadora, impresora etc.", 
  "Libros, computadora, maquetas", "Libros, computadora, moldes, herramientas propias para demostracionesias", 
  "Laptop", "Laptop, cañonera, herramientas de carpintería", 
  "Laptop, folletos, reglas etc.", "Laptop, hojas de actividades, textos, otros", 
  "LAPTOP, INTERNET Y LIBROS PROPIOS DE PRODUCTIVIDAD", 
  "Herramientas y equipo de computación", 
  "Internet, computadora, Cañonera,fotocopias, impresiones,ampliaciones,",
  "COMPUTADORA, CAÑONERA, IMPRESORA, LIBRONTES S DE DIFEREAUTORES", 
  "COMPUTADORA, REPRODUCTOR DE MÚSICA, USB, MATERIALES IMPRESOS, RECURSOS EN LÍNEA, AGENDA DE TRABAJO, HOJAS DE TRABAJO, COPIAS, REVISTAS, PERIÓDICOS, LIBROS DIGITALES, HUERTO ESCOLAR, UTENSILIOS DE COCINA, HERRAMIENTAS DE SIEMBRA, CALCULADORA, TELÉFONO, MEDIOS DIGITALES, HOJAS DE COLUMNAS, PAQUETES CONTABLES, HOJAS DE COLORES VARIADOS, SILICÓN, ORGANIZADORES GRÁFICOS, DRAMATIZACIONES, PRÁCTICA DE COCINA, ETC."
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    emp_recursos_propios = case_match(
      emp_recursos_propios,
      all_of(valores_tecnologia_emp2) ~ "Equipo tecnológico",
      all_of(valores_digitales_emp2) ~ "Recursos digitales",
      all_of(valores_planificador_emp3) ~ "Planificador",
      all_of(valores_papeleria_emp2) ~ "Material de papelería y manualidades",
      all_of(valores_sinespecificar_emp2) ~ "No especifica el recurso",
      all_of(valores_ninguno_emp4) ~ "Ninguno",
      all_of(valores_mobiliario_emp2) ~ "Mobiliario",
      all_of(valores_libros_emp2) ~ "Libros",
      all_of(valores_carpinteria_emp) ~ "Herramientas de carpintería",
      all_of(valores_guia_folleto_emp) ~ "Guías y folletos",
      all_of(valores_confeccion2) ~ "Equipo y material de corte y confección",
      all_of(valores_utensilio2) ~ "Equipo y electrodomésticos de cocina",
      all_of(valores_agricola_emp2) ~ "Equipo y material agrícola",
      all_of(valores_taller2) ~ "Maquinaria y herramientas de taller",
      all_of(valores_electricidad_emp) ~ "Equipo de electricidad",
      all_of(valores_cnb_emp3) ~ "CNB",
      all_of(valores_audio_didactico_emp2) ~ "Material audiovisual y didáctico",
      all_of(valores_estilismo) ~ "Equipo y material de estilismo",
      .default = emp_recursos_propios
    )
  )

## volver a ver errores

valores_emp_recursos_propios <- df_dyd |>
  filter(!(is.na(emp_recursos_propios))) |>
  select(emp_recursos_propios) |>
  tabyl(emp_recursos_propios)

#View(valores_emp_recursos_propios)

#emp_otro_ultima_vez_libros_mineduc ----

## ver errores
valores_fisica_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(fisica_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(fisica_otro_ultima_vez_libros_mineduc ))) |>
  select(fisica_otro_ultima_vez_libros_mineduc) |>
  tabyl(fisica_otro_ultima_vez_libros_mineduc)

dput(valores_fisica_otro_ultima_vez_libros_mineduc)

## Define los vectores

### 2006
valores_emp_2006 <- c(
  "2006"
)

### 2007
valores_emp_2007 <- c(
  "2007"
)

### 2018
valores_emp_2018 <- c(
  "2018"
)

### 2019
valores_emp_2019 <- c(
  "2019"
)

### 2020
valores_emp_2020 <- c(
  "2020"
)

### no especifica
valores_sinespecificar_emp3 <- c(
  "No especifica", "No recuerda"
)

## no ha recibid
valores_nunca_emp <- c(
  "Nunca ha recibido"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    emp_otro_ultima_vez_libros_mineduc = case_match(
      emp_otro_ultima_vez_libros_mineduc,
      all_of(valores_nunca_emp) ~ "Nunca ha recibido",
      all_of(valores_sinespecificar_emp3) ~ "No especifica",
      all_of(valores_emp_2020) ~ "2020",
      all_of(valores_emp_2019) ~ "2019",
      all_of(valores_emp_2018) ~ "2018",
      all_of(valores_emp_2007) ~ "2007",
      all_of(valores_emp_2006) ~ "2006",
      .default = emp_otro_ultima_vez_libros_mineduc
    )
  )

## volver a ver errores

valores_fisica_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(fisica_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(fisica_otro_ultima_vez_libros_mineduc ))) |>
  select(fisica_otro_ultima_vez_libros_mineduc) |>
  tabyl(fisica_otro_ultima_vez_libros_mineduc)

View(valores_fisica_otro_ultima_vez_libros_mineduc)


# emp_tipo_materiales_recibidos ----

## ver errores

valores_emp_tipo_materiales_recibidos <- df_dyd |>
  filter(emp_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(emp_tipo_materiales_recibidos ))) |>
  select(emp_tipo_materiales_recibidos) |>
  tabyl(emp_tipo_materiales_recibidos)

dput(valores_emp_tipo_materiales_recibidos)

## Define los vectores ----

### guias
valores_guias_emp <- c(
  "Guías"
)

### papeleria

valores_papeleria_emp3 <- c(
  "Hojas, marcadores , engrapadora, perforador, grapas, regla tinta.", 
  "Marcadores, hojas, cartulinas, pegamentos, etc"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    emp_tipo_materiales_recibidos = case_match(
      emp_tipo_materiales_recibidos,
      all_of(valores_papeleria_emp3) ~ "Material de papelería",
      all_of(valores_guias_emp) ~ "Guías",
      .default = emp_tipo_materiales_recibidos
    )
  )

## volver a ver errores

valores_emp_tipo_materiales_recibidos <- df_dyd |>
  filter(emp_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(emp_tipo_materiales_recibidos ))) |>
  select(emp_tipo_materiales_recibidos) |>
  tabyl(emp_tipo_materiales_recibidos)

#View(valores_emp_tipo_materiales_recibidos)

# mate_libros_primero----

## ver errores

valores_mate_libros_primero <- df_dyd |>
  filter(mate_uso_libros == "Sí") |>
  filter(mate_grados == "Primer grado") |>
  filter(!(is.na(mate_libros_primero))) |>
  select(mate_libros_primero) |>
  tabyl(mate_libros_primero)

dput(valores_mate_libros_primero)

## Define los vectores ----

### algebra de baldor

valores_algebra <- c(
  "Algebra de Baldor", "libro Guatemática, MINEDUC, Algebra A. Baldor"
)

### guia de aprendizaje
valores_guia_aprendizaje_mate <- c(
  "GUIAS DE APRENDIZAJE, LIBROS EDITORIAL SANTILLANA", 
  "GUÍA DE APRENDIZAJE Y PLANIFICADOR",
  "Guía de Aprendizaje", "Guía de aprendizaje", "Guía de matemáticas", 
  "Guías de aprendizaje y Planificador docente", "Guías de aprendizaje y planificador", 
  "Guías y planificadores", 
  "Guia de aprendizaje", "Guia y planificador", 
  "Guia y planificador telesecundaria del curso",
  "Planificador del facilitador y Guia de Aprendizaje Matemáticas 1", 
  "Planificador y guia", "Planificador y guias"
  
)

### guatematica
valores_guatematica <- c(
  "Guatematica", "libro Guatemática, MINEDUC, Algebra A. Baldor"
)

### guia de matematica para 1ero grado
valores_guia_mate <- c(
  "Guia de Matematicas para primer grado", 
  "Guia de matemáticas","Guía  matemática  1"
)

### planificador
valores_planificador_mate <- c(
  "Guia de aprendizaje", "Guia y planificador", 
  "Guia y planificador telesecundaria del curso",
  "Guías de aprendizaje y Planificador docente", "Guías de aprendizaje y planificador", 
  "Guías y planificadores",
  "Planificador del facilitador y Guia de Aprendizaje Matemáticas 1", 
  "Planificador y guia", "Planificador y guias"
  )

### Matemáticas MINEDUC
valores_mate_mineduc <- c(
  "Libro de Guatemática. Mineduc", 
  "Libro de Matemática MINEDUC", "Libro del Ministerio de Educación", 
  "Mineduc"
)

### Matemáticas
valores_matematicas <- c(
  "Matematicas", "Matemáticas"
)

### matemáticas 1
valores_mate_1 <- c(
  "Matemáticas 1"
)

### sin especificar
valores_noespecifica_mate <- c(
  "Multimateria", "Santillana,  conceptos básicos"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    mate_libros_primero = case_match(
      mate_libros_primero,
      all_of(valores_noespecifica_mate) ~ "No especifica",
      all_of(valores_mate_1) ~ "Matemáticas 1",
      all_of(valores_matematicas) ~ "Matemáticas",
      all_of(valores_mate_mineduc) ~ "Matemáticas de MINEDUC",
      all_of(valores_planificador_mate) ~ "Planificador del facilitador",
      all_of(valores_guia_mate) ~ "Guía de Matemática para 1er. Grado",
      all_of(valores_guatematica) ~ "Guatemática",
      all_of(valores_guia_aprendizaje_mate) ~ "Guía de aprendizaje",
      all_of(valores_algebra) ~ "Álgebra de Baldor",
      .default = mate_libros_primero
    )
  )

## volver a ver errores

valores_mate_libros_primero <- df_dyd |>
  filter(mate_uso_libros == "Sí") |>
  filter(mate_grados == "Primer grado") |>
  filter(!(is.na(mate_libros_primero))) |>
  select(mate_libros_primero) |>
  tabyl(mate_libros_primero)

#View(valores_mate_libros_primero)

# mate_libros_segundo ----

## ver errores

valores_mate_libros_segundo <- df_dyd |>
  filter(mate_uso_libros == "Sí") |>
  filter(mate_grados == "Segundo grado") |>
  filter(!(is.na(mate_libros_segundo))) |>
  select(mate_libros_segundo) |>
  tabyl(mate_libros_segundo)

dput(valores_mate_libros_segundo)

## Define los vectores ----

### algebra de baldor
valores_algebra2 <- c(
  "Planifocador, guia de aprendizaje, Baldor, aritmetica .algebra.",
  "Álgebra de baldor y aritmética de baldor, entre otras",
  "Algebra de Baldor, Manual de la Educación, Pearson CONAMAT, Guía de aprendizaje"
)

### no especifica
valores_noespecifica_mate2 <- c(
  "Google", "Guía 2019"
)

### guatematica
valores_guatematica2 <- c(
  "Guatematica", "Guatematica, guiad. Conceptos"
)

### guia de aprendizaje
valores_guia_aprendizaje_mate2 <- c(
  "Guia de aprendizaje matemáticas",
  "Guía de Matemáticas y Planificador", "Libro de Matemáticas TS", 
  "Páginas web,planificados del facilitador,guía de aprendizaje", 
  "Guía de aprendizaje y Planificador del docente",
  "Planifocador, guia de aprendizaje, Baldor, aritmetica .algebra.", 
  "Guía y planificador Telesecundaria", "Guías de aprendizaje, Enciclopedias y otros libros"
)

### Planificador
valores_planificador_mate2 <- c(
  "Páginas web,planificados del facilitador,guía de aprendizaje", 
  "Guía de aprendizaje y Planificador del docente"
)

### guia matemáticas 2
valores_guia_mate2 <- c(
  "Guía Matematica 2","Guía de matemáticas 2"
)


### Matemáticas 2
valores_mate_2 <- c(
  "Matemática 2", "Matemáticas 2"
)

### matematicas 8
valores_mate_8 <- c(
  "Matemáticas para Segundo Básico. Matemáticas 8."
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    mate_libros_segundo = case_match(
      mate_libros_segundo,
      all_of(valores_mate_8) ~ "Matemáticas 8",
      all_of(valores_mate_2) ~ "Matemáticas 2",
      all_of(valores_guia_mate2) ~ "Guía de Matemáticas 2",
      all_of(valores_planificador_mate2) ~ "Planificador del facilitador",
      all_of(valores_guia_aprendizaje_mate2) ~ "Guía de aprendizaje",
      all_of(valores_guatematica2) ~ "Guatemática",
      all_of(valores_noespecifica_mate2) ~ "No especifica",
      all_of(valores_algebra2) ~ "Álgebra de Baldor",
     .default =  mate_libros_segundo
    )
  )

## volver a ver errores

valores_mate_libros_segundo <- df_dyd |>
  filter(mate_uso_libros == "Sí") |>
  filter(mate_grados == "Segundo grado") |>
  filter(!(is.na(mate_libros_segundo))) |>
  select(mate_libros_segundo) |>
  tabyl(mate_libros_segundo)

#View(valores_mate_libros_segundo)


# mate_libros_tercero ----

## ver errores

valores_mate_libros_tercero <- df_dyd |>
  filter(mate_uso_libros == "Sí") |>
  filter(mate_grados == "Tercer grado") |>
  filter(!(is.na(mate_libros_tercero))) |>
  select(mate_libros_tercero) |>
  tabyl(mate_libros_tercero)

dput(valores_mate_libros_tercero)


## Define los vectores ----

### algebra de baldor
valores_algebra3 <- c(
  "Algebra de Baldor y guía de aprendizaje de matemáticas", 
  "Algebra, Aritmética, Matemática",
  "Libro del ministerio de educación, Álgebra de Baldor, editorial Zatmaro y de Santillana", 
  "Libro de matemática del ministerio de educación, matemáticas de santilla, álgebra del Baldor"
)

### matematicas simplificada
valores_mate_simplificadas <- c(
  "Connamat"
)

### guía de aprendizaje
valores_guia_aprendizaje_mate3 <- c(
  "Guia",  "Guía de aprendizaje", "Guía del docente", 
  "Planificador y Guias de Aprendizaje del estudiante",
  "Guía de Aprendizje de matemáticas, Matemáticas III de Guatemáticas"
  
)

### guatematicas
valores_guatematica3 <- c(
  "Guía de Aprendizje de matemáticas, Matemáticas III de Guatemáticas"
)

### guia de matematica 3
valores_guia_mate3 <- c(
  "Guía de matemáticas 3"
)

### matematica mineduc
valores_mate_mineduc2 <- c(
  "Libro de matemática del ministerio de educación, matemáticas de santilla, álgebra del Baldor", 
  "Libro de matemáticas de mineduc"
)


### sin especificar
valores_noespecifica_mate3 <- c(
  "Libro de matemáticas para estudiantes, cada estudiantes tiene libros, aunque hay algunos que no alcanzan", 
  "Libros proporcionados por el ministerio",
  "Matematicas", "satillana", "matemáticas de nivel medio, Álgebra"
)

### matematica 3
valores_mate_3 <- c(
  "Matemática 3 Editorial Santillana Proyecto Caleidoscopio, Matemática Editorial Norma",
  "Matemática 3 Editorial Santillana Proyecto Caleidoscopio, Matemática Editorial Norma"
)

### matematica activa
valores_mate_activa <- c(
  "Matemática Activa", "Matemática activa"
)

### matematica de susaeta
valores_mate_susaeta <- c(
  "Matemática, Susaeta"
)

### planificador del facilitador
valores_planificador_mate3 <- c(
  "Planificador del facilitador, guia de aprendizaje", "Planificador docente y guías de aprendizajes", 
  "Planificador y Guias de Aprendizaje del estudiante",
  "Santillana Sulivan guías planificador"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    mate_libros_tercero = case_match(
      mate_libros_tercero,
      all_of(valores_planificador_mate3) ~ "Planificador del facilitador",
      all_of(valores_mate_susaeta) ~ "Matemática de Susaeta",
      all_of(valores_mate_activa) ~ "Matemática Activa",
      all_of(valores_noespecifica_mate3) ~ "No especifica",
      all_of(valores_mate_3) ~ "Matemática 3",
      all_of(valores_mate_mineduc2) ~ "Matemáticas del MINEDUC",
      all_of(valores_guia_mate3) ~ "Guía de Matemática 3",
      all_of(valores_guatematica3) ~ "Guatemática 3",
      all_of(valores_guia_aprendizaje_mate3) ~ "Guía de aprendizaje",
      all_of(valores_mate_simplificadas) ~ "Matemáticas Simplificadas",
      all_of(valores_algebra3) ~ "Álgebra de Baldor",
      .default = mate_libros_tercero
    )
  )

## volver a ver errores
valores_mate_libros_tercero <- df_dyd |>
  filter(mate_uso_libros == "Sí") |>
  filter(mate_grados == "Tercer grado") |>
  filter(!(is.na(mate_libros_tercero))) |>
  select(mate_libros_tercero) |>
  tabyl(mate_libros_tercero)

#View(valores_mate_libros_tercero)

# mate_recursos_centro_educativo ----

## ver errores

valores_mate_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(mate_recursos_centro_educativo))) |>
  select(mate_recursos_centro_educativo) |>
  tabyl(mate_recursos_centro_educativo)

dput(valores_mate_recursos_centro_educativo)

## Define los vectores ----

### audiovisuales y didacticos
valores_audio_didactico_mate <- c(
  "Audiovisuales, Libro de texto.", 
  "Balija didáctica y  libros de la biblioteca del establecimiento", 
  "Carteles numeros en cartolina",
  "Valija didáctica", "Valija didáctica, libros de texto y TICS.", 
  "Material Didáctico", "Material auditivo y visual", 
  "Material didáctico", "Material impreso", "Materiales didácticos", 
  "Libros, material didactico", "Libros, material impreso",
  "Libros de texto, juego geométrico, , Valija  didáctica.", 
  "Juegos de mesa, Libros, televisores, Cañonera, marcadores y pizarrones.",
  "Herramientas digitales, recursos didácticos",
  "Cañonera, folletos, actividades, audios, videos", 
  "Computadora, libros fichas", "Hojas, para las diferentes actividades",
  "Biblioteca, materiales didácticos (reglas, hojas, marcadores, etc), instalaciones óptimas"
)

### cnb
valores_cnb_mate <- c(
  "Cnb"
)

### espacio educativo
valores_espacio_mate <- c(
  "Aula"
)

### guias y folletos
valores_guia_folleto_mate <- c(
  "CNB PLANIFICADOR GUÍAS DE APRENDIZAJE",
  "Pdf, folletos proporcionados por la usac",
  "Folleto", "Folletos","Internet, libros y guías de estudio",
  "Guia", "Guia de aprendizaje matemáticas", "Guia y planificador", 
  "Guia y planificador telesecundaria", "Guias", "Guias de Matematicas", 
  "Guias y Planificadores", "Guía  y planificador", "Guía Programa , marcadores, hojas", 
  "Guía de Matemática", "Guía de aprendizaje", "Guía de aprendizaje,", 
  "Guía de docente de los tres grados en PDF", "Guía de matematica", 
  "Guía de matemáticas", "Guía de trabajo", "Guía del docente", 
  "Planificador  y guias",
  "Planificador del facilitador y guia de aprendizaje", "Planificador docente y guías de aprendizajes", 
  "Planificados del docente,guía de aprendizaje", "Planificados y guias", 
  "Guía del docente digital", "Guía docena, del Ministerio de Educación", 
  "Guía para el Docente de Matemáticas de tercero básico, versión digital", 
  "Guía para el docente de Matemática versión digital, solucionario de ejercicios del texto de mátemáticas para estudiantes de tercero básico versión impresa", 
  "Guía para el docente de matemáticas 3", "Guía para el docente matematica", 
  "Guía y planificador", "Guías", "Guías de aprendizaje y Planificador docente", 
  "Guías de aprendizaje y planificador", "Guías de aprendizaje, planificador y valija didáctica", 
  "Guías de texto y planificadores de Matemáticas", "Guías para el docente y libros de texto para los estudiantes", 
  "Guías y planificador", "Guías y planificadores",
  "GUIAS DE APRENDIZAJE", "GUÍA DE APRENDIZAJE Y PLANIFICADOR", 
  "GUÍA PARA EL DOCENTE DE MATEMÁTICAS DE TERCERO BÁSICO DIGITAL"
)

### juego de geometria
valores_geometria <- c(
  "Hojas, juego de geometría, libros y planificardor",
  "Hojas, marcadordores, juego geométrico", 
  "Regla, marcadores, archivo", "Regla, pizarrón, marcadores, libro.", 
  "Reglas",
  "Libros de texto, juego geométrico, , Valija  didáctica.", 
  "Hojas, juego goemetrico para pizarron, marcadores"
)

### libros
valores_libros_mate <- c(
  "1 libro", "libro", "libro de texto", 
  "libros, hojas de trabajo, web", 
  "Libro", "Libro Guatematica", 
  "Libro Mineduc años pasados", "Libro PDF de matemáticas del mineduc guatematica", 
  "Libro antiguo", "Libro de Matemáticas Mineduc y editorial educativa", 
  "Libro de Matemáticas y Álgebra", "Libro de Santillana, pero no está actualizado", 
  "Libro de Telesecundaria", "Libro de Texto", "Libro de guía y planificador", 
  "Libro de matemática", "Libro de matemática 2do basico", "Libro de matemáticas del MINEDUC", 
  "Libro de mineduc", "Libro de texto", "Libro de texto de Guatemática Nivel Medio", 
  "Libro de texto del Ministerio de Educación", "Libro de texto del Ministerio de Educación,  libros de texto de IGER, otros", 
  "Libro de texto del ministerio de educación", "Libro de texto para las alumnas que se han guardado desde la pandemia", 
  "Libro de texto, hojas, internet", "Libro de texto, marcadores y pizarrones", 
  "Libro de texto, marcadores, pizarra", "Libro de texto, salón de clases, pizarrón, escritorio.", 
  "Libro de texto. Marcadores. Pizarrón.", "Libro de textos", 
  "Libro del estudiante matemática", "Libro del gobierno", "Libro del ministerio de educación para alumnos", 
  "Libro guía y planificador", "Libro matemática básica", "Libro y tv", 
  "Libro, internet, folleto", "Libro, libro virtual, cañonera, caja con utiles escolares", 
  "Libro, marcadores de pizarrón, almohadilla, hojas", "Libro, valija didáctica", 
  "Libros", "Libros  virtuales. Fisicos u tecnologia", "Libros ,marcadores ,almohadilla,", 
  "Libros de MINEDUC, computadora", "Libros de Proyecto Saber", 
  "Libros de guatematica en pdf", "Libros de matemática del ministerio", 
  "Libros de matemáticas ñara cada estudiante", "Libros de santillana", 
  "Libros de texto", "Libros de texto de uso común y la valija didáctica", 
  "Libros de texto del estudiante", "Libros de texto y acceso a internet", 
  "LIBRO DE TEXTO", "LIBRO, PIZARRON, AULA", "LIBROS DE TEXTO", 
  "1 libro de MINEDUC","CONCEPTOS BASICOS", 
  "Libros, internet", "Libros, manipulativos, ipad, plataforma Classroom para docentes y para estudiantes", 
  "Libros, material de clase (hojas, marcadores, papel iris, silicona, etc.)", 
  "Libros de textos", "Libros, afiches",
  "Libros, pantallas", "Matematicas", "Connamat", "blador","Álgebra de Baldor",
  "Matemática Nivel de educación Media. Dado por el ministerio de educación", 
  "Matemática del MINEDUC.", "Matemáticas 2 nivel de educación media ciclo básico", 
  "Matemáticas Mineduc", "Libros de texto, computadora, cañonera",
  "Santillana", "TEXTO", "Baldor",
  "Texto", "Texto guia", "Textos", "Textos y guía", "UN LIBRO DE MATEMÁTICA", 
  "Un libro de matemáticas nada más", "Una biblioteca amplia de libros de texto", 
  "Unnlibro de matemática activa",
  "Libros, pantallas, pizarrones, proyectores.", "Libros, papelería, marcadores, pizarra, cañonera, computadora", 
  "Libros, proyectores", "Libros, tecnología y físicos", "Libros, videos y juegos lúdicos", 
  "Los del MINEDUC", "Los libros marcadores papelogragos cartulinas entre otros", 
  "Libros, Folletos, Recursos audiovisuales (bocina y televisión)", 
  "Libros, Juegos de Mesa, Cañonera, Televisores, Marcadores, Pizarrones y", 
  "Libros, computador, marcadores", "Libros, computadora", 
  "Libros e i ternet", "Libros e internet", "Libros hojas", "Libros impresos de matemáticas", 
  "Libros reutilizado", "Libros y balija didactica", "Libros y kit de experimento", 
  "Libros y marcadores", "Libros y materiales", "Libros y medios digitales", 
  "Libros y pizarrón", "Libros y planificador.", "Libros y videos", 
  "Hay como 10 libros disponibles de los enviados por el Mineduc desde hace como 5 años de cada grado.", 
  "Ejemplar de un libro de matemática","Guatemática"
)

### mobiliario
valores_mobiliario_mate <- c(
  "Pizarra y televisor","Pizarra", 
  "Pizarra y marcadores", 
  "Pizarrón", "Pizarrón, almohadilla  marcador", "Pizarrón, marcador, almohadilla", 
  "Pizarrón.","Solo pizarrón",
  "Pizarra, marcador, almohadilla, libros de texto del Mineduc.", 
  "Pizarra, marcador, hojas.", "Pizarra, marcadores, hojas de papel, fotocopiadora", 
  "Pizarra, marcadores, lapiceros, hojas de papel bond", "Pizarron, marcadores"
  
)

### ninguno
valores_ninguno_mate <- c(
  "Nada", "Ninguna", "Ninguno", "ninguno", "Ninguno son propios"
)

### papeleria
valores_papeleria_mate <- c(
  "Computadora, marcadores, libro, pizarra",
  "Hoja de ejercicios", "Libros de texto, marcadores, almohadillas, reglas",
  "Hojas, marcador","Impresos",
  "Marcadores", "Marcadores  y cartulinas", "Marcadores y cartulinas", 
  "Marcadores y pizarra", "Marcadores, almohadilla, hojas", "Marcadores, almohadilla, hojas, lápices, lapiceros", 
  "Marcadores, almohadilla, lapiceros, lápices, pegamento, hojas", 
  "Marcadores, carteles, televisión, bocinas, etc.", "Marcadores, cartulinas", 
  "Marcadores, hojas etc", "Marcadores, libros, pantallas, etc", 
  "Marcadores, pizarrón, hojas, guías locales.",
  "Hojas, libro de texto, internet",
  "hojas", "hojas y fotocopias","marcadores, almohadilla, hojas, fotocopias, regla, transportador, compás, escuadras, todos instrumentos para pizarra y escritorios,", 
  "Pizarra y marcadores", "Regla, marcadores, archivo", "Regla, pizarrón, marcadores, libro.", 
  "Pizarra, marcador, almohadilla, libros de texto del Mineduc.", 
  "Pizarra, marcador, hojas.", "Pizarra, marcadores, hojas de papel, fotocopiadora", 
  "Pizarra, marcadores, lapiceros, hojas de papel bond", "Pizarron, marcadores",
  "Papel, marcadores, pizarra, calculadora, cuaderno, lápices, tinta para marcador, etc.", 
  "Hojas bond", "Hojas de papel bond ,marcadores, pizarra, lapiceros, lápices", 
  "Hojas de trabajo y marcadores", "Hojas de trabajo, internet, apps de comunicación, juegos mentales", 
  "Hojas e impresiones", "Hojas, carteles y marcadores.", "Hojas, fotocopias, impresiones", 
  "Copias", "Cuadros de registro", "Fotocopias",
  "HOJAS DE PAPEL, MARCADORES, ALMOHADILLA"
)

### Planificador
valores_planificador_mate4 <- c(
  "Planificador", "Planificador  y guias", "Planificador de matemáticas 2", 
  "Planificador del facilitador y guia de aprendizaje", "Planificador docente y guías de aprendizajes", 
  "Planificados del docente,guía de aprendizaje", "Planificados y guias"
)

### recursos digitales
valores_digitales_mate <- c(
  "Digitales", 
  "Plataformas", "Página w, otros medios impresos", "Recursos de actividades prácticas", 
  "Pagina web con los libros", 
  "Pantallas, plataforma educativa y libros de textos"
)

### tecnológicos
valores_tecnologia_mate <- c(
  "Accesorios básicos, cañonera, pantalla, internet, otros", 
  "Android TV, Internet, libro","PIZARRA. CAÑONERA", 
  "Laptop, proyector e impresiones.","Retroproyector", "Internet",
  "Pantalla, Impresora", "Pantalla, pizarra", "Impresora, cañonera y pizarra",
  "Libros, Folletos, Recursos audiovisuales (bocina y televisión)", 
  "Libros, Juegos de Mesa, Cañonera, Televisores, Marcadores, Pizarrones y", 
  "Libros, computador, marcadores", "Libros, computadora", 
  "Juegos de mesa, Libros, televisores, Cañonera, marcadores y pizarrones.",
  "Cañonera", "Cañonera y libro guia",  "recursos tecnologicos y espacios, libros",
  "Utensilios básicos, pantalla, cañonera, internet, otros", 
  "Tecnología y televisiones", "Tecnológicos y libros de texto", 
  "Cañonera, folletos, actividades, audios, videos", "Computadora, impresora y pizarrón", 
  "Computadora, libros fichas", "Computadora, marcadores, libro, pizarra"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    mate_recursos_centro_educativo = case_match(
      mate_recursos_centro_educativo,
      all_of(valores_tecnologia_mate) ~ "Equipo tecnológico",
      all_of(valores_digitales_mate) ~ "Recursos digitales",
      all_of(valores_planificador_mate4) ~ "Planificador",
      all_of(valores_papeleria_mate) ~ "Material de papelería",
      all_of(valores_ninguno_mate) ~ "Ninguno",
      all_of(valores_mobiliario_mate) ~ "Mobiliario",
      all_of(valores_libros_mate) ~ "Libros",
      all_of(valores_geometria) ~ "Juego de geometría",
      all_of(valores_guia_folleto_mate) ~ "Guías y folletos",
      all_of(valores_espacio_mate) ~ "Espacio educativo",
      all_of(valores_cnb_mate) ~ "CNB",
      all_of(valores_audio_didactico_mate) ~ "Material audiovisual y didáctico",
      .default = mate_recursos_centro_educativo
    )
  )

## volver a ver errores

valores_mate_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(mate_recursos_centro_educativo))) |>
  select(mate_recursos_centro_educativo) |>
  tabyl(mate_recursos_centro_educativo)

#View(valores_mate_recursos_centro_educativo)

# mate_recursos_propios ----

## ver errores
valores_mate_recursos_propios <- df_dyd |>
  filter(!(is.na(mate_recursos_propios))) |>
  select(mate_recursos_propios) |>
  tabyl(mate_recursos_propios)

dput(valores_mate_recursos_propios)

## Define los vectores ----

### audiovisual y didactico
valores_audio_didactico_mate2 <- c(
  "ACTIVIDADES, ESTRATEGIAS, MATERIA DIDACTICO TRBJADO EN PADEP",
  "Audiovisuales", "Calculadora  y material didactica", "Calculadora y material didactica",
  "Carteles, computadora, teléfono, internet", "Carteles, computadoras, libros", 
  "Carteles, marcadores reglas grandes, compas,  etc", "Carteles. Materiales concretos", 
  "Computadora, materiales didácticos adicionales", "Computadora, recursos lúdicos, folletos", 
  "Cubos, fichas, tarjetas, Dados, dominios, libros, rompecabezas, entre otros.", 
  "Didacticos pizarrón", "Dinamicas", "Valija didáctica", 
  "Libros, copias, Internet, computadora, material manipulativo", 
  "Guías,tecnología y material didáctico.", "Material didáctico",
  "computadora, impresora, infografías, videos",
  "Libro, guías, hojas de trabajo, ajedrez. Tangra", 
  "Libro, videos, imágenes, material impreso",
  "Material impreso, juegos lúdicos", "Material impreso, material",
  "Material impreso, material  reciclado, teléfono inteligente",
  "Material impreso, material reciclado", 
  "Metro. Materiales concretos",
  "Materiales didáctico", "Materiales impresos",
  "Materiales impresos (libros, fotocopias), materiales reciclados,recursos audiovisuales (videos), juegos interactivos y análisis de datos. Recursos digitales y tradicionales como libros de texto, cuadernos y calculadoras. También se emplean juegos de mesa, materiales impresos como fichas y herramientas tecnológicas",
  "Materiales impresos (libros, fotocopias), materiales reciclados,recursos audiovisuales (videos), juegos interactivos y análisis de datos. Recursos digitales y tradicionales como libros de texto, cuadernos y calculadoras. También se emplean juegos de mesa, materiales impresos.",
  "Libros de Baldor, audiovisual.", "libro, material didáctico,",
  "MATERIALS REALIZADOS EN PADEP-MATEMATICAS,  SI COMO ESTRATEEGIAS DE APRENDIZAJE",
  "Libros y materiales digitales", "Libros y recursos didácticos", 
  "Libros, Internet, videos, simuladores, juegos, bloques, carteles, etcétera.", 
  "Libros, internet, material didactico", 
  "Juegos", "Juegos de mesa, libros, Computadora, fotocopias, Guías.",
  "Libros, material didactico para actividades ludicas.", 
  "Mis cuadernos mi Internet mis materiales etc", 
  "Páginas web,material didactico,planificador y guía de aprendizaje", 
  "Recursos interactivos, juegos educativos", 
  "Recursos de actividades prácticas", 
  "Tarjetas, Computadora, Dados, Domino, Monedas, Carteles, Libros de Cálculo, Libros de lectura sobre Matemática y Ciencias.", 
  "Concretos figuras geométricas", 
  "Impresos,graficas,material concreto concreto"
)


### calculadora
valores_calculadora <- c(
  "Calculadora  y material didactica", "Calculadora y material didactica",
  "Calculadora, computadora", "Calculadora, hola de trabajo", "Calculadora, lápices, marcadores y pizarra", 
  "Libros de texto, calculadora, celular, tablet,",
  "Libros, reglas, guías, planes, calculadora etc"
)

### cnb
valores_cnb_mate2 <- c(
  "CNB libros planificación", "Folletos y CNB","Recursos del CNB", 
  "CNB, libros", "CNB, marcador y libros"
)

### guias y folletos
valores_guia_folleto_mate2 <- c(
  "Folleto  de ejercicio  hoja de trabajo reglas  calculadora", 
  "Folleto y guía del CNB", "Folletos", "Folletos de repaso personalizado con IA, hojas de trabajo, fotocopias de algunas secciones de otros libros, Khan Academy, PruebaT, Kahoot, Classroom, Canva, Socrative, Quizzizetc.", 
  "Folletos y CNB", "Folletos,  hojas de trabajo y calculadora", 
  "Libros, cañoneras, guías metodológicas", 
  "Regla, libro, copias", "Reglas para pizarra y hojas de trabajo", 
  "Reglas, compas, marcadores carteles", "Reglas, compás, pegamento, calculadora", 
  "Folletos, hojas de trabajo impresas y guías de estudio", "Folletos, hojas de trabajo, calculadora, reglas, transportadores", 
  "Folletos, laptop, Álgebra de Baldor", "Folletos, libros virtuales",
  "Guia", "Guia de aprendizaje", "Guía de aprendizaje", "Guía para docentes de Matemática", 
  "Guía, material didáctico y uso de tecnología.", "Guías", 
  "Guías de aprendizajes", "Guías y material acorde", "Hoja de trabajo",  "Guías,tecnología y material didáctico."
)

### juego de geometria
valores_geometria2 <- c(
  "Libro de Álgebra de Baldor, materiales en pdf, regla, transportador", 
  "Libro y juego gometrico", "Libro, Reglas, entre otros", 
  "Estuche geometrico, material concreto, libros", 
  "Libro, folletos, calculador, tangram, algedrez, reglas", 
  "Maquetas, Juego de geometría y tablas","Pizarrón, juego de geometría, marcadores de pizarron",
  "Libros con diferentes actividade, juego de geometría","Compás",
  "Libros de texto, juegos de geometría, app de destreza matemática, calculadoras.", 
  "Libros de texto, reglas graduadas, escalas gráficas, tablas de conversión",
  "Libros y juego geometrico", "Libros, reglas, guías, planes, calculadora etc"
)

### libros
valores_libros_mate2 <- c(
  "ALGEBRA DE BALDOR, LIBRO DE MATEMÁTICAS DE EDUARDO SÁNCHEZ 3, GUÍAS Y HOJAS DE TRABAJO", 
  "1 libro", "Algebra de Baldor e internet", "Aritmética libros de álgebra",
  "Algebra, pre calculo de Barnett, libros de IGER. Matemática I, y II",
  "La biblia de la matemática de Baldor", "Santillana",
  "Baldor", "Matemática 1ro. Básico santillana", "Libros, pizrrones, marcadores", 
  "Mis libros, cuaderno de trabajo,  internet, computadora , marcadores pizarrón", 
  "Varios libros", "Videos,  libros,  investigaciones.", "baldor",
  "libro, guía,", "libros de Santillana, Álgebra de Baldor, Algebra Trigonometria y Geométria de Sowkowski, modelos matematicas y experimentos propios que se aplican en la realidad.", 
  "libros de texto, intertnet, otros", "libros, table", "otros libros", 
  "Álgebra de Baldor, libro de matemáticas de tercero básico de Eduardo Sánchez, guías y hojas de trabajo, crucigramas", 
  "Álgebra de Baldor, libro de matemáticas de tercero básico de Eduarso Sánchez, guías y hojas de trabajo", 
  "Álgebra de Baldor, material impreso", "Álgebra de gobran, álgebra de baldor, serie de matemática de Conamat, recursos digitales diversos", 
  "Área Curricular, pdf", "textos adicionales, hojas impresas", 
  "Libro", "Libro de Guatemática, internet, libro de santillana", 
  "Libro de Santillana", "Libro de Telesecundaria", "Libro de aritmética y álgebra y hojas de trabajo", 
  "Libro de guatemática", "Libro de texto", "Libro de texto y videos", 
  "Libro de texto, malla curricular", "Libro de Álgebra de Baldor, materiales en pdf, regla, transportador", 
  "Libro del estudiante, PDF y web", "Libro externo de apoyo, recursos de la Web y herramientas virtuales", 
  "Libro santillana", "Libro y juego gometrico", "Libro, Reglas, entre otros", 
  "Libro, folleto, computadora", "Libro, folletos, calculador, tangram, algedrez, reglas", 
  "Libro, guías, hojas de trabajo, ajedrez. Tangra", "Libro, hojas", 
  "Libro, videos, imágenes, material impreso", "Libro,pizarra, marcadores, etc.", 
  "Libro. Investigaciones. Hojas.", "Libros", "Libros (aritmética y algebra de Baldor)", 
  "Libros Santillana, álgebra de Baldor, otros textos", "Libros carteleles", 
  "Libros con diferentes actividade, juego de geometría", "Libros de Baldor, audiovisual.", 
  "Libros de IGER, libros de algebra, videos de prof. Jesús Grajeda Rosas", 
  "Libros de algebra de baldor, editoral educativa", "Libros de apoyo, hojas, marcadores", 
  "Libros de consulta", "Libros de ejercicios", "Libros de guatematica en digital, recursos en internet para ejercicios y operaciones como liveworksheet, khan academy", 
  "Libros de matemáticas, copias", "Libros de matemáticas, material realizado personalmente.", 
  "Libros de texto", "Libros de texto computadora", "Libros de texto de Ministerio de educación, libros de texto de IGER", 
  "Libros de texto de diferentes autores", "Libros de texto de matemáticas", 
  "Libros de texto diferentes autores", "Libros de texto varios, internet", 
  "Libros de texto y tutoriales y revistas tecnológica", "Libros de texto, calculadora, celular, tablet,", 
  "Libros de texto, folletos  fotocopias  calculadora celular", 
  "Libros de texto, hojas de trabajo", "Libros de texto, hojas de trabajo, recursos digitales, CNB impreso, etc.", 
  "Libros de texto, internet", "Libros de texto, internet, laboratorios Google entre otros", 
  "Libros de texto, juegos de geometría, app de destreza matemática, calculadoras.", 
  "Libros de texto, reglas graduadas, escalas gráficas, tablas de conversión", 
  "Libros de texto, tablet, celular, fotocopias, otros", "Libros de texto, tecnología, hojas de trabajo", 
  "Libros de textos", "Libros de textos d l área matemática, impresiones , internet", 
  "Libros de textos medios digitales.", "Libros de álgebra y aritmética", 
  "Libros digitales", "Libros digitales, guías y hojas de trabajo", 
  "Libros digitales.", "Libros diversos, fotocopias", "Libros e internet", 
  "Libros e investigación", "Libros en PDF y Hojas de Trabajo.", 
  "Libros extra, internet, computadora, celular", "Libros folletos videos", 
  "Libros marcador Pizarra entre oteos", "Libros marcadores hojas", 
  "Libros propios", "Libros propios, libros en PDF como CONAMAT, reglas geométricas", 
  "Libros propios.", "Libros y PDF", "Libros y apuntes", "Libros y hojas de trabajo", 
  "Libros y juego geometrico", "Libros y material en PDF", "Libros y materiales", 
  "Libros y materiales digitales", "Libros y recursos didácticos", 
  "Libros y videos", "Libros y videos de youtube", "Libros,", "Libros, Computadora, Guías de trabajo, Juegos de mesa.", 
  "Libros, Internet, videos, simuladores, juegos, bloques, carteles, etcétera.", 
  "Libros, cañoneras, guías metodológicas", "Libros, computadora, maletín...", 
  "Libros, copias, Internet, computadora, material manipulativo", 
  "Libros, cuadernos de generaciones pasadas, videos, archivos PDF con ejercicios prácticos, etc...", 
  "Libros, cuadernos otros", "Libros, documentos pdf, instrumentos prácticos", 
  "Libros, folletos y algunos materiales de trabajo (hojas o tarjetas)", 
  "Libros, folletos, hojas de trabajo.", "Libros, hojas de texto, pantalla, internet", 
  "Libros, hojas de trabajo.", "Libros, internet", "Libros, internet, material didactico", 
  "Libros, lapto, cañonera", "Libros, material didactico para actividades ludicas.", 
  "Libros, pizrrones, marcadores", "Libros, recursos digitales propios y de otros sitios educativos, material manipulativo fabricado por mi persona", 
  "Libros, reglas, guías, planes, calculadora etc", "Libros, telefono, marcadores, hojas entre otras", 
  "Libros, videos que aporten aprendizaje", 
  "Otras bibliográficas, papel, dispositivo", 
  "Otros libros", "Otros textos", 
  "Enciclopedia océano", "Enciclopedia, internet, folletos", 
  "Documentos de apoyo, libros, textos digitales", 
  "Documentos de universidades, Libros universitarios, libros de otras editoriales", 
  "Texto", "Textoa de otras editoriales", 
  "Textos, Folletos, guías de trabajo"
) 

### mobiliario
valores_mobiliario_mate2 <- c(
  "Pizarra , libros", "Pizarra, guías, planificadores,", "Pizzaron, marcadores, afiches e impresiones"
)


### ninguno
valores_ninguno_mate2 <- c(
  "Ninguna", "Ninguno","Teoria y practico", "Recursos de la comunidad"
)

### no especifica
valores_noespecifica_mate4 <- c(
  "Todos", "Económicos", "4"
)

### papeleria
valores_papeleria_mate2 <- c(
  "Actividades impresas, marcadores, hojas de trabajo", 
  "Aplicaciones, hojas impresas", 
  "Planificación, texto, internet", "Libro,pizarra, marcadores, etc.",
  "Plastilina, crayones, marcadores, hojas", 
  "Celular, hojas de trabajo y libros",
  "Papeles, tablas,  lapicero, hojas, marcadores",
  "Hojas", "Hojas , lapiceros, cuaderno", "Hojas bond, guías de trabajos, plan anuales y bimensuales.", 
  "Hojas de actividades", "Hojas de colores, marcadores, crayones, cañonera, compu", 
  "Hojas de cuadros, milimetradas,regla, transportador, tijera. etc", 
  "Hojas de ejercicios", "Hojas de trabajo", "Hojas de trabajo e impresiones.", 
  "Hojas de trabajo y ejercicios de elaboración propia", "Hojas de trabajo, ejercicios para realizar en clase, material audiovisual", 
  "Hojas de trabajo, laboratorios elaborados, documentos físicos y digitales", 
  "Hojas de trabajo, material didáctico", "Hojas de trabajo, regla, lápiz, borrador", 
  "Hojas impresas", "Hojas marcadores libros", "Hojas y fotocopias", 
  "Hojas y marcadores", "Hojas, colores, tijeras etc", "Hojas, copias", 
  "Hojas, internet, tinta de impresora, computadora.", "Hojas, marcadores, libros, juegos de reglas, cajonera, computadora", 
  "Hojas,cartulinas,marcadores",
  "cuaderno de contenidos. impresiones.", 
  "cuadernos, calculadora, lapiz, borradores", 
  "hojas de trabajo impresas", "hojas de trabajo, folletos, medios digitales", 
  "Impresiones", "Impresos","Cuaderno, celular, hojas impresas", 
  "Cuadernos libros y hojas de cuadricula", "Cuadernos y hojas de trabajo", 
  "Marcadores", "Marcadores y el cerebro",  "Material impreso",
  "Marcadores, almohadilla, lapiceros, sellos, tinta", "Marcadores, almohadilla, lápiz, lapiceros.", 
  "Marcadores, almohadillas, reglas, calculadora", "Marcadores, bocinas, materiales, actividad, almohadilla", 
  "Marcadores, carteles", "Marcadores, hojas, impresora, computadora, cajonera, libros adicionales, internet, celular, juegos matemáticos, entre otros.", 
  "Marcadores, libros, calculadora, lapiceros, lapices", "Marcadores, pizarrón y regla", 
  "Marcadores, regla, sellador, papelografos",
  "Hojas de colores, marcadores, crayones, cañonera, compu", 
  "Hojas, marcadores, libros, juegos de reglas, cajonera, computadora", 
  "Impresiones, hojas de trabajo, calculadora, computadora e impresiones", 
  "Impresiones, tinta, sellos, plataformas digitales", "Impresora, fotocopiadora,computadora", 
  "Lapicero, calculadora, impresora y computadora."
  
)

### recursos digitales
valores_digitales_mate2 <- c(
  "Consultas de internet y Algebra", 
  "Guatematicas en digital, recurso tomados del internet para ejercicios en Kahoot, quiziziz y liveworksheet.", 
  "INTERNET Y LIBROS ADICIONALES", 
  "Materiales impresos (libros, fotocopias), materiales reciclados,recursos audiovisuales (videos), juegos interactivos y análisis de datos. Recursos digitales y tradicionales como libros de texto, cuadernos y calculadoras. También se emplean juegos de mesa, materiales impresos.",
  "Impresos y digitales", "Internet", "Internet y hojas de trabajo", "Internet y libros personales", 
  "Investigaciones", "Investigaciones en el internet", "Investigaciones propios, reglas si es necesario, entre otros", 
  "Investigaciones, pizarrones marcadores de formica", "Investigó en otros libros e internet", 
  "LIBROS EN PDF Y HOJAS DE EJERCICIOS.",
  "Impresiones, tinta, sellos, plataformas digitales", 
  "Libro del estudiante, PDF y web", "Libro externo de apoyo, recursos de la Web y herramientas virtuales", 
  "Libros de texto, hojas de trabajo, recursos digitales, CNB impreso, etc.", 
  "Libros, recursos digitales propios y de otros sitios educativos, material manipulativo fabricado por mi persona", 
  "Paginas web.","Pdf, folletos de la usac, libros descargados", 
  "Plataformas digitales, tinta, impresiones", "Presentaciones especiales, simuladores.", 
  "Páginas de Internet, plataformas educativas, documentos de universidad", 
  "Páginas web", "Páginas web,material didactico,planificador y guía de aprendizaje", 
  "Web"
)
### tecnologia

valores_tecnologia_mate2 <- c(
  "CAÑONERA COMPUTADORA","Libro, folleto, computadora",
  "COMPUTADORA E IMPRESORA", 
  "Geoplano, internet, tablet, juegos para estimular el razonamiento lógico", 
  "Cañonera, computadora, bocina, calculadora, juego de geometría, libros.", 
  "Centro de computación", "Electrónicos",
  "Computadora", "Computadora calculadora y folletos", "Computadora e impresora", 
  "Computadora impresora cañonera marcadores", "Computadora y celular", 
  "Computadora y material sólido", "Computadora y otros", "Computadora, Internet", 
  "Computadora, Internet e impresora", "Computadora, bocinas", 
  "Computadora, folletos", "Computadora, impresora, Internet", 
  "Computadora, impresora, libros, Internet, material didactico,", 
  "Computadora, impresora, objetos abstracto, entre otros", "Computadora, internet y libros.", 
  "Computadora, internet, cámara de video, trípode, etc.", "Computadora, internet, impresiones", 
  "Computadora, internet, otros libros", "Computadora, libros de texto", 
  "Computadora, libros, impresora", "Computadora, libros, regla", 
  "Computadora. Videos tutoriales. Internet","Equipo de computación",
  "Guías,tecnología y material didáctico.", 
  "Internet, computadora, libros propios, tinta para pizarrón, hojas de cuadros", 
  "Internet, computadora, libros, impresora, páginas web", "Internet, laptop, copias.", 
  "LAPTOP, INTERNET Y LIBROS ESPECIFICOS DE APOYO",
  "Laptop e internet", "Laptop, Impresora", 
  "Libros, computadora, maletín...","Libros, lapto, cañonera", 
  "Pantalla, Impresora", "Proyector",
  "Pc, útiles de medida, texto, impresiones",
  "Tecnológicos y libros de texto",
  "Tecnológicos, físicos y libros",
  "Telefono con internet",
  "Teléfono, internet, tablet y materiales concretos",
  "equípo de computo. Material didactico", 
  "Computadora, álgebra, aritmetica, impresiones etcétera.", 
  "Pantalla, computadora", "Pizsrras, computadora, cañoneras",
  "TELEFONO CELULAR", "Tablet, celular, laptop, libros de texto", 
  "Tablet, hojas de trabajo", "Tablet, internet", "Tableta", "Tablets", 
  "recurtsos tecnologicos, calculadora y papel", "tecnologicos y Folletos"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    mate_recursos_propios = case_match(
      mate_recursos_propios,
      all_of(valores_tecnologia_mate2) ~ "Equipo tecnológico",
      all_of(valores_digitales_mate2) ~ "Recursos digitales",
      all_of(valores_papeleria_mate2) ~ "Material de papelería",
      all_of(valores_noespecifica_mate4) ~ "No especifica el recurso",
      all_of(valores_ninguno_mate2) ~ "Ninguno",
      all_of(valores_mobiliario_mate2) ~ "Mobiliario",
      all_of(valores_libros_mate2) ~ "Libros",
      all_of(valores_geometria2) ~ "Juego de geometría",
      all_of(valores_guia_folleto_mate2) ~ "Guías y folletos",
      all_of(valores_cnb_mate2) ~ "CNB",
      all_of(valores_calculadora) ~ "Calculadora",
      all_of(valores_audio_didactico_mate2) ~ "Material audiovisual y didáctico",
      .default = mate_recursos_propios
    )
  )

## volver a ver errores
valores_mate_recursos_propios <- df_dyd |>
  filter(!(is.na(mate_recursos_propios))) |>
  select(mate_recursos_propios) |>
  tabyl(mate_recursos_propios)

#View(valores_mate_recursos_propios)

# mate_otro_ultima_vez_libros_mineduc ----

## ver errores

valores_mate_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(mate_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(mate_otro_ultima_vez_libros_mineduc ))) |>
  select(mate_otro_ultima_vez_libros_mineduc) |>
  tabyl(mate_otro_ultima_vez_libros_mineduc)

dput(valores_mate_otro_ultima_vez_libros_mineduc)

## Define los vectores ----

### 2014

valores_mate_2014 <- c(
  "2014"
)

### 2016
valores_mate_2016 <- c(
  "2016"
) 

### 2017
valores_mate_2017 <- c(
  "2017"
)

### 2018 
valores_mate_2018 <- c(
  "2018", "2015 al 2018 guias de aprendizaje."
)

### 2019
valores_mate_2019 <- c(
  "2019"
)

### 2020
valores_mate_2020 <- c(
  "2020"
)

### No especifica
valores_sinespecificar_mate <- c(
  "Hace años","No recuerdo","No sé"
)

### nunca
valores_nunca_mate <- c(
  "No hay", "No se ha recibido ningún material de esta área", 
  "Nunca", "Nunca me proporcionaron"
  
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    mate_otro_ultima_vez_libros_mineduc = case_match(
      mate_otro_ultima_vez_libros_mineduc,
      all_of(valores_nunca_mate) ~ "Nunca ha recibido",
      all_of(valores_sinespecificar_mate) ~ "No especifica",
      all_of(valores_mate_2020) ~ "2020",
      all_of(valores_mate_2019) ~ "2019",
      all_of(valores_mate_2018) ~ "2018",
      all_of(valores_mate_2017) ~ "2017",
      all_of(valores_mate_2016) ~ "2016",
      all_of(valores_mate_2014) ~ "2014",
      .default = mate_otro_ultima_vez_libros_mineduc
    )
  )

## volver a ver errores

valores_mate_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(mate_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(mate_otro_ultima_vez_libros_mineduc ))) |>
  select(mate_otro_ultima_vez_libros_mineduc) |>
  tabyl(mate_otro_ultima_vez_libros_mineduc)

#View(valores_mate_otro_ultima_vez_libros_mineduc)

# mate_tipo_materiales_recibidos ----

## ver errores

valores_mate_tipo_materiales_recibidos <- df_dyd |>
  filter(mate_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(mate_tipo_materiales_recibidos ))) |>
  select(mate_tipo_materiales_recibidos) |>
  tabyl(mate_tipo_materiales_recibidos)

dput(valores_mate_tipo_materiales_recibidos)

## Define los vectores ----

### guia y planificador
valores_guia_plani_mate <- c(
  "GUÍA DE APRENDIZAJE Y PLANIFICADOR", 
  "Guia y planificador", "Guias", "Guías y planificadores"
)

### juego geometrico
valores_geometrico <- c(
  "Hojas, marcadores, juego geométrico"
)

### libros
valores_libro_mate <- c(
  "Libro de Matematicas", "Libros", "Libros y guía del docente", 
  "libro de Matemática para estudiantes"
)

### pizzara
valores_pizzara_mate <- c(
  "Pizarra,Protector"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    mate_tipo_materiales_recibidos = case_match(
      mate_tipo_materiales_recibidos,
      all_of(valores_pizzara_mate) ~ "Pizzara",
      all_of(valores_libro_mate) ~ "Libros",
      all_of(valores_geometrico) ~ "Juego geométrico",
      all_of(valores_guia_plani_mate) ~ "Guía y planificador",
      .default = mate_tipo_materiales_recibidos
    )
  )

## volver a ver errores

valores_mate_tipo_materiales_recibidos <- df_dyd |>
  filter(mate_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(mate_tipo_materiales_recibidos ))) |>
  select(mate_tipo_materiales_recibidos) |>
  tabyl(mate_tipo_materiales_recibidos)

#View(valores_mate_tipo_materiales_recibidos)

# tac_libros_primero ----

## ver errores

valores_tac_libros_primero <- df_dyd |>
  filter(tac_uso_libros == "Sí") |>
  filter(tac_grados == "Primer grado") |>
  filter(!(is.na(tac_libros_primero))) |>
  select(tac_libros_primero) |>
  tabyl(tac_libros_primero)

dput(valores_tac_libros_primero)

## Define los vectores ----

### educacion tecnologica

valores_edu_tecno <- c(
  "Educación tecnológica"
)

### tecnologia de la comunicacion
valores_tecno_comu <- c(
  "Tecnología de la Comunicación"
)

### tecnologia del aprendizaje
valores_tecno_aprend <- c(
  "Tecnología del Aprendizaje", 
  "Tecnología del aprendizaje Digecur"
)

### guias
valores_guias_tac <- c(
  "Guias Docente", "Guías" 
)

### no espcifica
valores_noespecifica_tac <- c(
  "Documentos impresos  y digital", 
  "El libro que envió el MINEDUC", 
  "Folletos impresos", "Investigaciones", 
  "LIBRO DE TECNOLOGÍA DE PRIMERO", "Mecanografía en computacion"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    tac_libros_primero = case_match(
      tac_libros_primero,
      all_of(valores_noespecifica_tac) ~ "No especifica",
      all_of(valores_guias_tac) ~ "Guías",
      all_of(valores_tecno_aprend) ~ "Tecnología del Aprendizaje",
      all_of(valores_tecno_comu) ~ "Tecnología de la Comunicación",
      all_of(valores_edu_tecno) ~ "Educación Tecnológica",
      .default = tac_libros_primero
    )
  )

## volver a ver errores

valores_tac_libros_primero <- df_dyd |>
  filter(tac_uso_libros == "Sí") |>
  filter(tac_grados == "Primer grado") |>
  filter(!(is.na(tac_libros_primero))) |>
  select(tac_libros_primero) |>
  tabyl(tac_libros_primero)

#View(valores_tac_libros_primero)

# tac_libros_segundo ----

valores_tac_libros_segundo <- df_dyd |>
  filter(tac_uso_libros == "Sí") |>
  filter(tac_grados == "Segundo grado") |>
  filter(!(is.na(tac_libros_segundo))) |>
  select(tac_libros_segundo) |>
  tabyl(tac_libros_segundo)

dput(valores_tac_libros_segundo)

## Define los valores ----

### guia metodologica tac
valores_guia_metodo_tac <- c(
  "Guias", "Guias .", "Guía metodológica TAC"
)

### manual de tecnologia
valores_manual_tac <- c(
  "Manual de Tecnología."
)

### tecnologia del aprendizaje y la comunicación 2
valores_tac_2 <- c(
  "Tecnología del Aprendizaje y la Comunicación TAC", 
  "Tecnología del aprendizaje y comunicación 2"
)

### ninguno 
valores_ninguno_tac <- c(
  "Google","Páginas web"
)

### no especifica
valores_noespecifica_tac2 <- c(
  "Enciclopedia y libros de computación", 
  "Libro externo", "Maya curricular y manuales de computación"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    tac_libros_segundo = case_match(
      tac_libros_segundo,
      all_of(valores_noespecifica_tac2) ~ "No especifica",
      all_of(valores_ninguno_tac) ~ "Ninguno",
      all_of(valores_tac_2) ~ "Tecnología del Aprendizaje y la Comunicación 2",
      all_of(valores_manual_tac) ~ "Manual de Tecnología",
      all_of(valores_guia_metodo_tac) ~ "Guía metodológica TAC",
      .default = tac_libros_segundo
    )
  )

## volver a ver errores

valores_tac_libros_segundo <- df_dyd |>
  filter(tac_uso_libros == "Sí") |>
  filter(tac_grados == "Segundo grado") |>
  filter(!(is.na(tac_libros_segundo))) |>
  select(tac_libros_segundo) |>
  tabyl(tac_libros_segundo)

#View(valores_tac_libros_segundo)

# tac_libros_tercero ----

valores_tac_libros_tercero <- df_dyd |>
  filter(tac_uso_libros == "Sí") |>
  filter(tac_grados == "Tercer grado") |>
  filter(!(is.na(tac_libros_tercero))) |>
  select(tac_libros_tercero) |>
  tabyl(tac_libros_tercero)

dput(valores_tac_libros_tercero)

## Define los vectores ----

### cnb
valores_cnb_tac <- c(
  "Cnb", "Cnb y paginas web"
)

### tecnologia del aprendizaje y comunicación educativa

valores_tac_3 <- c(
  "Libro de Tecnología del Aprendizaje y la Comunicación Editora Educativa"
)

### guias
valores_guias_tac_2 <- c(
  "Guías de ejercicios"
)

### tecnologia activa
valores_tecno_activa <- c(
  "Tecnología activa"
)

### Planificador de cyl idioma español
valores_planificador_cyl <- c(
  "Planificador de Comunicación y Lenguaje Idioma Espalo de Segundo Básico"
)

### no especifica
valores_noespecifica_tac3 <- c(
  "Libros digitales", "Libros digitales sobre aplicaciónes del teléfono inteligente en el ámbito educativo", 
  "Tecnologias", "Variados"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    tac_libros_tercero = case_match(
      tac_libros_tercero,
      all_of(valores_noespecifica_tac3) ~ "Sin especificar",
      all_of(valores_planificador_cyl) ~ "Planificador de Comunicación y Lenguaje Idioma Español",
      all_of(valores_tecno_activa) ~ "Tecnología Activa",
      all_of(valores_guias_tac_2) ~ "Guías",
      all_of(valores_tac_3) ~ "Tecnología del Aprendizaje y la Comunicación",
      all_of(valores_cnb_tac) ~ "CNB",
      .default = tac_libros_tercero
    )
  )

## volver a ver errores

valores_tac_libros_tercero <- df_dyd |>
  filter(tac_uso_libros == "Sí") |>
  filter(tac_grados == "Tercer grado") |>
  filter(!(is.na(tac_libros_tercero))) |>
  select(tac_libros_tercero) |>
  tabyl(tac_libros_tercero)

#View(valores_tac_libros_tercero)

# tac_recursos_centro_educativo ----

## ver errores

valores_tac_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(tac_recursos_centro_educativo))) |>
  select(tac_recursos_centro_educativo) |>
  tabyl(tac_recursos_centro_educativo)

dput(valores_tac_recursos_centro_educativo)

## Define los vectores ----

### audiovisuales y didactico
valores_audio_didactico_tac <- c(
  "Material audiovisual", 
  "Valija didáctica", "Visuales y auditivos", 
  "Material de internet", "Material didáctico.", "Material digital y Computadora", 
  "Material propio", "Materiales audiovisuales", "Más computadoras y didácticos"
)

### cnb
valores_cnb_tac2 <- c(
  "CNB", "CNB guías de aprendizzaje", 
  "CNB y folletos", "Cnb", "Cnb,", "El CNB y Malla curricular"
)

### guias y folletos

valores_guia_folleto_tac <- c(
  "Folleto impreso", "Folletos", 
  "Folletos impresos y  guías docentes", "Guias", "Guías", "Guías docentes", 
  "Guías y planificadores"
)

### Libros
valores_libros_tac <- c(
  "Computadora libros de textos de Tecnología y revistas actuales.", 
  "Computadora y libros",
  "Computadoras, libro","Informática para diversificado", 
  "Libro", "Libro Tecnología de la Información y la Comunicación del 1-3. Y un laboratorio de computación", 
  "Libro de computación total", "Libro de texto", "Libro, computadora, folletos", 
  "Libros", "Libros , folletos", "Libros Impresos", "Libros de texto", 
  "Libros de texto. Material audiovisual", "Libros digitales", 
  "Libros te texto, documentos digitales, tutoriales, paginas web, CNB", 
  "Libros y computadoras", "Libros, computadoras, internet, proyector", 
  "Libros, mobiliario.", "Libtos de texto, paginas de internet, capacitaciones, tutoriales, experiencia, guias de trabajo", 
  "Malla Curricular", "Manual de técnico operador", "Manuales y páginas web", 
  "libro", "libros y CNB", "libros, internet", "libros, internet, CNB"
)

### mobiliario
valores_mobiliario_tac <- c(
  "Pizarrón", "Pizarrón y hojas","Solo pizarrón", 
  "Pizarrón y marcador de formica, papel de diferentes tamaños etc"
)

### ninguno
valores_ninguno_tac2 <- c(
  "N/A", "NINGUNO", "NO APLICA", "Nada", "Ninguna", "Ninguno", 
  "Ningún", "No", "No hay","no utilizo libros",
  "ninguno", "ninguno, no hay computadores, solo pizarrón  y marcadores"
)

### papelerias
valores_papeleria_tac <- c(
  "Copias", "Hojas bond, impresiones, pliego manila, marcadores permanente, tinta para marcadores, grapadora, tijera, hojas bond, selladores, maskintape, entre algunos otros.", 
  "Hojas de trabajo","Impresiones", "Impresión de las hojas de trabajo", 
  "Marcador, almohadilla, pizzara, cartulina, videos."
)

### planificador del facilitador
valores_planificador_tac <- c(
  "Planificador de Comunicación y Lenguaje Idioma Español de Segundo Básico"
)

### recursos digitales
valores_digital_tac <- c(
  "Algunas capacitaciones sobre temas digitales, laboratorio de cómputo, pantalla e impresora.", 
  "Digitales","Bibliotecas virtuales, Páginas Web,",
  "PDF", "PDF DEL CNB DE TAC", "Paginas web",
  "Todo digital, cañoneras, Internet, plataformas", 
  "Página w", "Páginas web", "Recursos de la Web", "Recursos digitales", 
  "Plataforma Virtual Vinciu, Plataforma Virtual Tinkercad y CNB", 
  "Plataformas, bibliotecas y inteligencia artificial",
  "IA", "Impresos y digital", "Información de internet", 
  "Internet", "Internet computadoras", "Internet y otros textos de referencia antiguos", 
  "Investigaciones"
)

### tecnologia
valores_tecnologia_tac <- c(
  "1 laptop",
  "Un laboratorio y 38 computadoras", "Uso de libros y el laboratorio de computación", 
  "Tecnologias digitales", "Tecnologico", "Tecnológico, Infraestructura...", 
  "Tecnológicos","computadoras y Cañonera", 
  "computadoras, proyector", "equipo de computo e internet", "internet, televisión, computadoras", 
  "laboratorio de computación", "laboratorio de computación, computadoras, cañoneras", 
  "Proporcina laboratorio de Computación e internet", 
  "Recursos tecnológicos", "Salón de computación, Internet, Cañonera, Impresora", 
  "Proyector", "Proyector, computadora, tic", "Proyector, libros, Escritorio, computadoras, aire acondicionado", 
  "Proyector, uso de Internet, aula, equipos de computación", 
  "La computadora e internet", "Laboratorio", "PCs, TV, Cañonera, equipo hardware", 
  "Laboratorio Internet", "Laboratorio de Computación", "Laboratorio de Computación e internet", 
  "Laboratorio de Computación equipado con computadores y laptops.", 
  "Laboratorio de Computación, tabletas, WiFi", "Laboratorio de computacion", 
  "Laboratorio de computación", "Laboratorio de computación, Internet", 
  "Laboratorio de computación, cañoneras, internet.", "Laboratorio equipado.", 
  "Laboratorio para prácticas", "Laboratorio y Computadoras", 
  "Laptops","Pantalla,  1 computadora, pizarra", 
  "Pantalla, Impresora", "Pizarra, computadora, cañonera, marcadores",
  "Algunas capacitaciones sobre temas digitales, laboratorio de cómputo, pantalla e impresora.", 
  "El celular", "El equipo tecnologico", 
  "Impresora y papel", "Impresora, cañonera y pizarra", 
  "Equipo de computacion", "Equipo de computación, marcadores, impresiones.", 
  "Equipo de computación, pizarra, marcadores, proyector, televisión, internet, otros.", 
  "Equipo de computo, reproducción audivisual, entre otros...", 
  "Equipo de cómputo", "Folleto",
  "EQUIPO DE COMPUTO ENTRE OTROS MATERIALES", 
  "COMPUTADORA, CAÑONERA, IMPRESORA, LIBRONTES S DE DIFEREAUTORES", 
  "COMPUTADORAS Y PROYECTOR", "CURRRICULUM EMERGENTE", "Cañonera y la computadora", 
  "Cañonera, computadora", "Cañonera, libros, computadora", "Centro de computación", 
  "Centro de computación, pantalla, retroproyector",
  "Computadora", "Computadora libros de textos de Tecnología y revistas actuales.", 
  "Computadora y libros", "Computadora, Internet, etc", "Computadora, luz", 
  "Computadora, marcadores y cartulinas.", "Computadora, proyector, impresiones.", 
  "Computadoras", "Computadoras donadas por ONG", "Computadoras e internet", 
  "Computadoras marcadores   cartulinas", "Computadoras,  Televisores, Escritorio, Aire Acondicionado y Útiles de Oficina", 
  "Computadoras, Cañonera, Internet y material didactico", "Computadoras, Ncomputing", 
  "Computadoras, internet, cañonera", "Computadoras, internet, televisión", 
  "Computadoras, laptop archivos digitales", "Computadoras, libro", 
  "Computadoras, marcadores y cartulinas", "Computadoras, pantalla interactiva", 
  "Computadoras, proyector, plataforma, pizarra, marcadores, almohadilla, otros", 
  "Computadoras."
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    tac_recursos_centro_educativo = case_match(
      tac_recursos_centro_educativo,
      all_of(valores_tecnologia_tac) ~ "Equipo tecnológico",
      all_of(valores_digital_tac) ~ "Recursos digitales",
      all_of(valores_planificador_tac) ~ "Planificador",
      all_of(valores_papeleria_tac) ~ "Material de papelería",
      all_of(valores_ninguno_tac2) ~ "Ninguno",
      all_of(valores_mobiliario_tac) ~ "Mobiliario",
      all_of(valores_libros_tac) ~ "Libros",
      all_of(valores_guia_folleto_tac) ~ "Guías y folletos",
      all_of(valores_cnb_tac2) ~ "CNB",
      all_of(valores_audio_didactico_tac) ~ "Material audiovisual y didáctico",
      .default = tac_recursos_centro_educativo
    )
  )

## volver a ver errores

valores_tac_recursos_centro_educativo <- df_dyd |>
  filter(!(is.na(tac_recursos_centro_educativo))) |>
  select(tac_recursos_centro_educativo) |>
  tabyl(tac_recursos_centro_educativo)

#View(valores_tac_recursos_centro_educativo)

# tac_recursos_propios ----

## ver errores
valores_tac_recursos_propios <- df_dyd |>
  filter(!(is.na(tac_recursos_propios))) |>
  select(tac_recursos_propios) |>
  tabyl(tac_recursos_propios)

dput(valores_tac_recursos_propios)

## Define los vectores -----

### audiovisual y didactico
valores_audio_didactico_tac2 <- c(
  "Audiovisual.","Cbn, computadora y material didactico",
  "Fichas de contenido", "internet, material impreso",
  "USB, videos , presentaciones","Utilizo varias herramientas virtuales, presentaciones digitales sitios web etc", 
  "Videos", "Videos,  investigaciones,  fotocopias.", "Videos, computadoras, guías de trabajo etc.", 
  "Vídeos y hojas de trabajo",
  "Infigrafias, videos, presentaciones, materiales digitales elaborados", 
  "Material didactico, equipo de computo", "Material didáctico, proyector, internet", 
  "Material impreso, teléfono inteligente", 
  "Materiales didácticos digitales creados desde cero, recursos en línea, dispositivos propios...", 
  "Materiales impresos", "Materias didactico y audiovisual", "Maya curricular y manuales de computación"
)

### cnb
valores_cnb_tac3 <- c(
  "CNB y folletos", "CNB, folletos","Cnb", "internet, libros, CNB", 
  "Cbn, computadora y material didactico"
)

### guias y folletos
valores_guia_folleto_tac2 <- c(
  "CNB y folletos", "CNB, folletos",
  "Folleto", "Folletos", 
  "Folletos   hoja de trabajo  computadora", "Folletos de computación", 
  "Folletos de resúmenes", "Folletos impresos", "Folletos videos", 
  "Folletos y CNB", "Folletos y hojas de trabajo", "Folletos y libros bajados de la web", 
  "Folletos y temas de tecnología en Google", "Folletos, hojas de trabajo computadora", 
  "Folletos, internet, CNB digital", "Folletos, sólo se da área teórica", 
  "Guias", "Guias de trabajo", "Guias de trabajo, libros de texto, paginas y plataformas digitales", 
  "Guias de trabajo, portafolios, textos paralelos, actividades especificas de aplicacion, herramientas tecnologicas", 
  "Guias educativas", "Guías de trabajo", "Guías de trabajo, folletos, hojas de trabajo"
)

### Libros
valores_libros_tac2 <- c(
  "LIBROS DE TECNOLOGÍA", "internet, libros, CNB", 
  "libro y pdf  sobre TAC", "libros, internet", 
  "mi computadora, mis libros", "mis libros, mi computadora.", 
  "Libro", "Libro de tecnología", "Libro de texto", 
  "Libro de texto, tutoreales,  revistas.", "Libro e investigaciones", 
  "Libros", "Libros Digitales", "Libros de editoriales particulares", 
  "Libros de texto, manuales básicos para técnico operador en tecnología", 
  "Libros de textos de Tecnología actualizados", "Libros digitales, Manuales y Computadora Portatil", 
  "Libros e Internet", "Libros e investigaciones", "Libros en Internet", 
  "Libros propios", "Libros y CNB", "Libros y PDF", "Libros y computadora", 
  "Libros y computadora personal", "Libros y computadoras", "Libros y guias", 
  "Libros y materiales impresos", "Libros y recursos tecnológicos", 
  "Libros, folletos", "Libros, folletos, hojas", "Libros, fotocopias, dibujos entre otros", 
  "Manual de computación, computadora, impresora.", "Manual de técnico operador en tecnología", 
  "Manuales, Internet, videos."
)

### mobiliario
valores_mobiliario_tac2 <- c(
  "El pizarrón", "computadoras marcadores pizarron almohadilla hojas"
)

### ninguno
valores_ninguno_tac3 <- c(
  "Conocimiento", "Ninguna", "Ninguno", "Ninguno."
)

### no especifica
valores_noespecifica_tac4 <- c(
  "Económicos", "3","Teoria y practico","Todos"
)

### papeleria
valores_papeleria_tac2 <- c(
  "Cartel marcador  rubricas,lista de cotejo", 
  "Celulares, libros, hojas, marcadores etc", 
  "Cuaderno", "Cuaderno, libros, hojas.","copias, impresiones, teléfono celular", "hojas de trabajo impreso o digital",
  "Planificacion y cuaderno de trabajo.", "Rubricas .lista de cotejo carteles marcadores", 
  "Cuadernos, libros","Marcadores, computadoras etc",
  "Impresiones", "Impresiones, folletos", "Impreso", 
  "HOJAS, INTERNET, ENTRE OTROS", "Hojas de actividades", "Hojas de trabaja para la práctica misma", 
  "Hojas de trabajo", "Hojas de trabajo computadora", "Hojas, teléfono propio, marcadores"
)

### recursos digitales
valores_digital_tac2 <- c(
  "Aplicaciones de teléfono inteligente, internet", "Módulos de tac",
  "PDF DEL CNB SUB AREA DE TAC", "Paginas web y material didactico", 
  "Pdf","Textos, plataformas, recursos multimedia, dispositivos.", 
  "Plataformas digitales para diferentes ejercicios, laptop y teléfono.", 
  "Páginas digitales", "Páginas web,material didactico entre otros", 
  "Recursos de la Web", "Recursos digitales", "Red","Web", "internet", 
  "Cuentas de correos", "Digitales","IA","Material digital con ejemplos",
  "Información del Internet", "Intecap", "Internet", "Internet del celular", 
  "Internet e impresiones", "Internet residencial, textos de ingeniería", 
  "Internet, Computadora", "Internet, folletos", "Internet, impresión", 
  "Internet, libros de texto", "Internet, proyector, laptop", "Internet, tablet, minicomputadora, teléfono", 
  "Internet,tutorial", "Investigaciones", "Investigaciones propias y capacidad logrqda", 
  "Investigaciones, autoaprendizaje.", "Investigación, Pizarron, marcadores y computadoras", 
  "Investigación, hojas de trabajo", "Juegos en Internet y plataformas educativas"
)

### tecnologia
valores_tecnologia_tac2 <- c(
  "COMPUTADORA",  "PCs, TV","computadora", 
  "TELEFONO CELULAR", "Tabletas. Y electrónica", "Teléfono inteligente", 
  "Teléfono internet", "Una cañonera", "Usb,",
  "equipo de computo, audivisual, etc...", "personal", "teléfono inteligente",
  "Pantalla", "Pantalla, computadora", "Pc, textos, proyector", 
  "Proyecciones, recurso humano,", "Proyector", "Proyector , computadora",
  "Memoria Usb e Internet", "Memorias Usb e Internet", "Mi Laptop", 
  "La computadora, televisión, libros y artículos", "Laptop", 
  "Laptop e internet", "Laptop personal, materiales tecnológicos", 
  "Laptop y proyector", "Laptop, cañonero", "Laptop, internet, aplicaciones, imágenes, entre otros.", 
  "Laptop, internet.",
  "COMPUTADORA, CAÑONERA, IMPRESORA, LIBRONTES S DE DIFEREAUTORES, TECLAS INCERVIBLES, ETC.", 
  "COMPUTADORA, IMPRESORA","Impresora y papel",
  "Cañonera", "Cañonera, computadora",
  "LAPTOP, INTERNET, CAÑONERA, EXTENSIONES, REGLETAS, MATERIAL DIDACTICO", 
  "LATOP, INTERNET Y LIBRO ESPECIFICOS DE LA CLASE",
  "Celular hojas de trabajo", "Celular, internet",
  "Compu con internet", "Computación", "Computador personal, USB , impresora, y otros materiales como hojas, marcadores, lapiceros.", 
  "Computadora", "Computadora Personal", "Computadora cañonera", 
  "Computadora e impresora", "Computadora personal", "Computadora personal e internet", 
  "Computadora personal y Computadora del instituto", "Computadora personal, Internet.", 
  "Computadora portátil", "Computadora y ejercicios manuales y fotocopias", 
  "Computadora y libro", "Computadora, Internet", "Computadora, Internet propio", 
  "Computadora, Internet, correo electrónico, platillas creadas, videos", 
  "Computadora, Pantalla de presentación", "Computadora, USB, internet y guia de computación avanzada (ofimática)", 
  "Computadora, bocinas", "Computadora, cañonera y otros", "Computadora, cañonera, bocina, hojas bond, impresiones, entre otros.", 
  "Computadora, celular y cuaderno, libro.", "Computadora, impresora, internet, libros de texto", 
  "Computadora, internet, libros", "Computadora, libros", "Computadora, libros virtuales, lapiceros hojas marcadores", 
  "Computadora, luz", "Computadora, marcadores de pizarra, libros digitales", 
  "Computadora, material de apoyo digital", "Computadora, proyector", 
  "Computadora, usb", "Computadora. Internet.", "Computadora. TV", 
  "Computadoras", "Computadoras . Bosina", "Computadoras, marcadores impresoras cañoneras, internet", 
  "Computadoras, proyector, pizarra, marcadores, plataforma, internet, otros.", 
  "Comutadora, Internet"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    tac_recursos_propios = case_match(
      tac_recursos_propios,
      all_of(valores_tecnologia_tac2) ~ "Equipo tecnológico",
      all_of(valores_digital_tac2) ~ "Recursos digitales",
      all_of(valores_papeleria_tac2) ~ "Material de papelería",
      all_of(valores_noespecifica_tac4) ~ "No especifica",
      all_of(valores_ninguno_tac3) ~ "Ninguno",
      all_of(valores_mobiliario_tac2) ~ "Mobiliario",
      all_of(valores_libros_tac2) ~ "Libros",
      all_of(valores_guia_folleto_tac2) ~ "Guías y folletos",
      all_of(valores_cnb_tac3) ~ "CNB",
      all_of(valores_audio_didactico_tac2) ~ "Audiovisual y didáctico",
      .default = tac_recursos_propios
    )
  )

## volver a ver errores

valores_tac_recursos_propios <- df_dyd |>
  filter(!(is.na(tac_recursos_propios))) |>
  select(tac_recursos_propios) |>
  tabyl(tac_recursos_propios)

#View(valores_tac_recursos_propios)

# tac_otro_ultima_vez_libros_mineduc ----

## ver errores

valores_tac_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(tac_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(tac_otro_ultima_vez_libros_mineduc ))) |>
  select(tac_otro_ultima_vez_libros_mineduc) |>
  tabyl(tac_otro_ultima_vez_libros_mineduc)

dput(valores_tac_otro_ultima_vez_libros_mineduc)

## Define los vectores ----

### 2006
valores_tac_2006  <- c(
  "2006"
)

### 2017
valores_tac_2017 <- c(
  "2017"
)

### 2018
valores_tac_2018 <- c(
  "2018"
)

### 2019
valores_tac_2019 <- c(
  "2019"
)

### 2020
valores_tac_2020 <- c(
  "2020"
)

### ninguno
valores_nunca_tac <- c(
  "N/A", "NINGUNO", "NO APLICA", "Ninguna",
  "Ninguno", "No", "No existen", "No hay", "No he recibido", 
  "No he recibido material", "No he recibido nada", "No recuerdo", 
  "No se ha recibido material sobre esta área", "No se ha recibido ningún material de esta", "No se han recibido material.", "Nunca", "nunca"
  
)

## Recode con case match ----

df_dyd <- df_dyd  |>
  mutate(
    tac_otro_ultima_vez_libros_mineduc = case_match(
      tac_otro_ultima_vez_libros_mineduc,
      all_of(valores_nunca_tac) ~ "Nunca ha recibido",
      all_of(valores_tac_2020) ~ "2020",
      all_of(valores_tac_2019) ~ "2019",
      all_of(valores_tac_2018) ~ "2018",
      all_of(valores_tac_2017) ~ "2017",
      all_of(valores_tac_2006) ~ "2006",
      .default = tac_otro_ultima_vez_libros_mineduc
    )
  )


## volver a ver errores
valores_tac_otro_ultima_vez_libros_mineduc <- df_dyd |>
  filter(tac_ultima_vez_libros_mineduc == "Otro") |>
  filter(!(is.na(tac_otro_ultima_vez_libros_mineduc ))) |>
  select(tac_otro_ultima_vez_libros_mineduc) |>
  tabyl(tac_otro_ultima_vez_libros_mineduc)

#View(valores_tac_otro_ultima_vez_libros_mineduc)

# tac_tipo_materiales_recibidos ----

valores_tac_tipo_materiales_recibidos <- df_dyd |>
  filter(tac_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(tac_tipo_materiales_recibidos ))) |>
  select(tac_tipo_materiales_recibidos) |>
  tabyl(tac_tipo_materiales_recibidos)

dput(valores_tac_tipo_materiales_recibidos)

## Define los vectores ----

### Guias
valores_tac_guias <- c(
  "Guias"
)

### Ninguno
valores_tac_ninguno <- c(
  "Ninguno"
)

### Tecnologia
valores_tecnologia_tac3 <- c(
  "Televisión, tablet, plataforma digital,"
)

### didactico
valores_didactico_tac <- c(
  "Valija didáctica"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    tac_tipo_materiales_recibidos = case_match(
      tac_tipo_materiales_recibidos,
      all_of(valores_didactico_tac) ~ "Material didáctico",
      all_of(valores_tecnologia_tac3) ~ "Equipo tecnológico",
      all_of(valores_tac_ninguno) ~ "Ninguno",
      all_of(valores_tac_guias) ~ "Guías",
      .default = tac_tipo_materiales_recibidos
    )
  )

## volver a ver errores

valores_tac_tipo_materiales_recibidos <- df_dyd |>
  filter(tac_recepcion_materiales_mineduc == "Sí") |>
  filter(!(is.na(tac_tipo_materiales_recibidos ))) |>
  select(tac_tipo_materiales_recibidos) |>
  tabyl(tac_tipo_materiales_recibidos)

#View(valores_tac_tipo_materiales_recibidos)

# imparte_otra_area ----

## ver errores
valores_otra_area <- df_dyd |>
  filter(!(is.na(imparte_otra_area))) |>
  select(imparte_otra_area) |>
  tabyl(imparte_otra_area)

dput(valores_otra_area)

## Define los vectores ----

### Diseño gráfico
valores_diseno_grafico <- c(
  "A diversificado imparto las clases específicas de diseño gráfico",
  "Diseño Gráfico Computarizado, Cromatología del Color, Fundamentos del Diseño"
)

### administración
valores_administracion_area <- c(
  "Administración"
)

### biologia y quimica
valores_biologia_quimica <- c(
  "Biología y Quimica", "Química"
)

### cultura maya
valores_cultura_maya <- c(
  "CULTURA MAYA"
)

### Carpinteria
valores_carpinteria_area <- c(
  "Carpintería", "Orientación ocupacional Carpinteria"
)

### seminario
valores_seminario <- c(
  "Carrera de psicología seminario", "Legislacioon Aduanera y Seminario",
  "Seminario", "Seminario Literatura, Ética Profesional y Práctica Supervisada", 
  "Seminario, Etica profesional, literatura y Practica Supervisada", 
  "Seminario, etica profesional, práctica supervisada, literatura"
)

### ciencias naturales
valores_naturales_area <- c(
  "Ciencias Medicas", "Ciencias naturales",
  "Comunicación y Lenguaje, Matemáticas, Ciencias Sociales y Formación Ciudadana,  Ciencias Naturales, Productividad y Desarrollo, Expresión Artística Educación Física", "Científica"
)

### ciencias sociales
valores_ciencias_sociales_area <- c(
  "Ciencias sociales y formation ciudadana", 
  "Ciencias sociales, idioma maya, geografía económica" ,
  "Comunicación y Lenguaje, Matemáticas, Ciencias Sociales y Formación Ciudadana,  Ciencias Naturales, Productividad y Desarrollo, Expresión Artística Educación Física"
)

### cocina y reposteria
valores_cocina_reposteria <- c(
  "Cocina y Reposteria", "cocina y reposteria", 
  "Sub area de emprendimiento. Orientacion Economia Domestica. Cocina y Reposteria"
)

### comercio y servicios
valores_comercio_servicios <- c(
  "Comercial", "Comercio y Servicios", "Comercio y servicio", 
  "Comercio y servicios", "Contabilidad y Comercio y Servicios"
)

### comunicación y lenguaje
valores_comu_lenguaje_area <- c(
  "Comunicacion y lenguaje L2", "Comunicación y Lenguaje", 
  "Comunicación y Lenguaje, Matemáticas, Ciencias Sociales y Formación Ciudadana,  Ciencias Naturales, Productividad y Desarrollo, Expresión Artística Educación Física"
)

### contabilidad
valores_contabilidad_area <- c(
  "Contabilidad", "Contabilidad y Comercio y Servicios", "Contabilidad, finanzas, presupuestos, entre otros", 
  "Contabilidad.", "Contables", "contabilidad", "Área contable"
)

### artes visuales y musica
valores_visuales_musica <- c(
  "DANZA, TEATRO , artes visuales y música", 
  "Teatro", "Teatro y danza", "Teatro y visuales"
)

### danza
valores_danza_area <- c(
  "DANZA, TEATRO , artes visuales y música",
  "Danza"
)

### dibujo tecnico y construcción
valores_dibujo_tec_cons <- c(
  "Dibujo Técnico y de Construcción", "Dibujo Técnico y de construcción"
)

### educacion en valores
valores_valores <- c(
  "EDUCACIÓN EN VALORES", "Educación en Biblia y todas las áreas del grado de segundo primaria"
)

### electricidad
valores_electricidad_area <- c(
  "Electricidad", "Sub área de electricidad"
)

### emprendimiento
valores_emprendimiento <- c(
  "Emprendimiento", "Emprendimiento a la productividad", 
  "Sub area de emprendimiento. Orientacion Economia Domestica. Cocina y Reposteria",
  "Emprendimiento para la productividad, orientación industrial, orientación ocupacional electricidad."
)

### expresion artistica
valores_expre_artistica_area <- c(
  "Expresión Artistica", "Expresión Artística"
)

### filosofia
valores_filosofia <- c(
  "Filosofía", "Filosofía, Administración, Finanzas, Ética", 
  "Filosofía, Psicología, ética Profesional, Administración y Problemas socieconomicos"
)

### floricultura
valores_floricultura <- c(
  "Floricultura", "Sub área de floricultura"
)

### formación ciudadana y musical
valores_formacion_ciudadana_area <- c(
  "Formación", "Formación Ciudadana","Formación Musical"
)

### fisica fundamental
valores_fisica_fundamental <- c(
  "Fìsica Fundamental", "Física Fundamental", "Física fundamental", 
  "Física fundamental y química", "Física fundamental/Física, Estadística aplicada a la educación"
)

### historia del arte
valores_historia_arte <- c(
  "Historia del Arte"
)

### ingles
valores_ingles_area <- c(
  "Idioma Extranjero Ingles", "Ingles", "Inglés", "Lenguaje extranjero"
)

### introduccion a la economia
valores_intro_economia <- c(
  "Elementos de Lógica, Introducción a la Economía, Admimistración Educativa, Pedagogía y Medio Ambiente",
  "Introducción a la economía, legislación fiscal, geografía economía"
)

### lectoescritura
valores_lectoescritura <- c(
  "Lectoescritura", "Lectura", 
  "Lectura y Escritura", "Lectura y redacción"
)

### lengua y literatura
valores_lengua_literatura <- c(
  "Lengua y Literatura en Diversificado", "Lengua y literatura", "Literatura Hispanoamericana en 5o bachillerato"
)

### mecanografia
valores_mecanografia <- c(
  "Mecanografía"
)

### ninguna
valores_ninguna_area <- c(
  "0", "Puesto de Secretaria", 
  "AUXILIATURA", "Area Ocupacional", 
  "Cubrir cursos en el caso de que falte algún docente por emergencia médica u otra excusa.", 
  "Derecho", "Subdirector",
  "Director", "Educación en la fe", 
  "Fe y vida", "PECUARIA", "Paradigmas de Educaciòn para Adultos", "Perdon. Se me fue esta opcion", 
  "Practica docente", "Preparación de alimentos", 
  "Ixil bloom, una ONG", "LECTÓPOLIS", 
  "Manualidades", "N/A", "NO TRABAJO NINGUNA AREA, SOY DIRECTORA DEL OTRO ESTABLECIMIENTO", 
  "Ninguna", "Ninguna otra", "Ninguns", "Nunguna más"
)

### no especifica
valores_sinespecificar_area <- c(
  "cubro cuando es necesario, cualquier área", 
  "secretariada", "Secretaria", "Secretaria contadora", 
  "Secretario contador","Solo"
)

### orientacion educativa ocupacional
valores_orientacion_edu_area <- c(
  "Orientacio  Educativa", 
  "Orientación Agropecuaria", "Orientación Educativa", "Orientación Ocupaciónal Electricidad", 
  "Orientación de Belleza","Área de Economía Doméstica sub Área de Manualidades",
  "Área Doméstica, Manualidades y Decoración", 
  "Área Ocupacional", "Área Ocupacional Manualidades"
)

### productividad y desarrollo
valores_produ_desa_area <- c(
  "Productividad y desarrollo, Filosofía, Psicología Física fundamental", 
  "Productividad y desarrollo, sub área electricidad"
)

### practica supervisada
valores_practica <- c(
  "Práctica Docente", 
  "Práctica Supervisada",
  "Seminario, Etica profesional, literatura y Practica Supervisada", 
  "Seminario, etica profesional, práctica supervisada, literatura"
)

### psicologia
valores_psicologia_area <- c(
  "Psicología"
)

### robotica
valores_robotica <- c(
  "Robotica"
)

### mate
valores_mate_area <- c(
  "Solo matemáticas"
)

### tecnicas de mercadeo
valores_mercadeo <- c(
  "Tecnicas de Mercadeo y Comercializacion", 
  "Tecnología y taller de Electrónica", "Turismo,", "Técnicas de Mercadeo I, Técnicas de Mercadeo II"
)

### etica
valores_etica <- c(
  "Ética", "Ética , problemas socioeconómicos , economía"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    imparte_otra_area = case_match(
      imparte_otra_area,
      all_of(valores_etica) ~ "Ética",
      all_of(valores_mercadeo) ~ "Técnicas de Mercadeo",
      all_of(valores_mate_area) ~ "Matemáticas",
      all_of(valores_robotica) ~ "Robótica",
      all_of(valores_psicologia_area) ~ "Psicología",
      all_of(valores_practica) ~ "Práctica supervisada",
      all_of(valores_produ_desa_area) ~ "Productividad y desarrollo",
      all_of(valores_orientacion_edu_area) ~ "Orientación educativa y ocupacional",
      all_of(valores_sinespecificar_area) ~ "No especifica",
      all_of(valores_ninguna_area) ~ "Ninguna",
      all_of(valores_mecanografia) ~ "Mecanografía",
      all_of(valores_lengua_literatura) ~ "Lengua y literatura",
      all_of(valores_lectoescritura) ~ "Lectoescritura",
      all_of(valores_intro_economia) ~ "Introducción a la economía",
      all_of(valores_ingles_area) ~ "Inglés",
      all_of(valores_historia_arte) ~ "Historia del arte",
      all_of(valores_fisica_fundamental) ~ "Física fundamental",
      all_of(valores_formacion_ciudadana_area) ~ "Formación ciudadana",
      all_of(valores_floricultura) ~ "Floricultura",
      all_of(valores_filosofia) ~ "Filosofía",
      all_of(valores_expre_artistica_area) ~ "Expresión artística",
      all_of(valores_emprendimiento) ~ "Emprendimiento",
      all_of(valores_electricidad_area) ~ "Electricidad",
      all_of(valores_valores) ~ "Valores",
      all_of(valores_dibujo_tec_cons) ~ "Dibujo técnico y construcción",
      all_of(valores_danza_area) ~ "Danza",
      all_of(valores_visuales_musica) ~ "Artes visuales y música",
      all_of(valores_contabilidad_area) ~ "Contabilidad",
      all_of(valores_comu_lenguaje_area) ~ "Comunicación y lenguaje",
      all_of(valores_comercio_servicios) ~ "Comercio y servicios",
      all_of(valores_cocina_reposteria) ~ "Cocina y reposteria",
      all_of(valores_ciencias_sociales_area) ~ "Ciencias sociales",
      all_of(valores_naturales_area) ~ "Ciencias naturales",
      all_of(valores_seminario) ~ "Seminario",
      all_of(valores_carpinteria_area) ~ "Carpintería",
      all_of(valores_cultura_maya) ~ "Cultura maya",
      all_of(valores_biologia_quimica) ~ "Biología y química",
      all_of(valores_administracion_area) ~ "Administración",
      all_of(valores_diseno_grafico) ~ "Diseño gráfico",
      .default = imparte_otra_area
    )
  )

## volver a ver errores

valores_otra_area <- df_dyd |>
  filter(!(is.na(imparte_otra_area))) |>
  select(imparte_otra_area) |>
  tabyl(imparte_otra_area)

#View(valores_otra_area)

# caracteristicas_otra_area ----

valores_caracteristicas_otra_area <- df_dyd |>
  filter(!(is.na(caracteristicas_otra_area))) |>
  select(caracteristicas_otra_area) |>
  tabyl(caracteristicas_otra_area)

dput(valores_caracteristicas_otra_area)

## Define los vectores ----

### orientación y acompañamiento
valores_orientacion_acompanamiento <- c(
  "APOYAR A LOS ESTUDIANTES, MAESTROS Y ADMINISTRACIÓN",
  "Apoyar el rendimiento académico de los estudiantes, para que se puedan desarrollar tanto intelectual, emocional y física mente, apoyo a estudiantes de educación especial, orientación a docentes en temas relacionados a dificultades académicas de los estudiantes, apoyo y acompañamiento a padres de familia, en temas de conducta de los estudiantes…",
  "Dar acompañamiento e inducción en la prepararción y proceso de la docencia directa",
  "Dar acompañamiento y supervisar que cumplan con lo estiulado en el proceso de dar clases en un centro educativo",
  "Es Orientacion Educativa y Vocacional", "Orientación y acompañamiento",
  "Es una ONG para educar a peronas con discapacidad"
) 

### desarrollo habilidades y destrezas
valores_habi_des <- c(
  "Desarrolla habilidades en los estudiantes por medio de diversas técnicas",
  "Desarrollar sus habilidades de motricidad fina y gruesa y pueda expresar sus sentimientos y emociones por medio del arte",
  "En fomentar habilidades motoras en baile, coordinacion, expresión gestual y oral ante un público.",
  "Consiste en escribir en un teclado utilizando correctamente todos los dedos de la mano permitiendo de esta manera que los estudiantes adquieran habilidades.",
  "El área de mecanografía desarrolla diversos temas como la postura correcta del cuerpo y dedos con el principal objetivo de poder desarrollar destrezas del alumno capacidad para una mejor movilidad y rapidez de su trabajo",
  "Aplicación y aprendizaje técnico",
  "Desarrollo de habilidades y destrezas",
  "Describe la actuación del ser humano",
  "Las características de la danza es incluir el cuerpo como instrumento principal desarrollar habilidades y destrezas su conocer que tipos de danza hay en Guatemala",
  "Formación musical es una subárea de Expresión Artística del nivel medio. Tiene como finalidad formar en los estudiantes de desarrollo de sus destrezas musicales y comprender como la música es vital en su medio."
)

### formación teórica y conceptual
valores_teoria_concep <- c(
  "Conocer, identificaer, aplicar y resolver lo relacionado con los principales libros auxiliares de la contabiludad como ciencia",
  "Consiste en cuantificar de manera ordenada y detallada",
  "El área de filosofía se dedica al estudio crítico de las preguntas fundamentales sobre la existencia, la realidad, el conocimiento y la moralidad la belleza y la mente humana.",
  "Es un enforque teòrico que oriente còmo se entiende, organiza y desarrolla el aprendzaje en personas adultas.",
  "Fundamentos y conceptos básicos de psicología",
  "Historia de la filosofía",
  "Áreas teóricas"
)

### aprendizaje y aplicación tecnico
valores_tecnico <- c(
  "Administración de taller, capacitación en área de electrónica, programación, reparación de equipo electrónico, robótica, mantenimiento preventivo y correctivo de equipo electrónico y computadoras... Etc",
  "Capacitación y uso de herramientas de electricidad.",
  "Aprender diferentes métodos de cocción, técnicas culinarias, entre otros.",
  "Consiste en la presentación de servicios relacionados con la cosmetologia. Los estudiantes desarrollarán habilidades, destreza y conocimientos con el cuidado de piel, uñad, cabello y aplicación adecuada de productos quimicos",
  "Cuarenta por ciento teórico y sesenta por ciento práctico",
  "Teórica 40% Práctica 60%",
  "En el taller de electricidad aprenden los alumnos, desde un circuito simple y en tercero ya aprender a hacer instalaciones domiciliares",
  "Que el alumno aprenda conocimientos básicos de electricidad",
  "Realizar todo tipo de colecciones eléctricas",
  "instalaciones eléctricas residenciales realizando circuitos eléctricos de fuerza e iluminación y una introducción a la electrónica",
  "Teoría de nutrición y alimentos y prácticas en la. Elaboración de alimentos",
  "Contenidos y practicas relacionadas con cocina y reposteria. Area ocupacional",
  "El área de carpintería se basa en el conocimiento básico de los diferentes procesos para poder desarrollar o crear utensilios, amueblado y en el área industrial. Tomando en cuenta la carpintería básica como pilar del curso",
  "En conocimiento teórico práctico de principios de electricidad y electricidad domiciliar",
  "En el area de cocina y reposterialos estudiantes adquieren un sólido conocimiento de técnicas culinarias, básicas y avanzadas. Así como de los principios de nutrición básica, cocina y repostería, incluyendo métodos de cocción, manipulación de alimentos, combinación de sabores y la presentación de platos, así como la organización y gestión del tiempo, el trabajo en equipo",
  "Enseñanza básica de la Medicina teórica y practica",
  "Instructor en el área para el manejo con seguridad de las herramientas y materiales para la construcción de diferentes  muebles y aprendizaje de diferentes instalaciones",
  "Es una área ocupacional de los institutos PEMEN",
  "Elementos: trata temas filosóficos y lógica. Introducción a la Economía: Sistema mometario, oferta demanda. Administración Educativa: proceso administrativo. Pedagogia y Medio Ambiente: Recuersos Naturales, cuidados y consecuencias."
  
)

### valores y ética
valores_etica_valores <-c(
  "Conocer el amor de Dios en la propia vida a través de reflexiones personales y actividades que fortalezcan la autoestima y el sentido de pertenencia a la comunidad cristiana.",
  "Consiste trabajar valores, temas sociales y problemas a los que se pueden enfrentar los jóvenes.",
  "Educación en valores, la familia, sus actitudes basados en la palabra de Dios. En segundo primaria  imparto las áreas de Lenguaje, Matemática, Medio Social y Natural, Expresión Artística, formación Ciudadana.  dana,",
  "Espiritualidad del colegio,  con los valores de fraternidad,  contemplación,  simplicidad,  cuidado de la creación y sentido crítico",
  "Valores",
  "Se enseña la moral humana del ser humano",
  "Ética es un curso que se comparte con estudiantes de bachillerato",
  "Rama de la filosofía que estudia el comportamiento humano, el bien y el mal, y las normas que guían nuestras acciones para la prosperidad individual y social.",
  "Desarrollo de la sociedad",
  "Interculturalidad",
  "Introducción al derecho o sea las normas que rigen el estado del derecho en Guatemala",
  "La Filosofía estudia las grandes preguntas sobre la vida y el conocimiento. La psicología  estudia el comportamiento humano...",
  "Seminario es una area obligatoria... Etica profesional es darle conocimientos de orígenes hasta la actualidad...",
  "Valores, ética y formación ciudadana",
  "La Filosofía estudia las grandes preguntas sobre la vida y el conocimiento. La psicología  estudia el comportamiento humano y los procesos mentales. La ética  estudia los principios y valores que guía el comportamiento de las personas. La administración  estudia como planificar, organizar, dirigir y controlar los recursos de cualquier tipo de trabajo y los problemas son los problemas que afectan a nuestro país por ejemplo la economía."
  
)

### lectoescritura y comunicacion
valores_lectoescritura_comu <- c(
  "Analisis, redacción, hábito de lectura",
  "Aprender a dialogar y conocer  una segunda lengua",
  "Comprensión de lectura y mejoramiento de la caligrafía",
  "Comunicación y lenguaje L1, kaqchikel, filosofía, ciencias sociales, etc",
  "Comunicación y lenguaje L3",
  "Consiste en familiarizar al estudiante con el vocabulario básico en el área del idioma Inglés, así como frases de uso común, oraciones simples y conversaciones cortas.",
  "Es El área de Comunicación y Lenguaje.",
  "Ingles",
  "La clase de Lectópolis es una metodología de animación a la lectura...",
  "Lectoescritura y comunicación",
  "La forma correcta para emplear el idioma español, tanto oral colmo escrito",
  "Literatura Hispanoamericana, ortografía, comprensión lectora, semántica",
  "Esta clase consiste en impartir la clase con el apoyo de libro enseñandoles tecnicas  en la lectura y reforzando lo mas inportante la escritura y ortografia de la misma.",
  "Se encargará de fortaleces los conocimientos  de la lecrura y de la buena escritura",
  "Tipos de lecturas, redacción de textos, técnicas de estudio, reglas ortografícaa. Entre otros",
  "yo utilizo el cnb para enseñar ingles a diversificado, utilizo la IA para organizar la información y utilizo un material personal para impartir las clases",
  "La clase de Lectópolis es una metodología de animación a la lectura que convierte el aula en una “ciudad de lectores” donde los estudiantes participan de manera activa y divertida. Se caracteriza por el uso de la gamificación (retos, puntos o recompensas), la creación de un ambiente motivador, el trabajo cooperativo, y la lectura como un proceso de comprensión y reflexión crítica. El docente actúa como guía y facilitador, proponiendo actividades dinámicas como juegos, debates o dramatizaciones. Además, integra diferentes tipos de textos para desarrollar el gusto por la lectura y fortalecer las competencias comunicativas, haciendo que leer sea una experiencia significativa, atractiva y formativa."
  
)

### investigacion, analisis y pensamiento
valores_analisis_invest <- c(
  "Busca desarrollar en los estudiantes abilidades y conosimientos para participate n una sociedad democratica",
  "Dar los pasos para el informe qué hacen los estudiantes para entregar al finalizar s prácticas supervisada",
  "El desarrollo del pensamiento analitico, sobre la leyes que rigen a la naturaleza",
  "Fortalecer los procesos de investigación, citación y sobre todo crear documentos donde ellos pueda ser críticos de un proceso",
  "Proyecto de vida, y estudio y solución de problemas de la realidad social o emanados por el MINEDUC.",
  "¿Qué es Seminario?  Es una subárea del área de Investigación en el Bachillerato en Ciencias y Letras.  cnbguatemala.org +2 Scribd +2  En Seminario, los estudiantes desarrollan proyectos de investigación‐acción...",
  "Investigación, análisis y pensamiento",
  "Seminario es una area obligatoria para los estudiantes que están afines de graduarse y consiste en sembrar conocimientos de identidad  como su proyecto de vida y la nacionalidad...",
  "Seminario es una área obligatoria que los estudiantes graduandos deben de seguir",
  "Seminario área obligatoria de los estudiantes a fin de graduarse",
  "¿Qué es Seminario?  Es una subárea del área de Investigación en el Bachillerato en Ciencias y Letras...",
  "Seminario es una area obligatoria para los estudiantes que están afines de graduarse y consiste en sembrar conocimientos de identidad  como su proyecto de vida y la nacionalidad en contenido su pais para identificar problemas y darle soluicion.     Etica profesional es darle conocimientos de orígenes hasta la actualidad en relación a los valores y el desempeño  laboral que realizaran en el lugar de practicas y",
  "¿Qué es Seminario?  Es una subárea del área de Investigación en el Bachillerato en Ciencias y Letras.  cnbguatemala.org +2 Scribd +2  En Seminario, los estudiantes desarrollan proyectos de investigación‐acción que les permiten acercarse a problemas reales de su contexto (familiar, comunitario, nacional) para entenderlos, analizarlos y proponer soluciones.  Scribd +2 cnbguatemala.org +2  Tiene un fuerte componente reflexivo: se trabaja sobre valores, misión, visión, metas personales, su relación con la sociedad, y se articula con los ejes de la Reforma Educativa. En caracteristicas, es que es objetivos de aprendizajes ya establecidos, contextualizacion y permite la evaluación individual y grupal."
)

### emprendimiento, economía y vida laboral
valores_emprendimiento_laboral <- c(
  "Consiste en lograr que cada estudiante sea capaz de integrarse a la vida productiva mediante la adquisición de abilidades y destrezas  en el campo de la electricidad,  el emprendimiento de su propia micro empresa...",
  "Consiste en: Formar alumnos qué tengan el interés y la oportunidad de emprender un negocio...",
  "Dedarrollar habilidades , conocer practicas del mercado actuial...",
  "En enseñar técnicas para la elaboración de manualidades y decoraciones, donde el alumno pueda a traves de lo aprendido crear un emprendimiento para subsistir",
  "En enseñarle a los Jóvenes crianza de aves, emprender con un negocio",
  "En orientar al alumno en emprendimientos en relación a desarrollo de dibujo técnico y de construcción",
  "Es un sistema económico y cuantitativo, que consiste en actividades económicas",
  "Realizar emprendimientos por medio de la transformación de productos...",
  "Se podría decir que es emprendimiento",
  "Todo lo relacionado con emprendimiento",
  "Trabajar el área comercial","Cultura turística",
  "analizar los temas de economía, ver los avances y el movimiento de la economía en el mundo...",
  "Consiste en lograr que cada estudiante sea capaz de integrarse a la vida productiva mediante la adquisición de abilidades y destrezas  en el campo de la electricidad,  el emprendimiento de su propia micro empresa, o integrarse alguna empresa , si en caso  no pueda continuar estudiando",
  "Consiste en: Formar alumnos qué tengan el interés y la oportunidad de emprender un negocio. Caracteristicas: Dar temas sibre negocios, ventas, ofertas, demandas, pasos para proyectos de emprendimiento,  consumidores,  productos y otros afines a la clase.",
  "Dedarrollar habilidades , conocer practicas del mercado actuial,las exigencias de cambio en el mercado productivo, sus principales caracteristicas,planificar, organizar y ejecutar",
  "Emprendimiento y vida laboral",
  "Enseñar el proceso de comercialización de productos o servicios",
  "Realizar emprendimientos por medio de la transformación de productos, atendiendo a las necesidades con ideas creativas. Creando proyectos de forma individual o grupal.",
  "analizar los temas de economía, ver los avances y el movimiento de la economía en el mundo así como las leyes que protegen la economía y las que rigen a un país",
  "Ética se relaciona con el comportamiento humano y como desarrollarse en una empresa, a la hora de laborar . Economía todo lo relacionado a la manera en cómo dar buen uso de los recursos económicos y a emprender, socioeconómicos se relaciona a la manera como nos relacionamos con las personas los problemas sociales, etc."
)


### finanzas y administracion
valores_gestion_admin <- c(
  "Administrar, responsablemente, los bienes que posee y aplicar los principios de contabilidad en la solución de problemas de la vida cotidiana.",
  "Conocimientos relacionados a ka Administración Educativa",
  "Consiste en analizar y registras los ingresos y egreso de la empresa y sus características  son sistemática,objetiva, útil",
  "Consiste en registrar,  clasificar y analizar las transacciones financieras.",
  "Contabilidad",
  "Determinar los costos y gastos en su vida diaria",
  "Director",
  "Durector del centro educativo",
  "El Curso de Contabilidad de 3ro. básico, inicia con Cultura Tributaria, luego las Cuentas, clasificación y registro.",
  "El uso de la economía o control de procesos administrativos y Contables de una entidad publica y privada",
  "El área de Contabilidad consiste en el registro, clasificación y análisis de las operaciones financieras...",
  "El área de contabilidad, tiene el objetivo de organizar las transacciones económicas...",
  "Elaboración de inventarios, nomenclatura contable, balance, ajustes, presentación de States Financieros",
  "Gubernamental, Finanzas Públicas,",
  "Llenado de papelería inscripción registros a los estudiantes al SIRE",
  "Planificar, organizar, supervisar el trabajo docente",
  "Secretario contador del establecimiento",
  "contadora del intituto",
  "registrar, controlar los bienes de las diferentes empresas",
  "Área financiera y contable",
  "El área de Contabilidad consiste en el registro, clasificación y análisis de las operaciones financieras, con el fin de generar información veraz y útil para la toma de decisiones. Sus principales características son el control de recursos, elaboración de estados financieros, cumplimiento legal y desarrollo de habilidades analíticas y organizativas en los estudiantes.",
  "El área de contabilidad, tiene el objetivo de organizar las transacciones económicas y financieras de un a entidad en particular. La información que se trabaja en el área contable, proporciona información clara a las personas importantes de la empresa. Y es fundamental para la toma de decisiones.",
  "Elaboración de inventarios, nomenclatura contable, balance, ajustes, presentación de Estados Financieros",
  "En principios contables básicos. Que gracias a las autoridades ya no figura como área en el nivel básico.",
  "En transmitir conocimientos relacionados con los métodos y técnicas sobre el registro y bienes de una empresa, con el objetivo que los estudiantes adqyieran habilidades de registro y se familiaricen con un vocabulario técnico y sean capaces de resolver.",
  "Enseñar los principios de contabilidad así como los diferentes procesos contables, empresas inscritas",
  "Es una disciplina que muestra o ayuda a registrar, clasificar y resumir todas las operaciones económicas de una empresa (ganancias, perdida, ingresos, gastos). Sus características son: registrar operaciones económicas, es cuantitativa, es sistemática e histórica, cumple con normas legales y es universal.",
  "Esta área se orienta al conocimiento de los fundamentos de la contabilidad, se analiza la lógica contable, las diferentes concepciones de la contabilidad, el objeto de su estudio y el marco conceptual de las Normas Internacionales de Información Financiera –NIIF, así también al tipo de propietario individual o jurídico y a las diferentes aplicaciones en actividades comerciales, industriales, bancarias, gubernamentales, de servicios, agrícolas, cooperativas y otros.",
  "Gestión y administración",
  "Por ser un instituto experimental o INEBE, todavía se imparten esos cursos en el área comercial",
  "Se encarga de registrar y organizar de forma ordenada los ingresos y egresos, permitiendo controlar y analizar la información, ayudando a planificar el uso de los valores como la honestidad y responsabilidad, fortaleciendo en los estudiantes el sentido del orden, la organización y el buen manejo de recursos.",
  "Vemos una introducción a la contabilidad, elaboración de libro inventario, diario, mayor, balance, hoja de trabajo y estados financieros",
  "es la disciplina que registra, clasifica y resume las transacciones financieras de una empresa para proporcionar información útil en la toma de decisiones y el cumplimiento de obligaciones",
  "Es el estudio de leyes, reglamentos y disposiciones legales que rigen las operaciones de comercio y el cumplimiento de las leyes tributarias, su característica principal el control fronterizo,la recaudación de impuestos al comercio exterior, regularización de trafico aduanero y normativo de convenios internacionales, Seminario: Curso en el cual los alumnos consolida conocimientos y habilidades, y se preparan para enfrentar retos del mundo profesional y académico, teniendo como caracteristica principal el autoconocimiento, la identidad y la socialización de retos que se plantean para el alcance de sus metas"
  
)


### uso de tecnología y herramientas
valores_tecno_herramienta <- c(
  "El uso del razonamiento usando la tecnología llevando la imaginación utilizando la tecnología y herramientas digitales y tecnologícas",
  "En desarrollar todas las bases de la expresión gráfica, conocer acerca de los colores y el software",
  "La imnobacion",
  "Uso de tecnología y herramientas",
  "Es un centro experimental, por lo cual se da las bases para ejercer como dibujante enfocado en un emprendimiento",
  "El área de mecanografía desarrolla diversos temas como la postura correcta del cuerpo y dedos con el principal objetivo de poder desarrollar destrezas del alumno capacidad para una mejor movilidad y rapidez de su trabajo."
  
)


### expresion artistica y creatividad
valores_expre_creatividad <- c(
  "Danza música y teatro consisten en el conocimiento3n los movimientos corporales...",
  "Crear, manualidades utiles para el hogar con materiales reciclables",
  "Desarrollar sus habilidades de motricidad fina y gruesa y pueda expresar sus sentimientos y emociones por medio del arte",
  "Es un proceso educativo que busca el desarrollo integral del estudiante a través de la música...",
  "Historia del Arte I y II. Diseño Gráfico Computarizado I y II...",
  "Las características de la danza es incluir el cuerpo como instrumento principal...",
  "Les enseño a las alumnos sobre los tipos de teatro...",
  "Música, Danza, Teatro",
  "Para los graduandos de diseño gráfico",
  "Parte de expresión artística",
  "Técnicas de artes plásticas",
  "Teatro es el conocimiento de la actuación que incluye conocimiento  amplio de los signos teatrales...",
  "Danza música y teatro consisten en el conocimiento3n los movimientos corporales qué sirven como forma de expresión Artística social y cultural. Artes visuales  consisten en percibir principalmente a través de la vista la.pintura, escultura y el dibujo .",
  "Es un proceso educativo que busca el desarrollo integral del estudiante a través de la música, fomentando la experimentación, la apreciación y la creación musical",
  "Expresión artística y creatividad",
  "Historia del Arte I y II. Diseño Gráfico Computarizado I y II. Cromatología del Color, Fundamentos del Diseño, Diseño y Expresión Gráfica",
  "Les enseño a las alumnos sobre los tipos de teatro, tipos de actuación historia etc",
  "Teatro es el conocimiento de la actuación que incluye conocimiento  amplio de los signos teatrales, en visuales las técnicas de la creatividad plásticas y las técnicas de la pintura"
)

###  comprensión del mundo físico
valores_mundo_fisico <- c(
  "Biología: Organización celular, Método científico. Química: Nomenclatura, ecuaciones y reacciones químicas",
  "Ciencias naturales en segundo grado",
  "Area que se dedica al cultivo industrializado de flores y plantas ornamentales, ya sean para decoración, jardinería o floristería",
  "El estudio de las hortalizas en general",
  "Es el área que estudia la naturaleza en su forma microscópica",
  "Física fundamental se relaciona con la matemática...",
  "La fisica estudio el movimiento de los cuerpos...",
  "La física es parte de la matemáticas...",
  "La física fundamental es la rama de la física...",
  "Comprensión del mundo físico",
  "Area que se dedica al cultivo industrializado de flores y plantas ornamentales, ya sean para decoración, jardinería o floristería",
  "Comprensión del mundo físico",
  "El área de Física fundamental/Física que se imparte consiste en el estudio de los conceptos básicos y experimentales de la física, aplicando métodos estadísticos para el análisis y validación de datos experimentales. Esta área está orientada a fortalecer el pensamiento analítico científico, haciendo uso de la estadística descriptiva e inferencial como herramientas para estudiar fenómenos físicos y validar modelos científicos y hipótesis. Su objetivo es que el estudiante comprenda y maneje conceptos estadísticos esenciales para analizar datos en experimentos físicos, apoyando la enseñanza en niveles medio y medio superior, sin requerir demostraciones complejas pero con un alto nivel de análisis.  Por otro lado, la Estadística aplicada a la educación se enfoca en la recopilación, análisis e interpretación de datos relacionados a fenómenos educativos, en particular en el ámbito de la educación física, para sistematizar observaciones y tomar decisiones informadas. Esta área desarrolla una forma especial de pensamiento que ayuda a resumir y visualizar los datos numéricos o gráficos, favoreciendo la investigación y evaluación en contextos educativos, y proporcionando herramientas prácticas para el profesional en educación.",
  "Ciencias sociales se refiere a historia, idioma maya costumbres e idioma, geografia ubicación de tierras y aguas productivas",
  "DEDICADAA LA CRIANZA DE ANIMALES DE CORRAL",
  "Dónde se enseña la floriculruta, el cultivo de las flores, la jardinización, jardinizar áreas verdes y el manejo agronómico de las plantas ornamentales",
  "El área de Física fundamental/Física que se imparte consiste en el estudio de los conceptos básicos y experimentales de la física, aplicando métodos estadísticos para el análisis y validación de datos experimentales...",
  "El área de física fundamental, se da la introducción a como se rige la materia en unidades físicas, así como la comprensión de espacio geométrico y su funcionalidad  en un ambiente inmediato.",
  "Física fundamental se relaciona con la matemática pero más en la aplicación de fórmulas y química estudia la composición de la vida, los átomos, moléculas, compuestos químicos etc",
  "La fisica estudio el movimiento de los cuerpos reposo o en movimiento Etc.y las leyes de Newton",
  "La física es parte de la matemáticas que ayuda a comprender fenómenos físicos y aplicarlo a la vida cotidiana. Las características son ánalisis de vectores, ángulos además de experimentar los diferentes fenómenos físicos",
  "La física fundamental es la rama de la física que estudia las leyes básicas y las fuerzas fundamentales que gobiernan el comportamiento del universo.",
  "Química,Estadística ,matemáticas",
  "Valores, estructura de los organismos"
)


### No aplica
valores_noaplica_area <- c( 
  "Ninguna", "Ninguna","Ninguna más",
  "Ninguno","Ningún otro curso",
  "No hay más asignaturas que se imparten",
  "No son cursos", "No aplica",
  "Todas las áreas curriculares han sido actualizadas en términos del nuevo paradigma curricular, avances científicos,  tecnológicos y humanisticos y demandas sociales.",
  "Es en el área de básicos, consiste en que los alumnos logren cumplir los indicadores y las competencias de área",
  "Solo esas áreas descritas en el grado de tercero basico",
  "Solo imparto las anteriores",
  "acabo de empezar, por lo que no tengo curso específico",
  "Ninguna más","Ninguno","Ningún otro curso",
  "No hay más asignaturas que se imparten",
  "NO IMPARTO NINGUNA CLASE ESTOY EN EL AREA ADMINISTRATIVA",
  "No son cursos","Soy subdirector no docente",
  "acabo de empezar, por lo que no tengo curso específico",
  "NO IMPARTO NINGUNA CLASE ESTOY EN EL AREA ADMINISTRATIVA",
  "No soy docente soy Secretaria",
  "Personal adminitrativa",
  "Soy subdirector no docente"
)

## Recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    caracteristicas_otra_area = case_match(
      caracteristicas_otra_area,
      all_of(valores_noaplica_area) ~ "No aplica",
      all_of(valores_expre_creatividad) ~ "Expresión artística y creatividad",
      all_of(valores_tecno_herramienta) ~ "Uso de tecnología y herramientas",
      all_of(valores_gestion_admin) ~ "Gestión, finanzas y administración",
      all_of(valores_emprendimiento_laboral) ~ "Emprendimiento y vida laboral",
      all_of(valores_analisis_invest) ~ "Investigación, análisis y pensamiento",
      all_of(valores_lectoescritura_comu) ~ "Lectoescritura y comunicación",
      all_of(valores_etica_valores) ~ "Valores, ética y formación ciudadana",
      all_of(valores_tecnico) ~ "Aplicación y aprendizaje técnico",
      all_of(valores_teoria_concep) ~ "Formación teórica y conceptual",
      all_of(valores_habi_des) ~ "Desarrollo de habilidades y destrezas",
      all_of(valores_mundo_fisico) ~ "Comprensión del mundo físico",
      all_of(valores_orientacion_acompanamiento) ~ "Orientación y acompañamiento",
      .default = caracteristicas_otra_area
    ) 
  )

## volver a ver errores

valores_caracteristicas_otra_area <- df_dyd |>
  filter(!(is.na(caracteristicas_otra_area))) |>
  select(caracteristicas_otra_area) |>
  tabyl(caracteristicas_otra_area)

#View(valores_caracteristicas_otra_area)


# competencias_otra_area ----

## ver errores
valores_competencias_otra_area <- df_dyd |>
  filter(!(is.na(competencias_otra_area))) |>
  select(competencias_otra_area) |>
  tabyl(competencias_otra_area)

dput(valores_competencias_otra_area)

## Define los vectores ----

### pensamiento critico y resolucion de problemas
valores_pensamiento_resolucion <- c(
  "1. Análisis y resolución de problemas 2. Pensamiento crítico 3. Modelado matemático 4. Experimentación  Estas competencias son fundamentales para entender y describir el universo.",
  "Aplicación de pensamiento critico",
  "Capacidad de Síntesis, Análisis e interpretación",
  "Capacidad de analizar problemas y dar soluciones funcionales, afrontar fallas en imprevistos",
  "El pensamiento crítico, autoevaluación y trabajo en equipo",
  "Pensamiento analitico al conocer las leyes físicas",
  "Pensamiento crítico, flexible y analítico",
  "Que el estudiante tenga una fluidez para resolver problemas de una manera profesional",
  "i. Interpretan los fenómenos naturales para hallar una solución a sus problemas.",
  "Compara opciones de solución a problemas comunitarios en base a la información sobre desarrollo humano proveniente de la agropecuaria.",
  "Capacidad de analizar problemas y dar soluciones funcionales, afrontar fallas en imprevistos , saber coordinarse con nuestros compañeros, tener disciplina en el trabajo aprendiendido y aplicado, trabajar y mantener el entorno de trabajo seguro",
  "Desarrollar el pensamiento indagador y reflexivo como producto del manejo selectivo y riguroso de la información",
  "Pensamiento crítico y resolución de problemas",
  "Se enfocan en la comprencion critica del mundo y la capacidad de actuar en la sociedad",
  "Que el estudiante desarrolle su sentido crítico, analítico y resolución de problemas en el contexto que vive",
  "Pensamiento crítico y resolución de problemas",
  "Se enfocan ennla comprencion critics del mundo y la caps ideas de actuar en la sociedad",
  "Mejorar su calidad de entedimiento",
  "Mejorar la comprensión lectora en el estudiante, así como también la escritura y caligrafía de si mismo.",
  "Que el aprendizajes, sean exitosos",
  "En el desarrollo adecuado de los conocimientos",
  "Permitir que el estudiante desarrollo diferentes habilidades",
  "DESARROLAR HABIILIDADES Y DESTREZAS DE LOS ESTUDIANTES",
  "Desarrollar la habilidad intelectual de personas con discapacidad",
  "i.	Interpretan los fenómenos naturales para hallar una solución a sus problemas. ii.	Aplican los fenómenos matemáticos y físicos para el mejoramiento del ambiente. iii.	Conocen el campo de la física para entender la naturaleza.",
  "Aplicar Normas del Código Tributario en el cumplimiento de las leyes en los procesos administrativos, Desarrollar el pensamiento indagador y reflexivo como producto del manejo selectivo y riguroso d la información del conocimiento de los pueblos y de las capacidades de análisis crítico de las fuentes metodológicas utilizadas en las investigaciones.",
  "Pensamiento crítico y resolución de problemas",
  "En estas áreas se busca desarrollar en los estudiantes competencias para el pensamiento crítico, la reflexión ética, la comprensión del comportamiento humano, la toma de decisiones responsables y el análisis de la realidad social y económica. Además, se fomenta la capacidad de liderazgo, la comunicación efectiva, la responsabilidad profesional y la habilidad para administrar recursos y resolver problemas con una visión humanista y ética.",
  "En las competencias bàsicas, uso de tecnologìa, lectoescritura funcional, comprender y producir textos. y en las competencias personales su capacidad de aprender por sì mismo y organizar su tiempo, tener un pensamiento crìtico, para analizar, cuestionar y reflexionar sobre la readlidad"
)

### competencias comunicativas y lingüísticas
valores_competencia_comunicativa <- c(
  "Ampliar sus habilidades de entendimiento y comprensión se lo que lee.",
  "Aplicación de palabras de enlace para unir textos",
  "Competencias linguisticas",
  "Aprenda sobre filosofía clásica, patristica, Escolastica, revolución religiosa, racionalismo, Empirismo e ilustración",
  "Comprensión lectora en sus tres niveles, interpretación de la cosmovisión y el contexto social",
  "Comunicación y lenguaje extranjero",
  "Dominar una segunda lengua",
  "El mejoramiento de la lectura a base de tecnica así como la escritura",
  "Expresarse de una manera adecuada.",
  "Interactuar de forma oral, escrita, signada o multimodal",
  "Mejorar la comprensión lectora en el estudiante",
  "Obtener el hábito de lectura",
  "Velocidad y precisión al escribir",
  "Ampliar sus habilidades de entendimiento y comprensión se lo que lee. Que participe en diferentes conversaciones formales e informales",
  "Comprensión lectora en sus tres niveles, interpretación de la cosmovisión y el contexto social; hábito de la lectura, redacción correcta y ampliación de vocabulario",
  "Interactuar de forma oral, escrita, signada o multimodal de manera coherente y adecuada",
  "Mejorar la comprensión lectora en el estudiante, así como también la escritura y caligrafía de si mismo",
  "Perder el miedo de hablar en público, desarrollar la capacidad de desenvolvimiento",
  "Que ellos puedan desenvolverse con el idioma para sus carreras y trabajos",
  "Que identifique y responda a instrucciones simples en el idioma Inglés, que escriba oraciones con vocabulario básico y lea textos cortos",
  "Competencias comunicativas y lingüísticas",
  "Ampliar sus habilidades de entendimiento y comprensión se lo que lee. Que participe en diferentes conversaciones formales e informales,",
  "Comprensión lectora en sus tres niveles, interpretación de la cosmovisión y el contexto social; hábito de la lectura, Redacción correcta, ampliación de vocabulario; movimientos literarios, sus características, poetas y/o escritores representativos, así como sus principales obras y premios recibidos y por supuesto reforzar las 4 habilidades; hablar, escuchar, leer y escribir.",
  "Comunicación y lenguaje, ciencias sociales.",
  "Comunicativas y lingüisticas",
  "Identificar los principales funcionamiento del lenguaje y la escritura",
  "Interactuar de forma oral, escrita, signada o multimodal de manera coherente y adecuada en diferentes ámbitos y contextos y con diferentes propósitos comunicativos",
  "Perder el mido de hablar en público, desarrollar la capacidad de desembolvimiento",
  "Que ellos puedan desenvolverse con el idioma para sus carreras y trabajos.",
  "Que identifique y responda a instrucciones simples en el idioma Inglés, que escriba oraciones con el vocabulario básico y que lea textos cortos.",
  "se busca desarrollar en los estudiantes competencias lectoras y comunicativas como la comprensión lectora (entender, interpretar y reflexionar sobre los textos), la expresión oral y escrita (comunicar ideas con claridad y creatividad), el pensamiento crítico (analizar, cuestionar y opinar sobre lo leído), la colaboración (trabajar en equipo y compartir ideas), y el gusto por la lectura (valorar la importancia de leer para aprender y disfrutar)."
  
)

### competencias científicas y lógico-matemáticas
valores_competencia_cientifica <- c(
  "Análisis de los cambios en la materia y estudio de la organización celular.",
  "Competencias en Física fundamental/Física Comprensión y aplicación de conceptos fundamentales de la física",
  "Comprender procesos físicos de su entorno.Análisis e interpretación de fenómenos físicos.",
  "Conocer y socializarse con el estudio de la química y los pilares que la sustentan",
  "Identificar leyes que marcan la materia como una sola",
  "Identificar los conocimientos de números naturales y enteros",
  "Pensamiento analitico al conocer las leyes físicas",
  "Química,Estadística ,matemáticas",
  "Abordaje del paciente, primeros auxilios, anatomía, biología, química, salud pública",
  "Competencias en Física fundamental/Física Comprensión y aplicación de conceptos fundamentales de la física",
  "Identificar leyes que marcan la materia como una sola, descubrir la diferencia entre masa y peso",
  "Conocer y socializarse con el estudio de la química y los pilares que la sustentan",
  "Aplicar el pensamiento lógico matemático",
  "Interpretan los fenómenos naturales para hallar una solución a sus problemas",
  "Competencias científicas y lógico-matemáticas",
  "Abordaje del paciente,  primeros auxilios,  anatomia , Biología,  Quimica,  Salud Publica",
  "Competencias científicas y lógico-matemáticas",
  "Competencias en Física fundamental/Física Comprensión y aplicación de conceptos fundamentales de la física para analizar fenómenos naturales.  Uso del método científico y experimental para validar hipótesis y resolver problemas físicos.  Desarrollo del pensamiento analítico y lógico a través del análisis de datos experimentales.  Habilidad para interpretary e integrar resultados estadísticos en la comprensión de fenómenos físicos.  Capacidad para relacionar la teoría con la práctica, apoyando la enseñanza en niveles medio y medio superior.  Preparación para desarrollar investigaciones básicas y aplicadas en física con rigor científico.  Competencias en Estadística aplicada a la educación Dominio de técnicas estadísticas para la recopilación, organización y análisis de datos educativos.  Habilidad para interpretar resultados estadísticos que permitan caracterizar el desempeño y desarrollo de estudiantes.  Aplicación de la estadística como herramienta para la toma de decisiones fundamentadas en contextos educativos, especialmente en educación física.  Desarrollo de capacidades para diseñar y evaluar estrategias pedagógicas basadas en evidencias cuantitativas.  Potenciación del pensamiento crítico y lógico en el manejo de información numérica y gráfica.  Uso de resultados estadísticos para mejorar métodos de enseñanza, programas de estudio y evaluación.",
  "Conocer y socializarse con el estudio de la química y los pilares que la sustentan, utilizando métodos deductivos.   Identificas y comprender las propiedades y los cambios de la materia, y estados de agregación.  Expresar aplicaciones de los cambios de la materia en los fenómenos que observa en el entorno.",
  "Identificar leyes que marcan la materia como una sola, comprender la funcionalidad de los espacios vectoriales y los movimientos que se realizan en los planos, Descubrir la diferencia que existe entre masa y peso y su uso",
  "Abordaje del paciente,  primeros auxilios,  anatomia , Biología,  Quimica,  Salud Publica",
  "Competencias científicas y lógico-matemáticas",
  "Competencias en Física fundamental/Física Comprensión y aplicación de conceptos fundamentales de la física para analizar fenómenos naturales.  Uso del método científico y experimental para validar hipótesis y resolver problemas físicos.  Desarrollo del pensamiento analítico y lógico a través del análisis de datos experimentales.  Habilidad para interpretary e integrar resultados estadísticos en la comprensión de fenómenos físicos.  Capacidad para relacionar la teoría con la práctica, apoyando la enseñanza en niveles medio y medio superior.  Preparación para desarrollar investigaciones básicas y aplicadas en física con rigor científico.  Competencias en Estadística aplicada a la educación Dominio de técnicas estadísticas para la recopilación, organización y análisis de datos educativos.  Habilidad para interpretar resultados estadísticos que permitan caracterizar el desempeño y desarrollo de estudiantes.  Aplicación de la estadística como herramienta para la toma de decisiones fundamentadas en contextos educativos, especialmente en educación física.  Desarrollo de capacidades para diseñar y evaluar estrategias pedagógicas basadas en evidencias cuantitativas.  Potenciación del pensamiento crítico y lógico en el manejo de información numérica y gráfica.  Uso de resultados estadísticos para mejorar métodos de enseñanza, programas de estudio y evaluación.",
  "Conocer y socializarse con el estudio de la química y los pilares que la sustentan, utilizando métodos deductivos.   Identificas y comprender las propiedades y los cambios de la materia, y estados de agregación.  Expresar aplicaciones de los cambios de la materia en los fenómenos que observa en el entorno.",
  "Identificar leyes que marcan la materia como una sola, comprender la funcionalidad de los espacios vectoriales y los movimientos que se realizan en los planos, Descubrir la diferencia que existe entre masa y peso y su uso",
  "Competencias científicas y lógico-matemáticas",
  "Competencias en Física fundamental/Física Comprensión y aplicación de conceptos fundamentales de la física para analizar fenómenos naturales.  Uso del método científico y experimental para validar hipótesis y resolver problemas físicos.  Desarrollo del pensamiento analítico y lógico a través del análisis de datos experimentales.  Habilidad para interpretar e integrar resultados estadísticos en la comprensión de fenómenos físicos.  Capacidad para relacionar la teoría con la práctica, apoyando la enseñanza en niveles medio y medio superior.  Preparación para desarrollar investigaciones básicas y aplicadas en física con rigor científico.  Competencias en Estadística aplicada a la educación Dominio de técnicas estadísticas para la recopilación, organización y análisis de datos educativos.  Habilidad para interpretar resultados estadísticos que permitan caracterizar el desempeño y desarrollo de estudiantes.  Aplicación de la estadística como herramienta para la toma de decisiones fundamentadas en contextos educativos, especialmente en educación física.  Desarrollo de capacidades para diseñar y evaluar estrategias pedagógicas basadas en evidencias cuantitativas.  Potenciación del pensamiento crítico y lógico en el manejo de información numérica y gráfica.  Uso de resultados estadísticos para mejorar métodos de enseñanza, programas de estudio y evaluación."
  
)

### tecnicas y procedimientales
valores_competencia_procedimental <- c(
  "Adquiere criterios primarios de la electricidad su origen.",
  "Comprensión y aplicación de la electricidad",
  "Desarrolla habilidades en el uso de herramientas básicas e instalación básica.",
  "El estudiante es capaz de realizar una instalación eléctrica de fuerza e iluminación de una vivienda",
  "Que los jóvenes tengan velocidad, precisión y atención en el detalle en las destrezas de consentración en la mecanografía",
  "Uso de las medidas métricas, pulgadas, Pies y centímetros",
  "Adquiere criterios primarios de la electricidad su origen. Ejecutar técnicas con efectividad y calidad en el manejo de la herramienta",
  "Aplica los principios tecnológicos con normas de calidad, seguridad, higiene y pertinencia en procesos productivos",
  "Aplicar técnicas para realizar dibujos de acuerdo a normativas de construcción",
  "Desarrollar habilidades culinarias",
  "Higiene y seguridad alimentaria",
  "Desarrollar sus habilidades motrices",
  "Desarrollar sus habilidades motrices utilizando diversas técnicas",
  "Sus destrezas y habilidades para realizar su trabajo en el área ocupacional",
  "Competencias técnicas y procedimentales",
  "Adquiere criterios primarios de la electricidad su origen.  Ejecutar técnicas con efectividad y calidad en el manejo de la herramienta. Promover acciones del manejo de información adecuada que le induce al mejoramiento de su capacidad de análisis en relación a la electricidad . Formular proyectos viables que propiciar el mejoramiento familiar , escolar  y comunitario.",
  "Aplica los principios tecnológicos con normas de calidad, seguridad, higiene y pertenencia en procesos de belleza.",
  "Aplicar tecnicas para realizar dibujos de acuerdo a normativas de construccion",
  "Competencias técnicas y procedimentales",
  "Diseño de jardines, manejo agronómico de plantas ornamentales, realización de huertos verticales, cuadros vivos; entre otras",
  "EL ALUMNO APRENDE A MANEJAR  ADECUADAMENTE LA CRIANZA Y ALIMENTACIÓN Y SANIDAD DE LOS ANIMALES",
  "Habilidades y destrezas en la elaboración de los diferentes platillo gastronómicos.",
  "Sus destrezas y habilidades para realizar  su trabajo en el área ocupacional",
  "Tener e doinio de cada ua de las Áreas",
  "Competencias técnicas y procedimentales",
  "Que el alumno desarrolle habilidades para dibujar con conocimientos técnicos y de construcción con fines productivos basados en emprendimiento.",
  "Que el alumno egresado de la orientación ocupacional de electricidad posee un conjunto de conocimientos y habilidades que lo preparan para continuar estudios en la rama de la electricidad en el ciclo diversificado",
  "Que el egresado de la orientación de electricidad posea un conjunto de conocimientos y habilidades que lo preparan para continuar estudios en la rama de la electricidad en el ciclo diversificado",
  "Desarrollar habilidades y destrezas en la aplicación de secuencias didácticas en el aula con estudiantes de nivel medio.",
  "Desarrollar habilidades y destrezas para aplicarlas dentro del salón de clases con los estudiantes del nivel medio"
  
)

### gestion administración y conta
valores_gestion_contabilidad <- c(
  "Aplica los fundamentos de la contabilidad y la lógica contable",
  "Aplica principios contables.",
  "Aprender la contabilidad",
  "Busca analizar el manejo correcto de los procesos administrativos y financieros",
  "Comprensión y aplicación de principios contables",
  "Conocimientos contables, de impuesto y tecnológico",
  "Desarrolla técnicas para elaborar los libros",
  "El área de Contabilidad desarrolla competencias como el registro y control de operaciones financieras",
  "Identificar los principales libros contables",
  "Manejar los principios y las normas internacionales de contabilidad",
  "Registrar correctamente las operaciones económicas de una empresa",
  "Aplica los fundamentos de la contabilidad y la lógica contable",
  "Aplica principios contables. Demuestra dominio de las cuentas patrimoniales",
  "Busca analizar el manejo correcto de los procesos administrativos y financieros",
  "Comprensión y aplicación de principios contables, registro y control de operación",
  "Desarrolla técnicas para elaborar los libros y saber cómo declarar en SAT",
  "Identificar los principales libros contables y cuentas del activo y pasivo",
  "Manejar los principios y las normas internacionales de contabilidad",
  "Registrar correctamente las operaciones económicas de una empresa",
  "Gestión, administración y contabilidad",
  "Desarrollar habilidades y tecnicas comerciales y personales",
  "Aplica los fundamentos de la contabilidad y la lógica contable, en el proceso de la construcción de la contabilidad.",
  "Aplica principios contables. Demuestra dominio de las cuentas patrimoniales. Valora la legislación guatemalteca para aplicar la contabilidad.",
  "Busca analizar el manejo correcto de los procesos administrativos y financieros de las entidades administrativas privadas y públicas",
  "Comprensión y aplicación de principios contables, Registro y control de operación, análisis e interpretación de información financiera, responsabilidad y ética profesional, entre otros.",
  "Conocimientos contables, de impuesto y tecnológico donde el estudiante se informe y se capacite como manejar su economía",
  "Desarrolla técnicas para elaborar los libros, y prepararlos para la vida. Administra responsablemente los bienes que tiene.  Aplica lo aprendido para resolver problemas de la vida cotidiana, saber cómo declarar en SAT",
  "Gestión, administración y contabilidad",
  "Identificar las caracteristicas de los libros auxiliares de la contabiludad, reconocer las cuentas contables y su uso, aplicar los peocesos contables para la construcción de los libros de la contabilidad",
  "Manejar los conceptos de la terminología contable y el uso adecuado de las cuentas, utilizando el debe y el haber de cada cuenta",
  "Manejar los principios y las normas internacionales de contabilidad y las normas internacionales de información financiera, en los procedimientos, técnicas y registro de operaciones contables.",
  "Registrar correctamente las operaciones económicas de una empresa, así como organiza y analiza la información financiera  para conocer la situación de una empresa.  Con ello es capaz de planificar, evaluar y comunicar datos contables de una manera práctica.",
  "Valora la importancia de administrar los bienes y recursos financieros para el desarrollo familiar y comunitario",
  "desarrollo intelectual para realizar registros y control del area economico de cualquier empresa",
  "es la persona que se nombra como contadora que se encarga de estar atenta junto a la directora",
  "manejar lo referente a la ciculación de bienes y servicios en la economía de un país",
  "Que lleven conocimientos básicos de contabilidad al elegir una carrera en perito contador",
  "Desarrollar en el estudiante interes por aprender y desenvolverse sin problemas en la economía que maneja diariamente de diferentes maneras",
  "El área de Contabilidad desarrolla competencias como el registro y control de operaciones financieras, la interpretación y análisis de estados financieros, el uso de normas contables y fiscales, así como la capacidad de organización, responsabilidad, pensamiento crítico y toma de decisiones en el ámbito administrativo y empresarial.",
  "Gestión, administración y contabilidad",
  "Identificar los principales libros contables; reconocer las principales cuentas del activo y el pasivo; elaborar los principales libros auxiliares de la contabilidad",
  "La comprensión lógica de las transacciones económicas. Identificación del manejo de los recursos económicos d ella entidad. El uso adecuado de los libros contables en las transacciones económicas de las entidades lucrativas y no lucrativas. Uso y aplicación de las leyes tributarias de Guatemala.",
  "Que conozcan y apliquen los principios, tecnicas y procedimientos contables de acuerdo a las NIC's.  Que pongan en práctica las habilidades,técnicas y procedimientos en la práctica de la contabilidad en proyectos productivos.adquiridas en proyectos productivos de su comunidad y su entorno. Que sean capacidad de desarrollar un proyecto de emprendimiento de acuerdo a las habilidades adquiridas en la subárea ocupacional. Que desarrolle un plan empresarial, aplicando técnicas de administración en proyectos de emprendimiento."
  
)

### emprendimiento y productividad
valores_emprendimiento_productividad <- c(
  "Ejecuta iniciativas emprendedoras orientadas a la productividad con responsabilidad social.",
  "Ejecuta proyectos emprendedores de Manualidades y Decoración",
  "Emplear todo el conocimiento adquirido para poder realizar algún proyecto de emprendimiento propio",
  "Incentivar al estudiante a generar ingresos con los recursos que cuenta",
  "Que sean capaces de crear su propio negocio",
  "Plantea formas de organización de trabajo para mejorar las condiciones de vida",
  "Planifica procesos que integran normas de calidad y técnicas adecuadas en procesos productivos",
  "Plantea formas de organización de trabajo caracterizadas por el uso de recursos locales",
  "Diseñar propuestas de proyectos emprendedores de Floricultura",
  "Ejecuta iniciativas emprendedoras orientadas a la productividad con responsabilidad social",
  "Ejecuta proyectos emprendedores de Cocina y Repostería",
  "Emplear todo el conocimiento adquirido para realizar algún proyecto de emprendimiento propio",
  "Incentivar al estudiante a generar ingresos con los recursos que cuenta",
  "Emprendimiento y productividad",
  "1.	Compara opciones de solución a problemas comunitarios en base a la información sobre desarrollo humano proveniente de la agropecuaria. 2.	Planifica procesos que integran normas de calidad, técnicas adecuadas en el desarrollo de procesos productivos. 3.	Plantea formas de organización de trabajo caracterizadas por el uso de recursos locales, incorporación de valores culturales y ambientales para     mejores condiciones de vida.",
  "Aplica los principios tecnológicos con normas de calidad, seguridad, higiene y pertinencia en procesos de producción de Cocina y Repostería. 2- . Ejecuta proyectos emprendedores de Cocina y Repostería para contribuir al desarrollo local comunitario.",
  "Aplica principios tecnológicos, proyectos emprendedores y procesos productivos",
  "Diseñar propuestas de proyectos emprendedores de Floricultura para contribuir en la solución de problemáticas de la comunidad y aportar en el aspecto económico, familiar y local.  Desarrollar habilidades en el manejo de especies ornamentales para su siembra, cultivo y comercialización.",
  "Ejecuta iniciativas emprendedoras orientadas a la productividad con responsabilidad social. Emprende procesos productivos mediante el uso de la tecnología para mejorar la calidad en la producción de bienes y servicios. Administra recursos económicos y financieros en el desarrollo de proyectos sustentables y sostenibles, familiares y comunitarios",
  "Ejecuta proyectos emprendedores de Manualidades y Decoración, para contribuir al desarrollo local comunitario.",
  "Ejecuta proyectos. Usa tecnología.",
  "Ejecutar proyectos de manualidades y decoración para contribuir al desarrollo local comunitario",
  "Emplear todo el conocimiento adquirido para en el curso y poder realizar algún proyecto de emprendimiento propio",
  "Emprendimiento y productividad",
  "Incentivar al estudiante a generar ingresos con los recursos que cuenta y a poner en práctica lo aprendido en el área de especialización",
  "Promueve el mejoramiento constante y progresivos en las actividades humana orientada el desarrollo comunitario sostenible",
  "Promueve la organización comunitaria que satisfaga las necesidades productivas de la comunidad y se oriente hacia el desarrollo sostenible.",
  "Que puedan desarrollar y aplicar el aprendizaje de su carrera para poder entrar en el gambito laborar y desenvolverse de manera autónoma",
  "Que sea capaz de cultivar las principales hortalizas de la region"
)

### artísticas y creativas
valores_competencia_creativa <- c(
  "Combinar elementos como linea, forma, color y textura",
  "Crear, motricidad",
  "Creatividad e imaginacion",
  "Desarrollar su motricidad fina y gruesa y que pueda expresar sus sentimientos y emociones por medio del arte",
  "En teatro es la habilidad de desarrollar una obra teatral",
  "Habilidad para escuchar, analizar y valorar la música",
  "Utiliza los trece signos teatrales y elementos básicos de la actuación",
  "Combinar elementos como línea, forma, color, volumen y textura",
  "Comprender el mundo del diseño",
  "Conocer la historia de la humanidad a través del arte",
  "En teatro es la habilidad de desarrollar una obra teatral",
  "Habilidad para escuchar, analizar y valorar la música",
  "Se enfoca en el desarrollo de habilidades para la expresión corporal y vocal",
  "Artísticas y creativas",
  "Combinar elementos como.linea, formación,  color ,volumen.en , textura, experimentar competencias y materiales para elaborar retratos, paisajes, utilizandambra y la perspectiva para crear efectos visuales.",
  "Conocer la historia de la humanidad a través de arte",
  "En teatro es la habilidad de desarrollar  una obra teatral y en visuales  adquieran las habilidad de crear paisaje en oleo",
  "Habilidad para escuchar, analizar y valorar la música, reconociendo sus elementos (melodía, ritmo, armonía, dinámica) y entendiendo su valor cultural y artístico.",
  "Participa en actividades que promueven la conservación del ambiente ecológico-acústico que le rodea. Esto implica hacerse consciente del sonido, el ruido, la salud auditiva, y del entorno acústico. Expresa ideas, emociones, actitudes y valores por medio de la música, individual o grupalmente, de manera vocal, instrumental o mixta.  Emite juicios sobre creaciones musicales y diferentes manifestaciones estético-sonoras. Aquí entra la apreciación musical, diferenciar estilos, épocas, valorar lo que se escucha.  Aplica principios, conocimientos y técnicas musicales a su alcance en la realización de creaciones propias, vocales o instrumentales. Incluye uso del lenguaje musical, creación, interpretación, prácticas musicales.",
  "Se enfoca en el desarrollo de habilidades para la expresión corporal y vocal, la creación artística, la interpretación de obras, el trabajo en equipo."
  
)

### socioemocionales, valores y formación integral 
valores_compentencia_emocional <- c(
  "Apoyar a los estudiantes en los problemas que presentan, familiares o emocionales",
  "Convivencia pacífica, resolución de conflictos, apoyo emocional",
  "Comprender por medio de valores la moral humana",
  "Gormar jóvenes con valores y responsables",
  "Reconocer y practicar los principios eticos",
  "VALORES, CRECIMIENTO EDUCATIVO INTEGRAL",
  "Vivencia de valores",
  "Convivencia pacífica, resolución de conflictos, apoyo emocional",
  "Entregar a la sociedad profesionales con principios y valores éticos",
  "Reconocer la capacidad de crecimiento personal y aceptar las diferencias",
  "Que aprenda la importancia de una buena conducta en el ámbito laboral",
  "Valores",
  "Socioemocionales, valores y formación integral",
  "Ética Profesional estudia desde sus orígenes hasta la modernidad",
  "Agilidad",
  "Convivencia pacífica, resolución de conflictos, apoyo emocional, entre otros.",
  "Contemplar , Meditar y actuar",
  "Desarrollar un sentido de amor a Dios y al prójimo a través de la Biblia fomentando principios cristianos para su desarrollo profesional y espiritual. En el grado de segundo primaria  que los niños puedan desarrollan su sentido crítico, analítico y resolución de problemas en el.contexto que vive. Adquiriendo aprendizajes científicos, prácticos útiles para su vida.",
  "Entregar a la sociedad profesionales con principios y valores ético, con inteligencia emocional, capaces de desarrollarse en el área para lo cual fueron formados.",
  "Las características es como interactuar las habilidades y destrezas que posee cad estudiante y aprender sobre la sdnazas que existen en Guatemala",
  "Que aprenda la importancia de una buena conducta en el ámbito laboral , que su conicimiento para llevar una buena economía sea viable , que sus relaciones personales sean  más saludables y aprendan a practicar la solidaridad",
  "Reconocer la capacidad de crecimiento personal, y aceptando las diferencias de los demás como criatura de Dios.",
  "Socioemocionales, valores y formación integral", "valores ..",
  "Ética Profesional estudia desde sus orígenes hasta la modernidad en relación a la vida personal, social cultural religioso y laboral"
  
)

### ciudadana, intercultural y social
valores_ciudadana <- c(
  "Conocer la interculturalidad",
  "Conocer, valorar y cultivar en cada estudiante el interés de conocer Guatemala.",
  "Que el estudiante relacione su contexto socio económico con su propia realidad",
  "Sociales el estudiante conozca la historia",
  "Solucion de problemas , conocimiento de su pais",
  "Promueve la organización comunitaria orientada al desarrollo sostenible",
  "Que conozca las leyes más importantes del país y las aplique en su diario vivir",
  "Competencias ciudadanas e interculturales",
  "Sociales el estudiante conozca la historia, idioma maya, costumbres y tradiciones",
  "Valora la importancia de administrar los bienes y recursos financieros para el desarrollo comunitario",
  "Analizar temas filósoficos y lógicos. Conocer el sistema monetario y su función. Analizar el proceso administrativo. Clasificar y describir la importancia del medio ambiente.",
  "Competencias ciudadana e interculturales",
  "Conocer su entorno natural y social como conocer su cuerpo y desarrollo",
  "Conserva el entorno natural y la salud individual y colectiva,  comunicarse en un medio multicultural y plurilingue,  aplicar el pensamiento lógico matemático,  utilizar la tecnología de manera productiva,  relacionarse y cooperar con un conjunto de personas , actuar con valores en un entorno ciudadano, Especializarse, Aplicar principios aprendidos a la práctica en contextos específicos y cotidianos,  Actuar con autonomía e iniciativa personal, Aprender a aprender.",
  "Historia presocraticos Sofistas, Socrates, Platon, Aristóteles, Edad Media, Escolastica, Edad Moderna, Revolución religiosa y científica, Empirismo, Ilustración",
  "Identifiquen los principales conceptos de la psicología y la apliquen en su entorno personal, académico y laboral",
  "Que el estudiante describe la administración pública guatemalteca y saber ejercerlo en la aplicación.",
  "Quegonozca las leyes más importantes del país y las aplique en su diario vivir",
  "Sociales el estudiante conozca la historia. Idioma ma maya Que elvestudiante conzca elvidioma costumbres y tradiciones. Geografía económica Que el estudiante conozca la ubicación geográgica y ubicación de producción"
  
)

### no aplica
valores_noaplica_area2 <- c(
  "Ninguna","Ninguna más", "Ningún", "No","No aplica",
  "No hay","No sé cuáles",
  "No aplica","No hay doy ningún otro curso",
  "Director sin Grado","Soy personal administrativa",
  "Libros y páginas web","Tener el orden en papelería",
  "Que el aprendizajes sean exitosos",
  "Eficiencia y calidad en la elaboración de documentos",
  "El conocimiento de las tecnologías y medios digitales",
  "En el área de secretaria llevar control de todos quee estén inscritos realizar informes",
  "Fortalecer la administración Educativa",
  "Habilidades  analíticas, trabajo en equipo, presión en los registros contabilidad comprensión de conceptos de activo y pasivo",
  "No aplica",
  "Principios Contables, uso de herramientas tecnológicos, normas fiscales",
  "Tener el orden en papeleria",
  "tiene como propósito orientar a los docentes que imparten el área o asignatura de Seminario en cuanto a los contenidos, metodología e instrumentos que utilizarán en las sesiones de trabajo con sus estudiantes del último grado del Ciclo de Educación Diversificada del Nivel de Educación Media.",
  "¿Qué es Seminario?  Es una subárea del área de Investigación en el Bachillerato en Ciencias y Letras.  cnbguatemala.org +2 Scribd +2  En Seminario, los estudiantes desarrollan proyectos de investigación‐acción que les permiten acercarse a problemas reales de su contexto (familiar, comunitario, nacional) para entenderlos, analizarlos y proponer soluciones.  Scribd +2 cnbguatemala.org +2  Tiene un fuerte componente reflexivo: se trabaja sobre valores, misión, visión, metas personales, su relación con la sociedad, y se articula con los ejes de la Reforma Educativa., Desarrollar la capacidad de plantear, planificar, ejecutar y evaluar proyectos de investigación, Elaborar informes finales aplicando criterios científicos,",
  "Modo IA Todo Imágenes Videos Noticias Videos cortos Libros Web Maps Finanzas Herramientas de búsqueda Comentarios Visión general creada por IA    +7 La competencia en la ética profesional es la capacidad de un profesional para identificar y aplicar principios y valores éticos en su toma de decisiones y práctica laboral"
  
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    competencias_otra_area = case_match(
      competencias_otra_area,
      all_of(valores_noaplica_area2) ~ "No aplica",
      all_of(valores_ciudadana) ~ "Competencias ciudadana e interculturales",
      all_of(valores_compentencia_emocional) ~ "Socioemocionales, valores y formación integral",
      all_of(valores_competencia_creativa) ~ "Artísticas y creativas",
      all_of(valores_emprendimiento_productividad) ~ "Emprendimiento y productividad",
      all_of(valores_gestion_contabilidad) ~ "Gestión, administración y contabilidad",
      all_of(valores_competencia_procedimental) ~ "Competencias técnicas y procedimentales",
      all_of(valores_competencia_cientifica) ~ "Competencias científicas y lógico-matemáticas",
      all_of(valores_competencia_comunicativa) ~ "Comunicativas y lingüisticas",
      all_of(valores_pensamiento_resolucion) ~ "Pensamiento crítico y resolución de problemas",
      .default = competencias_otra_area
    ) 
  )

## volver a ver errores

valores_competencias_otra_area <- df_dyd |>
  filter(!(is.na(competencias_otra_area))) |>
  select(competencias_otra_area) |>
  tabyl(competencias_otra_area)

#View(valores_competencias_otra_area)

#bilinguismo_extranjero ----
## ver errores
valores_bilinguismo_extranjero <- df_dyd |>
  filter(bilinguismo_idiomas == "Idioma extranjero") |>
  filter(bilinguismo == "Sí") |>
  filter(!(is.na(bilinguismo_extranjero))) |>
  select(bilinguismo_extranjero) |>
  tabyl(bilinguismo_extranjero)


## Define los vectores ----
### ingles
valores_ingles_idioma <- c(
  "Inglés"
)

## recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    bilinguismo_extranjero = case_match(
      bilinguismo_extranjero,
      all_of(valores_ingles_idioma) ~ "Inglés",
      .default = bilinguismo_extranjero
    )
  )

## volver a ver errores
valores_bilinguismo_extranjero <- df_dyd |>
  filter(bilinguismo_idiomas == "Idioma extranjero") |>
  filter(bilinguismo == "Sí") |>
  filter(!(is.na(bilinguismo_extranjero))) |>
  select(bilinguismo_extranjero) |>
  tabyl(bilinguismo_extranjero)

#View(valores_bilinguismo_extranjero)


# bilinguismo_otro_idioma ----
## ver errores

valores_bilinguismo_otro_idioma <- df_dyd |>
  filter(bilinguismo_idiomas == "Otro") |>
  filter(bilinguismo == "Sí") |>
  filter(!(is.na(bilinguismo_idiomas))) |>
  select(bilinguismo_otro_idioma)|>
  tabyl(bilinguismo_otro_idioma)

dput(valores_bilinguismo_otro_idioma)

## Define los vectores ----

### chorti
valores_chorti <- c(
  "Chorti"
)

### kaqchikel
valores_kaqchikel <- c(
  "Kachiquel",  "Kakchiquel"
)

### k'iche
valores_kiche <- c(
  "Kiche"
)

### mam
valores_mam <- c(
  "Mam"
)

### q'eqchi
valores_qeqchi2 <- c(
  "Qeqchi", "Queqchi"
)

## Recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    bilinguismo_otro_idioma = case_match(
      bilinguismo_otro_idioma,
      all_of(valores_qeqchi2) ~ "Q'eqchi'",
      all_of(valores_mam) ~ "Mam",
      all_of(valores_kiche) ~ "K'iche",
      all_of(valores_kaqchikel) ~ "Kaqchikel",
      all_of(valores_chorti) ~ "Chortí",
      .default = bilinguismo_otro_idioma
    )
  )

## volver a ver errores
valores_bilinguismo_otro_idioma <- df_dyd |>
  filter(bilinguismo_idiomas == "Otro") |>
  filter(bilinguismo == "Sí") |>
  filter(!(is.na(bilinguismo_idiomas))) |>
  select(bilinguismo_otro_idioma)|>
  tabyl(bilinguismo_otro_idioma)

#View(valores_bilinguismo_otro_idioma)

# bilinguismo_idioma_extranjero_docente ----

## ver errores
valores_bilinguismo_idioma_extranjero_docente <- df_dyd |>
  filter(bilinguismo_docente_con_estudiantes == "Idioma extranjero") |>
  filter(!(is.na(bilinguismo_idioma_extranjero_docente))) |>
  select(bilinguismo_idioma_extranjero_docente)|>
  tabyl(bilinguismo_idioma_extranjero_docente)

dput(valores_bilinguismo_idioma_extranjero_docente)

## Define los vectores ----

### ingles
valores_ingles_idioma2 <- c(
  "Inglés"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    bilinguismo_idioma_extranjero_docente = case_match(
      bilinguismo_idioma_extranjero_docente,
      all_of(valores_ingles_idioma2) ~ "Inglés",
      .default = bilinguismo_idioma_extranjero_docente
    )
  )

## volver a ver errores

valores_bilinguismo_idioma_extranjero_docente <- df_dyd |>
  filter(bilinguismo_docente_con_estudiantes == "Idioma extranjero") |>
  filter(!(is.na(bilinguismo_idioma_extranjero_docente))) |>
  select(bilinguismo_idioma_extranjero_docente)|>
  tabyl(bilinguismo_idioma_extranjero_docente)

#View(valores_bilinguismo_idioma_extranjero_docente)


# bilinguismo_otro_idioma_docente_con_estudiantes ----

## ver errores

valores_bilinguismo_otro_idioma_docente_con_estudiantes <- df_dyd |>
  filter(bilinguismo_docente_con_estudiantes == "Otro") |>
  filter(!(is.na(bilinguismo_otro_idioma_docente_con_estudiantes))) |>
  select(bilinguismo_otro_idioma_docente_con_estudiantes)|>
  tabyl(bilinguismo_otro_idioma_docente_con_estudiantes)

dput(valores_bilinguismo_otro_idioma_docente_con_estudiantes)

## Define los vectores ----

### castellano
valores_castellano <- c(
  "Casteyano"
)

### q'eqchi'
valores_qeqchi3 <- c(
  "Qeqchi"
)

## Recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    bilinguismo_otro_idioma_docente_con_estudiantes = case_match(
      bilinguismo_otro_idioma_docente_con_estudiantes,
      all_of(valores_qeqchi3) ~ "Q'eqchi'",
      all_of(valores_castellano) ~ "Castellano",
      .default = bilinguismo_otro_idioma_docente_con_estudiantes
    )
  )

## volver a ver errores
valores_bilinguismo_otro_idioma_docente_con_estudiantes <- df_dyd |>
  filter(bilinguismo_docente_con_estudiantes == "Otro") |>
  filter(!(is.na(bilinguismo_otro_idioma_docente_con_estudiantes))) |>
  select(bilinguismo_otro_idioma_docente_con_estudiantes)|>
  tabyl(bilinguismo_otro_idioma_docente_con_estudiantes)

#View(valores_bilinguismo_otro_idioma_docente_con_estudiantes)


# energia_otra_fuente ----

## ver errores

valores_energia_otra_fuente <- df_dyd |>
  filter(energia_fuente == "Otra fuente") |>
  filter(!(is.na(energia_otra_fuente))) |>
  select(energia_otra_fuente)|>
  tabyl(energia_otra_fuente)

dput(valores_energia_otra_fuente)

## Define los vectores ----

### Deocsa
valores_deocsa <- c(
  "Deocsa"
)

### energuate
valores_energuate <- c(
  "Energuate"
)

### municipalidad
valores_municipalidad <- c(
  "MUNCIPALIDAD", "MUNICIPAL", "Municipalidad"
)

### propia
valores_propia_energia <- c(
  "Somos INDE, tenemos fuente directa y propia."
)

### módulo
valores_modulo_energia <- c(
  "módulo"
)

### no aplica
valores_noaplica_energia <- c(
  "No aplica"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    energia_otra_fuente = case_match(
      energia_otra_fuente,
      all_of(valores_noaplica_energia) ~ "No aplica",
      all_of(valores_modulo_energia) ~ "Módulo",
      all_of(valores_propia_energia) ~ "Propia",
      all_of(valores_municipalidad) ~ "Municipalidad",
      all_of(valores_energuate) ~ "Energuate",
      all_of(valores_deocsa) ~ "Deocsa",
      .default = energia_otra_fuente
    )
  )

## volver a ver errores

valores_energia_otra_fuente <- df_dyd |>
  filter(energia_fuente == "Otra fuente") |>
  filter(!(is.na(energia_otra_fuente))) |>
  select(energia_otra_fuente)|>
  tabyl(energia_otra_fuente)

#View(valores_energia_otra_fuente)

# agua_razon ----

## ver errores
valores_agua_razon <- df_dyd |>
  filter(agua_centro == "No") |>
  filter(!(is.na(agua_razon))) |>
  select(agua_razon)|>
  tabyl(agua_razon)

dput(valores_agua_razon)

## Define los vectores ----

### inexistencia de red o instalación de agua
valores_sinred_agua <- c(
  "No cuenta con instalación de agua","No hay bomba electrica",
  "NO HYA RED DE AGUA POTABLE EN LA COMUNIDAD",
  "No hay distribución de sistema de agua",
  "No llega el agua de red municipal",
  "No hay fuente de agua cerca del centro educativo",
  "Problema de limites del área de la comunidad con otras",
  "Que el establecimiento pertenece a la comunidad y es área rural, nadie tiene agua potable"
)

### escasez de agua en comunidad o municipio
valores_escasez_agua <- c(
  "No hay agua potable",
  "Que en el municipio hay escacez del vital liquido y no llega al establecimiento.",
  "no hay agua en el caderio.",
  "Acceso limitado por diversos factores"
)

### abastecimiento limitado o por turnos
valores_limite_agua <- c(
  "Es intermitente y la comunidad la canta de un río y a veces se quiebran los tubos y no hay encargados de arreglarlos",
  "No llega por los turnos",
  "Solo hay agua solo viene en la noche",
  "se abastece de la red de agua comunitaria y rara vez hay turnos para la zona"
)

### dependencia de fuentes alternativas
valores_dependencia_agua <- c(
  "No hay acceso y solo se tiene pozo",
  "Por qué la comunidad no cuenta con agua potable ,cada persona lleva su agua, y  nuestro centro tambien  se compra para tener el metal líquido."
)

## Recode con case match ----
df_dyd  <- df_dyd |>
  mutate(
    agua_razon = case_match(
      agua_razon,
      all_of(valores_dependencia_agua) ~ "Dependencia de fuentes alternativas",
      all_of(valores_limite_agua) ~ "Abastecimiento limitado o por turnos",
      all_of(valores_escasez_agua) ~ "Escasez de agua en la comunidad o municipio",
      all_of(valores_sinred_agua) ~ "Inexistencia de red o instalación de agua",
      .default = agua_razon
    )
  )

## volver a ver errores

valores_agua_razon <- df_dyd |>
  filter(agua_centro == "No") |>
  filter(!(is.na(agua_razon))) |>
  select(agua_razon)|>
  tabyl(agua_razon)

#View(valores_agua_razon)



# cat_tiempo ----

## ver errores

valores_cat_tiempo <- df_dyd |>
  filter(cat_laboratorio_centro == "No") |>
  filter(cat_encargado == "No") |>
  filter(!(is.na(cat_tiempo))) |>
  select(cat_tiempo)|>
  tabyl(cat_tiempo)

dput(valores_cat_tiempo)

## Define lso vectores ----
### teorícamente
valores_teoria_cat <- c(
  "A través de enseñanza teórica de programa como Word, PowerPoint otros",
  "Clases teóricas por no contar con laboratorio",
  "Contenido teórico",
  "Dando las áreas teóricas",
  "PREPARCION PERSONAL Y APOYO TEORICO DE DOCENTES SIN PRACTICA",
  "Se da solo parte teórica",
  "Solo contenido teórico",
  "Sólo teoria",
  "Teóricamente",
  "Teórico", "Teoría impartida a estudiantes y otros Centros de Computación",
  "solo parte se imparte teoría",
  "teoría por el maestro"
)

### recursos propios (celular e internet)
valores_propio_cat <- c(
  "Con algunas computadoras aún buenas t con y con tel movil",
  "Con herramientas propias de los jóvenes",
  "Con teoría y utilizando el teléfono celular",
  "Por medio de celulares",
  "Teléfono propio","Personal",
  "Usan los celulares para el estudio",
  "Usando internet propio"
)


### material impreso, digital o audiovisual
valores_material_cat <- c(
  "CON FOLLETOS Y LIBROS PROPIOS",
  "Con material digital y audiovisual",
  "Solo material", "Investigar en librerías",
  "Tareas en el aula",
  "Temas del CNB"
)

### reciben en otras instituciones o centros
valores_practica_tac <- c(
  "PRACTICA",
  "Práctica personal",
  "Práctico en un centro de Internet y teórico en el instituto",
  "Teoría impartida a estudiantes y otros Centros de Computación",
  "Academia particular", "Intecap","Optativo estudiar en una institución privada",
  "El establecimiento se ubica en area rural y los jovenes, viajan al casco urbano para recibir la información.",
  "En otro lugar fuera del centro educativo por falta de tecnología propia."
  )

### no especifica
valores_sinespecificar_tac <- c(
  "30 minutos","40 por curso","Hay un horario para cada curso",
  "Los estudiantes eligen un horario distinto al de la jornada.",
  "Organización de Horario", "Por periodos",
  "Por períodos","Se solventa por períodos","Se encuentra en horario e integral",
  "Tiempo extraordinario", "Una vez a la semana","Reciben el curso aparte"
)

### integración con otras áreas
valores_integracion_tac <- c(
  "Cubriendo con otros periodos", "Implementando", "Implementando una área",
  "Reorganización","Se maneja de manera integrada",
  "Según lo requiere las áreas transversales",
  "Mediante el docente del curso", "Por medio del docente del curso"
)

### no se solventa
valores_sinsolvencia <- c(
  "Ninguno",
  "No contamos con espacio ni recursos",
  "No cuenta","No hay",
  "No tenemos","No tiene",
  "Si se solventa",
  "ningun tiempo.", "no se imparte"
)

## Recode con case match ----
df_dyd <- df_dyd |>
  mutate(
    cat_tiempo = case_match(
      cat_tiempo,
      all_of(valores_sinsolvencia) ~ "No se solventa",
      all_of(valores_integracion_tac) ~ "Se integran otras áreas",
      all_of(valores_sinespecificar_tac) ~ "No especifica",
      all_of(valores_material_cat) ~ "Se imparte con material impreso, digital o audiovisual",
      all_of(valores_propio_cat) ~ "Se utilizan recursos propios (celular, internet)",
      all_of(valores_practica_tac) ~ "Reciben en otras instituciones o centros",
      all_of(valores_teoria_cat) ~ "Reciben únicamente teoría",
      .default = cat_tiempo
    )
  )

## volver a ver errores


valores_cat_tiempo <- df_dyd |>
  filter(cat_laboratorio_centro == "No") |>
  filter(cat_encargado == "No") |>
  filter(!(is.na(cat_tiempo))) |>
  select(cat_tiempo)|>
  tabyl(cat_tiempo)

#View(valores_cat_tiempo)

# visita_supervisor_otra_temporalidad ----
## ver errores

valores_visita_supervisor_otra_temporalidad <- df_dyd |>
  filter(visita_supervisor == "Otra temporalidad") |>
  filter(!(is.na(visita_supervisor_otra_temporalidad))) |>
  select(visita_supervisor_otra_temporalidad)|>
  tabyl(visita_supervisor_otra_temporalidad)

dput(valores_visita_supervisor_otra_temporalidad)

## Define los vectores ----

### cuando se requiere 
valores_requerimiento <- c(
  "CADA VEZ QUE SE NECESITA DE SU APOYO",
  "CUANDO EL CONSIDERA NECESARIO.",
  "CUANDO ES NECESARIA SU PRESENCIA",
  "Cuando es necesario",
  "Actividad de supervisión, capacitación.",
  "Cuando  llega a resolver problemas  o hacer visitas oculares",
  "En alguna actividad",
  "Monitoreos de alimentación",
  "Solo por si lo requiere alguna otra institución",
  "Cuando hay necesidad hace la visita la persona encargada",
  "Cuando las circunstancias lo ameritan. Pero se mantiene en comunicación constante con la dirección.",
  "Cuando se presenta las necesidades, la persona encargada realiza las visitas correspondientes",
  "En el momento que lo requiera el centro educativo",
  "Según la necesidad"
)

### ocasionalmente
valores_ocasionalmente <- c(
    "A veces", "Casi nunca",
    "De vez en cuando",
    "Debes en cuando", "Visita el centro educativo y tiene comunicación con la directora",
    "En vez encuanto","En los momentos y espacios que considera llegar, no hay períodos fijos",
    "No se programa, visita sin previo aviso",
    "Otra","Poca","Temporalmente"
  )

### según fechas programadas por supervisión
valores_programacion <- c(
  "Al año o esto varía en meses, etc.",
  "Cuando cubren su planificación",
  "Fechas programadas por supervisión o cuando se lo solicita",
  "Fechas programas por Supervisión o se le solicite en el establecimiento",
  "Por medio de citas"
)

### no especifica
valores_sinespecificar_sup <- c(
  "...",
  "0",
  "no he tenido contacto con ellos"
)

### cada 3 a 5 meses
valores_meses_sup <- c(
  "3 meses","Cada 4 meses", "Cada 4 o 5 meses",
  "Semestral", "Trimestral"
)

### una o dos veces año
valores_ano_sup <- c(
  "2 años", "Cada año", "Una vez al año"
)

### todos los días
valores_diariamente <- c(
  "Diario porque allí tiene su oficina"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    visita_supervisor_otra_temporalidad = case_match(
      visita_supervisor_otra_temporalidad,
      all_of(valores_diariamente) ~ "Todos los días",
      all_of(valores_ano_sup) ~ "Cada año",
      all_of(valores_meses_sup) ~ "Cada 3 a 5 meses",
      all_of(valores_sinespecificar_sup) ~ "No especifica",
      all_of(valores_programacion) ~ "Según programación de supervisión",
      all_of(valores_ocasionalmente) ~ "Ocasionalmente",
      all_of(valores_requerimiento) ~ "Cuando se requiere",
      .default = visita_supervisor_otra_temporalidad
    )
  )

## volver a ver errores

valores_visita_supervisor_otra_temporalidad <- df_dyd |>
  filter(visita_supervisor == "Otra temporalidad") |>
  filter(!(is.na(visita_supervisor_otra_temporalidad))) |>
  select(visita_supervisor_otra_temporalidad)|>
  tabyl(visita_supervisor_otra_temporalidad)

#View(valores_visita_supervisor_otra_temporalidad)


# visita_coordinador_otra_temporalidad ----

## ver errores

valores_visita_coordinador_otra_temporalidad <- df_dyd |>
  filter(visita_coordinador == "Otra temporalidad") |>
  filter(!(is.na(visita_coordinador_otra_temporalidad))) |>
  select(visita_coordinador_otra_temporalidad)|>
  tabyl(visita_coordinador_otra_temporalidad)

dput(valores_visita_coordinador_otra_temporalidad)

## Define los vectores ----

### 3 meses

valores_3meses <- c(
  "3 meses", "3"
)

### 1 a 2 años
valores_2tiempo <- c(
  "2", "Cada dos años", "Dos años", "Una vez en la temporada", "1"
)

### cada 3 años
valores_3tiempo <- c(
  "Cada 3 años"
)

### cada 5 años
valores_5tiempo <- c(
  "Cada 5 años","Cada dos años", "5 años"
)

### cuando se requiere
valores_requerimiento2 <- c(
  "Cuando es necesario y oportuno.",
  "Cuando es requerido",
  "Cuando se le solicita",
  "Cuando se necesita","Cuado trata asuntos administrativos con la dirección que es muy raro que suseda",
  "En súpervisiones sorpresa",
  "Cuando se requiere",
  "Cuando sea necesario",
  "Cuando corresponda realizar investigaciones o actuvidades"
)

### ocasionalmente
valores_ocasionalmente2 <- c(
  "Casi nunca","De vez en cuando",
  "Debes en cuando","En vez en cuando",
  "Poca"
)

### segun programacion
valores_programacion2 <- c(
  "Cuando cumple con su planificación",
  "Por medio de citas", "Según agenda",
  "Según su agenda", "Por comisiones"
)

### no especifica
valores_sinespecificar_coo <- c(
  "No hay fechas para especificar"
)

### No ha visitado
valores_sinvisita <- c(
  "NO ASISTE","Ni una",
  "Desconozco ya que nunca ha llegado desde que trabajo en el Centro.",
  "Por ser nuevo director no he recibido ninguna visita"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    visita_coordinador_otra_temporalidad = case_match(
      visita_coordinador_otra_temporalidad,
      all_of(valores_sinvisita) ~ "No ha visitado",
      all_of(valores_sinespecificar_coo) ~ "No especifica",
      all_of(valores_programacion2) ~ "Según programación de coordinación",
      all_of(valores_ocasionalmente2) ~ "Ocasionalmente",
      all_of(valores_requerimiento2) ~ "Cuando se requiere",
      all_of(valores_5tiempo) ~ "Cada 5 años",
      all_of(valores_3tiempo) ~ "Cada 3 años",
      all_of(valores_2tiempo) ~ "Cada 2 años",
      all_of(valores_3meses) ~ "Cada 3 meses",
      .default = visita_coordinador_otra_temporalidad
      )
  )

## volver a ver errores

valores_visita_coordinador_otra_temporalidad <- df_dyd |>
  filter(visita_coordinador == "Otra temporalidad") |>
  filter(!(is.na(visita_coordinador_otra_temporalidad))) |>
  select(visita_coordinador_otra_temporalidad)|>
  tabyl(visita_coordinador_otra_temporalidad)

View(valores_visita_coordinador_otra_temporalidad)

#visita_otro_personal_otra_temporalidad ----

## ver errores

valores_visita_otro_personal_otra_temporalidad <- df_dyd |>
  filter(visita_otro_personal == "Otra temporalidad") |>
  filter(!(is.na(visita_otro_personal_otra_temporalidad))) |>
  select(visita_otro_personal_otra_temporalidad)|>
  tabyl(visita_otro_personal_otra_temporalidad)

dput(valores_visita_otro_personal_otra_temporalidad)

## define los vectores ----

### cada año
valores_1personal <- c(
  "1 año", "A principios de ciclo escolar o a finales",
  "Fin de ciclo", "Año", "Un año",
  "Una vez en febrero 2024"
)

### cada 2 años
valores_2personal <- c(
  "2",
  "Dos años", "2 o 3 años",
  "No hay fechas puede cada 2 años o más"
)

### cada 5 años
valores_5personal <- c(
  "5"
)

### segun requerimiento
valores_requerimiento3 <- c(
  "Cuamdo hay encuesta",
  "Cuando algún caso lo requiera",
  "Cuando hay alguna queja de padres de familia",
  "Cuando hay algún  caso especial",
  "Cuando hay casos que resolver",
  "Cuando hay dificultades",
  "Cuando hay necesidad o problemas",
  "Cuando la circunstancias lo ameritan.",
  "Cuando se requiere","Cuando invitan a lo padres de la opf",
  "Cuando son nombrados",
  "talleres específicos", "Cuándo es necesario",
  "Es visitado cada ves cuando sea requerido"
)

### ocasionalmente
valores_ocasionalmente3 <- c(
  "De vez en cuando",
  "Eventual dependiendo de las demandas de trabajo",
  "Eventualmente", "Varia la temporalidad, puede ser 3 veces en un mismo mes o cada 3 meses",
  "eventualmente",
  "Relativo",
  "Variable"
)

### programadas
valores_programacion3 <- c(
  "Calendarizadas",
  "Según agenda", "Cuando tenga tiempo",
  "Según su agenda"
)

### no especifica
valores_sinespecificar_personal <- c(
  "No hay fecha recurrente o fija", "Cuatro veces en todo el tiempo.",
  "No hay fechas fijas y es de vez en cuando", "1 vez",
  "No tienen fechas", ".", "Una vez en febrero 2024"
)

### no hay visitas
valores_novisita <- c(
  "Ninguna"
)


## Recode con case match ----
df_dyd <- df_dyd  |>
  mutate(
    visita_otro_personal_otra_temporalidad = case_match(
      visita_otro_personal_otra_temporalidad,
      all_of(valores_novisita) ~ "No han visitado",
      all_of(valores_sinespecificar_personal) ~ "No especifica",
      all_of(valores_programacion3) ~ "Según programación del personal",
      all_of(valores_ocasionalmente3) ~ "Ocasionalmente",
      all_of(valores_requerimiento3) ~ "Cuando se requiere",
      all_of(valores_5personal) ~ "Cada 5 años",
      all_of(valores_2personal) ~ "Cada 2 años",
      all_of(valores_1personal) ~ "Cada año",
      .default = visita_otro_personal_otra_temporalidad
    )
  )

## volver a ver errores

valores_visita_otro_personal_otra_temporalidad <- df_dyd |>
  filter(visita_otro_personal == "Otra temporalidad") |>
  filter(!(is.na(visita_otro_personal_otra_temporalidad))) |>
  select(visita_otro_personal_otra_temporalidad)|>
  tabyl(visita_otro_personal_otra_temporalidad)

#View(valores_visita_otro_personal_otra_temporalidad)

# visita_digemoca_otra_temporalidad ----

## ver errores

valores_visita_digemoca_otra_temporalidad <- df_dyd |>
  filter(visita_digemoca == "Otra temporalidad") |>
  filter(!(is.na(visita_digemoca_otra_temporalidad))) |>
  select(visita_digemoca_otra_temporalidad)|>
  tabyl(visita_digemoca_otra_temporalidad)

dput(valores_visita_digemoca_otra_temporalidad)

## Define los vectores ----

### 2 a 3 meses
valores_meses_digemoca <- c(
  "Cada 2 meses o al mes y medio",
  "Cada 3 meses"
)

### 1 a 2 veces al año
valores_anos_digemoca <- c(
  "Cada año",
  "Una vez al año",
  "Una vez por año",
  "2 veces al año",
  "Unas dos veces al año",
  "Se tuvo la visita hasta este año, 2025."
)

### 5 años
valores_5digemoca <- c(
  "5 años"
)

### 2 años
valores_2digemoca <- c(
  "Dos años"
)

### ocasionalmente
valores_ocasionalmente4 <- c(
  "Irregularmente",
  "eventualmente", "Muy pocas veces hasta años.",
  "Cuando se tuvo la oportunidad de realizar la evaluación en Físico, se tuvo la presencia de autoridades de la DIGEMOCA (hace 2 años)",
  "Cada cierto tiempo indefinido"
)

### programacion
valores_programacion4 <- c(
  "Calendarizado",
  "Cuando ellos lo tienen planificado",
  "segun su planificación"
)


### requerimiento
valores_requerimiento4 <- c(
  "Cuando se necesita",
  "Cuando sea necesario", "CUANDO LE ORDENAN DEL MINEDUC",
  "Cuando hay casos o asuntos administrativos  que la directora no pueda resolver que son casos muy raros",
  "Cuando tiene asuntos administrativos con Dirección",
  "Supervicion"
)

### sin especificar
valores_sinespecificar_digemoca <- c(
  "Tres veces en toda la temporada",
  "Octubre", "1", "No se exactamente",
  "No tienen fecha"
)

### no visitan
valores_novisita2 <- c(
  "Justamente hoy, casi no nos visitan, desconozco las razones","NO",
  "Por ser nuevo director no he recibido vistas aún"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    visita_digemoca_otra_temporalidad = case_match(
      visita_digemoca_otra_temporalidad,
      all_of(valores_novisita2) ~ "No han visitado",
      all_of(valores_sinespecificar_digemoca) ~ "No especifica",
      all_of(valores_requerimiento4) ~ "Cuando se requiere",
      all_of(valores_programacion4) ~ "Según programación",
      all_of(valores_ocasionalmente4) ~ "Ocasionalmente",
      all_of(valores_2digemoca) ~ "Cada 2 años",
      all_of(valores_5digemoca) ~ "Cada 5 años",
      all_of(valores_anos_digemoca) ~ "1 a 2 veces al año",
      all_of(valores_meses_digemoca) ~ "Cada 2 a 3 meses",
      .default = visita_digemoca_otra_temporalidad
    )
  )

## volver a ver errores

valores_visita_digemoca_otra_temporalidad <- df_dyd |>
  filter(visita_digemoca == "Otra temporalidad") |>
  filter(!(is.na(visita_digemoca_otra_temporalidad))) |>
  select(visita_digemoca_otra_temporalidad)|>
  tabyl(visita_digemoca_otra_temporalidad)

#View(valores_visita_digemoca_otra_temporalidad)


# visita_defoce_otra_temporalidad ----

## ver errores

valores_visita_defoce_otra_temporalidad <- df_dyd |>
  filter(visita_defoce == "Otra temporalidad") |>
  filter(!(is.na(visita_defoce_otra_temporalidad))) |>
  select(visita_defoce_otra_temporalidad)|>
  tabyl(visita_defoce_otra_temporalidad)

dput(valores_visita_defoce_otra_temporalidad)

## Define los vectores ----

### 2 meses
valores_2meses <- c(
  "2 meses",
  "HACE UNOS DOS MESES"
)

### 2 a 3 años
valores_anos_defoce <- c(
  "2 a 3 años",
  "Dos años"
)

### 1 a 2 vez al año
valores_1defoce <- c(
  "Una vez al año",
  "En lo que va del año ha visitado una vez",
  "Hubo ya hasta la fecha una visita",
  "2 veces al año",
  "Una o dos veces al año", "Dos veces en todo la temporada"
)

### no especifica
valores_sinespecificar_defoce <- c(
  "Octubre"
)

### requerimiento
valores_requerimiento5 <- c(
  "CUANDO SEA NECESARIO",
  "Cuando sea necesario",
  "cuando sea requerido", "Cuando hay algún proyecto",
  "En lo que va del año han impartido una capacitación",
  "Por medio de circulares"
)

### programacion
valores_programacion5 <- c(
  "De forma calendarizada"
)

### ocasionalmente
valores_ocasionalmente5 <- c(
  "Repentinamente", "No siempre",
  "Cuando se recuerdan",
  "Se ha tenido visitas de DEFOCE en otros años, pero no cada año."
)

### no han visitado
valores_sinvisita2 <- c(
  "Ninguna","Por ser nuevo director no he recibido ninguna visita"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    visita_defoce_otra_temporalidad = case_match(
      visita_defoce_otra_temporalidad,
      all_of(valores_sinespecificar_defoce) ~ "No especifica",
      all_of(valores_1defoce) ~ "1 a 2 veces al año",
      all_of(valores_anos_defoce) ~ "2 a 3 años",
      all_of(valores_2meses) ~ "2 meses",
      all_of(valores_sinvisita2) ~ "No han visitado",
      all_of(valores_ocasionalmente5) ~ "Ocasionalmente",
      all_of(valores_programacion5) ~ "Según programación de DEFOCE",
      all_of(valores_requerimiento5) ~ "Cuando se requiere",
      .default = visita_defoce_otra_temporalidad
    )
  )

## volver a ver errores

valores_visita_defoce_otra_temporalidad <- df_dyd |>
  filter(visita_defoce == "Otra temporalidad") |>
  filter(!(is.na(visita_defoce_otra_temporalidad))) |>
  select(visita_defoce_otra_temporalidad)|>
  tabyl(visita_defoce_otra_temporalidad)

#View(valores_visita_defoce_otra_temporalidad)

# necesidades_otro_tipo ----

## ver errores
valores_necesidades_otro_tipo <- df_dyd |>
  filter(necesidades_tipo == "Otros") |>
  filter(!(is.na(necesidades_otro_tipo))) |>
  select(necesidades_otro_tipo)|>
  tabyl(necesidades_otro_tipo)

dput(valores_necesidades_otro_tipo)

## Define los vectores ----

### ampliación de infraestructura
valores_infraestructura <- c(
  "Construcción de un ámbiente para un lugar para tecnología solo tres salones tiene y que una pequeña direccion",
  "Es privado quizas ingraestructura",
  "Infraestructura", "Infraestructura. Más ambientes",
  "Más salones de clases", "Que fuese más amplio",
  "Remozamiento"
)

### mobiliario y equipo
valores_mobiliario_equipo <- c(
  "Implementación de laboratorio", "Mobiliario y equipo",
  "Mobiliario y infraestructura"
)

### capacitación del personal
valores_capacitacion <- c(
  "Actualización a través de capacitaciones", "Más capacitaciones"
)

### servicio de psicología
valores_psicologia_servicio <- c(
  "Área de psicología"
)

### mejoras en entorno y seguridad
valores_seguridad_entorno <- c(
  "Jardinizar","Seguridad fuera del establecimiento"
)

### adecuacion curricular
valores_curricular <- c(
  "Que el CNB sea en forma física"
)

### ninguna
valores_ninguna_necesidad <- c(
  "NO HAY NECESIDADES",
  "Ninguna",
  "ninguna"
)

### no aplica
valores_noaplica_necesidad <- c(
  "No es primaria",
  "No somos primaria",
  "Somos Media, no primaria",
  "Somos del nivel 45",
  "no aplica primaria el formulario, ciclo básico cuenta con todas las herramientas para su aplicación"
)

### no especifica
valores_sinespecificar_necesidad <- c(
  "Varias"
)

### Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    necesidades_otro_tipo = case_match(
      necesidades_otro_tipo,
      all_of(valores_sinespecificar_necesidad) ~ "No especifica",
      all_of(valores_noaplica_necesidad) ~ "No aplica",
      all_of(valores_ninguna_necesidad) ~ "No identifica necesidad",
      all_of(valores_curricular) ~ "Adecuación curricular",
      all_of(valores_seguridad_entorno) ~ "Mejoras en el entorno y seguridad",
      all_of(valores_psicologia_servicio) ~ "Servicio de psicología",
      all_of(valores_capacitacion) ~ "Capacitación a docentes",
      all_of(valores_mobiliario_equipo) ~ "Mobiliario y equipo",
      all_of(valores_infraestructura) ~ "Mejoras en infraestructura",
      .default = necesidades_otro_tipo
    )
  )

## volver a ver errores
valores_necesidades_otro_tipo <- df_dyd |>
  filter(necesidades_tipo == "Otros") |>
  filter(!(is.na(necesidades_otro_tipo))) |>
  select(necesidades_otro_tipo)|>
  tabyl(necesidades_otro_tipo)

#View(valores_necesidades_otro_tipo)




# necesidades_otras_implementacion_cnb ----
## ver errores

valores_necesidades_otras_implementacion_cnb <- df_dyd |>
  filter(necesidades_implementacion_cnb == "Otras dificultades") |>
  filter(!(is.na(necesidades_otras_implementacion_cnb))) |>
  select(necesidades_otras_implementacion_cnb)|>
  tabyl(necesidades_otras_implementacion_cnb)

dput(valores_necesidades_otras_implementacion_cnb)

## Define los vectores ----

### mejoras infraestructura
valores_infraesctructura2 <- c(
  "Ambientes pequeños",
  "No cuenta con baño"
)

### capacitación
valores_capacitacion_cnb <- c(
  "Falta de capacitaciones efectivas",
  "Falta de capacitación", "Más capacitaciones",
  "El MINEDUC no capacita al sector privado para manejar generalidades"
)

### contextualizar el cnb
valores_contexto_cnb <- c(
  "Contextualizacion correcta por parte de los docentes",
  "Que algunos no se adecuan al CNB"
)

### materiales y didácticas
valores_materiales_cnb <- c(
  "Material", "Más actividades didácticas para los estudiantes.",
  "Que nuestros estudiantes no tengan libros de texto, aunque somos colegio, los estudiantes no pagan nada, ni mensualidades, pero el INDE no cubre libros para los estudiantes."
)

### falta de apoyo de padres
valores_apoyo_padre <- c(
  "No hay apoyo de padres de fa"
)

### no identifica
valores_ninguna_necesidad2 <- c(
  "Ninguna"
)

## Recode con case match ----

df_dyd <- df_dyd |>
  mutate(
    necesidades_otras_implementacion_cnb = case_match(
      necesidades_otras_implementacion_cnb,
      all_of(valores_ninguna_necesidad2) ~ "No identifica dificultad",
      all_of(valores_apoyo_padre) ~ "Poco apoyo de padres de familia",
      all_of(valores_materiales_cnb) ~ "Sin materiales",
      all_of(valores_contexto_cnb) ~ "Falta de contexualización del CNB",
      all_of(valores_capacitacion_cnb) ~ "Poca capacitación docente",
      all_of(valores_infraesctructura2) ~ "Mejoras de infraestructura",
      .default = necesidades_otras_implementacion_cnb
    )
  )

## volver a ver errores

valores_necesidades_otras_implementacion_cnb <- df_dyd |>
  filter(necesidades_implementacion_cnb == "Otras dificultades") |>
  filter(!(is.na(necesidades_otras_implementacion_cnb))) |>
  select(necesidades_otras_implementacion_cnb)|>
  tabyl(necesidades_otras_implementacion_cnb)

#View(valores_necesidades_otras_implementacion_cnb)

# agregué esto keisy ----


write_xlsx(df_dyd, here("Datos","Listos para análisis","Encuesta docente","df_dyd.xlsx"))