# nombre_area ----

## ver errores

valores_nombre_area <- obs01_rost_areas_ad |>
  filter(!(is.na(nombre_area))) |>
  select(nombre_area)

dput(valores_nombre_area)

## Define los vectores ----

### contabilidad
valores_contabilidad <- c(
  "Contabilidad",
  "CONTABILIDAD",
  "Contabilidad.",
  "Contabilidad general",
  "Educación Cristiana y contabilidad.",
  "Robótica, Contabilidad"
)

### comercio y mercadeo
valores_comercio <- c(
  "Comercio",
  "Comercio y servicio",
  "O.P.E. Comercio y Servicio",
  "Técnicas de mercadeo",
  "Correspondencia",
  "Redacción y correspondencia"
)

#### comunicacion y lenguaje
valores_cyl <- c(
  "Lectura.","Lectura","LECTURA","Lectura obligatoria",
  "Lectura Bíblica", "Lectura y Valores",
  "Lectura y Proyecto de vida",
  "Lectura, guía: utilizan el periodo para que el docente guía trabaje la lectura y este pendiente de los estudiantes pues la hora que se les da la alimentación",
  "Idioma español",
  "Idioma materno ( la docente responsable indica que el curso de idioma materno lo colocaron con ese nombre porque el español es el idioma materno de los estudiantes. O sea lo plantea como idioma español",
  "Ortocaligrafia","Caligrafía","Ortografía",
  "Redacción y correspondencia"
)

### valores y educación cristiana
valores_cristiana <- c(
  "Valores",
  "Educación en Valores",
  "Moral cristiana",
  "Devocional",
  "Religión",
  "Educación Cristiana y contabilidad.",
  "Educación en la fe",
  "Orientación Cristiana"
)

### ciencias naturales y exactas
valores_cyn <- c(
  "Física Fundamental",
  "Laboratorio  de ciencias",
  "Química","Science",
  "Ciencias Naturales","Matemáticas"
)

### programación y robotica
valores_progra <- c(
  "Robótica",
  "Programación","TAC"
)

### expresion artística
valores_expresion <- c(
  "Artes plásticas","Expresión Artística.",
  "Expresión artística","Clase de teclado",
  "Teclado",
  "Expresión Artística (integrada).",
  "Práctica de flauta","Ensayo"
)

### área ocupacional y tecnica
valores_ocupacional <- c(
  "Industriales", "Electricidad",
  "Carpintería", "Estructuras metálicas",
  "Cocina","Belleza",
  "Corte y Confeccion", "Taller",
  "Talleres","Talleres"
)

### aprestamiento y productividad
valores_aprestamiento <- c(
  "Orientación", "Aprender a aprender",
  "Aprestamiento", "Hogar"
)

### ciencias sociales
valores_cys <- c(
  "Ciencias sociales","Culturas e idiomas  maya, garifuna y xinka"
)

### idiomas
valores_idioma <- c(
  "Culturas e idiomas  maya, garifuna y xinka",
  "Ingles","Language Arts"
)

### agricultura 
valores_agricultura <- c(
  "Agricultura"
)

### agronomia
valores_agronomia <- c(
  "Agronomía","Agronomía doméstica"
)

### educacion fisica
valores_edu_fisica <-c(
  "Educación Física"
)

### no especifica
valores_noespecifica_area <- c(
  "Área"
)

## Recode con case match ----

obs01_rost_areas_ad <- obs01_rost_areas_ad |>
  mutate(
    nombre_area = case_match(
      nombre_area,
      all_of(valores_noespecifica_area) ~ "No especifica",
      all_of(valores_edu_fisica) ~ "Educación Física",
      all_of(valores_agronomia) ~ "Agronomía",
      all_of(valores_agricultura) ~ "Agricultura",
      all_of(valores_idioma) ~ "Idiomas",
      all_of(valores_cys) ~ "Ciencias Sociales",
      all_of(valores_aprestamiento) ~ "Aprestamiento y productividad",
      all_of(valores_ocupacional) ~ "Ocupacional y técnica",
      all_of(valores_expresion) ~ "Expresión Artística",
      all_of(valores_progra) ~ "Programación y Robótica",
      all_of(valores_cyn) ~ "Ciencias Naturales",
      all_of(valores_cristiana) ~ "Valores y educación cristiana",
      all_of(valores_cyl) ~ "Comunicación y Lenguaje",
      all_of(valores_comercio) ~ "Comercio y mercadeo",
      all_of(valores_contabilidad) ~ "Contabilidad",
      .default = nombre_area
    )
  )

## volver a ver errores

valores_nombre_area <- obs01_rost_areas_ad |>
  
  filter(!(is.na(nombre_area))) |>
  select(nombre_area)

#View(valores_nombre_area)

