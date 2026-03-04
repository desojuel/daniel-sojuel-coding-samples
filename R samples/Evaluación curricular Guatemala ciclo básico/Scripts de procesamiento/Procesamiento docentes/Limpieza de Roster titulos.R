df_dyd_rost_titulos <- read_xlsx(here("Datos","Encuesta docente","df_dyd_rost_titulos.xlsx"))

# reportado_profesorado ----

## ver errores

valores_reportado_profesorado <- df_dyd_rost_titulos |>
  filter(reportado_grado_academico == "Profesorado")|>
  filter(!(is.na(reportado_profesorado)))

#dput(valores_reportado_profesorado)

## Define los vectores ----


### ciencias naturales
valores_pem_naturales <- c(
  "Ciencias naturales con orientación ambiental",
  "PEM en Ciencias Naturales","Profesor de Enseñanza Media con Especialidad en Ciencias Naturales",
  "Profesorado de Enseñanza Media: Ciencias Naturales",
  "PEM en Ciencias Naturales con Orientación Ambiental",
  "PEM en Química y Biología","PEM Ciencias Naturales con orientación Ambiental",
  "PEM Ciencias naturales con orientación ambiental",
  "PEM En Pedagogía Cuencias Naturales Con Orientación Ambiental",
  "PEM en Pedagogía y Ciencias Naturales con Orientación Ambiental",
  "PEM en pedagogía y Ciencias Naturales Con Orientación Ambiental",
  "PEM en pedagogía y Ciencias Naturales con orientación ambiental.",
  "PEM con Espacialidad en  Ciencias Naturales",
  "PEM con Orientación en Medio Ambiente",
  "PEM en educación ambiental","PEM en Pedagogìa y Ciencias Naturales con orientación ambiental",
  "Pem en ciencias","P.E.M en Pedagogía con Esp. en Ciencias Naturales; P.E.M,en Pedagogía con Esp. en Física Y Matemáticas",
  "P.E.M. en Pedagogía con Esp Matematicasen Ciencias Naturales  y Esp en",
  "PEM. en Enseñanza Media en Pedagogía con Especialidad en Ciencias Naturales",
  "Pem en ciencias naturalea y prm en pedagogía",
  "Profeorado en educación media  de ciencias naturales con orientación ambiental",
  "Profesora de Enseñanza  Media en  Ciencias  especializada en Biología",
  "Profesora en Enseñanza Media en Ciencias Naturales",
  "Profesora en enseñanza media en ciencias Naturales con Orientación Ambiental",
  "Profesorado  de nivel medio en ciencias naturales con orientación en educación ambiental",
  "Profesorado de Enseñanza Media: Ciencias Naturales",
  "Profesorado de Enseñanza Media en  Pedagogía y Ciencias Naturales con  Orientación Ambiental.",
  "Profesorado de Enseñanza Media en Ciencias Naturales",
  "Profesorado de Enseñanza Media en Ciencias de la Educación",
  "Profesorado de Enseñanza Media en ciencias con especialidad en Química y Biología",
  "Profesorado de Enseñanza Media en la especialidad de Química y BIología",
  "Profesorado de Enseñanza Media y Ciencias Naturales con Orientación Ambiental",
  "Profesorado de Enseñanza Media en Pedagogía y Ciencias Naturales con Orientación Ambiental",
  "Profesorado de Enseñanza Media en Pedagogía y Ciencias Naturales con Orientación Ambiental.",
  "Profesorado de enseñanza media con especialidad en ciencias naturales con orientación ambiental",
  "Profesorado de enseñanza media en Pedagogía y Ciencias Naturales",
  "Profesorado en Educación Media en Pedagogía y Ciencias Naturales con Orientación Ambiental",
  "Profesorado en Educación Media en ciencias Naturales",
  "Profesorado en Enseñanza Media en Ciencias Naturales",
  "Profesorado en Enseñanza Media de la Química y la Biología.",
  "Profesorado en enseñanza media y ciencias naturales con orientación ambiental",
  "Profesorado en ciencias naturales con orientación en  educación ambiental",
  "Profesorado en educacion media de ciencias naturales con orientación en educación ambiental",
  "Profesorado en pedagogía con especialidad en ciencias naturales con orientación ambiental",
  "Pem en ciencias naturales","Profesorado en Enseñanza Media en Pedagogía y Ciencias Naturales con Orientación Ambiental", 
  "Profesora de Enseñanza Media Biología",
  "Profesora de Enseñanza Media en Ciencias Naturales",
  "Profesorado de Enseñanza Media: Ciencias Naturales",
  "Profesorado en ciencias naturales con orientación Ambiental",
  "Profesorado en Enseñanza Media en Ciencias Naturales",
  "PEM. Ciencias Naturales con Orientación Ambiental",
  "Profesor de Enseñanza Media en Ciencias Naturales",
  "PEM Ciencias Naturales con Orientación Ambiental",
  "P.E.M En Pedagogía con Especialidad en Ciencia Naturles",
  "Profesorado en Ciencias Naturales con Orientación Ambiental",
  "PEM en Ciencias Químicas y Biológicas",
  "PEM en Ciencias biológicas y químicas",
  "PEM. En Química y Biología",
  "PEM. QUIMICA Y BIOLOGIA",
  "PEM. QUIMICA Y BIOLOGÍA",
  "PEM. Ciencias Naturales",
  "PEM. Ciencias Naturales en PADED",
  "PEM en Ciencias", "PEM EN PEDAGOGÍA CON ORIENTACIÓN EN MEDIO AMBIENTE",
  "PEM. En Pedagogía en Ciencias Naturales con orientación Ambiental",
  "PEM. en pedagogía y Ciencias Naturales con Orientación Ambiental",
  "Profesor de Enseñanza Media con Orientación en Medio Ambiente",
  "Profesor de Enseñanza Media y Ciencias Naturales con Orientación en Computación",
  "Profesora de Enseñanza Media en Ciencias",
  "Profesora de enseñanza media en ciencias naturales",
  "Profesorado de Enseñanza Media con Especialidad en Ciencias Naturales",
  "Profesorado de Enseñanza Media en Ciencias Naturales",
  "Profesorado de Enseñanza Media en Ciencias de la Educación con Especialidad Ambiental",
  "Profesorado en ciencias Naturales con orientación Ambiental",
  "PEM en pedagogia Ciencias Naturales y Orientacion",
  "PEM en Pedagogía y Ciencias Naturales con orientación ambiental",
  "PEM en Pedagogía YCiencias Naturales Con Orientación Ambiental",
  "PEM en Pedagogía y Ciencias Naturales con orientación ambiental",
  "PEM. En Pedagogía y Ciencias Naturales con Orientación Ambiental",
  "PEM. En pedagogia y ciencias naturales con O. A",
  "PEM. Pedagogia y Ciencias naturales",  "Especialización en Ciencias Naturales",
  "Licenciatura en Pedagogía con especialidad en Ciencias Naturales",
  "PEM en ciencias","PEM EN PEDAGOGIA Y PEN EN CIENCIAS NATURALES",
  "Profesor de ciencias naturales con orientación ambiental",
  "Profesorado en Ciencias Naturales y orientación ambiental",
  "Quimica y biología", "PROFESOR DE ENSEÑANZA MEDIA CON ORIENTACIÓN EN MEDIO AMBIENTE",
  "PROFESORADO DE ENSEÑANZA MEDIA EN PEDAGOGIA Y CIENCIAS NATOURALES CON ORIENTACIÓN AMBIENTAL",
  "PROFESORADO EN ENSEÑANZA MEDIA EN PEDAGOGIA Y CIENCIAS NATURALES CON ORIENTACION AMBIENTAL",
  "Pem en pedagogía con orientación ambienta",
  "Profesor de enseñanza media en pedagogía con orientación ambiental",
  "Profesora de Enseñanza Media con Orientación en Medio Ambiente",
  "Profesora en Enseñanza Media en Pedagogía con Orientación en Medio Ambiente",
  "Profesorado de Enseñanza Media: Ciencias Naturales",
  "Profesorado de enseñanza media con orientación en medio ambiente",
  "Profesorado de enseñanza media especializada en química y biología",
  "Profesorado en Enseñanza Media con Orientación en Medio Ambiente",
  "Profesorado en Enseñanza Media en Pedagogía con Orientación en Medio Ambiente",
  "Profesorado en Enseñanza media en Ciencias de la Educación con énfasis en Educación Ambiental",
  "Profesorado en Pedagogía en Ciencias Naturales con Orientación ambiental.",
  "Profesorado en Pedagogía y especialidad en Ciencias Naturales y Orientación en el Medio Ambiente",
  "Profesorado en enseñanza media en pedagogía con orientación en medio ambiente",
  "Profesorado en enseñanza media en pedagogía y Ciencias Naturales con orientación Ambientalia",
  "Profesorado en ciencias naturales con orientación Ambiental",
  "Profesorado en ciencias naturales y educación ambiental",
  "Profesorado en Ciencias.  Especialidad Química y Biología",
  "PEM en ciencias naturales","Profesorado de Enseñanza Media en Pedagogía con Especialidad en Ciencias Naturales", 
  "Profesor Técnico en Biología y Química",
  "Profesorado en Ciencias Naturales",
  "Profesorado en Química y Biología","PEM EN PEDAGOGÍA CON ORIENTACIÓN EN MEDIO AMBIENTE",
  "PEM. En Pedagogía en Ciencias Naturales con orientación Ambiental",
  "Pem en pedagogía y ciencias naturales",
  "Profesora de Enseñanza Media en Ciencias",
  "Profesora de Enseñanza Media en Química y Biología",
  "Profesorado de Enseñanza Media: Ciencias Naturales",
  "Profesorado en ciencias Naturales con orientación Ambiental",
  "Profesorado de Enseñanza Media en Ciencias Naturales",
  "Profesor de Enseñanza Media con Orientación en Medio Ambiente",
  "Profesorado de Enseñanza Media con Orientación en Medio Ambiente",
  "Profesorado de Enseñanza Media: Ciencias Naturales",
  "Profesorado en Enseñanza Media en Ciencias Naturales"
)
### pem mate y fisica

valores_PEM_matefisica <- c(
  "P.E.M en Fisica- Matemática",
  "P.E.M. en Ciencias Especializado en Física-Matemática",
  "P.E.M. en Física-matemática","Profesorado en Matemáticas",
  "Profesor de segunda enseñanza en física y matematica",
  "Profesorado de Enseñanza Media: Matemática y Física",
  "PEM CIENCIAS ESPECIALIZADO EN MATEMÁTICAS","PEM en pedagogía, física y matemáticas",
  "Profesorado en Matemáticas",
  "Profesorado de Enseñanza Media: Matemática y Física",
  "Profesorado de Enseñanza Media en Física y Matemática",
  "Profesor de enseñanza media especializado en física y matemática",
  "Profesora de Enseñanza Media en Matemáticas y Física",
  "Profesorado en Enseñanza Media de la Matemática y Física",
  "PEM Física y Matemáticas", "Profesorado en Matemáticas",
  "Profesorado de Enseñanza Media: Matemática y Física",
  "Profesor de Enseñanza Media en Matemática y Ciencias Económico Contable",
  "Profesora de Enseñanza Media en Física - Matemática",
  "Profesorado en matemáticas y física",
  "Profesor de enseñanza media especialidad Matemática",
  "PEM EN MATEMATICAS","Matemática y Física",
  "Matemáticas y física","Enseñanza Media con especialidad en Física Matemática",
  "Física y Matemática",
  "Fisica- Matemática","PEM en Matemática y Física", "PEM Física Matemática",
  "Profesorado en Matemática y Física",
  "Profesor de Enseñanza Media en Física y Matemática",
  "PEM FISICA - MATEMÁTICA",
  "PEM FISICA Y MATEMATICA","En Matematicas",
  "En Matemáticas","Matematico",
  "PRM EN MATEMÁTICAS Y FÍSICA",
  "PEP en física y matemática",
  "Pem con especialidad en matematica y fisica",
  "Pem con especialidad física y matemáticas",
  "Profesorado en Matemática y Física.",
  "Profesorado en físico matemático",
  "Profesorado en la Enseñanza de la Física y Matemática",
  "Profesorado física matemática curriculum cerrado",
  "PEM FISICA- MATEMATICA",
  "PEM en Física - Matemática",
  "PEM en Física Matemática",
  "PEM en Física y Matemática",
  "PEM en Matemática y Físuca",
  "PEM en Matemáticas y Fisica",
  "PEM en Matemáticas y Física",
  "PEM en la Matemática y la Física",
  "PEM en la enseñanza de la Física y Matemáticas",
  "PEM en la matemática y Física",
  "PEM en la matemática y la física",
  "PEM especializada en Física Matemática",
  "PEM con especialidad en matemáticas y física",
  "PEM CON ESPECIALIDAD EN MATEMATICA",
  "Profesorado de Enseñanza Media en Matemática y Física", "PEM en Fisica y Matemáticas",
  "PEM. Matemática y física",
  "PEM en física y Matematica",
  "PEM en la enseñanza de la física y la matemática",
  "PEM en pedagogía, física y matemática",
  "PEM en Educación de la Matemática y la Física",
  "PEM Pedagogía-Matematica",
  "PEM con Especialidad en Matemáticas",
  "PEM en Ciencias especializado en Matemáticas",
  "PEM en Ciencias especializado  Matemáticas",
  "PEM EN CIENCIAS ESPECIALIZADO EN MATEMÁTICAS",
  "PEM EN CIENCIAS ESPECIALIZADO EN MATEÁTICAS",
  "PEM. En Ciencias Especializado en Matemática",
  "PEM en Matemáticas y Ciencias PADEP",
  "PEM en Matemáticas y Ciencias Económico Contable",
  "PEM. En Enseñanza Media en Matemática","Con Especialidad en Matemáticas",
  "En Enseñanza Media de la matemáticas y física",
  "En Matemática y Física","P.E.M en Pedagogía con Esp. en Ciencias Naturales; P.E.M,en Pedagogía con Esp. en Física Y Matemáticas",
  "P.E.M. en Pedagogía con Esp Matematicasen Ciencias Naturales  y Esp en",
  "Pem en ciencias especializadas en matemáticas",
  "Prof. de Enseñanza Media en Ciencias, aespecializado en Matemática.",
  "Profesor buenos de enseñanza media con especialidad en Matemática y Física",
  "Profesor de Enseñanza Media en Pedagogía con Especialidad en Matemáticas y Física",
  "Profesor de enseñanza media en pedagogía, física y matemática",
  "Profesor de enseñanza media, con especialidad en matemáticas y física fundamental",
  "Profesor en Enseñanza Media Ena Física y Matemáticas",
  "Profesor en enseñanza de la Matemática",
  "Profesora de enseñanza media en ciencias con doble especialidad en matemáticas y física",
  "Profesora de enseñanza media en matemática y física",
  "Profesora de Enseñanza Media con especialidad en matemática y física",
  "Profesora en Matemática y Física",
  "Profesorada en la enseñanza de la matemática y física",
  "Profesorado De Enseñanza Media con Especialidad en Matemática y Física",
  "Profesorado de Enseñanza Media en Física /Matemática",
  "Profesorado de Enseñanza Media en Física-Matemática",
  "Profesorado de Enseñanza Media en Matemática Y Física",
  "Profesorado de Enseñanza Media en Matemática y Fisica",
  "Profesorado de enseñanza media en fisica - matemática",
  "Profesorado de enseñanza Media en Física y Matemática",
  "Profesorado de enseñanza Media en Matemática y Física",
  "Profesorado de Segunda Enseñanza con especialidad en Matemáticas",
  "Profesorado de segunda enseñanza con especialidad en Matemáticas",
  "Profesorado en Ciencias especializado en Matemáticas y Física",
  "Profesorado en Enseñanza Media con Especialidad en Matemática y Física",
  "Profesorado en Enseñanza Media con especialidad en Matemática  y Física",
  "Profesorado en Enseñanza Media con especialización en Matemáticas y Fisica",
  "Profesorado en Enseñanza Media de Física Matemáticas",
  "Profesorado en Enseñanza Media de la Matemática y Física y Profesorado en Enseñanza Media de la Química y la Biología.",
  "Profesorado en Enseñanza Media de la Matemática y la Física",
  "Profesorado en Enseñanza Media en Física Matemática",
  "Profesorado en Enseñanza Media en Física y Matemática",
  "Profesorado en Enseñanza Media en Pedagogía, Física y Matemáticas",
  "Profesorado en Enseñanza Media: Matemática y Física",
  "Profrodado con especialidad en Matematicas",
  "profesorado física matemática curriculum cerrado",
  "En matemáticas y física",
  "Especialidad en Matemática",
  "Especialización en matemáticas y física",
  "Física- matemática","Matemática y fisica","Matemática y física",
  "Matemática-Física","PROFESORADO EN MATEMÁTICAS",
  "PSE En Pedagogía con Especialidad en Matemáticas",
  "Profesora de Enseñanza Media en Pedagogía con la Especialización en Matemáticas",
  "Profesora en Enseñanza Media Especializada en Matemática y Computación",
  "Profesorado de Enseñanza Media Especializado en Matemática y Computación",
  "Profesorado de Enseñanza Media con Especialidad en Matemática y Física",
  "Profesorado de Enseñanza Media: Matemática y Física",
  "Profesorado de eneseñanza media en fisica - matemática",
  "Profesorado de enseñanza media con especialidad en Matemática",
  "Profesorado en Pedagogía con Especialidad en Matemática y Física",
  "Profesorado en enseñanza media con especialización en matemáticas",
  "PEM ESPECIALIZADO en CC FÍSICA-MATEMÁTICA",
  "Pem en matemática y fisica","Pem matemática y fisica",
  "Profesorado en Matemáticas y Física",
  "Profesorado en Física y Matemática",
  "PEM en matemáticas"
)

### pem lenguaje e idiomas
valores_PEM_lenguaje <- c(
  "P.E.M en comunicación y lenguaje",
  "PEM CON ESPECIALIDAD EN COMUNICACIÓN Y LENGUAJE",
  "PEM EN COMUNICACIÓN Y LENGUAJE",
  "PEM EN LENGUA Y LITERATURA",
  "Profesorado de Enseñanza Media en Pedagogía, Comunicación y Lenguaje", 
  "PEM Lengua y Literatura","Profesora de enseñanza media con especialidad en comunicación",
  "Profesora de enseñanza media con especialidad en comunicación y lenguaje",
  "Profesora de enseñanza media con especialidad en comunicación y lenguaje",
  "Profesora en la Enseñanza Media en Lengua y Literatura",
  "Profesorado de Enseñanza Media en Ciencias de la Educación con especialidad en comunicación y lenguaje",
  "Profesorado de Enseñanza Media: Lenguaje e idiomas",
  "Profesorado de Enseñanza media en lengua y literatura",
  "Profesorado en Educación Media con Especialidad en Comunicación y Lenguaje, Idioma Español",
  "PEM EN INGLES","Educación Intercultural",
  "Idioma inglés","PEM. CON ESPECIALIDAD EN COMUNICACION Y LENGUAJE",
  "PEM. En Enseñanza Media en Lengua y Comunicación",
  "PEM. En Enseñanza Media en Lenguaje y Comunicación",
  "PEM en Lengua y Literatura",
  "PEM lenguaje","PEM  en Inglés",
  "PEM idioma Inglés","En lengua y literatura",
  "Lengua y literatura","Enseñanza Media en Lengua y Literatura",
  "PROFESORA DE ENSEÑANZA MEDIA  CON ENFASIS EN LENGUA Y LITERATURA",
  "Profesorado en Lengua y Literatura",
  "Profesor de Enseñanza Media en Comunicación y Lenguaje",
  "Profesora de Enseñanza Media en Lengua y Literatura",
  "lengua y literatura","Enseñanza media en Inglés",
  "Profesora de Enseñanza Media Especializada en Inglés",
  "Profesorado de Enseñanza Media en Inglés",
  "Profesorado en Idioma Inglés",
  "Profesorado en Lengua y literatura",
  "Profesorado en Lenguaje y Comunicación",
  "Profesorado con especialidad en Idioma Español",
  "Profesorado en idioma ingles",
  "Pem en Inglés","PEM. CON ESP. EN INGLES",
  "PEM en Inglés","PEM EN COMUNICACIÓN Y LENGUAJE,  IDIOMA ESPAÑOL",
  "PEM Con especialidad en comunicación y Lengiaje.",
  "PEM Especialización en Comunicación y Lenguaje",
  "PEM Especializado en Comunicación y Lenguaje",
  "PEM en Comunicación y lenguaje",
  "PEM en Pedagogía, Comunicación y Lenguaje",
  "PEM en Pedagogìa con la especialidad en Comunicaciòn y Lenguaje, Idioma Español",
  "PEM en Pedagogía con Especialidad en Comunicación y Lenguaje",
  "PEM en Pedagogía con especialidad en Comunicación y Lenguaje.",
  "PEM con especialidad Comunicación y lenguaje",
  "PEM en idioma español","PROFESORA DE ENSEÑANZA MEDIA EN COMUNICACIÓN Y LENGUAJE",
  "PROFESORA DE ENSEÑANZA MEDIA EN LENGUA Y LITERATURA, PROFESORADO EN ENSEÑANZA EN CIENCIAS Y MATEMÁTICAS",
  "PROFESORADO DE ENSEÑANZA MEDIA EN LENGUA Y LITERATURA",
  "PROFESORADO DE ENSEÑANZA MEDIA EN PEDAGOGIA Y CIENCIAS DE LA EDUCACION.",
  "PROFESORADO DE ENSEÑANZA MEDIA EN LENGUA Y LITERATURA",
  "PROFESORADO EN LENGUA Y LITERATURA",
  "PROFESORADO EN LENGUA Y LITERATURA",
  "Profesora de Educación Media en Lengua y Literatura",
  "Profesora de Enseñanza Media con Especialidad en Comunicación y Lenguaje",
  "Profesora de Enseñanza Media con especialidad en Comunicación y Lenguaje",
  "Profesora de Enseñanza Media con especialidad en comunicación y lenguaje",
  "Profesora de Segunda Enseñanza con especialidad en Comunicación y Lenguaje",
  "Profesora de Segunda Enseñanza en Comunicación y Lenguaje",
  "Profesorado con especialidad en comunicación y lenguaje",
  "Profesorado con especialidad en idioma español",
  "Profesorado de Enseñanza Media con Especialización en Comunicación y Lenguaje",
  "Profesorado de Enseñanza Media con especialidad en Comunicacion y Lenguaje",
  "Profesorado de Enseñanza Media en Comunicación y Lenguaje",
  "Profesorado de enseñanza media en Comunicación y lenguaje",
  "Profesorado en Enseñanza Media con Especialización en Inglés",
  "Profesorado de Enseñanza Media con Especialización en Inglés",
  "Profesorado de Enseñanza Media en Idioma Inglés",
  "Profesorado de enseñanza media en idioma ingles",
  "Profesorado de enseñanza media en idioma inglés",
  "Profesorado de enseñanza media en inglés",
  "Profesorado enseñanza media en Idioma Ingles",
  "Profesorado en Enseñanza Media con Especialización en English Language Teaching (ELT)",
  "Profesorado en Lenguaje y Ciencias Sociales",
  "Profesorado de Enseñanza Media en Lenguaje, Filosofía y Estudios Sociales",
  "Profesorado en Enseñanza media en lengua y literatura",
  "Profesorado en la Enseñanza Media en Lengua y Literatura",
  "Profesorado en Endeñanza Media en Lenguaje y Comunucación",
  "Profesorado en enseñanza media con énfasis en lengua y literatura",
  "Profesorado en enseñanza media en ciencias de la educación con énfasis en lengua y literatura",
  "Profesorado en literatura","Profesorado en Educación Primaria Bilingüe Intercultural",
  "Profesorado en Enseñanza Media en Inglés",
  "PEM inglés", "Comunicación y Lenguaje",
  "Comunicación y lenguaje",
  "Lengua y Literatura","Profesora de Enseñanza Media en Comunicación y Lenguaje",
  "Profesorado en Lengua y Literatura",
  "Profesorado de Enseñanza Media en Lengua y Literatura",
  "Profesor de Enseñanza Media en Lengua y Leteratura",
  "Profesorado con especialidad en Comunicación y Lenguaje",
  "Profesorado en lengua y literatura",
  "Profesorado enseñanza media en Idioma Ingles",
  "PEM en Comunicación y Lenguaje",
  "PEM en Lengua y Literatura",
  "Profesorado en Comunicación y Lenguaje",
  "Profesorado en Lengua y Literatura", "Con Especialidad en Comunicación y Lenguaje",
  "Con la especialidad en Comunicación y lenguaje",
  "En comunicacion y lenguaje",
  "En Lengua y Literatura",
  "Lengua y literatura",
  "Lengua y literatura doble especialidad",
  "PEN. CON ESPECIALIDAD EN COMUNICACIÓN Y LENGUAJE.",
  "Profesorado en Comunicación y lenguaje",
  "Profesorado de Enseñanza Media en Lengua y Literatura"
)

### pem edu intercultural
valores_pem_intercultural <- c(
  "En Primaria Intercultural",
  "PEM EN EDUCACION BILINGÜE INTERCULTURAL","Profesor de Educación Primaria Intercultural",
  "Profesor de Educación Pre-primaria bilingüe Intercultural",
  "Profesorado en Educación Primaria Intercultural",
  "Profesor de Educación Primaria Intercultural","PEM.EN EDUCACIÓN INTERCULTURAL",
  "PROFESOR DE EDUCACIÓN PRIMARA CON ENFASIS EN EDUCACIÓN BILINGÜE",
  "Profesor en Educación Primaria Intercultural",
  "Profesora de Educación Primaria Intercultural",
  "Profesor de Educacion Primaria Intercultural",
  "Profesor de Educación Primria Bilingüe Intercultural",
  "Profesor de Rducación Primaria Intercultural",
  "Profesor de educación primaria Intercultural",
  "Profesora en Educación Primaria Bilingue Intercultural",
  "Profesora en Educación Primaria Bilingüe Intercultural",
  "Profesorado En Educación Primaria Intercultural",
  "Profesorado de Educación Primaria Intercultural con énfasis en educación bilingüe",
  "Profesorado de educacion primaria intercultural",
  "Profesorado en Educacion primaria intercultural",
  "Profesorado en educación Primaria Bilingüe Intercultural",
  "Profesorado en Educación Intercultural Bilingüe",
  "Profesorado de Enseñanza con Énfasis en la Cultura Maya", 
  "PEM Y Profesorado de Educación Primaria Intercultural",
  "PROFESOR EN EDUCACIÓN PRIMARIA INTERCULTURAL",
  "Profesora de Educación Primaria Intercultural",
  "Profesor de Educación Primaria Intercultural",
  "Profesorado en Educación Primaria Intercultural",
  "Profesorado en Educación Primaria Intercultural PADEP",
  "Profesorado de Enseñanza Media: Educación Intercultural",
  "Profesorado con énfasis en Cultura Maya","Profesor de Educación Primaria Intercultural",
  "Profesorado en Educación Primaria Intercultural",
  "Profesorado en Educación Primaria Intercultural PADEP",
  "Profesora universitaria con especialidad en preprimaria intercultural",
  "Proferado en Educación Preprimaria Intercultural",
  "Profesor de Educación Primaria Intercultural","PEM en Pedagogía y Educación Intercultural",
  "PROFESOR DE EDUCACIÓN PRIMARIA INTERCULTURAL",
  "Profesor de Educación Primaria Bilingue Intercultural",
  "Profesorado en Educación Intercultural Bilingüe",
  "PEM y Profesorado de Educación Primaria Intercultural",
  "PROFESORA DE EDUCACION PRIMARIA INTERCULTURAL",
  "Pen en Pedagoga y Educación Intercultural",
  "Profesora de Enseñanza Media de Educación para Contextos Multiculturales", 
  "Profesorado de Enseñanza Media Con Énfasis en La Cultura Maya de la San Carlos de Guatemala deRigoberta Menchu Tum", 
  "Profesorado de Enseñanza Media Con Énfasis en La Cultura Maya de la San carlos de la rigoberta menchu Tun", 
  "Profesor de Enseñanza media en educación para contextos multiculturales",
  "Prefesorado Intwrcultural","PSE en Pedagogia con Orientacion en Billinguismo e intercultural.", 
  "Profesor de Educacion Primaria Intercultural",
  "Profesor de Educación Primria Bilingüe Intercultural",
  "Profesor de enseñanza Media en educación para contextos multiculturales",
  "Profesor de Rducación Primaria Intercultural",
  "Profesor de educación primaria Intercultural",
  "Profesor de enseñanza Media y ciencias de la educación Y profesot de educación primaria Interciltural",
  "Profesora de Segunda Enseñanza en Ciencias Humanísticas con Orientación en Educación Intercultural",
  "Profesora de educacion preprimara intercultural",
  "Profesora en Educación Primaria Bilingue Intercultural",
  "Profesora en Educación Primaria Bilingüe Intercultural",
  "Profesorado En Educación Primaria Intercultural",
  "Profesorado de Educación Primaria Intercultural con énfasis en educación bilingüe",
  "Profesorado de Enseñanza Media en Educación Intercultural Bilingüe",
  "Profesorado de Enseñanza Media: Educación Intercultural",
  "Profesorado de enseñanza media bilingue intercultural",
  "Profesorado de Segunda Enseñanza con orientación intercultural",
  "Profesorado de educacion primaria intercultural",
  "Profesorado en Educacion primaria intercultural",
  "Profesorado en Educación Media y Profesorado en Educación Primaria Bilingue Intercultural",
  "Profesorado en Educación Media y Profesorado en Educación Primaria Bilingue Intercultural PADEP",
  "Profesorado en Enseñanza Media y Educación Intercultural",
  "Profesorado en educación Primaria Bilingüe Intercultural",
  "Profesorado en primaria intercultural",
  "Profesorado de Educación Primaria Intercultural",
  "Profesorado de Enseñanza Media: Educación Intercultural",
  "PSE en Cien. Humanisticas con Orientacion en Billinguismo e Interculturalidad.",
  "Profesorado en Educación Primaria Intercultural",
  "Profesorado en educación primaria intercultural",
  "Profesorado en pedagógica y ciencias  humanisticas    e intercultural",
  "educacion bilingue","Profesor de Educación Primaria Intercultural",
  "Profesor de Educación Primaria Bilingüe Intercultural",
  "Profesorado en enseñanza media bilingue intercultural"
)

###pem pedagogia
valores_PEM_pedagogia <- c(
  "P.E.M. en Pedagogía con Esp.en Ciencias Naturales",
  "P.E.M. en Pedagogía con Especialidad en Ciencias Naturales",
  "PEM EN CIENCIAS DE LA EDUCACIÓN","Educación Especial",
  "En Pedagogía y administración Educativa",
  "Profesorado en enseñanza  media en ciencias de la educación",
  "En pedagogía con especialización en área industrial",
  "P S E en pedagogía y ciencias sociales",
  "PEDAGOGIA CON ADMINISTRACIÓN EDUCATIVA",
  "PEM en pedagogia","PEM en pedagogía",
  "Pem en pedagogia","PEM en Pedagogía","Profesorado de enseñanza media en ciencias de la educación con especialidad  en pedagogia", 
  "Profesorado de enseñanza media en pedagogía y psicología", 
  "Profesorado de enseñanza media en pedagogía y técnico en administración educativa", 
  "Profesorado de enseñanza media y ciencias de la Educación", 
  "P.E.M en Pedagogía","PEM en pedagogía con Especialización en Área Industrial",
  "PEM en pedagogía con especialización en área industrial",
  "PEM en pedagogía y técnico en adelante educativa PEM en pedagogía, ciencias sociales y formación ciudadana",
  "PEM. En Pedagogía con Especialidad en Ciencias de la Educación",
  "PEM. Y técnico en Pedagogía",
  "PEM. en Pedagogía y Administración Educativa",
  "PEM. en Pedagogía y CC. EE.",
  "PROFESOR DE ENSEÑANZA MEDIA EN CIENCIAS DE LA EDUCACION CON ORIENTACION EN PEDAGOGIA",
  "PROFESOR DE ENSEÑANZA MEDIA EN PEDAGOGÍA Y CIENCIAS DE LA EDUCACIÓN",
  "PROFESOR EENSEÑANZA MEDIA EN PEDAGOGÍA Y CIENCIAS DE LA EDUCACIÓN",
  "PROFESORA DE ENSEÑANZA MEDIA EN PEDAGOGÍA Y CIENCIAS DE LA EDUCACIÓN",
  "PROFESORADO DE ENSEÑANZA MEDIA EN PEDAGOGÍA Y CIENCIAS DE LA EDUCACIÓN",
  "PROFESORADO EN PEDAGOGIA",
  "PROFESORADO EN PEDAGOGIA Y CIENCIAS DE LA EDUCACIÓN",
  "PROFESORADO EN PEDAGOGÍA Y CIENCIAS DE LA EDUCACIÓN",
  "Pem en Pedagogía y Ciencias de la Educación",
  "Pem en pedagogía con orientación ambienta",
  "Profesor De Enseñanza Media en Pedagogía y Ciencias de la Educación",
  "Profesor de Enseñanza Media en Pedadlgía  y Ciencias de la Educacion",
  "Profesor de Enseñanza Media en Pedagogía y Ciencias de la Educación.",
  "Profesor de Enseñanza Media y Técnico en Pedagogía",
  "Profesor de Eseñanza Media en Pedagogia, con especialidad en área industrial",
  "Profesor de enseñanza Media en Pedagogía y Ciencias de la Educación",
  "Profesor de enseñanza media en  pedagogía.",
  "Profesor de enseñanza media en pedagogia y ciencias de la educación",
  "Profesor de enseñanza media en pedagogía con orientación ambiental",
  "Profesor de segunda enseñanza en pedagogía",
  "Profesor enseñanza media y ciencia de la educación",
  "Profesora de Enseñanza Media en Pedagogia y Técnico en Administración Educativo",
  "Profesora de Enseñanza Media en Pedagogía con la Especialización en Matemáticas",
  "Profesora de Enseñanza Media en Pedagogía de la Educacion",
  "Profesora de Enseñanza Media en pedagogia y Ciencias de la educacion.    pedn Pedagogia y Ciencias de la educacion",
  "Profesora de Segunda Enseñanza en Pedagogía con Orient. En Direc. Y Admin. De Centros Educativos",
  "Profesora de enseñanza media con énfasis en pedagogia",
  "Profesora de enseñanza media y técnica en pedagogía",
  "Profesora de enseñanzas media en pedagogía y técnico en administración educativa",
  "Profesora de segunda enseñanza en pedagogía y Ciencias de la Educación",
  "Profesora en Educación",
  "Profesora en Enseñanza Media en Pedagogía con Orientación en Medio Ambiente",
  "Profesora en Enseñanza Media en Pedagogía y Administración Educativa",
  "Profesora en Enseñanza Media en Pedagogía y Técnico en Administración Educativa",
  "Profesora en Enseñanza Media y Pedagogía",
  "Profesora en Pedagogía y Ciencias de la Educación",
  "Profesorado de Enseñanza En Pedagogía, Ciencias Sociales y Formación Ciudadana",
  "Profesorado de Enseñanza Media En Pedagogía y Ciencias de la Educación",
  "Profesorado de Enseñanza Media En Pedagogía, Ciencias Sociales y Formación Ciudadana",
  "Profesorado de Enseñanza Media en Pedagogia con Especializacion en Artes Industriales",
  "Profesorado de Enseñanza Media en Pedagogía  y Ciencias de la Educación",
  "Profesorado de Enseñanza Media en Pedagogía con Orientación en Pedagogía",
  "Profesorado de Enseñanza Media en Pedagogía y Promotor de Derechos Humanos y Cultura de Paz",
  "Profesorado de Enseñanza Media en Pedagogía y Técnica en Docencia Universitaria",
  "Profesorado en Enseñanza Media con Énfasis en Pedagogía",
  "Profesorado en Enseñanza Media en Ciencias de la Educación   con énfasis en Pedagogía",
  "Profesorado en Enseñanza Media en Pedagogía con Orientación en Medio Ambiente",
  "Profesorado en Enseñanza Media en Pedagogía en Ciencias de la educación",
  "Profesorado en Enseñanza Media en Pedagogía y Administración Educativa",
  "Profesorado en Enseñanza Media en Pedagogía y Técnica en Administración Educativa",
  "Profesorado en Enseñanza Media en Pedagogía y Técnico en Administración Educativa",
  "Profesorado en Enseñanza Media en Pedagogía y técnico en Administración Educativa",
  "Profesorado en Pedagogía y Ciencias de la Edcucaión",
  "Profesorado en Pedagogía y Ciencias de la Educat",
  "Profesorado en Pedagogía y Técnico en Administración Educativa",
  "Profesorado en Pedagogía y técnico en Administración Educativa",
  "Profesorado en Pedagogía y técnico en administración educativa",
  "Profesorado en enseñanza media en pedagogpia y psicología",
  "Profesorado en enseñanza media en pedagogía con orientación en medio ambiente",
  "Profesorado en pedagogía en DDHH",
  "Profesorado en pedagogía y gestión y liderazgo escolar",
  "Ptofesorado de Educación Media y pedagogía",
  "profesor de enseñanza media en pedagogía y CC. de la Edic.",
  "profesorado en eseñanza media en pedagogia y tecnico en administracion educativa",
  "profesorado en pedagogia administracion educativa",
  "profesorado en pedagogia en eseñansa media y administración educativa",
  "Profesorado de Enseñanza Media en Pedagogía",
  "Profesorado en Enseñanza Media en Pedagogía",
  "Profesorado de enseñanza media en pedagogía",
  "Profesorado en pedagogía",
  "Profesorado en pedagogía y ciencias de la educación",
  "Profesorado de Enseñanza Media en Pedagogía y Ciencias de la Educación",
  "Profesorado de enseñanza media en pedagogía y ciencias de la educación",
  "Profesorado en Ciencias de la Educación",
  "Profesorado en Ciencias de la Educación con especialidad en Pedagogía",
  "PEM en pedagogía y ciencias de la educación",
  "PEM en Pedagogía y Ciencias de la Educación",
  "Licenciatura en Pedagogía",
  "Licenciatura en Pedagogía y Administración Educativa",
  "Pedagogía", "Pedagogía y Ciencias de la Educación",
  "Pedagogía con especialización en área industrial",
  "Pedagogía y Administración Educativa",
  "Profesorado en Educación con especialidad en Pedagogía",
  "Profesorado de Enseñanza Media en Educación",
  "PEM. En pedagogía",
  "Profesorado en Pedagogia",
  "Profesorado en Pedagogía",
  "Profesorado en Pedagogía y Ciencias de la Educación",
  "Profesorado en Pedagogía y Ciencias de la Educación",
  "Profesorado de Enseñanza Media en Pedagogía y Ciencias de la Educación",
  "Profesor de Enseñanza Media en Pedagogía y Ciencias de la Educación",
  "Profesor de Enseñanza Media en pedagogía",
  "Profesor de Segunda Enseñanza en Pedagogía",
  "Profesorado de Enseñanza Media: Pedagogía",
  "Profesorado en Ciencias de la Educación",
  "Profesora de Enseñanza Media en Pedagogía y Ciencias de la Educación",
  "Profesora en Ciencias de la Educación con Especialidad en Pedagogía",
  "Pedagogía e investigación educativa",
  "Pem Pedagogía en calidad educativa",
  "Profesorado en pedagogía con énfasis en pedagogía",
  "Profesorado en pedagogía",
  "Pem en pedagogía y ciencias de la educacion",
  "Profesorado en Pedagogia",
  "Profesorado de Enseñanza Media: Pedagogía",
  "Profesor de enseñanza media con énfasis en pedagogía",
  "Profesor de enseñanza media en pedagogia",
  "Profesora  de Enseñanza Media en pedagogia",
  "PEM EN PEDAGOGÍA Y ADMINISTRACIÓN EDUCATIVA",
  "PEM EN PEDAGOGÍA Y CIENCIAS DE LA EDUCACIÓN",
  "Ciencias de la Educación","En Pedagogía con orientación en centros educativos",
  "En Pedagogía y Administracion Educativa",
  "En pedagogía y Administración Educativa",
  "En pedagogía y administración Educativa",
  "En pedagogía y técnica de investigación",
  "En pedagogía y técnica en administración educativa",
  "En pedagógia y Ciencias Sociales",
  "En segunda Enseñanza y ciencias de la Educación",
  "PEM en pedagogía y ciencias de la educación",
  "PEM en pedagogía y ciencias de la educacion",
  "Profesorado en pedagogía",
  "Profesorado en pedagogía y ciencias de la educación",
  "En Ciencias de la Educación","PEM en Pedagogía",
  "PEM EN PEDADOGIA","EN PEDAGOGIA","En Pedagogía y Psicología",
  "PEM EN PEDAGOGÍA Y PSICOLOGíA",
  "Profesorado en Pedagogía y Psicología",
  "Profesorado en Psicología y Pedagogía",
  "Profesor de Educación Media en Pedagogía y Psicología",
  "En pedagogia", "En pedagogía","Pedagogia",
  "Pedagogía y administración educativa",
  "Pedagogía y ciencias de la educación",
  "PEM en pedagogía","PEM en Pedagogía y Ciencias de la Educación",
  "Profesor de Enseñanza Media en Pedagogía y Ciencias de la Educación",
  "Profesorado en Pedagogía",
  "PEM pedagogía y psicologia",
  "PEM en Pedagogía y PSICOLOGíA","PEM en Pedagogía y Ciencias de la Educación",
  "PEM. En Pedagogía y Ciencias de la Educación",
  "Pedagogía","PEM en pedagogia",
  "PEM pedagogia","PEM en Pedagogía y Ciencia de la Educación",
  "PEM en Pedagogía y Ciencias de la Educación",
  "Pedagogía y Ciencias de la Educación",
  "PEM en Ciencias de la Educación",
  "PEM en Pedagogía",
  "Profesor de Enseñanza Media en Pedagogía y Ciencias de la Educación",
  "Profesorado en Pedagogía y Ciencias de la Educación",
  "PSE en Pedagogía y Ciencias de la Educación",
  "PEM EN PEDAGOGÍA Y PSICOLOGÍA","Profesorado en Pedagogía y Administración Educativa",
  "PEM Psicología y Pedagogía","PEM en pedagogía y administración educativa",
  "PEM Pedagogía y Administración Educativa",
  "PEM EN PEDAGOGIA Y TECNICO EN ADMINISTRACIÓN EDUCATIVA",
  "PEM EN PEDAGOGÍA Y TECNICO ADMON EDUCATIVA",
  "PEM EN PEDAGOGÍA Y TECNICO EN ADMÓN EDUC",
  "En Pedagogia y Ciencias de la Educaciin.",
  "En Pedagogía y  Ciencias de la Educación",
  "En Pedagogía y Ciencias de la Educación","En Segunda Enseñanza y ciencias de la Educación",
  "Enseñanza Medua en Pedagogia y Cuencias de la Educacion.",
  "PEM En Pedagogía y Ciencias de la Educación",
  "PEM en Pedagogìa y Ciencias de la Educaciòn",
  "PEM en Pedagogía y Ciencias de la Educacion",
  "PEM en Pedagogía y Ciencias de la Esucacion",
  "PEM en Pedagogía y Ciencias de la educación",
  "PEDAGOGIA Y CC de la educacion",
  "Profesorado en Ciencias de la Educacion",
  "Profesorado en Ciencias de la Educación con Orientación en Pedagogía",
  "Pedagogia En cienciad de la educacion",
  "en pedagogía y ciencias de la educación",
  "Profesorado en pedagogia y ciencias de la educacion",
  "PEM EN PEDAGOGIA Y TECNICO EN ADMINISTRACIÓN EDUCATIVA",
  "PEM EN PEDAGOGÍA Y TECNICO ADMON EDUCATIVA",
  "PEM EN PEDAGOGÍA Y TECNICO EN ADMÓN EDUC",
  "PEM EN PEDAGOGÍA Y TÉCNICO EN ADMINISTRACIÓN EDUCATIVA",
  "PEM EN PEDAGOGÍA Y TÉCNICO EN ADMÓN. EDUCATIVA",
  "PEM EN PEDAGOGÍA Y TÉCNICO EN INVESTIGACIÓN EDUCATIVA",
  "PEM EN pedagogía y Tecnica en Administración Educativa",
  "PEM en Pedagogía y Administración Educativa",
  "PEM en pedagogía y Administración Educativa",
  "PEM en pedagogía y administración Educativa",
  "PEM en pedagogía y administración educativaa",
  "PEM en pedagogía y Técnico en Administración Educativa",
  "PEM en pedagogía y técnica en administración educativa",
  "PEM en Pedagogía y Técnico en Administración Educativa",
  "PEM en Pedagogía y Técnico En Administración Educativa",
  "PEM en Pedagogía y Técnico en Administración Educaftiva",
  "PEM en Pedagogía y Técnico en Administración Educativa con Orientación en Medio Ambiente",
  "PEM en Pedagogía y Técnico en Admón. Educativa",
  "PEM en Pedagogía y Técnico en dministración Educativa",
  "PEM en Pedagogía y técnico en Administración Educativa",
  "PEM Y ADMINISTRACION EDUCATIVO","PEM EN PEDAGOGÍA, CIENCIAS SOCIALES Y FORMACIÓN CIUDADANA",
  "PEM En pedagogía, ciencias sociales y formación ciudadana",
  "PEM en Pedagogía en  Ciencias Sociales y Formación Ciudadana.",
  "PEM en pedagogía, Ciencias Sociales y Formación Ciudadana",
  "PEM en pedagogía, Ciencias Sociales y Formación Ciudadana",
  "PEM en Pedagogia Sociales y formación Ciudadana",
  "PEM y Técnico en Admon. Educativa","PEM en Liderazgo y Gestión Educativa",
  "PEM y técnico en administración educativa",
  "PEM. En Pedagogía y Técnica en Administración Educativa",
  "PEM. En Pedagogía y Técnico Administración Educativa",
  "PEM. En Pedagogía y Técnico de administración educativa con orientación en medio ambiente",
  "PEM. Pedagogía y Técnico en Administración Educativa",
  "PEM EN PEDAGOGÍA Y TÉCNICO EN ADMINISTRACIÓN EDUCATIVA",
  "PEM EN PEDAGOGÍA Y TÉCNICO EN ADMÓN. EDUCATIVA","PEM en pedagogia",
  "PEM en Pedagogía y Ciencias de la Educación",
  "Pem en pedagogía y ciencias naturales",
  "Pem en pedagogía, física y matemáticas",
  "Profesorado en Enseñanza Media y Ciencias de ka Educación", 
  "Profesor De Enseñanza Media En Pedagogía y Ciencias De La Educación",
  "Profesor de Enseñanza Media en Pedagogía y Ciencias de la Educación",
  "Profesor de enseñanza media en ciencias de la educación",
  "Profesor de enseñanza media en pedagogía",
  "Profesor de enseñanza media en pedagogía y ciencias de la educación",
  "Profesora de Enseñanza Media en Pedagogia y Ciencias de la Educación",
  "Profesora de enseñanza media en pedagogía y ciencias de la educación",
  "Profesorado de Enseñanza Media en Pedagogía y Ciencias de la Educación",
  "Profesorado en Enseñanza Media en Pedagogía y Ciencias de la Educación",
  "Profesorado en pedagogía y ciencias de la educación",
  "PEM EN PEDAGOGÍA Y TÉCNICO EN INVESTIGACIÓN EDUCATIVA",
  "PEM EN pedagogía y Tecnica en Administración Educativa",
  "PEM en Pedagogía y Administración Educativa",
  "PEM en pedagogía y Administración Educativa",
  "PEM en pedagogía y administración Educativa",
  "PEM en pedagogía y administración educativaa",
  "PEM en pedagogía y Técnico en Administración Educativa",
  "PEM en pedagogía y técnica en administración educativa",
  "PEM en Pedagogía y Técnico en Administración Educativa",
  "PEM en Pedagogía y Técnico En Administración Educativa",
  "PEM en Pedagogía y Técnico en Administración Educaftiva",
  "PEM en Pedagogía y Técnico en Administración Educativa con Orientación en Medio Ambiente",
  "PEM en Pedagogía y Técnico en Admón. Educativa","PEM en ciencias de la educación",
  "PEM en Pedagogía y Técnico en dministración Educativa",
  "PEM en Pedagogía y técnico en Administración Educativa",
  "PEM. En Pedagogía y Técnica en Administración Educativa",
  "PEM. En Pedagogía y Técnico Administración Educativa",
  "PEM. En Pedagogía y Técnico de administración educativa con orientación en medio ambiente",
  "PEM. Pedagogía y Técnico en Administración Educativa"
)


### pem educacion fisica
valores_pem_fisica <- c(
  "Educación Física", "Profesor de Educación física",
  "Profesor de Educación Física", "Profesora de Esucacion Física",
  "PEM en Educación Física","Profesorado en Educación Física",
  "Profesor de Educación Física","En Educación Física",
  "Maestra de Educación Física",
  "Profesor De Educación Física","Pem en educación física",
  "Profesorado de Enseñanza Media: Educación Física",
  "Profesor de Educacio Fisica", "Profesorado de Enseñanza Media: Educación Física",
  "Profesorado en Educación Media (PEM) en Pedagogía con Especialidad en Educación Física.",
  "Profesorado en Educación Media con especialidad en Educación Física",
  "Profesorado en Enseñanza Media: Educación Física",
  "Profesorado en Educación Física, Deporte y recreación Física",
  "Profesorado de Educación Física"
)

###  pem administracion
valores_PEM_admin <- c(
  "P.E.M. en Administración Educativa y Técnico en Administración Educativa",
  "PEM Admón Educativa","En Pedagogía y Técnico en Administración Educativa",
  "En Pedagogía y Técnicos en Administración Educativa",
  "En enseñanza media en Pedagogía y técnico en administración educatica",
  "PEM con orientación en Administración Educativa",
  "Profesor de Enseñanza Media y Técnico en Administración Educativa",
  "Profesorado de Enseñanza Media: Administración Educativa",
  "PEM EN ADMINISTRACIÓN EDUCATIVA", "De enseñanza Media y Técnico en Administración Educativa",
  "En Pedagogía y administración Educativa",
  "Profesorado de Segunda Enseñanza en pedagogía con orientación en dirección de Centros Educativos",
  "Profesor de enseñanza media en pedagia y técnico en administración educativa", 
  "PEM en educación administrativa","PEM. en Pedagogía y Administración Educativa",
  "PROFESORADO EN ENSEÑANZA MEDIA Y TECNICO EN ADMINISTRACION EDUCATIVA",
  "Profesor de Ensañaza Media Espezializado en Liderazgo y Gestion Educativa.",
  "Profesor de enseñanza Media en pedagogía y tecnico en administración educativa con especialidad en Matemáticas.",
  "Profesor de enseñanza Media y Técnico administrativo",
  "Profesor de enseñanza media en pedagogia y tecnico en administración educativa",
  "Profesor de enseñanza media en pedagogía y técnico en administración educativa",
  "Profesora de Enseñanza Media en Pedagogia y Técnico en Administración Educativo",
  "Profesora de Segunda Enseñanza en Pedagogía con Orient. En Direc. Y Admin. De Centros Educativos",
  "Profesora de enseñanza media y técnico en administración educativo",
  "Profesora de enseñanzas media en pedagogía y técnico en administración educativa",
  "Profesorado de Enseñanza Media: Administración Educativa",
  "Profesorado de Enseñanza Media en Liderazgo y Gestión Educativa",
  "Profesorado de enseñanza media en liderazgo y gestión educativa",
  "Profesorado de enseñanza media en Pedagogía y Técnico en Administración Educativa",
  "Profesorado de enseñanza media en pedagogia y tecnico en administracion eduativa",
  "Profesorado de enseñanza media y tecnico en administración educativa",
  "Profesorado de enseñanza media y técnica en administración educativa",
  "Profesorado de enseñanza mediaia y técnico en administración educativa",
  "Profesorado de enseñaza media y tecnico en admistracion educativa",
  "Profesorado en Enseñanza Media en Ciencias de la Educación con Énfasis en Administración Educativa",
  "Profesorado en Enseñanza Media en Pedagogía y Administración Educativa",
  "Profesorado en Enseñanza Media en Pedagogía y Técnica en Administración Educativa",
  "Profesorado en Enseñanza Media en Pedagogía y Técnico en Administración Educativa",
  "Profesorado en Enseñanza Media en Pedagogía y técnico en Administración Educativa",
  "Profesorado en Pedagogía y Técnico en Administración Educativa",
  "Profesorado en Pedagogía y técnico en Administración Educativa",
  "Profesorado en Pedagogía y técnico en administración educativa",
  "profesorado en eseñanza media en pedagogia y tecnico en administracion educativa",
  "profesorado en pedagogia administracion educativa",
  "profesorado en pedagogia en eseñansa media y administración educativa",
  "PEM. EN ADMINISTRACIÓN EDUCACTIVA","Enseñanza Media y Técnico en Administración Educativa",
  "PEDAGOGÍA Y TECNICO EN ADMINISTRACIÓN EDUCATIVA",
  "PEM  en pedagogía y Técnica en Administración Educativa",
  "PEM EN PEDAGOGIA Y TÉCNICO EN ADMINISTRACIÓN EDUCATIVA",
  "PEM En Pedagogía y Tecnico En Admon Educativo",
  "PEM PENDAGOGIA Y TECNICO EN ADMINISTRACION EDUCATIVA",
  "PEM en Administración y Técnico Administrativo",
  "PEM en Pedagogía y Tecnico en Admon Educativa",
  "PEM en pedagogía  y Técnico en administrativa",
  "PEM. En Pedagogía Administrativa",
  "PEM. En pedagogía y Administración Educativa",
  "Profesor de Enseñanza Media y Técnico en Administración Educativa",
  "Profesorado de Enseñanza Media: Administración Educativa",
  "Profesorado en Pedagogia y Administración Educativa",
  "Profesorado en pedagogía y administración educativa",
  "Profesorado en Enseñanza Media y Técnico en Administración Educativa",
  "Profesor de enseñanza media y técnico en administración educativa",
  "Profesora de Enseñanza Media y Técnico en Administración Educativa",
  "Profesor de Enseñanza Media y Técnico en Administración Educativa",
  "Profesorado de Enseñanza Media: Administración Educativa",
  "Profesorado en Pedagogía y Administración Educativa",
  "profesor de enseñanza media y administración Educativa",
  "PEM Técnico Administración Educativa",
  "Administración Educativa",
  "Administración educativa","Liderazgo y Gestión Educativa",
  "PEM en técnico administrativo",
  "Profesorado en Administración Educativa",
  "Administración educativo",
  "Administrativo","PEM en administración educativa",
  "PEM EN ADMON EDICATIVA","PEM EN Pedagogía y Administración Educativa. PEM EN Ciencias Sociales y Formación Ciudadana",
  "PEM en Pedagogía y Tecnico en proyectos educativos",
  "PEM en Pedagogía y Técnica en Administración Educativa",
  "PEM. En Pedagogía y Técnica en Administración Educativa con Orientación en Medio Ambiente",
  "PEM. En pedagogía y técnico de administración educativa con orientación en medio ambiente",
  "PEM.Pedagogia y Técnica en Administración Educativa",
  "PROFESOR DE ENSEÑANZA MEDIA Y TECNICO EN ADMINISTRACION EDUCATIVA",
  "PROFESORADO DE ENSEÑANZA MEDIA CON ESPECIALIDAD EN ADMINISTRACIÓN EDUCATIVA",
  "PROFESORADO EN PEDAGOGIA Y ADMINISTRACION EDUCATIVA",
  "PROPFESORADO EN PEDAGOGIA Y ADMINISTRACIÓN EDUCATIVA",
  "PSE EN PEDAGOGÍA CON ORIENTACIÓN EN DIRECCIÓN Y ADMINISTRACIÓN DE CENTROS EDUCATIVOS EN",
  "PSE con Orientación y Administración de Centros Educativos",
  "PSE en PEdagogía con Orientación en Dirección y Administración en Centros Educativos",
  "Pedagogía y técnico en administración Educativa.",
  "Prof. De Segunda Enseñanza en Pedagogía con Orient. En Direc. Admin. De Centros Educativos",
  "Profesor de  Enseñanza Media  y Tecnico en Administracion Educativay Tecnico Administrati",
  "Profesor de Enseñanza Media con Orientacion en Medio Ambiente",
  "Profesor de Enseñanza Media en Liderazgo y Gestión Educativa",
  "Profesor de Enseñanza Media en Pedagogía con Especialidad en Administración Educativa",
  "Profesor de Enseñanza Media en Pedagogía y Administración Educativa",
  "Profesor de Enseñanza Media en Pedagogía y Técnico en Administración Educativa",
  "Profesor de Enseñanza media y Técnico administrativo",
  "Profesor de enseñanza media en pedagogia y técnico en administración educativa",
  "Profesor de enseñanza media en pedagogía con especialización en administración",
  "Profesor de enseñanza media en pedagogía y administración educativa",
  "Profesor de enseñanza media y Técnico en administración educativa",
  "Profesor de enseñanza media y técnico en administración Educativa",
  "Profesor de enseñanza media y técnico en administración educativo",
  "Profesor de enseñanza medía en pedagogía y tecnico admón educativa",
  "Profesor en Enseñanza Media en Pedagogía y Administración. Educativa",
  "Profesor en Enseñanza Media y Técnico en Administración Educativa",
  "Profesora de Enseñanza Media en Pedagogía y Administración Educativa",
  "Profesora de Enseñanza Media en Pedagogía y Técnica en Administración Educativa con Orientación en Medio Ambiente",
  "Profesora de Enseñanza Media en Pedagogía y Técnico en Administración Educativa con Orientación en Medio Ambiente",
  "Profesora de Enseñanza Media en pedagogía y Técnica en Administración educativa",
  "Profesora de Enseñanza Media y técnico en Administración Educativa",
  "Profesora en Pedagogía con especialización en Administración Educativa",
  "Profesorado De Enseñanza Media y Técnico en Administración Educatica",
  "Profesorado de Enseñanza Media y Administración Educativa",
  "Profesorado de Enseñanza Media: Administración Educativa",
  "Profesorado de Enseñanza media en Pedagogiia y Administración Educativa",
  "Profesorado de Enseñanza Media en Pedagogía  y Técnico en Administración Educativa",
  "Profesorado de Enseñanza Media en Pedagogía y Administración Educativa",
  "Profesorado de Enseñanza Media en Pedagogía y Técnica en Administración Educativa",
  "Profesorado de Enseñanza Media en Pedagogía y Técnico en Administración Educativa",
  "Profesorado de Enseñanza Media en Pedagogía y Técnico en Administración Educativa con orientación en Medio Ambiente",
  "Profesorado en Enseñanza  Media y Técnico en Administración  Educativa",
  "Profesorado en Enseñanza Media con especialidad en Administración  Educativa",
  "Profesorado en Enseñanza Media y Administración Educativa",
  "Profesorado en Enseñanza media en  pedagogía y técnico en Administracion Educativa",
  "Profesorado en Enseñanza media en Pedagogia y técnico en administración Educativa",
  "Profesorado en enseñanza media con orientación en pedagogía y administración educativa",
  "Profesorado en enseñanza media en Pedagogia y Tecnico en  Administración Educativa",
  "Profesorado en enseñanza media en Pedagogía y técnico administrativo",
  "Profesorado en enseñanza media y técnica administración educativa",
  "Profesorado en enseñanza media y técnico en administración educativa",
  "Profesorado en pedagogía y técnico en administración educativa",
  "Profesorado en pedagogia administracion educativa",
  "Profesorado en pedagogia en eseñansa media y administración educativa",
  "Profesrado de ensañanza media con orientacion en administracion Educativo",
  "PEM en Educación y Técnico Administrativo",
  "PEM y Tecnico en Administración Educativa",
  "PEM y técnica en administración educativa",
  "Técnico en Administración Educativa","En Pedagagia y  ciencias  de  la  Educacion",
  "En Administración Educativa","Técnico administrativo",
  "Técnico en administración","En Enseñanza Media y Técnico en Administración Educativa",
  "En enseñanza Media y Técnico en Administración Educativa",
  "En enseñanza media y técnico en administración educativa",
  "En enseñanza media en pedagogía y técnico en administración educativa",
  "PEM en Pedagogía y Técnico en Administración Educativa",
  "PEM. En Pedagogía y Administración Educativa",
  "PEM. Y Técnico en Administración Educativa",
  "Profesor de Enseñanza Media y Tecnico en Administración Educativa",
  "Profesorado de Enseñanza Media y Técnico en Administración Educatica",
  "Profesorado en Enseñanza Media y Técnico en Administración Educativa",
  "Profesorado en Enseñanza Media: Administración Educativa",
  "Técnico en Administración educativa",
  "Técnico en administración educativo",
  "En Pedagogía y Administración Educativa",
  "En pedagogía y administración educativa",
  "Pedagogía y Administración Educativa",
  "PEM en Administración Educativa","PEM en Pedagogía y Promotor de Derechos Humanos",
  "PEM en Pedagogía y Tec. En Derechos Humanos",
  "PEM en Administración y Gestión Educativa",
  "PEM en pedagogía y administración educativa",
  "Profesor de Enseñanza Media y Técnico en Administración Educativa",
  "Profesorado en Pedagogía y Administración Educativa",
  "De Enseñanza Media y Técnico en Investigación Educativa",
  "Enseñanza Media y Técnico en Administración Educativa",
  "PEDAGOGÍA Y TECNICO EN ADMINISTRACIÓN EDUCATIVA",
  "PEM  en pedagogía y Técnica en Administración Educativa",
  "PEM  en pedagogía y técnico en administración Educativa",
  "PEM EN PEDAGOGIA Y TÉCNICO EN ADMINISTRACIÓN EDUCATIVA",
  "PEM En Pedagogía y Tecnico En Admon Educativo",
  "PEM PENDAGOGIA Y TECNICO EN ADMINISTRACION EDUCATIVA",
  "PEM en Administración y Técnico Administrativo",
  "PEM en Pedagogía y Tec. en Admon. Educativa",
  "PEM en Pedagogía y Tecnico en Admon Educativa",
  "PEM en pedagogía  y Técnico en administrativa",
  "PEM. En Pedagogía Administrativa",
  "PEM. En Pedagogía y Administración e",
  "PEM. En pedagogía y Administración Educativa",
  "PEM. pedagogía y técnico en administración educativa",
  "Pem en Pedagogía técnico en administración educativa",
  "Pem. Pedagogía y técnico en Administración Educativa",
  "Profesor  de Enseñanza Media y Técnico en Administración Educativa",
  "Profesor de Enseñanza Media en Pedagogia y Técnico en Administración Educativa",
  "Profesor de Enseñanza Media y Técnico en Admon Educativa",
  "Profesor de Enseñanza Media y técnico en administración Educativa",
  "Profesor de Enseñanza media en Pedagogía y Administración Educativa",
  "Profesor de enseñanza Media en pedagogía y tecnico en Administración Educativa.",
  "Profesor de enseñanza media y Técnico administrativo",
  "Profesor enseñanza media y Técnico Administracion Educativa",
  "Profesora de Enseña Media en Pedagogía y Técnico en Administración Educativa",
  "Profesora de Enseñanza  Media en Pedagogía y Técnica en Administración  Educativa",
  "Profesora de Enseñanza Media en Pedagogia y Tecnica en Administración Educativa",
  "Profesora de Enseñanza Media en Pedagogía y Técnica en Administración Educativa",
  "Profesora de Enseñanza Media en Pedagogía y Técnico en Administración Educativa",
  "Profesora de Enseñanza Media y Técnico en Administración Educativa",
  "Profesorado de Enseñanza Media y Técnico en Administración Educatica",
  "Profesorado de Enseñanza Media y Técnica en Administración Educativa",
  "Profesorado de Enseñanza Media y Técnico en Admon. Educativa",
  "Profesorado de enseñanza media con técnico en administración educativa",
  "Profesorado en Enseñanza Media Y Técnico en Administración Educativa",
  "Profesorado en Enseñanza Media y Técnico en Administración Educativa.",
  "Profesorado de Enseñanza Media y Técnico en Administración Educativa"
)

### economico contable
valores_pem_economico <- c(
  "Ciencias Económico Contables",
  "Económico contable",
  "PEM en Ciencias Económico Contables",
  "Profesor de Enseñanza Media en Económico Contable",
  "PEM ECONOMICO CONTABLE","PEM EN CIENCIAS ECONÓMICO CONTABLES",
  "PEM EN CIENCIAS ECONOMICO CONTABLES",
  "PEM en Ciencias Económico Contable",
  "PEM en Ciencias económico Contables",
  "PEM. en Economico Contable","PEM c9n especialización en mercadotecnia",
  "PEM/En Ciencias Económico Contables",
  "Profesorado de Enseñanza Media: Económico Contable",
  "Profesorado de Enseñanza Media en Ciencias Económicas",
  "Profesorado de Enseñanza Media en ciencias comerciales",
  "Profesor de enseñanza media con especialización en ciencias comerciames",
  "Profesora de enseñanza media en pedagogia y tecnico en administracion de empresas",
  "PEM ECONOMICO CONTABLES", "Ciencias Contables",
  "Economíco contable h","PEM. en Ciencias Económico Contables",
  "Profesorado en Ciencias Comerciales",
  "Profesorado de Enseñanza Media: Económico Contable",
  "En especialización en Económico Contable",
  "Enseñanza Media en Ciencias Económico Contables",
  "PEM en Ciencias Económico-Contable",
  "PEM en ciencias económico contables",
  "PEM en Pedagogía y Ciencias Económicas Contables",
  "Pem, ciencias económico contable",
  "Pem, ciencias económico contables",
  "Profesor Enseñanza Media en Económico Contable",
  "Profesor de la enseñanza de las ciencias económico contable",
  "Profesorado en Ciencias Económico Contables",
  "Profesorado en ciencias Económico contable",
  "Profesorado en Enseñanza Media en Económico Contable"
)

### pem musica
valores_pem_Musica <- c(
  "P.E.M. en Música","PEM EN PEDAGOGÍA CON ESPECIALIDAD EN MÚSICA",
  "En arte con especialización en música","Profesorado de Enseñanza media especializado en música",
  "Profesorado en Enseñanza Media: Música","Profesorado de Enseñanza Media: Música",
  "Profesorado de Enseñanza Media en Educación Musical",
  "Profesorado en Enseñanza Media en Expresión Artística con Especialidad en Música",
  "Profesorado de enseñanza media con especial en Música",
  "Profesorado en enseñanza media con especialidad en música",
  "Profesorado en Educación Musical","Profesorado en Educación Musical",
  "musica", "PEM en Educación Musical",  "Profesorado en Educación Musical"
)

### productividad y desarrollo
valores_pem_pyd <- c(
  "Productividad y Desarrollo",
  "Profesorado en Productividad y Desarrollo",
  "Profesor de Segunda Enseñanza en Pedagogía y Ciencias de la Educación con especialidad en Productividad y Desarrollo",
  "Profesorado de Enseñanza Media: Productividad y desarrollo",
  "Profesorado en Enseñanza Media: Productividad y desarrollo",
  "Profesorado en Emprendimiento para la Productividad",
  "Profesor de Segunda Enseñanza (PSE) en Pedagogía y Ciencias de la Educación con especialidad en Productividad y Desarrollo"
)

### pem artes plasticas
valores_PEM_artes <- c(
  "P.E.M en Artes Plásticas e Historia del Arte",
  "PEM Artes Plásticas e Historia","PEM en Artes Plásticas e Historia del arte",
  "Tecnico artístico en artes plasticas","Profesorado en Enseñanza Media en Artes Plásticas e Historia del Arte",
  "Profesora en Enseñanza Media en Artes Plásticas e Historia del Arte.",
  "PEM EN ARTES PLASTICAS E HISTORIA DEL ARTE",
  "PEM ESPECIALIDAD Artes Plásticas",
  "Profesora en Segunda Enseñanza en Artes Plásticas e Historia del Arte",
  "Profesora en Segunda Enseñanza en Artes Plásticas e Historia del Arte.",
  "Profesorado de Enseñanza Media: Artes Plásticas",
  "Profesorado de Eseñanza Media en Artes Pládticas",
  "PEM. Artes Plásticas e Historia del Arte.",
  "PEM Artes Plásticas e Historia del Arte",
  "En Expresión Artística con especialidad en Música",
  "Diplomado en formación musical","Profesorado de Enseñanza Media: Artes Plásticas",
  "Profesorado de Enseñanza Media En Artes Plásticas e Historia del Arte",
  "Profesorado de Enseñanza Media en Artes Plasticas e Historia del Arte",
  "Profesorado de Enseñanza Media en Artes Plásticas e Historia del Arte",
  "Profesorado de Enseñanza Media En Artes Plásticas e Historia del Arte",
  "Profesorado de Enseñanza Media: Artes Plásticas",
  "Profesorado de Enseñanza Media En Artes Plásticas e Historia del Arte",
  "Profesorado de Segunda Enseñanza en Artes Plásticas e Historia del Arte",
  "Profesorado de Segunda Enseñanza en Artes Plásticas e Historia del Arte.",
  "Profesorado en expresión Artística con especialidad en Artes Plásticas",
  "PEM en Artes Plásticas e Historia del Arte", "Profesorado en Artes Plásticas e Historia del Arte",
  "Profesorado en Artes Plásticas e Historia del Arte"
)


### ciencias sociales
valores_pem_sociales <- c(
  "Ciencias Sociales",
  "Ciencias sociales","PROFESORADO EN CIENCIAS SOCIALES Y FORMACION CIUDADANA",
  "Profesorado en Historia y Ciencias Sociales",
  "Profesorado en Ciencias Sociales","Pedagogía y ciencias sociales",
  "Pem en cc ss. Y formación ciudadana",
  "PEM EN Ciencias Sociales y Formación Ciudadana",
  "PEM en pedagogía, ciencias sociales y formación ciudadana",
  "PROFESORADO EN CINCIAS SOCIALES Y FORMACION CIUDADANA",
  "PROFESORADO EN HISTORIA Y CIENCIAS SOCIALES",
  "PSE En Ciencias Sociales","Profesorado en Pedagogía con especialidad en ciencias sociales",
  "PSE En Pedagogía y Ciencias Sociales",
  "PSE en Ciencias sociales","PEM en pedagogía y técnico en adelante educativa PEM en pedagogía, ciencias sociales y formación ciudadana",
  "PROFESORADO EN PEDAGOGIA Y CIENCIAS SOCIALES",
  "Profesor de segunda enseñanza en pedagogía y ciencias sociales",
  "Profesor en pedagogía  y ciencias sociales",
  "Profesor de enseñanza media y ciencias de la educación Y profesot de educación primaria Interciltural",
  "Profesor de segunda enseñanza en pedagogía y ciencias sociales",
  "Profesora de segunda enseñanza en pedagogía y ciencias sociales",
  "Profesorado de Enseñanza En Pedagogía, Ciencias Sociales y Formación Ciudadana",
  "Profesorado de Enseñanza Media En Pedagogía, Ciencias Sociales y Formación Ciudadana",
  "Profesorado de Enseñanza Media: Ciencias Sociales",
  "Profesorado de Pedagogía y Ciencias Sociales",
  "Profesorado de Segunda Enseñanza en Pedagogía y Ciencias de la educación",
  "Profesorado de enseñanza Enseñanza Media en Pedagogía Ciencias Sociales y Formación Ciudadana",
  "Profesorado de enseñanza media con especialidad en ciencias sociales y formación ciudadana",
  "Profesorado de enseñanza media en ciencias sociales y Formación Ciudadana",
  "Profesorado de enseñanza media en historia y ciencias sociales",
  "Profesorado de segunda enseñanza en pedagogía y ciencias",
  "Profesorado de segunda enseñanza en pedagogía y ciencias sociales",
  "Profesorado de segunda enseñanza en pegagia y ciencias sociales",
  "Profesorado en enseñanza media con especialización en ciencias sociales",
  "Profesorado en enseñanza media en historia y ciencias sociales",
  "Profesorado en segunda Enseñanza en Pedagogía y Ciencias Sociales",
  "Pedagogía en ciencia sociales",
  "Profesado en Enseñanza Media Especializado en Ciencias sociales y formación ciudadana",
  "Profesor de Enseñanza Media en Pedagogía Ciencias Sociales Formación Ciudadana e Interculturalidad",
  "Profesor de Segunda Enseñanza en Pedagogia y Ciencias Sociales",
  "Profesor de Segunda enseñanza en Pedagogía y Ciencias Sociales",
  "Profesor de segunda Enseñanza en pedagogía y ciencias sociales",
  "Profesorado de Enseñanza Media: Ciencias Sociales",
  "Profesorado de Enseñanza Media Ciencias Sociales y Formación ciudadana",
  "Profesorado de Enseñanza Media Ciencias Sociales y en Formación Ciudadana",
  "Profesorado de Enseñanza Media con Especialidad en Ciencias Sociales y Formación Ciudadana",
  "Profesorado de Enseñanza Media en Ciencias sociales y Formación ciudadana",
  "Profesorado de Enseñanza Media en Historia y Ciencias Sociales",
  "Profesorado de Enseñanza Media en Pedagogía y Ciencias Sociales",
  "Profesorado de Enseñanza Media en Pedagogía, Ciencias Sociales y Formación Ciudadana",
  "Profesorado de Segunda Enseñanza en Pedagogía y Ciencias Sociales",
  "Profesorado de segunda enseñanza  en ciencias sociales",
  "Profesorado de segunda enseñanza en Pedagogía y Ciencias Sociales",
  "Profesorado en Lenguaje y Ciencias Sociales",
  "Profesorado en Pedagogía y Ciencias Sociales",
  "Profesorado en Pedagogía, Ciencias Sociales y Formación Ciudadana",
  "Profesora de Enseñanza Media en Pedagogía, Ciencias Sociales, Formación Ciudadana.",
  "Profesora de Segunda Enseñanza en Pedagogía y Ciencias Sociales",
  "Profesora de Segunda en Enseñanza en Pedagogía y Ciencias Sociales",
  "Profesora de segunda Enseñanza en pedagogía y ciencias sociales",
  "Profesora de segunda enseñanza en Pedagogía y Ciencias Sociales",
  "Profesora de enseñanza media y ciencias sociales",
  "Profesorado en Historia y Ciencias Sociales",
  "Profesora Enseñanza Media  en  Ciencias Sociales y Formación Ciudadana",
  "Profesorado de Enseñanza Media: Ciencias Sociales",
  "Profesor de Enseñanza Media en Historia de las Ciencias Sociales",
  "Ciencias Sociales y Formación Ciudadana","PEM EN Ciencias Sociales y Formación Ciudadana",
  "PEM. En pedagogía con especialidad en Ciencias Sociales y Formación Ciudadana",
  "Profesorado en Ciencias Sociales",
  "Profesorado en Historia y Ciencias Sociales",
  "Profesora de Enseñanza Media en Historia y Ciencias Sociales.",
  "Profesorado de Enseñanza Media: Ciencias Sociales",
  "Profesorado en Ciencias Sociales y Formación Ciudadana",
  "Ciencias sociales y formación ciudadana",
  "Historia y Ciencias Sociales","En Pedagogía y Ciencias Sociales",
  "PSE EN PEDAGOGÍA Y CIENCIAS SOCIALES",
  "Profesorado EN CIENCIAS SOCIALES Y FORMACION CIUDADANA",
  "Profesorado en Historia y Ciencias Sociales",
  "Profesorado en Ciencias Sociales y Formación Ciudadana",
  "Profesorado en pedagogía y Ciencias Sociales",
  "Profesor de Segunda Enseñanza en Pedagogía y Ciencias Sociales",
  "PEM en Ciencias Sociales",
  "PEM en Historia y Ciencias Sociales",
  "Profesorado en Ciencias Sociales","P.E.M ciencias sociales y formación ciudadana",
  "PEM con especialidad en ciencias Sociales y Formación Ciudadana",
  "PEM con especialización en Ciencias Sociales",
  "PEM en historia y las ciencias sociales",
  "PEM Pedagogía y Ciencias Sociales",
  "Profesorado en Ciencias Sociales y Formación Ciudadana",
  "PEM en Ciencias Sociales y Formación Ciudadana",
  "PEM en Pedagógica ciencias Sociales y Formación Ciudadana",
  "PEM. En Pedagogía,  Ciencias Sociales y Formación Ciudadana",
  "PEM. en pedagogía con especialidad en Ciencias Sociales y Formación Ciudadana",
  "PEM en Derechos Humanos y Cultura de Paz",
  "PEM en Pedagogía y Promotor de Derechos Humanos",
  "PEM en Pedagogía y Tec. En Derechos Humanos", "Ciencias Sociales Formacion Ciudadana e Interculturalidad",
  "Ciencias Sociales Y Formación Ciudadana",
  "Ciencias sociales y Formación Ciudadana",
  "En Ciencias Sociales e Historia",
  "En Ciencias Sociales y Formación Ciudadana",
  "En pedagogia y Ciencias Sociales",
  "Lenguaje y Ciencias Sociales",
  "PEM EN CIENCIAS SOCIALES",
  "PEM EN PEDAGOGIA Y ADMINISTRACION EDUCATIVA, PEM EN CIENCIAS SOCIALES",
  "Profesorado EN CIENCIAS SOCIALES Y FORMACION CIUDADANA",
  "Profesorado EN HISTORIA Y CIENCIAS SOCIALES",
  "Profesor de Enseñanza Media en Pedagogia, Ciencias Sociales y Formacion Ciudadana",
  "Profesorado en ciencias sociales y formación ciudadana",
  "Profesorado de Enseñanza Media: Ciencias Sociales",
  "Profesorado en Historia y Ciencias Sociales"
)

### tecnologia y computacion
valores_PEM_tecno_compu <- c(
  "P.E.M. Computación e Informático","Profesora de Enseñanza Media En Tecnología Educativa",
  "Profesora de Enseñanza Media en Tecnología Educativa",
  "Profesorado de Enseñanza Media en Computación",
  "Profesorado de Enseñanza Media: Tecnología y Computación",
  "Profesorado en Enseñanza Media con Especialidad en Tecnologías de la Información y la Comunicación",
  "Profesorado en Enseñanza Media Especializada en Matemática y Computación",
  "Profesorado en Enseñanza Media en Tecnología Educativa",
  "Profesorado en Pedagogía y Tecnología de la Información y la Comunicación",
  "Profesorado en Pedagogía y Tecnologías de la Información y la Comunicación",
  "Profesorado en enseñanza media especializado en matemáticas y computación",
  "P.E.M. Especializado en Matemática y Computación",
  "PEM informática","Profesorado en Computación",
  "Profesor de Enseñanza Media en Informática y Ciencias de la Computación",
  "Profesorado tics","Profesor de Enseñanza Media con Especialidad en Computación",
  "Profesor de Enseñanza Media en Computación e Informatica.",
  "Profesorado de Enseñanza Media: Tecnología y Computación",
  "Profesorado en Informática y Ciencias de la Computación, Profesorado en Matemáticas y Física.",
  "PEM EN TECNOLOGÍA EDUCATIVA", "Informática",
  "PEM en Informática","PEM en Computación e Informática",
  "PEM en informática y Ciencias de la Computación",
  "PEM en Tecnología Educativa",
  "Profesora en Enseñanza Media Especializada en Matemática y Computación",
  "Profesorado de Enseñanza Media Especializado en Matemática y Computación",
  "Profesorado de Enseñanza Media: Tecnología y Computación",
  "Profesorado de Enseñanza Media en Tecnología Educativa",
  "Profesorado en Informática y Ciencias de la Computación",
  "PEM CON ESPECIALIDAD EN TECNOLOGÍAS DE LA INFORMACIÓN Y LA COMUNICACIÓN",
  "PEM en Tecnologías de la Información y Comunicación",
  "PEM en Informática y Ciencias de la Computación",
  "PEM de la Informática y Ciencias de la Computación",
  "PEM EN TECNOLOGIA EDUCATIVA",
  "Profesorado en Tecnología Educativa"
)

### pem psicologia
valores_pem_psicologia <- c(
  "PEM en Psicología","En Pedagogía y Psicología",
  "PEM EN PEDAGOGÍA Y PSICOLOGíA","Profesorado de Enseñanza Media en Pedagogía y Psicología",
  "Pedagogía y Psicología","PROFESORADO DE ENSEÑANZA MEDIA EN PEDAGOGÍA Y PSICOLOGÍA",
  "PROFESORADO DE ENSEÑANZA MEDIA EN PEDAGOGÍA Y PSICOLOGÍA.",
  "PSE en Pedagogía y Psicología",
  "Profesor en enseñanza media en Pedagogía y Psicología",
  "Profesorado de Enseñanza Media: Psicología",
  "Profesorado de Enseñanza Media en Pedagogía y Psicología.",
  "Profesorado de enseñanza media en psicología",
  "Profesorado de enseñanza media en pedagogía y Psicología",
  "Profesorado de segunda enseñanza en pedagogía y psicología",
  "Profesorado de Segunda Enseñanza en Pedagogia y Psicologia",
  "Profesorado en Enseñanza Media: Psicología",
  "Profesorado en pedagogpia y psicología",
  "Profesorado en pedagogía y psicología",
  "Profesora de enseñanza media en Psicología",
  "Profesorado en Psicología","Especializado en problemas del aprendizaje",
  "Pedagogía y psicología",
  "Profesorado de Enseñanza Media: Psicología",
  "Profesora de Enseñanza Media en Psicología",
  "Profesorado en Pedagogía y Psicología",
  "Profesorado en Psicología y Pedagogía",
  "Profesor de Educación Media en Pedagogía y Psicología"
)

### filosofia
valores_pem_filosofia <- c(
  "En Filosofía",
  "Profesor de enseñanza media en filosofía",
  "Profesorado de Enseñanza Media: Filosofía",
  "Profesorado de Enseñanza Media en Lenguaje, Filosofía y Estudios Sociales",
  "Profesor de Enseñanza Media en Filosofía",
  "Profesorado en Filosofía", "En Filosofía y Ciencias de la Religión",
  "Profesorado en Filosofía y Ciencias de la Religión"
)

### investigacion educativa
valores_pem_invest <- c(
  "PEM en investigación educativa", "PEM en Investigación Educativa",
  "En Enseñanza Media y Técnico en Investigación Educativa",
  "De Enseñanza Media y Técnico en Investigación Educativa",
  "PEM EN PEDAGOGIA Y TÉCNICA EN INVESTIGACIÓN EDUCATIVA",
  "PEM en Ped. Y Técnico en Inv. Educativa",
  "PEM en investigacion educativa",
  "Profesora de Enseñanza Media en Pedagogía y Técnico en Investigación Educativa",
  "Profesorado en pedagogía y técnica de investigación",
  "Profesorado en Pedagogía y Técnico en Formulación de Proyectos Educativos"
)

### no especifica
valores_pem_sinespecificar <- c(
  "Giovanny Reyes Guevara",
  "...", "PEM Y PEPI", "Se desconoce","Nivel medio",
  "No mencionó", "xx", "Ninguna", "Educación Media",
  "En Educación Media","Rosa Puac Pineda de Romero",
  "Enseñanza Media","MEF", "Magisterio de primaria urbana",
  "PSE","PEPI","PADEP/DEF",
  "Nivel Medio","PEM",
  "Pe y tae",
  "Profesorado de  Enseñanza media",
  "Profesorado de Educación Media",
  "Profesorado en Educación Media",
  "Profesorado en Enseñanza Media",
  "Profesorado en enseñanza media",
  "profesorado en enseñanza media",
  "Profesorado en educación media","Profesorado en enseñanza media de la educación",
  "Pem","PEM en enseñanza media",
  "PEM.", "Enseñanza media",
  "Profesor de educación media",
  "No especifica", "Profesor de enseñanza media",
  "Profesor de segunda enseñanza", "PROFESOR DE ENSEÑANZA MEDIA",
  "Profesor de Educación Media",
  "Profesor de Enseñanza Media",
  "Profesor de Ensen̈snza Media",
  "Profesor de ensrñanza media",
  "Profesor en enseñanza media",
  "Profesor en segunda enseñanza",
  "Profesorado de Enseñanza Media",
  "Profesorado de Enseñanza media",
  "Profesorado de enseñanza Media",
  "Profesorado enseñanza media",
  "Profesora de Enseñanza Media",
  "Profesora de segunda enseñanza"
)

### preprimaria
valores_pem_preprimaria <- c(
  "Especializado en Educación Primariamaria",
  "Profesora  universitaria con especialidad en pre primaria",
  "Profesora de educacion preprimara intercultural",
  "Profesorado de Enseñanza Media: Preprimaria",
  "Profesora universitaria con especialidad en pre primaria",
  "Profesora universitaria con especialidad en pre primaria",
  "Profesora universitaria con especialidad en pre primaria"
)

### primaria
valores_pem_primaria <- c(
  "Profesora en Educación Primaria",  "profesorado de Educacion Primaria Urbana"
)

## REcode con case match ----

df_dyd_rost_titulos <- df_dyd_rost_titulos |>
  mutate(
    reportado_profesorado = case_match(
      reportado_profesorado,
      all_of(valores_pem_intercultural) ~ "Profesorado de Enseñanza Media: Educación Intercultural",
      all_of(valores_pem_sinespecificar) ~ "No especifica",
      all_of(valores_pem_primaria) ~ "Profesorado de Enseñanza Media: Primaria",
      all_of(valores_pem_preprimaria) ~ "Profesorado de Enseñanza Media: Preprimaria",
      all_of(valores_pem_filosofia) ~ "Profesorado de Enseñanza Media: Filosofía",
      all_of(valores_pem_invest) ~ "Profesorado de Enseñanza Media: Investigación educativa",
      all_of(valores_pem_psicologia) ~ "Profesorado de Enseñanza Media: Psicología",
      all_of(valores_pem_sociales) ~ "Profesorado de Enseñanza Media: Ciencias Sociales",
      all_of(valores_pem_pyd) ~ "Profesorado de Enseñanza Media: Productividad y desarrollo",
      all_of(valores_pem_fisica) ~ "Profesorado de Enseñanza Media: Educación Física",
      all_of(valores_PEM_matefisica) ~ "Profesorado de Enseñanza Media: Matemática y Física",
      all_of(valores_pem_naturales) ~ "Profesorado de Enseñanza Media: Ciencias Naturales",
      all_of(valores_PEM_tecno_compu) ~ "Profesorado de Enseñanza Media: Tecnología y Computación",
      all_of(valores_PEM_artes) ~ "Profesorado de Enseñanza Media: Artes Plásticas",
      all_of(valores_pem_Musica) ~ "Profesorado de Enseñanza Media: Música",
      all_of(valores_PEM_admin) ~ "Profesorado de Enseñanza Media: Administración Educativa",
      all_of(valores_pem_economico) ~ "Profesorado de Enseñanza Media: Económico Contable",
      all_of(valores_PEM_pedagogia) ~ "Profesorado de Enseñanza Media: Pedagogía",
      all_of(valores_PEM_lenguaje) ~ "Profesorado de Enseñanza Media: Lenguaje e idiomas",
      .default = reportado_profesorado
    )
  )

## volver a ver errores

valores_reportado_profesorado <- df_dyd_rost_titulos |>
  filter(reportado_grado_academico == "Profesorado")|>
  filter(!(is.na(reportado_profesorado))) |>
  select(reportado_profesorado) 

#View(valores_reportado_profesorado)



# reportado_tecnico_universitario ----

## ver errores
valores_reportado_tecnico_universitario <- df_dyd_rost_titulos |>
  filter(reportado_grado_academico == "Técnico Universitario")|>
  filter(!(is.na(reportado_tecnico_universitario))) |>
  select(reportado_tecnico_universitario) 

#dput(valores_reportado_tecnico_universitario)

## Define los vectores ----

### administracion educativa
valores_tec_admin_edu <- c(
  "Administración Educativa",
  "En Administración Educativa",
  "En administración educativa",
  "PEM CON ÉNFASIS EN ADMINISTRACIÓN EDUCATIVA",
  "PEM EN EDUCACION Y TECNICO EN ADMINISTRACION EDUCATIVA",
  "PEM EN EDUCACION Y TECNICO EN ADMINISTRACION EDUCATIVO",
  "PEM EN PEDAGOGIA  Y TÉCNICO EN ADMINISTACIÓN EDUCATIVA",
  "PEM EN PEDAGOGIA Y TECNICO EN ADMINISTRACION EDUCATIVA",
  "PEM EN PEDAGOGÍA Y TÉCNICO EN ADMÓN EDUCATIVA",
  "PEM en Administración Educativa",
  "PEM en Admón Educativa",
  "PEM en Pedagogía y Administración Educativa",
  "PEM en pedagogía en enseñanza media y técnico en administración educativa",
  "PEM en pedagogía y administración educativa",
  "PEM en administración Educativa y técnico en Administración Educativa Especializado en Telesecundaria",
  "PEM y Técnico en Administración Educativa",
  "PEM. EN PEDAGOGÍA Y TÉCNICO EN ADMINISTRACIÓN EDUCATIVA",
  "PEM. Y técnico en administración educativa",
  "Profesor de Enseñanza Media y Técnico en Administración Educativa",
  "Profesor de Enseñanza Media en Pedagogía y Técnico en Administración Educativa",
  "Profesora de Educación Media y Técnico en Pedagogía y Administración Educativa",
  "Profesora de Enseñanza Media y Técnica en Administración Educativa",
  "Profesora de Enseñanza Media y Técnico en Administración Educativa",
  "Profesora de Enseñanza Media en Pedagogía y Administración Educativa",
  "Profesora de Enseñanza Media en Pedagogía y Técnica en Administación Educativa.",
  "Profesora de Enseñanza Media en Pedagogía y Técnica en Administración Educativa",
  "Profesora de Enseñanza Media en Pedagogía y Técnico en Administración Educativa",
  "Profesora de Segunda Enseñanza con Orientación y administración de centros Educativos",
  "Profesora de Segunda Enseñanza con orientación y administración de centros educativos.",
  "Profesora de enseñanza media y administracion educativa",
  "Profesora de enseñanza media y técnico en administración educativa",
  "Profesora en Enseñanza Media y Técnico en Administración Educativa",
  "Profesorado Enseñanza Media en pedagogía co orientación en dirección  y administración  en Centros Educativos",
  "Profesorado de Enseñanza Media en Pedagogía Y Técnico en administración Educativa",
  "Profesorado de Enseñanza Media en Pedagogía y Técnica en Administración Educativa",
  "Profesorado de Enseñanza Media en Pedagogía y Técnico en Administración Educativa",
  "Profesorado de Enseñanza Media en Pedagogía y Técnico en Administración Educativa.",
  "Profesorado en Pedagogía y Técnico en Administración Educativa",
  "Profesorado en pedagogía y tecnico en Administración educativa",
  "TECNICO EN ADMINISTRACIÓN EDUCATIVA",
  "Técnico Admon. Educativa y profesorado en Pedagogía",
  "Técnico en Administración Educativa",
  "Técnico en administración educativa"
)


### economico
valores_tec_economico <- c(
  "Contaduría pública y auditoría",
  "Auditor Técnico",
  "Técnico en Administración de Empresas",
  "Técnico en administración de empresas",
  "Tecnico Universitario en Tecnólogia y Administracion"
)

### sociales
valores_tec_sociales <- c(
  "Especialidad en estudios sociales",
  "PEM Ciencias Sociales y Formación Ciudadana",
  "PEM EN CIENCIAS SOCIALES Y FORMACIÓN CIUDADANA",
  "PEM en Pedagogía y Ciencias Sociales",
  "Profesor de Segunda en Enseñanza con Orientación en Ciencias Sociales",
  "Profesora de Segunda Enseñanza en Ciencias Sociales",
  "Profesora en Enseñanza Media Estudios Sociales y Formación Ciudadana",
  "Técnico en trabajo social"
)

### matematica y fisica
valores_tec_mate_fisica <- c(
  "PEM EN CIENCIAS ESPECIALIZADO EN FÍSICA-MATEMÁTICA",
  "PEM EN FISICA MATEMÁTICA",
  "PEM EN PEDAGOGÍA, FÍSICA Y MATEMÁTICAS",
  "PEM en Matemática y Física",
  "Profesor de enseñanza media con especialidad en la física y matemática",
  "Profesor de matemáticas y física",
  "Profesora de Enseñanza Media En Pedagogía con la Especialización en Matemáticas",
  "Profesora de Enseñanza Media en Física Matemática",
  "Profesora de Enseñanza Media en Pedagogía Física y Matemática",
  "Profesora de Enseñanza Media en Pedagogía Física y Matemática.",
  "Profesora de Enseñanza Media en Pedagogía, Física y Matemática",
  "Profesorado en Educación de Matemática y Física",
  "Profesorado en Educación de la Matemática y la Física",
  "Profesorado en Matemáticas y Física",
  "Técnico Universitario en la enseñanza de física"
)

### cyl e idiomas
valores_tec_lenguaje_idiomas <- c(
  "Ciencias de la Comunicación",
  "Cominicación y Lemguaje",
  "Lengua y literatura",
  "Lenguas Mayas",
  "Licenciatura en Idioma Español",
  "PEM En Comunicación y Lenguaje",
  "PEM Lengua y Literatura",
  "PEM en Idioma Maya",
  "Pem en Comunicación y Lenguaje",
  "Profesor de enseñanza media en idioma inglés",
  "Profesorado en Educación Media con Especialidad en Comunicación y Lenguaje",
  "Profesorado en Enseñanza Media en Inglés"
)

### naturales y orientacion ambiental
valores_tec_naturales_ambiental <- c(
  "Especialización en ciencias naturales",
  "PEM EN PEDAGOGÍA Y CIENCIAS NATURALES CON ORIENTACIÓN AMBIENT",
  "PEM en Matemáticas y Ciencias Naturales",
  "PEM en ciencias de la educación, con especialidad en ciencias naturales, pensum cerrado Ingeniería en sistemas",
  "PEM. En ciencias de la educación, con especialidad en Ciencias Naturales",
  "Pem en pedagogía y Ciencias Naturales con Orientación Ambiental",
  "PEM en pedagogía con orientación ambiental",
  "PEM en Pedagogía y administración educativa con orientación en medio ambiente",
  "PEM. en Pedagogía con Orientación en Medio Ambiente",
  "PEM.. en Pedagogía con orientación en",
  "Profesor de Enseñanza Media en Pedagogía con Orientación en Medio Ambiente",
  "Profesorado de Enseñanza Media en Ciencias Naturales con Orientación Ambiental",
  "Profesorado en Enseñanza Media en Pedagogía con Orientación en Medio Ambiente",
  "TÉCNICO EN PRODUCCIÓN PECUARIA",
  "PRODUCCIÓN PECUARIA",
  "Técnico Universitario en Producción Agrícola",
  "Técnico en Producción Agricola",
  "Técnico en producción fruticola"
)

### tecno  y computacion
valores_tec_tecno_compu <- c(
  "Ingenieria en sistemas",
  "PEM Con especialidad en Tecnología Educativa",
  "PEM en Computación e Informática",
  "Tecnico Universitario en Tecnólogia y Administracion",
  "Operador y programador de computadoras",
  "Profesorado de Enseñanza Media en Tecnología Educativa",
  "TÉCNICO EN ELECTRÓNICA INDUSTRIAL",
  "Técnico en informática y las telecomunicaciones",
  "Tecnico Universitario en Tecnólogia y Administracion"
)

### artes
valores_tec_artes <- c(
  "Estudiante de Licenciatura de Diseño Gráfico",
  "Licenciatura en Diseño Gráfico",
  "Historia del arte",
  "PEM Artes Pláticas e História del Arte",
  "Profesor de Enseñanza Media Especializado en Música",
  "Publicista Profesional"
)

### administracion de instituciones deportivas
valores_tec_admin_deportiva <- c(
  "Educación Física",
  "Profesora en enseñanza media en educación física",
  "Técnico Universitario en Administración de Instituciones Deportivas",
  "Técnico Universitario en Administración en Instituciones Deportivas"
)

### psicologia
valores_tec_psicologia <- c(
  "PSE en Pedagogía y Psicología",
  "Técnico Universitario en Psicología"
)

### invest e innovacion
valores_tec_inves_inn <- c(
  "Investigación educativa",
  "Investigación educativa",
  "Técnico en Innovación Educativa"
)

### intercultural
valores_tec_intercultural <- c(
  "PEM EN PEDAGOGIA E INTERCULTURAL",
  "PROFESOR DE ENSEÑANZA MADIA Y TÉCNICO EN EDUCACIÓN INTERCULTURAL",
  "Profesora de educación primaria intercultural"
)

### pedagogia
valores_tec_pedagogia <- c(
  "En Pedagogia",
  "En Pedagogia y Ciencias de la Educación",
  "En ciencias de la educación",
  "Necesidades Educativas especiales",
  "PEM","Profesorado en Enseñanza media en ciencias de la educación con especialidad en pedagogia",
  "PEM EN PEDAGOGIA  Y TÉCNICO EN ADMINISTACIÓN EDUCATIVA",
  "PEM EN PEDAGOGIA Y TECNICO EN ADMINISTRACION EDUCATIVA",
  "PEM en Pedagogía",
  "PEM en Pedagogía y Administración Educativa",
  "PEM en Pedagogía y Ciencias Sociales",
  "PEM en pedagogía en enseñanza media y técnico en administración educativa",
  "PEM en pedagogía y administración educativa",
  "PEM y Técnico en Administración Educativa",
  "PEM. EN PEDAGOGÍA Y TÉCNICO EN ADMINISTRACIÓN EDUCATIVA",
  "PEM. En pedagogia",
  "PEM. Y técnico en administración educativa",
  "PROFESOR DE ENSEÑANZA MEDIA",
  "Profesor de Enseñanza Media y Técnico en Administración Educativa",
  "Profesor de Segunda Enseñanza",
  "Profesor se Enseñanza Media",
  "Profesora de Educación Media y Técnico en Pedagogía y Administración Educativa",
  "Profesora de Enseñanza Media",
  "Profesora de Enseñanza Media en Pedagogía y Administración Educativa",
  "Profesora de Enseñanza Media en Pedagogía y Técnica en Administación Educativa.",
  "Profesora de Enseñanza Media en Pedagogía y Técnica en Administración Educativa",
  "Profesora de Enseñanza Media en Pedagogía y Técnico en Administración Educativa",
  "Profesora de Enseñanza Media y Técnica en Administración Educativa",
  "Profesora de Enseñanza Media y Técnico en Administración Educativa",
  "Profesora de enseñanza Media",
  "Profesora de enseñanza media y administracion educativa",
  "Profesora de enseñanza media y técnico en administración educativa",
  "Profesora en Enseñanza Media y Técnico en Administración Educativa",
  "Profesorado de Enseñanza Media en Pedagogía",
  "Profesorado de Enseñanza Media en Pedagogía Y Técnico en administración Educativa",
  "Profesorado de Enseñanza Media en Pedagogía y Técnica en Administración Educativa",
  "Profesorado de Enseñanza Media en Pedagogía y Técnico en Administración Educativa",
  "Profesorado de Enseñanza Media en Pedagogía y Técnico en Administración Educativa.",
  "Profesorado de enseñanza media",
  "Profesorado de enseñanza media en pedagogía y ciencias de la educación",
  "Profesorado de segunda enseñanza",
  "Profesorado de segunda enseñanza con administración de centros educativos",
  "Profesorado en ciencias de la Educación",
  "Profesorado en enseñanza media con énfasis en pedagogía",
  "Profesorado en pedagogía y tecnico en Administración educativa",
  "Profesorado en segunda enseñanza",
  "Técnico en educación media",
  "Técnico Admon. Educativa y profesorado en Pedagogía"
)

### togorafía y agrimensura
valores_tec_topo_arqui <- c(
  "Ingeniería, Topografía",
  "Tecnico en arquitectura",
  "Técnico Universitario en Agrimensura",
  "Técnico en Administación de Tierra",
  "Técnico Universitario en Agrimensura",
  "Técnico en Administración de Tierras"
)

### adminis de tierra
valores_tec_tierra <- c(
  "Técnico en Administación de Tierra",
  "Técnico en Administración de Tierras"
)

### ocupacional
valores_tec_ocupacional <- c(
  "Terapista ocupacional"
)

### teologia
valores_tec_teologia <- c(
  "Teología"
)

### educacion especial
valores_tec_educacion_especial <- c(
  "Necesidades Educativas especiales",
  "Profesorado en Educación Especial"
)


### produccion agropecuaria
valores_tec_produ_agro <- c(
  "PRODUCCIÓN PECUARIA",
  "TÉCNICO EN PRODUCCIÓN PECUARIA",
  "Técnico Universitario en Producción Agrícola",
  "Técnico en Producción Agricola",
  "Técnico en producción fruticola"
)

### automatizacion de oficinas
valores_auto_oficina <- c(
  "Automatización de Oficinas"
)

### sistemas
valores_tec_sistema <- c(
  "Ingenieria en sistemas"
) 

### Publicista
valores_tec_publi <- c(
  "Publicista Profesional"
)

### orientacion vocacional
valores_orien_voca <- c(
  "Técnico Universitario en Orientación Vocacional y Laboral"
)

### educacion media
valores_tec_edu_media <- c(
  "Técnico en educación media"
)
### no especifica
valores_tec_noespecifica <- c(
  "Profesorado de Enseñanza Mediaia ia",
  "Profesorado de enseñanza media",
  "Profesorado de segunda enseñanza",
  "PSE"
)

## Recode con case match ----

df_dyd_rost_titulos <- df_dyd_rost_titulos |>
  mutate(
    reportado_tecnico_universitario = case_match(
      reportado_tecnico_universitario,
      all_of(valores_tec_noespecifica) ~ "No especifica",
      all_of(valores_tec_edu_media) ~ "Técnico Universitario en Educación Media",
      all_of(valores_orien_voca) ~ "Técnico Universitario en Orientación Vocacional",
      all_of(valores_tec_publi) ~ "Técnico Universitario en Publicidad",
      all_of(valores_tec_sistema) ~ "Técnico Universitario en Sistemas",
      all_of(valores_auto_oficina) ~ "Técnico Universitario en Automatización de oficinas",
      all_of(valores_tec_produ_agro) ~ "Técnico Universitario en Producción Agropecuaria",
      all_of(valores_tec_educacion_especial) ~ "Técnico Universitario en Educación Especial",
      all_of(valores_tec_teologia) ~ "Técnico Universitario en Teología",
      all_of(valores_tec_ocupacional) ~ "Ocupacional",
      all_of(valores_tec_tierra) ~ "Técnico Universitario en Administración de Tierras",
      all_of(valores_tec_topo_arqui) ~ "Topogrofía y Agrimensura",
      all_of(valores_tec_pedagogia) ~ "Técnico Universitario en Pedagogía",
      all_of(valores_tec_intercultural) ~ "Técnico Universitario en Educación Intercultural",
      all_of(valores_tec_inves_inn) ~ "Investigación e innovación educativa",
      all_of(valores_tec_psicologia) ~ "Psicología",
      all_of(valores_tec_admin_deportiva) ~ "Técnico Universitario en Administración de Instituciones Deportivas",
      all_of(valores_tec_artes) ~ "Técnico Universitario en Artes",
      all_of(valores_tec_tecno_compu) ~ "Técnico Universitario en Tecnología y Computación",
      all_of(valores_tec_naturales_ambiental) ~ "Técnico Universitario en Ciencias Naturales y Orientación Ambiental",
      all_of(valores_tec_lenguaje_idiomas) ~ "Técnico Universitario en Lenguaje e idiomas",
      all_of(valores_tec_mate_fisica) ~ "Técnico Universitario en enseñanza de Matemática y Física",
      all_of(valores_tec_sociales) ~ "Técnico Universitario en Ciencias Sociales",
      all_of(valores_tec_economico) ~ "Técnico en Administración de Empresas",
      all_of(valores_tec_admin_edu) ~ "Técnico en Administración Educativa",
      .default = reportado_tecnico_universitario
    )
  )

## volver a ver errores

valores_reportado_tecnico_universitario <- df_dyd_rost_titulos |>
  filter(reportado_grado_academico == "Técnico Universitario")|>
  filter(!(is.na(reportado_tecnico_universitario))) 

#View(valores_reportado_tecnico_universitario)


# reportado_licenciatura ----

## ver errores

valores_reportado_licenciatura <- df_dyd_rost_titulos |>
  filter(reportado_grado_academico == "Licenciatura")|>
  filter(!(is.na(reportado_licenciatura))) |>
  select(reportado_licenciatura) 

#dput(valores_reportado_licenciatura)

## Define los vectores ----

### lic investigacion educativa
valores_lic_inve_edu <- c(
  "Licenciatura en Investigación Educativa"
)

### lic admin. educativa

valores_lic_admin_edu <- c(
  "Administracion Educativa","Licenciado en Administración Educativa con Especialidad en Gerencia de Calidad.",
  "Administracion Educativa", "Licenciatura en Educación con Especialización en Edministracion Educativa",
  "Administración Educativa","Licenciatura en Admininistracion Educativa",
  "Licenciatura en administracion educativa con especialidad en gerencia de calidad",
  "Licenciatura en administración educativa.",
  "Licenciatura en Educación con especialización en Administración Educativa",
  "Administración Educativa con Especialidad en Gerencia de La Calidad",
  "Administración Educativa con Gerencia de Calidad",
  "Administración Educativa con especialidad en Gerencia de Calidad",
  "Administración Educativa con especialidad en gerencia de calidad",
  "Administración educativa","Licenciatura en Admininistracion Educativa",
  "Licenciatura en Administraciòn Educativa",
  "Licenciatura en Administración Educativa con Orientación Educativa",
  "Licenciatura en Administración Educativa con especialización en Gerencia de Calidad",
  "Licenciatura en Administración educativa con orientación en medio ambiente",
  "Licenciatura en Administración y Liderazgo Educativo",
  "Licenciatura en Admon. Educativa",
  "Licenciatura en Admón. Educativa",
  "Administración educativa con especialidad en gerencia de calidad",
  "Administradora Educativa",
  "Admón Educativa","Licda. Admón. Educativa",
  "Licda. En admin. Educativa",
  "Admón. Educativa","Licenciatura en administración educativa con especialidad en gerencia de calidad", # si consideras que es más contable también se puede incluir
  "Licenciado en Administración Educativa",  # algunas menciones mezcladas con educativo
  "Licenciado en administración Educativa",
  "Licenciado en administración educativa",
  "Licenciado en Admon. Educativa",
  "Licenciatura en Admininistracion Educativa",
  "Licenciatura en Administracion Educativa",
  "Licenciatura en Administración Educativa",
  "Licenciatura en administración educativa",
  "Licenciatura en administración educativa con Especialidad en Gerencia de calidad",
  "Licenciatura en administración educativa con especialización en Gerencia de Calidad",
  "cierre de lic en administración educativa",
  "Cierre de pensum Administración Educativa",
  "Educación en Administración Educativa",
  "Educador con Especialización en Administración Educativa",
  "En Administracion Educativa", "Licenciada en educacion administrativa",
  "En Administracion educativa y pedagogia",
  "En Administración De Educación",
  "En Administración de Educación","En Educación con Especialidad en Administración Educativa",
  "En Educación con especialización en Administración Educativa",
  "En Administración y Liderazgo Educativo",
  "Administración Educativa","Licencenciatura en Administración Educativa",
  "Licenciada en Administración Educativa con especialización en Gerencia de Calidad",
  "Licenciada en Ciencias de la Educación con Énfasis en Administración Educativa",
  "Administración Educativa con Especialidad en Gerencia de La Calidad",
  "Administración Educativa con Gerencia de Calidad",
  "Administración Educativa con especialidad en Gerencia de Calidad",
  "Administración educativa con especialidad en gerencia de calidad",
  "Educación en Administración Educativa",
  "Educación con Especialidad en Administración Educativa",
  "Educación con especialización en Administración Educativa",
  "Lic. en Administración Educativa",
  "Licenciatura en Administración Educativa",
  "Licenciatura en Administración Educativa con Especialidad en Gerencia de Calidad",
  "Licenciada en Administración Educativa",
  "Licenciada en Administración Educativa Con Orientación en Medio Ambiente",
  "Licenciada en Educación con Especialidad en Administración Educativa",
  "Licenciada en Educación con especialización en Administración Educativa",
  "En Administración Educativa",
  "Lic en administración educativa con especialidad en gerencia de calidad",
  "Lic. En Admón Educativa","Licenciada en administracion educativa",
  "Licenciada en administración Educativa",
  "Licenciada en administración Educativa con Especialidad en Gerencia de Calidad",
  "En Pedagogía y Administración Educativa",
  "En Pedagogía en Administración Educativa",
  "En Pedagogía y Admón. Educativa",
  "Licenciatura en Pedagogía y Administración Educativa",
  "Licenciatura en Pedagogía y administración educativa",
  "Pedagogía y Administración Educativa",
  "Pedagogía con Orientación en Administración y Evaluación Educativa",
  "Pedagogo con Orientación en Administración y Evaluación Educativas",
  "Pedagoga con Especialidad en Administración y Evaluación Educativa"
)


### Administración de Empresas 
valores_lic_admin_empresas <- c(
  "Adm. De Empresas",
  "Administracion de Empresas",
  "Administracion de empresas",
  "Administración de Empresas",
  "Adm. De Empresas",
  "Administracion de Empresas",
  "Administracion de empresas",
  "Administración de Empresas",
  "Administración de Enpresas",
  "Administración de negocios",
  "Ciencias de la Administración",
  "Lic en Adm de Empresas",
  "Licenciatura en Administración de Empresas",
  "Técnico en Administración de Empresas",
  "Técnico en administración de empresas",
  "Lic. Mercadotecnia y Publicidad",
  "Licenciatura en ciencias de la administración.",
  "Tecnología y negocios",
  "Administración de Enpresas",  # errores de escritura incluidos
  "Administración de negocios",
  "Administración en Informatica y Recursos Humanos",
  "Ciencias de la Administración",
  "Lic en Adm de Empresas",
  "Licenciado de la enseñanza de ciencias económico contables",
  "Licenciatura en Administración de Empresas",
  "Licenciatura en Ciencias de la administración."
)

### pedadogia y administraccion educativa

valores_lic_peda_admin <- c(
  "En Pedagagía y Administración Educativa",
  "Licenciatura en pedagogía con Orientación en Administración Educativa",
  "Pedagogia y administración educativa",
  "Pedagogía Y Administración Educativa",
  "Pedagogía y Administración  Educativa",
  "Pedagógia y Administración Educativa", 
  "en Pedagogía y Administración Educativa",
  "licenciatura en Pedagogía y Admón. Educativa",
  "licenciatura en educación con especialidad en administración educativa",
  "En Pedagogia y Administración Educativa",
  "En Pedagogía en Administración Educativa",
  "Licenciatura en Pedagogia y Admin Educativa",
  "Licenciatura en Pedagogia y Administracion Educativa",
  "En Pedagogía y Administración Educativa",
  "Licenciatura en Pedagogia y Admin Educativa",
  "Licenciatura en Pedagogia y Administracion Educativa",
  "Licenciatura en Pedagogia y CC. De la Educación",
  "Licenciatura en Pedagogia y administración educativa",
  "Licenciatura en Pedagogía  y Administración Educativa",
  "Licenciatura en Pedagogía con Orientación en Administración y Evaluación Educativa",
  "Licenciatura en Pedagogía en Administración Educativa",
  "Licenciatura en Pedagogía y Administracion Educativa",
  "Licenciatura en Pedagogía y Administración Educativa.",
  "Licenciatura en Pedagogía y Administración educativa",
  "Licenciatura en Pedagogía y Admón. Educativa",
  "En Pedagogía y Tec. En Administración. Educativa",
  "En Pedagogía y administración Educativa",
  "En Pedagogía y administración educativa",
  "En Pedagógía y Administración Educativa",
  "En pedagogia y administración educativa",
  "En pedagogía y Administración Educativa",
  "En pedagogía y Administración. Educativa",
  "Licenciada en pedagogia y admon efucativa",
  "Licenciado  en Pedagogía y Administración Educativa",
  "Licenciado En Pedagogìa y Administraciòn Educativa",
  "Licenciado en Administración Educativa con Especialidad en Gerencia de Calidad",
  "En pedagogía y administración educativa",
  "LIC. en Administración Educativa","Pedagpgía y Admón Educativa",
  "LICENCIADA EN EDUCACIÓN CON ESPECIALIDAD EN ADMINISTRACION EDUCATIVA",
  "LICENCIADA en Pedagogía y Administración Educativa",
  "LICENCIADO EN PEDAGOGÍA Y ADMINISTRACIÓN EDUCATIVA",
  "LICENCIATURA EN PEDAGOGIA Y ADMINISTRACIÓN EDUCATIVA",
  "Lic. En pedagogia y Administración Educativa",
  "Lic. En pedagogía y Administración Educativa",
  "Lic. En Pedagogía y Ad.inistración Educativa",
  "Lic. En Pedagogía y Administración Educativa",
  "Lic. En Pedagogía y Admon Educativa",
  "Lic. En Pedagogía y Admon. Educativa",
  "Lic. En Pedagogía y Admón Educ.",
  "Licda en pedagogía y Administracion Educativa",
  "Licda. En Pedagogía y Administración  Educativa",
  "Licenciada Pedagogía y Administración Educativa",
  "Licenciada en Pedagogía en Administración Educativa",
  "Licenciada en Pedagogía y Administración  Educativa",
  "Licenciada en Pedagogía y Administración Educativa",
  "Licenciada en Pedagogía y Administración Educativa con Orientación en Medio Ambiente",
  "Licenciada en Pedagogía y Administración Educativa.",
  "Licenciada en Pedagogía y Técnico en Administración. Educativa.",
  "Licenciada en pedagogía y Administración Educativa",
  "Licenciada en pedagogía y administración educativa",
  "Licenciada en pedagogía y técnico en administración educativa",
  "Pedagogia y Administración educativa",
  "Pedagogía y Administración Educativa",
  "Pedagogía y Administración educativa",
  "Pedagogía y Admiración Educativa",
  "Pedagogía y Admon. Educativa",
  "Pedagogía y Aministracion Educativa",
  "Pedagogía y Aministracion educativa",
  "Pedagogía con Orientación en Administración y Evaluación Educativa",
  "Pedagogo con Orientación en Administración y Evaluación Educativas",
  "Pedagoga con orientación en Administración y Evaluación Edicativa",
  "Pedagoga con Especialidad en Administración y Evaluación Edicativa",
  "PEDAGOGIA Y ADMINISTRACION EDUCATIVA",
  "PEDAGOGÍA Y ADMINISTRACIÓN EDUCATIVA","Lic. Pedagogia y Admo. Educativa",
  "Licenciada en Educación con Especialidad en Administración  Educativa",
  "Licenciada en Pedagogía y Administración Educación",
  "Licenciada en pedagogía y Administracion Educativa",
  "Licenciado en Administración Educativa con Especialidad en Gerencia de Calidad.",
  "Licenciado en Pedagogía con Énfasis en Administración y Evaluación de Proyectos Educativos.",
  "Licenciado en Pedagogía yAdmnistracion Educativa",
  "Licenciatura en Pedagogia y Especialidad Primaria",
  "Licenciatura en Pedagogia y adminstración educativa",
  "Licenciatura en Pedagogìa y Administraciòn Educativa",
  "Licenciatura en Pedagogía con Énfasis en Administración y Evaluación de Proyectos Educativos",
  "Licenciatura en Pedagogía y Administración Edicativa",
  "Licenciatura en Pedagogía y Administración educativa",
  "Licenciatura en Pedagogía y Admón. Educativa y profesorado con especialidad en Comunicación y lenguaje",
  "Licenciatura en Pedagogía y Ciencias de la Educacion",
  "Licenciatura en Pedagogía y técnico en Administración Educativa",
  "Licenciado en pedagogía y administración educativa",
  "Licenciatura en pedagogia y admon. Educ",
  "Licenciatura en pedagogia y técnico en administración educativa",
  "Licenciatura en pedagogía con Especialidad en Educación Primaria",
  "Licenciatura en pedagogía y Administración Educativa",
  "Licenciatura en pedagogía y administración educativa",
  "Licenciatura en educación con especialidad en administración",
  "Licenciados en Pedagogía y Administración. Educativa",
  "Licenciatura  en pedagogía y administración Educativa",
  "Lic. En pedagogía y administración educativa",
  "Licenciatura en pedagogía y técnico en administración educativa",
  "Licenciatura en pedagogía y técnico en administración educativa.",
  "Licencinciatura en Pedagogía y Administración Educativa",
  "Lienciada en Educación",
  "Linda en Pedagogía  y Administración  Educativa",
  "Linda en pedagogía  y administración Educativa",
  "Linda. Pedagogía y Admon.Educativa",
  "Pedagogía con Enfásis en Administración y Evaluación de Proyectos Educativos",
  "Pedagogía con Orientación en Administración y Evaluación Educativa",
  "Pedagogía con Orientación en Administración y Evaluación Educativas",
  "Pedagogía con orientación en Evaluación y administración educativa",
  "Pedagogía con orientación en administración y evaluación educativa",
  "Pedagogía con orientación en evaluación y administración educativa",
  "Pedagogía  y administración  educativa",
  "Pedagogía admon educativo",
  "Pedagogía con Enfásis en Administración y Evaluación de Proyectos Educativos",
  "PEDAGOGIA", "LICENCADA EN PEDAGOGIA Y ADMINISTRACIÓN EDUCATIVA",
  "LICENCIADA EN EDUCACIÓN CON ESPECIALIDAD EN ADMINISTRACIÓN EDUCATIVA",
  "LICENCIATURA CON ESPECIALIZACIÓN EN ADMINISTRACIÓN EDUCATIVA",
  "LICENCIATURA EN ADMON. EDUC.",
  "LICENCIATURA EN EDUCACIÓN CON ESPECIALIDAD EN ADMINISTRACION EDUCATIVA",
  "LICENCIATURA EN PEDAGOGIA Y ADMINISTRACION EDUCATIVA",
  "LICENCIATURA EN PEDAGOGÍA Y ADMINISTRACIÓN EDUCATIVA",
  "LKCENCIADO EN PEDAGOGÍA Y ADMINISTRACIÓN EDUCATIVA",
  "Lcda. En Educación con Especialidad en Administración Educativa",
  "Pedagoga con Orientación en Administración y Evaluación Educativas",
  "Pedagoga con Especialidad en Administración y Evaluación Edicativa",
  "Pedagoga con orientación en Administración y Evaluación Edicativa",
  "PEDAGOGIA Y ADMINISTRACION EDUCATIVA",
  "Literatura en pedagogía y Administración Educativa",
  "Lic. Pedagogía y Admon. educativa","Licenciada en Pedagogia y Administracion Educativa",
  "Licenciada en Pedagogia y Administracion Educativa",
  "Lic. en Pedagogía y Administración Educativa",
  "Pedagogía y administración educatuva","Lic.Pedagogia Especialidad Primaria",
  "Pedagogía y admón educativa", "Lic. En Pedagogía con Especialidad en Educación Primaria",
  "Profesorado en enseñanza mefia y tecnico en admon efucativa",
  "Licda en Pedagogía y Tecnico en Administración Educativa en Adm",
  "PEDAGOGA CON ORIENTACIÓN EN ADMINISTRACIÓN Y EVALUACIÓN EDUCATIVA",
  "Licenciatura en Pedagogía y Administración Educativa",
  "Licenciatura en Pedagogía y Administración Educativa con Especialidad en Medio Ambiente",
  "Licenciatura en Pedagogía y Administración Educativa con Orientación en Medio Ambiente",
  "Licenciatura en Pedagogía y Administración Educativa y Administración",
  "Licenciatura en Pedagogía y administración Educativa con especialidad en Medio Ambiente",
  "Licenciatura en Pedagogía y administración educativa",
  "Licenciatura en Pedagogía y Técnico en Administración Educativa",
  "Licenciatura en Pedagogía y administración educativa",
  "Licenciatura en Pedagogía y ciencias de la educación",
  "Licenciatura en Pedagpgía y Admón Educativa",
  "Pedagogía y administración educativa","Licenciatura en Educación con Especialidad en Administración Educativa",
  "Licenciatura en Educación con Especialización en Administración Educativa",
  "Licenciatura en Educación con especialidad en Administración Educativa",
  "Licenciatura  en Pedagogía y administración Educativa",
  "Licenciado en pedagogía y Administración Educativa",
  "Licenciado en Pedagogía Y Administración Educativa.",
  "Licenciado en Pedagogía con Orientación en Administración y Evaluación Educativas",
  "Licenciado en Pedagogía con Énfasis en Administración y Evaluación de Proyectos Educativos",
  "Licenciado en Pedagogía y Administración Educativa",
  "Licenciado en Pedagogía y Técnico en Administración Educativa",
  "Profesorado en pedagogía y ciencias de la Educación",
  "EN PEDAGOGIA Y ADMINISTRACION EDUCATIVA",
  "EN PEDAGOGIA Y ADMON EDUCATIVA",
  "Licenciatura En Pedagogia y Administracion Educativa",
  "Licenciatura de pedagogía y administraciión educativa",
  "Profesorado en pedagogía y administrativa educativa con orientación en medio ambiente",
  "Profesorado de Enseñanza Media y administración Educativa",
  "Profesorado en Enseñanza Media y Administración Educativa",
  "En Pedagogía y Admiración Educativa",
  "En administración educativa",
  "Profesor en Enseñanza Media y Tècnico en Administracion Educativa"
)

### pedagogia y ciencias de la educación

valores_lic_peda_ciencia <- c(
  "De educación","Lecenciatura en Pedagogía", "Licenciatura  en pedagogía",
  "Licenciada en Pedagogìa y Ciencias de la Educaciòn",
  "Educacion","En Educación", "Licda. En pedagogía y ciencias de la Educacion",
  "En pedagogía y ciencias de la educación",
  "Formador de formadores", "Lic educación en formador de Formadores",
  "Educacion para contextos Multiculturales",
  "Educacion para contextos multiculturales con Enfasis en la Enseñanza de los Idiomas Mayas",
  "Educación En la Química y la BiologÍa",
  "Licenciado en Ciencias Auxiliares de la Educación con especialidad en Pedagogía",
  "Licenciado en Formador de Formadores","Licenciatura en Ciencias de la Educacion",
  "Licenciatura en Ciencias de la Educación con Orientación en Pedagogía",
  "Licenciatura en Educacion",
  "Licenciatura en Educaciòn",
  "Licenciatura en Pedagogía y Ciencias de la Educacion",
  "Licenciatura en Educación",
  "Licenciatura en Formador de Formadores",
  "Licenciatura en Pedagogia",  "Licenciatura en Pedagogía y Ciencias de la Educacion",
  "Licenciatura en pedagogía", "Pegadogia",
  "Pedagogía y ciencias de la educación",
  "Licenciado en Pedagogía y ciencias de la educación",
  "Licenciado en Formador de Formadores con especialidad en Educación",
  "Educación en la Química la Biología","Licenciada en ciencias de la educación con especialidad en pedagogía",
  "En Pedagogía y Ciencias de la Educación",
  "En Pedagogía y Ciencias de la Educacion",
  "En Pedagogía y ciencias de la Educacion",
  "En ciencias de la educación", "Licenciada en Educación",
  "Lic. En pedagogia y Ciencias de la Educación",
  "En educación", "Ciencias de la educación",
  "En musica","En Pedagogía","Lic. En pedagogia y Ciencias de la Educación",
  "En Pedagogía y Ciencias de la Educación",
  "En pedagogía e investigación educativa",
  "Lic. en pedagogía y Ciencias de la Educación",
  "Licenciatura en Pedagogía","Lienciada en Educación",
  "Licenciatura en Pedagogía y Ciencias de la Educación",
  "Licenciatura en pedagogía y Ciencias de la Educacion",
  "Licenciada en Pedagogía","Licenciatura en pedagogia y Ciencias de la Educacion",
  "Licenciatura en pedagogía y ciencias de la educación",
  "Licenciada en Pedagogía y Ciencias de la Educación",
  "Pedagogía", "Licenciatura en Pedagogía y Ciencias de la Educacion",
  "Pedagogía y Ciencias de la Educación",
  "Pedagogía y Ciencias de la educaccion",
  "Pedagogía y Ciencias de la Educacionca i",
  "Pedagogía y Ciencias de la educa",
  "Pedagogía y Ciencias de la educación",
  "Pedagogía y Ciencias de la educacion",
  "En pedagogía e investigación educativa",
  "En pedagogía y ciencias de la Educacion",
  "Pedagogia","Licenciatura en Pedagogia y CC. De la Educación",
  "Pedagogia y Ciencias de la Educación",  "Licenciatura en Educación y Desarrollo Político Social",
  "Pedagogia y Ciencias de la educaccion",
  "Pedagogia y Ciencias de la educacion",
  "Pedagogía","Licenciatura en Pedagogía y Ciencias de la Educacion",
  "Pedagogía y Ciencias de la Educacion",
  "Pedagogía y Ciencias de la Educación"
)

### Derecho / Ciencias Jurídicas
valores_lic_derecho <- c(
  "Ciencias Juridicas",
  "Ciencias Jurídicas","Licenciatura en Ciecias Juridicas y sociales",
  "Licenciatura en Ciencias Jurídicas y Sociales",
  "LICENCIADO EN PEDAGOGIA Y ADMINISTRACION EDUCATIVA. LICENCIADO EN CIENCIAS JURÍDICAS Y SOCIALES. ABOGADO Y NOTARIO",
  "Ciencias Jurídicas y Sociales",
  "Ciencias Jurídicas y Sociales Abogado y notario",
  "Derecho","Ciencias Jurídicas y Sociales",
  "Ciencias Jurídicas y Sociales Abogado y notario",
  "Licenciatura  en Ciencias jurídicas y Sociales",
  "Licenciado en Ciencias Juridicas y Sociales",
  "Licenciado en Ciencias Jurídicas y Sociales"
)

### lic criminologia y criminalistica
valores_lic_crimi <- c(
  "Criminalistica",
  "Criminalística y Ciencias Forenses"
)

### psicologia
valores_lic_psicologia <- c(
  "En Psicología general",
  "PSICOLOGIA",
  "Psicología",
  "Psicología Industrial",
  "Psicología y consejería social",
  "Psicóloga general",
  "Psicólogo","Psicología",
  "Psicologia","Licda. En Psicología y Consejería Social",
  "Licda. Psicología Clínica",
  "Licenciatura En Psicología",
  "Licenciatura en Psicología",
  "Licenciatura en Psicología.",
  "Licenciatura en Psicología",
  "Licenciada en Psicología",
  "Psicología Educativa","Bachelors of Science in Human Development and Family Science",
  "Licenciatura en Psicología Educativa",
  "Licenciada en Psicología.",
  "Licenciatura en Psicologia",
  "Licenciatura en Psicología",
  "Licenciado en Psicología"
)

### psicopedagogia
valores_lic_psicopedagogia <- c(
  "Psicopedagogía","Psicopedagogía",
  "Licenciatura en Psicopedagogía",
  "Licenciatura en Psicopedagogía"
)

### bilingue intercultural 
valores_lic_bil_inter <- c(
  "En Educacion para contextos multiculturales con enfasis en la enseñanza de los ifiomad",
  "LICENCIATURA EN EDUCACIÓN BILINGUE INTERCULTURAL",
  "Lic. En Educación Bilingüe Intercultural",
  "Licenciada en Educación Primaria Intercultural",
  "Licenciado en Educación Bilingue Intercultural con énfasis en la cultura Maya",
  "Licenciado en educacion bilingue Intercultural con énfasis en la cultura maya",
  "Licenciado en educación bilingue intercultural con énfasis en la cultura maya",
  "Licenciatura en Educación Bilingue Intercultural con enfasis en la cultura maya",
  "Licenciatura en Educación Bilingue Intercultural con Énfasis en la Cultura Maya",
  "Licenciatura en Educación Primaria Intercultural",
  "Licenciatura en Ingles y Tecnologia Educativa",
  "Licenciatura en Letras", "Licenciada en Educación Primaria intercultural",
  "Licenciado en Educación Primaria Intercultural con  Enfasis en Educación Bilingue",
  "Licenciado en Educación Primaria Intercultural con Enfasis en Educación Bilingue",
  "Licenciado en Educación Primaria Intercultural con Énfasis en Educacion Bilingue",
  "Licenciado en Educación Primaria Intercultural con Énfasis en Educación Bilingüe",
  "Licenciado en Educación Primaria con énfasis en intercultural.",
  "Licenciado en educacion primaria","Licenciatura en Educación BilingÜe Intercultural",
  "Licenciado en educación bilingue intercultural con enfasis en la cultura maya",
  "Licenciado en educación bilingue intercultural, con énfasis en la cultura maya",
  "Licenciado en educación primaria intercultural",
  "Licenciafo en Educacion Primiaria con énfsis en Educación Bilingúe Intercultural.",
  "Licenciatura en Educacion para contextos multiculturales",
  "Licenciatura en Educación BilingÜe Intercultural",
  "Licenciatura en la Enseñanza del Idioma Español y la Literatura",
  "Licenciatura en la enseñanza del Idioma Español y la literatura de"
)

### ingenieria agronoma
valores_inge_agro <- c(
  "Ing Agronomo",
  "Ingeniera Agrónoma con énfasis en Gerencia Agrícola",
  "Ingeniero Agronomo",
  "Ingeniero Agrónomo con Orientación en Agricultua Sostenible",
  "Ingeniero Agrónomo con Énfasis en Fruticultura",
  "Ingeniero Agrónomo en Sistemas de Producción Agropecuaria"
)

### ingenieria civil
valores_inge_civil <- c(
  "Ingeniero civil",   "Ingenieria Civil"
)

### medico y cirujano
valores_med_ciru <- c(
  "Médico y Cirujano"
)

### teologia
valores_lic_teologia <- c(
  "Teología"
)

### ingenieria ciencias agricolas
valores_inge_agricola <- c(
  "Ingeniería en Administración de Tierras",
  "Licenciado en ciencias agricolas",
  "Licenciado en Ciencias Agricolas","LICENCIADO EN CIENCIAS AGRICOLAS",
  "Licenciado en Ciencias Agricolas."
)

### licenciatura en mate y fisica
valores_lic_matematica_fisica <- c(
  "LIC. En Matemática y Física",
  "Lic. En matemática y física",
  "Licenciada en la Enseñanza de la Física y Matemática",
  "Licenciado en la Enseñanza de la Física y Matemática",
  "Licenciatura en la enseñanza de la matemática y física",
  "Profesorado en matematica",
  "Licenciada en Administración y Profesora en enseñanza media con especialidad en Matemática",
  "Licenciada en la Enseñanza de la Matemática y Física",
  "Licenciada en la Enseñanza de la Física y Matemática",
  "Licenciatura en Enseñanza de Matemática y Física",
  "Licenciatura en la Enseñanza de la Matemática y Física",
  "Profesor de enseñanza media con especialidad en la física y matemática",
  "Profesor de matemáticas y física",
  "Licenciada en Educación de la Matemática y la Física",
  "Licenciada en la Enseñanza de la Física y Matemática",
  "Licenciado en Matemática y Física",
  "Licenciatura de la Enseñanza de la Matemática y Física",
  "Licenciatura en Educación de la Física y Matemática",
  "Licenciatura en Educación de la Matemática y la Física",
  "Licenciatura en Matemática y Física",
  "Licenciatura en enseñanza de la matemática y la física",
  "Licenciatura en la Enseñanza de Matemática y Física",
  "Licenciatura en la Enseñanza de la Matematica y Física",
  "Licenciatura en la enseñanza de la Matemática y la Física",
  "Profesora de Enseñanza Media En Pedagogía con la Especialización en Matemáticas",
  "Profesora de Enseñanza Media en Física Matemática",
  "Profesora de Enseñanza Media en Pedagogía Física y Matemática",
  "Profesora de Enseñanza Media en Pedagogía, Física y Matemática",
  "Profesorado en Educación de Matemática y Física",
  "Profesorado en Educación de la Matemática y la Física",
  "Profesorado en Matemáticas y Física",
  "Licenciatura en la Enseñanza de la Matemática y Física",
  "Licenciatura en la Enseñanza de la Física y Matemática",
  "Licenciada en la Enseñanza de la Matemática y Física",
  "Licenciada en la Enseñanza de la Física y Matemática",
  "Lic. En Matemática y Física", "Licenciada en la Enseñanza de la Física y Matemática",
  "LIC. En Matemática y Física"
)

### filosofia
valores_lic_filo <- c(
  "Licenciado en Filosofía"
)

### ciencias de la comunicacion
valores_lic_comunicacion <- c(
  "Ciencias de la Comunicación",
  "Ciencias xd la comunicación",
  "Licenciatura en ciencias de la comunicación"
)

### lic de idioma español y literatura
valores_lic_literatura <- c(
  "Enseñame del idioma español y literatura",
  "Lengua y literatura",
  "Licenciatura del Idioma Español y la Literatura",
  "Licenciatura en la Enseñanza de la Lengua y la Literatura",
  "Licenciatura en la Enseñanza del Idioma Español y la Literatura",
  "Licenciatura en la enseñanza del Idioma español y literatura",
  "Licenciatura en lenguaje y comunicación",
  "Licenciatura especializada en lenguaje y comunicación",
  "Licenciada en Docencia en la Comunicación y el Lenguaje"
)


### lic en letrras
valores_lic_letras <- c(
  "Licenciatura en Letras",
  "Letras"
)

### deportes y desarrollo humano
valores_deporte_humano <- c(
  "Deportes y desarrollo humano", 
  "Licenciatura en Deportes y Desarrollo Humano.",
  "LICDA. En Pedagogía y Derechos Humanos",
  "Licenciatura en Deportes y desarrollo humano"
)

### educacion fisica y recreacion
valores_fisica_recreacion <- c(
  "Educación física Deporte y Recreación física",
  "Licenciatura en Educación física, deporte y recreación física",
  "Educación física deporte y recreación física",
  "Licenciatura en educación física deporte y recreación física",
  "Profesora en enseñanza media en educación física"
)

### lic orientacion educativa
valores_lic_orienta_edu <- c(
  "Orientación Educativa",
  "Licenciatura en Orientación Educativa"
)

### arquitectura
valores_lic_arqui <- c(
  "Arquitectura",
  "LICENCIATURA EN ARQUITECTURA"
)

### educacion en la quimica y biologia
valores_lic_quimica_bio <- c(
  "Educación En la Química y la BiologÍa",
  "Educación en la Química la Biología",
  "Licenciatura en biología",
  "Licenciatura en la enseñanza de la Química y la Biología",
  "Licenciatura en la Enseñanza de la Química y Biología",
  "Licenciatura en la enseñanza de la Química y Biología",
  "Licenciatura en la enseñanza de la Química y la Biología",
  "Especialización en ciencias naturales"
)

### lic en educacion ambiental
valores_lic_edu_ambiental <- c(
  "Lic en Educación Ambiental","Licenciatura en Medio Ambiente",
  "Licda en Educación Ambiental",
  "Licenciatura en Educación Ambiental",
  "Licenciada en Educación Ambiental"
)

### lic en ciencias naturales con orientacion ambiental
valores_cn_ambiental <- c(
  "Licenciatura en Ciencias Naturales con Orientación Ambiental",
  "Licenciatura en pedagogía y técnico en administración educativa, Licenciatura en ciencias naturales y educación ambiental",
  "Licenciatura en Ciencias Naturales con Orientación Ambiental",
  "Licenciatura en Ciencias naturales con orientación ambiental",
  "Licenciatura en ciencias naturales con orientación ambiental",
  "Licenciatura en ciencias naturales y educación ambiental"
)

### lic enseñanza historia
valores_lic_ensen_historia <- c(
  "LICENCIATURA PARA LA ENSEÑANZA DE LA HISTORIA"
)

### ingeniera en sistemas
valores_lic_sistemas <- c(
  "Ingeniería en Sistemas y Ciencias de la Información",
  "Ingeniería en sistemas", "Operador y programador de computadoras",
  "Licenciada en Ingeniería en Sistemas de Información y Ciencias de la Computación",
  "Licenciatura en Sistemas de información y la computación",
  "Ingeniería en Sistemas y Ciencias de la Información",
  "Ingeniería en sistemas", "Ingenieria en sistemas"
)

### lic en informatica y admon
valores_lic_info_admon <- c(
  "Licenciatura en informática y admon","Técnico en Informática en",
  "Licenciatura en informática y admon de negocios"
)

### enseñanza de la computacion e informatica
valores_lic_ensen_compu <- c(
  "Licenciatura en la enseñanza de la Computación e Informática",
  "Licenciatura en Educación y Tecnología de la Información y Comunicación",
  "Licenciatura en Informática y Administración",
  "Licenciatura en la enseñanza de la Informática y Computación",
  "Licenciatura en Tecnologia y negocios",
  "Licenciatura en supervision eléctrica y electrónica",
  "Licenciatura en la enseñanza de la informática y computación"
)

### tecnologia y negocios
valores_tecno_negocio <- c(
  "Tecnología y negocios"
)

### lic en diseño grafico
valores_lic_diseno <- c(
  "Estudiante de Licenciatura de Diseño Gráfico",
  "Licenciatura en Diseño Gráfico","Licenciatura en Comunicación y Diseño Gráfico",
  "Comunicación y Diseño Gráfico",  "Licenciatura en Ciencias de la Comunicación y Diseño Gráfico",
  "Licenciatura en Diseño Gráfico con especialidad en Diseño Editorial"
)

## Lic en arte
valores_lic_arte <- c(
  "Historia del arte",
  "Licenciatura en Arte"
)

### lic en musica
valores_lic_musica <- c(
  "Licenciatura en musica","En musica",
  "Licenciatura en educación con especialidad en música",
  "Licenciado en expresión artística con especialidad en música",
  "Profesor de Enseñanza Media Especializado en Música"
)



### no especifica
valores_lic_sinespecificar <- c(
  "Especialidad en estudios sociales","PEM en Estudios sociales",
  "Licenciatura", "Deportes", "Tecnico universitario"
)

### trabajo social
valores_lic_trabajo_social <- c(
  "Lic en trabajo social",
  "Lic. En trabajo sicial",
  "Licenciada en Trabajo Social",
  "Licenciatura en Trabajo Social"
)

### educacion y desarrollo politico social
valores_edu_desa_politico <- c(
  "Licenciado en Educación y Desarrollo Político Social",
  "Licenciatura en Educación y Desarrollo Político Social",
  "Licenciatura en Educación y Desarrollo político Social"
)

### lic en derechos humanos
valores_lic_ddhh <- c(
  "Licenciatura en Derechos Humanos", "Pedagogía y Derechos Humanos",
  "Licenciatura en derechos humanos", "Licenciatura en Pedagogía en DDHH", "Licenciatura en pedagogía en DDHH"
)

### Enseñanza de Ciencias Sociales y Formación Ciudadana
valores_lic_ensen_cs <- c(
  "Licenciatura en Enseñanza de Ciencias Sociales y Formación Ciudadana",
  "Licenciatura en la enseñanza de Ciencias sociales y Formación ciudadana",
  "Licenciatura en enseñanza de las Ciencias Sociales y Formación Ciudadana",
  "Licenciada de Pedagogía en formación ciudadana de estudios sociales",
  "Profesora en Enseñanza Media Estudios Sociales y Formación Ciudadana",
  "Profesor de Segunda en Enseñanza con Orientación en Ciencias Sociales",
  "PEM Ciencias Sociales y Formación Ciudadana"
)

### contaduria publica y auditoria
valores_conta_audi <- c(
  "Contaduría Publica y Auditoria",
  "Auditor Técnico",
  "Contaduría pública y auditoría"
)

### enseñanza de ciencias económico contables
valores_ensen_economico <- c(
  "Licenciado de la enseñanza de ciencias económico contables",
  "Licenciatura en la Enseñanza de las Ciencias Económico Contables",
  "Licenciatura en la Enseñanza de las Ciencias Económico-Contables",
  "Licenciatura en la enseñanza de las ciencias económico contables"
)

## recode con case match ----
df_dyd_rost_titulos <- df_dyd_rost_titulos |>
  mutate(
    reportado_licenciatura = case_match(
      reportado_licenciatura,
      all_of(valores_ensen_economico) ~ "Licenciatura en la enseñanza de Ciencas económico-contables",
      all_of(valores_conta_audi) ~ "Contaduría pública y Auditoría",
      all_of(valores_lic_ensen_cs) ~ "Licenciatura en la enseñanza de Ciencias sociales y Formación ciudadana",
      all_of(valores_lic_ddhh) ~ "Licenciatura en Derechos Humanos",
      all_of(valores_edu_desa_politico) ~ "Licenciatura en Educación y Desarrollo Político Social",
      all_of(valores_lic_trabajo_social) ~ "Licenciatura en Trabajo Social",
      all_of(valores_lic_sinespecificar) ~ "No especifica",
      all_of(valores_lic_musica) ~ "Licenciatura en Música",
      all_of(valores_lic_arte) ~ "Licenciatura en Arte",
      all_of(valores_lic_diseno) ~ "Licenciatura en Diseño Gráfico",
      all_of(valores_tecno_negocio) ~ "Licenciatura en Tecnología y negocios",
      all_of(valores_lic_ensen_compu) ~ "Licenciatura en la enseñanza de la Informática y Computación",
      all_of(valores_lic_info_admon) ~ "Licenciatura en Informática y Administración",
      all_of(valores_lic_sistemas) ~ "Ingenieria en sistemas",
      all_of(valores_cn_ambiental) ~ "Licenciatura en Ciencias Naturales con Orientación Ambiental",
      all_of(valores_lic_edu_ambiental) ~ "Licenciatura en Educación Ambiental",
      all_of(valores_lic_quimica_bio) ~ "Licenciatura en la enseñanza de la Química y la Biología",
      all_of(valores_lic_orienta_edu) ~ "Licenciatura en Orientación Educativa",
      all_of(valores_fisica_recreacion) ~ "Licenciatura en Educación física, deporte y recreación física",
      all_of(valores_deporte_humano) ~ "Licenciatura en Deportes y desarrollo humano",
      all_of(valores_lic_letras) ~ "Licenciatura en Letras",
      all_of(valores_lic_literatura) ~ "Licenciatura en la Enseñanza del Idioma Español y la Literatura",
      all_of(valores_lic_comunicacion) ~ "Licenciatura en Ciencias de la Comunicación",
      all_of(valores_lic_matematica_fisica) ~ "Licenciada en la Enseñanza de la Física y Matemática",
      all_of(valores_inge_agricola) ~ "Ingenieria Agrícola",
      all_of(valores_inge_civil) ~ "Ingenieria Civil",
      all_of(valores_inge_agro) ~ "Ingenieria en Agronomía",
      all_of(valores_lic_psicopedagogia) ~ "Licenciatura en Psicopedagogía",
      all_of(valores_lic_psicologia) ~ "Licenciatura en Psicología",
      all_of(valores_lic_derecho) ~ "Licenciatura en Ciencias Jurídicas y Sociales",
      all_of(valores_lic_peda_ciencia) ~ "Licenciatura en Pedagogía y Ciencias de la Educacion",
      all_of(valores_lic_peda_admin) ~ "Licenciatura en Pedagogía y Administración educativa",
      all_of(valores_lic_admin_empresas) ~ "Licenciatura en Administración de Empresas",
      all_of(valores_lic_admin_edu) ~ "Licenciatura en Admininistracion Educativa",
      all_of(valores_lic_arqui) ~ "Licenciatura en Arquitectura",
      all_of(valores_lic_teologia) ~ "Licenciatura en Teología",
      all_of(valores_med_ciru) ~ "Médico y Cirujano",
      all_of(valores_lic_ensen_historia) ~ "Licenciatura en la Enseñanza de Historia",
      all_of(valores_lic_inve_edu) ~ "Licenciatura en Investigación Educativa",
      all_of(valores_lic_bil_inter) ~ "Licenciatura en Educación BilingÜe Intercultural",
      all_of(valores_lic_crimi) ~ "Licenciatura en Criminología y Criminalística",
      .default = reportado_licenciatura
    )
  )

## volver a ver errores

valores_reportado_licenciatura <- df_dyd_rost_titulos |>
  filter(reportado_grado_academico == "Licenciatura")|>
  filter(!(is.na(reportado_licenciatura))) |>
  select(reportado_licenciatura) 

#View(valores_reportado_licenciatura)

# reportado_posgrado ----

## ver errores

valores_reportado_posgrado <- df_dyd_rost_titulos |>
  filter(reportado_grado_academico == "Posgrado antes de una maestría")|>
  filter(!(is.na(reportado_posgrado))) |>
  select(reportado_posgrado) |>
  tabyl(reportado_posgrado)

dput(valores_reportado_posgrado)

# reportado_maestria ----

## ver erroes

valores_reportado_maestria <- df_dyd_rost_titulos |>
  filter(reportado_grado_academico == "Maestría")|>
  filter(!(is.na(reportado_maestria))) |>
  select(reportado_maestria) 

#dput(valores_reportado_maestria)
## define los vectores ----

### docencia universitaria
valores_maes_docencia <- c(
  "Dicencia universitaria",
  "Docencia Superior",
  "Docencia Universitaria",
  "Docencia Universitaria con Énfasis en Andragogia",
  "Docencia Universitaria con énfasis en Andragogia",
  "Docencia universitaria y especialidad en aprendizaje",
  "En Docencia Universitaria",
  "En Docencia Universitaria con enfasis en Estrategías de Aprendizaje",
  "En docencia universitaria",
  "MAESTRIA EN DOCENCIA UNIVERSITARIA CON ENFASIS EN ANDRAGOGIA",
  "MAESTRO  EN ARTES  EN DOCENCIA  UNIVERITARIA  CON ÉNFASIS EN  ANDRAGOGÍA",
  "MAESTRO EN ARTES  DE DOCENCIA UNIVERSITARIA CON ÉNFASIS EN ANDRAGOGÍA",
  "MAESTRO EN ARTES EN DOCENCIA UNIVERSITARIA CON ÉNFASIS  EN ANDRAGOGÍAGÍA",
  "Maestro en Docencia Universitaria con Orientación en Estrategias de Aprendzaje",
  "Maestro en docencia universitaria con orientación en estrategias de aprendizaje",
  "Maestrìa en Docencia Universitaria con ènfasis en Andragogìa",
  "Maestría en Docencia Superior",
  "Maestría en Docencia Superior, Innovación y Tecnología Educativa",
  "Maestría en Docencia Universitaria",
  "Maestría en Docencia Universitaria Con Énfasis en Andragogía",
  "Maestría en Docencia Universitaria con Énfasis en Andragogia",
  "Maestría en Docencia Universitaria con Énfasis en Andragogía",
  "Maestría en Educación con especialidad en Docencia Superior",
  "Maestría en Educación con especialidad en Docencia Universitaria",
  "Maestría en docencia universitaria",
  "Maestría en innovación de la docencia universitaria",
  "Magister Artium con especialidad en  docencia superior",
  "Magister Artium con especialidad en docencia superior",
  "Magister en Andragogìa y Docencias Superior",
  "Maguster en docencia universitaria",
  "Maestría en Formación Universitaria",
  "Magíster en Docencia Universitaria",
  "Planificación, Desarrollo y Evaluación Docente",
  "Proyecto Educativo","Maestría en Innovación y formación Universitaria",
  "Mastría en Docencia Universitaria"
)

### educacion
valores_maes_educacion <- c(
  "Maestría en Educacion",
  "Maestría en Educación",
  "Maestría en Educación Superior",
  "Maestría en Educación con Orientación Ambiental",
  "Maestría en Educación y Ambientalización Curricular",
  "Magister Artium en Educación Superior"
)

### ciecias de la educación
valores_maes_ciencia_edu <- c(
  "Maestría en Ciencias de la Educación"
)

### curriculum
valores_maes_curriculum <- c(
  "Maestría en Currículum"
)

### investigacion
valores_maes_inves <- c(
  "Maestría en Investigación"
)

### politicas educativas
valores_maes_poli_edu <- c(
  "Maestría en Políticas Educativas"
)

### gerencia educativa
valores_maes_gerencia <- c(
  "Gerencia educativa",
  "MAESTRIA EN GERENCIA EDUCATIVA",
  "Maestría en Gerencia Educativa",
  "Maestría en Planeamiento y Gerencia Educativa",
  "Magister Artium en Planeamiento y Gerencia Educativa"
)

### admin educativa
valores_maes_admin_edu <- c(
  "Maestría en Educación Superior con Especialización en Administración Educativa",
  "Maestría en Administración Educativa con orientación en currículo",
  "Elaboración y Evaluación de Proyectos Educativos",
  "Maestria en Educación Superior con Especialización en Administración Educativa",
  "Magíster en Dirección y Gestión de Instituciones Educativas"
)

### administración con especializacion finanzas
valores_maes_admin_finanzas <- c(
  "Administración con Especialiación en Finanzas",
  "Administración con Especialización en Finanzas",
  "Maestria en Adminsitración con Especialización en Finanzas"
)


### admin negocios
valores_maes_admin_negocio <- c(
  "Administración de Negocios con mención en marketing digital","Dirección del Marketing",
  "Maestría en Administración de Negocios con mención en marketing digital"
)

### admini rrrhh
valores_maes_admin_rrhh <- c(
  "Administración de Recursos Humanos",
  "Maestría en Gerencia de Recursos Humanos"
)

### admin empresas
valores_maes_empresas <- c(
  "Maestria en Administracion de Empresas"
)

### educacion ambiental
valores_maes_edu_ambiental <- c(
  "Educación ambiental",
  "Maestría en Educación con orientación en Educación Ambiental",
  "Maestría en educación con orientación en medio ambiente",
  "Maestría en educación con orientación en Educación Ambiental"
)

### innovacion educativa
valores_maes_inno_edu <- c(
  "INNOVACIÓN EDUCATIVA",
  "MAESTRÍA EN INNOVACIÓN EDUCATIVA",
  "Maestría en Innovación y formación Universitaria",
  "Master en innovaciones Educativas",
  "Maestría en Tecnología Educativa con Énfasis en Entornos Virtuales",
  "Mastria en Innovación Educativa", "Maestría en Innovación y formación Universitaria"
)

### psicopedagogia
valores_maes_psicopeda <- c(
  "Maestría en Psicopedagogía",
  "Magister en psicopedagia"
)

### asesoria familiar
valores_maes_aseo_fami <- c(
  "Asesoramiento Educativo Familiar",
  "Magister en Educación con especialidad en asesoría familiar"
)

### psicologia forense
valores_maes_psico_forense <- c(
  "PSICOLOGIA FORENSE",
  "Psicologia Forense"
)

### comunicacion educativa
valores_maes_comu_edu <- c(
  "Master en Comunicación Educativa"
)

### seguridad informativa
valores_maes_segu_info <- c(
  "Ingenieria en Seguridad Informatica"
)

### ciencias del suelo
valores_maes_cien_suelo <- c(
  "Magíster en Ciencias del Suelo"
)

### no especifica
valores_maes_noespecifica <- c(
  "Biblia", "2",
  "d"
)

## Recode con case match ----

df_dyd_rost_titulos <- df_dyd_rost_titulos |>
  mutate(
    reportado_maestria = case_match(
      reportado_maestria,
      all_of(valores_maes_noespecifica) ~ "No especifica",
      all_of(valores_maes_cien_suelo) ~ "Maestría en Ciencias del Suelo",
      all_of(valores_maes_segu_info) ~ "Maestría en Seguridad informática",
      all_of(valores_maes_comu_edu) ~ "Maestría en Comunicación Educativa",
      all_of(valores_maes_psico_forense) ~ "Psicología forense",
      all_of(valores_maes_aseo_fami) ~ "Asesoría Familiar",
      all_of(valores_maes_psicopeda) ~ "Maestría en Psicopedagogía",
      all_of(valores_maes_inno_edu) ~ "Maestría en Innovación Educativa",
      all_of(valores_maes_edu_ambiental) ~ "Maestría en Educación Ambiental",
      all_of(valores_maes_empresas) ~ "Maestría en Administración de Empresas",
      all_of(valores_maes_admin_rrhh) ~ "Maestría en Administración de Recursos Humanos",
      all_of(valores_maes_admin_negocio) ~ "Maestría en Administración de Negocios",
      all_of(valores_maes_admin_finanzas) ~ "Maestría en Administración con Especialización en Finanzas",
      all_of(valores_maes_admin_edu) ~ "Maestría en Administración Educativa",
      all_of(valores_maes_gerencia) ~ "Maestría en Gerencia Educativa",
      all_of(valores_maes_poli_edu) ~ "Maestría en Políticas Educativas",
      all_of(valores_maes_inves) ~ "Maestría en Investigación",
      all_of(valores_maes_curriculum) ~ "Maestría en Currículum",
      all_of(valores_maes_ciencia_edu) ~ "Maestría en Ciencias de la educación",
      all_of(valores_maes_educacion) ~ "Maestría en Educación",
      all_of(valores_maes_docencia) ~ "Maestría en Docencia Universitaria",
      .default = reportado_maestria
    )
  )

## volver a ver errores

valores_reportado_maestria <- df_dyd_rost_titulos |>
  filter(reportado_grado_academico == "Maestría")|>
  filter(!(is.na(reportado_maestria))) |>
  select(reportado_maestria) |>
  tabyl(reportado_maestria)

#View(valores_reportado_maestria)

# reportado_doctorado ----

## ver errores

valores_reportado_doctorado <- df_dyd_rost_titulos |>
  filter(reportado_grado_academico == "Doctorado")|>
  filter(!(is.na(reportado_doctorado))) |>
  select(reportado_doctorado) 

#dput(valores_reportado_doctorado)

## Define los vectores ----

### educacion 
valores_doc_edu <- c(
  "DOCTORADO EN EDUCACIÓN."
)

### investigacion en educacion
valores_doc_inves_edu <- c(
  "Doctorado en Investigación en Educación"
)

### liderazgo organizacional
valores_doc_liderazgo <- c(
  "Liderazgo Organizacional"
)

## Recode con case match ----

df_dyd_rost_titulos <- df_dyd_rost_titulos |>
  mutate(
    reportado_doctorado = case_match(
      reportado_doctorado,
      all_of(valores_doc_liderazgo) ~ "Doctorado en Liderazgo organizacional",
      all_of(valores_doc_inves_edu) ~ "Doctorado en Investigación educativa",
      all_of(valores_doc_edu) ~ "Doctorado en Educación",
      .default = reportado_doctorado
    )
  )

## volver a ver errores

valores_reportado_doctorado <- df_dyd_rost_titulos |>
  filter(reportado_grado_academico == "Doctorado")|>
  filter(!(is.na(reportado_doctorado))) |>
  select(reportado_doctorado) 

#View(valores_reportado_doctorado)