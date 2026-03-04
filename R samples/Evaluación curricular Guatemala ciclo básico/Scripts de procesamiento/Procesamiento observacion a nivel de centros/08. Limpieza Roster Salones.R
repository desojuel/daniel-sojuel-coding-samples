#salon_adicional ----

## ver errores
valores_salon_adicional <- obs01_rost_salones_ad |>
  filter(!(is.na(salon_adicional))) |>
  select(salon_adicional)

# dput(valores_salon_adicional)

## Define los vectores ----
### area deportiva
valores_deportiva <-c(
  "Gimnasio","El área de la piscina que no está en uso por falta de mantenimiento"
)

### agropecuario
valores_agropecuario <- c(
  "Huerto",
  "Espacio para sembrar",
  "Salón agropecuario"
)

### cocina
valores_cocina <- c(
  "Salón para emprendimiento, diseñado para trabajar cocina, pastelería, panadería.",
  "Cocina",
  "Cocina y que cada aula tiene su propia biblioteca con libros y textos del MINEDUC"
)

### usos múltiples
valores_uso_multiple <- c(
  "Salones", "Aula recurso con butacas", "Es un espacio techado con una parte de patio",
  "Salón adecuado para desarrollar las sub áreas que imparte el establecimiento educativo.",
  "Salón de actividades múltiples y se utiliza cuando necesitan espacio para el desarrollo de actividades relacionadas al CNB por la cantidad de estudiantes."
)

### artistico y cultural
valores_artistico_cultura <- c(
  "Pequeño escenario en el patio del establecimiento",
  "Espacio donde imparten sub áreas de Educación  artística. En el que se observan instrumentos musicales y pinturas finalizadas."
)

### de lectura
valores_lectura <- c(
  "Literatura,  en el área de comunicación y lenguaje idioma español, este espacio es utilizado únicamente para literatura donde los alumnos tienen disponibles libros para lectura y comprensión lectora",
  "Rincón de lectura"
)

### audiovisual
valores_audiovisual <-c(
  "Salón de Audiovisuales."
)

### bodega
valores_bodega <-c(
  "Bodega"
)

## Recode con case match ----

obs01_rost_salones_ad <- obs01_rost_salones_ad |>
  mutate(
    salon_adicional = case_match(
      salon_adicional,
      all_of(valores_bodega) ~ "Bodega",
      all_of(valores_audiovisual) ~ "Audiovisual",
      all_of(valores_lectura) ~ "De lectura",
      all_of(valores_artistico_cultura) ~ "Espacio artístico y cultural",
      all_of(valores_cocina) ~ "Cocina",
      all_of(valores_uso_multiple) ~ "Usos múltiples",
      all_of(valores_agropecuario) ~ "Agropecuario",
      all_of(valores_deportiva) ~ "Deportivo",
      .default = salon_adicional
    )
  )

## volver a ver errores

valores_salon_adicional <- obs01_rost_salones_ad |>
  filter(!(is.na(salon_adicional))) |>
  select(salon_adicional)

#View(valores_salon_adicional)

# area_salon_adicional ----

## ver errores
valores_area_salon_adicional <- obs01_rost_salones_ad |>
  filter(!(is.na(area_salon_adicional))) |>
  select(area_salon_adicional)

# dput(valores_area_salon_adicional)

## Define los vectores ----

### educacion física
valores_edu_fisica2 <- c(
  "Educación física",
  "Educación física para natación",
  "Educación física (natación) ya no se utiliza"
)

### ciencias naturales
valores_cyn2 <- c(
  "Ciencias naturales",
  "Ciencias Naturales y Productividad"
)

###productividad y emprendimiento
valores_emprendimiento2 <- c(
  "Productividad y desarrollo",
  "La utilizan para algunas actividades del curso de emprendimiento.",
  "Emprendimiento",
  "Para elaborar alimentos para los estudiantes",
  "Para cocinar la alimentación diaria de los estudiantes",
  "1.Cocina y reposteria\n2.Belleza\n3.Manualidades\n4.Corte y confección\n5.Electricidad.\n6.Carpinteria\n7.Cultivos básicos \n8.Estructura metálicas"
)

### lectura
valores_lectura2 <-c(
  "Comunicación lenguaje idioma español",
  "Es un espacio creado para que los estudiantes en grupo hagan lecturas o realicen actividades lúdicas",
  "Para lectura y actividades lúdicas"
)

###expresion artistica
valores_artistica <- c(
  "Música, Artes Visuales, Teatro y Danza",
  "Educación Artística", "Principalmente para bailes, actos, presentaciones y sesiones con padres de familia"
)


### ingles
valores_ingles2 <-c(
  "Será utilizada para el área de idiomas extranjero Inglés"
)

### usos multiples
valores_multiple <-c(
  "Cada docente tiene su salón de clases",
  "Se utiliza para proyectar contenidos de las distintas áreas cuando se necesita; lo utilizan todos los grados y lo reservan cuando lo van a utilizar."
)

### bodega
valores_bodega2 <- c(
  "No se utiliza para el desarrollo de áreas si no para el resguardo de instrumentos musicales y otros."
)

## Recode con case match ----

obs01_rost_salones_ad <- obs01_rost_salones_ad |>
  mutate(
    area_salon_adicional = case_match(
      area_salon_adicional,
      all_of(valores_bodega2) ~ "Bodega",
      all_of(valores_multiple) ~ "Usos múltiples",
      all_of(valores_ingles2) ~ "Inglés",
      all_of(valores_artistica) ~ "Expresión Artística",
      all_of(valores_lectura2) ~ "Lectura",
      all_of(valores_emprendimiento2) ~ "Productividad y Emprendimiento",
      all_of(valores_cyn2) ~ "Ciencias Naturales",
      all_of(valores_edu_fisica2) ~ "Educación Física",
      .default = area_salon_adicional
    )
  )

## volver a ver errores

valores_area_salon_adicional <- obs01_rost_salones_ad |>
  filter(!(is.na(area_salon_adicional))) |>
  select(area_salon_adicional) 

#View(valores_area_salon_adicional)

# caractersiticas_salon_adicional ----

## ver errores
valores_caractersiticas_salon_adicional <- obs01_rost_salones_ad |>
  filter(!(is.na(caractersiticas_salon_adicional))) |>
  select(caractersiticas_salon_adicional)

dput(valores_caractersiticas_salon_adicional)

## define los vectores ----

### ventilado, amplio e iluminado
valores_espacio <- c(
  "Techado y espacioso",
  "Amplio, ventilado e iluminado, cuenta con energía eléctrica y se encuentra ubicado en el segundo nivel.",
  "Espacios amplios adecuados para cada especialidad.",
  "El salón cuenta con escenario, puerta principal, piso, energía eléctrica, amplitud."
)
### con equipamiento y mobiliario
valores_equipamiento <-c(
  "Esta equipado", "Es una cocina con utensilios y mobiliario básico",
  "Está en remozamiento, indica la docente de inglés que está siendo remozado para que la clase de inglés sea más práctica, se instalará proyector y pantalla",
  "El área de la cocina es un espacio pequeño detrás de la dirección cuenta con una estufa de gas, una estufa de leña, ollas, trastes, leña",
  "Pizarras, mesas, escritorios", "Está en remozamiento, indica la docente de inglés que está siendo remozado para que la clase de inglés sea más práctica, se instalará proyector y pantalla 1",
  "Cuenta con escritorios; dimensiones, 4.5 por 4.5 ms aproximadamente. Ahí colocan el proyector.",
  "Salón con escritorios,  libros disponibles, cuenta con toma cogientes y energía eléctrica"
)

### infraestructura básica
valores_infraestructurabasica <-c(
  "Es un aula extra, cuenta con ventanas, puerta principal, ventanas y energía eléctrica"
)

### espacio pequeño
valores_pequeno <-c(
  "Es un espacio pequeño detrás de la dirección de un aproximado de 1.5*2 mts"
)

### para sembrar
valores_sembrar <- c(
  "Lo utilizan para sembrar",
  "Es un espacio al aire libre que lo usan como huerto"
)

### con decoracion
valores_decoracion <- c(
  "Pequeño escenario de Block y cemento en el patio del establecimiento",
  "Esta ubicado en un corredor al final de las aulas totalmente decorado, también hay otro a la par del área de educación física"
  
)

### resguarda instrumentos
valores_instrumentos <- c(
  "No se utiliza para el desarrollo de áreas si no para el resguardo de instrumentos musicales y otros."
)

### malas condiciones
valores_condicion <- c(
  "Es un espacio en el que se cuenta con la piscina, dos sanitarios y una ducha. No está en uso porque adicional que el monte está muy crecido por falta de chapeo, han encontrado diversidad de serpientes y el piso de la piscina está con rajaduras"
)

## Recode con case match ----

obs01_rost_salones_ad <- obs01_rost_salones_ad |>
  mutate(
    caractersiticas_salon_adicional = case_match(
      caractersiticas_salon_adicional,
      all_of(valores_instrumentos) ~ "Resguarda instrumentos",
      all_of(valores_decoracion) ~ "Decorado",
      all_of(valores_sembrar) ~ "Utilizado para sembrar",
      all_of(valores_pequeno) ~ "Espacio pequeño",
      all_of(valores_infraestructurabasica) ~ "Infraestructura básica",
      all_of(valores_equipamiento) ~ "Con equipamiento y mobiliario",
      all_of(valores_espacio) ~ "Amplio, ventilado e iluminado",
      all_of(valores_condicion) ~ "Malas condiciones",
      .default = caractersiticas_salon_adicional
    )
  )

## volver a ver errores
valores_caractersiticas_salon_adicional <- obs01_rost_salones_ad |>
  filter(!(is.na(caractersiticas_salon_adicional))) |>
  select(caractersiticas_salon_adicional) 

#View(valores_caractersiticas_salon_adicional)
