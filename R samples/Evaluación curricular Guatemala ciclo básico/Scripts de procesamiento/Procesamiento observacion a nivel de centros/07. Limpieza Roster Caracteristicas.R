# otra_area_adicional_cnb ----

valores_otra_area_adicional_cnb <- obs01_rost_car_doc |>
  filter(caracteristicas_docente_area == "Otra área") |>
  filter(!(is.na(otra_area_adicional_cnb))) |>
  select(otra_area_adicional_cnb)

# dput(valores_otra_area_adicional_cnb)

## Define los vectores ----

### contabilidad
valores_contabilidad_cnb <- c(
  "CONTABILIDAD","Contabilidad","Contabilidad.",
  "Contabilidad general"
)

### comercio y servicio
valores_comer_serv <- c(
  "Comercio y servicio",
  "Comercio y servicios",
  "Comercio y servicio."
)

### técnicas y mercadeo
valores_tec_merca <- c(
  "Técnicas de mercadeo"
)

### ortografia y caligrafia
valores_cyl_cnb <- c(
  "ortografía y caligrafía"
)

### redaccion y correspondencia
valores_reda_corres <- c(
  "Redacción y correspondencia"
)

### lectura
valores_lectura_cnb <- c(
  "Lectura", "Lectura y valores"
)

### valores 
valores_valores_cnb <- c(
  "Valores",
  "Educación en Valores."
)

### moral cristiana
valores_moral_cris <- c(
  "Moral Cristiana"
)

### orientacion
valores_orientacion_cnb <- c(
  "Orientación"
)

### expresión artística
valores_expresion_cnb <- c(
  "Expresión artística"
)


### manualidades y decoracion
valores_manu_deco <- c(
  "Manualidades y decoración."
)

### robotica
valores_robotica_cnb <- c(
  "Robótica"
)

### taller
valores_taller_cnb <- c(
  "Taller"
)

### artes industriales
valores_arte_indus <- c(
  "Artes Industriales"
)

### electricidad
valores_electricidad_cnb <- c(
  "Electricidad"
)

### estructuras metalicas
valores_estruc_meta <- c(
  "Estructuras metálicas."
)

### corte y confeccion
valores_corte_confecc <-c(
  "Corte y confección",
  "Corte y confección."
)

### hogar
valores_hogar_cnb <- c(
  "Hogar"
)

### belleza
valores_belleza_cnb <- c(
  "Belleza"
)

### carpinteria
valores_carpinteria_cnb <- c(
  "Carpintería"
)

### cocina y reposteria
valores_cocina_repos <- c(
  "Cocina y repostería",
  "Cocina y Reposteria"
)

### area ocupacional de cultivos
valores_area_ocup_cultivo <- c(
  "Area ocupacional de cultivos regionales y pecuaria"
)

## Recode con case match ----

obs01_rost_car_doc <- obs01_rost_car_doc  |>
  mutate(
    otra_area_adicional_cnb = case_match(
      otra_area_adicional_cnb,
      all_of(valores_area_ocup_cultivo) ~ "Area ocupacional de cultivos regionales y pecuaria",
      all_of(valores_cocina_repos) ~ "Cocina y Repostería",
      all_of(valores_carpinteria_cnb) ~ "Carpintería",
      all_of(valores_belleza_cnb) ~ "Belleza",
      all_of(valores_hogar_cnb) ~ "Hogar",
      all_of(valores_corte_confecc) ~ "Corte y Confección",
      all_of(valores_estruc_meta) ~ "Estructuras metálicas",
      all_of(valores_electricidad_cnb) ~ "Electricidad",
      all_of(valores_arte_indus) ~ "Artes Industriales",
      all_of(valores_taller_cnb) ~ "Taller",
      all_of(valores_robotica_cnb) ~ "Robótica",
      all_of(valores_manu_deco) ~ "Manualidades y decoración",
      all_of(valores_expresion_cnb) ~ "Expresión artística",
      all_of(valores_orientacion_cnb) ~ "Orientación",
      all_of(valores_moral_cris) ~ "Moral cristiana",
      all_of(valores_valores_cnb) ~ "Valores",
      all_of(valores_lectura_cnb) ~ "Lectura",
      all_of(valores_reda_corres) ~ "Redacción y Correspondencia",
      all_of(valores_cyl_cnb) ~ "Ortografía y Caligrafía",
      all_of(valores_tec_merca) ~ "Técnicas de mercadeo",
      all_of(valores_comer_serv) ~ "Comercio y Servicio",
      all_of(valores_contabilidad_cnb) ~ "Contabilidad",
      .default = otra_area_adicional_cnb
    )
  )

## volver a ver errores

valores_otra_area_adicional_cnb <- obs01_rost_car_doc |>
  filter(caracteristicas_docente_area == "Otra área") |>
  filter(!(is.na(otra_area_adicional_cnb))) |>
  select(otra_area_adicional_cnb)

#View(valores_otra_area_adicional_cnb)

obs01_rost_car_doc <- obs01_rost_car_doc |> 
  mutate(caracteristicas_docente_area = if_else(
    caracteristicas_docente_area == "Otra área",
    coalesce(otra_area_adicional_cnb, caracteristicas_docente_area),
    caracteristicas_docente_area
  ))



