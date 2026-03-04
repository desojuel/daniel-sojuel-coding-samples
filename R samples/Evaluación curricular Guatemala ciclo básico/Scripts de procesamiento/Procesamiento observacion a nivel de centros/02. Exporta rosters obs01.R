# Roster áreas adicionales ----

obs01_rost_areas_ad <- read_xlsx(here("Datos","Observación a nivel de centro educativo", "observacion centro 18.11.25.xlsx"),sheet = 2)

cols_obs01_rost1 <- read_xlsx(
  here("datasets para procesamiento", "colnames evaluacion curricular basico.xlsx"),
  col_names = T
)[[7]] |> 
  na.omit() |> 
  as.vector()

colnames(obs01_rost_areas_ad) <- cols_obs01_rost1

obs01_rost_areas_ad$periodos_otra_area[obs01_rost_areas_ad$periodos_otra_area == "Otro"] <- 99

obs01_rost_areas_ad$periodos_otra_area <- as.numeric(obs01_rost_areas_ad$periodos_otra_area)

obs01_rost_areas_ad <- obs01_rost_areas_ad |> 
  mutate(
    periodos_otra_area = case_when(
      periodos_otra_area == 99 ~ otro_periodos_otra_area,
      .default = periodos_otra_area
    )
  ) |> 
  select(-otro_periodos_otra_area)


obs01_rost_areas_ad$debug <- "rost_areas_ad"
  
  
# Roster características docentes ----

obs01_rost_car_doc <- read_xlsx(here("Datos","Observación a nivel de centro educativo", "observacion centro 18.11.25.xlsx"),sheet = 3)

cols_obs01_rost2 <- read_xlsx(
  here("datasets para procesamiento", "colnames evaluacion curricular basico.xlsx"),
  col_names = T
)[[8]] |> 
  na.omit() |> 
  as.vector()

colnames(obs01_rost_car_doc) <- cols_obs01_rost2

# Roster salones adicionales

obs01_rost_salones_ad <- read_xlsx(here("Datos","Observación a nivel de centro educativo", "observacion centro 18.11.25.xlsx"),sheet = 5)

cols_obs01_rost3 <- read_xlsx(
  here("datasets para procesamiento", "colnames evaluacion curricular basico.xlsx"),
  col_names = T
)[[9]] |> 
  na.omit() |> 
  as.vector()

colnames(obs01_rost_salones_ad) <- cols_obs01_rost3
