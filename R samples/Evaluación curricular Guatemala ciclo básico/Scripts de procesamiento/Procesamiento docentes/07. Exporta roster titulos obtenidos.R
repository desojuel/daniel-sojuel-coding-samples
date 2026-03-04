# Roster títulos obtenidos

df_dyd_rost_titulos <- read_xlsx(here("Datos","Encuesta docente","encuesta docente 20.11.25 con versiones.xlsx"), sheet = 2)

df_dyd <- read_xlsx(here("Datos/Listos para análisis/Encuesta docente/df_dyd.xlsx"))

df_dyd_rost1 <- read_xlsx(
  here("datasets para procesamiento", "colnames evaluacion curricular basico.xlsx"),
  col_names = T
)[[10]] |> 
  na.omit() |> 
  as.vector()

colnames(df_dyd_rost_titulos) <- df_dyd_rost1

df_dyd_rost_titulos <- df_dyd_rost_titulos |> 
  filter(index %in% df_dyd$index)

write_xlsx(df_dyd_rost_titulos, here("Datos/Encuesta docente/df_dyd_rost_titulos.xlsx"))

