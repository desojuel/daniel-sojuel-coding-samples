gfocales <- read_xlsx(here("Datos","Formulario grupos focales","formulario grupos focales 20.11.25 con versiones.xlsx"))

# colnames

cols_gfocales <- read_xlsx(
  here("datasets para procesamiento", "colnames evaluacion curricular basico.xlsx"),
  col_names = T
)[[5]] |> 
  na.omit() |> 
  as.vector()

colnames(gfocales) <- cols_gfocales

gfocales <- gfocales |> 
  select(-starts_with("out_"))

write_xlsx(gfocales, here("Datos","Formulario grupos focales","gfocales.xlsx"))
