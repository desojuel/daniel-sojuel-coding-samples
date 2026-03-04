gfocales <- read_xlsx(here("Datos","Formulario grupos focales", "gfocales.xlsx"))

val_cleaned <- read_xlsx(here("datasets para procesamiento/limpieza gfocales.xlsx"))

#otro_rol_centro----
var1_cleaned <- val_cleaned %>%  
  filter(nombre == "otro_rol_centro") %>%  
  select(-nombre)

valores_otro_rol_centro <- gfocales |> 
  filter(!(is.na(otro_rol_centro))) |> 
  select(rol_centro, otro_rol_centro)

select_multiple_puntocoma(gfocales, rol_centro, "Var original")


gfocales <- gfocales |> 
  rename(valor = otro_rol_centro) |> 
  left_join(var1_cleaned, by = "valor") |> 
  rename(otro_rol_centro = categoria) |> 
  select(-valor) %>% 
  relocate(otro_rol_centro, .after = rol_centro)

gfocales <- gfocales |> 
  mutate(rol_centro = str_replace_all(rol_centro, "Otros", otro_rol_centro))

