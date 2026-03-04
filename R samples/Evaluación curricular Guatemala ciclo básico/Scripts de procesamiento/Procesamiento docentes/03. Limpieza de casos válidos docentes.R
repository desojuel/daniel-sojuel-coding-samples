# leer

df_dyd <- read_xlsx(here("Datos","Encuesta docente","df_dyd.xlsx"))

# limpiar por consentimiento informado

## consentimiento ----

pre_consentimiento <- nrow(df_dyd)

df_dyd <- df_dyd |> 
  filter(consentimiento != "No")

post_consentimiento <- nrow(df_dyd)

sin_consentimiento <- pre_consentimiento - post_consentimiento

## no son docentes o directores

df_dyd <- df_dyd |> 
  filter(confirmar_continuacion != "No")

post_confirmacion <- nrow(df_dyd)

sin_confirmacion <- post_consentimiento - post_confirmacion

df_dyd <- read_xlsx(here("Datos","Encuesta docente","df_dyd.xlsx"))
