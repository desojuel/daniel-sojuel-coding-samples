# leer

df_est <- read_xlsx(here("Datos","Encuesta estudiantes","df_est.xlsx"))

# limpiar por consentimiento informado

## consentimiento ----

pre_consentimiento <- nrow(df_est)

df_est <- df_est |> 
  filter(participacion != "No")

post_consentimiento <- nrow(df_est)

sin_consentimiento <- pre_consentimiento - post_consentimiento

write_xlsx(df_est, here("Datos","Listos para análisis","Encuesta estudiantes/df_est.xlsx"))



