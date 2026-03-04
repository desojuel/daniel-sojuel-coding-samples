# problema de edad

df_est <- read_xlsx(here("Datos","Encuesta estudiantes", "df_est.xlsx"))

prueba_validation <- validator(!(edad >= 18))

out   <- confront(df_est, prueba_validation)

summary(out)

df_violating <- violating(df_est, out)


