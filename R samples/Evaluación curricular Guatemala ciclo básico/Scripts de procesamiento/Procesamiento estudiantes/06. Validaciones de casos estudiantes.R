library(validate)

# reglas de validación

df_est <- read_xlsx("Datos/Listos para análisis/Encuesta estudiantes/df_est.xlsx")

# Any R expression that results in a logical is accepted by validate as a validation rule.

prueba_validation <- validator(!(edad == 18 & grado == "Primero básico"))

# summary(prueba_validation)

out   <- confront(df_est, prueba_validation)
summary(out)


df_violating <- violating(df_est, out)


# para arreglar usar case_match o case_when dependiendo de lo que se necesite








