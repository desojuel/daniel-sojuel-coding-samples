# limpieza de errores numéricos

obs01 <- read_xlsx(here("Datos","Listos para análisis","Observacion a nivel de centros", "obs01.xlsx"))

rule <- validator(!(banos_hombres_cantidad >= 15),
                  !(banos_hombres_cantidad_inodoros >= 15)
                  )

out <- confront(obs01, rule)

summary(out)

codigo_violating <- violating(obs01, out)


table(obs01$banos_hombres_cantidad)
