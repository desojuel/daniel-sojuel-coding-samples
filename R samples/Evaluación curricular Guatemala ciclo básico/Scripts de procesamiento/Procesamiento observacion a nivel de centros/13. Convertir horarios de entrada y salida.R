time_to_decimal_hour <- function(x) {
  
  out <- rep(NA_real_, length(x))
  ok  <- !is.na(x)
  
  t <- x[ok] |>
    stringr::str_remove("-\\d{2}:\\d{2}$") |>
    stringr::str_remove("\\.\\d+$") |>
    lubridate::hms()
  
  out[ok] <-
    lubridate::hour(t) +
    lubridate::minute(t) / 60 +
    lubridate::second(t) / 3600
  
  out
}


obs01 <- obs01 |> 
  mutate(
    horario_inicio = time_to_decimal_hour(horario_inicio),
    horario_fin     = time_to_decimal_hour(horario_fin),
    duration_hour = horario_fin - horario_inicio
  )

obs01 <- obs01 |>
  dplyr::mutate(
    missing_time = is.na(horario_inicio) | is.na(horario_fin)
  )

obs01 |> 
  filter(jornada == "Vespertina") |> 
  ggplot(aes(x = horario_inicio, y = horario_fin)) +
  geom_point(alpha = 0.7, na.rm = TRUE) +
  labs(
    x = "Entrance time (decimal hours)",
    y = "Exit time (decimal hours)"
  ) +
  theme_minimal()

write_xlsx(obs01, here("Datos/Listos para análisis/Observacion a nivel de centros/obs01.xlsx"))
