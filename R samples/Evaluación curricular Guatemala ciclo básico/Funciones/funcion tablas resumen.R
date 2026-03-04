tabla_resumen <- function(df, var) {
  # Función auxiliar para formatear los valores numéricos
  format_value <- function(x) {
    if (is.numeric(x)) {
      rounded_x <- round(x, 3)
      if (rounded_x == as.integer(rounded_x)) {
        return(as.character(as.integer(rounded_x)))
      } else {
        return(sub("\\.?0+$", "", as.character(rounded_x)))
      }
    }
    return(as.character(x))
  }
  
  # Evaluar la variable pasada como símbolo
  var <- rlang::ensym(var)
  
  df |> 
    summarise(
      Media = round(mean(!!var, na.rm = TRUE), 3),
      Mediana = round(median(!!var, na.rm = TRUE), 3),
      `Desviación estándar` = round(sd(!!var, na.rm = TRUE), 3),
      Mínimo = min(!!var, na.rm = TRUE),
      Máximo = max(!!var, na.rm = TRUE),
      Rango = (Máximo - Mínimo)
    ) |> 
    pivot_longer(cols = everything(), names_to = "Estadístico", values_to = "Valor") |> 
    mutate(Valor = sapply(Valor, format_value))
}