# Funciones y librerías runner

# Lista de scripts en orden
scripts <- c("Scripts de procesamiento/Librerías/Librerías.R",
             "Scripts de análisis/Funciones/funcion tablas simples.R",
             "Scripts de análisis/Funciones/flextable tabla simple.R",
             "Scripts de análisis/Funciones/funcion tablas resumen.R",
             "Scripts de análisis/Funciones/flextable tabla simple resumen.R",
             "Scripts de análisis/Funciones/funcion tabla seleccionar varias.R")

for (s in scripts) {
  message("\n🔹 Ejecutando: ", s)
  source(s, echo = TRUE, chdir = TRUE, encoding = "UTF-8")
}
message("\n✅ Todos los scripts se ejecutaron (o al menos se intentó ejecutarlos).")




