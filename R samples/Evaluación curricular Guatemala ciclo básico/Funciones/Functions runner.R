# Correr todas las librerías y funciones

# función para correr scripts

run_scripts <- function() {
  for (s in scripts) {
    message("\n🔹 Ejecutando: ", s)
    source(s, echo = TRUE, chdir = TRUE, encoding = "UTF-8")
  }
  message("\n✅ Todos los scripts se ejecutaron (o al menos se intentó ejecutarlos).")
}

# Lista de scripts en orden
scripts <- c("Scripts de procesamiento/Librerías/Librerías.R",
             "Funciones/flextable tabla simple resumen.R",
             "Funciones/flextable tabla simple.R",
             "Funciones/funcion longtables para separators.R",
             "Funciones/funcion para arreglar separadores.R",
             "Funciones/funcion tabla seleccionar varias.R",
             "Funciones/funcion tablas resumen.R",
             "Funciones/funcion tablas simples.R",
             "Funciones/limpieza de periodos.R",
             "Funciones/Para hacer longtables.R"
)




run_scripts()


