# Procesamiento listos para análisis

# Lista de scripts en orden
scripts <- c(
             "Scripts de procesamiento/Procesamiento observacion a nivel de centros/01. Archivo df obs01.R",
             "Scripts de procesamiento/Procesamiento observacion de areas/01. Unión de versiones obs02.R",
             "Scripts de procesamiento/Procesamiento observacion de areas/02. Archivo df obs02.R",
             "Scripts de procesamiento/Procesamiento observacion especialistas/01. Unión de versiones obs03.R",
             "Scripts de procesamiento/Procesamiento observacion especialistas/02. Archivo df obs03.R",
             "Scripts de procesamiento/Procesamiento estudiantes/01. Unión de versiones estudiantes.R",
             "Scripts de procesamiento/Procesamiento estudiantes/02. Limpiador de códigos estudiantes.R",
             "Scripts de procesamiento/Procesamiento docentes/01. Unión de versiones docente.R",
             "Scripts de procesamiento/Procesamiento docentes/02. Limpiador de códigos docentes.R",
             "Scripts de procesamiento/Procesamiento participantes grupos focales/01. Unión de versiones gfocales.R",
             "Scripts de procesamiento/Procesamiento docentes/02. Limpiador de códigos gfocales.R"
)

for (s in scripts) {
  message("\n🔹 Ejecutando: ", s)
  source(s, echo = TRUE, chdir = TRUE, encoding = "UTF-8")
}
message("\n✅ Todos los scripts se ejecutaron (o al menos se intentó ejecutarlos).")




