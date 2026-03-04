df_est <- read_xlsx(here("Datos","Listos para análisis","Encuesta estudiantes","df_est.xlsx"))

# ciencias naturales, primero básico ----

#Aquí asigné qué ítem va en cada competencia
competencia_por_item <- c(
  #Competencia 1
  cn1_campos_estudio = "C1. Actitudes y habilidades científicas",
  cn1_conocimientos_cientificos = "C1. Actitudes y habilidades científicas",
  cn1_metodo_cientifico = "C1. Actitudes y habilidades científicas",
  cn1_recursos_laboratorio = "C1. Actitudes y habilidades científicas",
  cn1_reportes_cientificos = "C1. Actitudes y habilidades científicas",
  cn1_ciencia_tecnologia = "C1. Actitudes y habilidades científicas",
  cn1_maquinas_simples = "C1. Actitudes y habilidades científicas",
  
  #Competencia 2
  cn1_sistema_solar = "C2. Elementos y fenómenos naturales",
  cn1_fuerza_gravedad = "C2. Elementos y fenómenos naturales",
  cn1_hidrosfera = "C2. Elementos y fenómenos naturales",
  cn1_conservacion_agua = "C2. Elementos y fenómenos naturales",
  cn1_placas_tectonicas = "C2. Elementos y fenómenos naturales",
  cn1_suelo = "C2. Elementos y fenómenos naturales",
  cn1_explotacion_minera = "C2. Elementos y fenómenos naturales",
  cn1_deforestacion = "C2. Elementos y fenómenos naturales",
  cn1_organismos = "C2. Elementos y fenómenos naturales",
  
  cn1_biomas = "C2.2 Elementos y fenómenos naturales",
  cn1_areas_protegidas = "C2.2 Elementos y fenómenos naturales",
  cn1_bienes_naturales = "C2.2 Elementos y fenómenos naturales",
  cn1_ecosistemas = "C2.2 Elementos y fenómenos naturales",
  cn1_proyecto_4r = "C2.2 Elementos y fenómenos naturales",
  cn1_riesgos_comunidad = "C2.2 Elementos y fenómenos naturales",
  cn1_calentamiento_global = "C2.2 Elementos y fenómenos naturales",
  cn1_adaptacion_familiar = "C2.2 Elementos y fenómenos naturales",
  
  #Competencia 3
  cn1_funciones_vitales = "C3. Reconocimiento de su organismo y sus funciones",
  cn1_alimentos = "C3. Reconocimiento de su organismo y sus funciones",
  cn1_enfermedad_nutricional = "C3. Reconocimiento de su organismo y sus funciones",
  cn1_estructuras_celulares = "C3. Reconocimiento de su organismo y sus funciones",
  cn1_cuerpo_humano = "C3. Reconocimiento de su organismo y sus funciones",
  cn1_sexo = "C3. Reconocimiento de su organismo y sus funciones",
  cn1_transmision_sexual = "C3. Reconocimiento de su organismo y sus funciones",
  cn1_medicina_convencional = "C3. Reconocimiento de su organismo y sus funciones",
  cn1_drogas = "C3. Reconocimiento de su organismo y sus funciones",
  cn1_efectos_drogas = "C3. Reconocimiento de su organismo y sus funciones",
  cn1_medicamentos = "C3. Reconocimiento de su organismo y sus funciones",
  
  #Competencia 4
  cn1_estados_materia = "C4. Fenómenos físicos y químicos en la materia y la energía",
  cn1_atomo = "C4. Fenómenos físicos y químicos en la materia y la energía",
  cn1_modelo_atomico = "C4. Fenómenos físicos y químicos en la materia y la energía",
  cn1_tabla_periodica = "C4. Fenómenos físicos y químicos en la materia y la energía",
  cn1_mecanica_newton = "C4. Fenómenos físicos y químicos en la materia y la energía"
)

#PRIMERA COMPETENCIA

#Filtrar ítems de la competencia 1
items_c1 <- names(competencia_por_item[competencia_por_item ==
                                         "C1. Actitudes y habilidades científicas"])

#Seleccionar columnas y ordenar factores
df_est_c1 <- df_est[, items_c1] %>% 
  lapply(factor, levels = c("Sí", "No recuerdo", "No")) %>% 
  as.data.frame()

#Etiquetas
df_est_c1 <- set_variable_labels(
  df_est_c1,
  cn1_campos_estudio = "...diferenciar los campos de estudio de las Ciencias Naturales?",
  cn1_conocimientos_cientificos = "...identificar los conocimientos científicos (crítico, fundamentado, metódico, verificable y otros)?",
  cn1_metodo_cientifico = "...aplicar el método científico?",
  cn1_recursos_laboratorio = "...utilizar recursos de laboratorio?",
  cn1_reportes_cientificos = "...elaborar reportes científicos?",
  cn1_ciencia_tecnologia = "...diferenciar ciencia y tecnología?",
  cn1_maquinas_simples = "...diferenciar entre máquinas simples y compuestas?")

# Gráfica
naturales_primero_compe1 <- gglikert(
  df_est_c1,
  sort = "descending",
  totals_include_center = FALSE,
  sort_prop_include_center = FALSE,
  y_label_wrap = 55,
  labels_size = 4,
  labels_accuracy = 0.1,
  totals_size = 0,
  labels_color = "black"
) +
  labs(
    title = "Primero básico, Ciencias Naturales",
    subtitle = "Competencia 1: Actitudes y habilidades científicas",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin = margin(b = 15)),
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(b = 15)),
    legend.position = "none",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 11, hjust = 0),
    axis.text.x = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 25, 5, 25),
    plot.title.position = "plot"
  ) +
  scale_x_continuous(labels = NULL, breaks = NULL)

#Leyenda centrada
legend_grob <- get_legend(
  naturales_primero_compe1 + theme(legend.position = "bottom", legend.justification = "center"))
final <- plot_grid(
  naturales_primero_compe1,
  legend_grob,
  ncol = 1,
  rel_heights = c(1, 0.11))

#Guardar
ggsave(
  filename = "grafica_natuales_primero_compe1.png",
  plot = final,
  path = "C:/Users/50241/Documents/MINEDUC/Análisis 2025/Gráficas",
  width = 11, height = 6, dpi = 300, bg = "white") #Lo puse para mi compu para pruebas

#---SEGUNDA COMPETENCIA--- 

#PRIMERA PARTE

#Competencia 2: Elementos y fenómenos naturales

items_c2 <- names(competencia_por_item[competencia_por_item == "C2. Elementos y fenómenos naturales"])
df_est_c2 <- df_est[, items_c2] %>% 
  lapply(factor, levels = c("Sí", "No recuerdo", "No")) %>%  
  as.data.frame()

df_est_c2 <- df_est_c2 %>% 
  set_variable_labels(
    cn1_sistema_solar = "...diferenciar entre una galaxia, la Vía Láctea, el Sistema Solar y sus componentes?",
    cn1_fuerza_gravedad = "...describir cómo la fuerza de gravedad entre la Tierra, el Sol y la Luna afecta la vida diaria?",
    cn1_hidrosfera = "...distinguir los componentes de la hidrosfera (aguas lóticas, lénticas, freáticas y atmosféricas)?",
    cn1_conservacion_agua = "...mencionar formas para conservar el agua para el consumo?",
    cn1_placas_tectonicas = "...explicar los efectos de los movimientos de las placas tectónicas?",
    cn1_suelo = "...mostrar cómo es la estructura del suelo y las fuentes de energía geotérmica?",
    cn1_explotacion_minera = "...señalar las ventajas y desventajas de la explotación minera en el país?",
    cn1_deforestacion = "...mencionar formas para evitar la deforestación?",
    cn1_organismos = "...clasificar a los organismos según su nivel de organización ecológica (especies, poblaciones, comunidades, ecosistemas y biosfera)?")

naturales_primero_compe2 <- gglikert(
  df_est_c2,
  sort = "descending",
  totals_include_center = FALSE,
  sort_prop_include_center = FALSE,
  y_label_wrap = 55,
  labels_size = 4,
  labels_accuracy = 0.1,
  totals_size = 0,
  labels_color = "black"
) +
  labs(
    title = "Primero básico, Ciencias Naturales",
    subtitle = "Primera parte competencia 2: Elementos y fenómenos naturales",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin = margin(b = 15)), 
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(b = 15)),
    legend.position = "none",
    axis.text.y = element_text(size = 11, hjust = 0), 
    axis.text.x = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 25, 5, 25),
    plot.title.position = "plot"
  ) +
  scale_x_continuous(labels = NULL, breaks = NULL)

legend_grob <- get_legend(
  naturales_primero_compe2 + 
    theme(legend.position = "bottom", legend.justification = "center"))
final <- plot_grid(
  naturales_primero_compe2,
  legend_grob,
  ncol = 1,
  rel_heights = c(1, 0.11))

ggsave(
  filename = "grafica_natuales_primero_compe2.png", plot = final, path = "C:/Users/50241/Documents/MINEDUC/Análisis 2025/Gráficas",
  width = 11, height = 6,dpi = 300,bg = "white")


#SEGUNDA PARTE, SEGUNDA COMPETENCIA
items_c2_2 <- names(competencia_por_item[competencia_por_item ==
                                           "C2.2 Elementos y fenómenos naturales"])

df_est_c2_2 <- df_est[, items_c2_2]
df_est_c2_2 <- as.data.frame(lapply(df_est_c2_2, factor,
                                    levels = c("Sí", "No recuerdo", "No"))) #Factores ordenados

#Etiquetas + gráfica
df_est_c2_2 <- df_est_c2_2 %>% 
  set_variable_labels(
    cn1_areas_protegidas = "...identificar las áreas protegidas de Guatemala y su importancia?",
    cn1_bienes_naturales = "...mencionar ventajas y desventajas de los bienes naturales renovables y no renovables?",
    cn1_ecosistemas = "...describir acciones y procesos que dañan los ecosistemas (ciclos de producción, distribución, uso y forma de disponer los residuos y desechos sólidos)?",
    cn1_proyecto_4r = "...explicar en qué consiste el proyecto 4R (reducir, reutilizar, reciclar y recuperar)?",
    cn1_riesgos_comunidad = "...identificar acciones para gestionar riesgos locales ante amenazas para la comunidad y el país (sismos, inundaciones, sequías y erupciones volcánicas)?",
    cn1_calentamiento_global = "...explicar la relación entre el calentamiento global y el cambio climático?",
    cn1_adaptacion_familiar = "...mencionar acciones para la adaptación familiar o comunitaria al cambio climático?",
    cn1_biomas = "...mencionar las formas para conservar los biomas y la biodiversidad del país?")

naturales_primero_compe2_2 <- gglikert(
  df_est_c2_2,
  sort = "descending",
  totals_include_center = FALSE,
  sort_prop_include_center = FALSE,
  y_label_wrap = 55,
  labels_size = 4,
  labels_accuracy = 0.1,
  totals_size = 0,
  labels_color = "black"
) +
  labs(
    title = "Primero básico, Ciencias Naturales, Competencia 2",
    subtitle = "Segunda parte competencia 2: Elementos y fenómenos naturales",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin = margin(b = 15)), 
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(b = 15)),
    legend.position = "none",
    axis.text.y = element_text(size = 11, hjust = 0), 
    axis.text.x = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 25, 5, 25),
    plot.title.position = "plot"
  ) +
  scale_x_continuous(labels = NULL, breaks = NULL)

legend_grob <- get_legend(
  naturales_primero_compe2_2 + 
    theme(legend.position = "bottom", legend.justification = "center"))
final <- plot_grid(
  naturales_primero_compe2_2,
  legend_grob,
  ncol = 1,
  rel_heights = c(1, 0.11))

ggsave(
  filename = "grafica_natuales_primero_compe2_2.png", plot = final, path = "C:/Users/50241/Documents/MINEDUC/Análisis 2025/Gráficas",
  width = 11, height = 6,dpi = 300,bg = "white")

#TERCERA COMPETENCIA
items_c3 <- names(competencia_por_item[competencia_por_item ==
                                         "C3. Reconocimiento de su organismo y sus funciones"])

df_est_c3 <- df_est[, items_c3]
df_est_c3 <- as.data.frame(lapply(df_est_c3, factor,
                                  levels = c("Sí", "No recuerdo", "No"))) #Factores ordenados

#Etiquetas + gráfica
df_est_c3 <- df_est_c3 %>% 
  set_variable_labels(
    cn1_funciones_vitales = "...explicar cómo las plantas y los animales realizan sus funciones vitales?",
    cn1_alimentos = "...describir los alimentos que una persona de tu edad debe consumir para nutrirse?",
    cn1_enfermedad_nutricional = "...diferenciar entre una enfermedad nutricional y un desequilibrio de nutrientes?",
    cn1_estructuras_celulares = "...identificar estructuras celulares y sus procesos?",
    cn1_cuerpo_humano = "...describir las características de los sistemas del cuerpo humano, sus funciones y afecciones?",
    cn1_sexo = "...distinguir entre sexo (mujer y hombre) y género (femenino y masculino)?",
    cn1_transmision_sexual = "...describir infecciones de transmisión sexual y sus medidas de prevención?",
    cn1_medicina_convencional = "...diferenciar entre medicina convencional y medicina alternativa?",
    cn1_drogas = "...identificar drogas ilegales de mayor consumo en el país?",
    cn1_efectos_drogas = "...identificar los efectos de las drogas ilegales en la salud?",
    cn1_medicamentos = "...consumir medicamentos de manera correcta (cumplir las indicaciones del médico, dosis correcta, tiempo y forma de toma, analizar la información en el envase)?")

naturales_primero_compe3 <- gglikert(
  df_est_c3,
  sort = "descending",
  totals_include_center = FALSE,
  sort_prop_include_center = FALSE,
  y_label_wrap = 55,
  labels_size = 4,
  labels_accuracy = 0.1,
  totals_size = 0,
  labels_color = "black"
) +
  labs(
    title = "Primero básico, Ciencias Naturales, Competencia 3",
    subtitle = "Reconocimiento de su organismo y funciones",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin = margin(b = 15)), 
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(b = 15)),
    legend.position = "none",
    axis.text.y = element_text(size = 11, hjust = 0), 
    axis.text.x = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 25, 5, 25),
    plot.title.position = "plot"
  ) +
  scale_x_continuous(labels = NULL, breaks = NULL)

legend_grob <- get_legend(
  naturales_primero_compe3 + 
    theme(legend.position = "bottom", legend.justification = "center"))
final <- plot_grid(
  naturales_primero_compe3,
  legend_grob,
  ncol = 1,
  rel_heights = c(1, 0.11))

ggsave(
  filename = "grafica_natuales_primero_compe3.png", plot = final, path = "C:/Users/50241/Documents/MINEDUC/Análisis 2025/Gráficas",
  width = 11, height = 6,dpi = 300,bg = "white")

#CUARTA COMPETENCIA
items_c4 <- names(competencia_por_item[competencia_por_item ==
                                         "C4. Fenómenos físicos y químicos en la materia y la energía"])

df_est_c4 <- df_est[, items_c4]
df_est_c4 <- as.data.frame(lapply(df_est_c4, factor,
                                  levels = c("Sí", "No recuerdo", "No"))) #Factores ordenados

#Etiquetas + gráfica
df_est_c4 <- df_est_c4 %>% 
  set_variable_labels(
    cn1_estados_materia = "...describir qué es la materia, sus propiedades extensivas e intensivas y sus estados físicos (líquido, sólido y gaseoso, plasma, condensado Bose)?",
    cn1_atomo = "...identificar las partes del átomo?",
    cn1_modelo_atomico = "...describir el modelo atómico de Thomson?",
    cn1_tabla_periodica = "...usar la tabla periódica de los elementos?",
    cn1_mecanica_newton = "...aplicar conceptos básicos de la mecánica de Newton en un laboratorio, por ejemplo: el movimiento, el movimiento rectilíneo uniforme, la velocidad, el espacio y tiempo, la fuerza normal, el peso o el equilibrio estático?")

naturales_primero_compe4 <- gglikert(
  df_est_c4,
  sort = "descending",
  totals_include_center = FALSE,
  sort_prop_include_center = FALSE,
  y_label_wrap = 55,
  labels_size = 4,
  labels_accuracy = 0.1,
  totals_size = 0,
  labels_color = "black"
) +
  labs(
    title = "Primero básico, Ciencias Naturales, Competencia 4",
    subtitle = "Fenómenos físicos y químicos en la materia",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin = margin(b = 15)), 
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(b = 15)),
    legend.position = "none",
    axis.text.y = element_text(size = 11, hjust = 0), 
    axis.text.x = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 25, 5, 25),
    plot.title.position = "plot"
  ) +
  scale_x_continuous(labels = NULL, breaks = NULL)

legend_grob <- get_legend(
  naturales_primero_compe4 + 
    theme(legend.position = "bottom", legend.justification = "center"))
final <- plot_grid(
  naturales_primero_compe4,
  legend_grob,
  ncol = 1,
  rel_heights = c(1, 0.11))

ggsave(
  filename = "grafica_natuales_primero_compe4.png", plot = final, path = "C:/Users/50241/Documents/MINEDUC/Análisis 2025/Gráficas",
  width = 11, height = 6,dpi = 300,bg = "white")