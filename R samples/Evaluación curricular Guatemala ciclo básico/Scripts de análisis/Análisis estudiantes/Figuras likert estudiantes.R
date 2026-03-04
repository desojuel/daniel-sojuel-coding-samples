# figuras estudiantes

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

# ciencias naturales, segundo básico ----

competencia_por_item_segundo <- c(
  cn2_relacion_ciencia_tecnologia = "C1: Interpretación y discusión de resultados científicos",
  cn2_aplicar_metodo_cientifico   = "C1: Interpretación y discusión de resultados científicos",
  cn2_laboratorio                 = "C1: Interpretación y discusión de resultados científicos",
  cn2_comunicar_resultados        = "C1: Interpretación y discusión de resultados científicos",
  cn2_sistemas_internacionales_medida = "C1: Interpretación y discusión de resultados científicos",
  cn2_diferentes_sistemas_medida  = "C1: Interpretación y discusión de resultados científicos",
  cn2_notacion_cientifica         = "C1: Interpretación y discusión de resultados científicos",
  
  cn2_atmosfera                   = "C2: Comunicación de resultados según los problemas ambientales locales",
  cn2_tiempo_atmosferico          = "C2: Comunicación de resultados según los problemas ambientales locales",
  cn2_efectos_gravedad            = "C2: Comunicación de resultados según los problemas ambientales locales",
  cn2_hidrosfera                  = "C2: Comunicación de resultados según los problemas ambientales locales",
  cn2_aguas_superficiales         = "C2: Comunicación de resultados según los problemas ambientales locales",
  cn2_litosfera                   = "C2: Comunicación de resultados según los problemas ambientales locales",
  cn2_energia_geotermica          = "C2: Comunicación de resultados según los problemas ambientales locales",
  cn2_sobreexplotacion_minera     = "C2: Comunicación de resultados según los problemas ambientales locales",
  cn2_uso_sustentable_agua        = "C2: Comunicación de resultados según los problemas ambientales locales",
  cn2_organismos_diferentes_reinos= "C2: Comunicación de resultados según los problemas ambientales locales",
  
  cn2_bienestar_animal            = "C2.2: Comunicación de resultados según los problemas ambientales locales",
  cn2_explicar_bioma              = "C2.2: Comunicación de resultados según los problemas ambientales locales",
  cn2_ciclos_naturales            = "C2.2: Comunicación de resultados según los problemas ambientales locales",
  cn2_biodiversidad               = "C2.2: Comunicación de resultados según los problemas ambientales locales",
  cn2_bienes_naturales            = "C2.2: Comunicación de resultados según los problemas ambientales locales",
  cn2_aplicar_4r                  = "C2.2: Comunicación de resultados según los problemas ambientales locales",
  cn2_separacion_basura           = "C2.2: Comunicación de resultados según los problemas ambientales locales",
  cn2_fenomenos_nino              = "C2.2: Comunicación de resultados según los problemas ambientales locales",
  cn2_causas_calentamiento_global = "C2.2: Comunicación de resultados según los problemas ambientales locales",
  cn2_amenazas_naturales          = "C2.2: Comunicación de resultados según los problemas ambientales locales",
  
  cn2_celulas_animales            = "C3: Aplica conocimiento para el mejoramiento de la salud individual y colectiva",
  cn2_salud_celular               = "C3: Aplica conocimiento para el mejoramiento de la salud individual y colectiva",
  cn2_olla_alimentaria            = "C3: Aplica conocimiento para el mejoramiento de la salud individual y colectiva",
  cn2_trastornos_alimenticios     = "C3: Aplica conocimiento para el mejoramiento de la salud individual y colectiva",
  cn2_huesos_principales          = "C3: Aplica conocimiento para el mejoramiento de la salud individual y colectiva",
  cn2_sistemas_cuerpo_humano      = "C3: Aplica conocimiento para el mejoramiento de la salud individual y colectiva",
  cn2_sexo_genero                 = "C3: Aplica conocimiento para el mejoramiento de la salud individual y colectiva",
  cn2_salud_sexual                = "C3: Aplica conocimiento para el mejoramiento de la salud individual y colectiva",
  cn2_medicina_convencional       = "C3: Aplica conocimiento para el mejoramiento de la salud individual y colectiva",
  cn2_abuso_drogas                = "C3: Aplica conocimiento para el mejoramiento de la salud individual y colectiva",
  cn2_medicamentos                = "C3: Aplica conocimiento para el mejoramiento de la salud individual y colectiva",
  
  cn2_clasificacion_materia       = "C4: Explica fenómenos físicos y químicos que ocurren en la materia y la energía",
  cn2_caracteristicas_atomos      = "C4: Explica fenómenos físicos y químicos que ocurren en la materia y la energía",
  cn2_modelo_rutheford            = "C4: Explica fenómenos físicos y químicos que ocurren en la materia y la energía",
  cn2_elementos_quimicos          = "C4: Explica fenómenos físicos y químicos que ocurren en la materia y la energía",
  cn2_diferenciar_metales         = "C4: Explica fenómenos físicos y químicos que ocurren en la materia y la energía",
  cn2_compuestos_quimicos_binarios= "C4: Explica fenómenos físicos y químicos que ocurren en la materia y la energía",
  cn2_magnitudes_escalares        = "C4: Explica fenómenos físicos y químicos que ocurren en la materia y la energía",
  
  cn2_graficas_magnitudes_vectoriales = "C4.2: Explica fenómenos físicos y químicos que ocurren en la materia y la energía",
  cn2_sistema_vectores            = "C4.2: Explica fenómenos físicos y químicos que ocurren en la materia y la energía",
  cn2_principios_newton           = "C4.2: Explica fenómenos físicos y químicos que ocurren en la materia y la energía",
  cn2_aplicar_mcu                 = "C4.2: Explica fenómenos físicos y químicos que ocurren en la materia y la energía",
  cn2_equilibro_estatico          = "C4.2: Explica fenómenos físicos y químicos que ocurren en la materia y la energía",
  cn2_cuerpo_libre                = "C4.2: Explica fenómenos físicos y químicos que ocurren en la materia y la energía"
)



#---Gráficas Ciencias Naturales---

#PRIMERO BÁSICO

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
# ggsave(
#   filename = "grafica_natuales_primero_compe1.png",
#   plot = final,
#   path = "C:/Users/50241/Documents/MINEDUC/Análisis 2025/Gráficas",
#   width = 11, height = 6, dpi = 300, bg = "white") #Lo puse para mi compu para pruebas

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

# ggsave(
#   filename = "grafica_natuales_primero_compe2.png", plot = final, path = "C:/Users/50241/Documents/MINEDUC/Análisis 2025/Gráficas",
#   width = 11, height = 6,dpi = 300,bg = "white")


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

# ggsave(
#   filename = "grafica_natuales_primero_compe2_2.png", plot = final, path = "C:/Users/50241/Documents/MINEDUC/Análisis 2025/Gráficas",
#   width = 11, height = 6,dpi = 300,bg = "white")

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

# ggsave(
#   filename = "grafica_natuales_primero_compe3.png", plot = final, path = "C:/Users/50241/Documents/MINEDUC/Análisis 2025/Gráficas",
#   width = 11, height = 6,dpi = 300,bg = "white")

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

# ggsave(
#   filename = "grafica_natuales_primero_compe4.png", plot = final, path = "C:/Users/50241/Documents/MINEDUC/Análisis 2025/Gráficas",
#   width = 11, height = 6,dpi = 300,bg = "white")

#SEGUNDO BÁSICO
#Primera competencia

items_segundo_c1 <- names(competencia_por_item_segundo[competencia_por_item_segundo ==
                                         "C1: Interpretación y discusión de resultados científicos"])

df_est_segundo_c1 <- df_est[, items_segundo_c1]
df_est_segundo_c1 <- as.data.frame(lapply(df_est_segundo_c1, factor,
                                  levels = c("Sí", "No recuerdo", "No"))) #Factores ordenados

#Etiquetas + gráfica
df_est_segundo_c1 <- df_est_segundo_c1 %>% 
  set_variable_labels( cn2_relacion_ciencia_tecnologia = "...explicar la relación entre ciencia, tecnología y sociedad?",
                       cn2_aplicar_metodo_cientifico   = "...aplicar el método científico al indagar u observar el medio natural o ambiental?",
                       cn2_laboratorio                 = "...usar recursos del laboratorio en indagaciones u observaciones científicas?",
                       cn2_comunicar_resultados        = "...comunicar resultados mediante reportes científicos?",
                       cn2_sistemas_internacionales_medida = "...usar sistemas internacionales de medida: longitud, masa, tiempo, volumen y temperatura?",
                       cn2_diferentes_sistemas_medida  = "...convertir medidas entre diferentes sistemas de medida, por ejemplo, convertir de pies a centímetros?",
                       cn2_notacion_cientifica         = "...expresar mediciones o cantidades en notación científica?"
    )

naturales_segundo_c1 <- gglikert(
  df_est_segundo_c1,
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
    title = "Segundo básico, Ciencias Naturales, Competencia 1",
    subtitle = "Interpretación y discusión de resultados científicos",
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
  naturales_segundo_c1 + 
    theme(legend.position = "bottom", legend.justification = "center"))
final <- plot_grid(
  naturales_segundo_c1,
  legend_grob,
  ncol = 1,
  rel_heights = c(1, 0.11))

# ggsave(
#   filename = "grafica_natuales_segundo_compe1.png", plot = final, path = "C:/Users/50241/Documents/MINEDUC/Análisis 2025/Gráficas",
#   width = 11, height = 6,dpi = 300,bg = "white")

#competencia 2

items_segundo_c2 <- names(competencia_por_item_segundo[competencia_por_item_segundo ==
                                                         "C2: Comunicación de resultados según los problemas ambientales locales"])

df_est_segundo_c2 <- df_est[, items_segundo_c2]
df_est_segundo_c2 <- as.data.frame(lapply(df_est_segundo_c2, factor,
                                          levels = c("Sí", "No recuerdo", "No"))) #Factores ordenados

#Etiquetas + gráfica
df_est_segundo_c2 <- df_est_segundo_c2 %>% 
  set_variable_labels( cn2_atmosfera                   = "...describir la atmósfera y los fenómenos atmosféricos?",
                       cn2_tiempo_atmosferico          = "...diferenciar entre tiempo atmosférico y el clima?",
                       cn2_efectos_gravedad            = "...describir los efectos de la fuerza de gravedad en la tierra?",
                       cn2_hidrosfera                  = "...explicar las funciones y la distribución de la hidrosfera?",
                       cn2_aguas_superficiales         = "...explicar la situación de las aguas superficiales y subterráneas del país?",
                       cn2_litosfera                   = "...describir los fenómenos que se producen en la litosfera?",
                       cn2_energia_geotermica          = "...explicar los usos, ventajas y desventajas de la energía geotérmica?",
                       cn2_sobreexplotacion_minera     = "...mencionar causas y efectos de la sobreexplotación minera en el país?",
                       cn2_uso_sustentable_agua        = "...explicar en qué consiste el uso sustentable del agua y de los bosques del país?",
                       cn2_organismos_diferentes_reinos= "...comparar organismos de los diferentes reinos?"
  )

naturales_segundo_c2 <- gglikert(
  df_est_segundo_c2,
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
    title = "Segundo básico, Ciencias Naturales, Competencia 2",
    subtitle = "Comunicación de resultados según los problemas ambientales locales",
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
  naturales_segundo_c2 + 
    theme(legend.position = "bottom", legend.justification = "center"))
final <- plot_grid(
  naturales_segundo_c2,
  legend_grob,
  ncol = 1,
  rel_heights = c(1, 0.11))

# ggsave(
#   filename = "grafica_natuales_segundo_compe2.png", plot = final, path = "C:/Users/50241/Documents/MINEDUC/Análisis 2025/Gráficas",
#   width = 11, height = 6,dpi = 300,bg = "white")

#competencia 2.2 

items_segundo_c2_2 <- names(competencia_por_item_segundo[competencia_por_item_segundo ==
                                                         "C2.2: Comunicación de resultados según los problemas ambientales locales"])

df_est_segundo_c2_2 <- df_est[, items_segundo_c2_2]
df_est_segundo_c2_2 <- as.data.frame(lapply(df_est_segundo_c2_2, factor,
                                          levels = c("Sí", "No recuerdo", "No"))) #Factores ordenados

#Etiquetas + gráfica
df_est_segundo_c2_2 <- df_est_segundo_c2_2 %>% 
  set_variable_labels( cn2_bienestar_animal            = "...indicar la importancia de la Ley de Bienestar Animal?",
                       cn2_explicar_bioma              = "...explicar en qué consiste un bioma?",
                       cn2_ciclos_naturales            = "...explicar la influencia de los ciclos naturales en el equilibrio ecológico?",
                       cn2_biodiversidad               = "...mencionar acciones para conservar la biodiversidad del país?",
                       cn2_bienes_naturales            = "...explicar el uso sustentable de los bienes naturales renovables y no renovables?",
                       cn2_aplicar_4r                  = "...aplicar las 4R (reducir, reutilizar, reciclar y recuperar)?",
                       cn2_separacion_basura           = "...colocar la basura de la casa en el lugar correcto, separando lo que se puede reciclar?",
                       cn2_fenomenos_nino              = "...explicar las causas y consecuencias de los fenómenos del niño y la niña?",
                       cn2_causas_calentamiento_global = "...explicar las causas del calentamiento global?",
                       cn2_amenazas_naturales          = "...mencionar medidas de prevención o mitigación de las amenazas naturales?"
  )

naturales_segundo_c2_2 <- gglikert(
  df_est_segundo_c2_2,
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
    title = "Segundo básico, Ciencias Naturales, Continuidad competencia 2",
    subtitle = "Comunicación de resultados según los problemas ambientales locales",
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
  naturales_segundo_c2_2 + 
    theme(legend.position = "bottom", legend.justification = "center"))
final <- plot_grid(
  naturales_segundo_c2_2,
  legend_grob,
  ncol = 1,
  rel_heights = c(1, 0.11))

# ggsave(
#   filename = "grafica_natuales_segundo_compe2_2.png", plot = final, path = "C:/Users/50241/Documents/MINEDUC/Análisis 2025/Gráficas",
#   width = 11, height = 6,dpi = 300,bg = "white")

#Gráfica competencia 3

items_segundo_c3 <- names(competencia_por_item_segundo[competencia_por_item_segundo ==
                                                           "C3: Aplica conocimiento para el mejoramiento de la salud individual y colectiva"])

df_est_segundo_c3 <- df_est[, items_segundo_c3]
df_est_segundo_c3 <- as.data.frame(lapply(df_est_segundo_c3, factor,
                                            levels = c("Sí", "No recuerdo", "No"))) #Factores ordenados

#Etiquetas + gráfica
df_est_segundo_c3 <- df_est_segundo_c3 %>% 
  set_variable_labels( cn2_celulas_animales            = "...mencionar las características de las células animales y vegetales?",
                       cn2_salud_celular               = "...explicar qué es la salud celular, el estrés oxidativo y la inflamación?",
                       cn2_olla_alimentaria            = "...explica qué es la olla alimentaria para Guatemala?",
                       cn2_trastornos_alimenticios     = "...explicar causas y efectos de los trastornos alimenticios?",
                       cn2_huesos_principales          = "...identificar huesos y músculos principales del cuerpo?",
                       cn2_sistemas_cuerpo_humano      = "...describir sistemas del cuerpo humano y sus funciones?",
                       cn2_sexo_genero                 = "...distinguir entre sexo (mujer y hombre), género (femenino y masculino)?",
                       cn2_salud_sexual                = "...explicar qué es la salud sexual y reproductiva?",
                       cn2_medicina_convencional       = "...argumentar acerca de las ventajas y desventajas de la medicina convencional y la medicina alternativa?",
                       cn2_abuso_drogas                = "...indicar las causas y efectos del abuso de drogas?",
                       cn2_medicamentos                = "...explicar a qué se refiere el uso correcto de medicamentos para mantener la salud?"
  )

naturales_segundo_c3 <- gglikert(
  df_est_segundo_c3,
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
    title = "Segundo básico, Ciencias Naturales, Competencia 3",
    subtitle = "Aplica conocimiento para el mejoramiento de la salud individual y colectiva",
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
  naturales_segundo_c3 + 
    theme(legend.position = "bottom", legend.justification = "center"))
final <- plot_grid(
  naturales_segundo_c3,
  legend_grob,
  ncol = 1,
  rel_heights = c(1, 0.11))

# ggsave(
#   filename = "grafica_natuales_segundo_compe3.png", plot = final, path = "C:/Users/50241/Documents/MINEDUC/Análisis 2025/Gráficas",
#   width = 11, height = 6,dpi = 300,bg = "white")

#Competencia 4


items_segundo_c4 <- names(competencia_por_item_segundo[competencia_por_item_segundo ==
                                                         "C4: Explica fenómenos físicos y químicos que ocurren en la materia y la energía"])

df_est_segundo_c4 <- df_est[, items_segundo_c4]
df_est_segundo_c4 <- as.data.frame(lapply(df_est_segundo_c4, factor,
                                          levels = c("Sí", "No recuerdo", "No"))) #Factores ordenados

#Etiquetas + gráfica
df_est_segundo_c4 <- df_est_segundo_c4 %>% 
  set_variable_labels( cn2_clasificacion_materia       = "...explicar cómo se clasifica la materia, cómo son sus propiedades físicas y cómo cambia de estado?",
                       cn2_caracteristicas_atomos      = "...mostrar cómo son las características de los átomos?",
                       cn2_modelo_rutheford            = "...describir los postulados del modelo atómico de Rutheford?",
                       cn2_elementos_quimicos          = "...calcular el número de masa de los elementos químicos del grupo I de la tabla periódica de los elementos?",
                       cn2_diferenciar_metales         = "...diferenciar entre metales y no metales?",
                       cn2_compuestos_quimicos_binarios= "...identificar algunos compuestos químicos binarios?",
                       cn2_magnitudes_escalares        = "...diferenciar entre magnitudes escalares y vectoriales?")

naturales_segundo_c4 <- gglikert(
  df_est_segundo_c4,
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
    title = "Segundo básico, Ciencias Naturales, Competencia 4",
    subtitle = "Explica fenómenos físicos y químicos que ocurren en la materia y la energía",
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
  naturales_segundo_c4 + 
    theme(legend.position = "bottom", legend.justification = "center"))
final <- plot_grid(
  naturales_segundo_c4,
  legend_grob,
  ncol = 1,
  rel_heights = c(1, 0.11))

# ggsave(
#   filename = "grafica_natuales_segundo_compe4.png", plot = final, path = "C:/Users/50241/Documents/MINEDUC/Análisis 2025/Gráficas",
#   width = 11, height = 6,dpi = 300,bg = "white")

#Competencia 4_2
items_segundo_c4_2 <- names(competencia_por_item_segundo[competencia_por_item_segundo ==
                                                         "C4.2: Explica fenómenos físicos y químicos que ocurren en la materia y la energía"])

df_est_segundo_c4_2 <- df_est[, items_segundo_c4_2]
df_est_segundo_c4_2 <- as.data.frame(lapply(df_est_segundo_c4_2, factor,
                                          levels = c("Sí", "No recuerdo", "No"))) #Factores ordenados

#Etiquetas + gráfica
df_est_segundo_c4_2 <- df_est_segundo_c4_2 %>% 
  set_variable_labels( cn2_graficas_magnitudes_vectoriales = "...usar gráficas para representar magnitudes vectoriales?",
                       cn2_sistema_vectores            = "...calcular la resultante de un sistema de vectores de fuerzas, desplazamientos o velocidades?",
                       cn2_principios_newton           = "...aplicar los principios y conceptos básicos de la mecánica de Newton en para resolver ejercicios o problemas?",
                       cn2_aplicar_mcu                 = "...aplicar los principios y conceptos básicos de la mecánica de Newton, como el Movimiento circular uniforme (MCU) y el Movimiento rectilíneo uniforme acelerado (MRUA), para resolver ejercicios o problemas?",
                       cn2_equilibro_estatico          = "...representar cuerpos en equilibro estático y las fuerzas que intervienen (la normal, el peso y la tensión)?",
                       cn2_cuerpo_libre                = "...elaborar diagramas de cuerpo libre?")

naturales_segundo_c4_2 <- gglikert(
  df_est_segundo_c4_2,
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
    title = "Segundo básico, Ciencias Naturales, Segunda parte competencia 4",
    subtitle = "Explica fenómenos físicos y químicos que ocurren en la materia y la energía",
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
  naturales_segundo_c4_2 + 
    theme(legend.position = "bottom", legend.justification = "center"))
final <- plot_grid(
  naturales_segundo_c4_2,
  legend_grob,
  ncol = 1,
  rel_heights = c(1, 0.11))

# ggsave(
#   filename = "grafica_natuales_segundo_compe4_2.png", plot = final, path = "C:/Users/50241/Documents/MINEDUC/Análisis 2025/Gráficas",
#   width = 11, height = 6,dpi = 300,bg = "white")
