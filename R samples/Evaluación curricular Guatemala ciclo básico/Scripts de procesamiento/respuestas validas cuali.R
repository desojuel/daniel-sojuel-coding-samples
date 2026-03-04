# respuestas válidas cuali


df_cuali_obs01 <- read_xlsx(here("Insumos cualis","df_cuali_obs01.xlsx"))
df_cuali_obs02 <- read_xlsx(here("Insumos cualis","df_cuali_obs02.xlsx"))
df_cuali_obs03 <- read_xlsx(here("Insumos cualis","df_cuali_obs03.xlsx"))

# centro ----

df_cuali_obs01 |>
  summarise(total_no_na = sum(rowSums(!is.na(across(c(comentarios_educacion_inclusiva,
                                                      comentario_idioma_estudiantes,
                                                      comentario_adicional_relevante
  ))))))


df_cuali_obs01 |>
  summarise(total_no_na = sum(rowSums(!is.na(across(c(comentario_energia_electrica,
                                                      comentarios_agua,
                                                      comentarios_banos_hombres,
                                                      comentarios_banos_mujeres,
                                                      comentarios_banos_mixtos,
                                                      comentarios_seguridad_hombres,
                                                      comentarios_seguridad_mujeres,
                                                      comentarios_seguridad_banos_mixtos,
                                                      comentarios_banos_limpieza,
                                                      comentarios_falta_banos_centro,
                                                      comentarios_lavado_manos,
                                                      comentarios_gestion_riesgo,
                                                      comentarios_laboratorio_tac,
                                                      comentarios_educacion_fisica_espacio,
                                                      comentarios_salon_musica,
                                                      comentarios_salon_artes_visuales,
                                                      comentarios_salon_teatro,
                                                      comentarios_salon_danza,
                                                      comentarios_laboratorio_ciencias,
                                                      comentario_espacio_emprendimiento,
                                                      comentarios_otros_salones,
                                                      comentarios_donaciones_centro,
                                                      comentarios_timbre_o_campana_centro,
                                                      comentarios_cocina,
                                                      comentarios_remozamiento,
                                                      comentarios_observaciones_generales,
                                                      comentarios_aspectos_fisicos_aula,
                                                      comentarios_mobiliario,
                                                      comentario_fotografia
  ))))))

# áreas ----

df_cuali_obs02 |>
  summarise(total_no_na = sum(rowSums(!is.na(across(c(docente_inicia_clase,
                                                      uso_pizarron,
                                                      gestionar_conducta,
                                                      gestionar_atencion,
                                                      explicacion_tarea,
                                                      acciones_evaluacion_sumativa,
                                                      acciones_evaluacion_formativa,
                                                      cierre_sesion,
                                                      vinculacion_clase,
                                                      acciones_participacion,
                                                      acciones_comprension,
                                                      acciones_dudas,
                                                      acciones_retroalimentacion,
                                                      relevante
  ))))))

df_cuali_obs02 |>
  summarise(total_no_na = sum(rowSums(!is.na(across(c(justificacion,
                                                      actividades_estudiantes_clase,
                                                      caracteristicas_telesecundaria
  ))))))

df_cuali_obs02 |>
  summarise(total_no_na = sum(rowSums(!is.na(across(c(razon_ningun_area,
                                                      razon_suspension,
                                                      incidente
  ))))))





# especialistas ----

df_cuali_obs03 |>
  summarise(total_no_na = sum(rowSums(!is.na(across(c(
    comentarios_docente, actitud_docente, dominio_docente
  ))))))

df_cuali_obs03 |>
  summarise(total_no_na = sum(rowSums(!is.na(across(c(uso_laboratorio, infraestructura
  ))))))

df_cuali_obs03 |>
  summarise(total_no_na = sum(rowSums(!is.na(across(c(uso_cuadernos,
                                                      uso_pizarron,
                                                      uso_libros,
                                                      uso_recursos_didacticos,
                                                      recursos_materiales
  ))))))


df_cuali_obs03 |>
  summarise(total_no_na = sum(rowSums(!is.na(across(c(aplicacion_enfoque_competencias,
                                                      enfoque_desarrollo_area,
                                                      desarrollo_contenido,
                                                      planificacion_docente,
                                                      desarrollo_area_cnb,
                                                      metodologias_docente,
                                                      adecuaciones_curriculares_cnb,
                                                      enfoque_competencias_relacion_docente
  ))))))

df_cuali_obs03 |>
  summarise(total_no_na = sum(rowSums(!is.na(across(c(secuencia_didactica,
                                                      actividades_clase,
                                                      estrategias_ensenanza,
                                                      gestion_aula,
                                                      comunicacion_docente_estudiantes,
                                                      adecuaciones_curriculares,
                                                      bilinguismo_aula,
                                                      asignacion_tareas,
                                                      evaluacion_sumativa_formativa,
                                                      calificacion_tareas,
                                                      instrumentos_evaluacion,
                                                      verificacion_comprension_estudiantes,
                                                      clarificacion_dudas,
                                                      retroalimentacion,
                                                      instrucciones_recordatorios,
                                                      acompanamiento,
                                                      actividades_grupales,
                                                      actividades_adecuaciones_curriculares,
                                                      vinculacion_anterior,
                                                      vinculacion_anterior_necesaria,
                                                      comentarios_vinculacion,
                                                      practicas_seguimiento,
                                                      revision_tareas,
                                                      retroalimentacion_actividades_previas,
                                                      repaso_sesion_anterior,
                                                      retomar_actividades_sesion_anterior,
                                                      relacion_contenidos_previos,
                                                      otras_actividades_seguimiento,
                                                      razon_ningun_periodo,
                                                      incidente_periodo,
                                                      actividades_aprendizaje
  ))))))
