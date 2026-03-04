obs03 <- read_xlsx(here("Datos","Observación especialistas","observacion especialistas 25.11.25 con versiones.xlsx"))

# limpiar por fecha
obs03 <- obs03[obs03$start >= as.POSIXct("2025-08-25 00:00:00"), ]

obs03 <- clean_names(obs03)


# unir variables duplicadas por las versiones

obs03 <- obs03 %>% 
  mutate(x29_que_otro_elemento_esta_presente_en_las_planificaciones = coalesce(
    x29_que_otro_elemento_esta_presente_en_las_planificaciones,  
    x25_que_otro_elemento_esta_presente_en_las_planificaciones),
    x30_en_este_espacio_escriba_sus_impresiones_comentarios_o_ideas_relacionadas_a_las_planificaciones_observadas_correspondientes_a_este_dia_periodo_o_semana_desde_su_perspectiva_de_especialista = coalesce(
      x30_en_este_espacio_escriba_sus_impresiones_comentarios_o_ideas_relacionadas_a_las_planificaciones_observadas_correspondientes_a_este_dia_periodo_o_semana_desde_su_perspectiva_de_especialista, 
      x26_en_este_espacio_escriba_sus_impresiones_comentarios_o_ideas_relacionadas_a_las_planificaciones_observadas_correspondientes_a_este_dia_periodo_o_semana_desde_su_perspectiva_de_especialista), 
    x31_lugar_en_donde_se_desarrollo_la_clase_salon_de_clases_actividad_fuera_de_clase_laboratorio_gimnasio_etc = coalesce(
      x31_lugar_en_donde_se_desarrollo_la_clase_salon_de_clases_actividad_fuera_de_clase_laboratorio_gimnasio_etc, 
      x27_lugar_en_donde_se_desarrollo_la_clase_salon_de_clases_actividad_fuera_de_clase_laboratorio_gimnasio_etc), 
    x32_duracion_del_periodo_o_el_tiempo_asignado_a_diferentes_actividades = coalesce(
      x32_duracion_del_periodo_o_el_tiempo_asignado_a_diferentes_actividades, 
      x28_duracion_del_periodo_o_el_tiempo_asignado_a_diferentes_actividades), 
    x33_secuencia_didactica_u_organizacion_del_periodo_de_clase = coalesce( 
      x33_secuencia_didactica_u_organizacion_del_periodo_de_clase,x29_secuencia_didactica_u_organizacion_del_periodo_de_clase), 
    x34_actividades_realizadas_en_clase = coalesce(
      x34_actividades_realizadas_en_clase, x30_actividades_realizadas_en_clase), 
    x35_aplicacion_del_enfoque_por_competencias_por_parte_del_docente = coalesce(
      x35_aplicacion_del_enfoque_por_competencias_por_parte_del_docente,
      x31_aplicacion_del_enfoque_por_competencias_por_parte_del_docente), 
    x36_enfoque_o_metodologia_para_el_desarrollo_del_area = coalesce(
      x36_enfoque_o_metodologia_para_el_desarrollo_del_area, x32_enfoque_o_metodologia_para_el_desarrollo_del_area), 
    x37_uso_de_estrategias_de_ensenanza_aplicadas_al_area_en_general_a_las_competencias_o_al_desarrollo_de_los_contenidos = coalesce(
      x37_uso_de_estrategias_de_ensenanza_aplicadas_al_area_en_general_a_las_competencias_o_al_desarrollo_de_los_contenidos, 
      x33_uso_de_estrategias_de_ensenanza_aplicadas_al_area_en_general_a_las_competencias_o_al_desarrollo_de_los_contenidos), 
    x38_contenido_si_es_propio_del_area_corresponde_al_cnb_si_es_actualizado = coalesce(
      x38_contenido_si_es_propio_del_area_corresponde_al_cnb_si_es_actualizado, x34_contenido_si_es_propio_del_area_corresponde_al_cnb_si_es_actualizado), 
    x39_desarrollo_del_contenido_por_parte_del_docente_dominio_precision_claridad_etc = coalesce(
      x39_desarrollo_del_contenido_por_parte_del_docente_dominio_precision_claridad_etc, 
      x35_desarrollo_del_contenido_por_parte_del_docente_dominio_precision_claridad_etc), 
    x40_gestion_del_aula_por_parte_del_docente_el_espacio_la_disciplina_la_resolucion_de_conflictos_la_participacion_rutinas_etc = coalesce(
      x40_gestion_del_aula_por_parte_del_docente_el_espacio_la_disciplina_la_resolucion_de_conflictos_la_participacion_rutinas_etc, 
      x36_gestion_del_aula_por_parte_del_docente_el_espacio_la_disciplina_la_resolucion_de_conflictos_la_participacion_rutinas_etc), 
    x41_interaccion_o_comunicacion_docente_estudiantes_y_estudiantes_estudiantes = coalesce(
      x41_interaccion_o_comunicacion_docente_estudiantes_y_estudiantes_estudiantes,
      x37_interaccion_o_comunicacion_docente_estudiantes_y_estudiantes_estudiantes), 
    x45_asignacion_de_tareas_o_actividades_de_aprendizaje_para_realizar_en_casa_o_durante_clase = coalesce(
      x45_asignacion_de_tareas_o_actividades_de_aprendizaje_para_realizar_en_casa_o_durante_clase, 
      x39_asignacion_de_tareas_o_actividades_de_aprendizaje_para_realizar_en_casa_o_durante_clase), 
    x46_actividades_o_acciones_de_evaluacion_sumativa_o_formativa = coalesce(
      x46_actividades_o_acciones_de_evaluacion_sumativa_o_formativa, x40_actividades_o_acciones_de_evaluacion_sumativa_o_formativa), 
    x49_solicitud_del_uso_de_cuadernos_hojas_o_materiales = coalesce(
      x49_solicitud_del_uso_de_cuadernos_hojas_o_materiales, x42_solicitud_del_uso_de_cuadernos_hojas_o_materiales), 
    x50_uso_del_pizarron_presentacion_u_otra_forma_para_presentar_informacion_a_los_estudiantes = coalesce(
      x50_uso_del_pizarron_presentacion_u_otra_forma_para_presentar_informacion_a_los_estudiantes, 
      x43_uso_del_pizarron_presentacion_u_otra_forma_para_presentar_informacion_a_los_estudiantes), 
    x52_uso_de_recursos_didacticos_para_la_ensenanza = coalesce(
      x52_uso_de_recursos_didacticos_para_la_ensenanza, x45_uso_de_recursos_didacticos_para_la_ensenanza), 
    x53_verificacion_de_la_comprension_de_los_estudiantes_por_parte_del_docente = coalesce(
      x53_verificacion_de_la_comprension_de_los_estudiantes_por_parte_del_docente, 
      x46_verificacion_de_la_comprension_de_los_estudiantes_por_parte_del_docente), 
    x54_clarificacion_de_dudas_de_los_estudiantes_por_parte_del_docente = coalesce(
      x54_clarificacion_de_dudas_de_los_estudiantes_por_parte_del_docente, 
      x47_clarificacion_de_dudas_de_los_estudiantes_por_parte_del_docente), 
    x55_retroalimentacion_del_aprendizaje = coalesce(
      x55_retroalimentacion_del_aprendizaje, x48_retroalimentacion_del_aprendizaje), 
    x56_preparacion_instrucciones_o_recordatorios_para_la_siguiente_clase = coalesce(
      x56_preparacion_instrucciones_o_recordatorios_para_la_siguiente_clase, 
      x49_preparacion_instrucciones_o_recordatorios_para_la_siguiente_clase), 
    x76_hubo_algun_incidente_durante_la_observacion_de_este_periodo_describalo_en_este_espacio = coalesce(
      x76_hubo_algun_incidente_durante_la_observacion_de_este_periodo_describalo_en_este_espacio, 
      x66_hubo_algun_incidente_durante_la_observacion_de_este_periodo_describalo_en_este_espacio), 
    x77_en_este_espacio_puede_escribir_comentarios_adicionales_que_considere_relevantes_sobre_lo_sucedido_en_este_periodo_de_clase = coalesce(
      x77_en_este_espacio_puede_escribir_comentarios_adicionales_que_considere_relevantes_sobre_lo_sucedido_en_este_periodo_de_clase, 
      x67_en_este_espacio_puede_escribir_comentarios_adicionales_que_considere_relevantes_sobre_lo_sucedido_en_este_periodo_de_clase))

# seleccionar las variables oficiales de df_dyd

rango_vars <- c(match("start",colnames(obs03)):match("georeferenciacion_precision",colnames(obs03)),
                match("id",colnames(obs03)):match("index",colnames(obs03)))


obs03 <- obs03 |> 
  select(all_of(rango_vars))


# leer archivo colnames con nombres de variables oficiales y asignarlos

cols_df_obs03 <- read_xlsx(
  here("datasets para procesamiento", "colnames evaluacion curricular basico.xlsx"),
  col_names = T
)[[6]] |> 
  na.omit() |> 
  as.vector()

colnames(obs03) <- cols_df_obs03

# exportar df_dyd para la siguiente fase de limpieza

write_xlsx(obs03, here("Datos","Observación especialistas","obs03.xlsx"))
