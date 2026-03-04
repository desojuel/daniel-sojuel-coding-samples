library(tidyverse)
library(janitor)
library(readxl)
library(writexl)
library(crosstable)
library(stringr)

df <- read_xlsx("export-15874160.xlsx")

# cambiar nombres a una sola variable

names(df)[names(df) == "Declaración de consentimiento: He leído el documento de consentimiento informado que me ha sido entregado, he comprendido las explicaciones en él facilitadas."] <- "consentimiento"

names(df)[names(df) == "Carné"] <- "carnet"

names(df)[names(df) == "Según su contrato o condiciones laborales, ¿cuántas horas trabaja al día?"] <- "horas_trabajo_al_dia"

names(df)[names(df) == "¿Aproximadamente cuántas horas diarias dedica en promedio a su trabajo fuera de la jornada laboral [transporte, actividad pendiente, hora extra en el trabajo]?"] <- "tiempo_fuera_joranda_laboral"

names(df)[names(df) == "Otro (escriba en números las horas):¿Aproximadamente cuántas horas diarias dedica en promedio a su trabajo fuera de la jornada laboral [transporte, actividad pendiente, hora extra en el trabajo]?"] <- "otro_tiempo_fuera_joranda_laboral"

# comunidad linguistica ----

df <- df |> 
  rename(com_li_poqomchi = "Poqomchi’:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_achi = "Achi’:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_queqchi = "Q'eqchi':¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_chorti = "Ch’orti’:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_kaqchikel = "Kaqchikel:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_poqomam = "Poqomam:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_sipakapense = "Sipakapense:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_tzutujil = "Tz’utujil:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_mam = "Mam:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_ixil = "Ixil:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_sakapulteko = "Sakapulteko:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_uspanteko = "Uspanteko:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_awakateko = "Awakateko:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_chalchiteko = "Chalchiteko:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_akateko = "Akateko:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_chuj = "Chuj:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_jakalteko_popti = "Jakalteko/Popti':¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_qanjobal = "Q’anjob’al:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_tektiteko = "Tektiteko:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_kiche = "K´iche´:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_itza = "Itza’:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_mopan = "Mopan:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_garifuna = "Garífuna:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_xinca = "Xinca:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)",
         com_li_espanol = "Español:¿Cuál es su comunidad lingüistica? (Esta respuesta puede basarse en su autoidentificación con alguna comunidad, aunque usted no hable el idioma, o en que alguno de estos idiomas sea su lengua materna. Puede selecionar más de una opción.)")

# cambios ----

df <- df |> 
  rename(cambios_residencia = "¿Cambió de residencia?",
         cambios_residencia_periodo = "Cambio de residencia: ¿En qué periodo de 2024 tuvo este cambio?",
         cambios_ascenso = "¿Durante el último año recibió un ascenso o aumento de salario?",
         cambios_ascenso_periodo = "Ascenso o aumento de salario: ¿En qué periodo de 2024 tuvo este cambio?",
         cambios_empleo = "¿Cambió de empleo?",
         cambios_empleo_periodo = "Cambio de empleo: ¿En qué periodo de 2024 tuvo este cambio?",
         cambios_casamiento_union = "¿Se casó o se unió?",
         cambios_casamiento_union_periodo = "Se casó o se unió: ¿En qué periodo de 2024 tuvo este cambio?",
         cambios_embarazo = "¿Usted o su pareja se embarazaron o tuvo hijo/a?",
         cambios_embarazo_periodo = "Embarazo o nacimiento: ¿En qué periodo de 2024 tuvo este cambio?",
         cambios_separacion_divorcio = "¿Se separó o divorció?",
         cambios_separacion_divorcio_periodo = "Separación o divorcio: ¿En qué periodo de 2024 tuvo este cambio?",
         cambios_diagnostico_salud_mental = "¿Recibió algún diagnóstico por parte de un profesional de la salud mental? (depresión, ansiedad, etc.)",
         cambios_diagnostico_salud_mental_periodo = "Diagnóstico de salud mental: ¿En qué periodo de 2024 tuvo este cambio?",
         cambios_tratamiento_psiquiatrico = "¿Llevó a cabo algún tratamiento psiquiátrico?",
         cambios_tratamiento_psiquiatrico_periodo = "Tratamiento psiquiátrico: ¿En qué periodo de 2024 tuvo este cambio?",
         cambios_enfermedad_grave = "¿Afrontó alguna enfermedad grave?",
         cambios_que_enfermedad = "¿Qué enfermedad?",
         cambios_enfermedad_grave_periodo = "Enfermedad grave: ¿En qué periodo de 2024 tuvo este cambio?",
         cambios_cirugia = "¿Le realizaron alguna cirugía?",
         cambios_que_cirugia = "¿Qué cirugía?",
         cambios_cirugia_periodo = "Cirugía: ¿En qué periodo de 2024 tuvo este cambio?",
         cambios_duelo_perdida = "¿Afrontó un duelo o pérdida significativa? (seres queridos, mascotas, proyectos, negocios)",
         cambios_duelo_perdida_periodo = "Duelo o pérdida significativa: ¿En qué periodo de 2024 tuvo este cambio?",
         cambios_familiar_enfermedad = "¿Algún familiar se enfermó de gravedad?",
         cambios_familiar_enfermedad_periodo = "Familiar enfermo de gravedad: ¿En qué periodo de 2024 tuvo este cambio?",
         cambios_conflicto_interpersonal = "¿Tuvo algún conflicto interpersonal significativo (con familiares o personas cercanas)?",
         cambios_conflicto_interpersonal_periodo = "Conflicto interpersonal: ¿En qué periodo de 2024 tuvo este cambio?",
         cambios_proyecto_personal_academico = "¿Inició algún proyecto personal o académico? (emprendimiento, inicio de estudios)",
         cambios_proyecto_personal_academico_periodo = "Proyecto personal o académico ¿En qué periodo de 2024 tuvo este cambio?",
         cambios_otro = "Si hubo otro cambio significativo durante el último/presente año, escríbalo aquí e indique la fecha.")

# discapacidad ----

df <- df |> 
  rename(dificultad_ver = "¿Tiene dificultad para ver, incluso cuando usa lentes?",
         dificultad_oir = "¿Tiene dificultad oír, incluso cuando usa un aparato auditivo?",
         dificultad_caminar = "¿Tiene dificultad para caminar o subir escaleras?",
         dificultad_recordar = "¿Tiene dificultad para recordar o concentrarse?",
         dificultad_vestirse = "¿Tiene dificultad para para valerse por sí mismo/a, como lavarse el cuerpo o vestirse?",
         dificultad_comunicarse = "Cuando emplea su lenguaje habitual, ¿tiene dificultad para comunicarse, por ejemplo, para entender a los demás o para que le entiendan?")


df <- clean_names(df)

names(df)[names(df) == "si_hubo_otro_cambio_significativo_durante_el_ultimo_presente_ano_escribalo_aqui_e_indique_la_fecha"] <- "otro_cambio_significativo"

names(df)[names(df) == "si_hubo_otro_cambio_significativo_durante_el_ultimo_presente_ano_escribalo_aqui_e_indique_la_fecha"] <- "otro_cambio_significativo"


names(df)[names(df) == "en_que_ano_inicio_sus_estudios_en_la_escuela_de_ciencias_psicologicas"] <- "cohorte"

names(df)[names(df) == "en_2024_participo_en_la_recoleccion_de_datos_para_un_estudio_similar_al_presente_en_donde_se_le_explico_que_tendria_seguimiento_durante_la_carrera"] <- "participacion_2024"


names(df)[names(df) == "aproximadamente_cuantas_horas_diarias_en_promedio_dedica_al_ocio_actividades_recreativas_de_diversion_o_descanso"] <- "horas_ocio"

names(df)[names(df) == "otro_escriba_en_numeros_las_horas_aproximadamente_cuantas_horas_diarias_en_promedio_dedica_al_ocio_actividades_recreativas_de_diversion_o_descanso"] <- "otro_horas_ocio"

names(df)[names(df) == "aproximadamente_cuantas_horas_en_promedio_dedica_a_la_semana_a_hacer_ejercicio"] <- "horas_semana_ejercicio"

names(df)[names(df) == "otro_escriba_en_numeros_las_horas_aproximadamente_cuantas_horas_en_promedio_dedica_a_la_semana_a_hacer_ejercicio"] <- "otro_horas_semana_ejercicio"

names(df)[names(df) == "cual_es_el_medio_de_transporte_principal_que_utiliza_para_ir_a_la_universidad"] <- "medio_trasporte_universidad"

names(df)[names(df) == "cual_es_el_medio_de_transporte_principal_que_utiliza_para_ir_a_la_universidad"] <- "medio_trasporte_universidad"

names(df)[names(df) == "otro_especifique_cual_es_el_medio_de_transporte_principal_que_utiliza_para_ir_a_la_universidad"] <- "otro_medio_trasporte_universidad"


names(df)[names(df) == "seleccione_su_rango_de_ingresos_personal"] <- "rango_ingresos_personal"

names(df)[names(df) == "¿Tiene dependientes económicos? (debe proveer los recursos económicos para atender las necesidades de otras personas)"] <- "dependientes_economicos"

names(df)[names(df) == "seleccione_el_rango_de_ingresos_de_su_familia"] <- "rango_ingresos_familia"

names(df)[names(df) == "respecto_a_sus_ingresos_y_gastos_seleccione_la_respuesta_mas_acertada_sobre_su_situacion"] <- "percepcion_situacion_economica"

names(df)[names(df) == "cuantos_anos_tenia_cuando_empezo_a_trabajar"] <- "edad_de_inicio_trabajo"


names(df)[names(df) == "afronto_un_duelo_o_perdida_significativa_seres_queridos_mascotas_proyectos_negocios"] <- "duelo_perdida"

## enfermedades

df <- df |> 
  rename(enfermedad_diabetes = diabetes_cual_cuales_de_las_siguientes_enfermedades_padece,
         enfermedad_asma = asma_cual_cuales_de_las_siguientes_enfermedades_padece,
         enfermedad_cardiopatias = cardiopatias_cual_cuales_de_las_siguientes_enfermedades_padece,
         enfermedad_artritis = artritis_cual_cuales_de_las_siguientes_enfermedades_padece,
         enfermedad_esclerosis = esclerosis_multiple_cual_cuales_de_las_siguientes_enfermedades_padece,
         enfermedad_endometriosis = endometriosis_cual_cuales_de_las_siguientes_enfermedades_padece,
         enfermedad_cancer = cancer_cual_cuales_de_las_siguientes_enfermedades_padece,
         enfermedad_hipertension = hipertension_cual_cuales_de_las_siguientes_enfermedades_padece,
         enfermedad_vih = vih_sida_cual_cuales_de_las_siguientes_enfermedades_padece,
         enfermedad_migranas = migranas_cual_cuales_de_las_siguientes_enfermedades_padece,
         enfermedad_renal = enfermedad_renal_cual_cuales_de_las_siguientes_enfermedades_padece,
         enfermedad_gastrointestinal = enfermedad_gastrointestinal_cual_cuales_de_las_siguientes_enfermedades_padece,
         enfermedad_gastritis = gastritis_cual_cuales_de_las_siguientes_enfermedades_padece,
         enfermedad_hipo_hipertiroidismo = hipo_o_hipertiroidismo_cual_cuales_de_las_siguientes_enfermedades_padece,
         enfermedad_hipo_hipertension = hipo_o_hipertension_cual_cuales_de_las_siguientes_enfermedades_padece,
         enfermedad_plastron = plastron_apendicular_cual_cuales_de_las_siguientes_enfermedades_padece,
         enfermedad_otra = otra_cual_cuales_de_las_siguientes_enfermedades_padece_106,
         enfermedad_otra_cual = otra_cual_cuales_de_las_siguientes_enfermedades_padece_107)

## rename con quienes vive

df <- df |> 
  rename(con_quien_vive_papa = papa_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_pareja_papa = pareja_de_mi_papa_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_mama = mama_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_pareja_mama = pareja_de_mi_mama_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_hermanos = hermano_s_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_hermanas = hermana_s_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_abuelo_materno = abuelo_materno_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_abuela_materna = abuela_materna_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_abuelo_paterno = abuelo_paterno_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_abuela_paterna = abuela_paterna_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_tios = tio_s_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_tias = tia_s_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_primos = primo_s_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_primas = prima_s_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_pareja = pareja_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_hijos = hijo_s_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_hijas = hija_s_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_amigos = amigo_s_o_no_familiares_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_amigas = amiga_s_o_no_familiares_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_mascotas = mascota_s_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_solx = solo_a_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen,
con_quien_vive_otro = otro_especifique_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen_149,
con_quien_vive_otro_cual = otro_especifique_con_quienes_vive_seleccione_todos_las_opciones_que_apliquen_150)

# otras variables con nombre largo

df <- df |> 
  rename(cursos_asignados_primer_semestre = "primer_semestre_en_que_semestre_tiene_cursos_asignados",
         cursos_asignados_tercer_semestre = "tercer_semestre_en_que_semestre_tiene_cursos_asignados",
         cursos_asignados_quinto_semestre = "quinto_semestre_en_que_semestre_tiene_cursos_asignados",
         carrera_diversificado = "de_cual_de_las_siguientes_carreras_se_graduo_usted_en_diversificado",
         carrera_diversificado_otro = "otro_de_cual_de_las_siguientes_carreras_se_graduo_usted_en_diversificado",
         ano_graduacion_diversificado = "en_que_ano_se_graduo_de_diversificado",
         sector_diversificado = "de_que_sector_es_el_colegio_o_instituto_del_que_se_graduo_de_diversificado",
         actualmente_estudiando_otra_carrera = "esta_estudiando_actualmente_otra_carrera_ademas_de_la_que_estudia_en_la_escuela_de_ciencias_psicologicas",
         actualmente_estudiando_otra_carrera_cual = "si_escriba_cual_en_el_espacio_en_blanco_esta_estudiando_actualmente_otra_carrera_ademas_de_la_que_estudia_en_la_escuela_de_ciencias_psicologicas",
         antes_otra_carrera = "estudio_otra_carrera_antes_de_entrar_a_la_escuela_de_ciencias_psicologicas_completa_o_incompleta",
         antes_otra_carrera_cual = "que_otra_carrera_estudio_anteriormente",
         antes_otra_carrera_san_carlos = "la_carrera_que_estudio_anteriormente_fue_en_la_universidad_de_san_carlos_de_guatemala",
         experiencias_psicoterapia = "con_respecto_a_sus_experiencias_con_la_psicoterapia_seleccione_la_opcion_que_mejor_describa_su_situacion",
         consumo_de_sustancias = "con_que_frecuencia_consume_sustancias_psicoactivas_por_ejemplo_alcohol_tabaco_u_otras_sustancias_psicoactivas",
         idiomas_espanol = "espanol_seleccione_los_idiomas_que_habla",
         idiomas_ingles = "ingles_seleccione_los_idiomas_que_habla",
         idiomas_maya = "idioma_maya_indique_cual_seleccione_los_idiomas_que_habla_111",
         idiomas_otro = "otro_indique_cual_seleccione_los_idiomas_que_habla_112",
         idiomas_maya_cual = "idioma_maya_indique_cual_seleccione_los_idiomas_que_habla_113",
         idiomas_otro_cual = "otro_indique_cual_seleccione_los_idiomas_que_habla_114",
         practica_una_religion = "sobre_la_religion_marque_la_opcion_que_mejor_describe_su_situacion",
         religiosidad = "con_cual_de_las_siguientes_opciones_se_identifica",
         migracion_ultimo_ano = "usted_migro_cambio_de_municipio_o_departamento_de_residencia_durante_el_ultimo_ano",
         motivo_migracion_ultimo_ano = "cual_fue_el_motivo_del_cambio_de_municipio_durante_el_ultimo_ano",
         motivo_migracion_ultimo_ano_otro = "otro_especifique_cual_fue_el_motivo_del_cambio_de_municipio_durante_el_ultimo_ano",
         mas_lejos_cerca_universidad = "ahora_vive_mas_lejos_o_mas_cerca_de_la_universidad",
         departamento_diferente_que_antes = "vive_en_un_departamento_diferente_que_antes",
         departamento_diferente_que_antes_especifique = "si_especifique_el_cambio_vive_en_un_departamento_diferente_que_antes",
         posicion_politica = "cual_de_las_siguientes_opciones_describe_mejor_su_posicion_politica",
         posicion_politica_otra = "otra_cual_de_las_siguientes_opciones_describe_mejor_su_posicion_politica",
         
         escolaridad_madre = "cual_es_la_escolaridad_de_su_madre",
         laborando_madre = "su_madre_esta_laborando_actualmente",
         escolaridad_padre = "cual_es_la_escolaridad_de_su_padre",
         laborando_padre = "su_padre_esta_laborando_actualmente",
         dinamica_lugar_residencia = "como_es_la_dinamica_en_su_lugar_de_residencia",
         actualmente_empleo_remunerado = "actualmente_tiene_un_empleo_remunerado",
         mas_de_un_empleo = "tiene_mas_de_un_empleo",
         numero_de_empleos = "cuantos_empleos_tiene",
         numero_de_empleos_otro = "otro_cuantos_empleos_tiene",
         area_empleo = "a_que_area_corresponde_su_empleo_o_trabajo",
         puesto_actividad_trabajo = "cual_es_su_puesto_o_actividad_principal_en_su_trabajo",
         trabajos_no_remunerados = "actualmente_realiza_trabajos_no_remunerados_como_voluntariado_o_trabajo_domestico",
         trabajos_no_remunerados_cual = "que_tipo_de_trabajo_no_remunerado_realiza",
         actualmente_buscando_empleo = "actualmente_esta_buscando_activamente_un_empleo",
         meses_buscando_empleo = "cuantos_meses_lleva_buscando_empleo",
         es_dependiente_economico = "es_usted_dependiente_economico_recibe_de_alguien_mas_los_recursos_economicos_para_sus_necesidades",
         tiene_dependientes_economicos = "tiene_dependientes_economicos_debe_proveer_los_recursos_economicos_para_atender_las_necesidades_de_otras_personas",
         equipo_de_computo = "el_equipo_de_computo_con_el_que_estudia_es")


### fecha de inicio ---- 

# Convert string to date format
df <- df |> 
  mutate(fecha = as.Date(strptime(time_started, format = "%b %d, %Y"))) |> 
  filter(fecha >= as.POSIXct("2025-03-03")) |> 
  select(-fecha)

### sin consentimiento ---- 

pre_limpieza <- nrow(df)

df <- df |> 
  filter(consentimiento != "No doy mi consentimiento de mi participación.")

post_limpieza <- nrow(df)
sin_consentimiento <- pre_limpieza - post_limpieza

### sin carnet ---- 

pre_limpieza <- nrow(df)

df <- df |> 
  filter(carnet != is.na(carnet))

post_limpieza <- nrow(df)
sin_carnet <- pre_limpieza - post_limpieza

### no es de primer ingreso ---- 

pre_limpieza <- nrow(df)

df <- df |> 
  filter(cohorte != "Otro", #se van todos los que sean de otra cohorte que no sea 2024 y 2025
        participacion_2024 != "No" | # se van todos los que sean de 2024, pero no participaron con nosotros el 2024 ó
        cohorte == "2025" # se quedan todos los que sean de 2025
        )

participacion <- crosstable(df, cohorte, by = participacion_2024)

post_limpieza <- nrow(df)
no_primer_ingreso <- pre_limpieza - post_limpieza

### carnet duplicado ---- 

pre_limpieza <- nrow(df)

df <- df |> 
  distinct(carnet, .keep_all = TRUE)

post_limpieza <- nrow(df)
n_duplicado <- pre_limpieza - post_limpieza

# para identificar carnets
carnets <- df$carnet
write_xlsx(as.data.frame(carnets), "Carnets 2025.xlsx")

# arreglar fecha nacimiento  ----

df <- df |> 
  mutate(fecha_de_nacimiento = as.Date(fecha_de_nacimiento, format = "%m/%d/%Y"))  |> 
  mutate(
    dias_diferencia = as.numeric(difftime(as.Date("2025-04-11"), fecha_de_nacimiento, units = "days")),
    edad_11_abril_25 = floor(dias_diferencia / 365.25)
  ) %>%
  # 3) (Opcional) eliminar la columna auxiliar de días
  select(-dias_diferencia) |> 
  relocate(edad_11_abril_25, .after = fecha_de_nacimiento)

# df limpia
write_xlsx(df, "df_2025_limpia_1.xlsx")