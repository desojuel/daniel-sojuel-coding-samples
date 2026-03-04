df_obs01 <- read_xlsx(here("Datos","Listos para análisis","Observacion a nivel de centros","obs01.xlsx"))
df_obs01r2 <- read_xlsx(here("Datos","Observación a nivel de centro educativo","observacion centro 18.11.25.xlsx"),
                        sheet = 2)
df_obs01r3 <- read_xlsx(here("Datos","Observación a nivel de centro educativo","observacion centro 18.11.25.xlsx"),
                        sheet = 3)

# Sección 2. Información del centro educativo ---- 
## Frecuencias y Porcentaje de la variable Sector 
sector_obs01 <- df_obs01 |> 
  make_freq_table(sector,"Sector")

## Frecuencias y Porcentaje de la variable Modalidad 
modalidad_obs01 <- df_obs01 |> 
  filter(sector == "Oficial") |>
  make_freq_table(modalidad,"Modalidad")

## Frecuencias y Porcentaje de la variable Plan 
plan_obs01 <- df_obs01 |> 
  make_freq_table(plan,"Plan")

## Frecuencias y Porcentaje de la variable Jornada 
jornada_obs01 <- df_obs01 |> 
  make_freq_table(jornada,"Jornada")

## Frecuencias y Porcentaje de la variable Población 
poblacion_obs01 <- df_obs01 |> 
  make_freq_table(poblacion,"Población")

## Frecuencias y Porcentaje de la variable Departamento 
departamento_obs01 <- df_obs01 |> 
  make_freq_table(departamento,"Departamento")

## output list
output_list2 <- list(
  "Sector" = sector_obs01,
  "Modalidad" = modalidad_obs01,
  "Plan" = plan_obs01,
  "Jornada" = jornada_obs01,
  "Población" = poblacion_obs01,
  "Departamento" = departamento_obs01)


# Sección 3. Energía eléctrica ---- 

## Frecuencias y Porcentaje de la variable Fuente de energía eléctrica del Centro Educativo
energia_fuente_centro_obs01 <- df_obs01 |> 
  make_freq_table(energia_fuente_centro,"Fuente de energía eléctrica del Centro Educativo")

## Frecuencias y Porcentaje de la variable Frecuencia de fallas eléctricas
energia_frecuencia_fallas_electricas_obs01 <- df_obs01 |> 
  filter(energia_otra_fuente_centro != "El Centro Educativo no cuenta con ninguna fuente de energía") |>
  make_freq_table(energia_frecuencia_fallas_electricas,"Frecuencia de fallas eléctricas")

## Frecuencias y Porcentaje de la variable Duración de fallas eléctricas
energia_duracion_fallas_electricas_obs01 <- df_obs01 |> 
  filter(energia_frecuencia_fallas_electricas != "Casi nunca fallas eléctricas") |>
  make_freq_table(energia_duracion_fallas_electricas,"Duración de fallas eléctricas")

## Frecuencias y Porcentaje de la variable Centro Educativo cuenta con energía eléctrica al momento de la visita
energia_momento_visita_obs01 <- df_obs01 |> 
  filter(energia_fuente_centro != "El Centro Educativo no cuenta con ninguna fuente de energía") |>
  make_freq_table(energia_momento_visita,"Centro Educativo cuenta con energía eléctrica al momento de la visita")

## Frecuencias y Porcentaje de la variable Tiempo aproximado del Centro Educativo sin energía eléctrica
energia_tiempo_aproximado_sin_energia_obs01 <- df_obs01 |> 
  filter(energia_momento_visita == "No") |>
  make_freq_table(energia_tiempo_aproximado_sin_energia,"Tiempo aproximado del Centro Educativo sin energía eléctrica")

## Frecuencias y Porcentaje de la variable Razón de que Centro Educativo nunca haya tenido energía eléctrica
energia_razon_nunca_obs01 <- df_obs01 |> 
  filter(energia_tiempo_aproximado_sin_energia == "El Centro Educativo nunca ha tenido energía eléctrica" ,
         energia_momento_visita == "No") |>
  make_freq_table(energia_razon_nunca,"Razón de que Centro Educativo nunca haya tenido energía eléctrica")

## Frecuencias y Porcentaje de la variable Razón por la que Centro Educativo no cuenta con energía eléctrica
energia_razon_centro_no_cuenta_obs01 <- df_obs01 |> 
  filter(energia_momento_visita == "No" ,
         energia_tiempo_aproximado_sin_energia != "El Centro Educativo nunca ha tenido energía eléctrica") |>
  make_freq_table(energia_razon_centro_no_cuenta,"Razón por la que Centro Educativo no cuenta con energía eléctrica")

## output list
output_list3 <- list(
  "Fuente de energía eléctrica del Centro Educativo" = energia_fuente_centro_obs01,
  "Frecuencia de fallas eléctricas" = energia_frecuencia_fallas_electricas_obs01,
  "Duración de fallas eléctricas" = energia_duracion_fallas_electricas_obs01,
  "Centro Educativo cuenta con energía eléctrica al momento de la visita" = energia_momento_visita_obs01,
  "Tiempo aproximado del Centro Educativo sin energía eléctrica" = energia_tiempo_aproximado_sin_energia_obs01,
  "Razón de que Centro Educativo nunca haya tenido energía eléctrica" = energia_razon_nunca_obs01, 
  "Razón por la que Centro Educativo no cuenta con energía eléctrica" = energia_razon_centro_no_cuenta_obs01)


## Sección 4. Agua y servicios sanitarios ----
## Frecuencias y Porcentaje de la variable El Centro Educativo cuenta con agua
agua_centro_obs01 <- df_obs01 |> 
  make_freq_table(agua_centro,"El Centro Educativo cuenta con agua")

## Frecuencias y Porcentaje de la variable Fuente de agua del Centro Educativo
fuente_agua_centro_obs01 <- df_obs01 |> 
  filter(agua_centro == "Sí") |>
  select_multiple(fuente_agua_centro,"Fuente de agua del Centro Educativo")

## Frecuencias y Porcentaje de la variable Centro Educativo tiene agua el día de la visita
agua_centro_dia_visita_obs01 <- df_obs01 |> 
  filter(agua_centro == "Sí") |>
  make_freq_table(agua_centro_dia_visita,"Centro Educativo tiene agua el día de la visita")

## Frecuencias y Porcentaje de la variable Tiempo que lleva el Centro Educativo sin agua
agua_tiempo_centro_obs01 <- df_obs01 |> 
  filter(agua_centro == "No") |>
  make_freq_table(agua_tiempo_centro,"Tiempo que lleva el Centro Educativo sin agua")

## Frecuencias y Porcentaje de la variable El agua del Centro Educativo es potable
agua_centro_segura_obs01 <- df_obs01 |> 
  filter(agua_centro == "Sí") |>
  make_freq_table(agua_centro_segura,"El agua del Centro Educativo es potable")

## Frecuencias y Porcentaje de la variable Regularidad del agua disponible durante jornada escolar
agua_centro_disponible_jornada_obs01 <- df_obs01 |> 
  filter(agua_centro == "Sí") |>
  make_freq_table(agua_centro_disponible_jornada,"Regularidad del agua disponible durante jornada escolar")

## Frecuencias y Porcentaje de la variable Centro Educativo tiene baños para estudiantes
banos_centro_obs01 <- df_obs01 |> 
  make_freq_table(banos_centro,"Centro Educativo tiene baños para estudiantes")

## Frecuencias y Porcentaje de la variable Baños exclusivos para estudiantes hombres
banos_centro_exclusivos_hombres_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí") |>
  make_freq_table(banos_centro_exclusivos_hombres,"Baños exclusivos para estudiantes hombres")

## Frecuencias y Porcentaje de la variable Baños exclusivos para estudiantes mujeres
banos_centro_exclusivos_mujeres_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí") |>
  make_freq_table(banos_centro_exclusivos_mujeres,"Baños exclusivos para estudiantes mujeres")

## Frecuencias y Porcentaje de la variable Baños para estudiantes son mixtos
banos_centro_mixtos_obs01 <- df_obs01 |> 
  filter(banos_centro_exclusivos_hombres != "Sí", 
         banos_centro_exclusivos_mujeres != "Sí",
         poblacion == "Mixto", 
         banos_centro == "Sí") |>
  make_freq_table(banos_centro_mixtos,"Baños para estudiantes son mixtos")

## Frecuencias y Porcentaje de la variable Ubicación de baños para estudiantes
banos_ubicacion_centro_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí") |>
  make_freq_table(banos_ubicacion_centro,"Ubicación de baños para estudiantes")

## Frecuencias y Porcentaje de la variable Tipo de instalación sanitaria
banos_tipo_instalacion_sanitaria_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí") |>
  make_freq_table(banos_tipo_instalacion_sanitaria,"Tipo de instalación sanitaria")

## Frecuencias y Porcentaje de la variable Docente tienen baños exclusivos
banos_centro_exclusivos_docentes_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí") |>
  make_freq_table(banos_centro_exclusivos_docentes,"Docente tienen baños exclusivos")

## Frecuencias y Porcentaje de la variable Cantidad de baños para estudiantes hombres 
banos_hombres_cantidad_obs01 <- df_obs01 |> 
  tabla_resumen(banos_hombres_cantidad)

## Frecuencias y Porcentaje de la variable Baños cuentan con mingitorios
banos_hombres_mingitorios_obs01 <- df_obs01 |> 
  make_freq_table(banos_hombres_mingitorios,"Baños cuentan con mingitorios")

## Frecuencias y Porcentaje de la variable Cantidad de inodoros o letrinas en baño de estudiantes hombres
banos_hombres_cantidad_inodoros_obs01 <- df_obs01 |> 
  filter(banos_tipo_instalacion_sanitaria != "Otro", 
         banos_tipo_instalacion_sanitaria != '') |>
  tabla_resumen(banos_hombres_cantidad_inodoros)

## Frecuencias y Porcentaje de la variable Cantidad de inodoros o letrinas en baño de estudiantes hombres utilizables
banos_hombres_cantidad_inodoros_utilizables_obs01 <- df_obs01 |> 
  filter(banos_hombres_cantidad_inodoros != '') |>
  tabla_resumen(banos_hombres_cantidad_inodoros_utilizables)

## Frecuencias y Porcentaje de la variable Inodoros o letrinas en baño de estudiantes hombres en mal estado
banos_hombres_inodoros_mal_estado_obs01 <- df_obs01 |> 
  filter(banos_tipo_instalacion_sanitaria != "Otro", 
         banos_tipo_instalacion_sanitaria != '') |>
  make_freq_table(banos_hombres_inodoros_mal_estado,"Inodoros o letrinas en baño de estudiantes hombres en mal estado")

## Frecuencias y Porcentaje de la variable Inodoros o letrinas en baño de estudiantes hombres tienen cubículo individual
banos_hombres_cubiculo_individual_inodoros_obs01 <- df_obs01 |> 
  filter(banos_tipo_instalacion_sanitaria != "Otro", 
         banos_tipo_instalacion_sanitaria != '') |>
  make_freq_table(banos_hombres_cubiculo_individual_inodoros,"Inodoros o letrinas en baño de estudiantes hombres tienen cubículo individual")

## Frecuencias y Porcentaje de la variable Cada cubículo en baño de estudiantes hombres tiene basurero
banos_hombres_recipiente_basura_obs01 <- df_obs01 |> 
  filter(banos_hombres_cubiculo_individual_inodoros == "Sí") |>
  make_freq_table(banos_hombres_recipiente_basura,"Cada cubículo en baño de estudiantes hombres tiene basurero")

## Frecuencias y Porcentaje de la variable Recursos en baño de estudiantes hombres
banos_hombres_recursos_obs01 <- df_obs01 |> 
  select_multiple(banos_hombres_recursos,"Recursos en baño de estudiantes hombres")

## Frecuencias y Porcentaje de la variable Cantidad de baños para estudiantes mujeres
banos_mujeres_cantidad_obs01 <- df_obs01 |> 
  tabla_resumen(banos_mujeres_cantidad)

## Frecuencias y Porcentaje de la variable Cantidad de inodoros o letrinas en baño de estudiantes mujeres
banos_mujeres_cantidad_inodoros_obs01 <- df_obs01 |> 
  filter(banos_tipo_instalacion_sanitaria != "Otro", 
               banos_tipo_instalacion_sanitaria != '') |>
  tabla_resumen(banos_mujeres_cantidad_inodoros)

## Frecuencias y Porcentaje de la variable Cantidad de inodoros o letrinas en baño de estudiantes mujeres utilizables
banos_mujeres_cantidad_inodoros_utilizables_obs01 <- df_obs01 |> 
  filter(banos_tipo_instalacion_sanitaria != "Otro", 
         banos_tipo_instalacion_sanitaria != '') |>
  tabla_resumen(banos_mujeres_cantidad_inodoros_utilizables)

## Frecuencias y Porcentaje de la variable Inodoros o letrinas en baño de estudiantes mujeres en mal estado
banos_mujeres_inodoros_mal_estado_obs01 <- df_obs01 |> 
  filter(banos_tipo_instalacion_sanitaria != "Otro", 
         banos_tipo_instalacion_sanitaria != '') |>
  make_freq_table(banos_mujeres_inodoros_mal_estado,"Inodoros o letrinas en baño de estudiantes mujeres en mal estado")

## Frecuencias y Porcentaje de la variable Inodoros o letrinas en baño de estudiantes mujeres tienen cubículo individual
banos_mujeres_cubiculo_individual_inodoros_obs01 <- df_obs01 |> 
  filter(banos_tipo_instalacion_sanitaria != "Otro", 
         banos_tipo_instalacion_sanitaria != '') |>
  make_freq_table(banos_mujeres_cubiculo_individual_inodoros,"Inodoros o letrinas en baño de estudiantes mujeres tienen cubículo individual")

## Frecuencias y Porcentaje de la variable Cada cubículo en baño de estudiantes mujeres tiene basurero
banos_mujeres_recipiente_basura_obs01 <- df_obs01 |> 
  filter(banos_mujeres_cubiculo_individual_inodoros == "Sí") |>
  make_freq_table(banos_mujeres_recipiente_basura,"Cada cubículo en baño de estudiantes mujeres tiene basurero")

## Frecuencias y Porcentaje de la variable Recursos en baño de estudiantes mujeres
banos_mujeres_recursos_obs01 <- df_obs01 |> 
  select_multiple(banos_mujeres_recursos,"Recursos en baño de estudiantes mujeres")

## Frecuencias y Porcentaje de la variable Cantidad de baños mixtos
banos_mixtos_cantidad_obs01 <- df_obs01 |> 
  filter(banos_centro_mixtos == "Sí") |>
  tabla_resumen(banos_mixtos_cantidad)

## Frecuencias y Porcentaje de la variable Baños mixtos cuentan con mingitorios
banos_mixtos_mingitorios_obs01 <- df_obs01 |> 
  filter(banos_centro_mixtos == "Sí") |>
  make_freq_table(banos_mixtos_mingitorios,"Baños mixtos cuentan con mingitorios")

## Frecuencias y Porcentaje de la variable Cantidad de inodoros o letrinas en baños mixtos
banos_mixtos_cantidad_inodoros_obs01 <- df_obs01 |> 
  filter(banos_centro_mixtos == "Sí", 
         banos_tipo_instalacion_sanitaria != "Otro", 
         banos_tipo_instalacion_sanitaria != '') |>
  tabla_resumen(banos_mixtos_cantidad_inodoros)

## Frecuencias y Porcentaje de la variable Cantidad de inodoros o letrinas en baños mixtos utilizables
banos_mixtos_cantidad_inodoros_utilizables_obs01 <- df_obs01 |> 
  filter(banos_centro_mixtos == "Sí", 
         banos_tipo_instalacion_sanitaria != "Otro") |>
  tabla_resumen(banos_mixtos_cantidad_inodoros_utilizables)

## Frecuencias y Porcentaje de la variable Inodoros o letrinas en baños mixtos en mal estado
banos_mixtos_mal_estado_inodoros_obs01 <- df_obs01 |> 
  filter(banos_centro_mixtos == "Sí", 
         banos_tipo_instalacion_sanitaria != "Otro", 
         banos_tipo_instalacion_sanitaria != '') |>
  make_freq_table(banos_mixtos_mal_estado_inodoros,"Inodoros o letrinas en baños mixtos en mal estado")

## Frecuencias y Porcentaje de la variable Inodoros o letrinas en baños mixtos tienen cubículo individual
banos_mixtos_cubiculo_individual_inodoros_obs01 <- df_obs01 |> 
  filter(banos_centro_mixtos == "Sí",
         banos_tipo_instalacion_sanitaria != "Otro", 
         banos_tipo_instalacion_sanitaria != '') |>
  make_freq_table(banos_mixtos_cubiculo_individual_inodoros,"Inodoros o letrinas en baños mixtos tienen cubículo individual")

## Frecuencias y Porcentaje de la variable Cada cubículo en baños mixtos tiene basurero
banos_mixtos_recipiente_basura_obs01 <- df_obs01 |> 
  filter(banos_centro_mixtos == "Sí", 
         banos_mixtos_cubiculo_individual_inodoros == "Sí") |>
  make_freq_table(banos_mixtos_recipiente_basura,"Cada cubículo en baños mixtos tiene basurero")

## Frecuencias y Porcentaje de la variable Recursos en baños mixtos
recursos_banos_mixtos_obs01 <- df_obs01 |> 
  filter(banos_centro_mixtos == "Sí") |>
  select_multiple(recursos_banos_mixtos,"Recursos en baños mixtos")

## Frecuencias y Porcentaje de la variable Baños de hombres tienen puerta de acceso general
seguridad_banos_hombres_puerta_principal_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_exclusivos_hombres == "Sí") |>
  make_freq_table(seguridad_banos_hombres_puerta_principal,"Baños de hombres tienen puerta de acceso general")

## Frecuencias y Porcentaje de la variable Cubículos de baños de hombres tienen puerta individual
seguridad_cubiculo_inodoro_puerta_hombres_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_exclusivos_hombres == "Sí", 
         banos_hombres_cubiculo_individual_inodoros == "Sí") |>
  make_freq_table(seguridad_cubiculo_inodoro_puerta_hombres,"Cubículos de baños de hombres tienen puerta individual")

## Frecuencias y Porcentaje de la variable Puertas de cubículos de baños de hombres cierran correctamente
seguridad_cubiculo_inodoro_puerta_hombres_cierran_correctamente_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_exclusivos_hombres == "Sí", 
         seguridad_cubiculo_inodoro_puerta_hombres == "Sí") |>
  make_freq_table(seguridad_cubiculo_inodoro_puerta_hombres_cierran_correctamente,"Puertas de cubículos de baños de hombres cierran correctamente")

## Frecuencias y Porcentaje de la variable Puertas de cubículos de baños de hombres permiten visibilidad parcial desde fuera
seguridad_cubiculo_inodoro_hombres_permiten_visibilidad_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_exclusivos_hombres == "Sí", 
         seguridad_cubiculo_inodoro_puerta_hombres == "Sí") |>
  make_freq_table(seguridad_cubiculo_inodoro_hombres_permiten_visibilidad,"Puertas de cubículos de baños de hombres permiten visibilidad parcial desde fuera")

## Frecuencias y Porcentaje de la variable Puertas de cubículos de baños de hombres tienen chapas que no representan riesgo
seguridad_cubiculo_inodoro_hombres_seguros_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_exclusivos_hombres == "Sí", 
         seguridad_cubiculo_inodoro_puerta_hombres == "Sí") |>
  make_freq_table(seguridad_cubiculo_inodoro_hombres_seguros,"Puertas de cubículos de baños de hombres tienen chapas que no representan riesgo")

## Frecuencias y Porcentaje de la variable Cubículos de baños de hombres tienen paredes altas
seguridad_paredes_cubiculos_hombres_altas_privacidad_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_exclusivos_hombres == "Sí", 
         banos_hombres_cubiculo_individual_inodoros == "Sí") |>
  make_freq_table(seguridad_paredes_cubiculos_hombres_altas_privacidad,"Cubículos de baños de hombres tienen paredes altas")

## Frecuencias y Porcentaje de la variable Cubículo adaptado para estudiantes con movilidad o visión reducida en baños de hombres
seguridad_banos_hombres_adaptado_movibilidad_reducida_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_exclusivos_hombres == "Sí", 
         banos_hombres_cubiculo_individual_inodoros == "Sí") |>
  make_freq_table(seguridad_banos_hombres_adaptado_movibilidad_reducida,"Cubículo adaptado para estudiantes con movilidad o visión reducida en baños de hombres")

## Frecuencias y Porcentaje de la variable Baños de hombres tienen iluminación
seguridad_banos_hombres_iluminacion_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_exclusivos_hombres == "Sí") |>
  make_freq_table(seguridad_banos_hombres_iluminacion,"Baños de hombres tienen iluminación")

## MUJERES
## Frecuencias y Porcentaje de la variable Baños de mujeres tienen puerta de acceso general
seguridad_banos_mujeres_puerta_principal_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_exclusivos_mujeres == "Sí") |>
  make_freq_table(seguridad_banos_mujeres_puerta_principal,"Baños de mujeres tienen puerta de acceso general")

## Frecuencias y Porcentaje de la variable Cubículos de baños de mujeres tienen puerta individual
seguridad_cubiculo_inodoro_puerta_mujeres_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_exclusivos_mujeres == "Sí", 
         banos_mujeres_cubiculo_individual_inodoros == "Sí") |>
  make_freq_table(seguridad_cubiculo_inodoro_puerta_mujeres,"Cubículos de baños de mujeres tienen puerta individual")

## Frecuencias y Porcentaje de la variable Puertas de cubículos de baños de mujeres cierran correctamente
seguridad_cubiculo_inodoro_puerta_mujeres_cierran_correctamente_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_exclusivos_mujeres == "Sí", 
         seguridad_cubiculo_inodoro_puerta_mujeres == "Sí") |>
  make_freq_table(seguridad_cubiculo_inodoro_puerta_mujeres_cierran_correctamente,"Puertas de cubículos de baños de mujeres cierran correctamente")

## Frecuencias y Porcentaje de la variable Puertas de cubículos de baños de mujeres permiten visibilidad parcial desde fuera
seguridad_cubiculo_inodoro_mujeres_permiten_visibilidad_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_exclusivos_mujeres == "Sí", 
         seguridad_cubiculo_inodoro_puerta_mujeres == "Sí") |>
  make_freq_table(seguridad_cubiculo_inodoro_mujeres_permiten_visibilidad,"Puertas de cubículos de baños de mujeres permiten visibilidad parcial desde fuera")

## Frecuencias y Porcentaje de la variable Puertas de cubículos de baños de mujeres tienen chapas que no representan riesgo
seguridad_cubiculo_inodoro_mujeres_seguros_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_exclusivos_mujeres == "Sí", 
         seguridad_cubiculo_inodoro_puerta_mujeres == "Sí") |>
  make_freq_table(seguridad_cubiculo_inodoro_mujeres_seguros,"Puertas de cubículos de baños de mujeres tienen chapas que no representan riesgo")

## Frecuencias y Porcentaje de la variable Cubículos de baños de mujeres tienen paredes altas
seguridad_paredes_cubiculos_mujeres_altas_privacidad_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_exclusivos_mujeres == "Sí", 
         banos_mujeres_cubiculo_individual_inodoros == "Sí") |>
  make_freq_table(seguridad_paredes_cubiculos_mujeres_altas_privacidad,"Cubículos de baños de mujeres tienen paredes altas")

## Frecuencias y Porcentaje de la variable Cubículo adaptado para estudiantes con movilidad o visión reducida en baños de mujeres
seguridad_banos_mujeres_adaptado_movibilidad_reducida_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_exclusivos_mujeres == "Sí", 
         banos_mujeres_cubiculo_individual_inodoros == "Sí") |>
  make_freq_table(seguridad_banos_mujeres_adaptado_movibilidad_reducida,"Cubículo adaptado para estudiantes con movilidad o visión reducida en baños de mujeres")

## Frecuencias y Porcentaje de la variable Baños de mujeres tienen iluminación
seguridad_banos_mujeres_iluminacion_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_exclusivos_mujeres == "Sí") |>
  make_freq_table(seguridad_banos_mujeres_iluminacion,"Baños de mujeres tienen iluminación")

##MIXTOS 
## Frecuencias y Porcentaje de la variable Baños mixtos tienen puerta de acceso general
seguridad_banos_mixtos_puerta_principal_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_mixtos == "Sí") |>
  make_freq_table(seguridad_banos_mixtos_puerta_principal,"Baños mixtos tienen puerta de acceso general")

## Frecuencias y Porcentaje de la variable Cubículos de baños mixtos tienen puerta individual
seguridad_cubiculo_inodoro_puerta_mixtos_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_mixtos == "Sí", 
         banos_mixtos_cubiculo_individual_inodoros == "Sí") |>
  make_freq_table(seguridad_cubiculo_inodoro_puerta_mixtos,"Cubículos de baños mixtos tienen puerta individual")

## Frecuencias y Porcentaje de la variable Puertas de cubículos de baños mixtos cierran correctamente
seguridad_cubiculo_inodoro_puerta_mixtos_cierran_correctamente_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_mixtos == "Sí", 
         seguridad_cubiculo_inodoro_puerta_mixtos == "Sí") |>
  make_freq_table(seguridad_cubiculo_inodoro_puerta_mixtos_cierran_correctamente,"Puertas de cubículos de baños mixtos cierran correctamente")

## Frecuencias y Porcentaje de la variable Puertas de cubículos de baños mixtos permiten visibilidad parcial desde fuera
seguridad_cubiculo_inodoro_mixtos_permiten_visibilidad_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_mixtos == "Sí", 
         seguridad_cubiculo_inodoro_puerta_mixtos == "Sí") |>
  make_freq_table(seguridad_cubiculo_inodoro_mixtos_permiten_visibilidad,"Puertas de cubículos de baños mixtos permiten visibilidad parcial desde fuera")

## Frecuencias y Porcentaje de la variable Puertas de cubículos de baños mixtos tienen chapas que no representan riesgo
seguridad_cubiculo_inodoro_mixtos_seguros_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_mixtos == "Sí", 
         seguridad_cubiculo_inodoro_puerta_mixtos == "Sí") |>
  make_freq_table(seguridad_cubiculo_inodoro_mixtos_seguros,"Puertas de cubículos de baños mixtos tienen chapas que no representan riesgo")

## Frecuencias y Porcentaje de la variable Cubículos de baños mixtos tienen paredes altas
seguridad_paredes_cubiculos_mixtos_altas_privacidad_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_mixtos == "Sí", 
         banos_mixtos_cubiculo_individual_inodoros == "Sí") |>
  make_freq_table(seguridad_paredes_cubiculos_mixtos_altas_privacidad,"Cubículos de baños mixtos tienen paredes altas")

## Frecuencias y Porcentaje de la variable Cubículo adaptado para estudiantes con movilidad o visión reducida en baños mixtos
seguridad_banos_mixtos_adaptado_movibilidad_reducida_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_mixtos == "Sí", 
         banos_mixtos_cubiculo_individual_inodoros == "Sí") |>
  make_freq_table(seguridad_banos_mixtos_adaptado_movibilidad_reducida,"Cubículo adaptado para estudiantes con movilidad o visión reducida en baños mixtos")

## Frecuencias y Porcentaje de la variable Baños mixtos tienen iluminación
seguridad_banos_mixtos_iluminacion_obs01 <- df_obs01 |> 
  filter(banos_centro == "Sí", 
         banos_tipo_instalacion_sanitaria != '', 
         banos_centro_mixtos == "Sí") |>
  make_freq_table(seguridad_banos_mixtos_iluminacion,"Baños mixtos tienen iluminación")

## Frecuencias y Porcentaje de la variable Alternativa ante falta de baños para estudiantes
alternativa_falta_banos_centro_obs01 <- df_obs01 |> 
  filter(banos_centro == "No") |>
  make_freq_table(alternativa_falta_banos_centro,"Alternativa ante falta de baños para estudiantes")

## Frecuencias y Porcentaje de la variable Centro Educativo tiene instalaciones para lavado de manos
lavado_manos_centro_obs01 <- df_obs01 |> 
  make_freq_table(lavado_manos_centro,"Centro Educativo tiene instalaciones para lavado de manos")

## Frecuencias y Porcentaje de la variable Ubicación de instalaciones para lavado de manos
lavado_manos_instalaciones_obs01 <- df_obs01 |> 
  filter(lavado_manos_centro != "No hay instalaciones", 
         banos_centro == "Sí") |>
  make_freq_table(lavado_manos_instalaciones,"Ubicación de instalaciones para lavado de manos")

## Frecuencias y Porcentaje de la variable Instalaciones para lavado de manos dentro tienen agua y jabón
lavado_manos_instalaciones_dentro_obs01 <- df_obs01 |> 
  filter(lavado_manos_instalaciones == "Dentro y fuera de los baños" | 
         lavado_manos_instalaciones == "Únicamente dentro de los baños") |>
  make_freq_table(lavado_manos_instalaciones_dentro,"Instalaciones para lavado de manos dentro tienen agua y jabón")

## Frecuencias y Porcentaje de la variable Instalaciones para lavado de manos fuera tienen agua y jabón
lavado_manos_instalaciones_fuera_obs01 <- df_obs01 |> 
  filter(lavado_manos_instalaciones == "Dentro y fuera de los baños" | 
           lavado_manos_instalaciones == "Únicamente fuera de los baños") |>
  make_freq_table(lavado_manos_instalaciones_fuera,"Instalaciones para lavado de manos fuera tienen agua y jabón")

## Frecuencias y Porcentaje de la variable Instalaciones para lavado de manos tienen agua y jabón
lavado_manos_instalaciones_agua_jabon_obs01 <- df_obs01 |> 
  filter(banos_centro  == "No",
         lavado_manos_centro != "No") |>
  make_freq_table(lavado_manos_instalaciones_agua_jabon,"Instalaciones para lavado de manos tienen agua y jabón")

## output list
output_list4 <- list(
  "El Centro Educativo cuenta con agua" = agua_centro_obs01, 
  "Fuente de agua del Centro Educativo" = fuente_agua_centro_obs01, 
  "Centro Educativo tiene agua el día de la visita" = agua_centro_dia_visita_obs01, 
  "Tiempo que lleva el Centro Educativo sin agua " = agua_tiempo_centro_obs01, 
  "El agua del Centro Educativo es potable" = agua_centro_segura_obs01, 
  "Regularidad del agua disponible durante jornada escolar" = agua_centro_disponible_jornada_obs01, 
  "Centro Educativo tiene baños para estudiantes" = banos_centro_obs01, 
  "Baños exclusivos para estudiantes hombres" = banos_centro_exclusivos_hombres_obs01, 
  "Baños exclusivos para estudiantes mujeres" = banos_centro_exclusivos_mujeres_obs01, 
  "Baños para estudiantes son mixtos" = banos_centro_mixtos_obs01, 
  "Ubicación de baños para estudiantes" = banos_ubicacion_centro_obs01, 
  "Tipo de instalación sanitaria" = banos_tipo_instalacion_sanitaria_obs01, 
  "Docente tienen baños exclusivos" = banos_centro_exclusivos_docentes_obs01, 
  "Cantidad de baños para estudiantes hombres" = banos_hombres_cantidad_obs01, 
  "Baños cuentan con mingitorios" = banos_hombres_mingitorios_obs01, 
  "Cantidad de inodoros o letrinas en baño de estudiantes hombres" = banos_hombres_cantidad_inodoros_obs01, 
  "Cantidad de inodoros o letrinas en baño de estudiantes hombres utilizables" = banos_hombres_cantidad_inodoros_utilizables_obs01, 
  "Inodoros o letrinas en baño de estudiantes hombres en mal estado" = banos_hombres_inodoros_mal_estado_obs01, 
  "Inodoros o letrinas en baño de estudiantes hombres tienen cubículo individual" = banos_hombres_cubiculo_individual_inodoros_obs01, 
  "Cada cubículo en baño de estudiantes hombres tiene basurero" = banos_hombres_recipiente_basura_obs01, 
  "Recursos en baño de estudiantes hombres" = banos_hombres_recursos_obs01, 
  "Cantidad de baños para estudiantes mujeres" = banos_mujeres_cantidad_obs01, 
  "Cantidad de inodoros o letrinas en baño de estudiantes mujeres" = banos_mujeres_cantidad_inodoros_obs01, 
  "Cantidad de inodoros o letrinas en baño de estudiantes mujeres utilizables" = banos_mujeres_cantidad_inodoros_utilizables_obs01, 
  "Inodoros o letrinas en baño de estudiantes mujeres en mal estado" = banos_mujeres_inodoros_mal_estado_obs01, 
  "Inodoros o letrinas en baño de estudiantes mujeres tienen cubículo individual" = banos_mujeres_cubiculo_individual_inodoros_obs01, 
  "Cada cubículo en baño de estudiantes mujeres tiene basurero" = banos_mujeres_recipiente_basura_obs01, 
  "Recursos en baño de estudiantes mujeres" = banos_mujeres_recursos_obs01, 
  "Cantidad de baños mixtos" = banos_mixtos_cantidad_obs01, 
  "Baños mixtos cuentan con mingitorios" = banos_mixtos_mingitorios_obs01, 
  "Cantidad de inodoros o letrinas en baños mixtos" = banos_mixtos_cantidad_inodoros_obs01, 
  "Cantidad de inodoros o letrinas en baños mixtos utilizables" = banos_mixtos_cantidad_inodoros_utilizables_obs01, 
  "Inodoros o letrinas en baños mixtos en mal estado" = banos_mixtos_mal_estado_inodoros_obs01, 
  "Inodoros o letrinas en baños mixtos tienen cubículo individual" = banos_mixtos_cubiculo_individual_inodoros_obs01, 
  "Cada cubículo en baños mixtos tiene basurero" = banos_mixtos_recipiente_basura_obs01, 
  "Recursos en baños mixtos" = recursos_banos_mixtos_obs01, 
  "Baños de hombres tienen puerta de acceso general" = seguridad_banos_hombres_puerta_principal_obs01, 
  "Cubículos de baños de hombres tienen puerta individual" = seguridad_cubiculo_inodoro_puerta_hombres_obs01, 
  "Puertas de cubículos de baños de hombres cierran correctamente" = seguridad_cubiculo_inodoro_puerta_hombres_cierran_correctamente_obs01, 
  "Puertas de cubículos de baños de hombres permiten visibilidad parcial desde fuera" = seguridad_cubiculo_inodoro_hombres_permiten_visibilidad_obs01, 
  "Puertas de cubículos de baños de hombres tienen chapas que no representan riesgo" = seguridad_cubiculo_inodoro_hombres_seguros_obs01, 
  "Cubículos de baños de hombres tienen paredes altas" = seguridad_paredes_cubiculos_hombres_altas_privacidad_obs01, 
  "Cubículo adaptado para estudiantes con movilidad o visión reducida en baños de hombres" = seguridad_banos_hombres_adaptado_movibilidad_reducida_obs01, 
  "Baños de hombres tienen iluminación" = seguridad_banos_hombres_iluminacion_obs01, 
  "Baños de mujeres tienen puerta de acceso general" = seguridad_banos_mujeres_puerta_principal_obs01, 
  "Cubículos de baños de mujeres tienen puerta individual" = seguridad_cubiculo_inodoro_puerta_mujeres_obs01, 
  "Puertas de cubículos de baños de mujeres cierran correctamente" = seguridad_cubiculo_inodoro_puerta_mujeres_cierran_correctamente_obs01, 
  "Puertas de cubículos de baños de mujeres permiten visibilidad parcial desde fuera" = seguridad_cubiculo_inodoro_mujeres_permiten_visibilidad_obs01, 
  "Puertas de cubículos de baños de mujeres tienen chapas que no representan riesgo" = seguridad_cubiculo_inodoro_mujeres_seguros_obs01, 
  "Cubículos de baños de mujeres tienen paredes altas" = seguridad_paredes_cubiculos_mujeres_altas_privacidad_obs01, 
  "Cubículo adaptado para estudiantes con movilidad o visión reducida en baños de mujeres" = seguridad_banos_mujeres_adaptado_movibilidad_reducida_obs01, 
  "Baños de mujeres tienen iluminación" = seguridad_banos_mujeres_iluminacion_obs01, 
  "Baños mixtos tienen puerta de acceso general" = seguridad_banos_mixtos_puerta_principal_obs01, 
  "Cubículos de baños mixtos tienen puerta individual" = seguridad_cubiculo_inodoro_puerta_mixtos_obs01, 
  "Puertas de cubículos de baños mixtos cierran correctamente" = seguridad_cubiculo_inodoro_puerta_mixtos_cierran_correctamente_obs01, 
  "Puertas de cubículos de baños mixtos permiten visibilidad parcial desde fuera" = seguridad_cubiculo_inodoro_mixtos_permiten_visibilidad_obs01, 
  "Puertas de cubículos de baños mixtos tienen chapas que no representan riesgo" = seguridad_cubiculo_inodoro_mixtos_seguros_obs01, 
  "Cubículos de baños mixtos tienen paredes altas" = seguridad_paredes_cubiculos_mixtos_altas_privacidad_obs01, 
  "Cubículo adaptado para estudiantes con movilidad o visión reducida en baños mixtos" = seguridad_banos_mixtos_adaptado_movibilidad_reducida_obs01, 
  "Baños mixtos tienen iluminación" = seguridad_banos_mixtos_iluminacion_obs01, 
  "Alternativa ante falta de baños para estudiantes" = alternativa_falta_banos_centro_obs01, 
  "Centro Educativo tiene instalaciones para lavado de manos" = lavado_manos_centro_obs01, 
  "Ubicación de instalaciones para lavado de manos" = lavado_manos_instalaciones_obs01, 
  "Instalaciones para lavado de manos dentro tienen agua y jabón" = lavado_manos_instalaciones_dentro_obs01, 
  "Instalaciones para lavado de manos fuera tienen agua y jabón" = lavado_manos_instalaciones_fuera_obs01, 
  "Instalaciones para lavado de manos tienen agua y jabón" = lavado_manos_instalaciones_agua_jabon_obs01)

## Sección 5. Gestión de riesgo y salud ----
## Frecuencias y Porcentaje de la variable Centro Educativo tiene extintores
extintores_obs01 <- df_obs01 |>
  make_freq_table(extintores,"Centro Educativo tiene extintores")

## Frecuencias y Porcentaje de la variable Centro Educativo tiene enfermería
enfermeria_obs01 <- df_obs01 |>
  make_freq_table(enfermeria,"Centro Educativo tiene enfermería")

## Frecuencias y Porcentaje de la variable Centro Educativo tiene botiquín
enfermeria_botiquin_obs01 <- df_obs01 |>
  filter(enfermeria  == "No") |>
  make_freq_table(enfermeria_botiquin,"Centro Educativo tiene botiquín")

## Frecuencias y Porcentaje de la variable Contenido del botiquín
contenido_botiquin_obs01 <- df_obs01 |>
  filter(enfermeria_botiquin  == "Sí") |>
  select_multiple(contenido_botiquin,"Contenido del botiquín")

## Frecuencias y Porcentaje de la variable Centro Educativo tiene rampas de acceso
rampas_acceso_obs01 <- df_obs01 |>
  make_freq_table(rampas_acceso,"Centro Educativo tiene rampas de acceso")

## Frecuencias y Porcentaje de la variable Centro Educativo tiene señalización para rutas de evacuación
rutas_evacuacion_obs01 <- df_obs01 |>
  make_freq_table(rutas_evacuacion,"Centro Educativo tiene señalización para rutas de evacuación")


## output list
output_list5 <- list(
  "Centro Educativo tiene extintores" = extintores_obs01, 
  "Centro Educativo tiene enfermería" = enfermeria_obs01,
  "Centro Educativo tiene botiquín" = enfermeria_botiquin_obs01, 
  "Contenido del botiquín" = contenido_botiquin_obs01, 
  "Centro Educativo tiene rampas de acceso" = rampas_acceso_obs01, 
  "Centro Educativo tiene señalización para rutas de evacuación" = rutas_evacuacion_obs01)


## Sección 6. Laboratorio para TAC y tecnología en las aulas ----

## Frecuencias y Porcentaje de la variable Centro Educativo tiene laboratorio de computación
laboratorio_cat_obs01 <- df_obs01 |>
  make_freq_table(laboratorio_cat,"Centro Educativo tiene laboratorio de computación")

## Frecuencias y Porcentaje de la variable Cantidad de computadoras del laboratorio
laboratorio_numero_computadoras_obs01 <- df_obs01 |> 
  filter(laboratorio_cat == "Sí") |>
  tabla_resumen(laboratorio_numero_computadoras)

## Frecuencias y Porcentaje de la variable Cantidad de computadoras del laboratorio en funcionamiento
laboratorio_numero_computadoras_funcionan_obs01 <- df_obs01 |> 
  filter(laboratorio_numero_computadoras >= "1") |>
  tabla_resumen(laboratorio_numero_computadoras_funcionan)

## Frecuencias y Porcentaje de la variable Adquisición de computadoras del laboratorio
laboratorio_adquisicion_computadoras_obs01 <- df_obs01 |> 
  filter(laboratorio_numero_computadoras >= "1") |>
  select_multiple(laboratorio_adquisicion_computadoras,"Adquisición de computadoras del laboratorio")

## Frecuencias y Porcentaje de la variable Laboratorio de computación tiene internet
laboratorio_internet_computadoras_obs01 <- df_obs01 |> 
  filter(laboratorio_numero_computadoras != '') |>
  make_freq_table(laboratorio_internet_computadoras,"Laboratorio de computación tiene internet")

## Frecuencias y Porcentaje de la variable Laboratorio de computación tiene cañonera
laboratorio_proyector_computadoras_obs01 <- df_obs01 |> 
  filter(laboratorio_cat == "Sí") |>
  make_freq_table(laboratorio_proyector_computadoras,"Laboratorio de computación tiene cañonera")

## Frecuencias y Porcentaje de la variable Laboratorio de computación usado por estudiantes durante su jornada
laboratorio_uso_estudiantes_obs01 <- df_obs01 |> 
  make_freq_table(laboratorio_uso_estudiantes,"Laboratorio de computación usado por estudiantes durante su jornada")

## Frecuencias y Porcentaje de la variable CAT encargado de brindar el área TAC a los estudiantes
laboratorio_cat_encargado_estudiantes_obs01 <- df_obs01 |> 
  filter(laboratorio_cat == "No") |>
  make_freq_table(laboratorio_cat_encargado_estudiantes,"CAT encargado de brindar el área TAC a los estudiantes")

## Frecuencias y Porcentaje de la variable Recursos del laboratorio de computación no considerados
laboratorio_otros_recursos_obs01 <- df_obs01 |> 
  filter(laboratorio_cat == "Sí") |>
  make_freq_table(laboratorio_otros_recursos,"Recursos del laboratorio de computación no considerados")

## Frecuencias y Porcentaje de la variable Estado de las computadoras del laboratorio
laboratorio_estado_computadoras_obs01 <- df_obs01 |> 
  filter(laboratorio_numero_computadoras > "0") |>
  make_freq_table(laboratorio_estado_computadoras,"Estado de las computadoras del laboratorio")

## Frecuencias y Porcentaje de la variable Software instalado en computadoras del laboratorio
laboratorio_software_computadoras_obs01 <- df_obs01 |> 
  filter(laboratorio_numero_computadoras > "0") |>
  select_multiple(laboratorio_software_computadoras,"Software instalado en computadoras del laboratorio")

## Frecuencias y Porcentaje de la variable Plataforma para gestionar el aprendizaje
laboratorio_plataforma_general_centro_obs01 <- df_obs01 |> 
  make_freq_table(laboratorio_plataforma_general_centro,"Plataforma para gestionar el aprendizaje")

## output list
output_list6 <- list(
  "Centro Educativo tiene laboratorio de computación" = laboratorio_cat_obs01, 
  "Cantidad de computadoras del laboratorio" = laboratorio_numero_computadoras_obs01, 
  "Cantidad de computadoras del laboratorio en funcionamiento" = laboratorio_numero_computadoras_funcionan_obs01, 
  "Adquisición de computadoras del laboratorio" = laboratorio_adquisicion_computadoras_obs01, 
  "Laboratorio de computación tiene internet" = laboratorio_internet_computadoras_obs01, 
  "Laboratorio de computación tiene cañonera" = laboratorio_proyector_computadoras_obs01, 
  "Laboratorio de computación usado por estudiantes durante su jornada" = laboratorio_uso_estudiantes_obs01, 
  "CAT encargado de brindar el área TAC a los estudiantes" = laboratorio_cat_encargado_estudiantes_obs01,  
  "Recursos del laboratorio de computación no considerados" = laboratorio_otros_recursos_obs01, 
  "Estado de las computadoras del laboratorio" = laboratorio_estado_computadoras_obs01, 
  "Software instalado en computadoras del laboratorio" = laboratorio_software_computadoras_obs01, 
  "Plataforma para gestionar el aprendizaje" = laboratorio_plataforma_general_centro_obs01)

## Sección 7. Espacio para Educación Física ----

## Frecuencias y Porcentaje de la variable Espacio para Educación Física
educacion_fisica_espacio_obs01 <- df_obs01 |> 
  make_freq_table(educacion_fisica_espacio,"Espacio para Educación Física")

## Frecuencias y Porcentaje de la variable Espacios utilizados para Educación Física
educacion_fisica_cuales_espacio_obs01 <- df_obs01 |> 
  filter(educacion_fisica_espacio == "Sí") |>
  select_multiple(educacion_fisica_cuales_espacio,"Espacios utilizados Educación Física")

## Frecuencias y Porcentaje de la variable Espacios fuera del centro educativo para Educación Física
educacion_fisica_espacio_fuera_centro_obs01 <- df_obs01 |> 
  filter(educacion_fisica_cuales_espacio == "Espacio fuera del Centro Educativo") |>
  select_multiple(educacion_fisica_espacio_fuera_centro,"Espacios fuera del centro educativo para Educación Física")

## Frecuencias y Porcentaje de la variable Propietario del espacio fuera del centro educativo para Educación Física
educacion_fisica_propiedad_espacio_fuera_centro_obs01 <- df_obs01 |> 
  filter(educacion_fisica_cuales_espacio == "Espacio fuera del Centro Educativo") |>
  make_freq_table(educacion_fisica_propiedad_espacio_fuera_centro,"Propietario del espacio fuera del centro educativo para Educación Física")

## Frecuencias y Porcentaje de la variable Espacio para Educación Física es techado o al aire libre
educacion_fisica_espacio_aire_libre_obs01 <- df_obs01 |> 
  filter(educacion_fisica_espacio == "Sí") |>
  make_freq_table(educacion_fisica_espacio_aire_libre,"Espacio para Educación Física es techado o al aire libre")

## output list
output_list7 <- list(
  "Espacio para Educación Física" = educacion_fisica_espacio_obs01, 
  "Espacios utilizados Educación Física" = educacion_fisica_cuales_espacio_obs01, 
  "Espacios fuera del centro educativo para Educación Física" = educacion_fisica_espacio_fuera_centro_obs01, 
  "Propietario del espacio fuera del centro educativo para Educación Física" = educacion_fisica_propiedad_espacio_fuera_centro_obs01, 
  "Espacio para Educación Física es techado o al aire libre" = educacion_fisica_espacio_aire_libre_obs01)

## Sección 8. Salones para Educación Artística ---- 

## Educación musical 
## Frecuencias y Porcentaje de la variable Lenguajes de Educación Artística que brinda el establecimiento
lenguajes_educacion_artistica_obs01 <- df_obs01 |> 
  select_multiple(lenguajes_educacion_artistica,"Lenguajes de Educación Artística que brinda el establecimiento")

## Frecuencias y Porcentaje de la variable Salón exclusivo para Educación Musical
salon_educacion_musical_obs01 <- df_obs01 |> 
  make_freq_table(salon_educacion_musical,"Salón exclusivo para Educación Musical")

## Frecuencias y Porcentaje de la variable Marimba en el centro educativo
centro_tiene_marimba_obs01 <- df_obs01 |> 
  make_freq_table(centro_tiene_marimba,"Marimba en el centro educativo")

## Frecuencias y Porcentaje de la variable Marimba está en el Salón de Educación Musical
marimba_salon_obs01 <- df_obs01 |> 
  filter(salon_educacion_musical == "Sí" , 
         centro_tiene_marimba == "Sí") |>
  make_freq_table(marimba_salon,"Marimba está en el Salón de Educación Musical")

## Frecuencias y Porcentaje de la variable Instrumentos musicales en el salón de Educación Musical
instrumentos_musicales_salon_obs01 <- df_obs01 |> 
  filter(salon_educacion_musical == "Sí") |>
  make_freq_table(instrumentos_musicales_salon,"Instrumentos musicales en el salón de Educación Musical")

## Frecuencias y Porcentaje de la variable Instrumentos musicales en el centro educativo
instrumentos_musicales_obs01 <- df_obs01 |> 
  filter(salon_educacion_musical == "No") |>
  make_freq_table(instrumentos_musicales,"Instrumentos musicales en el centro educativo")

## Frecuencias y Porcentaje de la variable Qué instrumentos musicales hay en el Centro Educativo
cuales_instrumentos_musicales_obs01 <- df_obs01 |> 
  filter(instrumentos_musicales_salon == "Sí" |
         instrumentos_musicales == "Sí") |>
  select_multiple(cuales_instrumentos_musicales,"Qué instrumentos musicales hay en el Centro Educativo")

## Frecuencias y Porcentaje de la variable Adquisición de instrumentos musicales
adquisicion_instrumentos_musicales_obs01 <- df_obs01 |> 
  filter(instrumentos_musicales_salon == "Sí" |
           instrumentos_musicales == "Sí") |>
  select_multiple(adquisicion_instrumentos_musicales,"Adquisición de instrumentos musicales")

## Frecuencias y Porcentaje de la variable Estado de los instrumentos musicales
estado_instrumentos_musicales_obs01 <- df_obs01 |> 
  filter(instrumentos_musicales_salon == "Sí") |>
  make_freq_table(estado_instrumentos_musicales,"Estado de los instrumentos musicales")

## Artes visuales 
## Frecuencias y Porcentaje de la variable Salón exclusivo para Artes Visuales
salon_artes_visuales_obs01 <- df_obs01 |> 
  make_freq_table(salon_artes_visuales,"Salón exclusivo para Artes Visuales")

## Frecuencias y Porcentaje de la variable Recursos del salón de Artes Visuales
recursos_salon_artes_visuales_obs01 <- df_obs01 |> 
  filter(salon_artes_visuales == "Sí") |>
  select_multiple(recursos_salon_artes_visuales,"Recursos del salón de Artes Visuales")

## Teatro 
## Frecuencias y Porcentaje de la variable Salón exclusivo para Teatro
salon_teatro_obs01 <- df_obs01 |> 
  make_freq_table(salon_teatro,"Salón exclusivo para Teatro")

## Frecuencias y Porcentaje de la variable Salón de teatro tiene espacio amplio
espacio_amplio_teatro_obs01 <- df_obs01 |> 
  filter(salon_teatro == "Sí") |>
  make_freq_table(espacio_amplio_teatro,"Salón de teatro tiene espacio amplio")

## Danza 
## Frecuencias y Porcentaje de la variable Salón exclusivo para Danza
salon_danza_obs01 <- df_obs01 |> 
  make_freq_table(salon_danza,"Salón exclusivo para Danza")

## Frecuencias y Porcentaje de la variable Salón de danza tiene espejos
salon_danza_espejos_obs01 <- df_obs01 |> 
  filter(salon_danza == "Sí") |>
  make_freq_table(salon_danza_espejos,"Salón de danza tiene espejos")

## output list
output_list8 <- list(
  "Lenguajes de Educación Artística que brinda el establecimiento" = lenguajes_educacion_artistica_obs01, 
  "Salón exclusivo para Educación Musical" = salon_educacion_musical_obs01, 
  "Marimba en el centro educativo" = centro_tiene_marimba_obs01, 
  "Marimba está en el Salón de Educación Musical" = marimba_salon_obs01, 
  "Instrumentos musicales en el salón de Educación Musical" = instrumentos_musicales_salon_obs01,
  "Instrumentos musicales en el centro educativo" = instrumentos_musicales_obs01,
  "Qué instrumentos musicales hay en el Centro Educativo" = cuales_instrumentos_musicales_obs01, 
  "Adquisición de instrumentos musicales" = adquisicion_instrumentos_musicales_obs01, 
  "Estado de los instrumentos musicales" = estado_instrumentos_musicales_obs01, 
  "Salón exclusivo para Artes Visuales" = salon_artes_visuales_obs01, 
  "Recursos del salón de Artes Visuales" = recursos_salon_artes_visuales_obs01, 
  "Salón exclusivo para Teatro" = salon_teatro_obs01, 
  "Salón de teatro tiene espacio amplio" = espacio_amplio_teatro_obs01, 
  "Salón exclusivo para Danza" = salon_danza_obs01, 
  "Salón de danza tiene espejos" = salon_danza_espejos_obs01)

## Sección 9. Laboratorio o salón de Ciencias ---- 
## Frecuencias y Porcentaje de la variable Laboratorio exclusivo para ciencias naturales
laboratorio_ciencias_obs01 <- df_obs01 |> 
  make_freq_table(laboratorio_ciencias,"Laboratorio exclusivo para ciencias naturales")

## Frecuencias y Porcentaje de la variable Laboratorio de ciencias pórtatil
laboratorio_ciencias_portatil_obs01 <- df_obs01 |> 
  make_freq_table(laboratorio_ciencias_portatil,"Laboratorio de ciencias pórtatil")

## Frecuencias y Porcentaje de la variable Recursos del laboratorio de ciencias
laboratorio_ciencias_recursos_obs01 <- df_obs01 |> 
  filter(laboratorio_ciencias == "Sí") |>
  select_multiple(laboratorio_ciencias_recursos,"Recursos del laboratorio de ciencias")

## output list
output_list9 <- list(
  "Laboratorio exclusivo para ciencias naturales" = laboratorio_ciencias_obs01, 
  "Laboratorio de ciencias pórtatil" = laboratorio_ciencias_portatil_obs01, 
  "Recursos del laboratorio de ciencias" = laboratorio_ciencias_recursos_obs01)

## Sección 10. Espacio para Emprendimiento y Desarrollo ----
## Frecuencias y Porcentaje de la variable Espacios exclusivos para el área de Emprendimiento Para la Productividad
espacio_emprendimiento_obs01 <- df_obs01 |> 
  filter(modalidad == "INEBE (PEMEM)" | 
         modalidad == "INEBOI") |>
  make_freq_table(espacio_emprendimiento,"Espacios exclusivos para el área de Emprendimiento Para la Productividad")

## Frecuencias y Porcentaje de la variable Orientaciones con espacios en el centro educativo 
espacio_emprendimiento_orientaciones_obs01 <- df_obs01 |> 
  filter(modalidad == "INEBE (PEMEM)" | 
           modalidad == "INEBOI", 
         espacio_emprendimiento == "Sí") |>
  select_multiple(espacio_emprendimiento_orientaciones,"Orientaciones con espacios en el centro educativo")


## output list
output_list10 <- list(
  "Espacios exclusivos para el área de Emprendimiento Para la Productividad" = espacio_emprendimiento_obs01, 
  "Orientaciones con espacios en el centro educativo" = espacio_emprendimiento_orientaciones_obs01)

## Sección 11. Otros salones del Centro Educativo ----
## Frecuencias y Porcentaje de la variable Salón exclusivo para docentes
salon_docentes_obs01 <- df_obs01 |> 
  make_freq_table(salon_docentes,"Salón exclusivo para docentes")

## Frecuencias y Porcentaje de la variable Salón exclusivo para dirección
salon_direccion_obs01 <- df_obs01 |> 
  make_freq_table(salon_direccion,"Salón exclusivo para dirección")

## Frecuencias y Porcentaje de la variable Salón de dirección es accesible para estudiantes
salon_direccion_accesible_estudiantes_obs01 <- df_obs01 |> 
  filter(salon_direccion == "Sí") |>
  make_freq_table(salon_direccion_accesible_estudiantes,"Salón de dirección es accesible para estudiantes")

## Frecuencias y Porcentaje de la variable Salón de usos múltiples
salon_usos_multiples_obs01 <- df_obs01 |> 
  make_freq_table(salon_usos_multiples,"Salón de usos múltiples")

## Frecuencias y Porcentaje de la variable Biblioteca en el centro educativo
biblioteca_centro_educativo_obs01 <- df_obs01 |> 
  make_freq_table(biblioteca_centro_educativo,"Biblioteca en el centro educativo")

## Frecuencias y Porcentaje de la variable Otros espacios usados para desarrollo de CNB
otros_espacios_cnb_obs01 <- df_obs01 |> 
  make_freq_table(otros_espacios_cnb,"Otros espacios usados para desarrollo de CNB")

## output list
output_list11 <- list(
  "Salón exclusivo para docentes" = salon_docentes_obs01, 
  "Salón exclusivo para dirección" = salon_direccion_obs01, 
  "Salón de dirección es accesible para estudiantes" = salon_direccion_accesible_estudiantes_obs01, 
  "Salón de usos múltiples" = salon_usos_multiples_obs01, 
  "Biblioteca en el centro educativo" = biblioteca_centro_educativo_obs01, 
  "Otros espacios usados para desarrollo de CNB" = otros_espacios_cnb_obs01) 
  
## Sección 12. Donaciones para el Centro Educativo ----
## Frecuencias y Porcentaje de la variable Otras donaciones recibidas por el centro educativo
donaciones_centro_obs01 <- df_obs01 |> 
  make_freq_table(donaciones_centro,"Otras donaciones recibidas por el centro educativo")

## output list
output_list12 <- list(
  "Otras donaciones recibidas por el centro educativo" = donaciones_centro_obs01) 

## Sección 13. Otros profesionales en el Centro Educativo ----
## Frecuencias y Porcentaje de la variable Otros profesionales trabajando para implementación de áreas
docentes_extra_obs01 <- df_obs01 |> 
  make_freq_table(docentes_extra,"Otros profesionales trabajando para implementación de áreas")

## Frecuencias y Porcentaje de la variable Cargos directivos del Centro Educativo
cargos_directivos_centro_obs01 <- df_obs01 |> 
  filter(sector == "Privado") |>
  select_multiple(cargos_directivos_centro,"Cargos directivos del Centro Educativo")

## output list
output_list13 <- list(
  "Otros profesionales trabajando para implementación de áreas" = docentes_extra_obs01, 
  "Cargos directivos del Centro Educativo" = cargos_directivos_centro_obs01)

## Sección 14. Timbres y tiempo de recreo ----
## Frecuencias y Porcentaje de la variable Centro educativo tiene timbre o campana
timbre_o_campana_centro_obs01 <- df_obs01 |> 
  make_freq_table(timbre_o_campana_centro,"Centro educativo tiene timbre o campana")

## Frecuencias y Porcentaje de la variable Uso del timbre o campana
uso_timbre_o_campana_obs01 <- df_obs01 |> 
  filter(timbre_o_campana_centro == "Sí") |>
  select_multiple(uso_timbre_o_campana,"Uso del timbre o campana")

## Frecuencias y Porcentaje de la variable Número de recreos
numero_recreos_obs01 <- df_obs01 |> 
  mutate(numero_recreos = ifelse(is.na(numero_recreos),
                                 "Sin información", as.character(numero_recreos))) |>
  make_freq_table(numero_recreos,"Número de recreos")

## Frecuencias y Porcentaje de la variable Grupo para cada recreo
grupo_recreos_obs01 <- df_obs01 |> 
  filter(numero_recreos != "4") |>
  make_freq_table(grupo_recreos,"Grupo para cada recreo")

## Frecuencias y Porcentaje de la variable Minutos de recreo
tiempo_recreo_obs01 <- df_obs01 |> 
  filter(numero_recreos != "4") |>
  tabla_resumen(tiempo_recreo)

## output list
output_list14 <- list(
  "Centro educativo tiene timbre o campana" = timbre_o_campana_centro_obs01, 
  "Uso del timbre o campana" = uso_timbre_o_campana_obs01, 
  "Número de recreos" = numero_recreos_obs01, 
  "Grupo para cada recreo" = grupo_recreos_obs01, 
  "Minutos de recreo" = tiempo_recreo_obs01) 

## Sección 15. Tiempos de refacción y cocina ----
## Frecuencias y Porcentaje de la variable Horario exclusivo para refacción o almuerzo
horario_refaccion_obs01 <- df_obs01 |> 
  make_freq_table(horario_refaccion,"Horario exclusivo para refacción o almuerzo")

## Frecuencias y Porcentaje de la variable Estudiantes refaccionan o almuerzan dentro o fuera del aula
refaccion_dentro_fuera_aula_obs01 <- df_obs01 |> 
  filter(horario_refaccion == "Sí") |>
  make_freq_table(refaccion_dentro_fuera_aula,"Estudiantes refaccionan o almuerzan dentro o fuera del aula")

## Frecuencias y Porcentaje de la variable Centro Educativo provee refacción o almuerzo a estudiantes
refaccion_proporcionada_centro_obs01 <- df_obs01 |> 
  filter(horario_refaccion == "Sí") |>
  make_freq_table(refaccion_proporcionada_centro,"Centro Educativo provee refacción o almuerzo a estudiantes")

## Frecuencias y Porcentaje de la variable Centro Educativo tiene cocina
cocina_centro_obs01 <- df_obs01 |> 
  make_freq_table(cocina_centro,"Centro Educativo tiene cocina")

## output list
output_list15 <- list(
  "Horario exclusivo para refacción o almuerzo" = horario_refaccion_obs01, 
  "Estudiantes refaccionan o almuerzan dentro o fuera del aula" = refaccion_dentro_fuera_aula_obs01, 
  "Centro Educativo provee refacción o almuerzo a estudiantes" = refaccion_proporcionada_centro_obs01, 
  "Centro Educativo tiene cocina" = cocina_centro_obs01)

## Sección 16. Remozamiento ----
## Frecuencias y Porcentaje de la variable Centro Educativo fue remozado recientemente (entre 2024 y 2025)
remozamiento_obs01 <- df_obs01 |> 
  make_freq_table(remozamiento,"Centro Educativo fue remozado recientemente (entre 2024 y 2025)")

## Frecuencias y Porcentaje de la variable Año de remozamiento
ano_remozamiento_obs01 <- df_obs01 |> 
  filter(remozamiento == "Sí") |>
  mutate(ano_remozamiento = ifelse(is.na(ano_remozamiento), "Sin información", 
                                   as.character(ano_remozamiento))) |> 
 make_freq_table(ano_remozamiento, "Año de remozamiento")

## Frecuencias y Porcentaje de la variable Mes de remozamiento
mes_remozamiento_obs01 <- df_obs01 |> 
  filter(remozamiento == "Sí") |>
  make_freq_table(mes_remozamiento, "Mes de remozamiento", 
                  levels_order = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                                   "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))

## Frecuencias y Porcentaje de la variable Tipo de remozamiento
tipo_remozamiento_obs01 <- df_obs01 |> 
  filter(remozamiento == "Sí") |>
  select_multiple(tipo_remozamiento, "Tipo de remozamiento")

## output list
output_list16 <- list(
  "Centro Educativo fue remozado recientemente (entre 2024 y 2025)" = remozamiento_obs01, 
  "Año de remozamiento" = ano_remozamiento_obs01, 
  "Mes de remozamiento" = mes_remozamiento_obs01, 
  "Tipo de remozamiento" = tipo_remozamiento_obs01)

## Sección 18. Grado, sección y número de estudiantes ----
## Frecuencias y Porcentaje de la variable Grado observado
grado_obs01 <- df_obs01 |> 
  make_freq_table(grado, "Grado observado", levels_order = c("Primero",
                                                                 "Segundo", "Tercero"))

## Frecuencias y Porcentaje de la variable Sección observada
seccion_obs01 <- df_obs01 |> 
  make_freq_table(seccion, "Sección observada", levels_order = c("A", "B", "C", "D", 
                                                                 "Única", "Otra"))

## Frecuencias y Porcentaje de la variable Número de estudiantes hombres
hombres_obs01 <- df_obs01 |> 
  tabla_resumen(hombres)

## Frecuencias y Porcentaje de la variable Número de estudiantes mujeres
mujeres_obs01 <- df_obs01 |> 
  tabla_resumen(mujeres)

## Frecuencias y Porcentaje de la variable Total de estudiantes
total_children_obs01 <- df_obs01 |> 
  make_freq_table(total_children, "Total de estudiantes")

## Frecuencias y Porcentaje de la variable Grado y sección tiene salón propio
grado_salon_propio_obs01 <- df_obs01 |> 
  make_freq_table(grado_salon_propio, "Grado y sección tiene salón propio")

## output list
output_list18 <- list(
  "Grado observado" = grado_obs01, 
  "Sección observada" = seccion_obs01, 
  "Número de estudiantes hombres" = hombres_obs01, 
  "Número de estudiantes mujeres" = mujeres_obs01, 
  "Total de estudiantes" = total_children_obs01, 
  "Grado y sección tiene salón propio" = grado_salon_propio_obs01)

## Sección 19. Períodos de las áreas ----
## Frecuencias y Porcentaje de la variable Duración de un período de clase
duracion_periodo_clase_obs01 <- df_obs01 |> 
  make_freq_table(duracion_periodo_clase, "Duración de un período de clase")

## Frecuencias y Porcentaje de la variable Áreas impartidas 
areas_impartidas_obs01 <- df_obs01 |> 
  select_multiple(areas_impartidas, "Áreas impartidas")

## Frecuencias y Porcentaje de la variable Periodos de Ciencias Naturales recibidos en una semana
periodos_ciencias_naturales_semana_obs01 <- df_obs01 |>
  filter(str_detect(areas_impartidas, "Ciencias Naturales")) |>
  make_freq_table(periodos_ciencias_naturales_semana, "Periodos de Ciencias Naturales recibidos en una semana", 
                  levels_order = c("1", "2", "3", "4", "Otro", "Sin información"))

## Frecuencias y Porcentaje de la variable Otro número de periodos de Ciencias Naturales
otro_periodos_ciencias_naturales_semana_obs01 <- df_obs01 |> 
  filter(periodos_ciencias_naturales_semana == "Otro") |>
  tabla_resumen(otro_periodos_ciencias_naturales_semana)

## Frecuencias y Porcentaje de la variable Periodos de Ciencias Sociales Formación Ciudadana e Interculturalidad
periodos_sociales_semana_obs01 <- df_obs01 |> 
  filter(str_detect(areas_impartidas, "Ciencias Sociales Formación Ciudadana e Interculturalidad")) |>
  make_freq_table(periodos_sociales_semana, "Periodos de Ciencias Sociales Formación Ciudadana e Interculturalidad", 
                  levels_order = c("1", "2", "3", "4", "Otro", "Sin información"))

## Frecuencias y Porcentaje de la variable Otro número de periodos de Ciencias Sociales Formación Ciudadana e Interculturalidad
otro_periodos_sociales_semana_obs01 <- df_obs01 |> 
  filter(periodos_sociales_semana == "Otro") |>
  tabla_resumen(otro_periodos_sociales_semana)

## Frecuencias y Porcentaje de la variable Periodos de Idioma Español
periodos_espanol_semana_obs01 <- df_obs01 |> 
  filter(str_detect(areas_impartidas, "Idioma Español")) |>
  make_freq_table(periodos_espanol_semana, "Periodos de Idioma Español", 
                  levels_order = c("1", "2", "3", "4", "Otro", "Sin información"))

## Frecuencias y Porcentaje de la variable Otro número de periodos de Idioma Español
otro_periodos_espanol_semana_obs01 <- df_obs01 |> 
  filter(periodos_sociales_semana == "Otro") |>
  tabla_resumen(otro_periodos_espanol_semana)

## Frecuencias y Porcentaje de la variable Periodos de Idioma Extranjero Inglés
periodos_ingles_semana_obs01 <- df_obs01 |> 
  filter(str_detect(areas_impartidas, "Idioma Extranjero Inglés")) |>
  make_freq_table(periodos_ingles_semana, "Periodos de Idioma Extranjero Inglés", 
                  levels_order = c("1", "2", "3", "4", "Otro", "Sin información"))

## Frecuencias y Porcentaje de la variable Otro número de periodos de Idioma Extranjero Inglés
otro_periodos_ingles_semana_obs01 <- df_obs01 |> 
  filter(periodos_ingles_semana == "Otro") |>
  tabla_resumen(otro_periodos_ingles_semana)

## Frecuencias y Porcentaje de la variable Periodos de Culturas e Idiomas Mayas, Garífuna o Xinka
periodos_cultura_maya_semana_obs01 <- df_obs01 |> 
  filter(str_detect(areas_impartidas, "Culturas e Idiomas Mayas, Garífuna o Xinka")) |>
  make_freq_table(periodos_cultura_maya_semana, "Periodos de Culturas e Idiomas Mayas, Garífuna o Xinka", 
                  levels_order = c("1", "2", "3", "4", "Otro", "Sin información"))

## Frecuencias y Porcentaje de la variable Otro número de periodos de Culturas e Idiomas Mayas, Garífuna o Xinka
otro_periodos_cultura_maya_semana_obs01 <- df_obs01 |> 
  filter(periodos_cultura_maya_semana == "Otro") |>
  tabla_resumen(otro_periodos_cultura_maya_semana)

## Frecuencias y Porcentaje de la variable Periodos de Educación Artística (Integrada)
periodos_artistica_integrada_semana_obs01 <- df_obs01 |> 
  filter(str_detect(areas_impartidas, "Educación Artística \\(integrada\\)")) |>
  make_freq_table(periodos_artistica_integrada_semana, "Periodos de Educación Artística (Integrada)", 
                  levels_order = c("1", "2", "3", "4", "Otro", "Sin información"))

## Frecuencias y Porcentaje de la variable Otro número de periodos de Educación Artística (Integrada)
otro_periodos_artistica_integrada_semana_obs01 <- df_obs01 |> 
  filter(periodos_artistica_integrada_semana == "Otro") |>
  tabla_resumen(otro_periodos_artistica_integrada_semana)

## Frecuencias y Porcentaje de la variable Periodos de Educación Artística (Educación Musical)
periodos_musica_semana_obs01 <- df_obs01 |> 
  filter(str_detect(areas_impartidas, "Educación Artística \\(Educación Musical\\)")) |>
  make_freq_table(periodos_musica_semana, "Periodos de Educación Artística (Educación Musical)", 
                  levels_order = c("1", "2", "3", "4", "Otro", "Sin información"))

## Frecuencias y Porcentaje de la variable Otro número de periodos de Educación Artística (Educación Musical)
otro_periodos_musica_semana_obs01 <- df_obs01 |> 
  filter(periodos_musica_semana == "Otro") |>
  tabla_resumen(otro_periodos_musica_semana)

## Frecuencias y Porcentaje de la variable Periodos de Educación Artística (Artes Visuales)
periodos_visuales_semana_obs01 <- df_obs01 |> 
  filter(str_detect(areas_impartidas, "Educación Artística \\(Artes Visuales\\)")) |>
  make_freq_table(periodos_visuales_semana, "Periodos de Educación Artística (Artes Visuales)", 
                  levels_order = c("1", "2", "3", "4", "Otro", "Sin información"))

## Frecuencias y Porcentaje de la variable Otro número de periodos de Educación Artística (Artes Visuales)
otro_periodos_visuales_semana_obs01 <- df_obs01 |> 
  filter(periodos_visuales_semana == "Otro") |>
  tabla_resumen(otro_periodos_visuales_semana)

## Frecuencias y Porcentaje de la variable Periodos de Educación Artística (Teatro)
periodos_teatro_semana_obs01 <- df_obs01 |> 
  filter(str_detect(areas_impartidas, "Educación Artística \\(Teatro\\)")) |>
  make_freq_table(periodos_teatro_semana, "Periodos de Educación Artística (Teatro)", 
                  levels_order = c("1", "2", "3", "4", "Otro", "Sin información"))

## Frecuencias y Porcentaje de la variable Otro número de periodos de Educación Artística (Teatro)
otro_periodos_teatro_semana_obs01 <- df_obs01 |> 
  filter(periodos_teatro_semana == "Otro") |>
  tabla_resumen(otro_periodos_teatro_semana)

## Frecuencias y Porcentaje de la variable Periodos de Educación Artística (Danza)
periodos_danza_semana_obs01 <- df_obs01 |> 
  filter(str_detect(areas_impartidas, "Educación Artística \\(Danza\\)")) |>
  make_freq_table(periodos_danza_semana, "Periodos de Educación Artística (Danza)", 
                  levels_order = c("1", "2", "3", "4", "Otro", "Sin información"))

## Frecuencias y Porcentaje de la variable Otro número de periodos de Educación Artística (Danza)
otro_periodos_danza_semana_obs01 <- df_obs01 |> 
  filter(periodos_danza_semana == "Otro") |>
  tabla_resumen(otro_periodos_danza_semana)

## Frecuencias y Porcentaje de la variable Periodos de Educación Física
periodos_educacion_fisica_semana_obs01 <- df_obs01 |> 
  filter(str_detect(areas_impartidas, "Educación Física")) |>
  make_freq_table(periodos_educacion_fisica_semana, "Periodos de Educación Física", 
                  levels_order = c("1", "2", "3", "4", "Otro", "Sin información"))

## Frecuencias y Porcentaje de la variable Otro número de periodos de Educación Física
otro_periodos_educacion_fisica_semana_obs01 <- df_obs01 |> 
  filter(periodos_educacion_fisica_semana == "Otro") |>
  tabla_resumen(otro_periodos_educacion_fisica_semana)

## Frecuencias y Porcentaje de la variable Periodos de Emprendimiento para la productividad
periodos_emprendimiento_semana_obs01 <- df_obs01 |> 
  filter(str_detect(areas_impartidas, "Emprendimiento para la productividad")) |>
  make_freq_table(periodos_emprendimiento_semana, "Periodos de Emprendimiento para la productividad", 
                  levels_order = c("1", "2", "3", "4", "Otro", "Sin información"))

## Frecuencias y Porcentaje de la variable Otro número de periodos de Emprendimiento para la productividad
otro_periodos_emprendimiento_semana_obs01 <- df_obs01 |> 
  filter(periodos_emprendimiento_semana == "Otro") |>
  tabla_resumen(otro_periodos_emprendimiento_semana)

## Frecuencias y Porcentaje de la variable Periodos de Matemáticas
periodos_matematicas_semana_obs01 <- df_obs01 |> 
  filter(str_detect(areas_impartidas, "Matemáticas")) |>
  make_freq_table(periodos_matematicas_semana, "Periodos de Matemáticas", 
                  levels_order = c("1", "2", "3", "4", "Otro", "Sin información"))

## Frecuencias y Porcentaje de la variable Otro número de periodos de Matemáticas
otro_periodos_matematicas_semana_obs01 <- df_obs01 |> 
  filter(periodos_matematicas_semana == "Otro") |>
  tabla_resumen(otro_periodos_matematicas_semana)

## Frecuencias y Porcentaje de la variable Periodos de Tecnologías del Aprendizaje y la Comunicación -TAC-
periodos_tac_semana_obs01 <- df_obs01 |> 
  filter(str_detect(areas_impartidas, "Tecnologías del Aprendizaje y la Comunicación -TAC-")) |>
  make_freq_table(periodos_tac_semana, "Periodos de Tecnologías del Aprendizaje y la Comunicación -TAC-", 
                  levels_order = c("1", "2", "3", "4", "Otro", "Sin información"))

## Frecuencias y Porcentaje de la variable Otro número de periodos de Tecnologías del Aprendizaje y la Comunicación -TAC-
otro_periodos_tac_semana_obs01 <- df_obs01 |> 
  filter(periodos_tac_semana == "Otro") |>
  tabla_resumen(otro_periodos_tac_semana)

## Frecuencias y Porcentaje de la variable Cantidad de áreas adicionales a las establecidas en el CNB
areas_adicionales_cnb_obs01 <- df_obs01 |> 
  tabla_resumen(areas_adicionales_cnb)


## output list
output_list19 <- list(
  "Duración de un período de clase" = duracion_periodo_clase_obs01, 
  "Áreas impartidas" = areas_impartidas_obs01, 
  "Periodos de Ciencias Naturales recibidos en una semana" = periodos_ciencias_naturales_semana_obs01, 
  "Otro número de periodos de Ciencias Naturales" = otro_periodos_ciencias_naturales_semana_obs01, 
  "Periodos de Ciencias Sociales Formación Ciudadana e Interculturalidad" = periodos_sociales_semana_obs01, 
  "Otro número de periodos de Ciencias Sociales Formación Ciudadana e Interculturalidad" = otro_periodos_sociales_semana_obs01, 
  "Periodos de Idioma Español" = periodos_espanol_semana_obs01,  
  "Otro número de periodos de Idioma Español" = otro_periodos_espanol_semana_obs01, 
  "Periodos de Idioma Extranjero Inglés" =  periodos_ingles_semana_obs01, 
  "Otro número de periodos de Idioma Extranjero Inglés" = otro_periodos_ingles_semana_obs01, 
  "Periodos de Culturas e Idiomas Mayas, Garífuna o Xinka" =  periodos_cultura_maya_semana_obs01, 
  "Otro número de periodos de Culturas e Idiomas Mayas, Garífuna o Xinka" = otro_periodos_cultura_maya_semana_obs01, 
  "Periodos de Educación Artística (Integrada)" = periodos_artistica_integrada_semana_obs01, 
  "Otro número de periodos de Educación Artística (Integrada)" = otro_periodos_artistica_integrada_semana_obs01, 
  "Periodos de Educación Artística (Artes Visuales)" = periodos_visuales_semana_obs01,
  "Otro número de periodos de Educación Artística (Artes Visuales)" = otro_periodos_visuales_semana_obs01, 
  "Periodos de Educación Artística (Teatro)" = periodos_teatro_semana_obs01, 
  "Otro número de periodos de Educación Artística (Teatro)" = otro_periodos_teatro_semana_obs01, 
  "Periodos de Educación Artística (Danza)" = periodos_danza_semana_obs01, 
  "Otro número de periodos de Educación Artística (Danza)" = otro_periodos_danza_semana_obs01, 
  "Periodos de Educación Física" = periodos_educacion_fisica_semana_obs01, 
  "Otro número de periodos de Educación Física" = otro_periodos_educacion_fisica_semana_obs01, 
  "Periodos de Emprendimiento para la productividad" = periodos_emprendimiento_semana_obs01, 
  "Otro número de periodos de Emprendimiento para la productividad" = otro_periodos_emprendimiento_semana_obs01, 
  "Periodos de Matemáticas" = periodos_matematicas_semana_obs01, 
  "Otro número de periodos de Matemáticas" = otro_periodos_matematicas_semana_obs01, 
  "Periodos de Tecnologías del Aprendizaje y la Comunicación -TAC-" = periodos_tac_semana_obs01, 
  "Otro número de periodos de Tecnologías del Aprendizaje y la Comunicación -TAC-" = otro_periodos_tac_semana_obs01, 
  "Cantidad de áreas adicionales a las establecidas en el CNB" = areas_adicionales_cnb_obs01)

## Sección 20. Necesidades educativas especiales ----
## Frecuencias y Porcentaje de la variable NEE discapacidad
nee_asociadas_discapacidad_obs01 <- df_obs01 |> 
  make_freq_table(nee_asociadas_discapacidad, "NEE discapacidad") 
                  
## Frecuencias y Porcentaje de la variable Tipos de NEE discapacidad
tipos_nee_identificadas_obs01 <- df_obs01 |>
  filter(nee_asociadas_discapacidad == "Sí") |>
  select_multiple(tipos_nee_identificadas, "Tipos de NEE discapacidad")

## Frecuencias y Porcentaje de la variable Estudiantes con NEE discapacidad
estudiantes_nee_asociadas_discapacidad_obs01 <- df_obs01 |>
  filter(nee_asociadas_discapacidad == "Sí") |>
  tabla_resumen(estudiantes_nee_asociadas_discapacidad)

## Frecuencias y Porcentaje de la variable NEE no discapacidad
nee_asociadas_no_discapacidad_obs01 <- df_obs01 |> 
  make_freq_table(nee_asociadas_no_discapacidad, "NEE no discapacidad") 

## Frecuencias y Porcentaje de la variable Tipos de NEE no discapacidad
tipos_nee_asociadas_no_discapacidad_identificadas_obs01 <- df_obs01 |>
  filter(nee_asociadas_no_discapacidad == "Sí") |>
  select_multiple(tipos_nee_asociadas_no_discapacidad_identificadas, "Tipos de NEE no discapacidad")

## Frecuencias y Porcentaje de la variable Estudiantes con NEE no discapacidad
estudiantes_nee_asociadas_no_discapacidad_obs01 <- df_obs01 |>
  filter(nee_asociadas_no_discapacidad == "Sí") |>
  tabla_resumen(estudiantes_nee_asociadas_no_discapacidad)

## output list
output_list20 <- list(
  "NEE discapacidad" = nee_asociadas_discapacidad_obs01, 
  "Tipos de NEE discapacidad" = tipos_nee_identificadas_obs01, 
  "Estudiantes con NEE discapacidad" = estudiantes_nee_asociadas_discapacidad_obs01, 
  "NEE no discapacidad" = nee_asociadas_no_discapacidad_obs01, 
  "Tipos de NEE no discapacidad" = tipos_nee_asociadas_no_discapacidad_identificadas_obs01, 
  "Estudiantes con NEE no discapacidad" = estudiantes_nee_asociadas_no_discapacidad_obs01)

## Sección 21. Bilingüismo ---- 
## Frecuencias y Porcentaje de la variable Idiomas que hablan los estudiantes en el aula
idiomas_estudiantes_obs01 <- df_obs01 |>
  select_multiple(idiomas_estudiantes, "Idiomas que hablan los estudiantes en el aula")

## Frecuencias y Porcentaje de la variable Idioma maya que hablan los estudiantes
idiomas_mayas_obs01 <- df_obs01 |>
  filter(str_detect(idiomas_estudiantes, "Un idioma maya")) |>
  select_multiple(idiomas_mayas, "Idioma maya que hablan los estudiantes")

## Frecuencias y Porcentaje de la variable Estudiantes en las clases generalmente
idioma_clases_general_obs01 <- df_obs01 |>
  filter(str_detect(idiomas_estudiantes, "Un idioma maya") |
         str_detect(idiomas_estudiantes, "Xinka")) |>
  select_multiple(idioma_clases_general, "Estudiantes en las clases generalmente")

## output list
output_list21 <- list(
  "Idiomas que hablan los estudiantes en el aula" = idiomas_estudiantes_obs01, 
  "Idioma maya que hablan los estudiantes" = idiomas_mayas_obs01, 
  "Estudiantes en las clases generalmente" = idioma_clases_general_obs01)

## Sección 23. Estado y disposición del mobiliario en el aula ----
## Frecuencias y Porcentaje de la variable Tipos de mobiliario en el salón
tipos_mobiliario_obs01 <- df_obs01 |>
  select_multiple(tipos_mobiliario, "Tipos de mobiliario en el salón")

## Frecuencias y Porcentaje de la variable Mobiliario en buen estado
mobiliario_buen_estado_obs01 <- df_obs01 |> 
  make_freq_table(mobiliario_buen_estado, "Mobiliario en buen estado")

## Frecuencias y Porcentaje de la variable Mobiliario flexible actividades
mobiliario_flexible_obs01 <- df_obs01 |> 
  make_freq_table(mobiliario_flexible, "Mobiliario flexible actividades")

## Frecuencias y Porcentaje de la variable Posibles riesgos físicos aula
posibles_riesgos_obs01 <- df_obs01 |>
  select_multiple(posibles_riesgos, "Posibles riesgos físicos aula")

## output list
output_list23 <- list(
  "Tipos de mobiliario en el salón" = tipos_mobiliario_obs01, 
  "Mobiliario en buen estado" = mobiliario_buen_estado_obs01, 
  "Mobiliario flexible actividades" = mobiliario_flexible_obs01, 
  "Posibles riesgos físicos aula" = posibles_riesgos_obs01)

## Sección 24. Recursos tecnológicos en el aula ---- 
## Frecuencias y Porcentaje de la variable Recursos tecnológicos del aula
recursos_tecnologicos_obs01 <- df_obs01 |> 
  select_multiple(recursos_tecnologicos, "Recursos tecnológicos del aula")

## Frecuencias y Porcentaje de la variable Cañonera propia o compartida
salon_canonera_obs01 <- df_obs01 |> 
  filter(str_detect(recursos_tecnologicos, "Cañonera")) |>
  make_freq_table(salon_canonera, "Cañonera propia o compartida")

## output list
output_list24 <- list(
  "Recursos tecnológicos del aula" = recursos_tecnologicos_obs01, 
  "Cañonera propia o compartida" = salon_canonera_obs01)

## Sección Roster Periodos de áreas adicionales al CNB ---- 
## Frecuencias y Porcentaje de la variable Cantidad de periodos en una semana
periodos_nombre_otra_area_obs01r2 <- df_obs01r2 |> 
  filter(`258. Nombre del área` != '') |>
  make_freq_table(`259. ¿Cuántos periodos de ${nombre_otra_area} se reciben en una semana?`, "Cantidad de periodos en una semana", 
                  levels_order = c("1", "2", "3", "4", "Otro"))

## Frecuencias y Porcentaje de la variable Otro número de periodos en una semana
otro_periodos_nombre_otra_area_obs01r2 <- df_obs01r2 |> 
  filter(`259. ¿Cuántos periodos de ${nombre_otra_area} se reciben en una semana?` == "Otro") |>
  tabla_resumen(`260. Otro número de periodos que se reciben en una semana de ${nombre_otra_area}`)

## output list
output_listr2 <- list(
  "Cantidad de periodos en una semana" = periodos_nombre_otra_area_obs01r2,
  "Otro número de periodos en una semana" = otro_periodos_nombre_otra_area_obs01r2)

## Sección Roster Características de docentes por área ---- 
## Frecuencias y Porcentaje de la variable Área
caracteristicas_docente_area_obs01r3 <- df_obs01r3 |> 
  make_freq_table(`286. Área`, "Área")

## Frecuencias y Porcentaje de la variable Sexo del docente
sexo_docente_area_obs01r3 <- df_obs01r3 |> 
  make_freq_table(`289. Sexo del docente del área`, "Sexo del docente")

## Frecuencias y Porcentaje de la variable Rango de edad del docente
rango_edad_docente_area_obs01r3 <- df_obs01r3 |> 
  make_freq_table(`290. ¿Aproximadamente en qué rango de edad se encuentra el docente del área?`, "Rango de edad del docente")

## Frecuencias y Porcentaje de la variable Docente es especialista de su área
docente_especialista_obs01r3 <- df_obs01r3 |> 
  make_freq_table(`291. ¿El o la docente es especialista en su área?`, "Docente es especialista de su área")

## output list
output_listr3 <- list(
  "Área" = caracteristicas_docente_area_obs01r3, 
  "Sexo del docente" = sexo_docente_area_obs01r3, 
  "Rango de edad del docente" = rango_edad_docente_area_obs01r3, 
  "Docente es especialista de su área" = docente_especialista_obs01r3)

# generar archivos de excel ----
write_xlsx(output_list2, here("Resultados","Observacion a nivel de centros","tablas_seccion_2.xlsx"))
write_xlsx(output_list3, here("Resultados","Observacion a nivel de centros","tablas_seccion_3.xlsx"))
write_xlsx(output_list4, here("Resultados","Observacion a nivel de centros","tablas_seccion_4.xlsx"))
write_xlsx(output_list5, here("Resultados","Observacion a nivel de centros","tablas_seccion_5.xlsx"))
write_xlsx(output_list6, here("Resultados","Observacion a nivel de centros","tablas_seccion_6.xlsx"))
write_xlsx(output_list7, here("Resultados","Observacion a nivel de centros","tablas_seccion_7.xlsx"))
write_xlsx(output_list8, here("Resultados","Observacion a nivel de centros","tablas_seccion_8.xlsx"))
write_xlsx(output_list9, here("Resultados","Observacion a nivel de centros","tablas_seccion_9.xlsx"))
write_xlsx(output_list10, here("Resultados","Observacion a nivel de centros","tablas_seccion_10.xlsx"))
write_xlsx(output_list11, here("Resultados","Observacion a nivel de centros","tablas_seccion_11.xlsx"))
write_xlsx(output_list12, here("Resultados","Observacion a nivel de centros","tablas_seccion_12.xlsx"))
write_xlsx(output_list13, here("Resultados","Observacion a nivel de centros","tablas_seccion_13.xlsx"))
write_xlsx(output_list14, here("Resultados","Observacion a nivel de centros","tablas_seccion_14.xlsx"))
write_xlsx(output_list15, here("Resultados","Observacion a nivel de centros","tablas_seccion_15.xlsx"))
write_xlsx(output_list16, here("Resultados","Observacion a nivel de centros","tablas_seccion_16.xlsx"))
write_xlsx(output_list18, here("Resultados","Observacion a nivel de centros","tablas_seccion_18.xlsx"))
write_xlsx(output_list19, here("Resultados","Observacion a nivel de centros","tablas_seccion_19.xlsx"))
write_xlsx(output_list20, here("Resultados","Observacion a nivel de centros","tablas_seccion_20.xlsx"))
write_xlsx(output_list21, here("Resultados","Observacion a nivel de centros","tablas_seccion_21.xlsx"))
write_xlsx(output_list23, here("Resultados","Observacion a nivel de centros","tablas_seccion_23.xlsx"))
write_xlsx(output_list24, here("Resultados","Observacion a nivel de centros","tablas_seccion_24.xlsx"))
write_xlsx(output_listr2, here("Resultados","Observacion a nivel de centros","tablas_roster_2.xlsx"))
write_xlsx(output_listr3, here("Resultados","Observacion a nivel de centros","tablas_roster_3.xlsx"))
