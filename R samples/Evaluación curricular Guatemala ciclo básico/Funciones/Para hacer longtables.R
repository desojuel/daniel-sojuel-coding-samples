prep_cn_grado <- function(grado_txt,
                          cn_tag,
                          n_estudiantes,
                          area_items) {
  
  labels_cn <- labels_est |>
    filter(str_detect(colname_df_est, cn_tag))
  
  cols_cn <- labels_cn[[3]]
  
  df_est |>
    filter(
      numero_estudiantes_encuestado == n_estudiantes,
      str_detect(area_cnb, area_items),
      grado == grado_txt
    ) |>
    select(all_of(c(est_demograficos, cols_cn))) |>
    pivot_longer(
      cols = all_of(cols_cn),
      names_to = "colname_df_est",
      values_to = "respuesta_likert"
    ) |>
    left_join(labels_cn, by = "colname_df_est")
}