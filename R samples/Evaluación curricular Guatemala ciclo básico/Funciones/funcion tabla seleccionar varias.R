select_multiple <- function(df, var, label) {
  var <- enquo(var)  # capture variable
  df |> 
    select(!!var) |> 
    separate_longer_delim(!!var, delim = ", ") |> 
    dplyr::count(!!label := !!var, sort = F, name = "f") |>
    dplyr::mutate(`%` = percent(f/nrow(df), accuracy = 0.01)) |>
    dplyr::arrange(desc(f)) 
}

select_multiple_puntocoma <- function(df, var, label) {
  var <- enquo(var)  # capture variable
  df |> 
    select(!!var) |> 
    separate_longer_delim(!!var, delim = ";") |> 
    dplyr::count(!!label := !!var, sort = F, name = "f") |>
    dplyr::mutate(`%` = percent(f/nrow(df), accuracy = 0.01)) |>
    dplyr::arrange(desc(f)) 
}
