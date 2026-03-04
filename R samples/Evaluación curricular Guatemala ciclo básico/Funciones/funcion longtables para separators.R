## salones ----

separa_long_tables <- function(df, separator) {

  df |> 
  select(all_of(c(obs01_demograficos, separator$name_char))) |> 
  pivot_longer(cols = all_of(separator$name_char),
               names_to = "variables1",
               values_to = "valores1") |> 
  separate_longer_delim(valores1, ";") |> 
  left_join(separator |> rename(variables1 = name_char), by = "variables1") |> 
  mutate(variables2 = NA_character_, valores2 = NA_integer_)

  }
