periodos_clean <- function(df, var, area) {

var <- enquo(var)  # capture variable
  
df <- df |> 
  mutate(!!var :=
           case_when(areas_impartidas != !!area ~ NA,
                     .default = !!var
           ))
}
