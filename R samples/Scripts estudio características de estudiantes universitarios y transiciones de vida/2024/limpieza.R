library(tidyverse)
library(janitor)
library(readxl)
library(writexl)
library(crosstable)
library(stringr)

# 2023
#df <- read_xlsx("export-14915666.xlsx")

2024
df <- read_xlsx("export-15031542.xlsx")

df <- df[-c(2:8)]

names(df)[names(df) == "Declaración de consentimiento: He leído el documento de consentimiento informado que me ha sido entregado, he comprendido las explicaciones en él facilitadas."] <- "consentimiento"

names(df)[names(df) == "¿Es usted estudiante de primer ingreso en la Escuela de Ciencias Psicológicas?"] <- "primer_ingreso"

names(df)[names(df) == "Carné"] <- "carnet"
df <- clean_names(df)

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
  filter(primer_ingreso != "No")

post_limpieza <- nrow(df)
no_primer_ingreso <- pre_limpieza - post_limpieza


### carnet duplicado ---- 

pre_limpieza <- nrow(df)

df <- df |> 
  distinct(carnet, .keep_all = TRUE)

post_limpieza <- nrow(df)
n_duplicado <- pre_limpieza - post_limpieza



# para identificar carnets
# carnets <- df$Carné
# write_xlsx(as.data.frame(carnets), "carnets_2023.xlsx")

# concatenar familiares 

df <- df %>%
  tidyr::unite(col = "parentesco_1", starts_with("x1_"), sep = ", ") %>% 
  dplyr::mutate(parentesco_1 = ifelse(parentesco_1 == "NA, NA, NA, NA, NA", NA, parentesco_1))


df <- df %>%
  unite(col = "parentesco_2", starts_with("x2_"), sep = ", ") %>% 
  mutate(parentesco_2 = ifelse(parentesco_2 == "NA, NA, NA, NA, NA", NA, parentesco_2))

df <- df %>%
  unite(col = "parentesco_3", starts_with("x3_"), sep = ", ") %>% 
  mutate(parentesco_3 = ifelse(parentesco_3 == "NA, NA, NA, NA, NA", NA, parentesco_3))

df <- df %>%
  unite(col = "parentesco_4", starts_with("x4_"), sep = ", ") %>% 
  mutate(parentesco_4 = ifelse(parentesco_4 == "NA, NA, NA, NA, NA", NA, parentesco_4))

df <- df %>%
  unite(col = "parentesco_5", starts_with("x5_"), sep = ", ") %>% 
  mutate(parentesco_5 = ifelse(parentesco_5 == "NA, NA, NA, NA, NA", NA, parentesco_5))

df <- df %>%
  unite(col = "parentesco_6", starts_with("x6_"), sep = ", ") %>% 
  mutate(parentesco_6 = ifelse(parentesco_6 == "NA, NA, NA, NA, NA", NA, parentesco_6))

df <- df %>%
  unite(col = "parentesco_7", starts_with("x7_"), sep = ", ") %>% 
  mutate(parentesco_7 = ifelse(parentesco_7 == "NA, NA, NA, NA, NA", NA, parentesco_7))

df <- df %>%
  unite(col = "parentesco_8", starts_with("x8_"), sep = ", ") %>%
  mutate(parentesco_8 = ifelse(parentesco_8 == "NA, NA, NA, NA, NA", NA, parentesco_8))

df <- df %>%
  unite(col = "parentesco_9", starts_with("x9_"), sep = ", ") %>% 
  mutate(parentesco_9 = ifelse(parentesco_9 == "NA, NA, NA, NA, NA", NA, parentesco_9))

df <- df %>%
  unite(col = "parentesco_10", starts_with("x10_"), sep = ", ") %>% 
  mutate(parentesco_10 = ifelse(parentesco_10 == "NA, NA, NA, NA, NA", NA, parentesco_10))

df <- df %>%
  mutate(numero_familia_nuclear = rowSums(!is.na(select(., starts_with("parentesco_"))))
         ) %>% 
  relocate("numero_familia_nuclear", .before = "parentesco_1")


# Use rowwise to apply operations on a per-row basis
df <- df %>%
  rowwise() %>%
  mutate(
    # Select all 'parentesco_' columns
    edad_papa = {
      papa_col = c_across(starts_with("parentesco_"))
      # Find the first instance with "Papá, "
      papa_entry = papa_col[grepl("^Papá,", papa_col, ignore.case = TRUE)]
      # If found, extract the age (second element after split by ', ')
      if (length(papa_entry) > 0) {
        str_split(papa_entry, ", ")[[1]][2]
      } else {
        NA
      }
    }
  ) %>%
  ungroup() %>% 
  relocate("edad_papa", .after = "numero_familia_nuclear") %>% 
  mutate(edad_papa = ifelse(edad_papa == "NA", NA, edad_papa))

df <- df %>%
  rowwise() %>%
  mutate(
    # Select all 'parentesco_' columns
    grado_papa = {
      papa_col = c_across(starts_with("parentesco_"))
      # Find the first instance with "Papá, "
      papa_entry = papa_col[grepl("^Papá,", papa_col, ignore.case = TRUE)]
      # If found, extract the age (second element after split by ', ')
      if (length(papa_entry) > 0) {
        str_split(papa_entry, ", ")[[1]][4]
      } else {
        NA
      }
    }
  ) %>%
  ungroup()  %>% 
  relocate("grado_papa", .after = "edad_papa") %>% 
  mutate(grado_papa = ifelse(grado_papa == "NA", NA, grado_papa))

df <- df %>%
  rowwise() %>%
  mutate(
    # Select all 'parentesco_' columns
    trabajo_papa = {
      papa_col = c_across(starts_with("parentesco_"))
      # Find the first instance with "Papá, "
      papa_entry = papa_col[grepl("^Papá,", papa_col, ignore.case = TRUE)]
      # If found, extract the age (second element after split by ', ')
      if (length(papa_entry) > 0) {
        str_split(papa_entry, ", ")[[1]][5]
      } else {
        NA
      }
    }
  ) %>%
  ungroup() %>% 
  relocate("trabajo_papa", .after = "grado_papa")  %>% 
  mutate(trabajo_papa = ifelse(trabajo_papa == "NA", NA, trabajo_papa))

df <- df %>%
  rowwise() %>%
  mutate(
    # Select all 'parentesco_' columns
    edad_mama = {
      mama_col = c_across(starts_with("parentesco_"))
      mama_entry = mama_col[grepl("^Mamá,", mama_col, ignore.case = TRUE)]
      # If found, extract the age (second element after split by ', ')
      if (length(mama_entry) > 0) {
        str_split(mama_entry, ", ")[[1]][2]
      } else {
        NA
      }
    }
  ) %>%
  ungroup() %>% 
  relocate("edad_mama", .after = "trabajo_papa")  %>% 
  mutate(edad_mama = ifelse(edad_mama == "NA", NA, edad_mama))

df <- df %>%
  rowwise() %>%
  mutate(
    # Select all 'parentesco_' columns
    grado_mama = {
      mama_col = c_across(starts_with("parentesco_"))
      mama_entry = mama_col[grepl("^Mamá,", mama_col, ignore.case = TRUE)]
      # If found, extract the age (second element after split by ', ')
      if (length(mama_entry) > 0) {
        str_split(mama_entry, ", ")[[1]][4]
      } else {
        NA
      }
    }
  ) %>%
  ungroup() %>% 
  relocate("grado_mama", .after = "edad_mama")  %>% 
  mutate(grado_mama = ifelse(grado_mama == "NA", NA, grado_mama))

df <- df %>%
  rowwise() %>%
  mutate(
    # Select all 'parentesco_' columns
    trabajo_mama = {
      mama_col = c_across(starts_with("parentesco_"))
      mama_entry = mama_col[grepl("^Mamá,", mama_col, ignore.case = TRUE)]
      # If found, extract the age (second element after split by ', ')
      if (length(mama_entry) > 0) {
        str_split(mama_entry, ", ")[[1]][5]
      } else {
        NA
      }
    }
  ) %>%
  ungroup() %>% 
  relocate("trabajo_mama", .after = "grado_mama") %>% 
  mutate(trabajo_mama = ifelse(trabajo_mama == "NA", NA, trabajo_mama))

df$edad_mama <- as.numeric(df$edad_mama, na.rm = T)
df$edad_papa <- as.numeric(df$edad_papa, na.rm = T)

# df limpia
# write_xlsx(df, "df_limpia.xlsx")

# df limpia 2024

write_xlsx(df, "df_limpia_2024.xlsx")



