library(tidyverse)
library(readxl)
library(writexl)
library(scales)
library(flextable)

df <- read_xlsx("df_2025_limpia_3.xlsx")

# discapacidad ----

# Discapacidad

#imprimir nombres que tengan dificultad
dificultades <- colnames(df)[grep("dificultad", colnames(df), ignore.case = TRUE)]

df <- df %>%
  mutate(across(
    .cols = c(dificultad_ver, dificultad_oir, dificultad_caminar, 
              dificultad_recordar, dificultad_vestirse, dificultad_comunicarse),
    .fns = ~ case_when(
      . == "1. No, ninguna dificultad" ~ 1,
      . == "2. Sí, cierta dificultad" ~ 2,
      . == "3. Sí, mucha dificultad" ~ 3,
      . == "4. Me resulta imposible" ~ 4,
      TRUE ~ NA_real_  # This line handles cases where none of the above conditions are met
    )
  ))

# Identificador 1: indican en una pregunta que tienen "alguna dificultad", "mucha dificultad" o "no puede hacerlo"

df <- df %>%
  mutate(
    discapacidad_1 = if_else(
      dificultad_ver %in% c(2:4) |
        dificultad_oir %in% c(2:4) |
        dificultad_caminar %in% c(2:4) |
        dificultad_recordar %in% c(2:4) |
        dificultad_vestirse %in% c(2:4) |
        dificultad_comunicarse %in% c(2:4),
      "Sí", "No"
    )
  )

# Discapacidad 2: indican en dos preguntas que tienen "alguna dificultad" o en una pregunta "mucha dificultad" o "no puede hacerlo".

df <- df %>%
  mutate(
    # Compute the number of times "2" appears in the specified columns for each row
    count_2s = rowSums(across(c(dificultad_ver, dificultad_oir, dificultad_caminar,
                                dificultad_recordar, dificultad_vestirse,
                                dificultad_comunicarse), ~ . == 2),na.rm=T),
    
    # Determine if there's at least one occurrence of "3" or "4"
    any_3_or_4 = rowSums(across(c(dificultad_ver, 
                                  dificultad_oir,
                                  dificultad_caminar,
                                  dificultad_recordar,
                                  dificultad_vestirse,
                                  dificultad_comunicarse), ~ . %in% c(3, 4)),
                         na.rm = T) > 0,
    
    # Create the new variable 'discapacidad_2' based on the conditions
    discapacidad_2 = if_else(count_2s >= 2 | any_3_or_4, "Sí", "No")
  ) %>%
  # Optional cleanup to remove intermediate variables
  select(-count_2s, -any_3_or_4)


# Discapacidad 3: indican en una pregunta que tienen "mucha dificultad" o "no puede hacerlo"

df <- df %>%
  mutate(
    discapacidad_3 = if_else(
      dificultad_ver %in% c(3:4) |
        dificultad_oir %in% c(3:4) |
        dificultad_caminar %in% c(3:4) |
        dificultad_recordar %in% c(3:4) |
        dificultad_vestirse %in% c(3:4) |
        dificultad_comunicarse %in% c(3:4),
      "Sí", "No"
    )
  )

# Discapacidad 4: indican en al menos una pregunta "no puede hacerlo"

df <- df %>%
  mutate(
    # Create an intermediate logical variable to check for the presence of a "4"
    has_four = rowSums(across(
      c(dificultad_ver, dificultad_oir, dificultad_caminar,
        dificultad_recordar, dificultad_vestirse,
        dificultad_comunicarse),
      ~ ifelse(is.na(.), 0, ifelse(. == 4, 1, 0))
    )) > 0,
    
    # Assign 'Sí' if there's a "4", otherwise 'No'
    discapacidad_4 = if_else(has_four, "Sí", "No")
  ) %>%
  select(-has_four) 


# Tablas

tabla_discapacidad_1 <- df %>% 
  count(Identificador = discapacidad_1, sort = T) %>%
  mutate(Porcentaje = percent(n/sum(n),accuracy = 0.01))

tabla_discapacidad_2 <- df %>% 
  count(Identificador = discapacidad_2, sort = T) %>%
  mutate(Porcentaje = percent(n/sum(n),accuracy = 0.01))

tabla_discapacidad_3 <- df %>% 
  count(Identificador = discapacidad_3, sort = T) %>%
  mutate(Porcentaje = percent(n/sum(n),accuracy = 0.01))

tabla_discapacidad_4 <- df %>% 
  count(Identificador = discapacidad_4, sort = T) %>%
  mutate(Porcentaje = percent(n/sum(n),accuracy = 0.01))

tabla_identificadores <- as.data.frame(rbind(
  c("", "n = 581", "100%"),
  c("Identificador 1", "", ""),
  tabla_discapacidad_1,
  c("Identificador 2", "", ""),
  tabla_discapacidad_2,
  c("Identificador 3", "", ""),
  tabla_discapacidad_3,
  c("Identificador 4", "", ""),
  tabla_discapacidad_4)
)

colnames(tabla_identificadores) <- c("Identificador",
                                     "Frecuencia",
                                     "Porcentaje")

tabla_identificadores <- tabla_identificadores %>%
  flextable() %>%
  bold(bold = TRUE, part = "header") %>%
  bold(i=c(2,5,8,11),j=1) %>%
  padding(i=c(3,4,6,7,9,10,12,13), j=1, padding.left=20) %>% 
  autofit()

discapacidad_1_detalle <- df %>%
  filter(discapacidad_1 == "Sí") %>% 
  count(dificultad_ver,
        dificultad_oir,
        dificultad_caminar,
        dificultad_recordar,
        dificultad_vestirse,
        dificultad_comunicarse) %>% 
  arrange(desc(n))

discapacidad_2_detalle <- df %>%
  filter(discapacidad_2 == "Sí") %>% 
  count(dificultad_ver,
        dificultad_oir,
        dificultad_caminar,
        dificultad_recordar,
        dificultad_vestirse,
        dificultad_comunicarse) %>% 
  arrange(desc(n))

discapacidad_3_detalle <- df %>%
  filter(discapacidad_3 == "Sí") %>% 
  count(dificultad_ver,
        dificultad_oir,
        dificultad_caminar,
        dificultad_recordar,
        dificultad_vestirse,
        dificultad_comunicarse) %>% 
  arrange(desc(n))

discapacidad_4_detalle <- df %>%
  filter(discapacidad_4 == "Sí") %>% 
  count(dificultad_ver,
        dificultad_oir,
        dificultad_caminar,
        dificultad_recordar,
        dificultad_vestirse,
        dificultad_comunicarse) %>% 
  arrange(desc(n))

write_xlsx(df, "df_2025_limpia_4.xlsx")

