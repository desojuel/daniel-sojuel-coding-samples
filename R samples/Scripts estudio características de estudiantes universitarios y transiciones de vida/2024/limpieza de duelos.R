library(tidyverse)
library(janitor)
library(readxl)
library(writexl)
library(crosstable)

# imputaciones basadas en valores

duelos <- crosstable(df,`Si hubo otro cambio significativo durante el último/presente año, escríbalo aquí.`, 
                     by = `¿Afrontó un duelo o pérdida significativa? (seres queridos, mascotas, proyectos, negocios)`)

df <- df %>%
  mutate(`¿Afrontó un duelo o pérdida significativa? (seres queridos, mascotas, proyectos, negocios)` = 
           if_else(`Si hubo otro cambio significativo durante el último/presente año, escríbalo aquí.` == "Perdí un bebé.",
                   "Sí",
                   `¿Afrontó un duelo o pérdida significativa? (seres queridos, mascotas, proyectos, negocios)`))

