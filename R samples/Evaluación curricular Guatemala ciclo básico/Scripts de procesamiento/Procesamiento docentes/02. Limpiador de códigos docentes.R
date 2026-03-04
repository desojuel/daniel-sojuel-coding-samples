# 02. Limpiador de códigos

# preámbulo 

# intalar pacman si no lo tenés


# leer datos estudiantes

df_dyd <- read_xlsx(here("Datos","Encuesta docente","df_dyd.xlsx"))

# leer muestra completa

muestra <- read_xlsx(here("Muestra","muestra completa.xlsx"), sheet = "Hoja4")

muestra_campo <- muestra |> 
  filter(!is.na(campo)) |> 
  select(-c(pik,Factor_de_expansion_ESCUELAS,orden_seleccion,ID_ESCUELA,ELEGIBLE)) |> 
  rename(codigo = CodEstablecimiento)

muestra_campo <- clean_names(muestra_campo)

# asignar los colnames

# colnames

cols_df_dyd <- read_xlsx(
  here("datasets para procesamiento", "colnames evaluacion curricular basico.xlsx"),
  col_names = T
)[[4]] |> 
  na.omit() |> 
  as.vector()


colnames(df_dyd) <- cols_df_dyd

# limpiar por fecha

df_dyd <- df_dyd[df_dyd$start > as.POSIXct("2025-08-25 00:00:00"), ]

# limpiar por códigos faltantes

# df_dyd <- df_dyd |> 
#   filter(!is.na(codigo))

# arreglador de códigos ----


# buscador de códigos correctos

## buscador por nombre 
muestra_campo |> 
  filter(str_detect(str_to_lower(nombredelestablecimiento), "inebe")) %>%
  select(codigo, nombredelestablecimiento)

## buscador por pedazos de código

muestra_campo |> 
  filter(str_detect(str_to_lower(codigo), "06-02")) %>%
  select(codigo, nombredelestablecimiento)

# corrección de mal codificados
df_dyd$codigo <- gsub("-43$", "-45", df_dyd$codigo)
df_dyd$codigo <- gsub("-14$", "-45", df_dyd$codigo)
df_dyd$codigo <- gsub("-16$", "-45", df_dyd$codigo)
df_dyd$codigo <- gsub("-44$", "-45", df_dyd$codigo)
df_dyd$codigo <- gsub("-46$", "-45", df_dyd$codigo)
df_dyd$codigo <- gsub("-47$", "-45", df_dyd$codigo)
df_dyd$codigo <- gsub("-05$", "-45", df_dyd$codigo)
df_dyd$codigo <- gsub("-55$", "-45", df_dyd$codigo)
df_dyd$codigo <- gsub("-25$", "-45", df_dyd$codigo)

df_dyd$codigo[df_dyd$codigo =="00-03-0036-45"] <- "00-03-0359-45"
df_dyd$codigo[df_dyd$codigo =="04-04-0029-45"] <- "04-04-0028-45"
df_dyd$codigo[df_dyd$codigo %in% c("00-05-0048-45", "00-05-4448-45", "00-08-0448-45")] <- "00-05-0448-45"
df_dyd$codigo[df_dyd$codigo %in% c("18-01-0471-45", "18-01-0355-45")] <- "09-01-1082-45"
df_dyd$codigo[df_dyd$codigo =="17-06-3435-45"] <- "17-06-3455-45"
df_dyd$codigo[df_dyd$codigo == "02-02-0001-45"] <- "04-02-0001-45"      
df_dyd$codigo[df_dyd$codigo == "00-11-0840-45"] <- "00-11-0844-45"
df_dyd$codigo[df_dyd$codigo =="04-01-0052-45"] <- "04-01-0051-45"
df_dyd$codigo[df_dyd$codigo =="03-01-0062-45"] <- "03-01-0061-45"
df_dyd$codigo[df_dyd$codigo =="00-01-0055-45"] <- "00-01-0155-45"
df_dyd$codigo[df_dyd$codigo =="01-01-0103-45"] <- "21-01-0103-45"
df_dyd$codigo[df_dyd$codigo =="02-03-0008-45"] <- "03-02-0008-45"
df_dyd$codigo[df_dyd$codigo =="05-01-0034-45"] <- "05-01-0134-45"
df_dyd$codigo[df_dyd$codigo =="08-01-0127-45"] <- "09-01-0127-45"
#no dio consentimiento df_dyd$codigo[df_dyd$codigo =="\n15-15-4533-45"] <- "15-15-4533-45" 
df_dyd$codigo[df_dyd$codigo =="14-09-0054-45"] <- "13-09-0054-45"
df_dyd$codigo[df_dyd$codigo =="17-05-0248-45"] <- "17-05-0218-45"
df_dyd$codigo[df_dyd$codigo =="14-06-5745-45"] <- "14-06-5207-45"
df_dyd$codigo[df_dyd$codigo =="11-03-0095-45"] <- "11-03-0019-45"
df_dyd$codigo[df_dyd$codigo =="06-02-0006-45"] <- "06-02-0031-45"
df_dyd$codigo[df_dyd$codigo =="05-01-2596-45"] <- "05-01-2695-45"
df_dyd$codigo[df_dyd$codigo %in% c("04-14-0018-45", "04-14-0019-45")] <- "04-14-0021-45"
df_dyd$codigo[df_dyd$codigo =="14-20-0179-45"] <- "14-20-5962-45"
df_dyd$codigo[df_dyd$codigo =="22-02-0008-45"] <- "22-01-0008-45"
df_dyd$codigo[df_dyd$codigo =="05-09-0446-45"] <- "05-09-0456-45"
df_dyd$codigo[df_dyd$codigo =="12-20-0108-45"] <- "12-02-0108-45"
df_dyd$codigo[df_dyd$codigo =="14-06-0296-45"] <- "14-06-0298-45"
df_dyd$codigo[df_dyd$codigo =="10-10-0013-45"] <- "06-10-0013-45"
df_dyd$codigo[df_dyd$codigo =="01-13-1426-45"] <- "03-13-1426-45"
df_dyd$codigo[df_dyd$codigo =="12-09-0086-45"] <- "12-04-0086-45"
df_dyd$codigo[df_dyd$codigo =="15-15-4533-45"] <- "14-15-4533-45"
df_dyd$codigo[df_dyd$codigo =="04-15-4650-45"] <- "04-15-2650-45"
df_dyd$codigo[df_dyd$codigo =="21-01-2102-45"] <- "21-01-0102-45"
df_dyd$codigo[df_dyd$codigo =="07-04-0091-45"] <- "07-04-0732-45"
df_dyd$codigo[df_dyd$codigo =="12-06-0298-45"] <- "14-06-0298-45"
df_dyd$codigo[df_dyd$codigo =="06-08-0066-45"] <- "08-06-0066-45"
df_dyd$codigo[df_dyd$codigo =="12-01-0064-45"] <- "12-01-0043-45"
df_dyd$codigo[df_dyd$codigo =="15-05-0218-45"] <- "17-05-0218-45"
df_dyd$codigo[df_dyd$codigo =="01-01-0381-45"] <- "00-01-0381-45"
df_dyd$codigo[df_dyd$codigo =="14-25-0633-45"] <- "14-15-0633-45"
df_dyd$codigo[df_dyd$codigo =="14-13-5797-45"] <- "14-13-5697-45"
df_dyd$codigo[df_dyd$codigo =="06-10-1185-45"] <- "06-10-1175-45"
df_dyd$codigo[df_dyd$codigo %in% c("04-14-0022-45", "04-14-0039-45", "04-14-0020-45")] <- "04-14-0021-45"

df_dyd$codigo[df_dyd$codigo =="09-01-0545-45"] <- "09-23-0042-45"


# anti join

mal_codificados <-  df_dyd |> 
  anti_join(muestra_campo, by = "codigo")


codigos_nada_que_ver <- mal_codificados$codigo[!is.na(mal_codificados$codigo)]

df_dyd <- df_dyd |> 
  filter(!(codigo %in% codigos_nada_que_ver))

# writexl

write_xlsx(df_dyd, here("Datos","Encuesta docente","df_dyd.xlsx"))