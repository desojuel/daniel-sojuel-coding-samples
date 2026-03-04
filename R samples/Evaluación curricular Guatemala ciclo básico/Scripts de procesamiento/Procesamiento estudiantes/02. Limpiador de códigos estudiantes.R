# 02. Limpieza de casos válidos

# leer datos estudiantes

df_est <- read_xlsx(here("Datos","Encuesta estudiantes","df_est.xlsx"))

# leer muestra completa

muestra <- read_xlsx(here("Muestra","muestra completa.xlsx"), sheet = "Hoja4")

muestra_campo <- muestra |> 
  filter(!is.na(campo)) |> 
  select(-c(pik,Factor_de_expansion_ESCUELAS,orden_seleccion,ID_ESCUELA,ELEGIBLE)) |> 
  rename(codigo = CodEstablecimiento)

muestra_campo <- clean_names(muestra_campo)

# asignar los colnames

# colnames

cols_df_est <- read_xlsx(
  here("datasets para procesamiento", "colnames evaluacion curricular basico.xlsx"),
  col_names = T
)[[3]] |> 
  na.omit() |> 
  as.vector()

colnames(df_est) <- cols_df_est

#df_est$codigo <- as.character(df_est$codigo)



# limpiar por códigos faltantes

#df_est <- df_est |> 
#  filter(!is.na(codigo))


# limpiador por fecha ----

df_est <- df_est[df_est$start >= as.POSIXct("2025-08-04 00:00:00"), ]


# arreglador de códigos ----

df_est$codigo <- gsub("\\.", "", df_est$codigo) # quita puntos
df_est$codigo <- gsub("\\’", "-", df_est$codigo)
df_est$codigo <- gsub("\\,", "", df_est$codigo) 
df_est$codigo <- gsub("\\+", "-", df_est$codigo)
df_est$codigo <- gsub("\\_", "-", df_est$codigo)
df_est$codigo <- gsub("\\_", "", df_est$codigo) 
df_est$codigo <- gsub("\\'", "-", df_est$codigo)
df_est$codigo <- gsub("\\!", "", df_est$codigo) 
df_est$codigo <- gsub("\\O", "0", df_est$codigo)
df_est$codigo <- gsub("\\o", "0", df_est$codigo) 
df_est$codigo <- gsub("\\ -", "-", df_est$codigo) 
df_est$codigo <- gsub("\\- ", "-", df_est$codigo) 
df_est$codigo <- gsub("\\ - ", "-", df_est$codigo) 
df_est$codigo <- gsub("\\--", "-", df_est$codigo) 
df_est$codigo <- gsub("\\ ", "-", df_est$codigo) 
df_est$codigo <- gsub("-43$", "-45", df_est$codigo)
df_est$codigo <- gsub("-44$", "-45", df_est$codigo)
df_est$codigo <- gsub("-46$", "-45", df_est$codigo)
df_est$codigo <- gsub("-48$", "-45", df_est$codigo)
df_est$codigo <- gsub("-49$", "-45", df_est$codigo)
df_est$codigo <- gsub("-54$", "-45", df_est$codigo)
df_est$codigo <- gsub("-55$", "-45", df_est$codigo)
df_est$codigo <- gsub("-245$", "-45", df_est$codigo)
df_est$codigo <- gsub("\\?$", "", df_est$codigo)
df_est$codigo <- gsub("^Q", "0", df_est$codigo)
df_est$codigo <- gsub("--+", "-", df_est$codigo)
df_est$codigo <- gsub("[⁰¹²³⁴⁵⁶⁷⁸⁹]", "", df_est$codigo)
#df_est$codigo <- str_replace_all(df_est$codigo, "O", "0")  # convierte O en 0
#df_est$codigo <- str_replace_all(df_est$codigo, "o", "0")

df_est$codigo <- gsub(
  "([0-9]{2})([0-9]{2})([0-9]{4})([0-9]{2})",
  "\\1-\\2-\\3-\\4",
  df_est$codigo
)

#Para códigos sin guión 

df_est$codigo <- gsub(
  "^(\\d{2})-(\\d{2})(\\d{4})-(\\d{2})$", "\\1-\\2-\\3-\\4",
  df_est$codigo)

#1603-0071-45 para códigos así

df_est$codigo <- gsub(
  "^(\\d{2})(\\d{2}-\\d{4}-\\d{2})$", "\\1-\\2", 
  df_est$codigo)

#05010300-45  para códigos así

df_est$codigo <- gsub(
  "^(\\d{2})(\\d{2})(\\d{4})-(\\d{2})$",
  "\\1-\\2-\\3-\\4", df_est$codigo)

#03-01-02-18-45 códigos así

df_est$codigo <- gsub(
 "^(\\d{2}-\\d{2})-(\\d{2})-(\\d{2}-\\d{2})$", "\\1-\\2\\3", 
 df_est$codigo)

#Código así 1801-0364-45

df_est$codigo <- gsub("^(\\d{2})(\\d{2}-.*)$", "\\1-\\2", df_est$codigo)


# buscador de códigos correctos

## buscador por nombre 
muestra_campo |> 
  filter(str_detect(str_to_lower(nombredelestablecimiento), "comercial")) %>%
  select(codigo, nombredelestablecimiento)

peten <- muestra_campo |> 
  filter(str_detect(str_to_lower(departamento), "peten")) %>%
  select(codigo, nombredelestablecimiento,municipio)

df_est |> 
  filter(str_detect(centro, "TELESECUNDARIA")) %>%
  select(codigo, centro) 

df_est |> 
  filter(str_detect(codigo, "01-14-0049-45")) %>%
  select(codigo, departamento) 

## buscador por municipio 
muestra_campo |> 
  filter(str_detect(str_to_lower(municipio), "joyabaj")) %>%
  select(codigo, municipio)

#buscador departamento y establecimiento
muestra_campo |> 
  filter(
    str_detect(str_to_lower(nombredelestablecimiento), "iebc"),
    str_detect(str_to_lower(departamento), "peten")
  ) |>
  select(codigo, nombredelestablecimiento, departamento, municipio, sector)


## buscador por pedazos de código

muestra_campo |> 
  filter(str_detect(str_to_lower(codigo), "0019")) %>%
  select(codigo, nombredelestablecimiento,departamento)


# corrección de mal codificados ----

df_est$codigo[df_est$codigo == "01-14-0049-45\n"] <- "01-14-0049-45"




df_est$codigo[df_est$codigo == "11-01-1186-45"] <- "11-01-1166-45"
df_est$codigo[df_est$codigo == "06-00-1229-45"] <- "06-09-1229-45"
df_est$codigo[df_est$codigo == "06-09-1225-45"] <- "06-09-1229-45"
df_est$codigo[df_est$codigo == "09-013614-45"] <- "09-01-3614-45"
df_est$codigo[df_est$codigo == "00-01‐9531-45"] <- "00-01-9531-45"
df_est$codigo[df_est$codigo == "15-07-0204-45"] <- "15-08-0204-45"
df_est$codigo[df_est$codigo == "15-08-02024-45"] <- "15-08-0204-45"
df_est$codigo[df_est$codigo %in% c("06-14-1418-45", "06-04-0418-45", "06-14-0518-45")] <- "06-14-0418-45"
df_est$codigo[df_est$codigo %in% c("13-02-0215-45", "13-02-0314-45", "13-02-0215-45",
                                   "13-02-0224-45")] <- "13-02-0214-45"



df_est$codigo[df_est$codigo =="13"] <- "13-01-1496-45"
df_est$codigo[df_est$codigo == "02-0-0248-45"] <- "02-01-0248-45"
df_est$codigo[df_est$codigo == "0111-01-0075-45"] <- "11-01-0075-45"
df_est$codigo[df_est$codigo == "04-07-3187-45"] <- "04-07-3187-45"
df_est$codigo[df_est$codigo == "17-00-3729-45"] <- "17-09-3729-45"
df_est$codigo[df_est$codigo == "09-01-0124-45"] <- "09-01-0129-45"
df_est$codigo[df_est$codigo %in% c("05-02-0133-45", "05-01-133-45")] <- "05-01-0133-45"
df_est$codigo[df_est$codigo %in% c( "14-08-0002-45", "14-05-0092-45", "14-05-0003-45")] <- "14-05-0002-45"
df_est$codigo[df_est$codigo == "05-04-0028-45"] <- "05-04-0028-45"
df_est$codigo[df_est$codigo == "06-10-1175045"] <- "06-10-1175-45"
df_est$codigo[df_est$codigo %in% c("14-13-00067-45","14-14-0067-45")] <- "14-13-0067-45"
df_est$codigo[df_est$codigo == "16-04-1909-45"] <- " 16-04-9109-45"
df_est$codigo[df_est$codigo %in% c("20-06-0256-45", "30-05-0256-45", "20")] <- "20-05-0256-45"
df_est$codigo[df_est$codigo %in% c("15-01-0101-45", "25-01-0101-45")] <- "15-01-0101-45"
df_est$codigo[df_est$codigo %in% c("14-14-0128-45", "14-13-0128")] <- "14-13-0128-45"
df_est$codigo[df_est$codigo %in% c("04-01-0214-45", "04-01-]213-45", "04-01",
                                   "05-01-0213-45")] <- "04-01-0213-45"
df_est$codigo[df_est$codigo %in% c("20-01-0072-45", "29-01-0072-45", "19-91-0072-45", "19-02-0072-45",
                                   "19-02-0072-45", "29-02-0072-45", "29-01-0072-45")] <- "19-01-0072-45"
df_est$codigo[df_est$codigo == "11-05-00008-45"] <- "11-05-0008-45"
df_est$codigo[df_est$codigo =="16-07-016-45"] <- "16-07-0167-45"
df_est$codigo[df_est$codigo %in% c("09-01-0130-", "00-01-0130-45", "00-01-0130-45",
                                   "09-01-0130")] <- "09-01-0130-45"
df_est$codigo[df_est$codigo %in% c("21-0103-45", "21-01-0103", 
                                   "21-91-0103-45")]<- "21-01-0103-45"
df_est$codigo[df_est$codigo %in% c("16-07-0167-45", "16-09-717-45" )] <- "16-09-0717-45"                               
df_est$codigo[df_est$codigo %in% c("02-06-0549-45-", "02-06-0559-45")] <- "02-06-0549-45"
df_est$codigo[df_est$codigo %in% c("09-19-0599-45", "00-19-0566-45", "09-19-0566")] <- "09-19-0566-45"
df_est$codigo[df_est$codigo %in% c("10-07-023-45", "10-07-0213", "10-07-0212-45")] <- "10-07-0213-45"
df_est$codigo[df_est$codigo %in% c("14-13-5597-45", "14-13-5674-45")] <- "14-13-5697-45"


df_est$codigo[df_est$codigo == "00-16-8288-45"] <- "00-16-8289-45"
df_est$codigo[df_est$codigo %in% c("01-10-1213-45", "01-10-02-3-45")] <- "01-10-0213-45"
df_est$codigo[df_est$codigo %in% c("01-03-0077645", "01-03-0977-45")] <- "01-03-0077-45"
df_est$codigo[df_est$codigo %in% c("01-06-9166-45", "0-1-05-9166-45")] <- "01-05-9166-45"
df_est$codigo[df_est$codigo =="19-04083745"] <- "19-04-0837-45"
df_est$codigo[df_est$codigo =="00-18-9285-45"] <- "00-18-9275-45"
df_est$codigo[df_est$codigo %in% c("06-07-0160-45", "26-07-0160-45")] <- "16-07-0160-45"
df_est$codigo[df_est$codigo %in% c("13–01–0334–45", "13-01-0434-45")] <- "13-01-0334-45"
df_est$codigo[df_est$codigo =="00-13-0949-45"] <- "00-13-0944-45"
df_est$codigo[df_est$codigo %in% c("19-11-0013-45", "29-11-0019-45", "29-11-0019-45", 
                                   "10-12-0014-45")] <- "19-11-0019-45"
df_est$codigo[df_est$codigo == " 16-04-9109-45"] <- "16-04-9109-45"
df_est$codigo[df_est$codigo %in% c("07-04-2039-45", "07-04-3029")] <- "07-04-3029-45"
df_est$codigo[df_est$codigo =="17-11-0130-45"] <- "17-12-0130-45"
df_est$codigo[df_est$codigo %in% c("13-12-59-45", "12-13-46-56")] <- "13-12-5946-45"
df_est$codigo[df_est$codigo == "21-01-0337-45"] <- "21-01-0307-45"
df_est$codigo[df_est$codigo == "2222-12-1506-45"] <- "22-12-1506-45"
df_est$codigo[df_est$codigo =="10-13-2651-45"] <- "10-13-2651-45"
df_est$codigo[df_est$codigo =="00-0524-45"] <- "00-06-0524-45"
df_est$codigo[df_est$codigo =="17-07-0037-45"] <- "17-01-0037-45"
df_est$codigo[df_est$codigo =="32-06-0898-45"] <- "21-06-0898-45"
df_est$codigo[df_est$codigo %in% c("04-06-2973", "P4-06-2973-45", "04-06-2973",
                                   "04-06-3973-45", "04-06-2973", "04-06-2973")] <- "04-06-2973-45"
df_est$codigo[df_est$codigo %in% c("05-03-3141-45", "04-03-3141-458", "04–03-3141-45")] <- "04-03-3141-45"
df_est$codigo[df_est$codigo %in% c("09040232", "09040232")] <- "09-04-0232-45"
df_est$codigo[df_est$codigo =="11-1-1473-45"] <- "11-01-1473-45"
df_est$codigo[df_est$codigo =="09-01-4029-45"] <- "09-16-4029-45"
df_est$codigo[df_est$codigo =="10-13-2651-45"] <- "10-13-2651-45"
df_est$codigo[df_est$codigo =="06-01-1380-45"] <- "16-01-1370-45"
df_est$codigo[df_est$codigo =="15-15-4533-45"] <- "14-15-4533-45"
df_est$codigo[df_est$codigo %in% c("14-010-4526-45", "14-10-4525-45")] <- "14-10-4526-45"
df_est$codigo[df_est$codigo =="01-10-0070-45"] <- "01-10-0071-45"
df_est$codigo[df_est$codigo =="05-05-0010-45"] <- "04-05-0010-45"
df_est$codigo[df_est$codigo =="02-06-0559-45"] <- "02-06-0549-45"
df_est$codigo[df_est$codigo =="14-12-0616-45"] <- "14-12-0116-45"
df_est$codigo[df_est$codigo =="26-09-0388-45"] <- "16-09-0388-45"
df_est$codigo[df_est$codigo =="00-05-0049-45"] <- "00-05-0449-45"
df_est$codigo[df_est$codigo =="06-09-0050-45"] <- "06-08-0050-45"
df_est$codigo[df_est$codigo %in% c("15-06-1057-45", "10-06-1057-45")] <- "19-06-1057-45"


df_est$codigo[df_est$codigo =="19-1-0072-45"] <- "19-01-0072-45"
df_est$codigo[df_est$codigo =="10-01-0060-45"] <- "10-01-0069-45"
df_est$codigo[df_est$codigo =="10-06-0023-"] <- "10-06-0023-45"
df_est$codigo[df_est$codigo %in% c("11-0100082-45","11-01-0082-42")] <- "11-01-0082-45"
df_est$codigo[df_est$codigo =="11-07-0035-45"] <- "11-07-0034-45"
df_est$codigo[df_est$codigo =="11-01-0218-45"] <- "11-07-0218-45"
df_est$codigo[df_est$codigo %in% c("11-09-1421","11-09-1421","11-09-1521-45")] <- "11-09-1421-45"
df_est$codigo[df_est$codigo %in% c("09-12-0024-25", "09-12-002445", "09-12-0025-46", "]9-12-0024-45")] <- "09-12-0024-45"
df_est$codigo[df_est$codigo %in% c("09-04-0019-54", "09-04-0019-4")] <- "09-04-0019-45" 
df_est$codigo[df_est$codigo =="09-17-0033-46"] <- "09-17-0033-45"
df_est$codigo[df_est$codigo =="09-01-0304-45"]  <- "09-01-0204-45"          
df_est$codigo[df_est$codigo =="09-09-0127-44"] <- "09-01-0127-45"
df_est$codigo[df_est$codigo =="14-02-0083-45"] <- "14-01-0083-45"
df_est$codigo[df_est$codigo %in% c("21-07-0262-45", "21-01-0272-45","21-08-0272-45", "21-07-0262-45")] <- "21-07-0272-45"
df_est$codigo[df_est$codigo %in% c("04-01-0051", "04-01-0951-45", "04-01-005-45",
                                   "04-01-005-45")] <- "04-01-0051-45"
df_est$codigo[df_est$codigo %in% c("11-01-0360-45", "11-01-0370")] <- "11-01-0370-45"
df_est$codigo[df_est$codigo =="03-01-0023-45"] <- "03-11-0023-45"
df_est$codigo[df_est$codigo =="05-11-007-45"] <- "05-11-0077-45"
#df_est$codigo[str_detect(str_to_lower(df_est$codigo),  "13-AVENIDA-3-76-Z0NA-7-C0L0NIA-QUINTA-SAMAY0A")] <- "00-07-0166-45"


df_est$codigo[df_est$codigo =="20-06-0236-45"] <- "21-06-0236-45"
df_est$codigo[df_est$codigo =="14-20-6962-45"] <- "14-20-5962-45"
df_est$codigo[df_est$codigo =="09-01-0015-45"] <- "09-03-0015-45"
df_est$codigo[df_est$codigo =="09-03-3624"] <- "09-03-3624-45"
df_est$codigo[df_est$codigo =="00-20-2622-45"] <- "09-20-2622-45"
df_est$codigo[df_est$codigo =="1410077-45"] <- "14-01-0077-45"
df_est$codigo[df_est$codigo =="01-08-0225-45-01-08-0225-45"] <- "01-08-0225-45"  
df_est$codigo[df_est$codigo =="02-21-8218-45"] <- "00-21-8218-45"
df_est$codigo[df_est$codigo =="13-15-00-68"] <- "13-15-0068-45"
df_est$codigo[df_est$codigo =="08-01-2227-45"] <- "08-01-2217-45"
df_est$codigo[df_est$codigo %in% c("05-0079-45", "05-09-0079-")] <- "05-09-0079-45"
df_est$codigo[df_est$codigo =="01-06-1401-45"] <- "01-06-1404-45"
df_est$codigo[df_est$codigo =="05-02-0142-45"] <- "05-01-0142-45"
df_est$codigo[df_est$codigo =="18-01-2035-45"] <- "18-01-3035-45"
df_est$codigo[df_est$codigo =="12-0091-45"] <- "12-06-0091-45"
df_est$codigo[df_est$codigo =="08-20-2622-45"] <- "09-20-2622-45"
df_est$codigo[df_est$codigo %in% c("3455-45", "17-05-3455-45", "17-05-3455-45", 
                                   "17-06-3455")] <- "17-06-3455-45"
df_est$codigo[df_est$codigo =="20-01-2153-45"] <- "20-04-2153-45"
df_est$codigo[df_est$codigo =="01-05-005-45"] <- "01-05-0005-45"
df_est$codigo[df_est$codigo =="04-15-4533-45"] <- "14-15-4533-45"
df_est$codigo[df_est$codigo =="22-0-00008-45"] <- "22-01-0008-45"
df_est$codigo[df_est$codigo =="16-09-0480-45"] <- "16-09-0380-45"
df_est$codigo[df_est$codigo =="01-15-7190-45"] <- "01-15-6190-45" 
df_est$codigo[df_est$codigo =="14-19-00039-45"] <- "14-19-0039-45"
df_est$codigo[df_est$codigo =="2201-02226-45"] <- "22-01-0226-45"
df_est$codigo[df_est$codigo =="04-06-0339-"] <- "04-06-0339-45"
df_est$codigo[df_est$codigo =="18-02-0391-45"] <- "18-01-0391-45"
df_est$codigo[df_est$codigo =="04-03-0007-45"] <- "05-03-0007-45"
df_est$codigo[df_est$codigo =="00-25-0013-45m"] <- "00-25-0013-45"
df_est$codigo[df_est$codigo =="13-15-0069-45"] <- "13-06-0069-45"
df_est$codigo[df_est$codigo =="04-06-0330-45"] <- "04-06-0339-45"
df_est$codigo[df_est$codigo =="00-0155-45"] <- "00-01-0155-45"
df_est$codigo[df_est$codigo =="07-050-25491-45"] <- "07-05-2481-45"



df_est$codigo[df_est$codigo =="09-09-0127-45"] <- "09-01-0127-45"
df_est$codigo[df_est$codigo =="03-0-1-0059-45"] <- "03-01-0059-45"
df_est$codigo[df_est$codigo %in% c("20-07-010445", "20-08-0104-45")] <- "20-07-0104-45"
df_est$codigo[df_est$codigo %in% c("22-01-2089-", "22-01–2089-45",
                                   "22-01-008-45")] <- "22-01-2089-45"
df_est$codigo[df_est$codigo =="03-001-0061-45"] <-"03-01-0061-45"
df_est$codigo[df_est$codigo =="05-05-0448-45"] <-"00-05-0448-45"
df_est$codigo[df_est$codigo =="30-01-0025-45"] <-"20-01-0025-45"
df_est$codigo[df_est$codigo =="01-04-0047-45"] <-"04-01-0047-45"        
df_est$codigo[df_est$codigo =="13-04-0064-45"] <-"13-01-0064-45"
df_est$codigo[df_est$codigo %in% c("11-01-0034-45", "11-07-1483")] <- "18-04-2514-45"
df_est$codigo[df_est$codigo =="09011082"] <- "09-01-1082-45"
df_est$codigo[df_est$codigo =="03-17-1366-45"] <- "03-14-1366-45"
df_est$codigo[df_est$codigo =="00-01-0381-45"] <- "00-01-0381-45"
df_est$codigo[df_est$codigo =="04-10-0410-45"] <- "04-10-0452-45"
df_est$codigo[df_est$codigo == "15"] <- "15-03-0068-45"
df_est$codigo[df_est$codigo =="19-12-0019-45"] <- "19-11-0019-45"
df_est$codigo[df_est$centro =="Liceo Luis von Ahn"] <- "18-01-0364-45"
df_est$codigo[df_est$codigo =="05-04-0124-45"] <- "05-02-0124-45"
df_est$codigo[df_est$codigo =="05-02-2945-45"] <- "05-02-2995-45"
df_est$codigo[df_est$codigo =="18-01-3935-45"] <- "18-01-3035-45"
df_est$codigo[df_est$codigo =="06-20-0123-45"] <- "09-20-0123-45"
df_est$codigo[df_est$codigo =="05-05-0028-45"] <- "05-04-0028-45"
df_est$codigo[df_est$codigo =="02-05-0549-45"] <- "02-06-0549-45"
#df_est$codigo[df_est$codigo =="05-04-0040-45"] <- "05-04-0040-45"
df_est$codigo[df_est$codigo =="091703346"] <- "09-17-0033-45"
df_est$codigo[df_est$codigo =="05-04-0038-45"] <- "04-04-0028-45"
df_est$codigo[df_est$codigo %in% c("09-93-3624-45", "09-03-3625-45")] <- "09-03-3624-45"
df_est$codigo[df_est$codigo %in% c("11-02-00049-45", "11-02-0040-45")] <- "11-02-0049-45"




df_est$codigo[df_est$codigo =="14-05-0016-45"] <- "14-05-0067-45"
df_est$codigo[df_est$codigo =="11-01-0076-45"] <- "11-01-1176-45"
df_est$codigo[df_est$codigo =="04-04-28-45"] <- "04-04-0028-45"
df_est$codigo[df_est$codigo %in% c("18-04-2515-45", "18-2514-45")] <- "18-04-2514-45"


df_est$codigo[df_est$codigo =="13-07-16-51-45-"] <- "13-07-1651-45"
df_est$codigo[df_est$codigo == "11-09-1421-45⁸⁸"] <- "11-09-1421-45"
df_est$codigo[df_est$codigo == "34-55-45"] <- "17-06-3455-45"

pedro_molina <- c("04-0044-45","04-02-0044-45","04-01-0045-45","04-01-0044-","04-01-0045-45")
df_est$codigo[df_est$codigo %in% pedro_molina] <- "04-01-0044-45"



escribano <- c("09-19-0437-45","09-19-0437-45","09-19-0637-45")
df_est$codigo[df_est$codigo %in% escribano] <- "09-16-0437-45"



df_est$codigo[df_est$codigo == "22-22-12-1506-45"] <- "22-12-1506-45"

# df_est$codigo[df_est$centro == "INEB DE TELESECUNDARIA",
#               df_est$codigo_aplicador == "SC01",
#               df_est$start == "2025-08-19 15:25:21"] <- "22-12-1506-45"

df_est |> 
  filter(str_detect(centro, "INEB DE TELESECUNDARIA"),
                      codigo_aplicador == "SC01") %>%
  select(equipo, codigo, start)

df_est$codigo[df_est$codigo == "01-11-01-0075-45"] <- "11-01-0075-45"





df_est$codigo[df_est$centro == "NÚCLEO FAMILIAR EDUCATIVO PARA EL DESARROLLO NUFED No. 384"] <- "04-07-3187-45"

df_est$codigo[df_est$centro == "Colegio Particular Mixto de Infantes Emanuel"] <- "15-01-0101-45"
df_est$codigo[df_est$centro == "Instituto Mixto Municipal de Educación Básica"] <- "05-04-0028-45"
df_est$codigo[df_est$centro == "NUFED 350"] <- "10-13-2651-45"


df_est$codigo[df_est$codigo == "00-01-0381-45\n"] <- "00-01-0381-45"
df_est$codigo[df_est$codigo == "\n05-04-0040-45"] <- "05-04-0040-45"



df_est$codigo[df_est$codigo == "22-01-02226-45"] <- "22-01-0226-45"
df_est$codigo[df_est$codigo == "00-04-0019-45"] <- "09-04-0019-45"
df_est$codigo[df_est$codigo == "09-16-0437"] <- "09-16-0437-45"
df_est$codigo[df_est$codigo == "14-01-5914-45"] <- "14-13-5914-45"
df_est$codigo[df_est$codigo == "14-01-592145"] <- "14-01-5921-45"
df_est$codigo[df_est$codigo == "09-04-3541-35"] <- "09-04-3541-45"
df_est$codigo[df_est$codigo == "09-04-3541-41"] <- "09-04-3541-45"
df_est$codigo[df_est$codigo == "17-01-0058-45"] <- "17-01-0059-45"
df_est$codigo[df_est$codigo == "18-09-4013-45"] <- "17-09-4013-45"
df_est$codigo[df_est$codigo == "17-10-02-68"] <- "17-10-0268-45"
df_est$codigo[df_est$codigo == "17-04-3957-45"] <- "17-04-3057-45"
df_est$codigo[df_est$codigo == "06-01-1380-45"] <- "16-01-1370-45"
df_est$codigo[df_est$codigo == "13-02-0314-45"] <- "13-02-0214-45"
df_est$codigo[df_est$codigo == "13-12-59-45"] <- "13-12-5946-45"
df_est$codigo[df_est$codigo == "21-91-0103-45"] <- "21-01-0103-45"
df_est$codigo[df_est$codigo == "10-06-1057-45"] <- "19-06-1057-45"
df_est$codigo[df_est$codigo == "09011082"] <- "09-01-1082-45"
df_est$codigo[df_est$codigo == "20-07-010445"] <- "20-07-0104-45"
df_est$codigo[df_est$codigo == "22-01-2089-"] <- "22-01-2089-45"
df_est$codigo[df_est$codigo =="04-15-4533-45"] <- "14-15-4533-45"
df_est$codigo[df_est$codigo =="18-01-3935-45"] <- "18-01-3035-45"
df_est$codigo[df_est$codigo =="06-20-0123-45"] <- "09-20-0123-45"
df_est$codigo[df_est$codigo =="01-10-02-3-45"] <- "02-06-0559-45"
df_est$codigo[df_est$codigo %in% c("08-06-066-45", "08-06-0066", "08-03-0553")] <- "08-06-0066-45"
df_est$codigo[df_est$codigo == "21-01-12013-45"] <- "21-01-1203-45"
df_est$codigo[df_est$codigo == "21-01-1510<45"] <- "21-01-1510-45"
df_est$codigo[df_est$codigo %in% c("21-91-0817-45", "22-01-0817-45")] <- "21-01-0817-45"
df_est$codigo[df_est$codigo == "18-03-3185-44"] <- "18-03-3185-45"
df_est$codigo[df_est$codigo == "08-08-2453-45"] <- "08-06-2453-45"
df_est$codigo[df_est$codigo =="14-1-6025-45"] <- "14-01-6025-45"
df_est$codigo[df_est$codigo %in% c("14-06-7097-45", "13-05-7097-45", "13-06-7997")] <- "13-06-7097-45"
df_est$codigo[df_est$codigo %in% c("13-06-0064-45", "13-06-00-45", "14-06-0069")] <- "13-06-0069-45"
df_est$codigo[df_est$codigo =="17-04=3057=46"] <- "17-04-3057-45"



df_est$codigo[df_est$codigo == "18-92-0418-45"] <- "18-02-0418-45"
df_est$codigo[df_est$codigo == "18-92-0518-45"] <- "18-02-0418-45"

df_est$codigo[df_est$codigo == "18-01-0207-45"] <- "18-02-0207-45"
df_est$codigo[df_est$codigo == "18-01-0208-45"] <- "18-02-0207-45"

df_est$codigo[df_est$codigo == "08-01-010245"] <- "08-01-0102-45"

df_est$codigo[df_est$codigo == "13-24-0011-15"] <- "13-24-0011-45"
df_est$codigo[df_est$codigo == "13-24-00145"] <- "13-24-0011-45"




#Corrección códigos con nombres
df_est$codigo[df_est$codigo == "Aldea-X0c0c"] <- "15-03-0068-45"
df_est$codigo[df_est$codigo == "INEBE-ANT0NI0-LARRAZABAL"] <- "03-01-0061-45"
df_est$codigo[df_est$codigo == "13-AVENIDA-3-76-Z0NA-7-C0L0NIA-QUINTA-SAMAY0A"] <- "00-07-0166-45"
df_est$codigo[df_est$codigo == "INEB-p0r-C00perativa"] <- "00-25-0013-45"




# anti join ----

mal_codificados <-  df_est |> 
  anti_join(muestra_campo, by = "codigo")

# volarse a los códigos que nada que ver ----

codigos_nada_que_ver <- mal_codificados$codigo[]


df_est <- df_est |> 
  filter(!(codigo %in% codigos_nada_que_ver))


# writexl

write_xlsx(df_est, here("Datos","Encuesta estudiantes","df_est.xlsx"))