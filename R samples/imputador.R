library(tidyverse)
library(readxl)

df <- as.data.frame(read_excel("~/Documents/datasets/digi_2022/Datos_digi_2022_2.xlsx"))
df["nombre"] <- NA # que los vacíos sean NA

#comprobación inicial de vacíos

sum(is.na(df[c(1:33),c(163:262)]))
names(which(colSums(is.na(df[c(1:33),c(163:262)]))>0))

#iputación con el anterior

for (i in 163:262) {
  if (i + 1 <= ncol(df) && (grepl("[A-Za-z]1_", colnames(df)[i]) == F && 
                            grepl("_1_", colnames(df)[i]) == F)) {
    df[1:33,i][is.na(df[1:33,i])] <- df[1:33,i-1][is.na(df[1:33,i])]
  }
}

#comprobación después de imputar con el anterior

sum(is.na(df[c(1:33),163:262]))
names(which(colSums(is.na(df[c(1:33),c(163:262)]))>0))

#imputación con el siguiente

for (i in 163:262) {
  if (i + 1 <= ncol(df) && (grepl("[A-Za-z]1_", colnames(df)[i]) == T || 
    grepl("_1_", colnames(df)[i]) == T)) {
    df[1:33,i][is.na(df[1:33,i])] <- df[1:33,i+1][is.na(df[1:33,i])]
  }
}

#comprobación después de imputar con el siguiente

sum(is.na(df[c(1:33),c(163:262)]))
names(which(colSums(is.na(df[c(1:33),c(163:262)]))>0))

