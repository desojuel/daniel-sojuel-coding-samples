library(dplyr)
library(psych)
library(GPArotation)
library(corrplot)
library(ggcorrplot)
library(plyr)
library(crosstable)
library("writexl")
library(ggstatsplot)
library(Rcpp)
library(lavaan)
library(semPlot)
library(ltm)
library(Hmisc)
library(ggplot2)
library(hrbrthemes)

setwd("D:/UVG 2021/Tesis/base_fina/analisis_tesis")

rm(list = ls())

df <- read.csv2("D:/UVG 2021/Tesis/base_fina/actualanswers/Autorregulación del aprendizaje durante la educación remota en emergencia ERT.csv", header=TRUE, encoding = "UTF-8", na.strings=c(""," ","NA"))
df[101,3] <- 18

indicadores_cfa = c("RMSEA", "RMSEA IC superior", "RMSEA IC inferior", "SRMR", "CFI", "TLI", "ECVI", "AIC", "X2 (df)", "Razón X2/df")
criterios_ajuste_cfa = c("<0.08", "<0.1", "<0.06", "≥0.08", "≥0.95", "0.95", "Al comparar modelos, el más bajo es mejor", "Al comparar modelos, el más bajo es mejor", "", "Entre 2 y 3")
revertir = 8

#revertir mslq

df$mslq_gte_02r <- revertir - df$mslq_gte_02
df$mslq_gte_06r <- revertir - df$mslq_gte_06

################ analisis factorial confirmatorio AFM

grep("^afm_11$", colnames(df)) #21

# afm 4 factores

g_afm_original_factores='
gusto_interacciones =~ afm_1 + afm_2 + afm_3 + afm_4 + afm_5 + afm_6 + afm_7
autoeficacia_interacciones_instructor =~ afm_8 + afm_9 + afm_10
preocupaciones_interaccion_pares =~ afm_11 + afm_12 + afm_13 + afm_14
autoeficacia_contribuciones_enlinea =~ afm_15 + afm_16 + afm_17
'

fit_afm_original_factores = cfa(g_afm_original_factores, data=df)

summary(fit_afm_original_factores, fit.measures=TRUE)


fitmeasures(fit_afm_original_factores)

semPaths(fit_afm_original_factores,
         whatLabels = "std",
         layout = "tree")

#afm 3 factores

g_afm_3factores='
gusto_interacciones =~ afm_2 + afm_4 + afm_5 + afm_6
autoeficacia_interacciones_instructor =~ afm_8 + afm_9 + afm_10
preocupaciones_interaccion_pares =~ afm_15 + afm_16 + afm_17
'

fit_g_afm_3factores = cfa(g_afm_3factores, data=df)

summary(fit_g_afm_3factores, fit.measures=TRUE)

fitafm = fitmeasures(fit_g_afm_3factores)

semPaths(fit_g_afm_3factores,
         whatLabels = "std",
         layout = "tree")

#afe afm

fa.parallel(df[, c(82,84:86,88:106,118,119)], fm="minres")
efa_afm = fa(df[, c(82,84:86,88:106,118,119)], nfactors =3, rotate="oblimin", fm="minres")
print(efa_afm$loadings,cutoff = 0.3)


################ analisis factorial confirmatorio ICM

# 8 factores 

icm_original_factores ='
c_declarativo =~ icm_cd_01 + icm_cd_02 + icm_cd_03 + icm_cd_04 + icm_cd_05 + icm_cd_06 + icm_cd_07 + icm_cd_08
c_procedimental =~ icm_cp_01 + icm_cp_02 + icm_cp_03 + icm_cp_04
c_condicional =~ icm_cc_01 + icm_cc_02 + icm_cc_03 + icm_cc_04 + icm_cc_05
planificacion =~ icm_pl_01 + icm_pl_02 + icm_pl_03 + icm_pl_04 + icm_pl_05 + icm_pl_06 + icm_pl_07
organizacion =~ icm_or_01 + icm_or_02 + icm_or_03 + icm_or_04 + icm_or_05 + icm_or_06 + icm_or_07 + icm_or_08 + icm_or_09 + icm_or_10
monitoreo =~ icm_mo_01 + icm_mo_02 + icm_mo_03 + icm_mo_04 + icm_mo_05 + icm_mo_06 + icm_mo_07
depuracion =~ icm_dep_01 + icm_dep_02 + icm_dep_03 + icm_dep_04 + icm_dep_05
evaluacion =~ icm_ev_01 + icm_ev_02 + icm_ev_03 + icm_ev_04 + icm_ev_05 + icm_ev_06
'

fit_icm_original_factores = cfa(icm_original_factores, data=df)
summary(fit_icm_original_factores, fit.measures=TRUE)
fitmeasures(fit_icm_original_factores)

#tabla de indicadores de ajuste

2942.449/1246.000

indicadores_cfa_icm_original = c(0.073,  0.077, 0.070, 0.075, 0.765, 0.750, 12.624, 42415.916, "2942.449 (1246)", 2.361516)

# 2 factores 8 items

grep("^icm_cd_01$", colnames(df)) #29
grep("^icm_cd_07$", colnames(df)) #35
grep("^icm_cp_03$", colnames(df)) #39
grep("^icm_cc_05$", colnames(df)) #45

grep("^icm_pl_03$", colnames(df)) #48
grep("^icm_or_08$", colnames(df)) #60
grep("^icm_mo_06$", colnames(df)) #68
grep("^icm_ev_06$", colnames(df)) #80

icm_2_factores ='
f1 =~ icm_cd_07 + icm_cp_03 + icm_cc_01 + icm_cc_02 + icm_cc_05
f2 =~ icm_pl_03 + icm_or_08 + icm_mo_06 + icm_ev_06 + icm_dep_04
'

fit_icm_2_factores = cfa(icm_2_factores, data=df)
summary(fit_icm_2_factores, fit.measures=TRUE)
fitmeasures(fit_icm_2_factores)

58.845/34

indicadores_cfa_icm_2 = c(0.054,  0.076, 0.029, 0.047, 0.960, 0.947, 0.397, 8530.622, "58.845 (34)", 1.730735)


# 2 factores 19 items

icm_2_factores19 ='
kc =~ icm_cd_02 + icm_cd_04 + icm_cd_06 + icm_cd_07 + icm_cp_03 + icm_cp_04 + icm_cc_03 + icm_cc_05
rc =~ icm_pl_02 + icm_pl_03 + icm_or_06 + icm_or_07 + icm_or_08 + icm_mo_04 + icm_dep_02 + icm_dep_03 + icm_dep_04 + icm_ev_03 + icm_ev_06
'

fit_icm_2_factores19 = cfa(icm_2_factores19, data=df)
summary(fit_icm_2_factores19, fit.measures=TRUE)
fitmeasures(fit_icm_2_factores19)

indicadores_cfa_icm_2_19 = c(0.101,  0.110, 0.091, 0.076, 0.786, 0.758, 2.428, 15906.720, "538.740 (151)", 3.567815)

538.740 / 151

tabla_indicadores_cfa_icm <- data.frame(indicadores_cfa, indicadores_cfa_icm_original, indicadores_cfa_icm_2_19, indicadores_cfa_icm_2, criterios_ajuste_cfa)

################ analisis factorial confirmatorio MSLQ

# original 4 factores

mslq_original_factores ='
gestion_tiempo_esfuerzo =~ mslq_gte_01 + mslq_gte_02r + mslq_gte_03 + mslq_gte_04 + mslq_gte_05 +  mslq_gte_06r 
uso_estrategias_cognitivas_complejas =~ mslq_ecc_01 + mslq_ecc_02 + mslq_ecc_03 + mslq_ecc_04 + mslq_ecc_05 + mslq_ecc_06 + mslq_ecc_07 + mslq_ecc_08 + mslq_ecc_09 + mslq_ecc_10
uso_estrategias_cognitivas_simples =~ mslq_ecc_06 + mslq_ecc_07 + mslq_ecc_08 + mslq_ecc_09 + mslq_ecc_10
contacto_con_pares =~ mslq_ccp_01 + mslq_ccp_02 + mslq_ccp_03 + mslq_ccp_04
pensamiento_academico =~ mslq_pa_01 + mslq_pa_02 + mslq_pa_03 + mslq_pa_04 + mslq_pa_05
'

fit_mslq_original_factores = cfa(mslq_original_factores, data=df)
summary(fit_mslq_original_factores, fit.measures=TRUE)
fitmeasures(fit_mslq_original_factores)

indicadores_cfa_mslq_orignal = c(0.082,  0.089, 0.075, 0.080, 0.843, 0.819 , 3.279, 22609.661, "702.968 (260)", 2.703723)
702.968/260

grep("^mslq_gte_02r$", colnames(df)) #118
grep("^mslq_gte_06r$", colnames(df)) #119

#efa mslq

fa.parallel(df[, c(82,84:86,88:106,118,119)], fm="minres")
efa_mslq = fa(df[, c(82,84:86,88:106,118,119)], nfactors = 5, rotate="oblimin", fm="minres")
print(efa_mslq$loadings,cutoff = 0.3)

#cfa mslq nuevo 

mslq_factores_4 ='
gestion_tiempo_esfuerzo =~ mslq_gte_01 + mslq_gte_03 + mslq_gte_04 + mslq_gte_05 
uso_estrategias_cognitivas_complejas =~ mslq_ecc_01 + mslq_ecc_02 + mslq_ecc_03 + mslq_ecc_04
uso_estrategias_cognitivas_simples =~ mslq_ecc_08 + mslq_ecc_09 + mslq_ecc_10
contacto_con_pares =~ mslq_ccp_01 + mslq_ccp_02 + mslq_ccp_04
pensamiento_academico =~ mslq_pa_02 + mslq_pa_03 + mslq_pa_04
'
fit_mslq_factores_4 = cfa(mslq_factores_4, data=df)
summary(fit_mslq_factores_4, fit.measures=TRUE)
fitmeasures(fit_mslq_factores_4)

indicadores_cfa_mslq_reduccion = c(0.069, 0.081, 0.057, 0.065, 0.920, 0.901, 1.293, 15323.241, "240.440 (109)", 2.205872)

240.440/109

tabla_indicadores_cfa_mslq <- data.frame(indicadores_cfa, indicadores_cfa_mslq_orignal, indicadores_cfa_mslq_reduccion, criterios_ajuste_cfa)

################ calcular variables según afc

####### afm

df$afm_f1 <- df$afm_2 + df$afm_4 + df$afm_5 + df$afm_6
df$afm_f2 <- df$afm_8 + df$afm_9 + df$afm_10
df$afm_f3 <- df$afm_15 + df$afm_16 + df$afm_17
df$total_afm <- df$afm_f1 + df$afm_f2 + df$afm_f3

variables_afm_f1 <- c(12,14:16)
variables_afm_f2 <- c(18:20)
variables_afm_f3 <- c(25:27)
variables_afm = c(12,14:16,18:20,25:27)

grep("^afm_2$", colnames(df)) #12
grep("^afm_4$", colnames(df)) #14
grep("^afm_5$", colnames(df)) #15
grep("^afm_6$", colnames(df)) #16
grep("^afm_8$", colnames(df)) #18
grep("^afm_9$", colnames(df)) #19
grep("^afm_10$", colnames(df)) #20
grep("^afm_15$", colnames(df)) #25
grep("^afm_16$", colnames(df)) #26
grep("^afm_17$", colnames(df)) #27

shapiro.test(df$total_afm)

####### icm

df$mai_cogk <- df$icm_cd_07 + df$icm_cp_03 + df$icm_cc_01 + df$icm_cc_02 + df$icm_cc_05
df$mai_cogr <- df$icm_pl_03 + df$icm_or_08 + df$icm_mo_06 + df$icm_ev_06 + df$icm_dep_04
df$total_mai <- df$icm_cogk + df$icm_cogr

grep("^icm_cd_07$", colnames(df)) #35
grep("^icm_cp_03$", colnames(df)) #39
grep("^icm_cc_01$", colnames(df)) #41
grep("^icm_cc_02$", colnames(df)) #42
grep("^icm_cc_05$", colnames(df)) #45

grep("^icm_pl_03$", colnames(df)) #48
grep("^icm_or_08$", colnames(df)) #60
grep("^icm_mo_06$", colnames(df)) #68
grep("^icm_dep_04$", colnames(df)) #73
grep("^icm_ev_06$", colnames(df)) #80


variables_icm_f1 = c(35,39,41,42,45)
variables_icm_f2 = c(48,60,68,73,80)
variables_icm = c(35,39,41,42,45, 48,60,68,73,80)


shapiro.test(df$total_icm)

####### mslq

grep("^mslq_gte_01$", colnames(df)) #82
grep("^mslq_gte_05$", colnames(df)) #86

grep("^mslq_ecc_01$", colnames(df)) #88

grep("^mslq_ecc_08$", colnames(df)) #95

grep("^mslq_ccp_04$", colnames(df)) #101

grep("^mslq_pa_02$", colnames(df)) #103

df$mslq_f1 <- df$mslq_gte_01 + df$mslq_gte_03 + df$mslq_gte_04 + df$mslq_gte_05
df$mslq_f2 <- df$mslq_ecc_01 + df$mslq_ecc_02 + df$mslq_ecc_03 + df$mslq_ecc_04
df$mslq_f3 <- df$mslq_ecc_08 + df$mslq_ecc_09 + df$mslq_ecc_10
df$mslq_f4 <- df$mslq_ccp_01 + df$mslq_ccp_02 + df$mslq_ccp_04
df$mslq_f5 <- df$mslq_pa_02 + df$mslq_pa_03 + df$mslq_pa_04
df$total_mslq <- df$mslq_f1 + df$mslq_f2 + df$mslq_f3 + df$mslq_f4 + df$mslq_f5


variables_mslq_f1 = c(82,84:86)
variables_mslq_f2 = c(88:91)
variables_mslq_f3 = c(95:97)
variables_mslq_f4 = c(98,99,101)
variables_mslq_f5 = c(103:105)
variables_mslq = c(82,84:86,88:91,95:97,98,99,101,103:105)


shapiro.test(df$total_mslq)

################### correlación entre escalas

var(df$afm_f1)
var(df$afm_f2)
var(df$afm_f3)

var(df$afm_f1)/var(df$afm_f2)
var(df$afm_f1)/var(df$afm_f3)
var(df$afm_f3)/var(df$afm_f2)

#afm

df_correlaciones_afm <- df[ ,c("afm_f1","afm_f2","afm_f3")]

# Compute a matrix of correlation p-values
rcorr_afm = rcorr(as.matrix(df_correlaciones_afm),type="pearson")
p.mat <- rcorr_afm$P
p.mat
print(rcorr_afm$P, digits=10)
figura_correlacion_afm <- ggcorrplot(rcorr_afm$r, hc.order = TRUE, type = "lower", outline.col = "white", lab = TRUE, p.mat = p.mat)
figura_correlacion_afm

df_correlaciones_afm_spearman <- df[ ,c("afm_f1","afm_f2","afm_f3")]

# Compute a matrix of correlation p-values
rcorr_afm = rcorr(as.matrix(df_correlaciones_afm_spearman),type="spearman")
p.mat <- rcorr_afm$P
print(rcorr_afm$P, digits=10)
figura_correlacion_afm <- ggcorrplot(rcorr_afm$r, hc.order = TRUE, type = "lower", outline.col = "white", lab = TRUE, p.mat = p.mat)
figura_correlacion_afm

ggsave("figura_correlacion_afm.png")

#icm

df_correlaciones_imc <- df[ ,c("mai_cogk", "mai_cogr")]

# Compute a matrix of correlation p-values
rcorr_icm = rcorr(as.matrix(df_correlaciones_imc),type="spearman")
p.mat <- rcorr_icm$P
print(rcorr_icm$P, digits=10)
figura_correlacion_icm <- ggcorrplot(rcorr_icm$r, hc.order = TRUE, type = "lower", outline.col = "white", lab = TRUE, p.mat = p.mat)
figura_correlacion_icm

ggsave("figura_correlacion_icm.png")

#mslq

df_correlaciones_mslq <- df[ ,c("mslq_f1", "mslq_f2", "mslq_f3", "mslq_f4", "mslq_f5")]
df_corrmatrix_mslq <- cor(df_correlaciones_mslq, method = "pearson")
df_corrmatrix_mslq
# Compute a matrix of correlation p-values
rcorr_mslq = rcorr(as.matrix(df_correlaciones_mslq),type="pearson")
p.mat <- rcorr_mslq$P
print(rcorr_mslq$P, digits=2)
figura_correlacion_mslq <- ggcorrplot(df_corrmatrix_mslq, hc.order = TRUE, type = "lower", outline.col = "white", lab = TRUE, p.mat = p.mat)
figura_correlacion_mslq

ggsave("figura_correlacion_mslq.png")


########################################################### correlaciones otra vez


#afm

df_correlaciones2_afm <- df[ ,c("afm_f1","afm_f2","afm_f3")]

# Compute a matrix of correlation p-values
rcorr_afm2 = rcorr(as.matrix(df_correlaciones2_afm),type="pearson")
p.mat <- rcorr_afm2$P
figura_correlacion_afm2 <- ggcorrplot(rcorr_afm2$r, hc.order = TRUE, type = "lower", outline.col = "white", lab = TRUE, p.mat = p.mat)
figura_correlacion_afm2

ggsave("figura_correlacion_afm2.png")

#la correlación no es fueret entre escalas + la confiabilidad con un factor unitario es muy baja, por eso se presentarán los datos por separado

#icm

df_correlaciones_imc <- df[ ,c("icm_cogk", "icm_cogr")]

# Compute a matrix of correlation p-values
rcorr_icm = rcorr(as.matrix(df_correlaciones_imc),type="pearson")
p.mat <- rcorr_icm$P
print(rcorr_icm$P, digits=10)
figura_correlacion_icm <- ggcorrplot(df_corrmatrix_icm, hc.order = TRUE, type = "lower", outline.col = "white", lab = TRUE, p.mat = p.mat)
figura_correlacion_icm

ggsave("figura_correlacion_icm.png")

#mslq

df_correlaciones_mslq <- df[ ,c("mslq_f1", "mslq_f2", "mslq_f3", "mslq_f4", "mslq_f5")]
df_corrmatrix_mslq <- cor(df_correlaciones_mslq, method = "pearson")
df_corrmatrix_mslq
# Compute a matrix of correlation p-values
rcorr_mslq = rcorr(as.matrix(df_correlaciones_mslq),type="pearson")
p.mat <- rcorr_mslq$P
print(rcorr_mslq$P, digits=2)
figura_correlacion_mslq <- ggcorrplot(df_corrmatrix_mslq, hc.order = TRUE, type = "lower", outline.col = "white", lab = TRUE, p.mat = p.mat)
figura_correlacion_mslq

ggsave("figura_correlacion_mslq.png")

#################################################################################### CONFIABILIDAD

#### afm 
# escala total
omega(df[ ,variables_afm],3) #omega .88 --- alpha .75

alpha_afm = cronbach.alpha(df[ ,variables_afm], standardized = TRUE, CI=TRUE) 
alpha_afm
round(alpha_afm$alpha, digits=2)

alpha_afm_f1 = cronbach.alpha(df[ ,variables_afm_f1], standardized = TRUE, CI=TRUE) 
alpha_afm_f1
round(alpha_afm_f1$alpha, digits=2)

alpha_afm_f2 = cronbach.alpha(df[ ,variables_afm_f2], standardized = TRUE, CI=TRUE)
alpha_afm_f2
round(alpha_afm_f2$alpha, digits=2)
alpha_afm_f3 = cronbach.alpha(df[ ,variables_afm_f3], standardized = TRUE, CI=TRUE)
alpha_afm_f3
round(alpha_afm_f3$alpha, digits=2)


nombres_escalas = c("AFM general", "alpha afm_f1", "alpha afm_f2", "alpha afm_f3", 
                    "MAI general", "alpha mai_f1", "alpha mai_f2", 
                    "MSLQ general", "alpha mslq_f1", "alpha mslq_f2", "alpha mslq_f3", "alpha mslq_f4","alpha mslq_f5")

#### icm
#escala total
omega(df[ ,c(variables_icm)],2, two.ok = TRUE) # omega .84 --- alpha .81

# factores separados
alpha_icm = cronbach.alpha(df[ ,c(variables_icm)], standardized = TRUE, CI=TRUE) 
alpha_icm
round(alpha_afm$alpha, digits=2)
alpha_icm_f1 = cronbach.alpha(df[ ,c(variables_icm_f1)],   CI=TRUE) 
alpha_icm_f1
round(alpha_icm_f1$alpha, digits=2)
alpha_icm_f2 = cronbach.alpha(df[ ,c(variables_icm_f2)],   CI=TRUE) 
alpha_icm_f2
round(alpha_icm_f2$alpha, digits=2)



#### mslq
# escala total
omega(df[ ,c(variables_mslq),5]) # omega .85 --- alpha .77

# factores separados

alpha_mslq = cronbach.alpha(df[ ,c(variables_mslq)], standardized = TRUE, CI=TRUE) 
alpha_mslq
round(alpha_mslq$alpha, digits=2)
alpha_mslq_f1 = cronbach.alpha(df[ ,c(variables_mslq_f1)],   CI=TRUE) 
alpha_mslq_f1
round(alpha_mslq_f1$alpha, digits=2)

alpha_mslq_f2 = cronbach.alpha(df[ ,c(variables_mslq_f2)],   CI=TRUE) 
alpha_mslq_f2
round(alpha_mslq_f2$alpha, digits=2)

alpha_mslq_f3 = cronbach.alpha(df[ ,c(variables_mslq_f3)],   CI=TRUE) 
alpha_mslq_f3
round(alpha_mslq_f3$alpha, digits=2)

alpha_mslq_f4 = cronbach.alpha(df[ ,c(variables_mslq_f4)],   CI=TRUE) 
alpha_mslq_f4
round(alpha_mslq_f4$alpha, digits=2)

round(alpha_mslq_f4$ci, digits=2)[1]

alpha_mslq_f5 = cronbach.alpha(df[ ,c(variables_mslq_f5)],   CI=TRUE) 
alpha_mslq_f5

confiabilidad_escalas = c(round(alpha_afm$alpha, digits=2),
                          round(alpha_afm_f1$alpha, digits=2),
                          round(alpha_afm_f2$alpha, digits=2),
                          round(alpha_afm_f3$alpha, digits=2),
                          round(alpha_icm$alpha, digits=2),
                          round(alpha_icm_f1$alpha, digits=2),
                          round(alpha_icm_f2$alpha, digits=2),
                          round(alpha_mslq$alpha, digits=2),
                          round(alpha_mslq_f1$alpha, digits=2), 
                          round(alpha_mslq_f2$alpha, digits=2),
                          round(alpha_mslq_f3$alpha, digits=2),
                          round(alpha_mslq_f4$alpha, digits=2),
                          round(alpha_mslq_f5$alpha, digits=2))

confiabilidad_escalas_ci_l = c(round(alpha_afm$ci, digits=2)[1],
                               round(alpha_afm_f1$ci, digits=2)[1],
                               round(alpha_afm_f2$ci, digits=2)[1],
                               round(alpha_afm_f3$ci, digits=2)[1],
                               round(alpha_icm$ci, digits=2)[1],
                               round(alpha_icm_f1$ci, digits=2)[1],
                               round(alpha_icm_f2$ci, digits=2)[1],
                               round(alpha_mslq$ci, digits=2)[1],
                               round(alpha_mslq_f1$ci, digits=2)[1],
                               round(alpha_mslq_f2$ci, digits=2)[1],
                               round(alpha_mslq_f3$ci, digits=2)[1],
                               round(alpha_mslq_f4$ci, digits=2)[1],
                               round(alpha_mslq_f5$ci, digits=2)[1])

confiabilidad_escalas_ci_u = c(round(alpha_afm$ci, digits=2)[2],
                               round(alpha_afm_f1$ci, digits=2)[2],
                               round(alpha_afm_f2$ci, digits=2)[2],
                               round(alpha_afm_f3$ci, digits=2)[2],
                               round(alpha_icm$ci, digits=2)[2],
                               round(alpha_icm_f1$ci, digits=2)[2],
                               round(alpha_icm_f2$ci, digits=2)[2],
                               round(alpha_mslq$ci, digits=2)[2],
                               round(alpha_mslq_f1$ci, digits=2)[2],
                               round(alpha_mslq_f2$ci, digits=2)[2],
                               round(alpha_mslq_f3$ci, digits=2)[2],
                               round(alpha_mslq_f4$ci, digits=2)[2],
                               round(alpha_mslq_f5$ci, digits=2)[2])

tabla_confiabilidad <- data.frame(nombres_escalas, confiabilidad_escalas_ci_l, confiabilidad_escalas, confiabilidad_escalas_ci_u)


######################################################################### medias, medianas, ds, min, max para escalas y subescalas

media_afm = round(mean(df$total_afm), digits = 2)
media_afm_f1 = round(mean(df$afm_f1), digits = 2)
media_afm_f2 = round(mean(df$afm_f2), digits = 2)
media_afm_f3 = round(mean(df$afm_f3), digits = 2)

mediana_afm = round(median(df$total_afm), digits = 2)
mediana_afm_f1 = round(median(df$afm_f1), digits = 2)
mediana_afm_f2 = round(median(df$afm_f2), digits = 2)
mediana_afm_f3 = round(median(df$afm_f3), digits = 2)

sd_afm = round(sd(df$total_afm), digits = 2)
sd_afm_f1 = round(sd(df$afm_f1), digits = 2)
sd_afm_f2 = round(sd(df$afm_f2), digits = 2)
sd_afm_f3 = round(sd(df$afm_f3), digits = 2)

min_afm = min(df$total_afm)
min_afm_f1 = min(df$afm_f1)
min_afm_f2 = min(df$afm_f2)
min_afm_f3 = min(df$afm_f3)

max_afm = max(df$total_afm)
max_afm_f1 = max(df$afm_f1)
max_afm_f2 = max(df$afm_f2)
max_afm_f3 = max(df$afm_f3)

media_icm = round(mean(df$total_icm), digits = 2)
media_icm_f1 = round(mean(df$icm_cogk), digits = 2)
media_icm_f2 = round(mean(df$icm_cogr), digits = 2)

mediana_icm = round(median(df$total_icm), digits = 2)
mediana_icm_f1 = round(median(df$icm_cogk), digits = 2)
mediana_icm_f2 = round(median(df$icm_cogr), digits = 2)

sd_icm = round(sd(df$total_icm), digits = 2)
sd_icm_f1 = round(sd(df$icm_cogk), digits = 2)
sd_icm_f2 = round(sd(df$icm_cogr), digits = 2)

min_icm = min(df$total_icm)
min_icm_f1 = min(df$icm_cogk)
min_icm_f2 = min(df$icm_cogr)

max_icm = max(df$total_icm)
max_icm_f1 = max(df$icm_cogk)
max_icm_f2 = max(df$icm_cogr)

media_mslq = round(mean(df$total_mslq),digits = 2)
media_mslq_f1 = round(mean(df$mslq_f1),digits = 2)
media_mslq_f2 = round(mean(df$mslq_f2),digits = 2)
media_mslq_f3 = round(mean(df$mslq_f3),digits = 2)
media_mslq_f4 = round(mean(df$mslq_f4),digits = 2)
media_mslq_f5 = round(mean(df$mslq_f5),digits = 2)

mediana_mslq = round(median(df$total_mslq),digits = 2)
mediana_mslq_f1 = round(median(df$mslq_f1),digits = 2)
mediana_mslq_f2 = round(median(df$mslq_f2),digits = 2)
mediana_mslq_f3 = round(median(df$mslq_f3),digits = 2)
mediana_mslq_f4 = round(median(df$mslq_f4),digits = 2)
mediana_mslq_f5 = round(median(df$mslq_f5),digits = 2)

sd_mslq = round(sd(df$total_mslq),digits = 2)
sd_mslq_f1 = round(sd(df$mslq_f1),digits = 2)
sd_mslq_f2 = round(sd(df$mslq_f2),digits = 2)
sd_mslq_f3 = round(sd(df$mslq_f3),digits = 2)
sd_mslq_f4 = round(sd(df$mslq_f4),digits = 2)
sd_mslq_f5 = round(sd(df$mslq_f5),digits = 2)

min_mslq = min(df$total_mslq)
min_mslq_f1 = min(df$mslq_f1)
min_mslq_f2 = min(df$mslq_f2)
min_mslq_f3 = min(df$mslq_f3)
min_mslq_f4 = min(df$mslq_f4)
min_mslq_f5 = min(df$mslq_f5)

max_mslq = max(df$total_mslq)
max_mslq_f1 = max(df$mslq_f1)
max_mslq_f2 = max(df$mslq_f2)
max_mslq_f3 = max(df$mslq_f3)
max_mslq_f4 = max(df$mslq_f4)
max_mslq_f5 = max(df$mslq_f5)

nombres_escalas_descriptivos = c("AFM General", "AFM Factor 1", "AFM Factor 2", "AFM Factor 3",
                                 "MAI General", "MAIL Factor 1", "MAI Factor 2",
                                 "MSLQ General", "MAI Factor 1", "MAI Factor 2", "MAI Factor 3", "MAI Factor 4", "MAI Factor 5")

no_items = c(length(variables_afm), length(variables_afm_f1), length(variables_afm_f2), length(variables_afm_f3),
             length(variables_icm), length(variables_icm_f1), length(variables_icm_f2),
             length(variables_mslq), length(variables_mslq_f1), length(variables_mslq_f2), length(variables_mslq_f3), length(variables_mslq_f4), length(variables_mslq_f5))


medias_escalas = c(media_afm, media_afm_f1, media_afm_f2, media_afm_f3, 
                   media_icm, media_icm_f1, media_icm_f2,
                   media_mslq, media_mslq_f1, media_mslq_f2, media_mslq_f3, media_mslq_f4, media_mslq_f5)
medianas_escalas = c(mediana_afm, mediana_afm_f1, mediana_afm_f2, mediana_afm_f3, 
                     mediana_icm, mediana_icm_f1, mediana_icm_f2,
                     mediana_mslq, mediana_mslq_f1, mediana_mslq_f2, mediana_mslq_f3, mediana_mslq_f4, mediana_mslq_f5)
sd_escalas = c(sd_afm, sd_afm_f1, sd_afm_f2, sd_afm_f3, 
               sd_icm, sd_icm_f1, sd_icm_f2,
               sd_mslq, sd_mslq_f1, sd_mslq_f2, sd_mslq_f3, sd_mslq_f4, sd_mslq_f5)

min_escalas = c(min_afm, min_afm_f1, min_afm_f2, min_afm_f3, 
                min_icm, min_icm_f1, min_icm_f2,
                min_mslq, min_mslq_f1, min_mslq_f2, min_mslq_f3, min_mslq_f4, min_mslq_f5)

max_escalas = c(max_afm, max_afm_f1, max_afm_f2, max_afm_f3, 
                max_icm, max_icm_f1, max_icm_f2,
                max_mslq, max_mslq_f1, max_mslq_f2, max_mslq_f3, max_mslq_f4, max_mslq_f5)

descriptivos_escalas <- data.frame(nombres_escalas_descriptivos, no_items, medias_escalas, medianas_escalas, sd_escalas, min_escalas, max_escalas)

######################################################################### correlaciones

grep("^total_afm$", colnames(df)) #120
grep("^total_icm$", colnames(df)) #123
grep("^total_mslq$", colnames(df)) #128

df_correlaciones <- df[ ,c(120,123,128)]
#names(df_correlaciones)[1] <- "Afecto y motivación en la interacción social en línea"
#names(df_correlaciones)[2] <- "Conciencia metagocnitiva"
#names(df_correlaciones)[3] <- "Estrategias de aprendizaje"
df_corrmatrix <- cor(df_correlaciones, method = "pearson")

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(df_correlaciones)

# afm vs icm
cor.test(df$total_afm, df$total_icm, method="pearson")
#afm vs mslq
cor.test(df$total_afm, df$total_mslq, method="pearson")
#icm vs mslq
cor.test(df$total_icm, df$total_mslq, method="pearson")

figura_correlacion <- ggcorrplot(df_corrmatrix, hc.order = TRUE, type = "lower", outline.col = "white", lab = TRUE, p.mat = p.mat)
figura_correlacion

ggsave("figura_correlacion.png")


######################################################################## quartiles


df %>% 
  summarise(total_afm = round(quantile(total_afm, c(0.25, 0.5, 0.75))), q = c(0.25, 0.5, 0.75))

df %>% 
  summarise(total_icm = round(quantile(total_icm, c(0.25, 0.5, 0.75))), q = c(0.25, 0.5, 0.75))

df %>% 
  summarise(total_mslq = round(quantile(total_mslq, c(0.25, 0.5, 0.75))), q = c(0.25, 0.5, 0.75))


######################################################################## rangos segun cuartiles

df$cuartiles_afm <- ifelse(df$total_afm <= 41, "q1", 
                           ifelse(df$total_afm <= 50, "q2", 
                                  ifelse(df$total_afm <= 58, "q3","q4")))


df$cuartiles_icm <- ifelse(df$total_icm <= 51, "q1", 
                           ifelse(df$total_icm <= 57, "q2", 
                                  ifelse(df$total_icm <= 63, "q3","q4")))

df$cuartiles_mslq <- ifelse(df$total_mslq <= 77, "q1", 
                           ifelse(df$total_mslq <= 92, "q2", 
                                  ifelse(df$total_mslq <= 102, "q3","q4")))

cuartiles = c("Cuartil 1", "Cuartil 2", "Cuartil 3", "Cuartil 4")

rangos_qs_afm = c("11 a 41", "42 a 50", "51 a 58", "59 a 70")
qs_afm = c(67,67,59,61)

rangos_qs_mai = c("28 a 51", "52 a 57", "58 a 63", "64 a 70")
qs_mai = c(74,61,56,63)

rangos_qs_mslq = c("49 a 77", "78 a 92", "93 a 102", "103 a 119")
qs_mslq = c(70,60,65,59)


tabla_cuartiles_afm <- data.frame(cuartiles, rangos_qs_afm, qs_afm)
tabla_cuartiles_mai <- data.frame(cuartiles, rangos_qs_mai, qs_mai)
tabla_cuartiles_mslq <- data.frame(cuartiles, rangos_qs_mslq, qs_mslq)

tabla_cuartiles_escalas <- cbind(tabla_cuartiles_afm, tabla_cuartiles_mai, tabla_cuartiles_mslq)



######################################################################## rangos segun criterio

df$criterio_afm <- ifelse(df$total_afm <= 24, "q1", 
                           ifelse(df$total_afm <= 39, "q2", 
                                  ifelse(df$total_afm <= 54, "q3","q4")))


df$criterio_icm <- ifelse(df$total_icm <= 24, "q1", 
                           ifelse(df$total_icm <= 39, "q2", 
                                  ifelse(df$total_icm <= 54, "q3","q4")))

df$criterio_mslq <- ifelse(df$total_mslq <= 42, "q1", 
                            ifelse(df$total_mslq <= 68, "q2", 
                                   ifelse(df$total_mslq <= 94, "q3","q4")))

criterios = c("Bajo", "Medio", "Alto", "Muy alto")

rangos_cr_afm = c("10 a 24", "25 a 39", "40 a 54", "55 a 70")
cr_afm = c(10,46,110,88)

rangos_cr_mai = c("10 a 24", "25 a 39", "40 a 54", "55 a 70")

cr_mai = c(0,12,90,152)

rangos_cr_mslq = c("17 a 42", "43 a 68", "69 a 94", "95 a 119")
cr_mslq = c(0,28,117,109)

tabla_criterio_afm <- data.frame(criterios, rangos_cr_afm, cr_afm)
tabla_criterio_mai <- data.frame(criterios, rangos_cr_mai, cr_mai)
tabla_criterio_mslq <- data.frame(criterios, rangos_cr_mslq, cr_mslq)

tabla_criterios_escalas <- cbind(tabla_criterio_afm, tabla_criterio_mai, tabla_criterio_mslq)

######################################################################## estandarizacion

################################### afm

ds_afm = round(sd(df$total_afm), digits = 2)

df$z_afm = round(((df$total_afm - media_afm)/ds_afm), digits = 2)

df %>% 
  summarise(z_afm = round((quantile(z_afm, c(0.25, 0.5, 0.75))), digits =2), q = c(0.25, 0.5, 0.75))

df$cuartiles_z_afm <- ifelse(df$z_afm <= -0.66, "q1", 
                           ifelse(df$z_afm <= 0.09, "q2", 
                                  ifelse(df$z_afm <= 0.63, "q3","q4")))

################################### icm

ds_icm = round(sd(df$total_icm), digits = 2)
df$z_icm = round(((df$total_icm - media_icm)/ds_icm), digits = 2)

df %>%
  summarise(z_icm = round((quantile(z_icm, c(0.25, 0.5, 0.75))),digits=2), q = c(0.25, 0.5, 0.75))

df$cuartiles_z_icm <- ifelse(df$z_icm <= -0.74, "q1", 
                             ifelse(df$z_icm <= -0.02, "q2", 
                                    ifelse(df$z_icm <= 0.86, "q3","q4")))

################################### mslq
ds_mslq = round(sd(df$total_mslq), digits = 2)
df$z_mslq = round(((df$total_mslq - media_mslq)/ds_mslq), digits = 2)

df %>%
  summarise(z_mslq = round((quantile(z_mslq, c(0.25, 0.5, 0.75))),digits=2), q = c(0.25, 0.5, 0.75))

df$cuartiles_z_mslq <- ifelse(df$z_mslq <= -0.72, "q1", 
                             ifelse(df$z_mslq <= 0.08, "q2", 
                                    ifelse(df$z_mslq <= 0.77, "q3","q4")))


######## nombres para mandar resultados

persona <- c("MARIA ISABEL FLORES", "MARIA JUDITH ALFARO ANLEU", "GUADALUPE DE LOS ANGELES ORTIZ ROSALES", 
             "KARLA GISSEL NORIEGA AIFAN", "ESTUARDO ENRIQUE SOTO INTERIANO", "Alex Mendez", 
             "ERICKA IVONNE RODAS RODRIGUEZ", "MARIA MERCEDES ACEVEDO VALDEZ",
             "ANA MARIA Perdomo Cabanas", "Juan Carlos Zambroni", "Helga")
correo <- c("flo14750@uvg.edu.gt", "alf19842@uvg.edu.gt", "ort161028@uvg.edu.gt",
            "nor18625@uvg.edu.gt", "sot21348@uvg.edu.gt", "men21627@uvg.edu.gt", "rod99360@uvg.edu.gt",
            "ace13641@uvg.edu.gt", "per99436@uvg.edu.gt", "chano.el.cuaz@gmail.com", "hramirezdel@uvg.edu.gt")


resultado_afm <- c(df$z_afm[143], df$z_afm[248], df$z_afm[40], NA, df$z_afm[102], df$z_afm[114], NA, df$z_afm[124], df$z_afm[209],df$z_afm[42],NA)
resultado_icm <- c(df$z_icm[143], df$z_icm[248], df$z_icm[40], NA, df$z_icm[102], df$z_icm[114], NA, df$z_icm[124], df$z_icm[209],df$z_icm[42],NA)
resultado_mslq <- c(df$z_mslq[143], df$z_mslq[248], df$z_mslq[40], NA, df$z_mslq[102], df$z_mslq[114], NA, df$z_mslq[124], df$z_mslq[209],df$z_mslq[42],NA)


cuartiles_afm <- c(df$cuartiles_z_afm[143], df$cuartiles_z_afm[248], df$cuartiles_z_afm[40], NA,
                   df$cuartiles_z_afm[102], df$cuartiles_z_afm[114], NA, df$cuartiles_z_afm[124],
                   df$cuartiles_z_afm[209],df$cuartiles_z_afm[42],NA)

cuartiles_icm <- c(df$cuartiles_z_icm[143], df$cuartiles_z_icm[248], df$cuartiles_z_icm[40], NA,
                   df$cuartiles_z_icm[102], df$cuartiles_z_icm[114], NA, df$cuartiles_z_icm[124],
                   df$cuartiles_z_icm[209],df$cuartiles_z_icm[42],NA)

cuartiles_mslq <- c(df$cuartiles_z_mslq[143], df$cuartiles_z_mslq[248], df$cuartiles_z_mslq[40], NA,
                   df$cuartiles_z_mslq[102], df$cuartiles_z_mslq[114], NA, df$cuartiles_z_mslq[124],
                   df$cuartiles_z_mslq[209],df$cuartiles_z_mslq[42],NA)

enviar_resultados <- data.frame(persona, correo, resultado_afm, resultado_icm, resultado_mslq, cuartiles_afm, cuartiles_icm, cuartiles_mslq)

write_xlsx(enviar_resultados, "enviar_resultados.xlsx")
tabla_confiabilidad

######################################################################## Cruces de variables

cruce_carrera_grado <- crosstable(df, cols="carrera", by ="grado", total = "both" )
cruce_carrera_grado <- crosstable(df, cols="carrera", by ="grado", total = "both" )
carrera_cuartiles_afm <- crosstable(df, cols="carrera", by ="cuartiles_afm", total = "both" )

# funcion para moda.

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


descriptivos_carrera <- df %>% group_by(carrera) %>% summarise_at(vars(total_afm, total_icm, total_mslq), funs(mean, median, getmode, sd, max, min))
descriptivos_grado <- df %>% group_by(grado) %>% summarise_at(vars(total_afm, total_icm, total_mslq), funs(mean, median, getmode, sd, max, min))

write_xlsx(df, "df_r.xlsx")

###plots


afm_plot <- ggplot(df, aes(x=df$afm_f1, y=df$afm_f2)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()

afm_plot

################################################################### descripción de la muestra

#edad

media_edad = round(mean(df$edad, na.rm =TRUE), digits=2)
mediana_edad = round(median(df$edad, na.rm =TRUE), digits=2)
ds_edad = round(sd(df$edad, na.rm =TRUE), digits=2)
min_edad = min(df$edad, na.rm = TRUE)
max_edad = max(df$edad, na.rm = TRUE)

df$edad_rangos = cut_width(df$edad,width=5, boundary = 0, closed = c("left"), 
                           labels = c("18 a 19",
                                      "20 a 24",
                                      "25 a 29", 
                                      "30 a 34", 
                                      "35 a 39", 
                                      "40 a 44", 
                                      "45 a 49", 
                                      "50 a 54", 
                                      "55 a 56"))

tabla_edad <- count(df$edad_rangos)

#carrera

tabla_carrera <- count(df$carrera)

#grado

tabla_grado <- count(df$grado)

#promedio de la carrera

df$promedio <- as.numeric(df$promedio)
media_promedio = round(mean(df$promedio, na.rm =TRUE), digits=2)
mediana_promedio = round(median(df$promedio, na.rm =TRUE), digits=2)
ds_promedio = round(sd(df$promedio, na.rm =TRUE), digits=2)
min_promedio = min(df$promedio, na.rm = TRUE)
max_promedio = max(df$promedio, na.rm = TRUE)

#cursos asignados

tabla_cursos <- count(df$cursos_asignados)

media_cursos = round(mean(df$cursos_asignados, na.rm =TRUE), digits=2)
mediana_cursos = round(median(df$cursos_asignados, na.rm =TRUE), digits=2)
ds_cursos = round(sd(df$cursos_asignados, na.rm =TRUE), digits=2)
min_cursos = min(df$cursos_asignados, na.rm = TRUE)
max_cursos = max(df$cursos_asignados, na.rm = TRUE)


#asistencia a psicología

count(df$asistencia_psych)

#satisfacción con la carrera

tabla_satisfaccion <- count(df$satisfaccion_carrera)

df$sat_r <- recode(df$satisfaccion_carrera, "Nada satisfecho"=1, "Poco Satisfecho"=2, "Más o menos satisfecho"=3, 
                   "Satisfecho"=4, "Muy satisfecho"=5)

media_sat_r = round(mean(df$sat_r, na.rm =TRUE), digits=2)
mediana_sat_r = round(median(df$sat_r, na.rm =TRUE), digits=2)
ds_sat_r = round(sd(df$sat_r, na.rm =TRUE), digits=2)
min_sat_r = min(df$sat_r, na.rm = TRUE)
max_sat_r = max(df$sat_r, na.rm = TRUE)


#cruces de variables

cruce_edad <- df %>% group_by(edad_rangos) %>% summarise_at(vars(nombres_variables_medias), funs(mean, median, sd, max, min))
names(cruce_edad)[1] <- "Demograficos"
cruce_edad <- cbind(variable = "Edad en rangos", cruce_edad)
count_edad = c(count(df$edad_rangos)$freq)
cruce_edad <- cbind(frecuencia = count_edad, cruce_edad)
cruce_edad <- cruce_edad %>% relocate(frecuencia, .after = Demograficos)
cruce_edad$Demograficos <- as.character(cruce_edad$Demograficos)
cruce_edad[10,2] <- "No delcarado"

#tabla variables cuanti

descriptivos = c("Media", "Mediana", "Desviación estándar", "Mínimo", "Máximo")
descriptivos_edad = c(media_edad, mediana_edad, ds_edad, min_edad, max_edad)
descriptivos_promedio = c(media_promedio, mediana_promedio, ds_promedio, min_promedio, max_promedio)
descriptivos_cursos = c(media_cursos, mediana_cursos, ds_cursos, min_cursos, max_cursos)
descriptivos_sat_r = c(media_sat_r, mediana_sat_r, ds_sat_r, min_sat_r, max_sat_r)
tabla_descriptivos_cuanti <- data.frame(descriptivos, descriptivos_edad, descriptivos_promedio, descriptivos_cursos, descriptivos_sat_r)

#correlación con otras variables

rcorr_edad = rcorr(as.matrix(df[ ,c("total_afm", "total_icm", "total_mslq", "edad")]),type="spearman")
p.mat <- rcorr_edad$P
print(rcorr_edad$P, digits=10)
figura_correlacion_edad <- ggcorrplot(rcorr_edad$r, hc.order = TRUE, type = "lower", outline.col = "white", lab = TRUE, p.mat = p.mat)
figura_correlacion_edad

rcorr_promedio = rcorr(as.matrix(df[ ,c("total_afm", "total_icm", "total_mslq", "promedio")]),type="spearman")
p.mat <- rcorr_promedio$P
print(rcorr_promedio$P, digits=10)
figura_correlacion_promedio <- ggcorrplot(rcorr_promedio$r, hc.order = TRUE, type = "lower", outline.col = "white", lab = TRUE, p.mat = p.mat)
figura_correlacion_promedio

rcorr_cursos = rcorr(as.matrix(df[ ,c("total_afm", "total_icm", "total_mslq", "cursos_asignados")]),type="spearman")
p.mat <- rcorr_cursos$P
print(rcorr_cursos$P, digits=10)
figura_correlacion_cursos <- ggcorrplot(rcorr_cursos$r, hc.order = TRUE, type = "lower", outline.col = "white", lab = TRUE, p.mat = p.mat)
figura_correlacion_cursos

rcorr_satisfaccion = rcorr(as.matrix(df[ ,c("total_afm", "total_icm", "total_mslq", "sat_r")]),type="spearman")
p.mat <- rcorr_satisfaccion$P
print(rcorr_satisfaccion$P, digits=10)
figura_correlacion_sat <- ggcorrplot(rcorr_satisfaccion$r, hc.order = TRUE, type = "lower", outline.col = "white", lab = TRUE, p.mat = p.mat)
figura_correlacion_sat


c("Edad con AFM: rs(252) = .03, p = .671",
  "Edad con MAI: rs(252) = -.04, p = .505",
  "Edad con MSLQ: rs(252) = -.03, p = .672",
  "Promedio con AFM: rs(252) = .1, p = .135",
  "Promedio con MAI: rs(252) = .19, p = .003",
  "Promedio con MSLQ: rs(252) = .19, p = .002",
  "Cursos asignados en el semestre con AFM: rs(252) = -.04, p = .477",
  "Cursos asignados en el semestre con MAI: rs(252) = -.04, p = .476",
  "Cursos asignados en el semestre con MSLQ: rs(252) = -.01, p = .909",
  "Satisfacción con la carreca con AFM: rs(252) = .41, p = < .001",
  "Satisfacción con la carreca con MAI: rs(252) = .33, p = < .001",
  "Satisfacción con la carreca con MSLQ: rs(252) = .29, p = < .001")


#correlación entre sí

df$total_mai <- df$total_icm

rcorr_escalas = rcorr(as.matrix(df[ ,c("total_afm", "total_mai", "total_mslq")]),type="spearman")
p.mat <- rcorr_escalas$P
print(rcorr_escalas$P, digits=10)
figura_correlacion_escalas <- ggcorrplot(rcorr_escalas$r, hc.order = TRUE, type = "lower", outline.col = "white", lab = TRUE, p.mat = p.mat)
figura_correlacion_escalas

c("AFM con MAI: rs(252) = .53, p = < .001",
  "AFM con MSLQ: rs(252) = .53, p = < .001",
  "MAI con MSLQ: rs(252) = .69, p = < .001")

# exportar 

write_xlsx(enviar_resultados, "enviar_resultados.xlsx")
write_xlsx(tabla_confiabilidad, "tabla confiabilidad.xlsx")

write_xlsx(tabla_edad, "tabla edad.xlsx")
write_xlsx(tabla_carrera, "tabla carrera.xlsx")
write_xlsx(tabla_satisfaccion, "tabla satisfaccion.xlsx")
write_xlsx(tabla_descriptivos_cuanti, "tabla descriptivos cuanti.xlsx")
write_xlsx(descriptivos_escalas, "tabla descriptivos escalas.xlsx")
write_xlsx(tabla_cuartiles_escalas, "tabla cuartiles.xlsx")
write_xlsx(tabla_criterios_escalas, "tabla criterios.xlsx")



