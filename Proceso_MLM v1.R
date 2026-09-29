library(readxl)
library(dplyr)
library(knitr)
library(flextable)
library(tidyr)
library(data.table)
library(ggplot2)
library(writexl)
library(gtools)
library(psych)
library(pscl)

# CORRIGE ESTA RUTA según tu archivo

setwd("C:\\Users\\USUARIO\\OneDrive - TELECYL PERU SAC\\Escritorio\\Revision Renato")
getwd()
list.files()

base = read.csv("Base de datos profesores CSV TODO.csv")

base$GD_01 = ifelse(base$G1 == 1 | base$G2 == 1 | base$G3 == 1,1,0)
base$GD_02 = ifelse(base$G1 == 2 | base$G2 == 2 | base$G3 == 2,1,0)
base$GD_03 = ifelse(base$G1 == 3 | base$G2 == 3 | base$G3 == 3,1,0)
base$GD_04 = ifelse(base$G1 == 4 | base$G2 == 4 | base$G3 == 4,1,0)
base$GD_05 = ifelse(base$G1 == 5 | base$G2 == 5 | base$G3 == 5,1,0)
base$GD_06 = ifelse(base$G1 == 6 | base$G2 == 6 | base$G3 == 6,1,0)
base$GD_07 = ifelse(base$G1 == 7 | base$G2 == 7 | base$G3 == 7,1,0)
base$GD_08 = ifelse(base$G1 == 8 | base$G2 == 8 | base$G3 == 8,1,0)
base$GD_09 = ifelse(base$G1 == 9 | base$G2 == 9 | base$G3 == 9,1,0)
base$GD_10 = ifelse(base$G1 == 10 | base$G2 == 10 | base$G3 == 10,1,0)
base$GD_11 = ifelse(base$G1 == 11 | base$G2 == 11 | base$G3 == 11,1,0)

base$GD_01[is.na(base$GD_01)] = 0
base$GD_02[is.na(base$GD_02)] = 0
base$GD_03[is.na(base$GD_03)] = 0
base$GD_04[is.na(base$GD_04)] = 0
base$GD_05[is.na(base$GD_05)] = 0
base$GD_06[is.na(base$GD_06)] = 0
base$GD_07[is.na(base$GD_07)] = 0
base$GD_08[is.na(base$GD_08)] = 0
base$GD_09[is.na(base$GD_09)] = 0
base$GD_10[is.na(base$GD_10)] = 0
base$GD_11[is.na(base$GD_11)] = 0

base$XD_01 = ifelse(base$X1 == 1 | base$X2 == 1 | base$X3 == 1,1,0)
base$XD_02 = ifelse(base$X1 == 2 | base$X2 == 2 | base$X3 == 2,1,0)
base$XD_03 = ifelse(base$X1 == 3 | base$X2 == 3 | base$X3 == 3,1,0)
base$XD_04 = ifelse(base$X1 == 4 | base$X2 == 4 | base$X3 == 4,1,0)
base$XD_05 = ifelse(base$X1 == 5 | base$X2 == 5 | base$X3 == 5,1,0)
base$XD_06 = ifelse(base$X1 == 6 | base$X2 == 6 | base$X3 == 6,1,0)
base$XD_07 = ifelse(base$X1 == 7 | base$X2 == 7 | base$X3 == 7,1,0)
base$XD_08 = ifelse(base$X1 == 8 | base$X2 == 8 | base$X3 == 8,1,0)
base$XD_09 = ifelse(base$X1 == 9 | base$X2 == 9 | base$X3 == 9,1,0)
base$XD_10 = ifelse(base$X1 == 10 | base$X2 == 10 | base$X3 == 10,1,0)
base$XD_11 = ifelse(base$X1 == 11 | base$X2 == 11 | base$X3 == 11,1,0)

base$XD_01[is.na(base$XD_01)] = 0
base$XD_02[is.na(base$XD_02)] = 0
base$XD_03[is.na(base$XD_03)] = 0
base$XD_04[is.na(base$XD_04)] = 0
base$XD_05[is.na(base$XD_05)] = 0
base$XD_06[is.na(base$XD_06)] = 0
base$XD_07[is.na(base$XD_07)] = 0
base$XD_08[is.na(base$XD_08)] = 0
base$XD_09[is.na(base$XD_09)] = 0
base$XD_10[is.na(base$XD_10)] = 0
base$XD_11[is.na(base$XD_11)] = 0

base = base %>%
  filter(!ID %in% c(3, 14, 29))

base = base %>%
  filter(!ID %in% c(26,35))


cols = c('ID', 'Tiempo', 'DNE', 'ObsAA', 'ObsEst', 'ObsCon', 'ObsCaos')
mini_base <- base %>%
  select(all_of(cols))


missing_summary <- data.frame(
  Variable = names(mini_base),
  Missing_Count = colSums(is.na(mini_base)),
  `% Missing` = colSums(is.na(mini_base)) / nrow(mini_base) * 100
)
print(missing_summary)

#        Variable Missing_Count X..Missing
# 1            ID             0   0.000000
# 2        Tiempo             0   0.000000
# 3           DNE            52   1.278269
# 4         ObsAA           343   8.431662
# 5        ObsEst           492  12.094395
# 6        ObsCon           312   7.669617
# 7       ObsCaos           235   5.776794
# 8      DNE_norm            52   1.278269

mini_base <- mini_base %>%
  arrange(ID, Tiempo) %>%
  group_by(ID) %>%
  fill(DNE, ObsAA, ObsEst, ObsCon, ObsCaos, .direction = "down") %>%
  ungroup() %>%
  drop_na()
cat("Final usable rows:", nrow(mini_base), "\n")

mini_base$DNE_norm <- scale(mini_base$DNE)
mini_base$ObsAA_norm <- scale(mini_base$ObsAA)
mini_base$ObsEst_norm <- scale(mini_base$ObsEst)
mini_base$ObsCon_norm <- scale(mini_base$ObsCon)
mini_base$ObsCaos_norm <- scale(mini_base$ObsCaos)


mini_base <- mini_base %>%
  mutate(Time_Block = ceiling(Tiempo / 5))

tablita <- mini_base %>%
  group_by(ID, Time_Block) %>%
  summarise(
    Tiempo  = max(Tiempo),   # conserva el minuto final: 5, 10, 15...
    DNE     = median(DNE),   # mediana de los 5 valores de DNE
    ObsAA   = first(ObsAA),  # toma el primer valor del bloque
    ObsEst  = first(ObsEst),
    ObsCon  = first(ObsCon),
    ObsCaos = first(ObsCaos),
    .groups = "drop"
  )

#       ID Time_Block Tiempo   DNE ObsAA ObsEst ObsCon ObsCaos
#    <int>      <dbl>  <int> <dbl> <dbl>  <dbl>  <dbl>   <dbl>
#  1     1          1      5   100  5.33   5.33    1         1
#  2     1          2     10    68  3.67   4.33    1         1
#  3     1          3     15    81  3      5.33    1         1
#  4     1          4     20    63  3.67   5.33    1         1
#  5     1          5     25    71  4.33   5.67    1         1
#  6     1          6     30    93  5.33   6       1         1
#  7     1          7     35    59  2      5.67    1         1
#  8     1          8     40    71  3.67   5.67    3.8       1
#  9     1          9     45    80  4.67   5.67    1         1
# 10     1         10     50    84  4      6       1         1


# Aplicar el lag sobre el dataframe agregado
mini_base <- mini_base %>%
  arrange(ID, Tiempo) %>%
  group_by(ID) %>%
  mutate(
    ObsAA_norm_lag1   = lag(ObsAA_norm),
    ObsEst_norm_lag1  = lag(ObsEst_norm),
    ObsCon_norm_lag1  = lag(ObsCon_norm),
    ObsCaos_norm_lag1 = lag(ObsCaos_norm)
  ) %>%
  ungroup()

# Eliminar el primer bloque de 5 minutos de cada ID
mini_base <- mini_base %>%
  filter(!is.na(ObsAA_norm_lag1))


# mini_promedio <- mini_base %>%
#   group_by(Tiempo) %>%
#   summarise(
#     DNE_norm = mean(DNE_norm, na.rm = TRUE),
#     ObsAA_norm_lag1 = mean(ObsAA_norm_lag1, na.rm = TRUE)
#   )

# ggplot(mini_promedio, aes(x = Tiempo)) +
#   geom_line(aes(y = DNE_norm, color = "DNE")) +
#   geom_line(aes(y = ObsAA_norm_lag1, color = "ObsAA_norm_lag1")) +
#   labs(
#     x = "Time",
#     y = "Normalized Value (Z-Score)",
#     color = NULL
#   )

##  modelos observaciones

## Modelos observacionales SIN lagged

### Significativo PERO R cuadrado hasta el piso
md_Obs_Est <- lmer(DNE_norm ~ ObsEst_norm + (1 | ID),data = mini_base,REML = TRUE)

### Todo mal
md_Obs_AA <- lmer(DNE_norm ~ ObsAA_norm + (1 | ID),data = mini_base,REML = TRUE)
md_Obs_con <- lmer(DNE_norm ~ ObsCon_norm + (1 | ID),data = mini_base,REML = TRUE)
md_Obs_Caos <- lmer(DNE_norm ~ ObsCaos_norm + (1 | ID),data = mini_base,REML = TRUE)

## Modelos observacionales CON lagged

### Significativo PERO R cuadrado hasta el piso
md_Obs_Est_L <- lmer(DNE_norm ~ ObsEst_norm_lag1 + (1 | ID),data = mini_base,REML = TRUE)

### Todo mal
md_Obs_AA_L <- lmer(DNE_norm ~ ObsAA_norm_lag1 + (1 | ID),data = mini_base,REML = TRUE)
md_Obs_con_L <- lmer(DNE_norm ~ ObsCon_norm_lag1 + (1 | ID),data = mini_base,REML = TRUE)
md_Obs_Caos_L <- lmer(DNE_norm ~ ObsCaos_norm_lag1 + (1 | ID),data = mini_base,REML = TRUE)


summary(md_Obs_Caos_L)
r2(md_Obs_Caos_L)

# mini_base = mini_base %>% select('ID', 'Tiempo', 'DNE', 'ObsAA_norm', 'ObsEst_norm', 'ObsCon_norm', 'ObsCaos_norm')


## Parte 02: Modelos G y X

modelos_01 = c('ID', 'Tiempo', 'DNE', 'G1', 'X1', "ApoyoAut", 
                "Estruc", "Control",  "Chaos", "SIS_AA",
                "SIS_Est", "SIS_Control", "SIS_Chaos", "NeedSat",
                "NeedFru", "NeedSup", "EstDesm",
                "AutSat", "CompSat", "SatRel", 
                "FrusAut", "FrusComp", "FrusRel",
                "SatLab", "EmoExha",
                "Cinismo", "Engagement", 
                "Joy",  "Anger", "Anxiety", "Hope", 
                "Hopelessness", "PerGrow", "RelGrow", 
                "MasGrow", "EducaMat",  "DesBeha", "NoGoal","GD_01","GD_02","GD_03","GD_04","GD_05",
                "GD_06","GD_07","GD_08","GD_09","GD_10","GD_11",
                "XD_01","XD_02","XD_03","XD_04","XD_05",
                "XD_06","XD_07","XD_08","XD_09","XD_10","XD_11")

modelito_01 <- base %>%
  select(all_of(modelos_01))
modelito_01$DNE_norm <- scale(modelito_01$DNE)


install.packages("performance")
library(lme4)
library(lmerTest)
library(moments)
library(performance)

table(modelito_01$GD_06)

modelo_g1 <- lmer(
  DNE_norm ~ GD_01 + GD_02   + 
            GD_05  + GD_07    + (1 | ID),
  data = modelito_01)
summary(modelo_g1)
r2(modelo_g1)


#REML criterion at convergence: 9322.8

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-3.1626 -0.6908 -0.0353  0.6184  3.8354 

#Random effects:
# Groups   Name        Variance Std.Dev.
# ID       (Intercept) 0.4386   0.6623  
# Residual             0.5709   0.7556  
#Number of obs: 4016, groups:  ID, 37

#Fixed effects:
#              Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)   -0.11450    0.11241   39.76563  -1.019  0.31455    
#GD_01         -0.10842    0.06375 3985.25280  -1.701  0.08908 .  
#GD_02          0.27491    0.06380 4007.77124   4.309 1.68e-05 ***
#GD_05          0.09872    0.03311 4003.09671   2.982  0.00288 ** 
#GD_07          0.14684    0.04477 4010.99687   3.280  0.00105 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# R2 for Mixed Models GD01 - GD08
# Conditional R2: 0.438
#    Marginal R2: 0.007


## Supuestos 01 Normalidad de residuos #####
plot(modelo_g1)
qqnorm(resid(modelo_g1))
qqline(resid(modelo_g1))
performance::check_normality(modelo_g1)
#Warning: Non-normality of residuals detected (p < .001).

e <- resid(modelo_g1)
mean(e)
sd(e)
moments::skewness(e)
moments::kurtosis(e)

#resultados - salvo la curtosis, todo está ok
#[media]        6.514824e-16
#[D.E]          0.5838525
#[Asimetría]   -0.0531468
#[Curtosis]     3.741055

check_collinearity(modelo_g1) #Todo ok

plot(
  fitted(modelo_g1),
  resid(modelo_g1),
  xlab = "Valores predichos",
  ylab = "Residuos"
)



modelo_g2 <- lmer(
  DNE_norm ~ XD_01 + XD_02 + XD_03  
     + XD_07 + XD_08 + XD_09   + (1 | ID),
  data = modelito_01, REML = FALSE
)
summary(modelo_g2)
r2(modelo_g2)

#REML criterion at convergence: 9286.1
#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-3.4329 -0.6900 -0.0391  0.6061  3.8409 
#Random effects:
# Groups   Name        Variance Std.Dev.
# ID       (Intercept) 0.4488   0.6699  
# Residual             0.5655   0.7520  
#Number of obs: 4016, groups:  ID, 37
#Fixed effects:
#              Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)   -0.07319    0.11130   36.61213  -0.658 0.514926    
#XD_01          0.09115    0.04276 3995.04418   2.132 0.033092 *  
#XD_02         -1.20809    0.23606 3979.39445  -5.118 3.24e-07 ***
#XD_03          0.32507    0.06397 3994.59314   5.082 3.91e-07 ***
#XD_07         -0.48557    0.25054 3982.99285  -1.938 0.052687 .  
#XD_08          0.20401    0.05330 3987.53244   3.828 0.000131 ***
#XD_09          0.10283    0.03479 3996.61831   2.956 0.003137 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#  Conditional R2: 0.449
#     Marginal R2: 0.012

## Supuestos 01 Normalidad de residuos #####
plot(modelo_g2)
qqnorm(resid(modelo_g2))
qqline(resid(modelo_g2))
performance::check_normality(modelo_g2)
#Warning: Non-normality of residuals detected (p < .001).

e <- resid(modelo_g2)
mean(e)
sd(e)
moments::skewness(e)
moments::kurtosis(e)

#[media]        1.698769e-16
#[D.E]          0.7481232
#[Asimetría]    0.2297898
#[Curtosis]     3.741055



check_collinearity(modelo_g2) #Todo ok

plot(
  fitted(modelo_g2),
  resid(modelo_g2),
  xlab = "Valores predichos",
  ylab = "Residuos"
)

## Parte 03: Interacciones

modelos_02 = c(
                "ID", "DNE",
                "ApoyoAut", "Estruc", "Control",  "Chaos", "SIS_AA",
                "SIS_Est", "SIS_Control", "SIS_Chaos", "NeedSat",
                "NeedFru", "NeedSup", "EstDesm",
                "AutSat", "CompSat", "SatRel", 
                "FrusAut", "FrusComp", "FrusRel",
                "SatLab", "EmoExha",
                "Cinismo", "Engagement", 
                "Joy",  "Anger", "Anxiety", "Hope", 
                "Hopelessness", "PerGrow", "RelGrow", 
                "MasGrow", "EducaMat",  "DesBeha", "NoGoal","CoreBO", "EmoPos", "EmoNeg",
                "GD_01","GD_02","GD_03","GD_04","GD_05",
                "GD_06","GD_07","GD_08","GD_09","GD_10","GD_11",
                "XD_01","XD_02","XD_03","XD_04","XD_05",
                "XD_06","XD_07","XD_08","XD_09","XD_10","XD_11")

modelito_02 <- base %>% select(all_of(modelos_02))
modelito_02$DNE_norm <- scale(modelito_02$DNE)



modelo_base <- lmer( DNE_norm ~ XD_01 + XD_02 + XD_03 + XD_07 + XD_08 + XD_09  + (1 | ID),
  data = modelito_02)
summary(modelo_base)
r2(modelo_base)

#Random effects:
# Groups   Name        Variance Std.Dev.
# ID       (Intercept) 0.4488   0.6699  
# Residual             0.5655   0.7520  
#Number of obs: 4016, groups:  ID, 37

#Fixed effects:
#              Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)   -0.07319    0.11130   36.61213  -0.658 0.514926    
#XD_01          0.09115    0.04276 3995.04418   2.132 0.033092 *  
#XD_02         -1.20809    0.23606 3979.39445  -5.118 3.24e-07 ***
#XD_03          0.32507    0.06397 3994.59314   5.082 3.91e-07 ***
#XD_07         -0.48557    0.25054 3982.99285  -1.938 0.052687 .  
#XD_08          0.20401    0.05330 3987.53244   3.828 0.000131 ***
#XD_09          0.10283    0.03479 3996.61831   2.956 0.003137 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#      (Intr) XD_01  XD_02  XD_03  XD_07  XD_08 
#XD_01 -0.050                                   
#XD_02 -0.006  0.017                            
#XD_03 -0.032  0.036 -0.144                     
#XD_07 -0.007  0.010  0.001  0.001              
#XD_08 -0.037 -0.032  0.047  0.019  0.012       
#XD_09 -0.068  0.113  0.018  0.061  0.009  0.063
# R2 for Mixed Models

#  Conditional R2: 0.449
#     Marginal R2: 0.012

modelo_block_01 <- lmer( DNE_norm ~ XD_01 + XD_02 + XD_03 + XD_07 + XD_08 + XD_09  + EstDesm + (1 | ID),
  data = modelito_02, REML = FALSE)
summary(modelo_block_01)
r2(modelo_block_01)

#Fixed effects:
#              Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)   -1.16273    0.45434   36.05147  -2.559 0.014833 *  
#XD_01          0.08440    0.04482 3894.15969   1.883 0.059787 .  
#XD_02         -1.20443    0.23765 3881.88724  -5.068 4.21e-07 ***
#XD_03          0.32464    0.06438 3899.52918   5.043 4.80e-07 ***
#XD_07         -0.49381    0.25223 3884.87428  -1.958 0.050331 .  
#XD_08          0.20489    0.05365 3891.25392   3.819 0.000136 ***
#XD_09          0.10209    0.03503 3901.58586   2.914 0.003585 ** 
#EstDesm        0.53545    0.22460   36.01813   2.384 0.022517 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# R2 for Mixed Models
#  Conditional R2: 0.423
#     Marginal R2: 0.070

modelo_block_02 <- lmer( DNE_norm ~ XD_01 + XD_02 + XD_03 + XD_07 + XD_08 + XD_09  + AutSat + CompSat + SatRel + FrusAut + FrusComp + FrusRel + (1 | ID),
  data = modelito_02, REML = FALSE)
summary(modelo_block_02)
r2(modelo_block_02)

modelo_block_02 <- lmer( DNE_norm ~ XD_01 + XD_02 + XD_03 + XD_07 + XD_08 + XD_09 + AutSat + CompSat + FrusAut + FrusComp  + (1 | ID),
  data = modelito_02, REML = FALSE)
summary(modelo_block_02)
r2(modelo_block_02)



modelo_block_03 <- lmer( DNE_norm ~ XD_01 + XD_02 + XD_03 + XD_07 + XD_08 + XD_09  + MasGrow +  EducaMat + DesBeha  + NoGoal  + CoreBO  + EmoPos  + EmoNeg +
(1 | ID),
  data = modelito_02, REML = FALSE)
summary(modelo_block_03)
r2(modelo_block_03)

check_collinearity(modelo_block_03)

#     Term  VIF       VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
#    XD_01 1.02 [1.00,     1.11]     1.01      0.98     [0.90, 1.00]
#    XD_02 1.03 [1.01,     1.09]     1.01      0.98     [0.92, 0.99]
#    XD_03 1.03 [1.01,     1.09]     1.01      0.97     [0.92, 0.99]
#    XD_07 1.00 [1.00,      Inf]     1.00      1.00     [0.00, 1.00]
#    XD_08 1.01 [1.00,     1.30]     1.00      0.99     [0.77, 1.00]
#    XD_09 1.02 [1.01,     1.09]     1.01      0.98     [0.91, 0.99]
#  MasGrow 1.33 [1.28,     1.38]     1.15      0.75     [0.72, 0.78]
# EducaMat 2.62 [2.50,     2.76]     1.62      0.38     [0.36, 0.40]
#  DesBeha 2.32 [2.21,     2.44]     1.52      0.43     [0.41, 0.45]
#   NoGoal 1.06 [1.04,     1.11]     1.03      0.94     [0.90, 0.97]
#   CoreBO 1.55 [1.49,     1.62]     1.24      0.65     [0.62, 0.67]
#   EmoPos 1.63 [1.56,     1.70]     1.27      0.62     [0.59, 0.64]
#   EmoNeg 1.72 [1.65,     1.80]     1.31      0.58     [0.55, 0.61]

#Modelo 03 corregido
modelo_block_03 <- lmer( DNE_norm ~ XD_01 + XD_02 + XD_03 + XD_07 + XD_08 + XD_09   + DesBeha   +
(1 | ID),
  data = modelito_02, REML = FALSE)
summary(modelo_block_03)
r2(modelo_block_03)


modelo_block_04 <- lmer( DNE_norm ~ XD_01 + XD_02 + XD_03 + XD_07 + XD_08 + XD_09 + Hope + Anger + Anxiety + Hopelessness + (1 | ID),
    data = modelito_02, REML = FALSE)
  summary(modelo_block_04)
  r2(modelo_block_04)

#El modelo 04 no sirve xD




modelo_block_01.2 <- lmer( DNE_norm ~ XD_01 + XD_02 + XD_03 + XD_07 + XD_08 + XD_09  + SIS_AA + SIS_Est + SIS_Control + SIS_Chaos+ (1 | ID),
  data = modelito_02, REML = FALSE)
summary(modelo_block_01.2)
r2(modelo_block_01.2)

cols <- c("ApoyoAut", "Estruc", "Control",  "Chaos", "SIS_AA",
                "SIS_Est", "SIS_Control", "SIS_Chaos", "NeedSat",
                "NeedFru", "NeedSup", "EstDesm",
                "AutSat", "CompSat", "SatRel", 
                "FrusAut", "FrusComp", "FrusRel",
                "SatLab", "EmoExha",
                "Cinismo", "Engagement", 
                "Joy",  "Anger", "Anxiety", "Hope", 
                "Hopelessness", "PerGrow", "RelGrow", 
                "MasGrow", "EducaMat",  "DesBeha", "NoGoal",
                "CoreBO", "EmoPos", "EmoNeg")

modelito_02[cols] <- lapply(
  modelito_02[cols],
  function(x) as.numeric(gsub(",", ".", x)))


anova(modelo_block_01, modelo_block_02,modelo_block_03,modelo_block_04, test = "Chisq")
test_block_significance <- function(model_base, model_block, block_name) {
  
  resultado <- anova(model_base, model_block, test = "Chisq")
  
  lr_stat <- resultado$Chisq[2]
  df_diff <- resultado$Df[2]
  p_val <- resultado$`Pr(>Chisq)`[2]
  
  cat("---", block_name, "---\n")
  cat(
    "Estadístico LRT:", round(lr_stat, 3),
    "| Grados de Libertad:", df_diff,
    "| p-valor:", round(p_val, 4), "\n"
  )
  
  if (p_val < 0.05) {
    cat("-> El bloque MEJORA significativamente el ajuste del modelo.\n\n")
  } else {
    cat("-> El bloque NO produce una mejora significativa del ajuste.\n\n")
  }
}

test_block_significance(
  modelo_base,
  modelo_block_01,
  "Bloque 1: Estilos Docentes"
)

test_block_significance(
  modelo_base,
  modelo_block_02,
  "Bloque 2: Necesidades Psicológicas"
)

test_block_significance(
  modelo_base,
  modelo_block_03,
  "Bloque 3: Crecimiento y Metas"
)

test_block_significance(
  modelo_base,
  modelo_block_04,
  "Bloque 4: Emociones"
)





## Moderación


correlacion = c('DNE_norm','AutSat','CompSat','SatRel','FrusAut','FrusComp','FrusRel',
                'MasGrow','EducaMat',"NoGoal",'EstDesm','NeedSup')


library(psych)

resultado = corr.test(
  modelito_02[, correlacion],
  use = "pairwise"
)

corrs_dne = resultado$r[, "DNE_norm"]
p_dne     = resultado$p[, "DNE_norm"]

resultado_dne = data.frame(
  r = round(corrs_dne,3),
  p = round(p_dne,3)
)

resultado_dne

#               r     p
# DNE_norm  1.000 0.000
# AutSat    0.060 0.000
# CompSat  -0.050 0.002
# SatRel    0.003 0.854
# FrusAut   0.104 0.000
# FrusComp -0.093 0.000
# FrusRel   0.140 0.000
# MasGrow  -0.093 0.000
# EducaMat  0.192 0.000 - aqui
# NoGoal    0.031 0.052
# EstDesm   0.210 0.000 - aqui
# NeedSup  -0.054 0.001


modelo_base <- lmer(
  DNE_norm ~
    XD_01 + XD_02 + XD_03 +
    XD_07 + XD_08 + XD_09 + EstDesm +
    (1 | ID),
  data = modelito_02,
  REML = FALSE
)
summary(modelo_base)
r2(modelo_base)

# Fixed effects:
#               Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   -1.16273    0.45434   36.05147  -2.559 0.014833 *  
# XD_01          0.08440    0.04482 3894.15969   1.883 0.059787 .  
# XD_02         -1.20443    0.23765 3881.88724  -5.068 4.21e-07 ***
# XD_03          0.32464    0.06438 3899.52918   5.043 4.80e-07 ***
# XD_07         -0.49381    0.25223 3884.87428  -1.958 0.050331 .  
# XD_08          0.20489    0.05365 3891.25392   3.819 0.000136 ***
# XD_09          0.10209    0.03503 3901.58586   2.914 0.003585 ** 
# EstDesm        0.53545    0.22460   36.01813   2.384 0.022517 *  

# Conditional R2: 0.423
#    Marginal R2: 0.070
  
modelo_inter <- lmer(
  DNE_norm ~
    XD_01 + XD_02 + XD_03 +
    XD_07 + XD_08 + XD_09 + EstDesm + XD_01:EstDesm + XD_02:EstDesm + XD_03:EstDesm +
    XD_07:EstDesm + XD_08:EstDesm + XD_09:EstDesm +
    (1 | ID),
  data = modelito_02,
  REML = FALSE
)
summary(modelo_inter)
r2(modelo_inter)

inte_EstDesm = modelo_inter

# Fixed effects:
#                 Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)     -1.28618    0.45842   36.71392  -2.806 0.007984 ** 
# XD_01            0.71748    0.24621 3891.04109   2.914 0.003587 ** 
# XD_02           27.03106   41.30740 3881.48694   0.654 0.512901    
# XD_03           -0.71660    0.29237 3899.32453  -2.451 0.014291 *  
# XD_07           -0.52528    0.25139 3884.92405  -2.089 0.036728 *  
# XD_08            0.53751    0.25857 3888.67006   2.079 0.037704 *  
# XD_09            0.52684    0.14860 3900.29142   3.545 0.000397 ***
# EstDesm          0.59588    0.22666   36.70367   2.629 0.012432 *  
# XD_01:EstDesm   -0.31537    0.12312 3889.42635  -2.561 0.010462 *  
# XD_02:EstDesm  -16.17871   23.74760 3881.46569  -0.681 0.495736    
# XD_03:EstDesm    0.53256    0.14776 3901.55110   3.604 0.000317 ***
# XD_08:EstDesm   -0.15269    0.12531 3888.39653  -1.218 0.223122    
# XD_09:EstDesm   -0.21083    0.07204 3898.82451  -2.926 0.003449 ** 
  # Conditional R2: 0.431
  #    Marginal R2: 0.078

anova(modelo_base, modelo_inter)

#              npar    AIC    BIC  logLik -2*log(L)  Chisq Df Pr(>Chisq)    
# modelo_base    10 9090.6 9153.3 -4535.3    9070.6                         
# modelo_inter   15 9069.6 9163.7 -4519.8    9039.6 31.011  5  9.322e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

modelo_base <- lmer(
  DNE_norm ~
    XD_01 + XD_02 + XD_03 +
    XD_07 + XD_08 + XD_09 + EducaMat +
    (1 | ID),
  data = modelito_02,
  REML = FALSE
)
summary(modelo_base)
r2(modelo_base)


# Fixed effects:
#               Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)   -0.45882    0.26995   35.96247  -1.700 0.097831 .  
# XD_01          0.08455    0.04483 3892.50676   1.886 0.059351 .  
# XD_02         -1.20632    0.23766 3881.22896  -5.076 4.04e-07 ***
# XD_03          0.32472    0.06439 3897.90832   5.043 4.79e-07 ***
# XD_07         -0.48576    0.25223 3885.29060  -1.926 0.054191 .  
# XD_08          0.20434    0.05366 3889.36965   3.808 0.000142 ***
# XD_09          0.10232    0.03504 3899.86123   2.920 0.003516 ** 
# EducaMat       0.10580    0.07467   35.86100   1.417 0.165146    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

  # Conditional R2: 0.425
  #    Marginal R2: 0.039

#No sale significativo, pero igual lo voy a calcular solo por curiosidad

modelo_inter <- lmer(
  DNE_norm ~
    XD_01 + XD_02 + XD_03 +
    XD_07 + XD_08 + XD_09 + EducaMat + XD_01:EducaMat + XD_02:EducaMat + XD_03:EducaMat +
    XD_07:EducaMat + XD_08:EducaMat + XD_09:EducaMat +
    (1 | ID),
  data = modelito_02,
  REML = FALSE
)
summary(modelo_inter)
r2(modelo_inter)
inte_EducaMat = modelo_inter

# Fixed effects:
#                  Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      -0.43015    0.27070   36.40703  -1.589 0.120700    
# XD_01             0.01902    0.11274 3882.62054   0.169 0.866069    
# XD_02            -0.59531    0.47092 3883.54034  -1.264 0.206257    
# XD_03            -0.24786    0.14804 3891.43226  -1.674 0.094168 .  
# XD_07            -0.48484    0.25083 3885.18123  -1.933 0.053312 .  
# XD_08            -0.27884    0.14379 3887.42666  -1.939 0.052544 .  
# XD_09             0.31786    0.08289 3891.47322   3.835 0.000128 ***
# EducaMat          0.09711    0.07493   36.40416   1.296 0.203133    
# XD_01:EducaMat    0.02295    0.03401 3884.22789   0.675 0.499936    
# XD_02:EducaMat   -0.14112    0.13748 3881.57356  -1.027 0.304720    
# XD_03:EducaMat    0.17131    0.03998 3896.01052   4.285 1.87e-05 ***
# XD_08:EducaMat    0.13321    0.03655 3888.68672   3.645 0.000271 ***
# XD_09:EducaMat   -0.06399    0.02221 3893.40544  -2.881 0.003991 ** 

  # Conditional R2: 0.432
  #    Marginal R2: 0.047

anova(modelo_base, modelo_inter)
#              npar    AIC    BIC  logLik -2*log(L)  Chisq Df Pr(>Chisq)    
# modelo_base    10 9093.9 9156.6 -4537.0    9073.9                         
# modelo_inter   15 9060.6 9154.7 -4515.3    9030.6 43.291  5  3.226e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova(inte_EducaMat, inte_EstDesm)

#               npar    AIC    BIC  logLik -2*log(L) Chisq Df Pr(>Chisq)
# inte_EducaMat   15 9060.6 9154.7 -4515.3    9030.6                    
# inte_EstDesm    15 9069.6 9163.7 -4519.8    9039.6     0  0     

write_xlsx(base,"no se que pasa.xlsx")
