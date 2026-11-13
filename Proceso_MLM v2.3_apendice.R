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
library(lme4)   
library(performance)
library(lmerTest)


# CORRIGE ESTA RUTA según tu archivo


setwd("C:\\SOL BACKUP\\Escritorio\\Renato")
getwd()
list.files()

base = read_excel("base_corregida.xlsx",sheet = "base")

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

## Modelos observacionales CON lagged
modelos_lagg = c('ID', 'Tiempo', "DNE",
    "ObsAA","ObsEst","ObsCon","ObsCaos",
    "ApoyoAut","Estruc","Control","Chaos",
    "SIS_AA",
    "SIS_Est",
    "SIS_Control",
    "SIS_Chaos",
    "NeedSat",
    "NeedFru",
    "NeedSup",
    "EstDesm",
    "AutSat",
    "CompSat",
    "SatRel",
    "FrusAut",
    "FrusComp",
    "FrusRel",
    "SatLab",
    "EmoExha",
    "Cinismo",
    "Engagement",
    "Joy",
    "Anger",
    "Anxiety",
    "Hope",
    "Hopelessness",
    "PerGrow",
    "RelGrow",
    "MasGrow",
    "EducaMat",
    "DesBeha",
    "NoGoal",
    "EmoPos",
    "EmoNeg","GD_01","GD_02","GD_03","GD_04","GD_05",
                "GD_06","GD_07","GD_08","GD_09","GD_10","GD_11",
                "XD_01","XD_02","XD_03","XD_04","XD_05",
                "XD_06","XD_07","XD_08","XD_09","XD_10","XD_11")

base_lagg <- base %>%
  select(all_of(modelos_lagg))


missing_summary <- data.frame(
  Variable = names(base_lagg),
  Missing_Count = colSums(is.na(base_lagg)),
  `% Missing` = colSums(is.na(base_lagg)) / nrow(base_lagg) * 100
)
print(missing_summary)

base_lagg <- base_lagg %>%
  arrange(ID, Tiempo) %>%
  group_by(ID) %>%
  fill(DNE, ObsAA, ObsEst, ObsCon, ObsCaos, .direction = "down") %>%
  ungroup()

base_lagg <- base_lagg %>%
  drop_na(DNE)

# Model 3: Momentos y eventos
# 3.1 Momentos G
validar_GD = base_lagg %>%
  summarise(
    GD_01 = sum(GD_01),
    GD_02 = sum(GD_02),
    GD_03 = sum(GD_03),
    GD_04 = sum(GD_04),
    GD_05 = sum(GD_05),
    GD_06 = sum(GD_06),
    GD_07 = sum(GD_07),
    GD_08 = sum(GD_08),
    GD_09 = sum(GD_09),
    GD_10 = sum(GD_10),
    GD_11 = sum(GD_11),
  )

#   GD_01 GD_02 GD_03 GD_04 GD_05 GD_06 GD_07 GD_08 GD_09 GD_10 GD_11
#   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1     8    40    14    19   463    90   145    85    45     0     0
base_lagg$VAL_GD = base_lagg$GD_01 + base_lagg$GD_02 + base_lagg$GD_03 + base_lagg$GD_04 + 
                  base_lagg$GD_05 + base_lagg$GD_06  + base_lagg$GD_07 + base_lagg$GD_08 + 
                  base_lagg$GD_09

base_lagg_GD = base_lagg %>% filter(VAL_GD != 0)

base_lagg_GD$DNE_norm = scale(base_lagg_GD$DNE)
## Sin borrar los GD = 0
modelo_GD <- lmer(DNE_norm ~ GD_01 + GD_02 + GD_03 + GD_04 + GD_05 + GD_06  
                            + GD_07 + GD_08 + GD_09 + (1 | ID),
  data = base_lagg_GD, REML = FALSE)
summary(modelo_GD)
r2(modelo_GD)  

modelo_GD_2 <- lmer(DNE_norm ~ GD_01       
                            + GD_07  + GD_09 + (1 | ID),
  data = base_lagg_GD, REML = FALSE)
summary(modelo_GD_2)
r2(modelo_GD_2)  
#Filtrado

#    Data: base_lagg_GD

#       AIC       BIC    logLik -2*log(L)  df.resid 
#    8731.4    8768.7   -4359.7    8719.4      3756 

# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.9914 -0.7039 -0.0488  0.6427  3.8176 

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  ID       (Intercept) 0.4209   0.6488  
#  Residual             0.5700   0.7550  
# Number of obs: 3762, groups:  ID, 37

# Fixed effects:
#               Estimate Std. Error         df t value Pr(>|t|)   
# (Intercept)  7.118e-03  1.080e-01  3.768e+01   0.066  0.94780   
# GD_01       -1.654e-01  5.237e-02  3.753e+03  -3.159  0.00159 **
# GD_07       -7.966e-02  4.099e-02  3.762e+03  -1.943  0.05208 . 
# GD_09       -1.284e-01  6.498e-02  3.738e+03  -1.976  0.04821 * 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:


## Supuestos 01 Normalidad de residuos #####
plot(modelo_GD_2)
qqnorm(resid(modelo_GD_2))
qqline(resid(modelo_GD_2), col = "red")

performance::check_normality(modelo_GD_2)
#Warning: Non-normality of residuals detected (p < .001).

e <- resid(modelo_GD_2)
mean(e) #1
sd(e) #2
moments::skewness(e) #3
moments::kurtosis(e) #4

# No hay normalidad
# [1] -3.031216e-17
# [2] 0.7962459
# [3] 1.169002
# [4] 6.78155

check_collinearity(modelo_GD_2)
#   Term  VIF   VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
#  GD_07 1.02 [1.00, 1.64]     1.01      0.98     [0.61, 1.00]
#  GD_09 1.02 [1.00, 1.64]     1.01      0.98     [0.61, 1.00]


efectos_aleatorios <- ranef(modelo_GD_2)$ID[, 1]
head(efectos_aleatorios)


# 3.2 Momentos X

validar_GD = base_lagg %>%
  summarise(
    XD_01 = sum(XD_01),
    XD_02 = sum(XD_02),
    XD_03 = sum(XD_03),
    XD_04 = sum(XD_04),
    XD_05 = sum(XD_05),
    XD_06 = sum(XD_06),
    XD_07 = sum(XD_07),
    XD_08 = sum(XD_08),
    XD_09 = sum(XD_09),
    XD_10 = sum(XD_10),
    XD_11 = sum(XD_11),
  )

#   XD_01 XD_02 XD_03 XD_04 XD_05 XD_06 XD_07 XD_08 XD_09 XD_10 XD_11
#   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
# 1    83     3    37     3    56   199     2    49   156     0    33

base_lagg$VAL_XD = base_lagg$XD_01 + base_lagg$XD_02 + base_lagg$XD_03 + base_lagg$XD_04 + 
                  base_lagg$XD_05 + base_lagg$XD_06  + base_lagg$XD_07 + base_lagg$XD_08 + 
                  base_lagg$XD_09 + base_lagg$XD_11

base_lagg_XD = base_lagg %>% filter(VAL_XD != 0)

base_lagg_XD$DNE_norm = scale(base_lagg_XD$DNE)

modelo_XD <- lmer(DNE_norm ~ XD_01 + XD_02 + XD_03  + XD_05 + XD_06  
                            + XD_07 + XD_08 + XD_09 + XD_11 + (1 | ID),
  data = base_lagg_XD, REML = FALSE)
summary(modelo_XD)
r2(modelo_XD)  
# Sin Depurar (limpiando tampoco pasa nada)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: DNE_norm ~ XD_01 + XD_02 + XD_03 + XD_05 + XD_06 + XD_07 + XD_08 +  
#     XD_09 + XD_11 + (1 | ID)
#    Data: base_lagg_XD

#       AIC       BIC    logLik -2*log(L)  df.resid 
#    5685.3    5754.8   -2830.6    5661.3      2410 

# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -2.9584 -0.6927 -0.0122  0.6335  3.7888 

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  ID       (Intercept) 0.4231   0.6505  
#  Residual             0.5739   0.7575  
# Number of obs: 2422, groups:  ID, 37

# Fixed effects:
#               Estimate Std. Error         df t value Pr(>|t|)   
# (Intercept) -2.586e-02  1.235e-01  5.908e+01  -0.209  0.83495   
# XD_01        4.947e-02  7.509e-02  2.415e+03   0.659  0.51004   
# XD_02       -9.485e-01  3.525e-01  2.398e+03  -2.691  0.00717 **
# XD_03        9.650e-02  8.785e-02  2.403e+03   1.099  0.27209   
# XD_05       -6.093e-02  7.278e-02  2.422e+03  -0.837  0.40257   
# XD_06       -5.184e-03  6.058e-02  2.416e+03  -0.086  0.93181   
# XD_07       -1.113e-01  5.424e-01  2.387e+03  -0.205  0.83741   
# XD_08        3.130e-02  9.351e-02  2.419e+03   0.335  0.73782   
# XD_09        4.110e-02  6.394e-02  2.417e+03   0.643  0.52043   
# XD_11        8.570e-02  1.106e-01  2.369e+03   0.775  0.43856   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


