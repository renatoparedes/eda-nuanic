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


base = read.csv("Base.csv")


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

cold2 = list("ObsAA","ObsEst","ObsCon","ObsCaos")
for (col in cold2) {
  mini_base[[col]] <- as.numeric(
    gsub(",", ".", as.character(mini_base[[col]]), fixed = TRUE)
  )
}




missing_summary <- data.frame(
  Variable = names(mini_base),
  Missing_Count = colSums(is.na(mini_base)),
  `% Missing` = colSums(is.na(mini_base)) / nrow(mini_base) * 100
)
print(missing_summary)

#         Variable Missing_Count X..Missing
# ID            ID             0   0.000000
# Tiempo    Tiempo             0   0.000000
# DNE          DNE            52   1.278269
# ObsAA      ObsAA           343   8.431662
# ObsEst    ObsEst           492  12.094395
# ObsCon    ObsCon           312   7.669617
# ObsCaos  ObsCaos           235   5.776794

mini_base <- mini_base %>%
  drop_na(all_of(cols))

mini_base <- mini_base %>%
  mutate(Time_Block = ceiling(Tiempo / 5))

mini_base <- mini_base %>%
  group_by(ID, Time_Block) %>%
  summarise(
    Tiempo = max(Tiempo),
    DNE_mean = mean(DNE),
    DNE_std = sd(DNE),
    ObsAA = first(ObsAA),
    ObsEst = first(ObsEst),
    ObsCon = first(ObsCon),
    ObsCaos = first(ObsCaos),
    .groups = "drop"
  )


# 5. Calcular el Coeficiente de Variación (CV) para cada bloque
mini_base$DNE_CV = mini_base$DNE_std / mini_base$DNE_mean




mini_base$DNE_CV_nom <- scale(mini_base$DNE_CV)
mini_base$ObsAA_norm <- scale(mini_base$ObsAA)
mini_base$ObsEst_norm <- scale(mini_base$ObsEst)
mini_base$ObsCon_norm <- scale(mini_base$ObsCon)
mini_base$ObsCaos_norm <- scale(mini_base$ObsCaos)


# Model 1: Concurrent ObsCon -> DNE
md_ObsAA_con <- lmer(DNE_CV_nom ~ ObsAA_norm + (1 | ID),data = mini_base,REML = TRUE) # no sale
md_ObsEst_con <- lmer(DNE_CV_nom ~ ObsEst_norm + (1 | ID),data = mini_base,REML = TRUE) # sí sale
summary(md_ObsEst_con)
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.9606 -0.6401 -0.1491  0.4730  5.5583 

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  ID       (Intercept) 0.002463 0.04963 
#  Residual             0.006397 0.07998 
# Number of obs: 697, groups:  ID, 37

# Fixed effects:
#              Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept) 1.281e-01  8.758e-03 3.710e+01  14.627   <2e-16 ***
# ObsEst_norm 8.029e-03  4.007e-03 6.376e+02   2.004   0.0455 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

r2(md_ObsEst_norm)  
  # Conditional R2: 0.279
  #    Marginal R2: 0.007

md_Obs_con <- lmer(DNE_CV_nom ~ ObsCon_norm + (1 | ID),data = mini_base,REML = TRUE) # no sale
md_Obs_Caos <- lmer(DNE_CV_nom ~ ObsCaos_norm + (1 | ID),data = mini_base,REML = TRUE) # no sale
summary(md_Obs_Caos)
r2(md_Obs_Caos)


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

lagg_2 <- c("ObsAA","ObsEst","ObsCon","ObsCaos",
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
    "EmoNeg")

for (col in lagg_2) {
  base_lagg[[col]] <- as.numeric(
    gsub(",", ".", as.character(base_lagg[[col]]), fixed = TRUE)
  )
}

missing_summary <- data.frame(
  Variable = names(base_lagg),
  Missing_Count = colSums(is.na(base_lagg)),
  `% Missing` = colSums(is.na(base_lagg)) / nrow(base_lagg) * 100
)
print(missing_summary)

#                  Variable Missing_Count X..Missing
# ID                     ID             0   0.000000
# Tiempo             Tiempo             0   0.000000
# DNE                   DNE            52   1.278269
# ObsAA               ObsAA           343   8.431662
# ObsEst             ObsEst           492  12.094395
# ObsCon             ObsCon           312   7.669617
# ObsCaos           ObsCaos           235   5.776794
# ApoyoAut         ApoyoAut           108   2.654867
# Estruc             Estruc           108   2.654867
# Control           Control           108   2.654867
# Chaos               Chaos           108   2.654867
# SIS_AA             SIS_AA           108   2.654867
# SIS_Est           SIS_Est           108   2.654867
# SIS_Control   SIS_Control           108   2.654867
# SIS_Chaos       SIS_Chaos           108   2.654867
# NeedSat           NeedSat           108   2.654867
# NeedFru           NeedFru           108   2.654867
# NeedSup           NeedSup           108   2.654867
# EstDesm           EstDesm           108   2.654867
# AutSat             AutSat           108   2.654867
# CompSat           CompSat           108   2.654867
# SatRel             SatRel           108   2.654867
# FrusAut           FrusAut           108   2.654867
# FrusComp         FrusComp           108   2.654867
# FrusRel           FrusRel           108   2.654867
# SatLab             SatLab           108   2.654867
# EmoExha           EmoExha           108   2.654867
# Cinismo           Cinismo           108   2.654867
# Engagement     Engagement           108   2.654867
# Joy                   Joy           108   2.654867
# Anger               Anger           108   2.654867
# Anxiety           Anxiety           108   2.654867
# Hope                 Hope           108   2.654867
# Hopelessness Hopelessness           108   2.654867
# PerGrow           PerGrow           108   2.654867
# RelGrow           RelGrow           108   2.654867
# MasGrow           MasGrow           108   2.654867
# EducaMat         EducaMat           108   2.654867
# DesBeha           DesBeha           108   2.654867
# NoGoal             NoGoal           108   2.654867
# EmoPos             EmoPos           108   2.654867
# EmoNeg             EmoNeg           108   2.654867
# GD_01               GD_01             0   0.000000
# GD_02               GD_02             0   0.000000
# GD_03               GD_03             0   0.000000
# GD_04               GD_04             0   0.000000
# GD_05               GD_05             0   0.000000
# GD_06               GD_06             0   0.000000
# GD_07               GD_07             0   0.000000
# GD_08               GD_08             0   0.000000
# GD_09               GD_09             0   0.000000
# GD_10               GD_10             0   0.000000
# GD_11               GD_11             0   0.000000
# XD_01               XD_01             0   0.000000
# XD_02               XD_02             0   0.000000
# XD_03               XD_03             0   0.000000
# XD_04               XD_04             0   0.000000
# XD_05               XD_05             0   0.000000
# XD_06               XD_06             0   0.000000
# XD_07               XD_07             0   0.000000
# XD_08               XD_08             0   0.000000
# XD_09               XD_09             0   0.000000
# XD_10               XD_10             0   0.000000
# XD_11               XD_11             0   0.000000


base_lagg <- base_lagg %>%
  arrange(ID, Tiempo) %>%
  group_by(ID) %>%
  fill(DNE, ObsAA, ObsEst, ObsCon, ObsCaos, .direction = "down") %>%
  ungroup()

base_lagg <- base_lagg %>%
  drop_na(DNE, ObsAA, ObsEst, ObsCon, ObsCaos)

base_lagg <- base_lagg %>%
  mutate(Time_Block = ceiling(Tiempo / 5))

base_lagg <- base_lagg %>%
  group_by(ID, Time_Block) %>%
  summarise(
    Tiempo = max(Tiempo, na.rm = TRUE),
    DNE_mean = mean(DNE, na.rm = TRUE),
    DNE_std = sd(DNE, na.rm = TRUE),

    ObsAA = first(ObsAA),
    ObsEst = first(ObsEst),
    ObsCon = first(ObsCon),
    ObsCaos = first(ObsCaos),

    GD_01 = first(GD_01),
    GD_02 = first(GD_02),
    GD_03 = first(GD_03),
    GD_04 = first(GD_04),
    GD_05 = first(GD_05),
    GD_06 = first(GD_06),
    GD_07 = first(GD_07),
    GD_08 = first(GD_08),
    GD_09 = first(GD_09),
    GD_10 = first(GD_10),
    GD_11 = first(GD_11),

    XD_01 = first(XD_01),
    XD_02 = first(XD_02),
    XD_03 = first(XD_03),
    XD_04 = first(XD_04),
    XD_05 = first(XD_05),
    XD_06 = first(XD_06),
    XD_07 = first(XD_07),
    XD_08 = first(XD_08),
    XD_09 = first(XD_09),
    XD_10 = first(XD_10),
    XD_11 = first(XD_11),

    ApoyoAut = first(ApoyoAut),
    Estruc = first(Estruc),
    Control = first(Control),
    Chaos = first(Chaos),
    SIS_AA = first(SIS_AA),
    SIS_Est = first(SIS_Est),
    SIS_Control = first(SIS_Control),
    SIS_Chaos = first(SIS_Chaos),
    NeedSat = first(NeedSat),
    NeedFru = first(NeedFru),
    NeedSup = first(NeedSup),
    EstDesm = first(EstDesm),
    AutSat = first(AutSat),
    CompSat = first(CompSat),
    SatRel = first(SatRel),
    FrusAut = first(FrusAut),
    FrusComp = first(FrusComp),
    FrusRel = first(FrusRel),
    SatLab = first(SatLab),
    EmoExha = first(EmoExha),
    Cinismo = first(Cinismo),
    Engagement = first(Engagement),
    Joy = first(Joy),
    Anger = first(Anger),
    Anxiety = first(Anxiety),
    Hope = first(Hope),
    Hopelessness = first(Hopelessness),
    PerGrow = first(PerGrow),
    RelGrow = first(RelGrow),
    MasGrow = first(MasGrow),
    EducaMat = first(EducaMat),
    DesBeha = first(DesBeha),
    NoGoal = first(NoGoal),
    EmoPos = first(EmoPos),
    EmoNeg = first(EmoNeg),
    .groups = "drop"
  )


base_lagg$DNE_CV = base_lagg$DNE_std / base_lagg$DNE_mean
base_lagg$DNE_CV_nom <- scale(base_lagg$DNE_CV)
base_lagg$ObsAA_norm <- scale(base_lagg$ObsAA)
base_lagg$ObsEst_norm <- scale(base_lagg$ObsEst)
base_lagg$ObsCon_norm <- scale(base_lagg$ObsCon)
base_lagg$ObsCaos_norm <- scale(base_lagg$ObsCaos)


for (obs in c("ObsAA", "ObsEst", "ObsCon", "ObsCaos")) {
  col_norm <- paste0(obs, "_norm")
  col_lag <- paste0(obs, "_norm_lag1")

  base_lagg <- base_lagg %>%
    group_by(ID) %>%
    mutate(!!col_lag := lag(.data[[col_norm]], n = 1)) %>%
    ungroup()
}

base_lagg <- base_lagg %>%
  drop_na(ObsAA_norm_lag1)


# Model 2: Lagged ObsCon -> DNE

md_ObsAA_lag <- lmer(DNE_CV_nom ~ ObsAA_norm_lag1 + (1 | ID),data = base_lagg,REML = TRUE) # no sale
md_ObsEst_lag <- lmer(DNE_CV_nom ~ ObsEst_norm_lag1 + (1 | ID),data = base_lagg,REML = TRUE) # no sale
md_ObsCon_lag <- lmer(DNE_CV_nom ~ ObsCon_norm_lag1 + (1 | ID),data = base_lagg,REML = TRUE) # no sale
md_Obs_Caos_lag <- lmer(DNE_CV_nom ~ ObsCaos_norm_lag1 + (1 | ID),data = base_lagg,REML = TRUE) # no sale


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

## Sin borrar los GD = 0
modelo_GD <- lmer(DNE_CV_nom ~ GD_01 + GD_02 + GD_03 + GD_04 + GD_05 + GD_06  
                            + GD_07 + GD_08 + GD_09 + (1 | ID),
  data = base_lagg, REML = FALSE)

## Borrando los GD = 0

# modelo_GD_2 <- lmer(DNE_CV_nom ~ GD_01 + GD_02 + GD_03 + GD_04 + GD_05 + GD_06 + GD_07 + GD_08 + GD_09 + (1 | ID),
#   data = base_lagg_GD, REML = FALSE)

modelo_GD_2 <- lmer(DNE_CV_nom ~   GD_07  + GD_09 + (1 | ID),
  data = base_lagg_GD, REML = FALSE)

summary(modelo_GD_2)
r2(modelo_GD_2)  
#Sin filtrar

# Fixed effects:
#               Estimate Std. Error         df t value Pr(>|t|)  
# (Intercept)  -0.052586   0.140643 169.809984  -0.374   0.7089  
# GD_01        -0.022703   0.317697 717.119426  -0.071   0.9431  
# GD_02        -0.053510   0.164062 724.759650  -0.326   0.7444  
# GD_03         0.110338   0.234658 715.678998   0.470   0.6383  
# GD_04        -0.212872   0.209481 715.182994  -1.016   0.3099  
# GD_05         0.003485   0.111327 721.067932   0.031   0.9750  
# GD_06        -0.069356   0.121476 654.570244  -0.571   0.5682  
# GD_07         0.242201   0.119663 711.327707   2.024   0.0433 *
# GD_08         0.077838   0.126226 718.026181   0.617   0.5377  
# GD_09        -0.295851   0.145398 713.358810  -2.035   0.0422 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Filrado

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  ID       (Intercept) 0.2601   0.510   
#  Residual             0.6626   0.814   
# Number of obs: 728, groups:  ID, 37

# Fixed effects:
#              Estimate Std. Error        df t value Pr(>|t|)  
# (Intercept)  -0.05969    0.09229  42.30060  -0.647   0.5213  
# GD_07         0.24877    0.09994 696.20172   2.489   0.0130 *
# GD_09        -0.29159    0.13542 708.47516  -2.153   0.0316 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# # Conditional R2: 0.294
# #    Marginal R2: 0.017

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

modelo_XD <- lmer(DNE_CV_nom ~ XD_01 + XD_02 + XD_03  + XD_05 + XD_06  
                            + XD_07 + XD_08 + XD_09 + XD_11 + (1 | ID),
  data = base_lagg_XD, REML = FALSE)
summary(modelo_XD)
r2(modelo_XD)  
# Sin Depurar (limpiando tampoco pasa nada)

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  ID       (Intercept) 0.2499   0.4999  
#  Residual             0.6781   0.8234  
# Number of obs: 487, groups:  ID, 37

# Fixed effects:
#              Estimate Std. Error        df t value Pr(>|t|)
# (Intercept)  -0.09706    0.17263 269.30443  -0.562    0.574
# XD_01         0.21954    0.14933 474.69085   1.470    0.142
# XD_02         0.22880    0.61931 470.64586   0.369    0.712
# XD_03         0.14446    0.20875 473.11957   0.692    0.489
# XD_04        -0.08208    0.50301 466.85585  -0.163    0.870
# XD_05        -0.05506    0.15771 486.62311  -0.349    0.727
# XD_06         0.04083    0.14812 484.59421   0.276    0.783
# XD_07         0.62112    0.61476 467.96705   1.010    0.313
# XD_08        -0.13677    0.16411 483.44161  -0.833    0.405
# XD_09         0.03875    0.16503 483.64038   0.235    0.814
# XD_11         0.34997    0.29037 413.90776   1.205    0.229


## Validar atipicos en EDA
ggplot(base_lagg, aes(x = Tiempo, y = DNE_CV)) +
  geom_line() +
  facet_wrap(~ ID, scales = "free_x") +
  labs(
    title = "Inspección de la señal DNE",
    x = "Tiempo",
    y = "DNE"
  ) +
  theme_minimal()

### Modelo 4: Autoreporte
table(base_lagg$NeedSup)
base_self  = base_lagg %>% filter(NeedSup > 0)
  
base_self$MasGrow_z =scale(base_self$MasGrow)
base_self$CompSat_z =scale(base_self$CompSat)
base_self$Chaos_z =scale(base_self$Chaos)
base_self$FrusComp_z =scale(base_self$FrusComp)


# Construir los Índices Compuestos (Promedio)
base_self$Idx_Eustres = (base_self$MasGrow_z + base_self$CompSat_z) / 2
base_self$Idx_Distres = (base_self$Chaos_z + base_self$FrusComp_z) / 2

# ACP Eustress
base_self <- base_self %>%
  mutate(
    Eustress_ACP = prcomp(
      cbind(MasGrow_z, CompSat_z),
      center = TRUE,
      scale. = TRUE
    )$x[, 1]
  )

base_self <- base_self %>%
  mutate(
    Distress_ACP = prcomp(
      cbind(Chaos_z, FrusComp_z),
      center = TRUE,
      scale. = TRUE
    )$x[, 1]
  )

base_self$Distress_ACP_Z = scale(base_self$Distress_ACP)
base_self$Eustress_ACP_Z = scale(base_self$Eustress_ACP)

# Modelo 5: Comparativa

m_base  <- lmer(DNE_CV_nom ~ ObsEst_norm +(1 | ID), data = base_self, REML = FALSE)
summary(m_base)
r2(m_base)  


m_block1  <- lmer(DNE_CV_nom ~ ObsEst_norm + Estruc + Chaos +(1 | ID), data = base_self, REML = FALSE)
summary(m_block1)
r2(modelo_XD)  

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  ID       (Intercept) 0.2179   0.4668  
#  Residual             0.7107   0.8430  
# Number of obs: 753, groups:  ID, 36

# Fixed effects:
#              Estimate Std. Error        df t value Pr(>|t|)   
# (Intercept)  -2.18365    1.08139  38.74692  -2.019  0.05041 . 
# ObsEst_norm   0.10744    0.03849 726.61661   2.792  0.00538 **
# Estruc        0.25554    0.16053  38.56445   1.592  0.11958   
# Chaos         0.36717    0.16250  36.99014   2.259  0.02983 * 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Conditional R2: 0.278
#    Marginal R2: 0.014

m_block2 =  lmer(DNE_CV_nom ~ ObsEst_norm + CompSat + FrusComp +(1 | ID), data = base_self, REML = FALSE)
summary(m_block2)

#    Data: base_self

#       AIC       BIC    logLik -2*log(L)  df.resid 
#    1961.3    1989.0    -974.6    1949.3       747 

# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.8021 -0.6361 -0.1461  0.4916  5.7748 

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  ID       (Intercept) 0.2075   0.4556  
#  Residual             0.7109   0.8432  
# Number of obs: 753, groups:  ID, 36

# Fixed effects:
#              Estimate Std. Error        df t value Pr(>|t|)  
# (Intercept)  -3.42799    1.32746  36.66349  -2.582   0.0139 *
# ObsEst_norm   0.09370    0.03826 711.07442   2.449   0.0146 *
# CompSat       0.46404    0.18853  36.67924   2.461   0.0187 *
# FrusComp      0.23984    0.09756  36.38840   2.458   0.0188 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
r2(m_block2)  


m_block3 =  lmer(DNE_CV_nom ~ ObsEst_norm + MasGrow + EducaMat + NoGoal +(1 | ID), data = base_self, REML = FALSE)
summary(m_block3)

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  ID       (Intercept) 0.2233   0.4725  
#  Residual             0.7109   0.8431  
# Number of obs: 753, groups:  ID, 36

# Fixed effects:
#              Estimate Std. Error        df t value Pr(>|t|)  
# (Intercept)  -2.04261    1.01551  38.53558  -2.011   0.0513 .
# ObsEst_norm   0.09788    0.03841 720.78460   2.549   0.0110 *
# MasGrow       0.33155    0.15111  38.19842   2.194   0.0344 *
# EducaMat     -0.07543    0.06265  36.01968  -1.204   0.2365  
# NoGoal        0.06502    0.10206  37.04783   0.637   0.5280  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

r2(m_block3)  

m_block4 =  lmer(DNE_CV_nom ~ ObsEst_norm + EmoPos + EmoNeg +(1 | ID), data = base_self, REML = FALSE)
summary(m_block4)

# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.7923 -0.6520 -0.1494  0.4817  5.7857 

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  ID       (Intercept) 0.2375   0.4873  
#  Residual             0.7108   0.8431  
# Number of obs: 753, groups:  ID, 36

# Fixed effects:
#              Estimate Std. Error        df t value Pr(>|t|)   
# (Intercept)  -2.09305    1.29275  42.13578  -1.619  0.11289   
# ObsEst_norm   0.10382    0.03858 731.37403   2.691  0.00729 **
# EmoPos        0.30602    0.17432  41.86711   1.756  0.08649 . 
# EmoNeg        0.06668    0.14723  39.14693   0.453  0.65311   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

r2(m_block4)  


m_block5 =  lmer(DNE_CV_nom ~ ObsEst_norm + Idx_Eustres + Idx_Distres +(1 | ID), data = base_self, REML = FALSE)
summary(m_block5)


#       AIC       BIC    logLik -2*log(L)  df.resid 
#    1954.9    1982.6    -971.4    1942.9       747 

# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.7765 -0.6631 -0.1373  0.4792  5.8013 

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  ID       (Intercept) 0.1683   0.4102  
#  Residual             0.7108   0.8431  
# Number of obs: 753, groups:  ID, 36

# Fixed effects:
#              Estimate Std. Error        df t value Pr(>|t|)   
# (Intercept)   0.02410    0.07560  37.27495   0.319  0.75173   
# ObsEst_norm   0.10102    0.03785 679.99768   2.669  0.00779 **
# Idx_Eustres   0.31738    0.09419  38.43805   3.370  0.00172 **
# Idx_Distres   0.36212    0.10421  37.11948   3.475  0.00132 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

r2(m_block5)  


m_block6 =  lmer(DNE_CV_nom ~ ObsEst_norm + Eustress_ACP_Z + Distress_ACP_Z +(1 | ID), data = base_self, REML = FALSE)
summary(m_block6)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']

# Formula: DNE_CV_nom ~ ObsEst_norm + Eustress_ACP_Z + Distress_ACP_Z +      (1 | ID)
#    Data: base_self

#       AIC       BIC    logLik -2*log(L)  df.resid 
#    1954.9    1982.6    -971.4    1942.9       747 

# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.7765 -0.6631 -0.1373  0.4792  5.8013 

# Random effects:
#  Groups   Name        Variance Std.Dev.
#  ID       (Intercept) 0.1683   0.4102  
#  Residual             0.7108   0.8431  
# Number of obs: 753, groups:  ID, 36

# Fixed effects:
#                 Estimate Std. Error        df t value Pr(>|t|)   
# (Intercept)      0.02410    0.07560  37.27495   0.319  0.75173   
# ObsEst_norm      0.10102    0.03785 679.99768   2.669  0.00779 **
# Eustress_ACP_Z  -0.27386    0.08127  38.43805  -3.370  0.00172 **
# Distress_ACP_Z  -0.28957    0.08333  37.11948  -3.475  0.00132 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

r2(m_block6)  

#  Conditional R2: 0.272
#      Marginal R2: 0.100

names(base_self)
base_self$Engagement_z = scale(base_self$Engagement)
base_self$Hope_z = scale(base_self$Hope)
base_self$FrusAut_z = scale(base_self$FrusAut)
base_self$FrusRel_z = scale(base_self$FrusRel)
base_self$Anxiety_z = scale(base_self$Anxiety)
base_self$Hopelessness_z = scale(base_self$Hopelessness)



base_self <- base_self %>%
  mutate(
    Eustress_ACP_2 = prcomp(
      cbind(MasGrow_z, CompSat_z,Engagement_z,Hope_z),
      center = TRUE,
      scale. = TRUE
    )$x[, 1]
  )

base_self <- base_self %>%
  mutate(
    Distress_ACP_2 = prcomp(
      cbind(Chaos_z, FrusComp_z,FrusAut_z),
      center = TRUE,
      scale. = TRUE
    )$x[, 1]
  )

base_self$Distress_ACP_2_Z = scale(base_self$Distress_ACP_2)
base_self$Eustress_ACP_2_Z = scale(base_self$Eustress_ACP_2)


## Supuestos  Normalidad de residuos #####
plot(m_block5)
qqnorm(resid(m_block5))
qqline(resid(m_block5), col = "red")

performance::check_normality(m_block5)
#Warning: Non-normality of residuals detected (p < .001).

e <- resid(m_block5)
mean(e) #1
sd(e) #2
moments::skewness(e) #3
moments::kurtosis(e) #4
check_collinearity(m_block5)
#         Term  VIF       VIF 95% CI adj. VIF Tolerance Tolerance 95% CI
#  ObsEst_norm 1.00 [1.00,      Inf]     1.00      1.00     [0.00, 1.00]
#  Idx_Eustres 1.20 [1.12,     1.33]     1.10      0.83     [0.75, 0.89]
#  Idx_Distres 1.20 [1.12,     1.33]     1.10      0.83     [0.75, 0.89]

m_block7 =  lmer(DNE_CV_nom ~ ObsEst_norm + Distress_ACP_2_Z + Eustress_ACP_2_Z +(1 | ID), data = base_self, REML = FALSE)
summary(m_block7)
r2(m_block7)

## Probando Modelos 


anova(m_base,m_block1, test = "Chisq") # Nel pastel
anova(m_base,m_block2, test = "Chisq") # Sí
anova(m_base,m_block3, test = "Chisq") # Nel pastel
anova(m_base,m_block4, test = "Chisq") # Nel pastel
anova(m_base,m_block5, test = "Chisq") # Sí
anova(m_base,m_block6, test = "Chisq") # Sí
anova(m_base,m_block7, test = "Chisq") # Sí



test_block_significance <- function(m_base, model_block, block_name) {
  
  resultado <- anova(m_base, model_block, test = "Chisq")
  
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
  m_base,
  m_block1,
  "Bloque 1: Estilos Docentes"
)

# --- Bloque 1: Estilos Docentes ---
# Estadístico LRT: 5.947 | Grados de Libertad: 2 | p-valor: 0.0511 
# -> El bloque NO produce una mejora significativa del ajuste.

test_block_significance(
  m_base,
  m_block2,
  "Bloque 2"
)

# --- Bloque 2 ---
# Estadístico LRT: 7.201 | Grados de Libertad: 2 | p-valor: 0.0273 
# -> El bloque MEJORA significativamente el ajuste del modelo.

test_block_significance(
  m_base,
  m_block3,
  "Bloque 3"
)

# --- Bloque 3 ---
# Estadístico LRT: 5.001 | Grados de Libertad: 3 | p-valor: 0.1718 
# -> El bloque NO produce una mejora significativa del ajuste.


test_block_significance(
  m_base,
  m_block4,
  "Bloque 4"
)

# --- Bloque 4 ---
# Estadístico LRT: 3.224 | Grados de Libertad: 2 | p-valor: 0.1995 
# -> El bloque NO produce una mejora significativa del ajuste.

test_block_significance(
  m_base,
  m_block5,
  "Bloque 5"
)

# --- Bloque 5 ---
# Estadístico LRT: 13.636 | Grados de Libertad: 2 | p-valor: 0.0011 
# -> El bloque MEJORA significativamente el ajuste del modelo.

test_block_significance(
  m_base,
  m_block6,
  "Bloque 3"
)
