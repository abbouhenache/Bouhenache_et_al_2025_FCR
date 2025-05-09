#### Script for ANOVA and Tukey test statistical analysis Bouhenache at al. 2025 FCR ####

### Script by: Abderrahim BOUHENACHE

# Detach all non-base packages in one-liner
invisible(lapply(setdiff(loadedNamespaces(), c("base", "stats", "utils", "methods", "datasets", "graphics", "grDevices")),
                 function(pkg) try(detach(paste("package", pkg, sep = ":"), unload = TRUE, character.only = TRUE), silent = TRUE)))

# Clear all objects from the working environment
rm(list = ls())

# Clear the console
cat("\014") 

# Clear all the graphics
if (!is.null(dev.list())) dev.off()

#### Call necessary packages and set directories ####

#### Import and process the data ####
## Call necessary packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, readr, here, lme4, lmerTest, car, multcompView,
               openxlsx, emmeans, performance)


# Create a path for saving figures
vis_path <- here("Bouhenache_et_al_2025_FCR_outputs", "Figures")

# Create a path for saving figures
tables_path <- here("Bouhenache_et_al_2025_FCR_outputs", "Tables")

## change system locale settings to English for x axis lab
Sys.setlocale("LC_ALL", "English")


#### Import and prepare data ####

# TAGB data 
AGB_data <- read_xlsx(here("Bouhenache_et_al_2025_FCR_data", "Bouhenache_et_al_2025_FCR_data.xlsx"), sheet = "TAGB_N_data")[-1, ]


AGB_data <- AGB_data %>%
  # drop non necessary variable of dry weight percentage
  select(c(Crop_season:Nitrogen, AGB_DM_t_ha, Harvest_index, AGB_N_uptake_kg_ha, Tot_N_AGB, N_harvest_index)) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
  mutate_at(vars(AGB_DM_t_ha, Harvest_index, AGB_N_uptake_kg_ha, Tot_N_AGB, N_harvest_index), as.numeric) %>%
  mutate_if(is.character, as.factor) %>%
  filter(!Rain_Treat =="PR") %>%
  droplevels()


# Grain yield data 
GY_data <- read_xlsx(here("Bouhenache_et_al_2025_FCR_data", "Bouhenache_et_al_2025_FCR_data.xlsx"), sheet = "Grain_yield_data")[-1, ]

GY_data <- GY_data %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
  mutate(across(Numb_ears_plant:G_yield_t_ha_moist_standard, ~ round(as.numeric(.), 2))) %>%
  mutate_if(is.character, as.factor) %>%
  filter(!Rain_Treat =="PR") %>%
  droplevels()



#### TAGB stats ####


### At V6 stage ####


## extract 
AGB_data_V6 <- AGB_data %>%
  filter(Sampling_stage == "V6")

## extract df of harvest for 2022-2023
AGB_data_V6_23 <- AGB_data %>%
  filter(Crop_season == "2022-2023", Sampling_stage == "V6") 


## extract df of harvest for 2023-2024
AGB_data_V6_24 <- AGB_data %>%
  filter(Crop_season == "2023-2024", Sampling_stage == "V6")

### 2022-2023 ###

## try to fit in a model that represent the best the experimental design
# model with two random effect: block replicate (random variation between blocks) and Rain_Treat in block replicate (randomization of rain factor within each block replicate)
AGB_V6_model_23_1 <- lmer(AGB_DM_t_ha ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_V6_23)

## Check variance from random effect
summary(AGB_V6_model_23_1)

## leave only the random effect of block as there is no variance attributed to the block effect
AGB_V6_model_23_2 <- lmer(AGB_DM_t_ha ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate), data = AGB_data_V6_23)

## Check variance from random effect
summary(AGB_V6_model_23_2)

## Compare models performance to confirm the choice
compare_performance(AGB_V6_model_23_1, AGB_V6_model_23_2)

## Logically, we consider AIC and BIC with R2 conditional (accounting for both random and fixed effects)
## So we take the lowest AIC and BIC for model Complexity,  RMSE and R2 (cond.) are the same

##### CONCLUSION: The model to keep is AGB_V6_model_23_2

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(AGB_V6_model_23_2, effects = c("fixed", "random"))

# -> p-value = 0.181 which high and > 0.05 and thus normality is verified

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_V6_model_23_2, effects = c("fixed", "random"))

# -> p-value = 0.494 which is high and > 0.05, homogeneity verified

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_V6_model_23_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned (homogeneity accepted)
# which is in accordance with statistical tests: we can perform anova according to this test

## Very weird linearity and homogeneity distributions but statistical test is valid 

## Perform ANOVA

Anova(AGB_V6_model_23_2)

### No need for Turkey's test


### 2023-2024 ###

## try to fit in a model that represent the best the experimental design
# model with two random effect: block replicate (random variation between blocks) and Rain_Treat in block replicate (randomization of rain factor within each block replicate)
AGB_V6_model_24_1 <- lmer(AGB_DM_t_ha ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_V6_24)

## Check variance from random effect
summary(AGB_V6_model_24_1)

## Remove the random effect of block as there is no variance attributed to the block effect
AGB_V6_model_24_2 <- lmer(AGB_DM_t_ha ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate:Rain_Treat), data = AGB_data_V6_24)

## Check variance from random effect
summary(AGB_V6_model_24_2)

## Compare models performance to confirm the choice
compare_performance(AGB_V6_model_24_1, AGB_V6_model_24_2)

## Logically, we consider AIC and BIC with R2 conditional (accounting for both random and fixed effects)
## So we take the lowest AIC and BIC for model Complexity, and higher R2 (cond.), RMSE is the same

##### CONCLUSION: The model to keep is AGB_V6_model_24_2

## test normality and homogeneity on the residuals of this model
## a. using statistical tests

## Shapiro test from performance package
check_normality(AGB_V6_model_24_2, effects = c("fixed", "random"))

# -> p-value = 0.304 which high and > 0.05 and thus normality is verified

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_V6_model_24_2, effects = c("fixed", "random"))

# -> p-value < .001, homogeneity suspected and we check visually

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_V6_model_24_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned (homogeneity accepted)
# which is in accordance with statistical tests: we can perform anova according to this test


## Perform ANOVA

Anova(AGB_V6_model_24_2)

### Turkey's test

Tukey_AGB_V6_24_M <- emmeans::emmeans(AGB_V6_model_24_2, pairwise ~  Mulch, adjust = "tukey")
summary(Tukey_AGB_V6_24_M)
Tukey_AGB_V6_24_M <- multcomp::cld(Tukey_AGB_V6_24_M$emmeans, 
                                         alpha=.05,
                                         Letters = letters, reversed = TRUE)
print(Tukey_AGB_V6_24_M)


Tukey_AGB_V6_24_Rain_M <- emmeans::emmeans(AGB_V6_model_24_2, pairwise ~ Rain_Treat:Mulch, adjust = "tukey")
summary(Tukey_AGB_V6_24_Rain_M)
Tukey_AGB_V6_24_Rain_M <- multcomp::cld(Tukey_AGB_V6_24_Rain_M$emmeans, 
                                           alpha=.05,
                                           Letters = letters, reversed = TRUE)
print(Tukey_AGB_V6_24_Rain_M)


#### At V10 stage ####


## extract 
AGB_data_V10 <- AGB_data %>%
  filter(Sampling_stage == "V10")

## extract df of harvest for 2022-2023
AGB_data_V10_23 <- AGB_data %>%
  filter(Crop_season == "2022-2023", Sampling_stage == "V10") 


## extract df of harvest for 2023-2024
AGB_data_V10_24 <- AGB_data %>%
  filter(Crop_season == "2023-2024", Sampling_stage == "V10")

### AGB variable ###


### 2022-2023 ###

## try to fit in a model that represent the best the experimental design
# model with two random effect: block replicate (random variation between blocks) and Rain_Treat in block replicate (randomization of rain factor within each block replicate)
AGB_V10_model_23_1 <- lmer(AGB_DM_t_ha ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_V10_23)

## Check variance from random effect
summary(AGB_V10_model_23_1)

## leave only the random effect of Rainfall nested in block as there is no variance attributed to the block effect
AGB_V10_model_23_2 <- lmer(AGB_DM_t_ha ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate), data = AGB_data_V10_23)

## Check variance from random effect
summary(AGB_V10_model_23_2)

## Compare models performance to confirm the choice
compare_performance(AGB_V10_model_23_1, AGB_V10_model_23_2)

## Logically, we consider AIC and BIC with R2 conditional (accounting for both random and fixed effects)
## So we take the lowest AIC and BIC for model Complexity, and the higher R2 (cond.), RMSE is the same

##### CONCLUSION: The model to keep is AGB_V10_model_23_2

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(AGB_V10_model_23_2, effects = c("fixed", "random"))

# -> p-value = 0.748 which high and > 0.05 and thus normality is verified

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_V10_model_23_2, effects = c("fixed", "random"))

# -> p-value 0.967 which is high and > 0.05, homogeneity verified

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_V10_model_23_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned 
# which is in accordance with statistical tests: we can perform anova according to this test

## Perform ANOVA

Anova(AGB_V10_model_23_2)

### Turkey's test on rainfall

Tukey_AGB_V10_Rain_23 <- emmeans::emmeans(AGB_V10_model_23_2, pairwise ~ Rain_Treat, adjust = "tukey")
summary(Tukey_AGB_V10_Rain_23)
Tukey_AGB_V10_Rain_23 <- multcomp::cld(Tukey_AGB_V10_Rain_23$emmeans, 
                                              alpha=.05,
                                              Letters = letters, reversed = TRUE)
print(Tukey_AGB_V10_Rain_23)


Tukey_AGB_V10_N_23 <- emmeans::emmeans(AGB_V10_model_23_2, pairwise ~ Nitrogen, adjust = "tukey")
summary(Tukey_AGB_V10_N_23)
Tukey_AGB_V10_N_23 <- multcomp::cld(Tukey_AGB_V10_N_23$emmeans, 
                                           alpha=.05,
                                           Letters = letters, reversed = TRUE)
print(Tukey_AGB_V10_N_23)


Tukey_AGB_V10_Rain_M_23 <- emmeans::emmeans(AGB_V10_model_23_2, pairwise ~ Rain_Treat:Mulch, adjust = "tukey")
summary(Tukey_AGB_V10_Rain_M_23)
Tukey_AGB_V10_Rain_M_23 <- multcomp::cld(Tukey_AGB_V10_Rain_M_23$emmeans, 
                                              alpha=.05,
                                              Letters = letters, reversed = TRUE)
print(Tukey_AGB_V10_Rain_M_23)


Tukey_AGB_V10_Rain_M_N_23 <- emmeans::emmeans(AGB_V10_model_23_2, pairwise ~ Rain_Treat:Mulch:Nitrogen, adjust = "tukey")
summary(Tukey_AGB_V10_Rain_M_N_23)
Tukey_AGB_V10_Rain_M_N_23 <- multcomp::cld(Tukey_AGB_V10_Rain_M_N_23$emmeans, 
                                                alpha=.05,
                                                Letters = letters, reversed = TRUE)
print(Tukey_AGB_V10_Rain_M_N_23)



### 2023-2024 ###

## try to fit in a model that represent the best the experimental design
# model with two random effect: block replicate (random variation between blocks) and Rain_Treat in block replicate (randomization of rain factor within each block replicate)
AGB_V10_model_24_1 <- lmer(AGB_DM_t_ha ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_V10_24)

## Check variance from random effect
summary(AGB_V10_model_24_1)

## leave only the random effect of Rainfall nested in block as there is no variance attributed to the block effect
AGB_V10_model_24_2 <- lmer(AGB_DM_t_ha ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate:Rain_Treat), data = AGB_data_V10_24)

## Check variance from random effect
summary(AGB_V10_model_24_2)

## Compare models performance to confirm the choice
compare_performance(AGB_V10_model_24_1, AGB_V10_model_24_2)

## Logically, we consider AIC and BIC with R2 conditional (accounting for both random and fixed effects)
## So we take the lowest AIC and BIC for model Complexity, and the higher R2 (cond.), RMSE is the same

##### CONCLUSION: The model to keep is AGB_V10_model_24_2

## test normality and homogeneity on the residuals of this model
## a. using statistical tests

## Shapiro test from performance package
check_normality(AGB_V10_model_24_2, effects = c("fixed", "random"))

# -> p-value = 0.630 which high and > 0.05 and thus normality is verified

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_V10_model_24_2, effects = c("fixed", "random"))

# -> p-value 0.503 which is high and > 0.05, homogeneity verified

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_V10_model_24_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned 
# which is in accordance with statistical tests: we can perform anova according to this test


## Perform ANOVA

Anova(AGB_V10_model_24_2)

### Turkey's test on rainfall

Tukey_AGB_V10_M_24 <- emmeans::emmeans(AGB_V10_model_24_2, pairwise ~ Mulch, adjust = "tukey")
summary(Tukey_AGB_V10_M_24)
Tukey_AGB_V10_M_24 <- multcomp::cld(Tukey_AGB_V10_M_24$emmeans, 
                                         alpha=.05,
                                         Letters = letters, reversed = TRUE)
print(Tukey_AGB_V10_M_24)







### At flowering ####

## extract data
AGB_data_FLW <- AGB_data %>%
  filter(Sampling_stage == "R1")

## extract df of harvest for 2022-2023
AGB_data_FLW_23 <- AGB_data %>%
  filter(Crop_season == "2022-2023", Sampling_stage == "R1") 


## extract df of harvest for 2023-2024
AGB_data_FLW_24 <- AGB_data %>%
  filter(Crop_season == "2023-2024", Sampling_stage == "R1")



### 2022-2023 ###

## try to fit in a model that represent the best the experimental design
# model with two random effect: block replicate (random variation between blocks) and Rain_Treat in block replicate (randomization of rain factor within each block replicate)
AGB_FLW_model_23_1 <- lmer(AGB_DM_t_ha ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_FLW_23)

## Check variance from random effect
summary(AGB_FLW_model_23_1)

## leave only the random effect of Rainfall nested in block as there is no variance attributed to the block effect
AGB_FLW_model_23_2 <- lmer(AGB_DM_t_ha ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate:Rain_Treat), data = AGB_data_FLW_23)

## Check variance from random effect
summary(AGB_FLW_model_23_2)

## Compare models performance to confirm the choice
compare_performance(AGB_FLW_model_23_1, AGB_FLW_model_23_2)

## Logically, we consider AIC and BIC with R2 conditional (accounting for both random and fixed effects)
## So we take the lowest AIC and BIC for model Complexity, and the higher R2 (cond.), RMSE is the same

##### CONCLUSION: The model to keep is AGB_FLW_model_23_2

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

check_normality(AGB_FLW_model_23_2, effects = c("fixed", "random"))

# -> p-value = 0.161 which high and > 0.05 and thus data is following
# a normal distribution: we can perform anova according to this test

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_FLW_model_23_2, effects = c("fixed", "random"))

# -> p-value 0.454 which is high and > 0.05, homogeneity verified

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_FLW_model_23_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned (some points seem to be far but still correct)
# which is in accordance with statistical tests: we can perform anova according to this test

## Perform ANOVA

Anova(AGB_FLW_model_23_2)

### Turkey's test on rainfall

Tukey_AGB_FLW_Rain_23 <- emmeans::emmeans(AGB_FLW_model_23_2, pairwise ~ Rain_Treat, adjust = "tukey")
summary(Tukey_AGB_FLW_Rain_23)
Tukey_AGB_FLW_Rain_23 <- multcomp::cld(Tukey_AGB_FLW_Rain_23$emmeans, 
                                         alpha=.05,
                                         Letters = letters, reversed = TRUE)
print(Tukey_AGB_FLW_Rain_23)



### 2023-2024 ###

## try to fit in a model that represent the best the experimental design
# model with two random effect: block replicate (random variation between blocks) and Rain_Treat in block replicate (randomization of rain factor within each block replicate)
AGB_FLW_model_24_1 <- lmer(AGB_DM_t_ha ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_FLW_24)

## Check variance from random effect
summary(AGB_FLW_model_24_1)

## we can remove block random effects its variance is 0, but we keep only Replicate:Rain_Treat 
AGB_FLW_model_24_2 <- lmer(AGB_DM_t_ha ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate:Rain_Treat), data = AGB_data_FLW_24)

## Check variance from random effect
summary(AGB_FLW_model_24_2)

## Compare models performance to confirm the choice
compare_performance(AGB_FLW_model_24_1, AGB_FLW_model_24_2)

## Logically, we consider AIC and BIC with R2 conditional (accounting for both random and fixed effects)
## So we take the lowest AIC and BIC for model Complexity with higher R2 conditional, RMSE is similar

##### CONCLUSION: The model to keep is AGB_FLW_model_24_2

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(AGB_FLW_model_24_2, effects = c("fixed", "random"))

# -> p-value = 0.540 which high and > 0.05 and thus data is following
# a normal distribution: we can perform anova according to this test

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_FLW_model_24_2, effects = c("fixed", "random"))

# -> p-value 0.164 which is high and > 0.05, homogeneity verified

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_FLW_model_24_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned (some points seem to be far but still correct)
# which is in accordance with statistical tests: we can perform anova according to this test

## Perform ANOVA

Anova(AGB_FLW_model_24_2)


Tukey_AGB_FLW_M_24 <- emmeans::emmeans(AGB_FLW_model_24_2, pairwise ~ Mulch , adjust = "tukey")
summary(Tukey_AGB_FLW_M_24)
Tukey_AGB_FLW_M_24 <- multcomp::cld(Tukey_AGB_FLW_M_24$emmeans, 
                                         alpha=.05,
                                         Letters = letters, reversed = TRUE)
print(Tukey_AGB_FLW_M_24)



### At harvest ####

## extract 
AGB_data_HVST <- AGB_data %>%
  filter(Sampling_stage == "R6")

## extract df of harvest for 2022-2023
AGB_data_HVST_23 <- AGB_data %>%
  filter(Crop_season == "2022-2023", Sampling_stage == "R6") 


## extract df of harvest for 2023-2024
AGB_data_HVST_24 <- AGB_data %>%
  filter(Crop_season == "2023-2024", Sampling_stage == "R6")

### 2022-2023 ###

## try to fit in a model that represent the best the experimental design
# model with two random effect: block replicate (random variation between blocks) and Rain_Treat in block replicate (randomization of rain factor within each block replicate)
AGB_HVST_model_23_1 <- lmer(AGB_DM_t_ha ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_HVST_23)

## Check variance from random effect
summary(AGB_HVST_model_23_1)

## leave only the random effect of Rainfall nested in block as there is no variance attributed to the block effect
AGB_HVST_model_23_2 <- lmer(AGB_DM_t_ha ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate:Rain_Treat), data = AGB_data_HVST_23)

## Check variance from random effect
summary(AGB_HVST_model_23_2)

## Compare models performance to confirm the choice
compare_performance(AGB_HVST_model_23_1, AGB_HVST_model_23_2)

## Logically, we consider AIC and BIC with R2 conditional (accounting for both random and fixed effects)
## So we take the lowest AIC and BIC for model Complexity, and the higher R2 (cond.), RMSE is the same

##### CONCLUSION: The model to keep is AGB_HVST_model_23_2

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(AGB_HVST_model_23_2, effects = c("fixed", "random"))

# -> p-value = 0.519 which high and > 0.05 and thus data is following
# a normal distribution: we can perform anova according to this test

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_HVST_model_23_2, effects = c("fixed", "random"))

# -> p-value = 0.041 which is significant but on the limit 0.05, we accept the values for homogeneity

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_HVST_model_23_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned (some points seem to be far but still correct)
# which is in accordance with statistical tests: we can perform anova according to this test

## Perform ANOVA

Anova(AGB_HVST_model_23_2)

### No need for Tukey's test because no factor effect is significant



### 2023-2024 ###

## try to fit in a model that represent the best the experimental design
# model with two random effect: block replicate (random variation between blocks) and Rain_Treat in block replicate (randomization of rain factor within each block replicate)
AGB_HVST_model_24_1 <- lmer(AGB_DM_t_ha ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_HVST_24)

## Check variance from random effect
summary(AGB_HVST_model_24_1)

## we can remove both random effects as their variances are 0, but we keep only block replicate to work with an lmer like for other variables
AGB_HVST_model_24_2 <- lmer(AGB_DM_t_ha ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate), data = AGB_data_HVST_24)

## Check variance from random effect
summary(AGB_HVST_model_24_2)

## Compare models performance to confirm the choice
compare_performance(AGB_HVST_model_24_1, AGB_HVST_model_24_2)


## Logically, we consider AIC and BIC with R2 conditional (accounting for both random and fixed effects)
## So we take the lowest AIC and BIC for model Complexity, other criteria are similar

##### CONCLUSION: The model to keep is AGB_HVST_model_24_2

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(AGB_HVST_model_24_2, effects = c("fixed", "random"))

# -> p-value = 0.264 which high and > 0.05 and thus data is following
# a normal distribution: we can perform anova according to this test

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_HVST_model_24_2, effects = c("fixed", "random"))

# -> p-value = 0.774 which is high and > 0.05, homogeneity is verified

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_HVST_model_24_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned (some points seem to be far but still correct)
# which is in accordance with statistical tests: we can perform anova according to this test

## Perform ANOVA

Anova(AGB_HVST_model_24_2)


Tukey_AGB_HVST_M_24 <- emmeans::emmeans(AGB_HVST_model_24_2, pairwise ~ Mulch, adjust = "tukey")
summary(Tukey_AGB_HVST_M_24)
Tukey_AGB_HVST_M_24 <- multcomp::cld(Tukey_AGB_HVST_M_24$emmeans, 
                                          alpha=.05,
                                          Letters = letters, reversed = TRUE)
print(Tukey_AGB_HVST_M_24)


Tukey_AGB_HVST_Rain_N_24 <- emmeans::emmeans(AGB_HVST_model_24_2, pairwise ~ Rain_Treat:Nitrogen, adjust = "tukey")
summary(Tukey_AGB_HVST_Rain_N_24)
Tukey_AGB_HVST_Rain_N_24 <- multcomp::cld(Tukey_AGB_HVST_Rain_N_24$emmeans, 
                                          alpha=.05,
                                          Letters = letters, reversed = TRUE)
print(Tukey_AGB_HVST_Rain_N_24)





#### N accumulation stats ####


### At V6 stage ####

# try to fit in a model that describes the experimental design

AGB_N_upt_model_V6_23_1 <- lmer(AGB_N_uptake_kg_ha ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_V6_23)

summary(AGB_N_upt_model_V6_23_1)

AGB_N_upt_model_V6_23_2 <- lmer(AGB_N_uptake_kg_ha ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate), data = AGB_data_V6_23)

compare_performance(AGB_N_upt_model_V6_23_1, AGB_N_upt_model_V6_23_2)


## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(AGB_N_upt_model_V6_23_2, effects = c("fixed", "random"))

# -> p-value = 0.294 which high and > 0.05 and thus data is following
# a normal distribution: we can perform anova according to this test

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_N_upt_model_V6_23_2, effects = c("fixed", "random"))

# -> p-value = 0.699 which high and > 0.05 and thus homogeneity is verified

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_N_upt_model_V6_23_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned, homogeneity is accepted visually
# which is in accordance with statistical tests: we can perform anova according to this test



Anova(AGB_N_upt_model_V6_23_2)


## Tukey test

Tukey_AGB_N_upt_V6_Rain_M_23 <- emmeans::emmeans(AGB_N_upt_model_V6_23_1, pairwise ~ Rain_Treat:Mulch, adjust = "tukey")
summary(Tukey_AGB_N_upt_V6_Rain_M_23)
Tukey_AGB_N_upt_V6_Rain_M_23 <- multcomp::cld(Tukey_AGB_N_upt_V6_Rain_M_23$emmeans, 
                                              alpha=.05,
                                              Letters=letters, reversed = TRUE)
print(Tukey_AGB_N_upt_V6_Rain_M_23)



### 2023-2024 ###
# try to fit in a model

AGB_N_upt_model_V6_24_1 <- lmer(AGB_N_uptake_kg_ha ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_V6_24)

summary(AGB_N_upt_model_V6_24_1)

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(AGB_N_upt_model_V6_24_1, effects = c("fixed", "random"))

# -> p-value = 0.077 which high and > 0.05 and thus data is following
# a normal distribution: we can perform anova according to this test

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_N_upt_model_V6_24_1, effects = c("fixed", "random"))

# -> p-value < .001 which is low and homogeneity is suspected, we check visually

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_N_upt_model_V6_24_1, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned, homogeneity is accepted visually
# which is in accordance with statistical tests: we can perform anova according to this test


Anova(AGB_N_upt_model_V6_24_1)



Tukey_AGB_N_upt_V6_M_24 <- emmeans::emmeans(AGB_N_upt_model_V6_24_1, pairwise ~ Mulch, adjust = "tukey")
summary(Tukey_AGB_N_upt_V6_M_24)
Tukey_AGB_N_upt_V6_M_24 <- multcomp::cld(Tukey_AGB_N_upt_V6_M_24$emmeans, 
                                               alpha=.05,
                                               Letters=letters, reversed = TRUE)
print(Tukey_AGB_N_upt_V6_M_24)


Tukey_AGB_N_upt_V6_Rain_M_24 <- emmeans::emmeans(AGB_N_upt_model_V6_24_1, pairwise ~ Rain_Treat:Mulch, adjust = "tukey")
summary(Tukey_AGB_N_upt_V6_Rain_M_24)
Tukey_AGB_N_upt_V6_Rain_M_24 <- multcomp::cld(Tukey_AGB_N_upt_V6_Rain_M_24$emmeans, 
                                                alpha=.05,
                                                Letters=letters, reversed = TRUE)
print(Tukey_AGB_N_upt_V6_Rain_M_24)



### V10 stage ####

# try to fit in a model that describes the experimental design

AGB_N_upt_model_V10_23_1 <- lmer(AGB_N_uptake_kg_ha ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_V10_23)

summary(AGB_N_upt_model_V10_23_1)

AGB_N_upt_model_V10_23_2 <- lmer(AGB_N_uptake_kg_ha ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate), data = AGB_data_V10_23)

compare_performance(AGB_N_upt_model_V10_23_1, AGB_N_upt_model_V10_23_2)


## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(AGB_N_upt_model_V10_23_2, effects = c("fixed", "random"))

# -> p-value = 0.978 which high and > 0.05 and thus data is following
# a normal distribution: we can perform anova according to this test

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_N_upt_model_V10_23_2, effects = c("fixed", "random"))

# -> p-value = 0.003 which is low and homogeneity is suspected, we check visually

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_N_upt_model_V10_23_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned, homogeneity is accepted visually
# which is in accordance with statistical tests: we can perform anova according to this test



Anova(AGB_N_upt_model_V10_23_2)


## No need for Tukey test

### 2023-2024 ###
# try to fit in a model

AGB_N_upt_model_V10_24_1 <- lmer(AGB_N_uptake_kg_ha ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_V10_24)

summary(AGB_N_upt_model_V10_24_1)

AGB_N_upt_model_V10_24_2 <- lmer(AGB_N_uptake_kg_ha ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate:Rain_Treat), data = AGB_data_V10_24)

summary(AGB_N_upt_model_V10_24_2)

compare_performance(AGB_N_upt_model_V10_24_1, AGB_N_upt_model_V10_24_2)

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(AGB_N_upt_model_V10_24_2, effects = c("fixed", "random"))

# -> p-value = 0.229 which high and > 0.05 and thus data is following
# a normal distribution: we can perform anova according to this test

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_N_upt_model_V10_24_2, effects = c("fixed", "random"))

# -> p-value < .001 which is low and homogeneity is suspected, we check visually

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_N_upt_model_V10_24_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned, homogeneity is accepted visually
# which is in accordance with statistical tests: we can perform anova according to this test


Anova(AGB_N_upt_model_V10_24_2)



Tukey_AGB_N_upt_V10_M_24 <- emmeans::emmeans(AGB_N_upt_model_V10_24_2, pairwise ~ Mulch, adjust = "tukey")
summary(Tukey_AGB_N_upt_V10_M_24)
Tukey_AGB_N_upt_V10_M_24 <- multcomp::cld(Tukey_AGB_N_upt_V10_M_24$emmeans, 
                                               alpha=.05,
                                               Letters=letters, reversed = TRUE)
print(Tukey_AGB_N_upt_V10_M_24)


### flowering stage ####

# try to fit in a model that describes the experimental design

AGB_N_upt_model_FLW_23_1 <- lmer(AGB_N_uptake_kg_ha ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_FLW_23)

summary(AGB_N_upt_model_FLW_23_1)

AGB_N_upt_model_FLW_23_2 <- lmer(AGB_N_uptake_kg_ha ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate), data = AGB_data_FLW_23)

compare_performance(AGB_N_upt_model_FLW_23_1, AGB_N_upt_model_FLW_23_2)


## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(AGB_N_upt_model_FLW_23_1, effects = c("fixed", "random"))

# -> p-value = 0.177 which high and > 0.05 and thus data is following
# a normal distribution: we can perform anova according to this test

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_N_upt_model_FLW_23_1, effects = c("fixed", "random"))

# -> p-value < .001 which is low and homogeneity is suspected, we check visually

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_N_upt_model_FLW_23_1, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned, homogeneity is accepted visually
# which is in accordance with statistical tests: we can perform anova according to this test



Anova(AGB_N_upt_model_FLW_23_1)


## Tukey test
Tukey_AGB_N_upt_FLW_Rain_23 <- emmeans::emmeans(AGB_N_upt_model_FLW_23_1, pairwise ~ Rain_Treat, adjust = "tukey")
summary(Tukey_AGB_N_upt_FLW_Rain_23)
Tukey_AGB_N_upt_FLW_Rain_23 <- multcomp::cld(Tukey_AGB_N_upt_FLW_Rain_23$emmeans, 
                                               alpha=.05,
                                               Letters=letters, reversed = TRUE)
print(Tukey_AGB_N_upt_FLW_Rain_23)


### 2023-2024 ###
# try to fit in a model

AGB_N_upt_model_FLW_24_1 <- lmer(AGB_N_uptake_kg_ha ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_FLW_24)

summary(AGB_N_upt_model_FLW_24_1)

AGB_N_upt_model_FLW_24_2 <- lmer(AGB_N_uptake_kg_ha ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate), data = AGB_data_FLW_24)

summary(AGB_N_upt_model_FLW_24_2)

compare_performance(AGB_N_upt_model_FLW_24_1, AGB_N_upt_model_FLW_24_2)

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(AGB_N_upt_model_FLW_24_2, effects = c("fixed", "random"))

# -> p-value = 0.424 which high and > 0.05 and thus data is following
# a normal distribution: we can perform anova according to this test

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_N_upt_model_FLW_24_2, effects = c("fixed", "random"))

# -> p-value < .001 which is low and homogeneity is suspected, we check visually

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_N_upt_model_FLW_24_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned, homogeneity is accepted visually
# which is in accordance with statistical tests: we can perform anova according to this test


Anova(AGB_N_upt_model_FLW_24_2)



Tukey_AGB_N_upt_FLW_N_24 <- emmeans::emmeans(AGB_N_upt_model_FLW_24_2, pairwise ~ Nitrogen, adjust = "tukey")
summary(Tukey_AGB_N_upt_FLW_N_24)
Tukey_AGB_N_upt_FLW_N_24 <- multcomp::cld(Tukey_AGB_N_upt_FLW_N_24$emmeans, 
                                                 alpha=.05,
                                                 Letters=letters, reversed = TRUE)
print(Tukey_AGB_N_upt_FLW_N_24)


Tukey_AGB_N_upt_FLW_M_24 <- emmeans::emmeans(AGB_N_upt_model_FLW_24_2, pairwise ~ Mulch, adjust = "tukey")
summary(Tukey_AGB_N_upt_FLW_M_24)
Tukey_AGB_N_upt_FLW_M_24 <- multcomp::cld(Tukey_AGB_N_upt_FLW_M_24$emmeans, 
                                                     alpha=.05,
                                                     Letters=letters, reversed = TRUE)
print(Tukey_AGB_N_upt_FLW_M_24)


Tukey_AGB_N_upt_FLW_Rain_N_24 <- emmeans::emmeans(AGB_N_upt_model_FLW_24_2, pairwise ~ Rain_Treat:Nitrogen, adjust = "tukey")
summary(Tukey_AGB_N_upt_FLW_Rain_N_24)
Tukey_AGB_N_upt_FLW_Rain_N_24 <- multcomp::cld(Tukey_AGB_N_upt_FLW_Rain_N_24$emmeans, 
                                                 alpha=.05,
                                                 Letters=letters, reversed = TRUE)
print(Tukey_AGB_N_upt_FLW_Rain_N_24)



#### N concentration for total AGB variable ####

### 2023-2024 ###
# try to fit in a model

AGB_N_conc_model_FLW_24_1 <- lmer(Tot_N_AGB ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_FLW_24)

summary(AGB_N_conc_model_FLW_24_1)

AGB_N_conc_model_FLW_24_2 <- lmer(Tot_N_AGB ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate), data = AGB_data_FLW_24)

summary(AGB_N_conc_model_FLW_24_2)

compare_performance(AGB_N_conc_model_FLW_24_1, AGB_N_conc_model_FLW_24_2)

# Almost similar performances, we keep AGB_N_conc_model_FLW_24_2 as there is some variance from Replicate:Rain_Treat even if compared to residual both random effect do not represent much

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(AGB_N_conc_model_FLW_24_2, effects = c("fixed", "random"))

# -> p-value = 0.179 which is good

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_N_conc_model_FLW_24_2, effects = c("fixed", "random"))

# -> p-value 0.980 which is high and > 0.05 and homogeneity is verified

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_N_conc_model_FLW_24_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned (except an outlier point but we accept the normality visually), homogeneity is accepted visually
# which is in accordance with statistical tests: we can perform anova according to this test


Anova(AGB_N_conc_model_FLW_24_2)


Tukey_AGB_N_conc_FLW_Rain_N_24 <- emmeans::emmeans(AGB_N_conc_model_FLW_24_2, pairwise ~ Rain_Treat:Nitrogen, adjust = "tukey")
summary(Tukey_AGB_N_conc_FLW_Rain_N_24)
Tukey_AGB_N_conc_FLW_Rain_N_24 <- multcomp::cld(Tukey_AGB_N_conc_FLW_Rain_N_24$emmeans, 
                                                alpha=.05,
                                                Letters=letters, reversed = TRUE)
print(Tukey_AGB_N_conc_FLW_Rain_N_24)





### Harvest stage ####

# try to fit in a model that describes the experimental design

AGB_N_upt_model_HVST_23_1 <- lmer(AGB_N_uptake_kg_ha ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_HVST_23)

summary(AGB_N_upt_model_HVST_23_1)

AGB_N_upt_model_HVST_23_2 <- lmer(AGB_N_uptake_kg_ha ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate), data = AGB_data_HVST_23)

compare_performance(AGB_N_upt_model_HVST_23_1, AGB_N_upt_model_HVST_23_2)


## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(AGB_N_upt_model_HVST_23_2, effects = c("fixed", "random"))

# -> p-value = 0.917 which high and > 0.05 and thus data is following
# a normal distribution: we can perform anova according to this test

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_N_upt_model_HVST_23_2, effects = c("fixed", "random"))

# -> p-value < .001 which is low and homogeneity is suspected, we check visually

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_N_upt_model_HVST_23_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned, homogeneity is accepted visually
# which is in accordance with statistical tests: we can perform anova according to this test



Anova(AGB_N_upt_model_HVST_23_2)


## Tukey test
Tukey_AGB_N_upt_HVST_N_23 <- emmeans::emmeans(AGB_N_upt_model_HVST_23_2, pairwise ~ Nitrogen, adjust = "tukey")
summary(Tukey_AGB_N_upt_HVST_N_23)
Tukey_AGB_N_upt_HVST_N_23 <- multcomp::cld(Tukey_AGB_N_upt_HVST_N_23$emmeans, 
                                                alpha=.05,
                                                Letters=letters, reversed = TRUE)
print(Tukey_AGB_N_upt_HVST_N_23)


### 2023-2024 ###
# try to fit in a model

AGB_N_upt_model_HVST_24_1 <- lmer(AGB_N_uptake_kg_ha ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_HVST_24)

summary(AGB_N_upt_model_HVST_24_1)

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(AGB_N_upt_model_HVST_24_1, effects = c("fixed", "random"))

# -> p-value = 0.659 which high and > 0.05 and thus data is following
# a normal distribution: we can perform anova according to this test

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(AGB_N_upt_model_HVST_24_1, effects = c("fixed", "random"))

# -> p-value < .001 which is low and homogeneity is suspected, we check visually

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(AGB_N_upt_model_HVST_24_1, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned, homogeneity is accepted visually
# which is in accordance with statistical tests: we can perform anova according to this test


Anova(AGB_N_upt_model_HVST_24_1)



Tukey_AGB_N_upt_HVST_M_24 <- emmeans::emmeans(AGB_N_upt_model_HVST_24_1, pairwise ~ Mulch, adjust = "tukey")
summary(Tukey_AGB_N_upt_HVST_M_24)
Tukey_AGB_N_upt_HVST_M_24 <- multcomp::cld(Tukey_AGB_N_upt_HVST_M_24$emmeans, 
                                                      alpha=.05,
                                                      Letters=letters, reversed = TRUE)
print(Tukey_AGB_N_upt_HVST_M_24)

Tukey_AGB_N_upt_HVST_N_24 <- emmeans::emmeans(AGB_N_upt_model_HVST_24_1, pairwise ~ Nitrogen, adjust = "tukey")
summary(Tukey_AGB_N_upt_HVST_N_24)
Tukey_AGB_N_upt_HVST_N_24 <- multcomp::cld(Tukey_AGB_N_upt_HVST_N_24$emmeans, 
                                                  alpha=.05,
                                                  Letters=letters, reversed = TRUE)
print(Tukey_AGB_N_upt_HVST_N_24)


Tukey_AGB_N_upt_HVST_Rain_M_24 <- emmeans::emmeans(AGB_N_upt_model_HVST_24_1, pairwise ~ Rain_Treat:Mulch, adjust = "tukey")
summary(Tukey_AGB_N_upt_HVST_Rain_M_24)
Tukey_AGB_N_upt_HVST_Rain_M_24 <- multcomp::cld(Tukey_AGB_N_upt_HVST_Rain_M_24$emmeans, 
                                                  alpha=.05,
                                                  Letters=letters, reversed = TRUE)
print(Tukey_AGB_N_upt_HVST_Rain_M_24)

Tukey_AGB_N_upt_HVST_Rain_N_24 <- emmeans::emmeans(AGB_N_upt_model_HVST_24_1, pairwise ~ Rain_Treat:Nitrogen, adjust = "tukey")
summary(Tukey_AGB_N_upt_HVST_Rain_N_24)
Tukey_AGB_N_upt_HVST_Rain_N_24 <- multcomp::cld(Tukey_AGB_N_upt_HVST_Rain_N_24$emmeans, 
                                                  alpha=.05,
                                                  Letters=letters, reversed = TRUE)
print(Tukey_AGB_N_upt_HVST_Rain_N_24)



### Nitrogen harvest index ####

NHI_model_23_1 <- lmer(N_harvest_index ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_HVST_23)

summary(NHI_model_23_1)

NHI_model_23_2 <- lmer(N_harvest_index ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate), data = AGB_data_HVST_23)

compare_performance(NHI_model_23_1, NHI_model_23_2)

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(NHI_model_23_2, effects = c("fixed", "random"))

# -> p-value = 0.819 which high and > 0.05 and thus data is following
# a normal distribution: we can perform anova according to this test

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(NHI_model_23_2, effects = c("fixed", "random"))

# -> p-value < .001 which is low and homogeneity is suspected, we check visually

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(NHI_model_23_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned, homogeneity is accepted visually
# which is in accordance with statistical tests: we can perform anova according to this test


Anova(NHI_model_23_2)


## Tukey test
Tukey_NHI_Rain_23 <- emmeans::emmeans(NHI_model_23_2, pairwise ~ Rain_Treat, adjust = "tukey")
summary(Tukey_NHI_Rain_23)
Tukey_NHI_Rain_23 <- multcomp::cld(Tukey_NHI_Rain_23$emmeans, 
                                     alpha=.05,
                                     Letters=letters, reversed = TRUE)
print(Tukey_NHI_Rain_23)


### 2023-2024 ###
# try to fit in a model
NHI_model_24_1 <- lmer(log(N_harvest_index) ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_HVST_24)

summary(NHI_model_24_1)

NHI_model_24_2 <- lmer(log(N_harvest_index) ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate:Rain_Treat) , data = AGB_data_HVST_24)

summary(NHI_model_24_2)

compare_performance(NHI_model_24_1, NHI_model_24_2)

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(NHI_model_24_2, effects = c("fixed", "random"))

# -> p-value = 0.140 which is on the limit of significance, we check visually

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(NHI_model_24_2, effects = c("fixed", "random"))

# -> p-value p = 0.686 homogeneity is verified

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(NHI_model_24_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned after a log10 transformation, homogeneity is accepted visually
# which is in accordance with statistical tests: we can perform anova according to this test


Anova(NHI_model_24_2)



Tukey_NHI_N_24 <- emmeans::emmeans(NHI_model_24_2, pairwise ~ Nitrogen, adjust = "tukey")
summary(Tukey_NHI_N_24)
Tukey_NHI_N_24 <- multcomp::cld(Tukey_NHI_N_24$emmeans, 
                                     alpha=.05,
                                     Letters=letters, reversed = TRUE)
print(Tukey_NHI_N_24)





#### Grain yield and yield components stats ####

## data of 2022-2023 season
GY_data_23 <- GY_data %>%
  filter(Crop_season == "2022-2023")



## data of 2022-2023 season
GY_data_24 <- GY_data %>%
  filter(Crop_season == "2023-2024")


### Grain yield ####

### 2022-2023 season ###

### ANOVA ###

## check ANOVA assumptions on the residuals of a fitted model

## assign block replicate to fixed effect and add an error from Replicate*Rain_Treat interaction (that refers to the error of the main plot level): 
GY_model_23_1 <- lmer(G_yield_t_ha_moist_standard ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = GY_data_23)

summary(GY_model_23_1)

## assign block replicate to random effect (i.e. error from block replicate) and add an error from Replicate*Rain_Treat interaction (that refers to the error of the main plot level): 
GY_model_23_2 <- lmer(G_yield_t_ha_moist_standard ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate), data = GY_data_23)

summary(GY_model_23_2)


## compare models performance to confirm the choice
compare_performance(GY_model_23_1, GY_model_23_2)

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(GY_model_23_2, effects = c("fixed", "random"))

# -> p-value = 0.780 which high and > 0.05 and thus normality is verified

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(GY_model_23_2, effects = c("fixed", "random"))

# -> p-value = 0.829 which is high and > 0.05, homogeneity verified

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(GY_model_23_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned 
# which is in accordance with statistical tests: we can perform anova according to this test


## Anova of car package (II way anova by default)
Anova(GY_model_23_2)

## the best function to use is Anova from car (see references)

## Tukey's test using emmeans function from car on all models

Tukey_GY_Rain_23 <- emmeans::emmeans(GY_model_23_2, pairwise ~ Rain_Treat , adjust = "tukey")
summary(Tukey_GY_Rain_23)
Tukey_GY_Rain_23 <- multcomp::cld(Tukey_GY_Rain_23$emmeans, 
                                    alpha=.05,
                                    Letters = letters, reversed = TRUE)
print(Tukey_GY_Rain_23)



## check ANOVA assumptions on the residuals of a fitted model
## => we follow the procedure described with first season 
GY_model_24_1 <- lmer(G_yield_t_ha_moist_standard ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = GY_data_24)

summary(GY_model_24_1)

GY_model_24_2 <- lmer(G_yield_t_ha_moist_standard ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate), data = GY_data_24)

summary(GY_model_24_2)

compare_performance(GY_model_24_1, GY_model_24_2)

## there is some variance from both random effects, we can keep them both 
## we keep model GY_model_24

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(GY_model_24_1, effects = c("fixed", "random"))

# -> p-value = 0.182 which high and > 0.05 and thus normality is verified

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(GY_model_24_1, effects = c("fixed", "random"))

# -> p-value < .001 which low and < 0.05, homogeneity suspected and we check visually

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(GY_model_24_1, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned (homogeneity accepted visually)
# which is in accordance with statistical tests: we can perform anova according to this test

## Anova of car package (II way anova by default)
Anova(GY_model_24_1)


## Tukey's test using emmeans function from car on all models
Tukey_GY_Rain_24 <- emmeans::emmeans(GY_model_24_1, pairwise ~ Rain_Treat, adjust = "tukey")
summary(Tukey_GY_Rain_24)
Tukey_GY_Rain_24 <- multcomp::cld(Tukey_GY_Rain_24$emmeans, 
                                         alpha=.05,
                                         Letters = letters, reversed = TRUE)
print(Tukey_GY_Rain_24)


Tukey_GY_M_24 <- emmeans::emmeans(GY_model_24_1, pairwise ~ Mulch, adjust = "tukey")
summary(Tukey_GY_M_24)
Tukey_GY_M_24 <- multcomp::cld(Tukey_GY_M_24$emmeans, 
                                          alpha=.05,
                                          Letters = letters, reversed = TRUE)
print(Tukey_GY_M_24)


Tukey_GY_N_24 <- emmeans::emmeans(GY_model_24_1, pairwise ~ Nitrogen, adjust = "tukey")
summary(Tukey_GY_N_24)
Tukey_GY_N_24 <- multcomp::cld(Tukey_GY_N_24$emmeans, 
                                      alpha=.05,
                                      Letters = letters, reversed = TRUE)
print(Tukey_GY_N_24)


Tukey_GY_Rain_M_24 <- emmeans::emmeans(GY_model_24_1, pairwise ~ Rain_Treat:Mulch, adjust = "tukey")
summary(Tukey_GY_Rain_M_24)
Tukey_GY_Rain_M_24<- multcomp::cld(Tukey_GY_Rain_M_24$emmeans, 
                                         alpha=.05,
                                         Letters = letters, reversed = TRUE)
print(Tukey_GY_Rain_M_24)


Tukey_GY_Rain_N_24 <- emmeans::emmeans(GY_model_24_1, pairwise ~ Rain_Treat:Nitrogen, adjust = "tukey")
summary(Tukey_GY_Rain_N_24)
Tukey_GY_Rain_N_24 <- multcomp::cld(Tukey_GY_Rain_N_24$emmeans, 
                                           alpha=.05,
                                           Letters = letters, reversed = TRUE)
print(Tukey_GY_Rain_N_24)



### Number of ears per plant ####

### 2022-2023 season ###

## check ANOVA assumptions on the residuals of a fitted model

## assign block replicate to random effect (i.e. error from block replicate) only: 
EN_model_23_1 <- lmer(Numb_ears_plant ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = GY_data_23)

summary(EN_model_23_1)

## we can leave only block random effect for simplification as variances are 0
EN_model_23_2 <- lmer(Numb_ears_plant ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate), data = GY_data_23)

## compare model performances 
compare_performance(EN_model_23_1, EN_model_23_2) #best model: lowest AIC & lowest BIC

# -> similar RMSE and R2 conditional, we keep lowest AIC and BIC model with EN_model_23_2

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(EN_model_23_2, effects = c("fixed", "random"))

# -> p-value = 0.610 which high and > 0.05 and thus normality is verified

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(EN_model_23_2, effects = c("fixed", "random"))

# -> p-value < .001 which low and < 0.05, homogeneity suspected and we check visually

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(EN_model_23_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned (homogeneity accepted visually)
# which is in accordance with statistical tests: we can perform anova according to this test


## Anova of car package
Anova(EN_model_23_2)

## Tukey's test using emmeans function from car on all models

Tukey_EN_Rain_23 <- emmeans::emmeans(EN_model_23_2, pairwise ~ Rain_Treat, adjust = "tukey")
summary(Tukey_EN_Rain_23)
Tukey_EN_Rain_23 <- multcomp::cld(Tukey_EN_Rain_23$emmeans, 
                                    alpha=.05,
                                    Letters = letters, reversed = TRUE)
print(Tukey_EN_Rain_23)



### 2023-2024 season ###

## check ANOVA assumptions on the residuals of a fitted model

## assign block replicate to random effect (i.e. error from block replicate) only: 
EN_model_24_1 <- lmer(Numb_ears_plant ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = GY_data_24)

summary(EN_model_24_1)

## we can leave only  (1 | Replicate:Rain_Treat) because block randome effect is null (variance = 0)
EN_model_24_2 <- lmer(Numb_ears_plant ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate:Rain_Treat), data = GY_data_24)

## compare model performances 
compare_performance(EN_model_24_1, EN_model_24_2) #best model: lowest AIC & lowest BIC

# -> EN_model_24_2 presents the lowest AIC and BIC, similar RMSE and a higher R2 conditional

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(EN_model_24_2, effects = c("fixed", "random"))

# -> p-value = 0.599 which high and > 0.05 and thus normality is verified

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(EN_model_24_2, effects = c("fixed", "random"))

# -> p-value < .001 which low and < 0.05, homogeneity suspected and we check visually

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(EN_model_24_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned (homogeneity accepted visually)
# which is in accordance with statistical tests: we can perform anova according to this test


### Test anova on all models

## Anova of car package 

Anova(EN_model_24_2)

## Tukey's test using emmeans function from car on all models

Tukey_EN_M_24 <- emmeans::emmeans(EN_model_24_2, pairwise ~ Mulch, adjust = "tukey")
summary(Tukey_EN_M_24)
Tukey_EN_M_24 <- multcomp::cld(Tukey_EN_M_24$emmeans, 
                                          alpha=.05,
                                          Letters = letters, reversed = TRUE)
print(Tukey_EN_M_24)


Tukey_EN_N_24 <- emmeans::emmeans(EN_model_24_2, pairwise ~ Nitrogen, adjust = "tukey")
summary(Tukey_EN_N_24)
Tukey_EN_N_24 <- multcomp::cld(Tukey_EN_N_24$emmeans, 
                                      alpha=.05,
                                      Letters = letters, reversed = TRUE)
print(Tukey_EN_N_24)




### Number of grains per ear ####

### 2022-2023 season ###

## check ANOVA assumptions on the residuals of a fitted model

## assign block replicate to random effect (i.e. error from block replicate) only: 
GN_model_23_1 <- lmer(Numb_grains_ear ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = GY_data_23)

summary(GN_model_23_1)

## we can leave only block replicate random effect
GN_model_23_2 <- lmer(Numb_grains_ear ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate), data = GY_data_23)

## compare model performances 
compare_performance(GN_model_23_1, GN_model_23_2) #best model: lowest AIC & lowest BIC

# -> GN_model_23_2 presents the lowest AIC and BIC, similar R2, RMSE 

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(GN_model_23_2, effects = c("fixed", "random"))

# -> p-value = 0.943 which high and > 0.05 and thus normality is verified

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(GN_model_23_2, effects = c("fixed", "random"))

# -> p-value < .001 which low and < 0.05, homogeneity suspected and we check visually

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(GN_model_23_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned (homogeneity accepted visually)
# which is in accordance with statistical tests: we can perform anova according to this test


## Anova of car package (II way anova by default)
Anova(GN_model_23_2)

## No factor effect is significant, no need for Tukey's test 


### 2023-2024 season ###

## assign block replicate to random effect (i.e. error from block replicate) only: 
GN_model_24_1 <- lmer(Numb_grains_ear ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = GY_data_24)

summary(GN_model_24_1)

## we can keep GN_model_24_1 because there is variance from both random effects

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(GN_model_24_1, effects = c("fixed", "random"))

# -> p-value = 0.865 which high and > 0.05 and thus normality is verified

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(GN_model_24_1, effects = c("fixed", "random"))

# -> p-value < .001 which low and < 0.05, homogeneity suspected and we check visually

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(GN_model_24_1, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned (homogeneity accepted visually)
# which is in accordance with statistical tests: we can perform anova according to this test

## Anova of car package 
Anova(GN_model_24_1)


## Tukey's test using emmeans function from car

Tukey_GN_Rain_24 <- emmeans::emmeans(GN_model_24_1, pairwise ~ Rain_Treat, adjust = "tukey")
summary(Tukey_GN_Rain_24)
Tukey_GN_Rain_24 <- multcomp::cld(Tukey_GN_Rain_24$emmeans, 
                                         alpha=.05,
                                         Letters = letters, reversed = TRUE)
print(Tukey_GN_Rain_24)


Tukey_GN_M_24 <- emmeans::emmeans(GN_model_24_1, pairwise ~ Mulch, adjust = "tukey")
summary(Tukey_GN_M_24)
Tukey_GN_M_24 <- multcomp::cld(Tukey_GN_M_24$emmeans, 
                                          alpha=.05,
                                          Letters = letters, reversed = TRUE)
print(Tukey_GN_M_24)


Tukey_GN_N_24 <- emmeans::emmeans(GN_model_24_1, pairwise ~ Nitrogen, adjust = "tukey")
summary(Tukey_GN_N_24)
Tukey_GN_N_24 <- multcomp::cld(Tukey_GN_N_24$emmeans, 
                                      alpha=.05,
                                      Letters = letters, reversed = TRUE)
print(Tukey_GN_N_24)




### Weight of 1000 grains at standard commercial moisture ####

### 2022-2023 season ###

## check ANOVA assumptions on the residuals of a fitted model

## assign block replicate to random effect (i.e. error from block replicate) only: 
GW_model_23_1 <- lmer(DW_1000grains_g_moist_standard ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = GY_data_23)

summary(GW_model_23_1)

## we keep only block random effect
GW_model_23_2 <- lmer(DW_1000grains_g_moist_standard ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate), data = GY_data_23)

## compare model performances 
compare_performance(GW_model_23_1, GW_model_23_2) #best model: lowest AIC & lowest BIC

# -> GW_model_23_2 presents the lowest AIC and BIC, similar RMSE and R2

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(GW_model_23_2, effects = c("fixed", "random"))

# -> p-value = 0.578 which high and > 0.05 and thus normality is verified

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(GW_model_23_2, effects = c("fixed", "random"))

# -> p-value < .001 which low and < 0.05, homogeneity suspected and we check visually

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(GW_model_23_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned (homogeneity accepted visually)
# which is in accordance with statistical tests: we can perform anova according to this test


## Anova of car package
Anova(GW_model_23_2)

## No factor effect is significant, no need for Tukey's test but we test on the interaction Rain * Mulch
## Tukey's test using emmeans function from car on all models

Tukey_GW_Rain_23 <- emmeans::emmeans(GW_model_23_2, pairwise ~ Rain_Treat, adjust = "tukey")
summary(Tukey_GW_Rain_23)
Tukey_GW_Rain_23 <- multcomp::cld(Tukey_GW_Rain_23$emmeans, 
                                    alpha=.05,
                                    Letters = letters, reversed = TRUE)
print(Tukey_GW_Rain_23)


### 2023-2024 season ###

## check ANOVA assumptions on the residuals of a fitted model

## assign block replicate to random effect (i.e. error from block replicate) only: 
GW_model_24_1 <- lmer(DW_1000grains_g_moist_standard ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = GY_data_24)

summary(GW_model_24_1)

GW_model_24_2 <- lmer(DW_1000grains_g_moist_standard ~  Rain_Treat * Mulch * Nitrogen + (1 | Replicate), data = GY_data_24)

summary(GW_model_24_2)

## compare model performances 
compare_performance(GW_model_24_1, GW_model_24_2) #best model: lowest AIC & lowest BIC

# -> Given that variance from Replicate:Rain_Treat is very low compared to Replicate,
# GW_model_24_2 presents the lowest AIC, with similar R2 and RMSE. We keep GW_model_24_2

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(GW_model_24_2, effects = c("fixed", "random"))

# -> p-value = 0.709 which high and > 0.05 and thus normality is verified

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(GW_model_24_2, effects = c("fixed", "random"))

# -> p-value < .001 which low and < 0.05, homogeneity suspected and we check visually

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(GW_model_24_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned (homogeneity accepted visually)
# which is in accordance with statistical tests: we can perform anova according to this test

## Anova of car package 
Anova(GW_model_24_2)

## No factor effect is significant

Tukey_GW_M_24 <- emmeans::emmeans(GW_model_24_2, pairwise ~ Rain_Treat * Mulch, adjust = "tukey")
summary(Tukey_GW_M_24)
Tukey_GW_M_24 <- multcomp::cld(Tukey_GW_M_24$emmeans, 
                                    alpha=.05,
                                    Letters = letters, reversed = TRUE)
print(Tukey_GW_M_24)



### Harvest index ####

### 2022-2023 ###

## try to fit in a model that represent the best the experimental design
# model with two random effect: block replicate (random variation between blocks) and Rain_Treat in block replicate (randomization of rain factor within each block replicate)
HI_HVST_model_23_1 <- lmer(Harvest_index ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_HVST_23)

## Check variance from random effect
summary(HI_HVST_model_23_1)

##### CONCLUSION: The model to keep is HI_HVST_model_23_1

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## Shapiro test from performance package
check_normality(HI_HVST_model_23_1, effects = c("fixed", "random"))

# -> p-value = 0.728 which high and > 0.05 and thus data is following
# a normal distribution: we can perform anova according to this test

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(HI_HVST_model_23_1, effects = c("fixed", "random"))

# -> p-value < .001 which is very low and < 0.05, homogeneity suspected, we check on the plots

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(HI_HVST_model_23_1, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned (some points seem to be far but still correct)
# which is in accordance with statistical tests: we can perform anova according to this test

## Perform ANOVA

Anova(HI_HVST_model_23_1)

## Tukey's test
Tukey_HI_HVST_Rain_23 <- emmeans::emmeans(HI_HVST_model_23_1, pairwise ~ Rain_Treat, adjust = "tukey")
summary(Tukey_HI_HVST_Rain_23)
Tukey_HI_HVST_Rain_23 <- multcomp::cld(Tukey_HI_HVST_Rain_23$emmeans, 
                                                    alpha=.05,
                                                    Letters = letters, reversed = TRUE)
print(Tukey_HI_HVST_Rain_23)




### 2023-2024 ###

## try to fit in a model that represent the best the experimental design
# model with two random effect: block replicate (random variation between blocks) and Rain_Treat in block replicate (randomization of rain factor within each block replicate)
HI_HVST_model_24_1 <- lmer(Harvest_index ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate) + (1 | Replicate:Rain_Treat), data = AGB_data_HVST_24)

## Check variance from random effect
summary(HI_HVST_model_24_1)

## we leave only (1 | Replicate:Rain_Treat) because there is some variance explained by it
HI_HVST_model_24_2 <- lmer(Harvest_index ~ Rain_Treat * Mulch * Nitrogen + (1 | Replicate:Rain_Treat), data = AGB_data_HVST_24)

## Check variance from random effect
summary(HI_HVST_model_24_2)

## Compare models performance to confirm the choice
compare_performance(HI_HVST_model_24_1, HI_HVST_model_24_2)

## Logically, we consider AIC and BIC with R2 conditional (accounting for both random and fixed effects)
## So we take the lowest AIC and BIC for model Complexity, highest R2 conditional and RMSE is similar

##### CONCLUSION: The model to keep is HI_HVST_model_24_2

## test normality and homogeneity on the residuals of this model

## a. using statistical tests

## a. using statistical tests

## Shapiro test from performance package
check_normality(HI_HVST_model_24_2, effects = c("fixed", "random"))

# -> p-value = 0.329 which high and > 0.05 and thus data is following
# a normal distribution: we can perform anova according to this test

## (non-)constant error or Breusch-Pagan test from performance package

check_heteroskedasticity(HI_HVST_model_24_2, effects = c("fixed", "random"))

# -> p-value < .001 which is very low and < 0.05, homogeneity suspected, we check on the plots

## b. using graphics of normality, homogeneity, QQ plot and fitted vs residuals

check_model(HI_HVST_model_24_2, check = c("homogeneity", "normality", "qq", "linearity"), detrend = F)

# -> the model residuals are following a normal distribution and are aligned (some points seem to be far but still correct)
# which is in accordance with statistical tests: we can perform anova according to this test

## Perform ANOVA

Anova(HI_HVST_model_24_2)


Tukey_HI_HVST_Rain_24 <- emmeans::emmeans(HI_HVST_model_24_2, pairwise ~ Rain_Treat, adjust = "tukey")
summary(Tukey_HI_HVST_Rain_24)
Tukey_HI_HVST_Rain_24 <- multcomp::cld(Tukey_HI_HVST_Rain_24$emmeans, 
                                                         alpha=.05,
                                                         Letters = letters, reversed = TRUE)
print(Tukey_HI_HVST_Rain_24)


Tukey_HI_HVST_N_24 <- emmeans::emmeans(HI_HVST_model_24_2, pairwise ~ Nitrogen, adjust = "tukey")
summary(Tukey_HI_HVST_N_24)
Tukey_HI_HVST_N_24 <- multcomp::cld(Tukey_HI_HVST_N_24$emmeans, 
                                                      alpha=.05,
                                                      Letters = letters, reversed = TRUE)
print(Tukey_HI_HVST_N_24)



Tukey_HI_HVST_Rain_N_24 <- emmeans::emmeans(HI_HVST_model_24_2, pairwise ~ Rain_Treat:Nitrogen, adjust = "tukey")
summary(Tukey_HI_HVST_Rain_N_24)
Tukey_HI_HVST_Rain_N_24 <- multcomp::cld(Tukey_HI_HVST_Rain_N_24$emmeans, 
                                                         alpha=.05,
                                                         Letters = letters, reversed = TRUE)
print(Tukey_HI_HVST_Rain_N_24)


#### Store all results in an excel file ####

### detect and store all anova tables
script_lines <- readLines("ANOVA_Tukey_stat_analysis.R", n = 1797)

# Detect Anova() calls and extract model names
anova_model_names <- script_lines %>%
  grep("Anova\\(", ., value = TRUE) %>%
  gsub(".*Anova\\(([^\\)]+)\\).*", "\\1", .) %>%
  trimws() %>%
  unique()

# Create cleaned sheet names without trailing _1 or _2
sheet_names <- gsub("_[0-9]+$", "", anova_model_names)

# Run Anova and store results
anova_tables <- setNames(
  lapply(anova_model_names, function(model_name) {
    model_obj <- get(model_name)
    Anova(model_obj) %>%
      as.data.frame() %>%
      rownames_to_column(var = "Effect")
  }),
  paste0(sheet_names, "_anova")  # Cleaned sheet names
)

# store ANOVA tables
anova_wb <- createWorkbook()

for (name in names(anova_tables)) {
  addWorksheet(anova_wb, sheetName = name)
  writeData(anova_wb, sheet = name, x = anova_tables[[name]])
}

saveWorkbook(anova_wb, file = file.path(tables_path, "ANOVA_tables.xlsx"), overwrite = TRUE)


### Detect all Tukey label objects
tukey_names <- ls(envir = .GlobalEnv) %>%
  keep(~ grepl("^Tukey_.*$", .x))

tukey_colnames <- sapply(tukey_names, function(x) colnames(as.data.frame(get(x))))
tukey_colnames

# Create a named list of Tukey label tables as data frames
tukey_tables <- setNames(
  lapply(tukey_names, function(obj_name) {
    get(obj_name) %>%
      as.data.frame() %>%
      rename(Group_label = .group)
  }),
  paste0(tukey_names)
)

# store tukey tables
tukey_wb <- createWorkbook()

for (name in names(tukey_tables)) {
  addWorksheet(tukey_wb, sheetName = name)
  writeData(tukey_wb, sheet = name, x = tukey_tables[[name]])
}

saveWorkbook(tukey_wb, file = file.path(tables_path, "Tukey_groups.xlsx"), overwrite = TRUE)




















