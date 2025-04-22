# TITLE:          Stats for CTI changes over time btwn treatments
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate and ecosystem response data for all experiments 
# DATA OUTPUT:    Combined figures
# PROJECT:        EcoAcc
# DATE:           Feb 2025


### Load packages
library(tidyverse)
library(lmerTest)
library(car)
library(patchwork)
library(MuMIn)



### Set path to turbo to get teracon data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TeRaCON/"
setwd(path_data)
# Load in data
CTI_sens_teracon <- read.csv(" CTI_sens_teracon.csv")
CTI_teracon <- read.csv(" CTI_teracon.csv")
CTI_CPI_teracon <- read.csv(" CTI_CPI_teracon.csv")
NPP_overall_teracon <- read.csv(" eco_response_overall_teracon.csv")

### Set path to jrgce data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/JRGCE/"
setwd(path_data)
# Load in data
CTI_sens_jrgce <- read.csv(" CTI_sens_jrgce.csv")
CTI_jrgce <- read.csv(" CTI_jrgce.csv")
CTI_CPI_jrgce <- read.csv(" CTI_CPI_jrgce.csv")
NPP_overall_jrgce <- read.csv(" eco_response_overall_jrgce.csv")

### Set path to phace data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/PHACE/"
setwd(path_data)
# Load in data
CTI_sens_phace <- read.csv(" CTI_sens_phace.csv")
CTI_phace <- read.csv(" CTI_phace.csv")
CTI_CPI_phace <- read.csv(" CTI_CPI_phace.csv")
NPP_overall_phace <- read.csv(" eco_response_overall_phace.csv")

### Set path to b4warmed data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/B4Warmed/"
setwd(path_data)
# Load in data
CTI_sens_b4_cfc <- read.csv(" CTI_sens_b4warmed_cfc.csv")
CTI_b4_cfc <- read.csv(" CTI_b4warmed_cfc.csv")
CTI_b4_cfc <- CTI_b4_cfc %>%
  filter(!(year == 2021))
CTI_CPI_b4_cfc <- read.csv(" CTI_CPI_b4warmed_cfc.csv")
NPP_overall_b4_cfc <- read.csv(" eco_response_overall_b4warmed_cfc.csv")
NPP_overall_b4_cfc <- NPP_overall_b4_cfc %>%
  filter(!(year == 2021))

CTI_sens_b4_hwrc <- read.csv(" CTI_sens_b4warmed_hwrc.csv")
CTI_b4_hwrc <- read.csv(" CTI_b4warmed_hwrc.csv")
CTI_b4_hwrc <- CTI_b4_hwrc %>%
  filter(!(year == 2021))
CTI_CPI_b4_hwrc <- read.csv(" CTI_CPI_b4warmed_hwrc.csv")
NPP_overall_b4_hwrc <- read.csv(" eco_response_overall_b4warmed_hwrc.csv")
NPP_overall_b4_hwrc <- NPP_overall_b4_hwrc %>%
  filter(!(year == 2021))

### Set path to oklahoma data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/OK/"
setwd(path_data)
# Load in data
CTI_sens_ok <- read.csv(" CTI_sens_ok.csv")
CTI_ok <- read.csv(" CTI_ok.csv")
CTI_CPI_ok <- read.csv(" CTI_CPI_ok.csv")
NPP_overall_ok <- read.csv(" eco_response_overall_ok.csv")

### Set path to Yu data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/Yu_2025_Nature/"
setwd(path_data)
# Load in data
CPI_sens_knz <- read.csv(" CPI_sens_knz.csv")
CPI_knz <- read.csv(" CPI_knz.csv")
NPP_overall_knz <- read.csv(" knz_biomass.csv")

CPI_sens_hys <- read.csv(" CPI_sens_hys.csv")
CPI_hys <- read.csv(" CPI_hys.csv")
NPP_overall_hys <- read.csv(" hys_biomass.csv")

CPI_sens_sgs <- read.csv(" CPI_sens_sgs.csv")
CPI_sgs <- read.csv(" CPI_sgs.csv")
NPP_overall_sgs <- read.csv(" sgs_biomass.csv")

CPI_sens_chy <- read.csv(" CPI_sens_chy.csv")
CPI_chy <- read.csv(" CPI_chy.csv")
NPP_overall_chy <- read.csv(" chy_biomass.csv")



##### Models testing disequilibrium effects on biomass #####

### Teracon
# Merging disequilibrium data with biomass data
MAT_dis_teracon <- CTI_teracon %>%
  dplyr::select(year,temp_treatment,MAT,disequilib,CTI) %>%
  distinct()
NPP_MAT_dis_teracon <- left_join(NPP_overall_teracon, MAT_dis_teracon,by=c("year","temp_treatment"))
NPP_MAT_dis_teracon <- NPP_MAT_dis_teracon %>%
  filter(!is.na(mean_ab_bio)) %>%
  group_by(year,temp_treatment) %>%
  mutate(CTI = mean(CTI), MAT = mean(MAT), disequilib = mean(disequilib)) %>%
  distinct()

## Using MAT model residuals to test disequilibrium effect
# Run model for biomass and MAT
mod.tera_dis <- lm(mean_ab_bio ~ MAT, data = NPP_MAT_dis_teracon)
summary(mod.tera_dis)
# Extract residuals from the MAT model
NPP_MAT_dis_teracon$residuals_biomass <- resid(mod.tera_dis)

# Fit a second model using residuals as the response variable and disequilibrium as the predictor
diseq_model.tera <- lm(residuals_biomass ~ disequilib * temp_treatment, data=NPP_MAT_dis_teracon)
# Summary of the model to see the effect of disequilibrium
summary(diseq_model.tera)
anova(diseq_model.tera)
# A VIF value of 1 means no correlation between this predictor and the other predictors (perfectly uncorrelated).
# VIF values between 1 and 5 indicate a moderate correlation, which is typically acceptable in most models.
# VIF values greater than 5 (especially above 10) suggest high multicollinearity, which may cause issues with the model's interpretation
vif(diseq_model.tera)

# Plot residuals vs fitted values
plot(diseq_model.tera$fitted.values, resid(diseq_model.tera), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference
# Check normality
qqnorm(resid(diseq_model.tera))
qqline(resid(diseq_model.tera), col = "red")
shapiro.test(resid(diseq_model.tera))

# Plot model predictions
# Create new data for predictions
new_data_tera <- expand.grid(
  disequilib = seq(min(NPP_MAT_dis_teracon$disequilib), max(NPP_MAT_dis_teracon$disequilib), length.out=100),
  temp_treatment = unique(NPP_MAT_dis_teracon$temp_treatment)
)
# Generate predictions and confidence intervals
new_data_fit_tera <- predict(diseq_model.tera, newdata = new_data_tera, interval = "confidence")
# Convert the predictions to a data frame
predictions_tera <- data.frame(new_data_fit_tera)

# Bind the new data and predictions together
predictions_tera <- cbind(new_data_tera, predictions_tera)

# Plot using ggplot
mod.dis.plot.tera <- ggplot(NPP_MAT_dis_teracon, aes(x = disequilib, y = residuals_biomass, color = temp_treatment)) +
  geom_point() +
  geom_line(data = predictions_tera, aes(x = disequilib, y = fit, color = temp_treatment)) +
  geom_ribbon(data = predictions_tera, aes(x = disequilib, y = fit, ymin = lwr, ymax = upr, fill = temp_treatment), alpha = 0.3) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  annotate("text", x = -1.25, y=360,
           label = "Disequilibrium: p = 0.69\nTreatment: p = 0.03\nInteraction: p = 0.51",
           size=3) +
  theme_minimal() +
  labs(x = "Disequilibrium (CTI - MAT)", y = "Residuals Biomass", title = "TeRaCON")


## Including MAT as factor in model
diseq_model.tera2 <- lm(mean_ab_bio ~ MAT + disequilib * temp_treatment, data = NPP_MAT_dis_teracon)
summary(diseq_model.tera2)

# Plot residuals vs fitted values
plot(diseq_model.tera2$fitted.values, resid(diseq_model.tera2), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference
# Check normality
qqnorm(resid(diseq_model.tera2))
qqline(resid(diseq_model.tera2), col = "red")
shapiro.test(resid(diseq_model.tera2))

# Plot model predictions
# Choose mean MAT or specific MAT levels to fix for visualization
mean_MAT_tera <- mean(NPP_MAT_dis_teracon$MAT, na.rm = TRUE)
# Create new data for predictions
new_data_tera2 <- expand.grid(
  disequilib = seq(min(NPP_MAT_dis_teracon$disequilib), max(NPP_MAT_dis_teracon$disequilib), length.out=100),
  temp_treatment = unique(NPP_MAT_dis_teracon$temp_treatment),
  MAT = mean_MAT_tera
)
# Generate predictions and confidence intervals
new_data_fit_tera2 <- predict(diseq_model.tera2, newdata = new_data_tera2, interval = "confidence")
# Convert the predictions to a data frame
predictions_tera2 <- data.frame(new_data_fit_tera2)

# Bind the new data and predictions together
predictions_tera2 <- cbind(new_data_tera2, predictions_tera2)

# Plot using ggplot
ggplot(NPP_MAT_dis_teracon, aes(x = disequilib, y = mean_ab_bio, color = temp_treatment)) +
  geom_point() +
  geom_line(data = predictions_tera2, aes(x = disequilib, y = fit, color = temp_treatment)) +
  geom_ribbon(data = predictions_tera2, aes(x = disequilib, y = fit, ymin = lwr, ymax = upr, fill = temp_treatment), alpha = 0.3) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme_minimal() +
  labs(x = "Disequilibrium (CTI - MAT)", y = "Residuals Biomass", title = "TeRaCON")



## jrgce
# Mering disequilibrium data with biomass data
MAT_dis_jrgce <- CTI_jrgce %>%
  dplyr::select(year,temp_treatment,MAT,disequilib,CTI) %>%
  distinct()
NPP_MAT_dis_jrgce <- left_join(NPP_overall_jrgce, MAT_dis_jrgce,by=c("year","temp_treatment"))
NPP_MAT_dis_jrgce <- NPP_MAT_dis_jrgce %>%
  filter(!is.na(mean_ab_bio)) %>%
  group_by(year,temp_treatment) %>%
  mutate(CTI = mean(CTI), MAT = mean(MAT), disequilib = mean(disequilib)) %>%
  distinct()

# Run model for biomass and MAT
mod.jrgce_dis <- lm(mean_ab_bio ~ MAT, data = NPP_MAT_dis_jrgce)
summary(mod.jrgce_dis)

# Extract residuals from the MAT model
NPP_MAT_dis_jrgce$residuals_biomass <- resid(mod.jrgce_dis)

# Fit a second model using residuals as the response variable and disequilibrium as the predictor
diseq_model.jrgce <- lm(residuals_biomass ~ disequilib * temp_treatment, data=NPP_MAT_dis_jrgce)
# Summary of the model to see the effect of disequilibrium
summary(diseq_model.jrgce)
anova(diseq_model.jrgce)
# A VIF value of 1 means no correlation between this predictor and the other predictors (perfectly uncorrelated).
# VIF values between 1 and 5 indicate a moderate correlation, which is typically acceptable in most models.
# VIF values greater than 5 (especially above 10) suggest high multicollinearity, which may cause issues with the model's interpretation
vif(diseq_model.jrgce)

# Plot residuals vs fitted values
plot(diseq_model.jrgce$fitted.values, resid(diseq_model.jrgce), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference
# Check normality
qqnorm(resid(diseq_model.jrgce))
qqline(resid(diseq_model.jrgce), col = "red")
shapiro.test(resid(diseq_model.jrgce))

# Plot model predictions
# Create new data for predictions
new_data_jrgce <- expand.grid(
  disequilib = seq(min(NPP_MAT_dis_jrgce$disequilib), max(NPP_MAT_dis_jrgce$disequilib), length.out=100),
  temp_treatment = unique(NPP_MAT_dis_jrgce$temp_treatment)
)
# Generate predictions and confidence intervals
new_data_fit_jrgce <- predict(diseq_model.jrgce, newdata = new_data_jrgce, interval = "confidence")
# Convert the predictions to a data frame
predictions_jrgce <- data.frame(new_data_fit_jrgce)

# Bind the new data and predictions together
predictions_jrgce <- cbind(new_data_jrgce, predictions_jrgce)

# Plot using ggplot
mod.dis.plot.jrgce <- ggplot(NPP_MAT_dis_jrgce, aes(x = disequilib, y = residuals_biomass, color = temp_treatment)) +
  geom_point() +
  geom_line(data = predictions_jrgce, aes(x = disequilib, y = fit, color = temp_treatment)) +
  geom_ribbon(data = predictions_jrgce, aes(x = disequilib, y = fit, ymin = lwr, ymax = upr, fill = temp_treatment), alpha = 0.3) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  annotate("text", x = -2.25, y=35,
           label = "Disequilibrium: p = 0.39\nTreatment: p = 0.11\nInteraction: p = 0.90",
           size=3) +
  theme_minimal() +
  labs(x = "Disequilibrium (CTI - MAT)", y = "Residuals Biomass", title = "JRGCE")


## Including MAT as factor in model
diseq_model.jrgce2 <- lm(mean_ab_bio ~ MAT + disequilib * temp_treatment, data = NPP_MAT_dis_jrgce)
summary(diseq_model.jrgce2)

# Plot residuals vs fitted values
plot(diseq_model.jrgce2$fitted.values, resid(diseq_model.jrgce2), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference
# Check normality
qqnorm(resid(diseq_model.jrgce2))
qqline(resid(diseq_model.jrgce2), col = "red")
shapiro.test(resid(diseq_model.jrgce2))

# Plot model predictions
# Extract unique combinations of disequilib and MAT in the original data
new_data_jrgce2 <- NPP_MAT_dis_jrgce %>%
  select(disequilib, MAT) %>%
  distinct()
# Generate predictions and confidence intervals
new_data_jrgce2_fitted <- predict(diseq_model.ok, newdata = new_data_jrgce2, interval = "confidence")
# Combine predictions with the new data
predictions_jrgce2 <- cbind(new_data_jrgce2, new_data_jrgce2_fitted)

# Or:
# Choose mean MAT or specific MAT levels to fix for visualization
mean_MAT_jrgce <- mean(NPP_MAT_dis_jrgce$MAT, na.rm = TRUE)
# Create new data for predictions
new_data_jrgce2 <- expand.grid(
  disequilib = seq(min(NPP_MAT_dis_jrgce$disequilib), max(NPP_MAT_dis_jrgce$disequilib), length.out=100),
  temp_treatment = unique(NPP_MAT_dis_jrgce$temp_treatment),
  MAT = seq(min(NPP_MAT_dis_jrgce$MAT), max(NPP_MAT_dis_jrgce$MAT))
)

# Generate predictions and confidence intervals
new_data_fit_jrgce2 <- predict(diseq_model.jrgce2, newdata = new_data_jrgce2, interval = "confidence")
# Convert the predictions to a data frame
predictions_jrgce2 <- data.frame(new_data_fit_jrgce2)
# Bind the new data and predictions together
predictions_jrgce2 <- cbind(new_data_jrgce2, predictions_jrgce2)

# Plot using ggplot
ggplot(NPP_MAT_dis_jrgce, aes(x = disequilib, y = mean_ab_bio, color = temp_treatment)) +
  geom_point() +
  geom_line(data = predictions_jrgce2, aes(x = disequilib, y = fit, color = temp_treatment)) +
  geom_ribbon(data = predictions_jrgce2, aes(x = disequilib, y = fit, ymin = lwr, ymax = upr, fill = temp_treatment), alpha = 0.3) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme_minimal() +
  labs(x = "Disequilibrium (CTI - MAT)", y = "Residuals Biomass", title = "JRGCE")



## ok
# Mering disequilibrium data with biomass data
MAT_dis_ok <- CTI_ok %>%
  dplyr::select(year,temp_treatment,MAT,disequilib,CTI) %>%
  distinct()
NPP_MAT_dis_ok <- left_join(NPP_overall_ok, MAT_dis_ok,by=c("year","temp_treatment"))
NPP_MAT_dis_ok <- NPP_MAT_dis_ok %>%
  filter(!is.na(mean_ab_bio)) %>%
  group_by(year,temp_treatment) %>%
  mutate(CTI = mean(CTI), MAT = mean(MAT), disequilib = mean(disequilib)) %>%
  distinct()

# Run model for biomass and MAT
mod.ok_dis <- lm(mean_ab_bio ~ MAT, data = NPP_MAT_dis_ok)
summary(mod.ok_dis)

# Extract residuals from the MAT model
NPP_MAT_dis_ok$residuals_biomass <- resid(mod.ok_dis)

# Fit a second model using residuals as the response variable and disequilibrium as the predictor
diseq_model.ok <- lm(residuals_biomass ~ disequilib * temp_treatment, data=NPP_MAT_dis_ok)
# Summary of the model to see the effect of disequilibrium
summary(diseq_model.ok)
anova(diseq_model.ok)
# A VIF value of 1 means no correlation between this predictor and the other predictors (perfectly uncorrelated).
# VIF values between 1 and 5 indicate a moderate correlation, which is typically acceptable in most models.
# VIF values greater than 5 (especially above 10) suggest high multicollinearity, which may cause issues with the model's interpretation
vif(diseq_model.ok)

# Plot residuals vs fitted values
plot(diseq_model.ok$fitted.values, resid(diseq_model.ok), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference
# Check normality
qqnorm(resid(diseq_model.ok))
qqline(resid(diseq_model.ok), col = "red")
shapiro.test(resid(diseq_model.ok))

# Plot model predictions
# Create new data for predictions
new_data_ok <- expand.grid(
  disequilib = seq(min(NPP_MAT_dis_ok$disequilib), max(NPP_MAT_dis_ok$disequilib), length.out=100),
  temp_treatment = unique(NPP_MAT_dis_ok$temp_treatment)
)
# Generate predictions and confidence intervals
new_data_fit_ok <- predict(diseq_model.ok, newdata = new_data_ok, interval = "confidence")
# Convert the predictions to a data frame
predictions_ok <- data.frame(new_data_fit_ok)

# Bind the new data and predictions together
predictions_ok <- cbind(new_data_ok, predictions_ok)

# Plot using ggplot
mod.dis.plot.ok <- ggplot(NPP_MAT_dis_ok, aes(x = disequilib, y = residuals_biomass, color = temp_treatment)) +
  geom_point() +
  geom_line(data = predictions_ok, aes(x = disequilib, y = fit, color = temp_treatment)) +
  geom_ribbon(data = predictions_ok, aes(x = disequilib, y = fit, ymin = lwr, ymax = upr, fill = temp_treatment), alpha = 0.3) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  annotate("text", x = -3, y=210,
           label = "Disequilibrium: p = 0.21\nTreatment: p = 0.17\nInteraction: p = 0.86",
           size=3) +
  theme_minimal() +
  labs(x = "Disequilibrium (CTI - MAT)", y = "Residuals Biomass", title = "Oklahoma")



## phace
# Mering disequilibrium data with biomass data
MAT_dis_phace <- CTI_phace %>%
  dplyr::select(year,temp_treatment,MAT,disequilib,CTI) %>%
  distinct()
NPP_MAT_dis_phace <- left_join(NPP_overall_phace, MAT_dis_phace,by=c("year","temp_treatment"))
NPP_MAT_dis_phace <- NPP_MAT_dis_phace %>%
  filter(!is.na(mean_ab_bio)) %>%
  group_by(year,temp_treatment) %>%
  mutate(CTI = mean(CTI), MAT = mean(MAT), disequilib = mean(disequilib)) %>%
  distinct()

# Run model for biomass and MAT
mod.phace_dis <- lm(mean_ab_bio ~ MAT, data = NPP_MAT_dis_phace)
summary(mod.phace_dis)

# Extract residuals from the MAT model
NPP_MAT_dis_phace$residuals_biomass <- resid(mod.phace_dis)

# Fit a second model using residuals as the response variable and disequilibrium as the predictor
diseq_model.phace <- lm(residuals_biomass ~ disequilib * temp_treatment, data=NPP_MAT_dis_phace)
# Summary of the model to see the effect of disequilibrium
summary(diseq_model.phace)
anova(diseq_model.phace)
# A VIF value of 1 means no correlation between this predictor and the other predictors (perfectly uncorrelated).
# VIF values between 1 and 5 indicate a moderate correlation, which is typically acceptable in most models.
# VIF values greater than 5 (especially above 10) suggest high multicollinearity, which may cause issues with the model's interpretation
vif(diseq_model.phace)

# Plot residuals vs fitted values
plot(diseq_model.phace$fitted.values, resid(diseq_model.phace), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference
# Check normality
qqnorm(resid(diseq_model.phace))
qqline(resid(diseq_model.phace), col = "red")
shapiro.test(resid(diseq_model.phace))

# Plot model predictions
# Create new data for predictions
new_data_phace <- expand.grid(
  disequilib = seq(min(NPP_MAT_dis_phace$disequilib), max(NPP_MAT_dis_phace$disequilib), length.out=100),
  temp_treatment = unique(NPP_MAT_dis_phace$temp_treatment)
)
# Generate predictions and confidence intervals
new_data_fit_phace <- predict(diseq_model.phace, newdata = new_data_phace, interval = "confidence")
# Convert the predictions to a data frame
predictions_phace <- data.frame(new_data_fit_phace)

# Bind the new data and predictions together
predictions_phace <- cbind(new_data_phace, predictions_phace)

# Plot using ggplot
mod.dis.plot.phace <- ggplot(NPP_MAT_dis_phace, aes(x = disequilib, y = residuals_biomass, color = temp_treatment)) +
  geom_point() +
  geom_line(data = predictions_phace, aes(x = disequilib, y = fit, color = temp_treatment)) +
  geom_ribbon(data = predictions_phace, aes(x = disequilib, y = fit, ymin = lwr, ymax = upr, fill = temp_treatment), alpha = 0.3) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  annotate("text", x = -2.35, y=175,
           label = "Disequilibrium: p = 0.17\nTreatment: p = 0.08\nInteraction: p = 0.86",
           size=3) +
  theme_minimal() +
  labs(x = "Disequilibrium (CTI - MAT)", y = "Residuals Biomass", title = "PHACE")



## b4_cfc
# Mering disequilibrium data with biomass data
MAT_dis_b4_cfc <- CTI_b4_cfc %>%
  dplyr::select(year,temp_treatment,MAT,disequilib,CTI) %>%
  distinct()
NPP_MAT_dis_b4_cfc <- left_join(NPP_overall_b4_cfc, MAT_dis_b4_cfc,by=c("year","temp_treatment"))
NPP_MAT_dis_b4_cfc <- NPP_MAT_dis_b4_cfc %>%
  filter(!is.na(mean_ab_bio)) %>%
  group_by(year,temp_treatment) %>%
  mutate(CTI = mean(CTI), MAT = mean(MAT), disequilib = mean(disequilib)) %>%
  distinct()

# Run model for biomass and MAT
mod.b4_cfc_dis <- lm(mean_ab_bio ~ MAT, data = NPP_MAT_dis_b4_cfc)
summary(mod.b4_cfc_dis)

# Extract residuals from the MAT model
NPP_MAT_dis_b4_cfc$residuals_biomass <- resid(mod.b4_cfc_dis)

# Fit a second model using residuals as the response variable and disequilibrium as the predictor
diseq_model.b4_cfc <- lm(residuals_biomass ~ disequilib * temp_treatment, data=NPP_MAT_dis_b4_cfc)
# Summary of the model to see the effect of disequilibrium
summary(diseq_model.b4_cfc)
anova(diseq_model.b4_cfc)
# A VIF value of 1 means no correlation between this predictor and the other predictors (perfectly uncorrelated).
# VIF values between 1 and 5 indicate a moderate correlation, which is typically acceptable in most models.
# VIF values greater than 5 (especially above 10) suggest high multicollinearity, which may cause issues with the model's interpretation
vif(diseq_model.b4_cfc)

# Plot residuals vs fitted values
plot(diseq_model.b4_cfc$fitted.values, resid(diseq_model.b4_cfc), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference
# Check normality
qqnorm(resid(diseq_model.b4_cfc))
qqline(resid(diseq_model.b4_cfc), col = "red")
shapiro.test(resid(diseq_model.b4_cfc))

# Plot model predictions
# Create new data for predictions
new_data_b4_cfc <- expand.grid(
  disequilib = seq(min(NPP_MAT_dis_b4_cfc$disequilib), max(NPP_MAT_dis_b4_cfc$disequilib), length.out=100),
  temp_treatment = unique(NPP_MAT_dis_b4_cfc$temp_treatment)
)
# Generate predictions and confidence intervals
new_data_fit_b4_cfc <- predict(diseq_model.b4_cfc, newdata = new_data_b4_cfc, interval = "confidence")
# Convert the predictions to a data frame
predictions_b4_cfc <- data.frame(new_data_fit_b4_cfc)

# Bind the new data and predictions together
predictions_b4_cfc <- cbind(new_data_b4_cfc, predictions_b4_cfc)

# Plot using ggplot
mod.dis.plot.cfc <- ggplot(NPP_MAT_dis_b4_cfc, aes(x = disequilib, y = residuals_biomass, color = temp_treatment)) +
  geom_point() +
  geom_line(data = predictions_b4_cfc, aes(x = disequilib, y = fit, color = temp_treatment)) +
  geom_ribbon(data = predictions_b4_cfc, aes(x = disequilib, y = fit, ymin = lwr, ymax = upr, fill = temp_treatment), alpha = 0.3) +
  scale_color_manual(name = "Treatment",
                     labels = c("Intermediate","Warmed","Ambient"),
                     values = c("orange","red","blue")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Intermediate","Warmed","Ambient"),
                    values = c("orange","red","blue")) +
  annotate("text", x = -1.15, y=180,
           label = "Disequilibrium: p = 0.75\nTreatments: p > 0.25\nInteractions: p > 0.45",
           size=3) +
  theme_minimal() +
  labs(x = "Disequilibrium (CTI - MAT)", y = "Residuals Biomass", title = "B4WarmED CFC")



## b4_hwrc
# Mering disequilibrium data with biomass data
MAT_dis_b4_hwrc <- CTI_b4_hwrc %>%
  dplyr::select(year,temp_treatment,MAT,disequilib,CTI) %>%
  distinct()
NPP_MAT_dis_b4_hwrc <- left_join(NPP_overall_b4_hwrc, MAT_dis_b4_hwrc,by=c("year","temp_treatment"))
NPP_MAT_dis_b4_hwrc <- NPP_MAT_dis_b4_hwrc %>%
  filter(!is.na(mean_ab_bio)) %>%
  group_by(year,temp_treatment) %>%
  mutate(CTI = mean(CTI), MAT = mean(MAT), disequilib = mean(disequilib)) %>%
  distinct()

# Run model for biomass and MAT
mod.b4_hwrc_dis <- lm(mean_ab_bio ~ MAT, data = NPP_MAT_dis_b4_hwrc)
summary(mod.b4_hwrc_dis)

# Extract residuals from the MAT model
NPP_MAT_dis_b4_hwrc$residuals_biomass <- resid(mod.b4_hwrc_dis)

# Fit a second model using residuals as the response variable and disequilibrium as the predictor
diseq_model.b4_hwrc <- lm(residuals_biomass ~ disequilib * temp_treatment, data=NPP_MAT_dis_b4_hwrc)
# Summary of the model to see the effect of disequilibrium
summary(diseq_model.b4_hwrc)
anova(diseq_model.b4_hwrc)
# A VIF value of 1 means no correlation between this predictor and the other predictors (perfectly uncorrelated).
# VIF values between 1 and 5 indicate a moderate correlation, which is typically acceptable in most models.
# VIF values greater than 5 (especially above 10) suggest high multicollinearity, which may cause issues with the model's interpretation
vif(diseq_model.b4_hwrc)

# Plot residuals vs fitted values
plot(diseq_model.b4_hwrc$fitted.values, resid(diseq_model.b4_hwrc), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference
# Check normality
qqnorm(resid(diseq_model.b4_hwrc))
qqline(resid(diseq_model.b4_hwrc), col = "red")
shapiro.test(resid(diseq_model.b4_hwrc))

# Plot model predictions
# Create new data for predictions
new_data_b4_hwrc <- expand.grid(
  disequilib = seq(min(NPP_MAT_dis_b4_hwrc$disequilib), max(NPP_MAT_dis_b4_hwrc$disequilib), length.out=100),
  temp_treatment = unique(NPP_MAT_dis_b4_hwrc$temp_treatment)
)
# Generate predictions and confidence intervals
new_data_fit_b4_hwrc <- predict(diseq_model.b4_hwrc, newdata = new_data_b4_hwrc, interval = "confidence")
# Convert the predictions to a data frame
predictions_b4_hwrc <- data.frame(new_data_fit_b4_hwrc)

# Bind the new data and predictions together
predictions_b4_hwrc <- cbind(new_data_b4_hwrc, predictions_b4_hwrc)

# Plot using ggplot
mod.dis.plot.hwrc <- ggplot(NPP_MAT_dis_b4_hwrc, aes(x = disequilib, y = residuals_biomass, color = temp_treatment)) +
  geom_point() +
  geom_line(data = predictions_b4_hwrc, aes(x = disequilib, y = fit, color = temp_treatment)) +
  geom_ribbon(data = predictions_b4_hwrc, aes(x = disequilib, y = fit, ymin = lwr, ymax = upr, fill = temp_treatment), alpha = 0.3) +
  scale_color_manual(name = "Treatment",
                     labels = c("Intermediate","Warmed","Ambient"),
                     values = c("orange","red","blue")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Intermediate","Warmed","Ambient"),
                    values = c("orange","red","blue")) +
  annotate("text", x = 0.60, y=150,
           label = "Disequilibrium: p = 0.49\nTreatments: p > 0.25\nInteractions: p > 0.40",
           size=3) +
  theme_minimal() +
  labs(x = "Disequilibrium (CTI - MAT)", y = "Residuals Biomass", title = "B4WarmED HWRC")

### Merge plots
all.exp.merge <- wrap_plots(mod.dis.plot.jrgce, mod.dis.plot.phace, mod.dis.plot.tera,
           mod.dis.plot.ok, mod.dis.plot.cfc, mod.dis.plot.hwrc, ncol = 3) +
  plot_layout(guides = "collect",axis_titles = "collect")



## knz
# Mering disequilibrium data with biomass data
MAP_dis_knz <- CPI_knz %>%
  dplyr::select(year,block,treatment,MAP,disequilib,CPI) %>%
  distinct()
NPP_MAP_dis_knz <- left_join(NPP_overall_knz, MAP_dis_knz,by=c("year","block","treatment"))

## Model comparisons
diseq_model.knz1 <- lmer(biomass ~ disequilib + (1|block), data=NPP_MAP_dis_knz)
diseq_model.knz2 <- lmer(biomass ~ treatment + (1|block), data=NPP_MAP_dis_knz)
diseq_model.knz3 <- lmer(biomass ~ MAP + (1|block), data=NPP_MAP_dis_knz)
diseq_model.knz4 <- lmer(biomass ~ CPI + (1|block), data=NPP_MAP_dis_knz)
diseq_model.knz5 <- lmer(biomass ~ disequilib + treatment + (1|block), data=NPP_MAP_dis_knz)
diseq_model.knz6 <- lmer(biomass ~ disequilib + CPI + (1|block), data=NPP_MAP_dis_knz)
diseq_model.knz7 <- lmer(biomass ~ treatment + CPI + (1|block), data=NPP_MAP_dis_knz)
diseq_model.knz8 <- lmer(biomass ~ treatment + MAP + (1|block), data=NPP_MAP_dis_knz)
diseq_model.knz9 <- lmer(biomass ~ disequilib + treatment + CPI + (1|block), data=NPP_MAP_dis_knz)
diseq_model.knz10 <- lmer(biomass ~ disequilib + treatment + MAP + (1|block), data=NPP_MAP_dis_knz)
# AIC
AIC(diseq_model.knz1, diseq_model.knz2, diseq_model.knz3, 
     diseq_model.knz4, diseq_model.knz5, diseq_model.knz6,
    diseq_model.knz7, diseq_model.knz8, diseq_model.knz9, diseq_model.knz10)
r.squaredGLMM(diseq_model.knz1)
r.squaredGLMM(diseq_model.knz2)
r.squaredGLMM(diseq_model.knz3)
r.squaredGLMM(diseq_model.knz4)
r.squaredGLMM(diseq_model.knz5)
r.squaredGLMM(diseq_model.knz6)
r.squaredGLMM(diseq_model.knz7)
r.squaredGLMM(diseq_model.knz8)
r.squaredGLMM(diseq_model.knz9)
r.squaredGLMM(diseq_model.knz10)
# Summary of the model to see the effect of disequilibrium
summary(diseq_model.knz8)
# A VIF value of 1 means no correlation between this predictor and the other predictors (perfectly uncorrelated).
# VIF values between 1 and 5 indicate a moderate correlation, which is typically acceptable in most models.
# VIF values greater than 5 (especially above 10) suggest high multicollinearity, which may cause issues with the model's interpretation
vif(diseq_model.knz8)

# Plot residuals vs fitted values
plot(diseq_model.knz8$fitted.values, resid(diseq_model.knz8), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference
# Check normality
qqnorm(resid(diseq_model.knz8))
qqline(resid(diseq_model.knz8), col = "red")
shapiro.test(resid(diseq_model.knz8))

# Plot model predictions
# Create new data for predictions
new_data_knz <- NPP_MAP_dis_knz %>%
  group_by(treatment) %>%
  summarise(min_map = min(MAP, na.rm = TRUE),
            max_map = max(MAP, na.rm = TRUE)) %>%
  rowwise() %>%
  do(data.frame(
    MAP = seq(.$min_map, .$max_map, length.out = 100),
    treatment = .$treatment
  )) %>%
  ungroup()

# Predict function for bootMer (only fixed effects)
predict_fun <- function(fit) {
  predict(fit, newdata = new_data_knz, re.form = NA)
}

# Run the bootstrap (e.g., 1000 simulations)
set.seed(123)  # for reproducibility
boot_preds <- bootMer(diseq_model.knz8, predict_fun, nsim = 1000, use.u = FALSE, type = "parametric", re.form = NA)

# Get confidence intervals: 2.5% and 97.5% quantiles for each row in prediction grid
ci <- apply(boot_preds$t, 2, quantile, probs = c(0.025, 0.975))
ci <- t(ci)
colnames(ci) <- c("lwr", "upr")

# Combine predictions with CIs
new_data_knz$fit <- predict(diseq_model.knz8, newdata = new_data_knz, re.form = NA)
new_data_knz$lwr <- ci[, "lwr"]
new_data_knz$upr <- ci[, "upr"]

mod.dis.plot.knz <- ggplot(NPP_MAP_dis_knz, aes(x = MAP, y = biomass, color = treatment)) +
  geom_point(alpha = 0.6) +
  geom_line(data = new_data_knz, aes(x = MAP, y = fit, color = treatment), size = 1) +
  geom_ribbon(data = new_data_knz,
              aes(x = MAP, y = fit, ymin = lwr, ymax = upr, fill = treatment),
              alpha = 0.3, color = NA) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","red")) +
  theme_minimal() +
  labs(x = "Annual Precipitation (AP)", y = "Biomass", title = "KNZ")



## hys
# Mering disequilibrium data with biomass data
MAP_dis_hys <- CPI_hys %>%
  dplyr::select(year,block,treatment,MAP,disequilib,CPI) %>%
  distinct()
NPP_MAP_dis_hys <- left_join(NPP_overall_hys, MAP_dis_hys,by=c("year","block","treatment"))

## Model comparisons
diseq_model.hys1 <- lmer(biomass ~ disequilib + (1|block), data=NPP_MAP_dis_hys)
diseq_model.hys2 <- lmer(biomass ~ treatment + (1|block), data=NPP_MAP_dis_hys)
diseq_model.hys3 <- lmer(biomass ~ MAP + (1|block), data=NPP_MAP_dis_hys)
diseq_model.hys4 <- lmer(biomass ~ CPI + (1|block), data=NPP_MAP_dis_hys)
diseq_model.hys5 <- lmer(biomass ~ disequilib + treatment + (1|block), data=NPP_MAP_dis_hys)
diseq_model.hys6 <- lmer(biomass ~ disequilib + CPI + (1|block), data=NPP_MAP_dis_hys)
diseq_model.hys7 <- lmer(biomass ~ treatment + CPI + (1|block), data=NPP_MAP_dis_hys)
diseq_model.hys8 <- lmer(biomass ~ treatment + MAP + (1|block), data=NPP_MAP_dis_hys)
diseq_model.hys9 <- lmer(biomass ~ disequilib + treatment + CPI + (1|block), data=NPP_MAP_dis_hys)
diseq_model.hys10 <- lmer(biomass ~ disequilib + treatment + MAP + (1|block), data=NPP_MAP_dis_hys)
# AIC
AIC(diseq_model.hys1, diseq_model.hys2, diseq_model.hys3, 
    diseq_model.hys4, diseq_model.hys5, diseq_model.hys6,
    diseq_model.hys7, diseq_model.hys8, diseq_model.hys9, diseq_model.hys10)
r.squaredGLMM(diseq_model.hys1)
r.squaredGLMM(diseq_model.hys2)
r.squaredGLMM(diseq_model.hys3)
r.squaredGLMM(diseq_model.hys4)
r.squaredGLMM(diseq_model.hys5)
r.squaredGLMM(diseq_model.hys6)
r.squaredGLMM(diseq_model.hys7)
r.squaredGLMM(diseq_model.hys8)
r.squaredGLMM(diseq_model.hys9)
r.squaredGLMM(diseq_model.hys10)
# Summary of the model to see the effect of disequilibrium
summary(diseq_model.hys8)
# A VIF value of 1 means no correlation between this predictor and the other predictors (perfectly uncorrelated).
# VIF values between 1 and 5 indicate a moderate correlation, which is typically acceptable in most models.
# VIF values greater than 5 (especially above 10) suggest high multicollinearity, which may cause issues with the model's interpretation
vif(diseq_model.hys8)

# Plot residuals vs fitted values
plot(diseq_model.hys8$fitted.values, resid(diseq_model.hys8), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference
# Check normality
qqnorm(resid(diseq_model.hys8))
qqline(resid(diseq_model.hys8), col = "red")
shapiro.test(resid(diseq_model.hys8))

# Plot model predictions
# Create new data for predictions
new_data_hys <- NPP_MAP_dis_hys %>%
  group_by(treatment) %>%
  summarise(min_map = min(MAP, na.rm = TRUE),
            max_map = max(MAP, na.rm = TRUE)) %>%
  rowwise() %>%
  do(data.frame(
    MAP = seq(.$min_map, .$max_map, length.out = 100),
    treatment = .$treatment
  )) %>%
  ungroup()

# Predict function for bootMer (only fixed effects)
predict_fun <- function(fit) {
  predict(fit, newdata = new_data_hys, re.form = NA)
}

# Run the bootstrap (e.g., 1000 simulations)
set.seed(123)  # for reproducibility
boot_preds <- bootMer(diseq_model.hys8, predict_fun, nsim = 1000, use.u = FALSE, type = "parametric", re.form = NA)

# Get confidence intervals: 2.5% and 97.5% quantiles for each row in prediction grid
ci <- apply(boot_preds$t, 2, quantile, probs = c(0.025, 0.975))
ci <- t(ci)
colnames(ci) <- c("lwr", "upr")

# Combine predictions with CIs
new_data_hys$fit <- predict(diseq_model.hys8, newdata = new_data_hys, re.form = NA)
new_data_hys$lwr <- ci[, "lwr"]
new_data_hys$upr <- ci[, "upr"]

mod.dis.plot.hys <- ggplot(NPP_MAP_dis_hys, aes(x = MAP, y = biomass, color = treatment)) +
  geom_point(alpha = 0.6) +
  geom_line(data = new_data_hys, aes(x = MAP, y = fit, color = treatment), size = 1) +
  geom_ribbon(data = new_data_hys,
              aes(x = MAP, y = fit, ymin = lwr, ymax = upr, fill = treatment),
              alpha = 0.3, color = NA) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","red")) +
  theme_minimal() +
  labs(x = "Annual Precipitation (AP)", y = "Biomass", title = "HYS")


## sgs
# Mering disequilibrium data with biomass data
MAP_dis_sgs <- CPI_sgs %>%
  dplyr::select(year,block,treatment,MAP,disequilib,CPI) %>%
  distinct()
NPP_MAP_dis_sgs <- left_join(NPP_overall_sgs, MAP_dis_sgs,by=c("year","block","treatment"))

## Model comparisons
diseq_model.sgs1 <- lmer(biomass ~ disequilib + (1|block), data=NPP_MAP_dis_sgs)
diseq_model.sgs2 <- lmer(biomass ~ treatment + (1|block), data=NPP_MAP_dis_sgs)
diseq_model.sgs3 <- lmer(biomass ~ MAP + (1|block), data=NPP_MAP_dis_sgs)
diseq_model.sgs4 <- lmer(biomass ~ CPI + (1|block), data=NPP_MAP_dis_sgs)
diseq_model.sgs5 <- lmer(biomass ~ disequilib + treatment + (1|block), data=NPP_MAP_dis_sgs)
diseq_model.sgs6 <- lmer(biomass ~ disequilib + CPI + (1|block), data=NPP_MAP_dis_sgs)
diseq_model.sgs7 <- lmer(biomass ~ treatment + CPI + (1|block), data=NPP_MAP_dis_sgs)
diseq_model.sgs8 <- lmer(biomass ~ treatment + MAP + (1|block), data=NPP_MAP_dis_sgs)
diseq_model.sgs9 <- lmer(biomass ~ disequilib + treatment + CPI + (1|block), data=NPP_MAP_dis_sgs)
diseq_model.sgs10 <- lmer(biomass ~ disequilib + treatment + MAP + (1|block), data=NPP_MAP_dis_sgs)
# AIC
AIC(diseq_model.sgs1, diseq_model.sgs2, diseq_model.sgs3, 
    diseq_model.sgs4, diseq_model.sgs5, diseq_model.sgs6,
    diseq_model.sgs7, diseq_model.sgs8, diseq_model.sgs9, diseq_model.sgs10)
r.squaredGLMM(diseq_model.sgs1)
r.squaredGLMM(diseq_model.sgs2)
r.squaredGLMM(diseq_model.sgs3)
r.squaredGLMM(diseq_model.sgs4)
r.squaredGLMM(diseq_model.sgs5)
r.squaredGLMM(diseq_model.sgs6)
r.squaredGLMM(diseq_model.sgs7)
r.squaredGLMM(diseq_model.sgs8)
r.squaredGLMM(diseq_model.sgs9)
r.squaredGLMM(diseq_model.sgs10)
# Summary of the model to see the effect of disequilibrium
summary(diseq_model.sgs8)
# A VIF value of 1 means no correlation between this predictor and the other predictors (perfectly uncorrelated).
# VIF values between 1 and 5 indicate a moderate correlation, which is typically acceptable in most models.
# VIF values greater than 5 (especially above 10) suggest high multicollinearity, which may cause issues with the model's interpretation
vif(diseq_model.sgs8)

# Plot residuals vs fitted values
plot(diseq_model.sgs8$fitted.values, resid(diseq_model.sgs8), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference
# Check normality
qqnorm(resid(diseq_model.sgs8))
qqline(resid(diseq_model.sgs8), col = "red")
shapiro.test(resid(diseq_model.sgs8))

# Plot model predictions
# Create new data for predictions
new_data_sgs <- NPP_MAP_dis_sgs %>%
  group_by(treatment) %>%
  summarise(min_map = min(MAP, na.rm = TRUE),
            max_map = max(MAP, na.rm = TRUE)) %>%
  rowwise() %>%
  do(data.frame(
    MAP = seq(.$min_map, .$max_map, length.out = 100),
    treatment = .$treatment
  )) %>%
  ungroup()

# Predict function for bootMer (only fixed effects)
predict_fun <- function(fit) {
  predict(fit, newdata = new_data_sgs, re.form = NA)
}

# Run the bootstrap (e.g., 1000 simulations)
set.seed(123)  # for reproducibility
boot_preds <- bootMer(diseq_model.sgs8, predict_fun, nsim = 1000, use.u = FALSE, type = "parametric", re.form = NA)

# Get confidence intervals: 2.5% and 97.5% quantiles for each row in prediction grid
ci <- apply(boot_preds$t, 2, quantile, probs = c(0.025, 0.975))
ci <- t(ci)
colnames(ci) <- c("lwr", "upr")

# Combine predictions with CIs
new_data_sgs$fit <- predict(diseq_model.sgs8, newdata = new_data_sgs, re.form = NA)
new_data_sgs$lwr <- ci[, "lwr"]
new_data_sgs$upr <- ci[, "upr"]

mod.dis.plot.sgs <- ggplot(NPP_MAP_dis_sgs, aes(x = MAP, y = biomass, color = treatment)) +
  geom_point(alpha = 0.6) +
  geom_line(data = new_data_sgs, aes(x = MAP, y = fit, color = treatment), size = 1) +
  geom_ribbon(data = new_data_sgs,
              aes(x = MAP, y = fit, ymin = lwr, ymax = upr, fill = treatment),
              alpha = 0.3, color = NA) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","red")) +
  theme_minimal() +
  labs(x = "Annual Precipitation (AP)", y = "Biomass", title = "SGS")


## chy
# Mering disequilibrium data with biomass data
MAP_dis_chy <- CPI_chy %>%
  dplyr::select(year,block,treatment,MAP,disequilib,CPI) %>%
  distinct()
NPP_MAP_dis_chy <- left_join(NPP_overall_chy, MAP_dis_chy,by=c("year","block","treatment"))

## Model comparisons
diseq_model.chy1 <- lmer(biomass ~ disequilib + (1|block), data=NPP_MAP_dis_chy)
diseq_model.chy2 <- lmer(biomass ~ treatment + (1|block), data=NPP_MAP_dis_chy)
diseq_model.chy3 <- lmer(biomass ~ MAP + (1|block), data=NPP_MAP_dis_chy)
diseq_model.chy4 <- lmer(biomass ~ CPI + (1|block), data=NPP_MAP_dis_chy)
diseq_model.chy5 <- lmer(biomass ~ disequilib + treatment + (1|block), data=NPP_MAP_dis_chy)
diseq_model.chy6 <- lmer(biomass ~ disequilib + CPI + (1|block), data=NPP_MAP_dis_chy)
diseq_model.chy7 <- lmer(biomass ~ treatment + CPI + (1|block), data=NPP_MAP_dis_chy)
diseq_model.chy8 <- lmer(biomass ~ treatment + MAP + (1|block), data=NPP_MAP_dis_chy)
diseq_model.chy9 <- lmer(biomass ~ disequilib + treatment + CPI + (1|block), data=NPP_MAP_dis_chy)
diseq_model.chy10 <- lmer(biomass ~ disequilib + treatment + MAP + (1|block), data=NPP_MAP_dis_chy)
# AIC
AIC(diseq_model.chy1, diseq_model.chy2, diseq_model.chy3, 
    diseq_model.chy4, diseq_model.chy5, diseq_model.chy6,
    diseq_model.chy7, diseq_model.chy8, diseq_model.chy9, diseq_model.chy10)
r.squaredGLMM(diseq_model.chy1)
r.squaredGLMM(diseq_model.chy2)
r.squaredGLMM(diseq_model.chy3)
r.squaredGLMM(diseq_model.chy4)
r.squaredGLMM(diseq_model.chy5)
r.squaredGLMM(diseq_model.chy6)
r.squaredGLMM(diseq_model.chy7)
r.squaredGLMM(diseq_model.chy8)
r.squaredGLMM(diseq_model.chy9)
r.squaredGLMM(diseq_model.chy10)
# Summary of the model to see the effect of disequilibrium
summary(diseq_model.chy8)
# A VIF value of 1 means no correlation between this predictor and the other predictors (perfectly uncorrelated).
# VIF values between 1 and 5 indicate a moderate correlation, which is typically acceptable in most models.
# VIF values greater than 5 (especially above 10) suggest high multicollinearity, which may cause issues with the model's interpretation
vif(diseq_model.chy8)

# Plot residuals vs fitted values
plot(diseq_model.chy8$fitted.values, resid(diseq_model.chy8), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference
# Check normality
qqnorm(resid(diseq_model.chy8))
qqline(resid(diseq_model.chy8), col = "red")
shapiro.test(resid(diseq_model.chy8))

# Plot model predictions
# Create new data for predictions
new_data_chy <- NPP_MAP_dis_chy %>%
  group_by(treatment) %>%
  summarise(min_map = min(MAP, na.rm = TRUE),
            max_map = max(MAP, na.rm = TRUE)) %>%
  rowwise() %>%
  do(data.frame(
    MAP = seq(.$min_map, .$max_map, length.out = 100),
    treatment = .$treatment
  )) %>%
  ungroup()

# Predict function for bootMer (only fixed effects)
predict_fun <- function(fit) {
  predict(fit, newdata = new_data_chy, re.form = NA)
}

# Run the bootstrap (e.g., 1000 simulations)
set.seed(123)  # for reproducibility
boot_preds <- bootMer(diseq_model.chy8, predict_fun, nsim = 1000, use.u = FALSE, type = "parametric", re.form = NA)

# Get confidence intervals: 2.5% and 97.5% quantiles for each row in prediction grid
ci <- apply(boot_preds$t, 2, quantile, probs = c(0.025, 0.975))
ci <- t(ci)
colnames(ci) <- c("lwr", "upr")

# Combine predictions with CIs
new_data_chy$fit <- predict(diseq_model.chy8, newdata = new_data_chy, re.form = NA)
new_data_chy$lwr <- ci[, "lwr"]
new_data_chy$upr <- ci[, "upr"]

mod.dis.plot.chy <- ggplot(NPP_MAP_dis_chy, aes(x = MAP, y = biomass, color = treatment)) +
  geom_point(alpha = 0.6) +
  geom_line(data = new_data_chy, aes(x = MAP, y = fit, color = treatment), size = 1) +
  geom_ribbon(data = new_data_chy,
              aes(x = MAP, y = fit, ymin = lwr, ymax = upr, fill = treatment),
              alpha = 0.3, color = NA) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","red")) +
  theme_minimal() +
  labs(x = "Annual Precipitation (AP)", y = "Biomass", title = "CHY")




### All experiments together
# Removing X column
NPP_MAT_dis_teracon2 <- NPP_MAT_dis_teracon %>%
  select(-c(X, mean_bl_bio, mean_total_n, mean_total_bio, mean_ab_and_root,mean_bl_c,mean_bl_n,mean_ab_c,mean_ab_n))
NPP_MAT_dis_teracon2$site <- "TeRaCON"

NPP_MAT_dis_jrgce2 <- NPP_MAT_dis_jrgce %>%
  select(-X)
NPP_MAT_dis_jrgce2$site <- "JRGCE"

NPP_MAT_dis_ok2 <- NPP_MAT_dis_ok %>%
  select(-X)
NPP_MAT_dis_ok2$site <- "Oklahoma"

NPP_MAT_dis_b4_cfc2 <- NPP_MAT_dis_b4_cfc %>%
  select(-X) %>%
  filter(!(temp_treatment == "1.7"))
NPP_MAT_dis_b4_cfc2$site <- "B4WarmED CFC"
NPP_MAT_dis_b4_cfc2$temp_treatment[NPP_MAT_dis_b4_cfc2$temp_treatment == "3.4"] <- "warmed"
NPP_MAT_dis_b4_cfc2$temp_treatment[NPP_MAT_dis_b4_cfc2$temp_treatment == "amb"] <- "ambient"

NPP_MAT_dis_b4_hwrc2 <- NPP_MAT_dis_b4_hwrc %>%
  select(-X) %>%
  filter(!(temp_treatment == "1.7"))
NPP_MAT_dis_b4_hwrc2$site <- "B4WarmED HWRC"
NPP_MAT_dis_b4_hwrc2$temp_treatment[NPP_MAT_dis_b4_hwrc2$temp_treatment == "3.4"] <- "warmed"
NPP_MAT_dis_b4_hwrc2$temp_treatment[NPP_MAT_dis_b4_hwrc2$temp_treatment == "amb"] <- "ambient"

NPP_MAT_dis_phace2 <- NPP_MAT_dis_phace
NPP_MAT_dis_phace2$site <- "PHACE"

# Merge all data
NPP_MAT_dis_all <- rbind(NPP_MAT_dis_teracon2, NPP_MAT_dis_jrgce2, NPP_MAT_dis_ok2,
                         NPP_MAT_dis_phace2, NPP_MAT_dis_b4_cfc2, NPP_MAT_dis_b4_hwrc2)

# Run model for biomass and MAT
mod.dis <- lm(mean_ab_bio ~ MAT, data = NPP_MAT_dis_all)
summary(mod.dis)

# Extract residuals from the MAT model
NPP_MAT_dis_all$residuals_biomass <- resid(mod.dis)

# Fit a second model using residuals as the response variable and disequilibrium as the predictor
diseq_model <- lm(residuals_biomass ~ disequilib * temp_treatment, data=NPP_MAT_dis_all)
diseq_model2 <- lm(mean_ab_bio ~ disequilib * temp_treatment, data=NPP_MAT_dis_all)
# Summary of the model to see the effect of disequilibrium
summary(diseq_model)
anova(diseq_model)
# A VIF value of 1 means no correlation between this predictor and the other predictors (perfectly uncorrelated).
# VIF values between 1 and 5 indicate a moderate correlation, which is typically acceptable in most models.
# VIF values greater than 5 (especially above 10) suggest high multicollinearity, which may cause issues with the model's interpretation
vif(diseq_model)

# Plot residuals vs fitted values
plot(diseq_model$fitted.values, resid(diseq_model), 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at 0 for reference
# Check normality
qqnorm(resid(diseq_model))
qqline(resid(diseq_model), col = "red")
shapiro.test(resid(diseq_model))

# Plot model predictions
# Create new data for predictions
new_data <- expand.grid(
  disequilib = seq(min(NPP_MAT_dis_all$disequilib), max(NPP_MAT_dis_all$disequilib), length.out=100),
  temp_treatment = unique(NPP_MAT_dis_all$temp_treatment)
)
# Generate predictions and confidence intervals
new_data_fit <- predict(diseq_model, newdata = new_data, interval = "confidence")
# Convert the predictions to a data frame
predictions <- data.frame(new_data_fit)

# Bind the new data and predictions together
predictions <- cbind(new_data, predictions)

#par(mfrow = c(2, 2))

# Plot using ggplot
mod.dis.plot <- ggplot(NPP_MAT_dis_all, aes(x = disequilib, y = residuals_biomass, color = temp_treatment)) +
  geom_point(aes(shape=site)) +
  geom_line(data = predictions, aes(x = disequilib, y = fit, color = temp_treatment)) +
  #geom_line(data = predictions_ok, aes(x = disequilib, y = fit, color = temp_treatment)) +
  #geom_line(data = predictions_tera, aes(x = disequilib, y = fit, color = temp_treatment)) +
  #geom_line(data = predictions_phace, aes(x = disequilib, y = fit, color = temp_treatment)) +
  #geom_line(data = predictions_jrgce, aes(x = disequilib, y = fit, color = temp_treatment)) +
  geom_ribbon(data = predictions, aes(x = disequilib, y = fit, ymin = lwr, ymax = upr, fill = temp_treatment), alpha = 0.3) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  annotate("text", x = -3.15, y=400,
           label = "Disequilibrium: p = 0.20\nTreatment: p = 0.09\nInteraction: p = 0.68",
           size=3) +
  theme_minimal() +
  labs(x = "Disequilibrium (CTI - MAT)", y = "Residuals Biomass")




# Export Rdata for plot
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
saveRDS(mod.dis.plot, paste(path_out,'comb_diseq_model.rds'))
saveRDS(mod.dis.plot.knz, paste(path_out,'diseq_model_knz.rds'))
saveRDS(mod.dis.plot.hys, paste(path_out,'diseq_model_hys.rds'))
saveRDS(mod.dis.plot.sgs, paste(path_out,'diseq_model_sgs.rds'))
saveRDS(mod.dis.plot.chy, paste(path_out,'diseq_model_chy.rds'))
saveRDS(all.exp.merge, paste(path_out,'diseq_model.rds'))


