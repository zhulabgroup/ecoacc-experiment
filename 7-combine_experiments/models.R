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
library(broom.mixed)
library(nlme)



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
CTI_CPI_b4_cfc <- read.csv(" CTI_CPI_b4warmed_cfc.csv")
NPP_overall_b4_cfc <- read.csv(" eco_response_overall_b4warmed_cfc.csv")

cfc_test <- read.csv(" b4warmed_cfc_cohort_test.csv")

CTI_sens_b4_hwrc <- read.csv(" CTI_sens_b4warmed_hwrc.csv")
CTI_b4_hwrc <- read.csv(" CTI_b4warmed_hwrc.csv")
CTI_CPI_b4_hwrc <- read.csv(" CTI_CPI_b4warmed_hwrc.csv")
NPP_overall_b4_hwrc <- read.csv(" eco_response_overall_b4warmed_hwrc.csv")

### Set path to oklahoma data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/OK/"
setwd(path_data)
# Load in data
CTI_sens_ok <- read.csv(" CTI_sens_ok.csv")
CTI_ok <- read.csv(" CTI_ok.csv")
CTI_CPI_ok <- read.csv(" CTI_CPI_ok.csv")
NPP_overall_ok <- read.csv(" eco_response_overall_ok.csv")

# Trait and contribution data
# Add in species trait values
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
setwd(path_data)
trait_con <- readRDS(" trait_contribution_data.rds")



### Models to test for CTI changes over time and due to warming treatment
# Teracon
outlier_indices_tera <- c(485, 437, 41,319, 533, 382, 55, 46)
CTI_teracon_clean <- CTI_teracon[-outlier_indices_tera, ]
mod.tera <- lm(CTI ~ year * temp_treatment + (1|plot), data = CTI_teracon)
# Check for homogeneity of variances assumption (true if p>0.05)
# If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(mod.tera) ~ CTI_teracon$temp_treatment) # pretty close
# Check normality assumption
qqPlot(resid(mod.tera))
hist(residuals(mod.tera))
shapiro.test(resid(mod.tera))
# Any outliers?
# Note: outliers are removed above based on outliers detected here
outlierTest(mod.tera)
# Model results
summary(mod.tera) # note: results are the same w/ and w/o outliers
# Save predictions
model_preds_tera <- predict(mod.tera)

# JRGCE
outlier_indices_jrgce <- c(924,655,928,932,383,792,931,2094,927,929)
CTI_jrgce_clean <- CTI_jrgce[-outlier_indices_jrgce, ]
mod.jrgce <- lmer(CTI ~ year * temp_treatment + (1|plot), data = CTI_jrgce)
# Check for homogeneity of variances assumption (true if p>0.05)
# If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(mod.jrgce) ~ CTI_jrgce$temp_treatment)
# Check normality assumption
qqPlot(resid(mod.jrgce))
hist(residuals(mod.jrgce))
shapiro.test(resid(mod.jrgce))
# Any outliers?
outlierTest(mod.jrgce)
# Model results
summary(mod.jrgce) # note: results are the same w/ and w/o outliers
# Save predictions
model_preds_jrgce <- predict(mod.jrgce)

# PHACE
outlier_indices_phace <- c(56)
CTI_phace_clean <- CTI_phace[-outlier_indices_phace, ]
mod.phace <- lmer(CTI ~ year * temp_treatment + (1|plot), data = CTI_phace)
# Check for homogeneity of variances assumption (true if p>0.05)
# If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(mod.phace) ~ CTI_phace$temp_treatment)
# Check normality assumption
qqPlot(resid(mod.phace))
hist(residuals(mod.phace))
shapiro.test(resid(mod.phace))
# Any outliers?
outlierTest(mod.phace)
# Model results
summary(mod.phace) # note: results are the same w/ and w/o outliers
# Save predictions
model_preds_phace <- predict(mod.phace)

# B4Warmed
# no outliers
CTI_b4_cfc$temp_treatment <- relevel(factor(CTI_b4_cfc$temp_treatment), ref = "amb")
mod.b4_cfc <- lmer(CTI ~ year * temp_treatment + (1|plot), data = CTI_b4_cfc)
# Check for homogeneity of variances assumption (true if p>0.05)
# If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(mod.b4_cfc) ~ CTI_b4_cfc$temp_treatment)
# Check normality assumption
qqPlot(resid(mod.b4_cfc))
hist(residuals(mod.b4_cfc))
shapiro.test(resid(mod.b4_cfc))
# Any outliers?
outlierTest(mod.b4_cfc)
# Model results
summary(mod.b4_cfc)
# Save predictions
model_preds_cfc <- predict(mod.b4_cfc)

CTI_b4_hwrc$temp_treatment <- relevel(factor(CTI_b4_hwrc$temp_treatment), ref = "amb")
mod.b4_hwrc <- lmer(CTI ~ year * temp_treatment + (1|plot), data = CTI_b4_hwrc)
# Check for homogeneity of variances assumption (true if p>0.05)
# If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(mod.b4_hwrc) ~ CTI_b4_hwrc$temp_treatment)
# Check normality assumption
qqPlot(resid(mod.b4_hwrc))
hist(residuals(mod.b4_hwrc))
shapiro.test(resid(mod.b4_hwrc))
# Any outliers?
outlierTest(mod.b4_hwrc)
# Model results
summary(mod.b4_hwrc)
# Save predictions
model_preds_hwrc <- predict(mod.b4_hwrc)

# OK
# no outliers
mod.ok <- lmer(CTI ~ year * temp_treatment + (1|plot), data = CTI_ok)
# Check for homogeneity of variances assumption (true if p>0.05)
# If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(mod.ok) ~ CTI_ok$temp_treatment)
# Check normality assumption
qqPlot(resid(mod.ok))
hist(residuals(mod.ok))
shapiro.test(resid(mod.ok))
# Any outliers?
outlierTest(mod.ok)
# Model results
summary(mod.ok)
# Save predictions
model_preds_ok <- predict(mod.ok)



### Models testing for CTI sensitivity over time
# Teracon
mod.tera_sens <- lm(sensitivity ~ year, data = CTI_sens_teracon)
qqPlot(resid(mod.tera_sens))
hist(residuals(mod.tera_sens))
shapiro.test(resid(mod.tera_sens))
summary(mod.tera_sens)
# Save predictions
model_preds_tera_sens <- predict(mod.tera_sens)

# JRGCE
mod.jrgce_sens <- lm(sensitivity ~ year, data = CTI_sens_jrgce)
qqPlot(resid(mod.jrgce_sens))
hist(residuals(mod.jrgce_sens))
shapiro.test(resid(mod.jrgce_sens))
summary(mod.jrgce_sens)
# Save predictions
model_preds_jrgce_sens <- predict(mod.jrgce_sens)

# PHACE
mod.phace_sens <- lm(sensitivity ~ year, data = CTI_sens_phace)
qqPlot(resid(mod.phace_sens))
hist(residuals(mod.phace_sens))
shapiro.test(resid(mod.phace_sens))
summary(mod.phace_sens)
# Save predictions
model_preds_phace_sens <- predict(mod.phace_sens)

# B4Warmed
mod.b4_cfc_sens_high <- lm(sensitivity_high_temp ~ year, data = CTI_sens_b4_cfc)
qqPlot(resid(mod.b4_cfc_sens_high))
hist(residuals(mod.b4_cfc_sens_high))
shapiro.test(resid(mod.b4_cfc_sens_high))
summary(mod.b4_cfc_sens_high)
# Save predictions
model_preds_cfc_3.4_sens <- predict(mod.b4_cfc_sens_high)

mod.b4_cfc_sens_med <- lm(sensitivity_med_temp ~ year, data = CTI_sens_b4_cfc)
qqPlot(resid(mod.b4_cfc_sens_med))
hist(residuals(mod.b4_cfc_sens_med))
shapiro.test(resid(mod.b4_cfc_sens_med))
summary(mod.b4_cfc_sens_med)
# Save predictions
model_preds_cfc_1.7_sens <- predict(mod.b4_cfc_sens_med)

mod.b4_hwrc_sens_high <- lm(sensitivity_high_temp ~ year, data = CTI_sens_b4_hwrc)
qqPlot(resid(mod.b4_hwrc_sens_high))
hist(residuals(mod.b4_hwrc_sens_high))
shapiro.test(resid(mod.b4_hwrc_sens_high))
summary(mod.b4_hwrc_sens_high)
# Save predictions
model_preds_hwrc_3.4_sens <- predict(mod.b4_hwrc_sens_high)

mod.b4_hwrc_sens_med <- lm(sensitivity_med_temp ~ year, data = CTI_sens_b4_hwrc)
qqPlot(resid(mod.b4_hwrc_sens_med))
hist(residuals(mod.b4_hwrc_sens_med))
shapiro.test(resid(mod.b4_hwrc_sens_med))
summary(mod.b4_hwrc_sens_med)
# Save predictions
model_preds_hwrc_1.7_sens <- predict(mod.b4_hwrc_sens_med)

# OK
mod.ok_sens <- lm(sensitivity ~ year, data = CTI_sens_ok)
qqPlot(resid(mod.ok_sens))
hist(residuals(mod.ok_sens))
shapiro.test(resid(mod.ok_sens))
summary(mod.ok_sens)
# Save predictions
model_preds_ok_sens <- predict(mod.ok_sens)



### Traits vs contribution
trait_con_plot <- trait_con %>%
  filter(!(TraitName == "Seed terminal velocity")) %>%
  filter(mean_trait_val > 0) %>%
  mutate(log_mean_trait_val = log(mean_trait_val))
# Initialize a list to store models
models_list <- list()

# Loop through each unique TraitID (or use TraitName if preferred)
for (trait in unique(trait_con_plot$TraitName)) {
  
  # Subset data for the specific TraitID
  trait_data <- trait_con_plot %>% filter(TraitName == trait)
  
  # Ensure sufficient data points before running the model
  if (nrow(trait_data) > 5) {  # Adjust threshold as needed
    mod <- lmer(contribution_center ~ log_mean_trait_val + (1|site), data = trait_data)
    
    # Store model in list with TraitID as key
    models_list[[as.character(trait)]] <- mod
  } else {
    message("Skipping TraitID ", trait, " due to insufficient data.")
  }
}

# Print summary for each model
lapply(models_list, summary)

# Create a data frame of significance
significance_df <- lapply(models_list, function(mod) {
  tidy(mod, effects = "fixed") %>%
    filter(term == "log_mean_trait_val") %>%
    dplyr::select(term, estimate, p.value)
}) %>%
  bind_rows(.id = "TraitName")

# Adjust p-values for multiple comparisons
significance_df <- significance_df %>%
  mutate(p.value.adj = p.adjust(p.value, method = "BH"))

# Flag significance after BH correction
significance_df <- significance_df %>%
  mutate(significant_light = ifelse(p.value.adj >= 0.05 & p.value.adj < 0.1, TRUE, FALSE)) %>%
  mutate(significant = ifelse(p.value.adj < 0.05, TRUE, FALSE))

# Create label with asterisk for significant traits
significance_df <- significance_df %>%
  mutate(TraitLabel = ifelse(significant, paste0(TraitName, " ***"), TraitName)) %>%
  mutate(TraitLabel = ifelse(significant_light, paste0(TraitName, " *"), TraitLabel))


### Plot
# Create an empty list to store prediction data with confidence intervals
predictions_list <- list()

# Loop through each model to generate predictions and confidence intervals
for (trait in names(models_list)) {
  
  mod <- models_list[[trait]]
  trait_data <- trait_con_plot %>% filter(TraitName == trait)  # Get original data
  
  # Create a new dataset covering the observed range of mean_trait_val
  new_data <- data.frame(log_mean_trait_val = seq(min(trait_data$log_mean_trait_val),
                                              max(trait_data$log_mean_trait_val),
                                              length.out = 100),
                         site = NA)  # Site is random, so set to NA
  
  # Get predictions with standard errors
  preds <- predict(mod, newdata = new_data, re.form = NA, se.fit = TRUE)
  
  # Add predictions and confidence intervals
  new_data$predicted <- preds$fit
  new_data$lower <- preds$fit - 1.96 * preds$se.fit  # 95% CI lower
  new_data$upper <- preds$fit + 1.96 * preds$se.fit  # 95% CI upper
  new_data$TraitName <- trait  # Add trait identifier
  
  # Store in list
  predictions_list[[trait]] <- new_data
}

# Combine all predictions into one dataframe
predictions_df <- bind_rows(predictions_list)

# Merge significance_df with trait_con_plot and predictions_df
trait_con_plot <- trait_con_plot %>%
  left_join(significance_df %>% dplyr::select(TraitName, TraitLabel), by = "TraitName")

predictions_df <- predictions_df %>%
  left_join(significance_df %>% dplyr::select(TraitName, TraitLabel), by = "TraitName")

## Plot with confidence intervals
# Specify order for facet wrap
ordered_traits <- c("Seed dry mass","Seed length","Seed number per plant",
                    "Seed germination rate","Seed longevity","SLA","LDMC",
                    "Leaf thickness","Stem diameter","Plant height vegetative","Specific root length",
                    "Specific fine root length","Fine root length")

trait_con_plot <- trait_con_plot %>%
  mutate(TraitLabel = factor(TraitLabel, levels = ordered_traits))
levels(trait_con_plot$TraitLabel)

# Fill color
trait_con_plot$fill_color <- ifelse(trait_con_plot$temp_niche_center > 0, "Warm", "Cold")

# Denote if significant or not
predictions_df$significant <- ifelse(predictions_df$TraitName %in% significance_df$TraitName[significance_df$significant], TRUE, FALSE)

png("traits_contributions.png", units="in", width=9, height=8, res=300)
#traits_con <- 
  ggplot(trait_con_plot, aes(x = log_mean_trait_val, y = contribution_center)) +
  geom_point(alpha=0.3,color="steelblue4") +  # Raw data points
  geom_ribbon(data = predictions_df, aes(y = predicted, ymin = lower, ymax = upper), alpha = 0.6, fill = "grey60") +  # Confidence interval
  geom_line(data = predictions_df, aes(y = predicted,linetype=significant), size = 1,color="steelblue4") +  # Predicted line
  facet_wrap(~factor(TraitLabel), scales = "free") +  # Separate plots for each trait
  #scale_color_manual(
  #    values = c("Warm" = "red", "Cold" = "blue"),
  #    name = "Species\ntemperature\nanomaly"
  #  ) +
  #geom_text(data = subset(trait_con_plot, TraitName %in% significance_df$TraitName[significance_df$significant]),
  #          aes(x = -Inf, y = Inf),
  #          label = "*",
  #          hjust = -0.3, vjust = 1.1,
  #          size = 6, fontface = "bold", color = "red", inherit.aes = FALSE) +
  labs(x = "Log(Mean trait value)", y = "Species contribution to β CTI") +
  theme_bw() +
  theme(panel.background = element_blank(),
        #panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold"),
        legend.position = "none")
dev.off()




### Traits vs abundance change
trait_con_abun <- trait_con %>%
  filter(!(TraitName == "Seed terminal velocity")) %>%
  filter(mean_trait_val > 0) %>%
  mutate(log_mean_trait_val = log(mean_trait_val))
# Initialize a list to store models
models_list_abun <- list()

# Loop through each unique TraitID (or use TraitName if preferred)
for (trait in unique(trait_con_abun$TraitName)) {
  
  # Subset data for the specific TraitID
  trait_data <- trait_con_abun %>% filter(TraitName == trait)
  
  # Ensure sufficient data points before running the model
  if (nrow(trait_data) > 5) {  # Adjust threshold as needed
    mod <- lmer(slope ~ log_mean_trait_val + (1|site), data = trait_data)
    
    # Store model in list with TraitID as key
    models_list_abun[[as.character(trait)]] <- mod
  } else {
    message("Skipping TraitID ", trait, " due to insufficient data.")
  }
}

# Print summary for each model
lapply(models_list_abun, summary)

# Create a data frame of significance
significance_df_abun <- lapply(models_list_abun, function(mod) {
  tidy(mod, effects = "fixed") %>%
    filter(term == "log_mean_trait_val") %>%
    dplyr::select(term, estimate, p.value)
}) %>%
  bind_rows(.id = "TraitName")

# Adjust p-values for multiple comparisons
significance_df_abun <- significance_df_abun %>%
  mutate(p.value.adj = p.adjust(p.value, method = "BH"))

# Flag significance after BH correction
significance_df_abun <- significance_df_abun %>%
  mutate(significant_light = ifelse(p.value.adj >= 0.05 & p.value.adj < 0.1, TRUE, FALSE)) %>%
  mutate(significant = ifelse(p.value.adj < 0.05, TRUE, FALSE))

# Create label with asterisk for significant traits
significance_df_abun <- significance_df_abun %>%
  mutate(TraitLabel = ifelse(significant, paste0(TraitName, " ***"), TraitName)) %>%
  mutate(TraitLabel = ifelse(significant_light, paste0(TraitName, " *"), TraitLabel))

### Plot
# Create an empty list to store prediction data with confidence intervals
predictions_list_abun <- list()

# Loop through each model to generate predictions and confidence intervals
for (trait in names(models_list_abun)) {
  
  mod <- models_list_abun[[trait]]
  trait_data <- trait_con_abun %>% filter(TraitName == trait)  # Get original data
  
  # Create a new dataset covering the observed range of mean_trait_val
  new_data <- data.frame(log_mean_trait_val = seq(min(trait_data$log_mean_trait_val),
                                                  max(trait_data$log_mean_trait_val),
                                                  length.out = 100),
                         site = NA)  # Site is random, so set to NA
  
  # Get predictions with standard errors
  preds <- predict(mod, newdata = new_data, re.form = NA, se.fit = TRUE)
  
  # Add predictions and confidence intervals
  new_data$predicted <- preds$fit
  new_data$lower <- preds$fit - 1.96 * preds$se.fit  # 95% CI lower
  new_data$upper <- preds$fit + 1.96 * preds$se.fit  # 95% CI upper
  new_data$TraitName <- trait  # Add trait identifier
  
  # Store in list
  predictions_list_abun[[trait]] <- new_data
}

# Combine all predictions into one dataframe
predictions_df_abun <- bind_rows(predictions_list_abun)

# Merge significance_df with trait_con_abun and predictions_df
trait_con_abun <- trait_con_abun %>%
  left_join(significance_df_abun %>% dplyr::select(TraitName, TraitLabel), by = "TraitName")

predictions_df_abun <- predictions_df_abun %>%
  left_join(significance_df_abun %>% dplyr::select(TraitName, TraitLabel), by = "TraitName")

## Plot with confidence intervals
# Specify order for facet wrap
ordered_traits <- c("Seed dry mass","Seed length","Seed terminal velocity","Seed number per plant",
                    "Seed germination rate","Seed longevity","SLA","LDMC",
                    "Leaf thickness","Stem diameter","Plant height vegetative","Specific root length",
                    "Specific fine root length","Fine root length ***")

trait_con_abun <- trait_con_abun %>%
  mutate(TraitLabel = factor(TraitLabel, levels = ordered_traits))
levels(trait_con_abun$TraitLabel)

png("traits_abun.png", units="in", width=9, height=8, res=300)
#traits_abun <-
  ggplot(trait_con_abun, aes(x = log_mean_trait_val, y = slope)) +
  geom_point(alpha=0.3,color="steelblue4") +  # Raw data points
  geom_ribbon(data = predictions_df_abun, aes(y = predicted, ymin = lower, ymax = upper), alpha = 0.6, fill = "grey60") +  # Confidence interval
  geom_line(data = predictions_df_abun, aes(y = predicted), color = "steelblue4", size = 1) +  # Predicted line
  facet_wrap(~factor(TraitLabel), scales = "free") +  # Separate plots for each trait
  #scale_fill_gradient2(
  #  low = "blue", mid = "grey90", high = "red", midpoint = 0,
  #  name = "Species\ntemperature anomaly"
  #) +
  #geom_text(data = subset(trait_con_abun, TraitName %in% significance_df_abun$TraitName[significance_df_abun$significant]),
  #          aes(x = -Inf, y = Inf),
  #          label = "*",
  #          hjust = -0.3, vjust = 1.1,
  #          size = 6, fontface = "bold", color = "red", inherit.aes = FALSE) +
  labs(x = "Log(Mean trait value)", y = expression(bold("Species " * beta * " Abundance"))) +
  theme_bw() +
  theme(panel.background = element_blank(),
        #panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold"))
dev.off()



### Traits vs temp niche
trait_con_niche <- trait_con %>%
  filter(!(TraitName == "Seed terminal velocity")) %>%
  filter(mean_trait_val > 0) %>%
  mutate(log_mean_trait_val = log(mean_trait_val))
# Initialize a list to store models
models_list_niche <- list()

# Loop through each unique TraitID (or use TraitName if preferred)
for (trait in unique(trait_con_niche$TraitName)) {
  
  # Subset data for the specific TraitID
  trait_data <- trait_con_niche %>% filter(TraitName == trait)
  
  # Ensure sufficient data points before running the model
  if (nrow(trait_data) > 5) {  # Adjust threshold as needed
    mod <- lmer(temp_niche_center ~ log_mean_trait_val + (1|site), data = trait_data)
    
    # Store model in list with TraitID as key
    models_list_niche[[as.character(trait)]] <- mod
  } else {
    message("Skipping TraitID ", trait, " due to insufficient data.")
  }
}

# Print summary for each model
lapply(models_list_niche, summary)

# Create a data frame of significance
significance_df_niche <- lapply(models_list_niche, function(mod) {
  tidy(mod, effects = "fixed") %>%
    filter(term == "log_mean_trait_val") %>%
    dplyr::select(term, estimate, p.value)
}) %>%
  bind_rows(.id = "TraitName")

# Adjust p-values for multiple comparisons
significance_df_niche <- significance_df_niche %>%
  mutate(p.value.adj = p.adjust(p.value, method = "BH"))

# Flag significance after BH correction
significance_df_niche <- significance_df_niche %>%
  mutate(significant_light = ifelse(p.value.adj >= 0.05 & p.value.adj < 0.1, TRUE, FALSE)) %>%
  mutate(significant = ifelse(p.value.adj < 0.05, TRUE, FALSE))

# Create label with asterisk for significant traits
significance_df_niche <- significance_df_niche %>%
  mutate(TraitLabel = ifelse(significant, paste0(TraitName, " ***"), TraitName)) %>%
  mutate(TraitLabel = ifelse(significant_light, paste0(TraitName, " *"), TraitLabel))

### Plot
# Create an empty list to store prediction data with confidence intervals
predictions_list_niche <- list()

# Loop through each model to generate predictions and confidence intervals
for (trait in names(models_list_niche)) {
  
  mod <- models_list_niche[[trait]]
  trait_data <- trait_con_niche %>% filter(TraitName == trait)  # Get original data
  
  # Create a new dataset covering the observed range of mean_trait_val
  new_data <- data.frame(log_mean_trait_val = seq(min(trait_data$log_mean_trait_val),
                                                  max(trait_data$log_mean_trait_val),
                                                  length.out = 100),
                         site = NA)  # Site is random, so set to NA
  
  # Get predictions with standard errors
  preds <- predict(mod, newdata = new_data, re.form = NA, se.fit = TRUE)
  
  # Add predictions and confidence intervals
  new_data$predicted <- preds$fit
  new_data$lower <- preds$fit - 1.96 * preds$se.fit  # 95% CI lower
  new_data$upper <- preds$fit + 1.96 * preds$se.fit  # 95% CI upper
  new_data$TraitName <- trait  # Add trait identifier
  
  # Store in list
  predictions_list_niche[[trait]] <- new_data
}

# Combine all predictions into one dataframe
predictions_df_niche <- bind_rows(predictions_list_niche)

# Merge significance_df with trait_con_niche and predictions_df
trait_con_niche <- trait_con_niche %>%
  left_join(significance_df_niche %>% dplyr::select(TraitName, TraitLabel), by = "TraitName")

predictions_df_niche <- predictions_df_niche %>%
  left_join(significance_df_niche %>% dplyr::select(TraitName, TraitLabel), by = "TraitName")

## Plot with confidence intervals
# Specify order for facet wrap
ordered_traits <- c("Seed dry mass","Seed length","Seed terminal velocity","Seed number per plant",
                    "Seed germination rate","Seed longevity","SLA","LDMC",
                    "Leaf thickness","Stem diameter","Plant height vegetative ***","Specific root length",
                    "Specific fine root length","Fine root length")

trait_con_niche <- trait_con_niche %>%
  mutate(TraitLabel = factor(TraitLabel, levels = ordered_traits))
levels(trait_con_niche$TraitLabel)

png("traits_niche.png", units="in", width=9, height=8, res=300)
#traits_niche <- 
  ggplot(trait_con_niche, aes(x = log_mean_trait_val, y = temp_niche_center)) +
  geom_point(alpha=0.3,color="steelblue4") +  # Raw data points
  geom_ribbon(data = predictions_df_niche, aes(y = predicted, ymin = lower, ymax = upper), alpha = 0.6, fill = "grey60") +  # Confidence interval
  geom_line(data = predictions_df_niche, aes(y = predicted), color = "steelblue4", size = 1) +  # Predicted line
  facet_wrap(~factor(TraitLabel), scales = "free") +  # Separate plots for each trait
  #geom_text(data = subset(trait_con_niche, TraitName %in% significance_df_niche$TraitName[significance_df_niche$significant]),
  #          aes(x = -Inf, y = Inf),
  #          label = "*",
  #          hjust = -0.3, vjust = 1.1,
  #          size = 6, fontface = "bold", color = "red", inherit.aes = FALSE) +
  labs(x = "Log(Mean trait value)", y = "Species temperature anomaly (°C)") +
  theme_bw() +
  theme(panel.background = element_blank(),
        #panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold"))
dev.off()




# Export Rdata for plot
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
saveRDS(traits_con, paste(path_out,'traits_contributions.rds'))
saveRDS(traits_abun, paste(path_out,'traits_abundances.rds'))
saveRDS(traits_niche, paste(path_out,'traits_niches.rds'))

saveRDS(model_preds_tera, paste(path_out,'cti_pred_tera.rds'))
saveRDS(model_preds_jrgce, paste(path_out,'cti_pred_jrgce.rds'))
saveRDS(model_preds_phace, paste(path_out,'cti_pred_phace.rds'))
saveRDS(model_preds_cfc, paste(path_out,'cti_pred_b4warmed_cfc.rds'))
saveRDS(model_preds_hwrc, paste(path_out,'cti_pred_b4warmed_hwrc.rds'))
saveRDS(model_preds_ok, paste(path_out,'cti_pred_ok.rds'))

saveRDS(model_preds_tera_sens, paste(path_out,'cti_sens_pred_tera.rds'))
saveRDS(model_preds_jrgce_sens, paste(path_out,'cti_sens_pred_jrgce.rds'))
saveRDS(model_preds_phace_sens, paste(path_out,'cti_sens_pred_phace.rds'))
saveRDS(model_preds_cfc_3.4_sens, paste(path_out,'cti_sens_pred_b4warmed_cfc_3.4.rds'))
saveRDS(model_preds_cfc_1.7_sens, paste(path_out,'cti_sens_pred_b4warmed_cfc_1.7.rds'))
saveRDS(model_preds_hwrc_3.4_sens, paste(path_out,'cti_sens_pred_b4warmed_hwrc_3.4.rds'))
saveRDS(model_preds_hwrc_1.7_sens, paste(path_out,'cti_sens_pred_b4warmed_hwrc_1.7.rds'))
saveRDS(model_preds_ok_sens, paste(path_out,'cti_sens_pred_ok.rds'))




