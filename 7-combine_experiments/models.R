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
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
setwd(path_data)
# Load in data
trait_con <- readRDS(" trait_contribution_data.rds")


### Models to test for CTI changes over time and due to warming treatment
# Teracon
outlier_indices_tera <- c(485, 437, 41, 319, 533, 382, 55, 46)
CTI_teracon_clean <- CTI_teracon[-outlier_indices_tera, ]
mod.tera <- lmer(CTI ~ year * temp_treatment + (1|plot), data = CTI_teracon)
# Check for homogeneity of variances assumption (true if p>0.05)
# If the result is not significant, the assumption of equal variances (homoscedasticity) is met (no significant difference between the group variances).
leveneTest(residuals(mod.tera) ~ CTI_teracon$temp_treatment)
# Check normality assumption
qqPlot(resid(mod.tera))
hist(residuals(mod.tera))
shapiro.test(resid(mod.tera))
# Any outliers?
# Note: outliers are removed above based on outliers detected here
outlierTest(mod.tera)
# Model results
anova(mod.tera)
summary(mod.tera)
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
anova(mod.jrgce)
summary(mod.jrgce)


# PHACE
mod.phace <- lmer(CTI ~ year * temp_treatment + (1|plot), data = CTI_phace)
anova(mod.phace)
summary(mod.phace)

# B4Warmed
CTI_b4_cfc <- within(CTI_b4_cfc, temp_treatment <- relevel(factor(temp_treatment), ref = "amb"))
mod.b4_cfc <- lmer(CTI ~ year * temp_treatment + (1|plot), data = CTI_b4_cfc)
anova(mod.b4_cfc)
summary(mod.b4_cfc)

CTI_b4_hwrc <- within(CTI_b4_hwrc, temp_treatment <- relevel(factor(temp_treatment), ref = "amb"))
mod.b4_hwrc <- lmer(CTI ~ year * temp_treatment + (1|plot), data = CTI_b4_hwrc)
anova(mod.b4_hwrc)
summary(mod.b4_hwrc)

# OK
mod.ok <- lmer(CTI ~ year * temp_treatment + (1|plot), data = CTI_ok)
anova(mod.ok)
summary(mod.ok)



### Models testing for CTI sensitivity over time
# Teracon
mod.tera_sens <- lm(sensitivity ~ year, data = CTI_sens_teracon)
anova(mod.tera_sens)
summary(mod.tera_sens)

# JRGCE
mod.jrgce_sens <- lm(sensitivity ~ year, data = CTI_sens_jrgce)
anova(mod.jrgce_sens)
summary(mod.jrgce_sens)

# PHACE
mod.phace_sens <- lm(sensitivity ~ year, data = CTI_sens_phace)
anova(mod.phace_sens)
summary(mod.phace_sens)

# B4Warmed
mod.b4_cfc_sens_high <- lm(sensitivity_high_temp ~ year, data = CTI_sens_b4_cfc)
anova(mod.b4_cfc_sens_high)
summary(mod.b4_cfc_sens_high)

mod.b4_cfc_sens_med <- lm(sensitivity_med_temp ~ year, data = CTI_sens_b4_cfc)
anova(mod.b4_cfc_sens_med)
summary(mod.b4_cfc_sens_med)

mod.b4_hwrc_sens_high <- lm(sensitivity_high_temp ~ year, data = CTI_sens_b4_hwrc)
anova(mod.b4_hwrc_sens_high)
summary(mod.b4_hwrc_sens_high)

mod.b4_hwrc_sens_med <- lm(sensitivity_med_temp ~ year, data = CTI_sens_b4_hwrc)
anova(mod.b4_hwrc_sens_med)
summary(mod.b4_hwrc_sens_med)

# OK
mod.ok_sens <- lm(sensitivity ~ year, data = CTI_sens_ok)
anova(mod.ok_sens)
summary(mod.ok_sens)



### Traits vs contribution
trait_con <- trait_con %>%
  filter(mean_trait_val > 0) %>%
  mutate(log_mean_trait_val = log(mean_trait_val))
# Initialize a list to store models
models_list <- list()

# Loop through each unique TraitID (or use TraitName if preferred)
for (trait in unique(trait_con$TraitName)) {
  
  # Subset data for the specific TraitID
  trait_data <- trait_con %>% filter(TraitName == trait)
  
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

### Plot
# Create an empty list to store prediction data with confidence intervals
predictions_list <- list()

# Loop through each model to generate predictions and confidence intervals
for (trait in names(models_list)) {
  
  mod <- models_list[[trait]]
  trait_data <- trait_con %>% filter(TraitName == trait)  # Get original data
  
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

## Plot with confidence intervals
# Specify order for facet wrap
ordered_traits <- c("Seed dry mass","Seed length","Seed terminal velocity","Seed number per plant",
                    "Seed germination rate","Seed longevity","SLA","LDMC",
                    "Leaf thickness","Stem diameter","Plant height vegetative","Specific root length",
                    "Specific fine root length","Fine root length")

trait_con <- trait_con %>%
  mutate(TraitName = factor(TraitName, levels = ordered_traits))
levels(trait_con$TraitName)

png("traits_contributions.png", units="in", width=9, height=8, res=300)
ggplot(trait_con, aes(x = log_mean_trait_val, y = contribution_center)) +
  geom_point(alpha = 0.4) +  # Raw data points
  geom_ribbon(data = predictions_df, aes(y = predicted, ymin = lower, ymax = upper), alpha = 0.6, fill = "grey") +  # Confidence interval
  geom_line(data = predictions_df, aes(y = predicted), color = "blue", size = 1) +  # Predicted line
  facet_wrap(~factor(TraitName), scales = "free") +  # Separate plots for each trait
  labs(x = "Log(Mean trait value)", y = "Species contribution") +
  theme_bw() +
  theme(panel.background = element_blank(),
        #panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold"))
dev.off()


