# ---------------
# Title: Grooming reciprocity
# Date: 12 Nov 2024
# Author: mgranellruiz and Josefien Tankink
# Goal: Do the model and maybe also the visualization of the grooming recirpocity between M and F
# ---------------

# library ---------------------
# data manipulation
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
source('/Users/mariagranell/Repositories/data/functions.R')
# plotting
library(patchwork)
library(ggplot2)
library(ggside)
library(ggpubr)
library(gridExtra)
library(ggtext)
# models
library(lme4)
library(ggstatsplot)
library(fitdistrplus)
library(gamlss)
library(DHARMa)
library(effects)
library(emmeans)
library(car)
library(lattice)
library(influence.ME)
library(segmented)

# path ------------------------
setwd()

# data ------------------------
# data ------------------------
# Grooming duration data. I.e. focal data
# the focal data was recorded on the perpective ont he males, bgr (grooming done by the female, i.e. beeing groomed), gr (grooming done by the male)
data <- read.csv("/Users/mariagranell/Repositories/RTS/Publication/Rscripts/establishment_grooming_exchanges/Data/Focal_per_male_total.csv") %>%
  #read.csv("C:/Users/josef/Documents/PhD/IVP DATA/Raising the stakes/Focal_per_male_total.csv") %>%
  distinct()  %>% 
  mutate(Date = as.Date(Date)) %>% 
  filter(Habituation != "UH") # remove males that are Unhabituated

focal <- data %>%
  group_by(MaleID, FemaleID, Date, Day, Group, Status) %>%
  summarise(
    # duration
    Duration_bgr = sum(Duration_bgr, na.rm = TRUE),
    Duration_gr = sum(Duration_gr, na.rm = TRUE),
    # frequency
    n_female_grooms = sum(Duration_bgr > 0, na.rm = TRUE),
    n_male_grooms   = sum(Duration_gr > 0, na.rm = TRUE),
  ) %>%
  mutate(
    Reciprocity_dur = (Duration_gr - Duration_bgr) / (Duration_gr + Duration_bgr),
    Reciprocity_freq = (n_male_grooms - n_female_grooms) /
                            (n_male_grooms + n_female_grooms)
  ) %>%
  dplyr::select(MaleID, FemaleID, Date, Day, Reciprocity_dur, Group, Reciprocity_freq) %>%
  mutate(month = month(Date),
         Season = case_when(
                month < 4  ~ "Summer",  # Months 1, 2, 3 → Summer
                month < 7  ~ "Mating",  # Months 4, 5, 6 → Mating
                month < 10 ~ "Winter",  # Months 7, 8, 9 → Winter
                TRUE       ~ "Baby" ),  # Months 10, 11, 12 → Baby (default case)
         Season = factor(Season,
                         levels = c("Baby", "Summer", "Mating", "Winter"),
                         ordered = T))

# visual inspections shows a change in grooming reciprocity
ggplot(focal, aes(x = Day, y = Reciprocity_dur)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(focal, aes(x = Day, y = Reciprocity_freq)) +
  geom_point() +
  geom_smooth(method = "loess")

# both types of measurning reciprocity are highly correlated, thus we will proceed with duration
cor.test(~ Reciprocity_dur + Reciprocity_freq, data = focal, method = "pearson", use = "complete.obs") # cor 0.94

## breakpoint reciprocity ----
# we converted the data into binomial, 1 or 0.
reciprocity <- focal %>%
  filter(Reciprocity_dur != 0) %>%
  mutate(Binom = ifelse(Reciprocity_dur >= 0, 1, 0)) %>%
  filter(Day < 365)

ggplot(reciprocity, aes(x = Day, y = Binom)) +
  geom_point() +
  geom_smooth(method = "loess")

hist(reciprocity$Reciprocity, breaks = 50)
hist(reciprocity$Binom, breaks = 50)

# Fit a binomial GLM without random effects (simpler model)
model_glm <- glm(Binom ~ Day + Season + Group, family = binomial, data = reciprocity)
# Add breakpoints to a predictor (e.g., DayStatus)
segmented_model <- segmented(model_glm, seg.Z = ~Day)

summary(segmented_model)
bgr_breakpoint <- ceiling(segmented_model$psi["psi1.Day", "Est."]) # 171 days
plot(segmented_model)

# modelling -------
# we determined the categories of Immigrant and Resident males based on the bgr_bgreakpoint

dd <- reciprocity %>%
   mutate(
     Status = case_when(
       Day <= bgr_breakpoint ~ "Immigrant",                     # Immigrants definition: from day 1 to breakpoint
       Day >= 365 & Day <= (bgr_breakpoint + 365) ~ "Resident", # Residents definition: from one year, to one year + breakpoint
       TRUE ~ NA),
     Status = as.factor(Status),
     DayStatus= ifelse(Status == "Resident", Day - 365, Day)) %>%
  filter(Day <= 365 + bgr_breakpoint, !is.na(Status))


# Rescale day and daystatus
dd$DayStatus_rescale <- as.numeric(scale(dd$DayStatus))

str(dd)

# Model selection -----
model1 <- glmer(Binom ~ DayStatus_rescale * Status + Season + Group + (1|MaleID) , family = binomial, data = dd,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)))

model2 <- glmer(Binom ~ DayStatus_rescale * Status + (1|MaleID), family = binomial, data = dd,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)))

anova(model1, model2) # Model 2 has a lower AIC

# Model -------
model <- glmer(Binom ~ DayStatus_rescale * Status + (1|MaleID), family = binomial, data = dd,
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl = list(maxfun = 2e5)))
summary(model)
Anova(model)
test(emtrends(model, pairwise ~ Status, var="DayStatus_rescale"))
# Immigrant - Resident (estimate =  0.63, SE = 0.35, p = 0.07)
# Immigrants           (estimate =  0.62, SE = 0.30, p = 0.04)
# Residents            (estimate = -0.01, SE = 0.18, p = 0.93)
plot(allEffects(model))

# Based on manual and DHARMa testing: We´re happy with this model and trust it's results.
## Test model assumptions manually --------

# Homoscedacity tests
# Extract residuals
residuals_nb <- residuals(model, type = "pearson")

leveneTest(residuals_nb ~ cut(dd$DayStatus_rescale, 4)) # Good!
leveneTest(residuals_nb ~ as.factor(dd$Status)) # Good!
## No heteroscedacity, all good

# Overdispersion
# Pearson residuals and Chi-squared test
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  ratio <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = Pearson.chisq/rdf, rdf = rdf, p = pval)
}
overdisp_fun(model) # looks good - no overdispersion

# Pearson residuals
residuals_pearson <- residuals(model, type = "pearson")
fitted_values <- fitted(model)

# Plot residuals vs. fitted values
plot(fitted_values, residuals_pearson, 
     main = "Pearson Residuals vs. Fitted Values", 
     xlab = "Fitted Values", 
     ylab = "Pearson Residuals")
abline(h = 0, col = "red")
plot(fitted(model), residuals(model, type = "pearson"), col = as.factor(dd$Season))
plot(fitted(model), residuals(model, type = "pearson"), col = as.factor(dd$Group))
# Season or group does not explain the clustering. DHARMa test do not show any signs of concern
# thus we continue to accept the fit of the model (fitted vs residuals do not include random effects)

# Certain datapoints that influence the data more than others?
infl <- influence(model, obs = TRUE)  # Observation-level influence
cooks <- cooks.distance(infl)
# Plot Cook's Distance
plot(cooks, type = "h", main = "Cook's Distance for Influential Observations") # One peak but still really low

# Variance Inflation Factor
vif(model) # Good

## DHARMa: ------------ 
# Model assumptions:
  simulationOutput <- simulateResiduals(fittedModel = model, plot = T)
  # 1) Overdispersion
  testDispersion(model)
  # 2) Normality of random effects
  random_residuals <- simulateResiduals(fittedModel = model, re.form = ~ (1|MaleID))
  plot(random_residuals) 
  # 3) Time autocorrelation
  aggregatedResiduals <- recalculateResiduals(simulationOutput, group = dd$DayStatus_rescale)
  testTemporalAutocorrelation(aggregatedResiduals, time = unique(dd$DayStatus_rescale))
  # 4) Test uniformity (whether residuals are uniformly distributed)
  testUniformity(simulationOutput) 
  # 5) Check for outliers
  testOutliers(simulationOutput)
  # 6) Test for residual patterns (temporal, spatial and other)
  testResiduals(simulationOutput)
  # Slightly non-normal random effects

## Summary data: ----
summary(dd)
# Reciprocity mean -0.10 median -0.23
  
## Plot: -----

# auxiliar dataframe
dd_year <- focal %>%
  filter(Reciprocity != 0) %>%
  mutate(Binom = ifelse(Reciprocity >= 0, 1, 0)) %>%
  filter(Day < 365) %>%
  mutate(Status = case_when(
    Day <= bgr_breakpoint ~ "Immigrant",
    TRUE ~ NA
  ))

# plot
plot_reciprocity <- ggplot(dd_year, aes(x = Day, y = Binom)) +
  # main trend
  geom_smooth(method = "loess", color = "#1b6a6a", fill = "gray70", size = 1.2) +
  # Regression line for reciprocity between females and immigrants
  geom_smooth(data = subset(dd_year, Status == "Immigrant"), method = 'lm', se = TRUE, size = 1.2,
              color = alpha("black", 0.5), fill = alpha("gray70", 0.3)) +
  # Horizontal reference line at 0.5 (equal grooming)
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "black") +

  # Vertical red dashed line at Day 171
  geom_vline(xintercept = 171, linetype = "dashed", color = "red", linewidth = 1) +
    
  # Comments
  annotate("text", x = 0, y = 1, label = "Male grooms more", size = 4.5, hjust = 0) +
  annotate("text", x = 0, y = 0.53, label = "Equivalent amount of grooming", size = 4.5, hjust = 0) +
  annotate("text", x = 0, y = 0.0, label = "Female grooms more", size = 4.5, hjust = 0) +
  annotate("text", x = 85, y = 0.38, label = "*", size = 7, color = "black") + # Significant difference

  labs(
      x = "Number of days a male has been in a group",
      y = "Grooming reciprocation"
  ) +
  theme_classic()

plot_reciprocity

ggsave("/Users/mariagranell/Repositories/RTS/Publication/Rscripts/PublishRscripts/Graphs/plot_reciprocity.png",
       plot_reciprocity,
       width = 23, height = 14, units = "cm", dpi = 600)
  