# ---------------
# Title: Grooming duration across
# Date: 20 Nov 2024
# Author: mgranellruiz and Josefien Tankink
# Goal: Is grooming duration different in how much females groom immigrant vs, how much females groom residents?
# sometimes groom is called gr in the comments.
# ---------------

# library ---------------------
# data manipulation
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
source('/Users/mariagranell/Repositories/data/functions.R')
# plotting
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
library(car)
library(lattice)
library(effects)
library(emmeans)
library(performance)

# path ------------------------
setwd("/Users/mariagranell/Repositories/RTS/Publication/Rscripts/PublishRscripts/Rscripts")

# data ------------------------
#data <- read.csv("C:/Users/josef/Documents/PhD/Raising the stakes/R scripts/MR/Final/GroomingDurationStruchange.csv") %>%
data <- read.csv("/Users/mariagranell/Repositories/RTS/Publication/Rscripts/establishment_grooming_exchanges/Data/GroomingDurationStruchange.csv") %>%
  mutate(Date = as.Date(Date)) %>% 
  filter(Habituation != "UH") # Remove unhabituated individuals

hist(data$Date, breaks = "weeks")

#### Select only the data with only Immigrant and Resident male periods.
dd <- data %>% filter(Status %in% c("Immigrant", "Resident"))

## Summary of data: --------------------------
nrow(dd) # 405 total number of grooming interactions
table(dd$Status) # 112 with Immigrants and 293 with Residents.

length(unique(dd$MaleID)) # 30 unique males
length(unique(dd$FemaleID)) # 73 unique females

# Immigrants
ddimm <- data %>% filter(Status == "Immigrant")
length(unique(ddimm$MaleID)) # 18 unique immigrant males
length(unique(ddimm$FemaleID)) # 39 unique females interacting with immigrant males
table(ddimm$Groomer)
# 61 times Immigrants gr Females
# 51 times Females groomed Immigrants
summary(subset(ddimm, ddimm$Groomer == "Male")) # Immigrants duration mean 93.06
summary(subset(ddimm, ddimm$Groomer == "Female")) # Females duration mean 158.6

# Residents
ddres <- data %>% filter(Status == "Resident")
length(unique(ddres$MaleID)) # 22 unique resident males
length(unique(ddres$FemaleID)) # 61 unique females interacting with resident males
table(ddres$Groomer)
# 142 times Residents gr Females
# 151 times Females groomed Residents
summary(subset(ddres, ddres$Groomer == "Male")) # Residents duration mean 137.42
summary(subset(ddres, ddres$Groomer == "Female")) # Females duration mean 157.7

# Females and Males interacting during the Immigrant status and the resident status
common_males <- intersect(unique(ddres$MaleID), unique(ddimm$MaleID))
length(unique(common_males)) # 10 males
common_females <- intersect(unique(ddres$FemaleID), unique(ddimm$FemaleID))
length(unique(common_females)) # 27 females

### modelling -----------------------------
hist(dd$Duration)
plot(fitdist(dd$Duration, "nbinom"))

# rescale DayStatus to allow for model convergence
hist(dd$DayStatus)
dd$DayStatus_rescale <- as.numeric(scale(dd$DayStatus))
hist(dd$DayStatus_rescale) # No clear outliers in DayStatus, maybe around -2?
#dd <- dd %>% filter(MaleID != "Buk") testing for outliers, no change in results (see below)

model <- glmer.nb(Duration ~ DayStatus_rescale * Groomer * Status + Season + Group +
                 (1|MaleID) + (1|FemaleID) + (1|Date), 
                 dd,
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
# for some computers an optimizer is needed, but results do not change
               
summary(model)
Anova(model)

plot(allEffects(model))
plot(effect("Groomer:Status", model))
plot(effect("DayStatus_rescale:Status", model))
plot(effect("DayStatus_rescale:Groomer:Status", model))

pairs(emmeans(model, ~ Groomer * Status))
# Females groom Immigrants more than viceversa (estimate = 0.6097, SE = 0.179, p = 0.0036)
# Immigrants groom Females less than Females groom residets (estimate = -0.5604, SE = 0.175, p = 0.0074)
test(emtrends(model, pairwise ~ Status | Groomer, var="DayStatus_rescale"))
# Females groom Immigrants more than Residents over time (estimate = -0.431, SE = 0.144, p = 0.0027)
# Females reduce gr to immigrants over time (estimate = -0.3827, SE = 0.1208, p = 0.0015)
test(emtrends(model, pairwise ~ Groomer | Status, var="DayStatus_rescale"))
# Marginally non-sign difference between Fem - imm and imm - fem: p = 0.08
# Both: significantly negative slope for fem - imm grooming, rest is n.s.

# Model assuptions ------------

# Based on manual and DHARMa testing: We are happy with this model and trust it's results.
# DHARMa detects slight deviations in homoscedasticity (residuals vs predictors), but Levene’s test shows no significant issues.
# One potential outlier identified in MaleID: "Buk"; removing it does not alter results. influence() and Cook's distance indicate minimal impact, so we keep it.

## Test model assumptions manually --------

# Homoscedacity tests
# Extract residuals
residuals_nb <- residuals(model, type = "pearson")

leveneTest(residuals_nb ~ cut(dd$DayStatus_rescale, 4)) # Good!
leveneTest(residuals_nb ~ as.factor(dd$Groomer)) # Good!
leveneTest(residuals_nb ~ as.factor(dd$Status)) # Good!
leveneTest(residuals_nb ~ as.factor(dd$Season)) # Good!
leveneTest(residuals_nb ~ as.factor(dd$Group)) # Good!

# White like test
residuals_nb <- residuals(model, type = "pearson")
fitted_values <- fitted(model)
# Create squared residuals model
white_test_model <- lm(residuals_nb^2 ~ fitted_values + I(fitted_values^2))
summary(white_test_model)
# also looks good

# Extract residuals
raw_residuals <- residuals(model, type = "response")
pearson_residuals <- residuals(model, type = "pearson")

# Plot residuals
plot(fitted(model), raw_residuals, main = "Raw Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Raw Residuals")
abline(h = 0, col = "red") # Doesen´t not the best but the Levene and White tests come out good

plot(fitted(model), pearson_residuals, main = "Pearson Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Pearson Residuals")
abline(h = 0, col = "red")

# Plot residuals against Day
plot(dd$DayStatus_rescale, residuals_nb,
     main = "Residuals vs. Day",
     xlab = "Day",
     ylab = "Pearson Residuals")
abline(h = 0, col = "red") # Looks good
## No heteroscedacity, all good

# Overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  pval <- pchisq(Pearson.chisq, rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = Pearson.chisq/rdf, rdf = rdf, p = pval)
}
overdisp_fun(model) # looks good - no overdispersion

# Zero inflation
check_zeroinflation(model) # no zeroes so no zero inflation

# Do certain datapoints that influence the data more than others?
influence <- influence(model, group = "MaleID")
plot(influence, which = "cook") # Buk most but some others also quite like Vla, Oke and Fur. So I think that's fine
# Cook's distance below .5, so all good

influence <- influence(model, group = "FemaleID")
plot(influence, which = "cook") # Two females (Timb and Aara) stand out, others are close too. Looks fine
# Cook's distance below .5, so all good

influence <- influence(model, group = "Date")
plot(influence, which = "cook")
# Highest is 2022-07-14, two interactions with Apa-Dore and Ted-Lanc.
# Cook's distance is still very low (0.088), so all good

## DHARMa: -------------

# Model assumptions:
simulationOutput <- simulateResiduals(fittedModel = model, plot = T)
# 1) Overdispersion. no need for binomial models
testDispersion(model)
# 2) Plot, looking for HOMOSCEDASTICITY and outliers
qqnorm(resid(model))  # residuals are NOT supposed to be normal, look only for outliers
qqp(resid(model))  # residuals are NOT supposed to be normal, look only for outliers
plot(model)
# 3) HOMOSCEDASTICITY, RESIDUALS AGAINS PREDICTORS
plotResiduals(simulationOutput, dd$DayStatus_rescale) # Slight heteroscedasticity here, however, Levene test was good enough.
plotResiduals(simulationOutput, dd$Status) # good
plotResiduals(simulationOutput, dd$Groomer) # good
plotResiduals(simulationOutput, dd$Group) # good enough
plotResiduals(simulationOutput, dd$Season) # good
# 4) Normality of random effects
random_residuals <- simulateResiduals(fittedModel = model, re.form = ~ (1|MaleID))
plot(random_residuals) # Not great
# the outlier tedt is because of BUk
random_residuals <- simulateResiduals(fittedModel = model, re.form = ~ (1|FemaleID))
plot(random_residuals) 
random_residuals <- simulateResiduals(fittedModel = model, re.form = ~ (1|Date))
plot(random_residuals) 

# 5) Time autocorrelation
aggregatedResiduals <- recalculateResiduals(simulationOutput, group = dd$DayStatus_rescale)
plot(aggregatedResiduals)

testOutliers(simulationOutput) # outlier test not significant


## Plot: -----

# Define colors for grooming categories
{Female <- "#a1a09a"        # light grey
  Male <- "#5a5f63"          # dark grey
  Residents <- "#008000"     # Deep green
  Immigrants <- "#FFD700"    # Bright yellow
  
  # Combinations
  FemaleResidents <- "#FF8C00"   # Energetic orange
  FemaleImmigrants <- "#FF4500"  # Fiery red
  MaleResidents <- "#4682B4"     # Cool steel blue
  MaleImmigrants <- "#8B008B"    # Rich purple
}

# Set breakpoint for immigrants vs residents
bgr_breakpoint <- 171 

# Create the plot
dd_plot <- data %>% filter(Day < 730) # cutoff of 2 years for readability
plot_interactions <- ggplot(dd_plot, aes(x = Day, y = Duration)) +

  # Grooming trends for females and males (loess smoothing for clarity)
  geom_smooth(data = subset(dd_plot, Groomer == "Female"), method = 'loess',
              aes(linetype = Groomer), se = FALSE, size = 1.5, color = "#a1a09a") + # Light grey (Female)
  
  geom_smooth(data = subset(dd_plot, Groomer == "Male"), method = 'loess',
              aes(linetype = Groomer), se = FALSE, size = 1.5, color = "#5a5f63") + # Dark grey (Male)
  
  # Regression lines for different categories (with shading for confidence intervals)
  geom_smooth(data = subset(dd_plot, Groomer == "Female" & Status == "Immigrant"),
              aes(color = "Females to Immigrants"), method = 'lm', se = TRUE, size = 2) +
  
  geom_smooth(data = subset(dd_plot, Groomer == "Female" & Status == "Resident"),
              aes(color = "Females to Residents"), method = 'lm', se = TRUE, size = 2) +
  
  geom_smooth(data = subset(dd_plot, Groomer == "Male" & Status == "Immigrant"),
              aes(color = "Immigrants to Females"), method = 'lm', se = TRUE, size = 2) +
  
  geom_smooth(data = subset(dd_plot, Groomer == "Male" & Status == "Resident"),
              aes(color = "Residents to Females"), method = 'lm', se = TRUE, size = 2) +
  
  # Color scales for different grooming interactions
  scale_color_manual(values = c("Females to Immigrants" = FemaleImmigrants,
                                "Females to Residents" = FemaleResidents,
                                "Immigrants to Females" = MaleImmigrants,
                                "Residents to Females" = MaleResidents),
                     name = "Who grooms whom?") +
  
  # Linetype scale for grooming by sex
  scale_linetype_manual(values = c("Female" = "dotted", "Male" = "dashed"),
                        name = "Grooming by") +
  
  # Immigrant and resident group definitions
  annotate("segment", x = 3.5, y = -3.5, xend = bgr_breakpoint, yend = -3.5, size = 1, colour = "black") +
  annotate("segment", x = 365, y = -3.5, xend = bgr_breakpoint + 365, yend = -3.5, size = 1, colour = "black") +
  annotate("text", x = 85, y = -15, label = "Immigrants", size = 3.5, color = "black") +
  annotate("text", x = 450, y = -15, label = "Residents", size = 3.5, color = "black") +
  
  # Significance annotations
  annotate("segment", x = 85, y = 350, xend = 450, yend = 350, size = 0.3, colour = "black") +
  annotate("text", x = 267.5, y = 355, label = "**", size = 7, color = "black") + # Significant difference
  
  annotate("segment", x = -5, y = 115, xend = -5, yend = 270, size = 0.3, colour = "black") +
  annotate("text", x = -15, y = 195, label = "•", size = 4.5, color = "black") + 
  
  annotate("segment", x = 85, y = 50, xend = 450, yend = 50, size = 0.3, colour = "#4a4a4a") +
  annotate("text", x = 267.5, y = 45, label = "n.s.", size = 3.5, color = "#4a4a4a") + # ns
  
  annotate("segment", x = 551, y = 150, xend = 551, yend = 180, size = 0.3, colour = "#4a4a4a") +
  annotate("text", x = 571, y = 163, label = "n.s.", size = 3.5, color = "#4a4a4a") + # ns
  
  annotate("text", x = 115, y = 200, label = "**", size = 7, color = "black") + # Female to Immigrants significance
  annotate("text", x = 90, y = 80, label = "n.s.", size = 3.5, color = "#4a4a4a") + # ns
  annotate("text", x = 450, y = 120, label = "n.s.", size = 3.5, color = "#4a4a4a") + # ns
  annotate("text", x = 450, y = 175, label = "n.s.", size = 3.5, color = "#4a4a4a") + # ns

  # Define X-axis breakpoints
  scale_x_continuous(breaks = c(0, bgr_breakpoint, 365, (bgr_breakpoint + 365), 730)) +
  
  # Labels and themes
  labs(x = "Number of days a male has been in a group",
       y = "Grooming duration (s)") +
  theme_classic() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 14))

plot_interactions

ggsave("/Users/mariagranell/Repositories/RTS/Publication/Rscripts/PublishRscripts/Graphs/plot_interactions.png",
       plot_interactions,
       width = 23, height = 14, units = "cm", dpi = 600)
