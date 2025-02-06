# ---------------
# Title: Grooming Duration Data cleaning
# Date: 10 Nov 2024
# Author: mgranellruiz and Josefien Tankink
# Goal: reshape the data to create the Immigrant and Resident categories based on the break point model.
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

# path ------------------------
setwd("/Users/mariagranell/Repositories/RTS/Publication/Rscripts/Struchange_analysis/")

# data ------------------------
# Grooming duration data. I.e. focal data
# the focal data was recorded on the perpective ont he males, bgr (grooming done by the female, i.e. beeing groomed), gr (grooming done by the male)
data <- read.csv("/Users/mariagranell/Repositories/RTS/Publication/Rscripts/establishment_grooming_exchanges/Data/Focal_per_male_total.csv") %>%
  #read.csv("C:/Users/josef/Documents/PhD/IVP DATA/Raising the stakes/Focal_per_male_total.csv") %>%
  filter(Habituation != "UH") %>%  # remove males that are Unhabituated
  distinct() %>%
  # PIVOT LONGER
  pivot_longer(cols = c("Duration_bgr", "Duration_gr"), names_to = "Groomer", values_to = "Duration") %>%
    mutate(Groomer = case_when(
    Groomer == "Duration_bgr" ~"Female",
    Groomer == "Duration_gr" ~ "Male"),
  Groomer_ID = case_when(
    Groomer == "Female" ~ FemaleID,
    TRUE ~ MaleID
  ))

hist(data$Duration, breaks = 100)

data <- data %>%
  filter(Duration < 900, Duration > 5) %>% # because of the nature of our data, we can have really small and large grooming interactions.
  # We decided to keep interactions longer than 5s, since they are a relevant time and not just a lapse of time when collecting data
  # we also removed outliers by filtering the data on an upper threashold of 900
  mutate(dyad = paste0(MaleID, FemaleID),
         month = month(ymd(Date)),
         Season = case_when(
                month < 4  ~ "Summer",  # Months 1, 2, 3 → Summer
                month < 7  ~ "Mating",  # Months 4, 5, 6 → Mating
                month < 10 ~ "Winter",  # Months 7, 8, 9 → Winter
                TRUE       ~ "Baby" ),  # Months 10, 11, 12 → Baby (default case)
         Season = factor(Season,
                         levels = c("Baby", "Summer", "Mating", "Winter"),
                         ordered = T))

hist(data$Duration, breaks = 100)

ggplot(data, aes(x = Day, y = Duration, colour = Groomer)) +
  geom_point() +
  geom_smooth(method = "loess")

# Breakpoint taken from reciprocation in the first year. See "1_Grooming_reciprocity_breakpoint.R"
bgr_breakpoint <- 171

dd_break <- data %>%
   mutate(
     Status = case_when(
       Day <= bgr_breakpoint ~ "Immigrant",                     # Immigrants definition: from day 1 to breakpoint
       Day > bgr_breakpoint & Day < 365 ~ "Intermediate",
       Day >= 365 & Day <= (365 + bgr_breakpoint) ~ "Resident", # Residents definition: from one year, to one year + breakpoint
       Day > (365 + bgr_breakpoint) ~ "LongResident"
     ), Status = as.factor(Status),
     MaleID = as.factor(MaleID), FemaleID = as.factor(FemaleID),
     DayStatus = case_when(
       Status == "Immigrant" ~ Day,
       Status == "Intermediate" ~ Day - bgr_breakpoint,
       Status == "Resident" ~ Day - 365,
       Status == "LongResident" ~ Day - 365 - bgr_breakpoint 
     )
       )


table(dd_break$Status)
unique(dd_break$MaleID)

write.csv(dd_break, "/Users/mariagranell/Repositories/RTS/Publication/Rscripts/PublishRscripts/Data/GroomingDurationStruchange.csv", row.names = F)
