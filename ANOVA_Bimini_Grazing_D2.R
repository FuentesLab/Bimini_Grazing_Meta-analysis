#This script constructs linear mixed effects models on seagrass parameters 
#and post-hoc contrasts


# Required libraries
library(tidyverse)
library(emmeans)
library(nlme)
library(car)

# Read in data
grazing_raw <- read.csv("biminigrazing_plot_summary.csv", header = TRUE)

# Convert to factors
grazing <- grazing_raw %>%
  mutate(
  waypoint = as.factor(waypoint),
  location = as.factor(location),
  treatment = as.factor(treatment),
  variable = as.factor(variable)
)

# Check structure
str(grazing)

# Run with just last time point adding starting value as covariate

# Subset by variable and run model 
# Note: t1 refers to the starting value (stated t0 in manuscript text)
aov_t3_list <- lapply(unique(grazing$variable), function(i) {
  
  x <- filter(grazing, variable == i )
  
  model <- lme(t3 ~ treatment * location + t1,
               random = ~ 1 |  waypoint,
               data = x)
  
} )

names(aov_t3_list) <- unique(grazing$variable)

# Check assumptions
# Normality of errors
lapply(aov_t3_list, function(i) hist(resid(i))) # all pretty normal

# Homogeneity of variance
lapply(aov_t3_list, plot) # also fine

# Get output tables
lapply(aov_t3_list, Anova)

#Post-hoc contrasts for width
contrwidth_t3 <- emmeans(aov_t3_list[[2]], pairwise ~ treatment | location)

# Post-hoc contrasts for ssdensity
contr_t3 <- emmeans(aov_t3_list[[4]], ~ treatment)

contrast(contr_t3, list("Control vs. exclusion" = c(1, -1))) # density higher in control at end of experiment

# Location differences
lapply(aov_t3_list, function(i) emmeans(i, pairwise ~ location)$contrasts)

