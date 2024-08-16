#This script constructs linear mixed effects models on seagrass parameters and post-hoc contrasts

# Required libraries
library(tidyverse)
library(emmeans)
library(nlme)
library(car)

grazing <- read.csv("grazingplot.csv", header = TRUE)

# Gather timepoints
grazing_long <- grazing %>% 
  
  pivot_longer(cols = t1:t3, names_to = "time") %>%
  
  # Convert to factors
  mutate_at(vars(waypoint:time), .funs = as.factor)

####

# Subset by variable and model repeated measures ANOVA
aov_list <- lapply(unique(grazing_long$variable), function(i) {
  
  x <- filter(grazing_long, variable == i)
  
  model <- lme(value ~ treatment * location,
               random = ~ 1 | time / waypoint,
               # here is where we specify structure of repeated measures
               correlation = corAR1(form = ~ 1 | time / waypoint),
               data = x)
  
} )

names(aov_list) <- unique(grazing_long$variable)

# Check assumptions
# Normality of errors
lapply(aov_list, function(i) hist(resid(i))) # all pretty normal
# Homogeneity of variance
lapply(aov_list, plot) # also fine

# Get output tables
lapply(aov_list, Anova)

# Post-hoc contrasts for interaction for 1st model
contr <- emmeans(aov_list[[1]], ~ location:treatment)

contrast(contr, list("BH control vs. exclusion" = c(1,0,-1,0),
                     "SF control vs. exclusion" = c(0,1,0,-1)))

# Post-hoc contrasts for location for last 3 models
lapply(aov_list[-1], function(i) emmeans(i, pairwise ~ location)$contrasts)

####

# Run with just last time point
aov_t3_list <- lapply(unique(grazing_long$variable), function(i) {
  
  x <- filter(grazing_long, variable == i & time == "t3")
  
  model <- lme(value ~ treatment * location,
               random = ~ 1 |  waypoint,
               data = x)
  
} )

names(aov_t3_list) <- unique(grazing_long$variable)

# Check assumptions
# Normality of errors
lapply(aov_t3_list, function(i) hist(resid(i))) # all pretty normal
# Homogeneity of variance
lapply(aov_t3_list, plot) # also fine

# Get output tables
lapply(aov_t3_list, Anova)

# Post-hoc contrasts for ssdensity
contr_t3 <- emmeans(aov_t3_list[[4]], ~ treatment)

contrast(contr_t3, list("Control vs. exclusion" = c(1, -1))) # density higher in control at end of experiment


lapply(aov_t3_list[-1], function(i) emmeans(i, pairwise ~ location)$contrasts)

####

# Run with time interaction
grazing_long <- mutate(grazing_long, time2 = ifelse(time == "t2", 30, ifelse(time == "t3", 90, 0)))

# Run with just last time point
aov_time_list <- lapply(unique(grazing_long$variable), function(i) {
  
  x <- filter(grazing_long, variable == i)
  
  model <- lme(value ~ treatment * location * log10(time2+1),
               random = ~ 1 |  waypoint,
               data = x)
  
} )

names(aov_time_list) <- unique(grazing_long$variable)

# Check assumptions
# Normality of errors
lapply(aov_time_list, function(i) hist(resid(i))) # all pretty normal
# Homogeneity of variance
lapply(aov_time_list, plot) # also fine, maybe a little wedging for ssdensity but I'll allow it

# Get output tables
lapply(aov_time_list, Anova)

# Examine significant treatment*location interaction for length
contr_time_int <- emmeans(aov_time_list[[1]], ~ treatment:location)

contrast(contr_time_int, list("BH control vs. exclusion" = c(1,-1,0,0),
                     "SF control vs. exclusion" = c(0,0,1,-1))) # not significant ...
