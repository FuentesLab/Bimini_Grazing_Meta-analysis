#This script creates Figure 4

library(tidyverse)
library(ggplot2)
library(dplyr)
library(cowplot)
library(viridis)




data <- tribble(
  ~Location, ~Parameter, ~Treatment, ~Time, ~Mean, ~SD,
  "Bonefish Hole", "Blade length (cm)", "Grazed", "July", 15.23, 7.23,
  "Bonefish Hole", "Blade length (cm)", "Grazed", "August", 16.58, 7.81,
  "Bonefish Hole", "Blade length (cm)", "Grazed", "October", 15.12, 7.51,
  "Bonefish Hole", "Blade length (cm)", "Exclusion", "July", 19.77, 8.32,
  "Bonefish Hole", "Blade length (cm)", "Exclusion", "August", 17.84, 7.75,
  "Bonefish Hole", "Blade length (cm)", "Exclusion", "October", 17.23, 7.40,
  "Bonefish Hole", "Blade width (cm)", "Grazed", "July", 0.81, 0.13,
  "Bonefish Hole", "Blade width (cm)", "Grazed", "August", 0.84, 0.17,
  "Bonefish Hole", "Blade width (cm)", "Grazed", "October", 0.93, 0.10,
  "Bonefish Hole", "Blade width (cm)", "Exclusion", "July", 0.91, 0.13,
  "Bonefish Hole", "Blade width (cm)", "Exclusion", "August", 0.86, 0.19,
  "Bonefish Hole", "Blade width (cm)", "Exclusion", "October", 0.90, 0.11,
  "Bonefish Hole", "Shoot density (shoots m-2)", "Grazed", "July", 172.0, 35.08,
  "Bonefish Hole", "Shoot density (shoots m-2)", "Grazed", "August", 151.60, 41.63,
  "Bonefish Hole", "Shoot density (shoots m-2)", "Grazed", "October", 176.00, 43.48,
  "Bonefish Hole", "Shoot density (shoots m-2)", "Exclusion", "July", 188.0, 36.91,
  "Bonefish Hole", "Shoot density (shoots m-2)", "Exclusion", "August", 155.6, 45.11,
  "Bonefish Hole", "Shoot density (shoots m-2)", "Exclusion", "October", 148.0, 54.92,
  "Bonefish Hole", "Shoot weight (mg)", "Grazed", "July", 246.85, 93.16,
  "Bonefish Hole", "Shoot weight (mg)", "Grazed", "August", 284.99, 135.07,
  "Bonefish Hole", "Shoot weight (mg)", "Grazed", "October", 611.15, 283.32,
  "Bonefish Hole", "Shoot weight (mg)", "Exclusion", "July", 291.11, 176.27,
  "Bonefish Hole", "Shoot weight (mg)", "Exclusion", "August", 243.58, 110.44,
  "Bonefish Hole", "Shoot weight (mg)", "Exclusion", "October", 585.49, 246.44,
  "South Flats", "Blade length (cm)", "Grazed", "July", 15.32, 5.92,
  "South Flats", "Blade length (cm)", "Grazed", "August", 16.82, 8.14,
  "South Flats", "Blade length (cm)", "Grazed", "October", 16.75, 8.40,
  "South Flats", "Blade length (cm)", "Exclusion", "July", 16.98, 7.76,
  "South Flats", "Blade length (cm)", "Exclusion", "August", 12.70, 6.10,
  "South Flats", "Blade length (cm)", "Exclusion", "October", 15.96, 7.65,
  "South Flats", "Blade width (cm)", "Grazed", "July", 0.71, 0.10,
  "South Flats", "Blade width (cm)", "Grazed", "August", 0.75, 0.12,
  "South Flats", "Blade width (cm)", "Grazed", "October", 0.79, 0.10,
  "South Flats", "Blade width (cm)", "Exclusion", "July", 0.72, 0.11,
  "South Flats", "Blade width (cm)", "Exclusion", "August", 0.65, 0.07,
  "South Flats", "Blade width (cm)", "Exclusion", "October", 0.84, 0.08,
  "South Flats", "Shoot density (shoots m-2)", "Grazed", "July", 420.0, 50.43,
  "South Flats", "Shoot density (shoots m-2)", "Grazed", "August", 358.0, 178.38,
  "South Flats", "Shoot density (shoots m-2)", "Grazed", "October", 448.8, 176.54,
  "South Flats", "Shoot density (shoots m-2)", "Exclusion", "July", 404.0, 124.66,
  "South Flats", "Shoot density (shoots m-2)", "Exclusion", "August", 370.0, 168.48,
  "South Flats", "Shoot density (shoots m-2)", "Exclusion", "October", 431.60, 196.38,
  "South Flats", "Shoot weight (mg)", "Grazed", "July", 241.90, 115.97,
  "South Flats", "Shoot weight (mg)", "Grazed", "August", 250.16, 146.65,
  "South Flats", "Shoot weight (mg)", "Grazed", "October", 371.47, 200.58,
  "South Flats", "Shoot weight (mg)", "Exclusion", "July", 240.81, 113.07,
  "South Flats", "Shoot weight (mg)", "Exclusion", "August", 129.87, 48.48,
  "South Flats", "Shoot weight (mg)", "Exclusion", "October", 361.36, 120.39
)



# Ensure the Time variable is a factor with specified levels
data <- data %>%
  mutate(Time = factor(Time, levels = c("July", "August", "October")))

y_scales <- list(
  "Blade length (cm)" = c(0, 30),
  "Blade width (cm)" = c(0, 1.25),
  "Shoot density (shoots m-2)" = c(0, 650),
  "Shoot weight (mg)" = c(0, 900)
)

create_plot <- function(data, parameter, location) {
  # Get the y-axis scale for the current parameter
  y_scale <- y_scales[[parameter]]
  parameter_label <- gsub("(.*)\\((.*)\\)", "\\1\n(\\2)", parameter)
  
  
  ggplot(data %>% filter(Parameter == parameter, Location == location), aes(x = factor(Treatment, levels = c("Grazed", "Exclusion")), y = Mean, fill = Time)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.7, size = 0.2) +
    geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, position = position_dodge(width = 0.7), size = 0.2) +
    scale_fill_viridis_d() +
    labs(title = NULL, x = NULL, y = parameter_label) +
    theme(
      plot.background = element_rect(fill = ifelse(data$Treatment == "Grazed", "white", "lightgrey")),
      panel.background = element_rect(fill = "white"),
      axis.line.x = element_line(color = "black", size = 0.2),
      axis.line.y = element_line(color = "black", size = 0.2),
      axis.ticks.y = element_line(size = 0.2),
      axis.text.x = element_blank(),  # Remove x-axis text
      axis.ticks.x = element_blank(),
      text = element_text(size = 8, family = "sans", color = "black"),
      axis.text = element_text(size = 8, family = "sans", color = "black"),
      axis.title.y = element_text(size = 8, family = "sans", color = "black", margin = margin(r = 8)),
    ) +
    guides(fill = FALSE) +
    scale_y_continuous(expand = c(0, 0), limits = y_scale) +
    scale_x_discrete(expand = c(0, 0))
}

parameters <- unique(data$Parameter)
locations <- unique(data$Location)

plots <- list()

# Fill the plots list with individual plot objects
for (param in parameters) {
  for (loc in locations) {
    plots[[paste(param, loc, sep = "_")]] <- create_plot(data, parameter = param, location = loc)
  }
}

# Plot grid
plot_grid_object <- plot_grid(plotlist = plots, nrow = length(parameters), ncol = length(locations))


ggsave("/Users/AlexaPutillo/Library/CloudStorage/OneDrive-FloridaStateUniversity/PhD/Projects/Bimini Grazing/Drafts/Submission documents 8.13.24/Figure 3.pdf", width = 12, height = 12, units = "cm")

###if want to add labels: geom_text(aes(label = round(Mean, 2)), vjust = -0.5, position = position_dodge(width = 0.7)) +  # Add values on top of bars




