#This script creates Figure S3 timeseries seagrass parameters

# Required libraries
library(tidyverse)
library(cowplot)
library(ggplot2)

# Load data
data <- read.csv("biminigrazing_plot_summary.csv", header = TRUE)

# Pivot the data to a long format
data_long <- data %>%
  pivot_longer(
    cols = c(t1, t2, t3),        # Specify only the time-point columns
    names_to = "time",           # New column for time (t1, t2, t3)
    values_to = "value"          # New column for values (numeric data)
  )

# Recode time for correct order and labels
data_long <- data_long %>%
  mutate(time = factor(time,
                       levels = c("t1", "t2", "t3"),
                       labels = c("Initiation", "1 Month", "3 Months")))

# Create a group variable for lines
data_long <- data_long %>%
  mutate(group = paste(location, treatment))

# Summarize: calculate mean and SD
summary_data <- data_long %>%
  group_by(variable, location, treatment, time, group) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

# Create a group label for cleaner legend
summary_data <- summary_data %>%
  mutate(group_label = case_when(
    location == "Bonefish Hole" & treatment == "Control" ~ "BH Control",
    location == "Bonefish Hole" & treatment == "Exclusion" ~ "BH Exclusion",
    location == "South Flats" & treatment == "Control" ~ "SF Control",
    location == "South Flats" & treatment == "Exclusion" ~ "SF Exclusion"
  ))

# Define y-axis labels
y_axis_labels <- c(
  "length" = "Length (cm)",
  "width" = "Width (cm)",
  "ssdensity" = "Shoot Density (shoots/m²)",
  "ssweight" = "Shoot Weight (mg)"
)

# Create individual time series plots
plots <- list()
response_vars <- unique(summary_data$variable)

for (resp in response_vars) {
  plot_data <- summary_data %>% filter(variable == resp)
  
  p <- ggplot(plot_data, aes(x = time, y = mean_value, group = group_label)) +
    geom_line(aes(color = group_label), linewidth = 1) +
    geom_point(aes(shape = group_label, color = group_label), size = 3) +
    geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value, color = group_label),
                  width = 0.2) +
    scale_color_manual(values = c(
      "BH Control" = "black",
      "BH Exclusion" = "black",
      "SF Control" = "grey50",
      "SF Exclusion" = "grey50"
    )) +
    scale_shape_manual(values = c(
      "BH Control" = 16,   # Filled circle
      "BH Exclusion" = 17, # Filled triangle
      "SF Control" = 16,   # Filled circle
      "SF Exclusion" = 17  # Filled triangle
    )) +
    labs(
      y = y_axis_labels[[resp]],
      x = "Sampling Period",
      color = "Site and Treatment",
      shape = "Site and Treatment"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, color = "black"),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 14, color = "black"),
      text = element_text(size = 14, color = "black"),
      strip.text = element_text(size = 14, face = "bold", color = "black"),
      legend.background = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 11, color = "black")
    )
  
  # Update y-axis for length
  if (resp == "length") {
    p <- p + scale_y_continuous(breaks = scales::breaks_width(2))
  }
  
  # If plotting Shoot Weight (ssweight), move legend inside top left
  if (resp == "ssweight") {
    p <- p + theme(
      legend.position = c(0.05, 0.99),
      legend.justification = c("left", "top"),
      legend.background = element_blank()
    )
  } else {
    # Remove legend from other panels
    p <- p + theme(legend.position = "none")
  }
  
  plots[[resp]] <- p
}

# Combine plots into a 2x2 grid
final_plot <- plot_grid(
  plots[["length"]] + ggtitle("a"),
  plots[["width"]] + ggtitle("b"),
  plots[["ssdensity"]] + ggtitle("c"),
  plots[["ssweight"]] + ggtitle("d"),  # Legend stays here
  ncol = 2, align = "hv"
)

# Save the final plot
ggsave(
  filename = "Sup Figure S3_Bimini_Grazing_timeseries.pdf",
  plot = final_plot,
  device = "pdf",
  width = 10,
  height = 8,
  units = "in"
)

ggsave(
  filename = "Sup Figure S3_Bimini_Grazing_timeseries.jpeg",
  plot = final_plot,
  device = "jpeg",
  dpi = 300,
  width = 10,
  height = 8,
  units = "in"
)

# Display the final plot
print(final_plot)


