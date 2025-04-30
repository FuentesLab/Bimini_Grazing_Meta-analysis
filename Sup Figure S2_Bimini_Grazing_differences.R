#This script creates Figure S2 differences seagrass parameters

# Required libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(dplyr)

# Load data
data <- read.csv("biminigrazing_plot_summary.csv", header = TRUE)

# Pivot the data to a long format
data_long <- data %>%
  pivot_longer(
    cols = c(t1, t2, t3),       # Specify only the time-point columns
    names_to = "time",          # New column for time (t1, t2, t3)
    values_to = "value"         # New column for values (numeric data)
  )

# Recode time
data_long <- data_long %>%
  mutate(time = factor(time, levels = c("t1", "t2", "t3"),
                       labels = c("Initiation", "1 Month", "3 Months")))

# Summarize mean and SD separately for Control and Exclusion
summary_sep <- data_long %>%
  group_by(variable, location, time, treatment) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Pivot wider to have separate columns for control and exclusion
summary_wide <- summary_sep %>%
  pivot_wider(
    names_from = treatment,
    values_from = c(mean_value, sd_value, n),
    names_sep = "_"
  )

# Calculate difference between means and SD
diff_summary <- summary_wide %>%
  mutate(
    mean_diff = mean_value_Exclusion - mean_value_Control,
    sd_diff = sqrt((sd_value_Exclusion^2 / n_Exclusion) + (sd_value_Control^2 / n_Control))
  ) %>%
  select(variable, location, time, mean_diff, sd_diff)

# View
print(diff_summary, n = 24)

# Write to CSV
write.csv(diff_summary, "diff_summary_table_updated.csv", row.names = FALSE)

y_axis_labels <- c(
  "length" = "Length difference (cm)",
  "width" = "Width difference (cm)",
  "ssdensity" = "Ss Density Difference (shoots/m²)",
  "ssweight" = "Ss Weight Difference (mg)"
)

# Create individual plots and store in list
plots <- list()
response_vars <- unique(diff_summary$variable)

for (resp in response_vars) {
  plot_data <- diff_summary %>% filter(variable == resp)
  
  p <- ggplot(plot_data, aes(x = location, y = mean_diff, shape = time)) +
    geom_point(size = 3, position = position_dodge(width = 0.4), color = "black") +
    geom_errorbar(aes(ymin = mean_diff - sd_diff, ymax = mean_diff + sd_diff),
                  width = 0.2, position = position_dodge(width = 0.4), color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray", linewidth = 0.7) +
    scale_shape_manual(values = c(16, 17, 18),
                       labels = c("t0", "t1", "t2")) +
    labs(
      y = y_axis_labels[[resp]],
      x = "Location",
      shape = "Sampling Time"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 14),
      strip.text = element_text(size = 14, face = "bold"),
      legend.position = "right", # Place legend to the right
      legend.box = "vertical", # Arrange legend vertically
      legend.background = element_rect(color = "black", linewidth = 0.5), 
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
      axis.text.y = element_text(size = 12)
    )
  
  # Custon y-axis breaks
  if (resp == "length") {
    p <- p + scale_y_continuous(breaks = scales::breaks_width(2))
  } else if (resp == "width") {
    p <- p + scale_y_continuous(breaks = scales::breaks_width(0.05))
  } else if (resp == "ssweight") {
    p <- p + scale_y_continuous(breaks = scales::breaks_width(50))
  }
  
  # Store plot in the list
  plots[[resp]] <- p
}

# Combine individual plots into a 2x2 grid
final_plot <- plot_grid(
  plots[["length"]] + ggtitle("a") + theme(legend.position = "none"),
  plots[["width"]] + ggtitle("b") + theme(legend.position = "none"),
  plots[["ssdensity"]] + ggtitle("c") + theme(legend.position = "none"),
  plots[["ssweight"]] + ggtitle("d") + theme(legend.position = "none"),
  ncol = 2, align = "hv"
)

# Extract the shared legend
shared_legend <- get_legend(plots[["length"]])

# Combine the plots and the shared legend
final_plot_with_legend <- plot_grid(
  final_plot,
  shared_legend,
  ncol = 2,
  rel_widths = c(4, 1) 
)

# Save the final plot as PDF and JPEG
ggsave(
  filename = "Sup Figure S2_Bimini_Grazing_differences.pdf",
  plot = final_plot_with_legend,
  device = "pdf",
  width = 10,
  height = 8,
  units = "in"
)

ggsave(
  filename = "Sup Figure S2_Bimini_Grazing_differences.jpeg",
  plot = final_plot_with_legend,
  device = "jpeg",
  dpi = 300,
  width = 10,
  height = 8,
  units = "in"
)

# Display the final plot
print(final_plot_with_legend)
