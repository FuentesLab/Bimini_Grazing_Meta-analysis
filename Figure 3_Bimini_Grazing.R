#This script creates Figure 3

# Required libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(dplyr)

# Load data

data <- read.csv("biminigrazing_plot_summary.csv", header = TRUE)

# Pivot the data to a long format for easier manipulation
data_long <- data %>%
  pivot_longer(
    cols = c(t1, t2, t3),       # Specify only the time-point columns
    names_to = "time",          # New column for time (e.g., t1, t2, t3)
    values_to = "value"         # New column for values (numeric data)
  )

# Calculate treatment - control differences
# Group by variable, location, time, and treatment
diff_summary <- data_long %>%
  pivot_wider(names_from = treatment, values_from = value) %>%  # Separate Control and Exclusion
  mutate(difference = Exclusion - Control) %>%                 # Calculate the difference
  group_by(variable, location, time) %>%                       # Group by variable, site, and time
  summarise(
    mean_diff = mean(difference, na.rm = TRUE),                # Mean of differences
    sd_diff = sd(difference, na.rm = TRUE),                    # SD of differences
    n = n()                                                    # Number of observations
  )

print(diff_summary, n=24)

write.csv(diff_summary, "diff_summary_table.csv", row.names = FALSE)

y_axis_labels <- c(
  "length" = "Length difference (cm)",
  "width" = "Width difference (cm)",
  "ssdensity" = "Ss Density Difference (shoots/m²)",
  "ssweight" = "Ss Weight Difference (mg)"
)

# Create individual plots and store them in a list
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
                       labels = c("t1", "t2", "t3")) +
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
      legend.background = element_rect(color = "black", linewidth = 0.5), # Box around legend
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
      axis.text.y = element_text(size = 12)
    )
  
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
  rel_widths = c(4, 1) # Adjust relative widths of plots and legend
)

# Save the final plot as PDF and JPEG
ggsave(
  filename = "final_plot_with_legend.pdf",
  plot = final_plot_with_legend,
  device = "pdf",
  width = 10,
  height = 8,
  units = "in"
)

ggsave(
  filename = "final_plot_with_legend.jpeg",
  plot = final_plot_with_legend,
  device = "jpeg",
  dpi = 300,
  width = 10,
  height = 8,
  units = "in"
)

# Display the final plot
print(final_plot_with_legend)





