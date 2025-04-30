#This script creates Figure 3 boxplot for t3

# Required libraries
library(tidyverse)
library(cowplot)
library(ggplot2)

# Load data
data <- read.csv("biminigrazing_plot_summary.csv", header = TRUE)

# Shorten location names
data$location <- recode(data$location,
                        "Bonefish Hole" = "Bonefish Hole",
                        "South Flats" = "South Flats")

# Set up the response variable labels
y_axis_labels <- c(
  "length" = "Length (cm)",
  "width" = "Width (cm)",
  "ssdensity" = "Shoot Density (shoots/m²)",
  "ssweight" = "Shoot Weight (mg)"
)

# Set control = black, exclusion = white
data$treatment_fill <- recode(data$treatment,
                              "Control" = "gray",
                              "Exclusion" = "white")

# Create a custom order for plotting
data$treatment <- factor(data$treatment, levels = c("Control", "Exclusion"))
data$location <- factor(data$location, levels = c("Bonefish Hole", "South Flats"))

# Now loop through each variable
plots <- list()

for (v in unique(data$variable)) {
  
  plot_data <- data %>% filter(variable == v)
  
  p <- ggplot(plot_data, aes(x = location, y = t3, fill = treatment)) +
    geom_boxplot(
      color = "black",
      width = 0.5,                          # Narrower boxes
      position = position_dodge(width = 0.6) # Tighter dodge
    ) +
    scale_fill_manual(values = c("gray", "white"),
                      name = "Treatment",
                      labels = c("Control", "Exclusion")) +
    labs(x = NULL,
         y = y_axis_labels[v],
         title = NULL) +
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
  legend.text = element_text(size = 11, color = "black"),
  legend.position = "none"
    )
  
  plots[[v]] <- p
}

# Pull out the legend separately from the "Shoot Density" plot
plot_for_legend <- ggplot(filter(data, variable == "ssdensity"), aes(x = location, y = t3, fill = treatment)) +
  geom_boxplot(
    color = "black",
    width = 0.5,
    position = position_dodge(width = 0.6)
  ) +
  scale_fill_manual(values = c("gray", "white"),
                    name = "Treatment",
                    labels = c("Control", "Exclusion")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.background = element_blank(),
    plot.background = element_blank()
  )

# Extract legend
legend <- get_legend(plot_for_legend)

# Make the 2x2 plot grid
plot_grid_main <- plot_grid(
  plots[["length"]] + ggtitle("a"),
  plots[["width"]] + ggtitle("b"),
  plots[["ssdensity"]] + ggtitle("c"),
  plots[["ssweight"]] + ggtitle("d"),   
  ncol = 2
)

# Combine legend and plots
final_plot <- plot_grid(plot_grid_main, legend, ncol = 1, rel_heights = c(1, 0.1))

# Print it
print(final_plot)


# Save as PDF
ggsave(
  filename = "Fig3_seagrass_response.pdf",
  plot = final_plot,
  device = "pdf",
  width = 10,
  height = 8,
  units = "in"
)

# Save as JPEG
ggsave(
  filename = "Fig3_seagrass_response.jpeg",
  plot = final_plot,
  device = "jpeg",
  dpi = 300,
  width = 10,
  height = 8,
  units = "in"
)

