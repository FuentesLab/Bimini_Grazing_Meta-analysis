#This script constructs the sea turtle grazing meta-analysis and generates figures 4 & 5

#####----Required libraries-----------------------------------------------------

# Load required libraries
library(effects)
library(emmeans)
library(ggthemes)
library(meta)
library(nlme)
# library(piecewiseSEM)
library(readxl)
library(tidyverse)
library(ggpubr)

#####----Data prep--------------------------------------------------------------

# Read in data
turtles_raw <- read_excel("C:/Users/jslef/OneDrive/Documents/Manuscripts/Sea Turtle Meta-Analysis/Data/Metadata_Bimini_Grazing_Reformatted Final.xlsx", sheet = 1)


######----Format data-----------------------------------------------------------

# Format data for analysis
turtles <- turtles_raw %>%
  
  # Recode response values as numerics
  mutate(across(Response.Mean_Grazed:Response.N_Ungrazed, as.numeric)) %>%
  
  # Recode covariates
  mutate(Latitude = as.numeric(as.character(Latitude)),
         Longitude = as.numeric(as.character(Longitude)),
         Experiment.Duration = as.numeric(as.character(Experiment.Duration)),
         # Create abbreviation for Reference
         Reference2 = 
           case_when(sum(nchar(str_extract_all(Author, "\\,")[[1]])) > 1 ~ 
                       paste(gsub(" .*", "", Author), "et al.", Year), 
                     .default = paste(paste(strsplit(gsub(",", "", Author), " ")[[1]][c(1, 3)], collapse = " & "), Year)),
         Reference2 = as.factor(Reference2),
         Reference2 = recode(Reference2, "van et al. 2017" = "van Tussenbroek et al. 2017")) 

# filter(turtles, is.na(Response.Mean_Grazed)) %>% View(.)

######-----Summarize LRRs-------------------------------------------------------

# Fit random effects model
# c = control = open to grazing
# e = experiment = grazer exclusion
meta <- metacont(
  n.c = Response.N_Grazed,
  mean.c = Response.Mean_Grazed,
  sd.c = Response.SD_Grazed,
  n.e = Response.N_Ungrazed,
  mean.e = Response.Mean_Ungrazed,
  sd.e = Response.SD_Ungrazed,
  data = turtles,
  # Response is log ratio of means
  sm = "ROM",
  studlab = Reference2,
  # Random effects model
  fixed = FALSE,
  random = TRUE,
  # Do not return backtransformed ratio
  backtransf = FALSE,
  # Group by reference
  subgroup = Reference2
  )

summary(meta)

# Create table of results
meta_out <- data.frame(
  Reference2 = names(meta$TE.random.w),
  LRR = meta$TE.random.w,
  upper = meta$upper.random.w,
  lower = meta$lower.random.w,
  n = meta$k.w,
  pval = meta$pval.random.w)

######----Catepillar plot-------------------------------------------------------

# Arrange LRRs in descending order
meta_out <- arrange(meta_out, desc(LRR)) 

meta_out$Reference2 <- factor(meta_out$Reference2, levels = rev(meta_out$Reference2))

# Assign significance
meta_out$lty <- ifelse(meta_out$pval < 0.05, "1", "2")

# Plot results
(cat_plot <- ggplot(meta_out, aes(x = as.factor(Reference2), y = LRR)) +
    # Add 0 line
    geom_hline(yintercept = 0, col = "grey50") +
    # Add line for mean effect size
    geom_hline(yintercept = meta$TE.random, col = "red") +
    # Add 95% CI's on mean (ignore warning)
    annotate("rect", 
             ymin = meta$lower.random,
             ymax = meta$upper.random,
             xmin = -Inf, xmax = Inf,
             fill = "red", alpha = 0.3) +
    # Add error bars for each reference
    geom_errorbar(aes(ymax = upper, ymin = lower, lty = lty), width = 0) +
    # Add sample sizes next to each observation
    geom_text(aes(y = upper + 0.1, x = Reference2, label = paste0("(", n, ")")), size = 3, nudge_y = 0.075) +
    # Add point for mean effect size for each reference
    geom_point(aes(shape = lty, fill = lty)) +
    scale_shape_manual(values = c(16, 21)) +
    scale_fill_manual(values = c("black", "white")) +
    coord_flip() +
    theme_bw(base_size = 12) +
    labs(y = bquote(LRR[mean]), x = "") +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      text = element_text(family = "sans"),
      axis.text.y = element_text(color = "black")
    )
)

ggsave("C:/Users/jslef/OneDrive/Documents/Manuscripts/Sea Turtle Meta-Analysis/Manuscript/Figure 4.pdf", width = 6, height = 8)

# Get statistics
# Grand mean
meta$TE.random
# Confidence intervals
meta$lower.random
meta$upper.random

# Convert to % change
pchange <- function(x) 100*(exp(x) - 1)

sapply(c(meta$TE.random, meta$lower.random, meta$upper.random), pchange)

#####----GLMM-------------------------------------------------------------------

# Examine covariates using meta package
(meta_model <- metareg(meta, ~ Grazing.Type + Response.Cat + abs(Latitude) + Longitude + Focal.Type + Experiment.Duration)) #  + Focal.Genera

# Extract LRRs from meta and use nlme for model building (for better visualization)
turtles$LRR <- meta$TE

# Analyze LRRs using lme
model <- lme(LRR ~ 
               Grazing.Type +
               Response.Cat + 
               abs(Latitude) + 
               Longitude +
               # Focal.Genera +
               Focal.Type +
               Experiment.Duration,
             random = list(Reference = ~ 1),
             na.action = na.omit,
             data = turtles)

# Check model assumptions
nobs(model)

plot(model) # heterogeneity of variance

hist(resid(model)) # normality of errors

# Get model R^2
piecewiseSEM::rsquared(model)

# Get ANOVA table
(anova_model <- car::Anova(model, type = "II"))

anova_model %>% mutate(Response = rownames(anova_model), .before = "Chisq",
                       Chisq = round(Chisq, 3),
                       `Pr(>Chisq)` = round(`Pr(>Chisq)`, 3)) %>% 
  gt::gt() %>% gt::gtsave("C:/Users/jslef/OneDrive/Documents/Manuscripts/Sea Turtle Meta-Analysis/Manuscript/ANOVA table.docx")

######----Post-hoc contrasts----------------------------------------------------

# Loop over responses
fig5_plots <- lapply(rownames(anova_model)[which(anova_model$`Pr(>Chisq)` < 0.05)], function(i) {

  # Post-hoc contrasts
  posthoc <- emmeans(model, formula(paste("pairwise ~", i)),
                     at = list(Response.Cat = c("Aboveground Biomass", 
                                                "Belowground Biomass", 
                                                "Epiphyte", 
                                                "Metabolic Rate", 
                                                "Morphology", 
                                                "Nutrient", 
                                                "Productivity", 
                                                "Reproductive Fitness", 
                                                "Total Biomass")))
  

  # Pairwise t-tests
  # pwpm(posthoc)
  # Letters for plotting
  # multcomp::cld(posthoc, Letters = letters)

  ylab <- ifelse(i == "Grazing.Type", "Grazing Type", ifelse(i == "Response.Cat", "Response", "Seagrass Type"))
  
  p <- plot(posthoc) +
    geom_vline(xintercept = 0, col = "black") +
    geom_text(
      data = posthoc$emmeans@grid,
      aes(x = as.data.frame(posthoc$emmeans)$emmean, 
          y = as.data.frame(posthoc$emmeans)[,1], 
          label = paste0("(",.wgt.,")")),
      nudge_y = 0.3, 
      size = 3) +
    geom_text(
      data = multcomp::cld(posthoc, Letters = letters),
      aes(x = upper.CL, 
          y = multcomp::cld(posthoc, Letters = letters)[,1], 
          label = .group),
      nudge_x = 0.2) +
    coord_flip() +
    theme_bw(base_size = 12) +
    labs(x = bquote(italic(LRR)[mean]), y = paste(ylab)) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.tag = element_text(),
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
      text = element_text(family = "sans", color = "black"),  
      axis.title = element_text(family = "sans", color = "black"),
      axis.text = element_text(family = "sans", color = "black") 
    )
  
  if(i != "Response.Cat") p <- p + labs(x = "")
  
  return(p)
  
} )

# # Append plot for latitude
# fig5_plots <- append(fig5_plots, list(
#   ggplot(as.data.frame(effect("abs(Latitude)", model, type = "scale")),
#        aes(x = Latitude, y = fit)) +
#   geom_hline(yintercept = 0, col = "grey50") +
#   # geom_rug(data = turtles_model, aes(x = Latitude, y = LRR)) +
#   geom_ribbon(aes(ymax = upper, ymin = lower), col = NA, alpha = 0.2) +
#   geom_line(lwd = 1) +
#   # scale_color_continuous(low = "blue", high = "red", name = bquote(Inshore~degree*C)) +
#   # scale_fill_continuous(low = "blue", high = "red", name = bquote(Inshore~degree*C)) +
#   labs(x = "Latitude", y = "LRR") +
#   theme_bw(base_size = 12) +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.tag = element_text()
#   )
# ))

fig5_plots <- list(fig5_plots[[2]], fig5_plots[[3]], fig5_plots[[1]])

(fig5 <- cowplot::plot_grid(plotlist = fig5_plots, align = "h", labels = letters, rel_widths = c(0.7, 0.3, 0.25), nrow =1))

ggsave("C:/Users/jslef/OneDrive/Documents/Manuscripts/Sea Turtle Meta-Analysis/Manuscript/Figure 5.pdf", width = 11, height = 4.5)

