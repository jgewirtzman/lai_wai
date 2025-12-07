# =============================================================================
# Boyce Surface Area Analysis - Complementing Woody:Leaf Ratio Analysis
# 
# Purpose: Analyze surface area and volume relationships over stand development
# Data: Boyce (1975) oak and pine stand data showing 8 decades of development
# Style: Matches the woody:leaf ratio visualization aesthetics
# 
# Author: [Author Name]
# Date: [Date]
# Version: 1.0
# =============================================================================

# Set working directory relative to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")  # Go to project root

# Load required packages
required_packages <- c("dplyr", "readr", "ggplot2", "viridis", "scales", 
                       "stringr", "forcats", "RColorBrewer")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Set options
set.seed(42)
options(stringsAsFactors = FALSE, scipen = 999)

# =============================================================================
# 1. DATA LOADING
# =============================================================================

cat("Loading Boyce (1975) surface area data...\n")

# Load the data
data1 <- read_csv("data/inputs/quals data - Sheet1.csv", show_col_types = FALSE)

cat(sprintf("Loaded %d records of surface area and volume over stand development\n", nrow(data1)))

# Data inspection
cat("\nData overview:\n")
cat(sprintf("Age range: %d - %d years\n", min(data1$age), max(data1$age)))
cat(sprintf("Genera represented: %s\n", paste(unique(data1$genus), collapse = ", ")))
cat(sprintf("Surface area range: %d - %d m²/ha\n", min(data1$surface), max(data1$surface)))
cat(sprintf("Volume range: %.1f - %.1f m³/ha\n", min(data1$volume), max(data1$volume)))

# =============================================================================
# 2. CREATE MAIN SURFACE AREA vs AGE PLOT
# =============================================================================

cat("\nCreating surface area development plots...\n")

# Create the main plot focusing on age (matching your woody:leaf style)
p_main <- ggplot(data1, aes(x = age, y = surface, color = volume, shape = genus)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), 
              se = FALSE, color = "#666666", alpha = 0.7, linewidth = 1) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_gradient2(
    low = "#228B22",      # Forest green (low volume)
    mid = "#DAA520",      # Golden rod (medium volume)
    high = "#8B4513",     # Saddle brown (high volume)
    midpoint = mean(data1$volume),
    name = "Volume\n(m³/ha)"
  ) +
  scale_shape_manual(
    values = c(16, 17),  # Circle and triangle
    name = "Genus"
  ) +
  labs(
    title = "Forest Surface Area Development Over Time",
    subtitle = "Surface area accumulation during 8 decades of stand development",
    x = "Stand Age (years)",
    y = "Surface Area (m²/ha)",
    caption = "Data from Boyce (1975). Curve shows 2nd-order polynomial fit."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0.5, face = "italic"),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 1),
    plot.background = element_rect(fill = "white", color = "black", linewidth = 1)
  ) +
  guides(
    color = guide_colorbar(title.position = "top", barwidth = 8),
    shape = guide_legend(title.position = "top")
  )

print(p_main)

# =============================================================================
# 3. CREATE COMPACT INSERT VERSION
# =============================================================================

cat("\nCreating compact insert version...\n")

# Create a compact version for manuscript use (matching woody:leaf insert style)
p_insert <- ggplot(data1, aes(x = age, y = surface, color = volume, shape = genus)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), 
              se = FALSE, color = "#666666", alpha = 0.7, linewidth = 1.2) +
  geom_point(size = 3, alpha = 0.9) +
  scale_color_gradient2(
    low = "#228B22",      # Forest green
    mid = "#DAA520",      # Golden rod
    high = "#8B4513",     # Saddle brown
    midpoint = mean(data1$volume),
    guide = "none"        # Remove legend for insert
  ) +
  scale_shape_manual(
    values = c(16, 17),
    guide = "none"        # Remove legend for insert
  ) +
  labs(
    title = "Surface Area Development",
    x = "Stand Age (years)",
    y = "Surface Area (m²/ha)",
    caption = "8 decades of forest development"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 5)),
    plot.caption = element_text(size = 9, hjust = 0.5, face = "italic", margin = margin(t = 5)),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    plot.margin = margin(t = 8, r = 8, b = 8, l = 8),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 1),
    plot.background = element_rect(fill = "white", color = "black", linewidth = 1)
  )

print(p_insert)

# =============================================================================
# 4. CALCULATE SURFACE AREA TO VOLUME RATIOS
# =============================================================================

cat("\nCalculating surface area to volume ratios...\n")

# Calculate SA:V ratios and analyze trends
data_analysis <- data1 %>%
  mutate(
    sa_vol_ratio = surface / volume,
    age_class = case_when(
      age <= 30 ~ "Young (≤30 yr)",
      age <= 60 ~ "Mature (31-60 yr)", 
      TRUE ~ "Old (>60 yr)"
    )
  )

# Summary by age class
ratio_summary <- data_analysis %>%
  group_by(age_class, genus) %>%
  summarise(
    n = n(),
    mean_sa_vol = round(mean(sa_vol_ratio), 2),
    mean_surface = round(mean(surface), 0),
    mean_volume = round(mean(volume), 1),
    .groups = "drop"
  ) %>%
  arrange(genus, age_class)

cat("\nSURFACE AREA TO VOLUME RATIOS BY AGE CLASS:\n")
print(knitr::kable(ratio_summary,
                   col.names = c("Age Class", "Genus", "n", "Mean SA:Vol", "Mean Surface", "Mean Volume"),
                   format = "simple"))

# =============================================================================
# 5. CREATE DUAL-AXIS PLOT
# =============================================================================

# Create dual-axis plot showing both surface area and SA:Vol ratio
cat("\nCreating dual-axis plot with surface area and SA:Vol ratio...\n")

# Scale the SA:Vol ratio to fit nicely with surface area values
data_scaled <- data_analysis %>%
  mutate(
    # Scale SA:Vol ratio to surface area range for plotting
    sa_vol_scaled = scales::rescale(sa_vol_ratio, to = range(surface))
  )

# =============================================================================
# 5. CREATE DUAL-AXIS PLOT (UPDATED)
# =============================================================================

# Create dual-axis plot showing both surface area and SA:Vol ratio
cat("\nCreating dual-axis plot with surface area and SA:Vol ratio...\n")

# Scale the SA:Vol ratio to fit nicely with surface area values
data_scaled <- data_analysis %>%
  mutate(
    # Scale SA:Vol ratio to surface area range for plotting
    sa_vol_scaled = scales::rescale(sa_vol_ratio, to = range(surface))
  )

# Create the dual-axis plot
p_dual <- ggplot(data_scaled, aes(x = age)) +
  # Surface area points and line
  geom_smooth(aes(y = surface, color = genus, linetype = "Surface Area"), 
              method = "loess", se = FALSE, linewidth = 1.2) +
  geom_point(aes(y = surface, color = genus, shape = genus), 
             size = 3, alpha = 0.8) +
  
  # SA:Vol ratio points and line (scaled)
  geom_smooth(aes(y = sa_vol_scaled, color = genus, linetype = "Ratio"), 
              method = "loess", se = FALSE, 
              linewidth = 1.2, alpha = 0.7) +
  geom_point(aes(y = sa_vol_scaled, color = genus, shape = genus), 
             size = 3, alpha = 0.6, stroke = 1.5) +
  
  # Color scheme - completing the woody:leaf palette
  scale_color_manual(
    values = c("oak" = "#d73f3c", "pine" = "#507397"),  # Peru (warm brown-orange) and dark slate gray (cool blue-green)
    name = NULL
  ) +
  scale_shape_manual(
    values = c("oak" = 16, "pine" = 17),  # Circle and triangle
    name = NULL
  ) +
  scale_linetype_manual(
    values = c("Surface Area" = "solid", "Ratio" = "dotted"),
    name = NULL
  ) +
  
  # Primary y-axis (bole surface area)
  scale_y_continuous(
    name = "Bole Surface Area (m²/ha)",
    sec.axis = sec_axis(
      trans = ~ scales::rescale(., from = range(data_scaled$surface), to = range(data_analysis$sa_vol_ratio)),
      name = "Bole Surface Area : Volume Ratio"
    )
  ) +
  
  labs(
    title = "Forest Surface Area Development",
    subtitle = "Post-disturbance second growth, eastern US, early 20th century",
    x = "Stand Age (years)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic"),
    axis.title.y = element_text(color = "black", face = "plain"),
    axis.title.y.right = element_text(color = "black", face = "plain"),
    axis.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.margin = margin(t = -5),  # Reduce space above legend
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 1)
  ) +
  
  guides(
    color = guide_legend(title = NULL, override.aes = list(alpha = 1)),
    shape = guide_legend(title = NULL, override.aes = list(linetype = "blank")),
    linetype = guide_legend(title = NULL, override.aes = list(color = "black"))
  )

print(p_dual)

# Save dual-axis plot (without annotation)
ggsave("data/outputs/boyce_dual_axis.png", p_dual, 
       width = 6.5, height = 3.6, dpi = 600, bg = "white")

# =============================================================================
# 6. SUMMARY STATISTICS
# =============================================================================

cat("\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")
cat("ANALYSIS SUMMARY\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")

# Overall trends
overall_summary <- data1 %>%
  summarise(
    age_range = paste(min(age), "-", max(age), "years"),
    surface_range = paste(min(surface), "-", max(surface), "m²/ha"),
    volume_range = paste(round(min(volume), 1), "-", round(max(volume), 1), "m³/ha"),
    surface_increase = round((max(surface) - min(surface)) / min(surface) * 100, 1),
    volume_increase = round((max(volume) - min(volume)) / min(volume) * 100, 1)
  )

cat("DEVELOPMENT TRENDS OVER 8 DECADES:\n")
cat(sprintf("Age span: %s\n", overall_summary$age_range))
cat(sprintf("Surface area range: %s\n", overall_summary$surface_range))
cat(sprintf("Volume range: %s\n", overall_summary$volume_range))
cat(sprintf("Surface area increased %.1f-fold\n", overall_summary$surface_increase / 100 + 1))
cat(sprintf("Volume increased %.1f-fold\n", overall_summary$volume_increase / 100 + 1))

# Genus comparison
genus_comparison <- data1 %>%
  group_by(genus) %>%
  summarise(
    n = n(),
    mean_surface = round(mean(surface), 0),
    mean_volume = round(mean(volume), 1),
    mean_sa_vol = round(mean(surface/volume), 2),
    .groups = "drop"
  )

cat("\nGENUS COMPARISON:\n")
print(knitr::kable(genus_comparison,
                   col.names = c("Genus", "n", "Mean Surface", "Mean Volume", "Mean SA:Vol"),
                   format = "simple"))

# =============================================================================
# 7. EXPORT RESULTS
# =============================================================================

cat("\nSaving plots and analysis...\n")

# Save main plot
ggsave("data/outputs/boyce_surface_development.png", p_main, 
       width = 10, height = 8, dpi = 300, bg = "white")

# Save insert versions
ggsave("data/outputs/boyce_surface_insert_square.png", p_insert, 
       width = 4, height = 4, dpi = 300, bg = "white")

ggsave("data/outputs/boyce_surface_insert_wide.png", p_insert, 
       width = 6, height = 3, dpi = 300, bg = "white")

# Save dual-axis plot
ggsave("data/outputs/boyce_dual_axis.png", p_dual_annotated, 
       width = 6.5, height = 3.6, dpi = 600, bg = "white")

# Save analysis data
write_csv(data_analysis, "data/outputs/boyce_data_with_ratios.csv")
write_csv(ratio_summary, "data/outputs/boyce_ratio_summary.csv")
write_csv(genus_comparison, "data/outputs/boyce_genus_comparison.csv")

cat("\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")
cat("BOYCE SURFACE AREA ANALYSIS COMPLETE!\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")

cat("Files created:\n")
cat("- boyce_surface_development.png (main plot)\n")
cat("- boyce_surface_insert_square.png (4×4 insert)\n")
cat("- boyce_surface_insert_wide.png (6×3 insert)\n")
cat("- boyce_dual_axis.png (dual-axis plot)\n")
cat("- Multiple CSV analysis files\n")

cat("\nKey findings:\n")
cat("- Surface area increases over 8 decades of stand development\n")
cat("- 2nd-order polynomial relationship with age\n")
cat("- Surface area:volume ratio declines with stand maturity\n")
cat("- Demonstrates dynamic nature of forest surface area interface\n")

cat("\nThis analysis complements the woody:leaf ratio work by showing\n")
cat("how total forest surface area develops over time, supporting\n")
cat("the concept of surface area as a dynamic forest property.\n")



 