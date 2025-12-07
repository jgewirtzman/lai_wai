# =============================================================================
# LAI Analysis: IQR Filtering, Distributions, and Spatial Mapping
# 
# Purpose: Analyze LAI data by Gauci biomes with outlier filtering and visualization
# Input: lai_data_with_gauci_mapping.csv (from previous biome mapping script)
# 
# Author: [Author Name]
# Date: [Date]
# Version: 1.0
# =============================================================================

# CHANGED: Set working directory relative to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")  # Go to project root

# Load required packages
required_packages <- c("dplyr", "readr", "ggplot2", "sf", "viridis", "scales", 
                       "stringr", "forcats", "patchwork", "RColorBrewer")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Set options for reproducible output
set.seed(42)
options(stringsAsFactors = FALSE, scipen = 999)

# Turn off spherical geometry
sf_use_s2(FALSE)

# =============================================================================
# 1. DATA LOADING
# =============================================================================

cat("Loading mapped LAI dataset...\n")

# Check if mapped data exists
# CHANGED: Use relative path to outputs folder
if (!file.exists("data/outputs/lai_data_with_gauci_mapping.csv")) {
  stop("Mapped LAI data not found. Please run the biome mapping script first.")
}

lai_mapped <- read_csv("data/outputs/lai_data_with_gauci_mapping.csv", show_col_types = FALSE)

cat(sprintf("Loaded %d records with Gauci biome mappings\n", nrow(lai_mapped)))

# Remove ambiguous classifications for analysis
lai_classified <- lai_mapped %>%
  filter(gauci_biome != "ambiguous" & !is.na(LAI)) %>%
  mutate(
    gauci_biome = factor(gauci_biome),
    LAI_numeric = as.numeric(LAI)
  ) %>%
  filter(!is.na(LAI_numeric) & LAI_numeric > 0)  # Remove invalid LAI values

cat(sprintf("Analyzing %d records with valid LAI and biome classifications\n", nrow(lai_classified)))

# =============================================================================
# 2. IQR FILTERING BY BIOME
# =============================================================================

cat("\nApplying IQR filtering within each biome...\n")

# Calculate IQR bounds for each biome
lai_with_bounds <- lai_classified %>%
  group_by(gauci_biome) %>%
  mutate(
    q1 = quantile(LAI_numeric, 0.25, na.rm = TRUE),
    q3 = quantile(LAI_numeric, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    lower_bound = q1 - 1.5 * iqr,
    upper_bound = q3 + 1.5 * iqr,
    is_outlier = LAI_numeric < lower_bound | LAI_numeric > upper_bound,
    outlier_type = case_when(
      LAI_numeric < lower_bound ~ "Low outlier",
      LAI_numeric > upper_bound ~ "High outlier", 
      TRUE ~ "Within IQR"
    )
  ) %>%
  ungroup()

# Create filtered dataset (IQR only)
lai_iqr_filtered <- lai_with_bounds %>%
  filter(!is_outlier)

# Summary of filtering
outlier_summary <- lai_with_bounds %>%
  group_by(gauci_biome) %>%
  summarise(
    total = n(),
    within_iqr = sum(!is_outlier),
    low_outliers = sum(outlier_type == "Low outlier"),
    high_outliers = sum(outlier_type == "High outlier"),
    percent_kept = round(100 * within_iqr / total, 1),
    .groups = "drop"
  )

cat("\nOutlier filtering summary by biome:\n")
print(outlier_summary)

# =============================================================================
# 3. STATISTICAL SUMMARIES
# =============================================================================

# Raw data statistics
stats_raw <- lai_classified %>%
  group_by(gauci_biome) %>%
  summarise(
    n = n(),
    mean_lai = round(mean(LAI_numeric, na.rm = TRUE), 2),
    sd_lai = round(sd(LAI_numeric, na.rm = TRUE), 2),
    median_lai = round(median(LAI_numeric, na.rm = TRUE), 2),
    q25 = round(quantile(LAI_numeric, 0.25, na.rm = TRUE), 2),
    q75 = round(quantile(LAI_numeric, 0.75, na.rm = TRUE), 2),
    min_lai = round(min(LAI_numeric, na.rm = TRUE), 2),
    max_lai = round(max(LAI_numeric, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(median_lai))

# IQR-filtered statistics
stats_iqr <- lai_iqr_filtered %>%
  group_by(gauci_biome) %>%
  summarise(
    n = n(),
    mean_lai = round(mean(LAI_numeric, na.rm = TRUE), 2),
    sd_lai = round(sd(LAI_numeric, na.rm = TRUE), 2),
    median_lai = round(median(LAI_numeric, na.rm = TRUE), 2),
    q25 = round(quantile(LAI_numeric, 0.25, na.rm = TRUE), 2),
    q75 = round(quantile(LAI_numeric, 0.75, na.rm = TRUE), 2),
    min_lai = round(min(LAI_numeric, na.rm = TRUE), 2),
    max_lai = round(max(LAI_numeric, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(median_lai))

cat("\nRAW DATA STATISTICS:\n")
print(stats_raw)

cat("\nIQR-FILTERED STATISTICS:\n")
print(stats_iqr)

# =============================================================================
# 4. DISTRIBUTION PLOTS
# =============================================================================

cat("\nCreating distribution plots...\n")

# Custom color palette for biomes
biome_colors <- RColorBrewer::brewer.pal(12, "Set3")
if (length(unique(lai_classified$gauci_biome)) > 12) {
  biome_colors <- rainbow(length(unique(lai_classified$gauci_biome)))
}

# Box plots - Raw vs IQR filtered
p1 <- ggplot(lai_classified, aes(x = fct_reorder(gauci_biome, LAI_numeric, median), y = LAI_numeric)) +
  geom_boxplot(aes(fill = gauci_biome), alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_manual(values = biome_colors) +
  coord_flip() +
  labs(
    title = "LAI Distribution by Gauci Biome (Raw Data)",
    x = "Gauci Biome",
    y = "Leaf Area Index (LAI)",
    caption = paste("n =", nrow(lai_classified), "observations")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, hjust = 0.5)
  )

p2 <- ggplot(lai_iqr_filtered, aes(x = fct_reorder(gauci_biome, LAI_numeric, median), y = LAI_numeric)) +
  geom_boxplot(aes(fill = gauci_biome), alpha = 0.7) +
  scale_fill_manual(values = biome_colors) +
  coord_flip() +
  labs(
    title = "LAI Distribution by Gauci Biome (IQR Filtered)",
    x = "Gauci Biome", 
    y = "Leaf Area Index (LAI)",
    caption = paste("n =", nrow(lai_iqr_filtered), "observations (outliers removed)")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, hjust = 0.5)
  )

# Combine plots
combined_boxplots <- p1 / p2
print(combined_boxplots)

# Save boxplots
# CHANGED: Save to outputs folder
ggsave("data/outputs/lai_distributions_by_biome.png", combined_boxplots, 
       width = 12, height = 10, dpi = 300, bg = "white")

# Violin plots with points
p3 <- ggplot(lai_iqr_filtered, aes(x = fct_reorder(gauci_biome, LAI_numeric, median), y = LAI_numeric)) +
  geom_violin(aes(fill = gauci_biome), alpha = 0.6, scale = "width") +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.size = 0.5) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 0.5) +
  scale_fill_manual(values = biome_colors) +
  coord_flip() +
  labs(
    title = "LAI Distribution by Gauci Biome (IQR Filtered)",
    subtitle = "Violin plots with boxplots and individual observations",
    x = "Gauci Biome",
    y = "Leaf Area Index (LAI)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5)
  )

print(p3)
# CHANGED: Save to outputs folder
ggsave("data/outputs/lai_violin_plots_by_biome.png", p3, width = 12, height = 8, dpi = 300, bg = "white")

# Density ridges plot
if (require(ggridges)) {
  p4 <- ggplot(lai_iqr_filtered, aes(x = LAI_numeric, y = fct_reorder(gauci_biome, LAI_numeric, median))) +
    geom_density_ridges(aes(fill = gauci_biome), alpha = 0.7, scale = 0.9) +
    scale_fill_manual(values = biome_colors) +
    labs(
      title = "LAI Density Distributions by Gauci Biome",
      x = "Leaf Area Index (LAI)",
      y = "Gauci Biome"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )
  
  print(p4)
  # CHANGED: Save to outputs folder
  ggsave("data/outputs/lai_density_ridges_by_biome.png", p4, width = 10, height = 8, dpi = 300, bg = "white")
}

# =============================================================================
# 6. SUMMARY STATISTICS TABLE
# =============================================================================

# Create comprehensive summary table
summary_table <- stats_iqr %>%
  left_join(
    outlier_summary %>% select(gauci_biome, total, percent_kept),
    by = "gauci_biome"
  ) %>%
  arrange(desc(median_lai)) %>%
  mutate(
    Range = paste0(min_lai, " - ", max_lai)
  ) %>%
  select(
    `Gauci Biome` = gauci_biome,
    `n (filtered)` = n,
    `n (total)` = total,
    `% kept` = percent_kept,
    `Mean LAI` = mean_lai,
    `Median LAI` = median_lai,
    `SD LAI` = sd_lai,
    `Q25` = q25,
    `Q75` = q75,
    Range
  )

cat("\nCOMPREHENSIVE LAI SUMMARY BY BIOME:\n")
print(summary_table)

# =============================================================================
# 7. EXPORT RESULTS
# =============================================================================

# Save datasets
# CHANGED: Save all outputs to outputs folder
write_csv(lai_iqr_filtered, "data/outputs/lai_data_iqr_filtered.csv")
write_csv(stats_raw, "data/outputs/lai_statistics_raw.csv") 
write_csv(stats_iqr, "data/outputs/lai_statistics_iqr_filtered.csv")
write_csv(summary_table, "data/outputs/lai_comprehensive_summary.csv")

if (exists("biome_summary_table")) {
  write_csv(biome_summary_table, "data/outputs/lai_by_biome_summary.csv")
}

cat("\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")
cat("ANALYSIS COMPLETE!\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")

cat("\nFiles created:\n")
cat("- lai_distributions_by_biome.png\n")
cat("- lai_violin_plots_by_biome.png\n")
if (require(ggridges)) cat("- lai_density_ridges_by_biome.png\n")
cat("- Multiple CSV summary files\n")

cat("\nSummary:\n")
cat(sprintf("- Analyzed %d LAI observations across %d Gauci biomes\n", 
            nrow(lai_classified), length(unique(lai_classified$gauci_biome))))
cat(sprintf("- IQR filtering retained %.1f%% of observations\n", 
            100 * nrow(lai_iqr_filtered) / nrow(lai_classified)))

# Session info
cat("\n")
cat(paste(rep("=", 30), collapse = ""))
cat("\n")
cat("SESSION INFO\n")
cat(paste(rep("=", 30), collapse = ""))
cat("\n")
print(sessionInfo())