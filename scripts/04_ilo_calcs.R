# =============================================================================
# ILO LAI Analysis: IQR Filtering, Distributions, and Spatial Mapping
# 
# Purpose: Analyze ILO LAI data by Gauci biomes with outlier filtering and visualization
# Input: ilo_data_with_gauci_mapping.csv (from previous ILO biome mapping script)
# 
# Author: [Author Name]
# Date: [Date]
# Version: 1.1 - FIXED: Column name issue
# =============================================================================

# CHANGED: Set working directory relative to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")  # Go to project root

# Load required packages
required_packages <- c("dplyr", "readr", "ggplot2", "sf", "viridis", "scales", 
                       "stringr", "forcats", "patchwork", "RColorBrewer", "tidyr")

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

cat("Loading mapped ILO LAI dataset...\n")

# Check if mapped data exists
# CHANGED: Use relative path to outputs folder
if (!file.exists("data/outputs/ilo_data_with_gauci_mapping.csv")) {
  stop("Mapped ILO LAI data not found. Please run the ILO biome mapping script first.")
}

ilo_mapped <- read_csv("data/outputs/ilo_data_with_gauci_mapping.csv", show_col_types = FALSE)

ilo_mapped$LAI<-ilo_mapped$Total_LAI

cat(sprintf("Loaded %d ILO records with Gauci biome mappings\n", nrow(ilo_mapped)))

# FIXED: Check what LAI column exists in the data
cat("\nAvailable columns in ILO data:\n")
cat(paste(names(ilo_mapped), collapse = ", "), "\n")

# Look for LAI-related columns
lai_columns <- names(ilo_mapped)[grepl("LAI|lai", names(ilo_mapped), ignore.case = TRUE)]
cat("LAI-related columns found:", paste(lai_columns, collapse = ", "), "\n")

# FIXED: Use Total_LAI instead of LAI for ILO data
ilo_classified <- ilo_mapped %>%
  filter(gauci_biome != "ambiguous" & !is.na(LAI)) %>%
  mutate(
    gauci_biome = factor(gauci_biome),
    LAI_numeric = as.numeric(LAI)  # CHANGED: Use Total_LAI
  ) %>%
  filter(!is.na(LAI_numeric) & LAI_numeric > 0 & LAI_numeric < 50)  # Remove invalid/extreme LAI values

cat(sprintf("Analyzing %d ILO records with valid LAI and biome classifications\n", nrow(ilo_classified)))

# Show data overview
cat("\nData overview:\n")
cat(sprintf("LAI range: %.2f - %.2f\n", min(ilo_classified$LAI_numeric), max(ilo_classified$LAI_numeric)))
cat(sprintf("Biomes represented: %d\n", length(unique(ilo_classified$gauci_biome))))
cat("Biomes:", paste(unique(ilo_classified$gauci_biome), collapse = ", "), "\n")

# =============================================================================
# 2. IQR FILTERING BY BIOME
# =============================================================================

cat("\nApplying IQR filtering within each biome...\n")

# Calculate IQR bounds for each biome
ilo_with_bounds <- ilo_classified %>%
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
ilo_iqr_filtered <- ilo_with_bounds %>%
  filter(!is_outlier)

# Summary of filtering
outlier_summary <- ilo_with_bounds %>%
  group_by(gauci_biome) %>%
  summarise(
    total = n(),
    within_iqr = sum(!is_outlier),
    low_outliers = sum(outlier_type == "Low outlier"),
    high_outliers = sum(outlier_type == "High outlier"),
    percent_kept = round(100 * within_iqr / total, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(total))

cat("\nOutlier filtering summary by biome:\n")
print(outlier_summary)

# =============================================================================
# 3. STATISTICAL SUMMARIES
# =============================================================================

# Raw data statistics
stats_raw <- ilo_classified %>%
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
stats_iqr <- ilo_iqr_filtered %>%
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

cat("\nILO RAW DATA STATISTICS:\n")
print(stats_raw)

cat("\nILO IQR-FILTERED STATISTICS:\n")
print(stats_iqr)

# =============================================================================
# 4. DISTRIBUTION PLOTS
# =============================================================================

cat("\nCreating distribution plots for ILO data...\n")

# Custom color palette for biomes
n_biomes <- length(unique(ilo_classified$gauci_biome))
if (n_biomes <= 12) {
  biome_colors <- RColorBrewer::brewer.pal(max(3, n_biomes), "Set3")
} else {
  biome_colors <- rainbow(n_biomes)
}
names(biome_colors) <- levels(ilo_classified$gauci_biome)

# Box plots - Raw vs IQR filtered
p1 <- ggplot(ilo_classified, aes(x = fct_reorder(gauci_biome, LAI_numeric, median), y = LAI_numeric)) +
  geom_boxplot(aes(fill = gauci_biome), alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_manual(values = biome_colors) +
  coord_flip() +
  labs(
    title = "ILO LAI Distribution by Gauci Biome (Raw Data)",
    x = "Gauci Biome",
    y = "Leaf Area Index (LAI)",
    caption = paste("n =", nrow(ilo_classified), "observations")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, hjust = 0.5)
  )

p2 <- ggplot(ilo_iqr_filtered, aes(x = fct_reorder(gauci_biome, LAI_numeric, median), y = LAI_numeric)) +
  geom_boxplot(aes(fill = gauci_biome), alpha = 0.7) +
  scale_fill_manual(values = biome_colors) +
  coord_flip() +
  labs(
    title = "ILO LAI Distribution by Gauci Biome (IQR Filtered)",
    x = "Gauci Biome", 
    y = "Leaf Area Index (LAI)",
    caption = paste("n =", nrow(ilo_iqr_filtered), "observations (outliers removed)")
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
ggsave("data/outputs/ilo_lai_distributions_by_biome.png", combined_boxplots, 
       width = 12, height = 10, dpi = 300, bg = "white")

# Violin plots with points (only if enough data)
if (nrow(ilo_iqr_filtered) > 50) {
  p3 <- ggplot(ilo_iqr_filtered, aes(x = fct_reorder(gauci_biome, LAI_numeric, median), y = LAI_numeric)) +
    geom_violin(aes(fill = gauci_biome), alpha = 0.6, scale = "width") +
    geom_boxplot(width = 0.2, alpha = 0.8, outlier.size = 0.5) +
    geom_jitter(width = 0.1, alpha = 0.3, size = 0.5) +
    scale_fill_manual(values = biome_colors) +
    coord_flip() +
    labs(
      title = "ILO LAI Distribution by Gauci Biome (IQR Filtered)",
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
  ggsave("data/outputs/ilo_lai_violin_plots_by_biome.png", p3, width = 12, height = 8, dpi = 300, bg = "white")
}

# Density ridges plot
if (require(ggridges, quietly = TRUE)) {
  p4 <- ggplot(ilo_iqr_filtered, aes(x = LAI_numeric, y = fct_reorder(gauci_biome, LAI_numeric, median))) +
    geom_density_ridges(aes(fill = gauci_biome), alpha = 0.7, scale = 0.9) +
    scale_fill_manual(values = biome_colors) +
    labs(
      title = "ILO LAI Density Distributions by Gauci Biome",
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
  ggsave("data/outputs/ilo_lai_density_ridges_by_biome.png", p4, width = 10, height = 8, dpi = 300, bg = "white")
}

# =============================================================================
# 5. PFT-SPECIFIC ANALYSIS
# =============================================================================

cat("\nAnalyzing LAI by Plant Functional Type (PFT)...\n")

# FIXED: Check if PFT column exists in the mapped data
if ("PFT" %in% names(ilo_iqr_filtered)) {
  # LAI by PFT
  pft_stats <- ilo_iqr_filtered %>%
    group_by(PFT) %>%
    summarise(
      n = n(),
      mean_lai = round(mean(LAI_numeric, na.rm = TRUE), 2),
      sd_lai = round(sd(LAI_numeric, na.rm = TRUE), 2),
      median_lai = round(median(LAI_numeric, na.rm = TRUE), 2),
      q25 = round(quantile(LAI_numeric, 0.25, na.rm = TRUE), 2),
      q75 = round(quantile(LAI_numeric, 0.75, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(desc(median_lai))
  
  cat("\nLAI STATISTICS BY PFT:\n")
  print(pft_stats)
  
  # PFT vs Biome crosstab
  if (require(tidyr, quietly = TRUE)) {
    pft_biome_crosstab <- ilo_iqr_filtered %>%
      count(PFT, gauci_biome) %>%
      pivot_wider(names_from = gauci_biome, values_from = n, values_fill = 0)
    
    cat("\nPFT vs BIOME CROSSTAB:\n")
    print(pft_biome_crosstab)
  } else {
    # Alternative approach using base R
    pft_biome_table <- table(ilo_iqr_filtered$PFT, ilo_iqr_filtered$gauci_biome)
    cat("\nPFT vs BIOME CROSSTAB:\n")
    print(pft_biome_table)
  }
  
  # Box plot by PFT
  p5 <- ggplot(ilo_iqr_filtered, aes(x = fct_reorder(PFT, LAI_numeric, median), y = LAI_numeric)) +
    geom_boxplot(aes(fill = PFT), alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.4, size = 0.8) +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    labs(
      title = "ILO LAI Distribution by Plant Functional Type",
      x = "Plant Functional Type (PFT)",
      y = "Leaf Area Index (LAI)",
      caption = "DB=Deciduous Broadleaf, EC=Evergreen Conifer, EB=Evergreen Broadleaf, etc."
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(size = 12)
    )
  
  print(p5)
  # CHANGED: Save to outputs folder
  ggsave("data/outputs/ilo_lai_by_pft.png", p5, width = 10, height = 6, dpi = 300, bg = "white")
} else {
  cat("PFT column not found in mapped data. Skipping PFT analysis.\n")
  pft_stats <- NULL
}

# =============================================================================
# 6. CLIMATE-LAI RELATIONSHIPS (if available)
# =============================================================================

cat("\nAnalyzing climate-LAI relationships...\n")

# Check if climate columns exist
climate_cols <- c("MAT_(Literature value)", "MAP_(Literature value)")
climate_available <- all(climate_cols %in% names(ilo_iqr_filtered))

if (climate_available) {
  # Filter data with climate information
  ilo_climate <- ilo_iqr_filtered %>%
    filter(!is.na(`MAT_(Literature value)`) & !is.na(`MAP_(Literature value)`))
  
  if (nrow(ilo_climate) > 10) {
    
    cat(sprintf("Analyzing climate relationships for %d records\n", nrow(ilo_climate)))
    
    # LAI vs Temperature
    p9 <- ggplot(ilo_climate, aes(x = `MAT_(Literature value)`, y = LAI_numeric)) +
      geom_point(aes(color = gauci_biome), alpha = 0.7, size = 2) +
      geom_smooth(method = "loess", se = TRUE, color = "black", linetype = "dashed") +
      scale_color_manual(values = biome_colors) +
      labs(
        title = "ILO LAI vs Mean Annual Temperature",
        x = "Mean Annual Temperature (°C)",
        y = "Leaf Area Index (LAI)",
        color = "Biome"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "right"
      )
    
    print(p9)
    # CHANGED: Save to outputs folder
    ggsave("data/outputs/ilo_lai_vs_temperature.png", p9, width = 12, height = 8, dpi = 300, bg = "white")
    
    # LAI vs Precipitation
    p10 <- ggplot(ilo_climate, aes(x = `MAP_(Literature value)`, y = LAI_numeric)) +
      geom_point(aes(color = gauci_biome), alpha = 0.7, size = 2) +
      geom_smooth(method = "loess", se = TRUE, color = "black", linetype = "dashed") +
      scale_color_manual(values = biome_colors) +
      scale_x_log10(labels = scales::comma) +
      labs(
        title = "ILO LAI vs Mean Annual Precipitation",
        x = "Mean Annual Precipitation (mm, log scale)",
        y = "Leaf Area Index (LAI)",
        color = "Biome"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        legend.position = "right"
      )
    
    print(p10)
    # CHANGED: Save to outputs folder
    ggsave("data/outputs/ilo_lai_vs_precipitation.png", p10, width = 12, height = 8, dpi = 300, bg = "white")
    
    # Climate summary by biome
    climate_summary <- ilo_climate %>%
      group_by(gauci_biome) %>%
      summarise(
        n = n(),
        mean_lai = round(mean(LAI_numeric), 2),
        mean_temp = round(mean(`MAT_(Literature value)`), 1),
        mean_precip = round(mean(`MAP_(Literature value)`), 0),
        temp_range = paste0(round(min(`MAT_(Literature value)`), 1), " to ", 
                            round(max(`MAT_(Literature value)`), 1)),
        precip_range = paste0(round(min(`MAP_(Literature value)`), 0), " to ", 
                              round(max(`MAP_(Literature value)`), 0)),
        .groups = "drop"
      ) %>%
      arrange(desc(mean_lai))
    
    cat("\nCLIMATE SUMMARY BY BIOME:\n")
    print(climate_summary)
    
    # CHANGED: Save to outputs folder
    write_csv(climate_summary, "data/outputs/ilo_climate_summary_by_biome.csv")
  } else {
    cat("Insufficient climate data for analysis\n")
    climate_summary <- NULL
  }
} else {
  cat("Climate columns not found in data. Skipping climate analysis.\n")
  climate_summary <- NULL
}

# =============================================================================
# 7. COMPREHENSIVE SUMMARY TABLE
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

cat("\nCOMPREHENSIVE ILO LAI SUMMARY BY BIOME:\n")
print(summary_table)

# =============================================================================
# 8. EXPORT RESULTS
# =============================================================================

# Save datasets
# CHANGED: Save all outputs to outputs folder
write_csv(ilo_iqr_filtered, "data/outputs/ilo_lai_data_iqr_filtered.csv")
write_csv(stats_raw, "data/outputs/ilo_lai_statistics_raw.csv") 
write_csv(stats_iqr, "data/outputs/ilo_lai_statistics_iqr_filtered.csv")
write_csv(summary_table, "data/outputs/ilo_lai_comprehensive_summary.csv")

if (!is.null(pft_stats)) {
  write_csv(pft_stats, "data/outputs/ilo_lai_by_pft_summary.csv")
}

if (!is.null(climate_summary)) {
  write_csv(climate_summary, "data/outputs/ilo_climate_summary_by_biome.csv")
}

cat("\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")
cat("ILO LAI ANALYSIS COMPLETE!\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")

cat("\nFiles created:\n")
cat("- ilo_lai_distributions_by_biome.png\n")
if (exists("p3")) cat("- ilo_lai_violin_plots_by_biome.png\n")
if (require(ggridges, quietly = TRUE)) cat("- ilo_lai_density_ridges_by_biome.png\n")
if (!is.null(pft_stats)) cat("- ilo_lai_by_pft.png\n")
if (!is.null(climate_summary)) {
  cat("- ilo_lai_vs_temperature.png\n")
  cat("- ilo_lai_vs_precipitation.png\n")
}
cat("- Multiple CSV summary files\n")

cat("\nSummary:\n")
cat(sprintf("- Analyzed %d ILO LAI observations across %d Gauci biomes\n", 
            nrow(ilo_classified), length(unique(ilo_classified$gauci_biome))))
cat(sprintf("- IQR filtering retained %.1f%% of observations\n", 
            100 * nrow(ilo_iqr_filtered) / nrow(ilo_classified)))

if (!is.null(pft_stats)) {
  cat(sprintf("- Data spans %d plant functional types (PFT)\n", 
              length(unique(ilo_iqr_filtered$PFT))))
}

if (!is.null(climate_summary)) {
  cat(sprintf("- Climate analysis for %d records with temperature and precipitation data\n", 
              nrow(ilo_climate)))
}

# Compare with original Asner results if available
if (file.exists("data/outputs/lai_data_iqr_filtered.csv")) {
  cat("\nComparison with Asner dataset:\n")
  asner_data <- read_csv("data/outputs/lai_data_iqr_filtered.csv", show_col_types = FALSE)
  cat(sprintf("- Asner dataset: %d observations\n", nrow(asner_data)))
  cat(sprintf("- ILO dataset: %d observations\n", nrow(ilo_iqr_filtered)))
  cat(sprintf("- Total combined: %d observations\n", nrow(asner_data) + nrow(ilo_iqr_filtered)))
}

# Biome coverage comparison
cat("\nBiome representation in ILO dataset:\n")
biome_coverage <- ilo_iqr_filtered %>%
  count(gauci_biome, sort = TRUE) %>%
  mutate(percentage = round(100 * n / sum(n), 1))

for (i in 1:nrow(biome_coverage)) {
  cat(sprintf("- %s: %d observations (%.1f%%)\n", 
              biome_coverage$gauci_biome[i], 
              biome_coverage$n[i], 
              biome_coverage$percentage[i]))
}

# Key findings summary
cat("\nKey Findings:\n")

# Highest LAI biomes
top_lai_biomes <- stats_iqr %>% 
  arrange(desc(median_lai)) %>% 
  head(3)

cat("- Highest LAI biomes:\n")
for (i in 1:nrow(top_lai_biomes)) {
  cat(sprintf("  %d. %s (median LAI: %.1f)\n", 
              i, top_lai_biomes$gauci_biome[i], top_lai_biomes$median_lai[i]))
}

# LAI range
overall_lai_range <- range(ilo_iqr_filtered$LAI_numeric)
cat(sprintf("- Overall LAI range: %.1f - %.1f\n", overall_lai_range[1], overall_lai_range[2]))

# Most common PFT (if available)
if (!is.null(pft_stats)) {
  most_common_pft <- ilo_iqr_filtered %>% count(PFT, sort = TRUE) %>% head(1)
  cat(sprintf("- Most common plant functional type: %s (%d observations)\n", 
              most_common_pft$PFT, most_common_pft$n))
}

# Geographic coverage
if ("Latitude" %in% names(ilo_iqr_filtered) && "Longitude" %in% names(ilo_iqr_filtered)) {
  lat_range <- range(ilo_iqr_filtered$Latitude, na.rm = TRUE)
  lon_range <- range(ilo_iqr_filtered$Longitude, na.rm = TRUE)
  cat(sprintf("- Geographic coverage: %.1f°N to %.1f°N, %.1f°E to %.1f°E\n", 
              lat_range[1], lat_range[2], lon_range[1], lon_range[2]))
  
  # Count countries
  if ("Country" %in% names(ilo_iqr_filtered)) {
    n_countries <- length(unique(ilo_iqr_filtered$Country))
    cat(sprintf("- Data from %d countries\n", n_countries))
  }
}

# Data quality metrics
if (!is.null(climate_summary)) {
  pct_with_climate <- round(100 * nrow(ilo_climate) / nrow(ilo_iqr_filtered), 1)
  cat(sprintf("- %.1f%% of records include climate data\n", pct_with_climate))
}

if ("Latitude" %in% names(ilo_iqr_filtered)) {
  pct_with_coords <- round(100 * sum(!is.na(ilo_iqr_filtered$Latitude)) / nrow(ilo_iqr_filtered), 1)
  cat(sprintf("- %.1f%% of records include coordinates\n", pct_with_coords))
}

# Session info
cat("\n")
cat(paste(rep("=", 30), collapse = ""))
cat("\n")
cat("SESSION INFO\n")
cat(paste(rep("=", 30), collapse = ""))
cat("\n")
print(sessionInfo())