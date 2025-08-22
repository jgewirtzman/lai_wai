# =============================================================================
# LAI Biome Comparison Analysis - REVISED (No Statistical Testing)
# 
# Purpose: Compare LAI results by biome between Asner and ILO workflows
# Inputs: Results from both LAI analysis workflows
# 
# Author: [Author Name]
# Date: [Date]
# Version: 3.0 - Removed statistical testing and fudging
# =============================================================================

# CHANGED: Set working directory relative to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")  # Go to project root

# Load required packages
required_packages <- c("dplyr", "readr", "ggplot2", "viridis", "scales", 
                       "stringr", "forcats", "patchwork", "RColorBrewer", 
                       "tidyr", "knitr")

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

cat("Loading LAI analysis results from both workflows...\n")

# Load Asner workflow results
# CHANGED: Use relative path to outputs folder
asner_file <- "data/outputs/lai_data_iqr_filtered.csv"
if (file.exists(asner_file)) {
  asner_data <- read_csv(asner_file, show_col_types = FALSE) %>%
    mutate(
      dataset = "Asner",
      LAI_value = as.numeric(LAI_numeric)
    ) %>%
    filter(!is.na(LAI_value) & gauci_biome != "ambiguous")
  cat(sprintf("Loaded Asner data: %d records\n", nrow(asner_data)))
} else {
  cat("WARNING: Asner filtered data not found. Please run LAI_calcs.R first.\n")
  stop("Cannot proceed without Asner data")
}

# Load ILO workflow results  
# CHANGED: Use relative path to outputs folder
ilo_file <- "data/outputs/ilo_lai_data_iqr_filtered.csv"
if (file.exists(ilo_file)) {
  ilo_data <- read_csv(ilo_file, show_col_types = FALSE) %>%
    mutate(
      dataset = "ILO",
      LAI_value = as.numeric(LAI_numeric)
    ) %>%
    filter(!is.na(LAI_value) & gauci_biome != "ambiguous")
  cat(sprintf("Loaded ILO data: %d records\n", nrow(ilo_data)))
} else {
  cat("WARNING: ILO filtered data not found. Please run ilo_calcs.R first.\n")
  stop("Cannot proceed without ILO data")
}

# =============================================================================
# 2. DATA HARMONIZATION
# =============================================================================

cat("\nHarmonizing data for comparison...\n")

# Combine datasets - LAI limit of 20
combined_data <- bind_rows(
  asner_data %>% select(
    dataset, gauci_biome, LAI_value, 
    Latitude, Longitude, Country
  ),
  ilo_data %>% select(
    dataset, gauci_biome, LAI_value = LAI_numeric,
    Latitude, Longitude, Country
  )
) %>%
  filter(!is.na(LAI_value) & LAI_value > 0 & LAI_value <= 20) %>%
  mutate(
    gauci_biome = factor(gauci_biome),
    dataset = factor(dataset, levels = c("Asner", "ILO"))
  )

cat(sprintf("Combined dataset: %d records total\n", nrow(combined_data)))
cat(sprintf("Asner: %d records, ILO: %d records\n", 
            sum(combined_data$dataset == "Asner"),
            sum(combined_data$dataset == "ILO")))

# =============================================================================
# 3. BASIC STATISTICAL SUMMARIES
# =============================================================================

cat("\nCalculating statistical summaries by biome and dataset...\n")

# Diagnostic: Check biome overlap
cat("\nDIAGNOSTIC: Checking biome names...\n")
asner_biomes <- unique(combined_data$gauci_biome[combined_data$dataset == "Asner"])
ilo_biomes <- unique(combined_data$gauci_biome[combined_data$dataset == "ILO"])

cat("Asner biomes:\n")
for(b in asner_biomes) cat(sprintf("  '%s'\n", b))
cat("\nILO biomes:\n") 
for(b in ilo_biomes) cat(sprintf("  '%s'\n", b))
cat("\nOverlapping biomes:\n")
overlap <- intersect(asner_biomes, ilo_biomes)
for(b in overlap) cat(sprintf("  '%s'\n", b))
cat(sprintf("\nNumber of overlapping biomes: %d\n", length(overlap)))

# Calculate statistics for each biome and dataset
biome_stats <- combined_data %>%
  group_by(dataset, gauci_biome) %>%
  summarise(
    n = n(),
    mean_lai = round(mean(LAI_value, na.rm = TRUE), 2),
    median_lai = round(median(LAI_value, na.rm = TRUE), 2),
    sd_lai = round(sd(LAI_value, na.rm = TRUE), 2),
    q25 = round(quantile(LAI_value, 0.25, na.rm = TRUE), 2),
    q75 = round(quantile(LAI_value, 0.75, na.rm = TRUE), 2),
    min_lai = round(min(LAI_value, na.rm = TRUE), 2),
    max_lai = round(max(LAI_value, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(gauci_biome, dataset)

# Create wide format for easier comparison
asner_stats <- biome_stats %>%
  filter(dataset == "Asner") %>%
  select(gauci_biome, n, mean_lai, median_lai, sd_lai) %>%
  rename(n_Asner = n, mean_lai_Asner = mean_lai, median_lai_Asner = median_lai, sd_lai_Asner = sd_lai)

ilo_stats <- biome_stats %>%
  filter(dataset == "ILO") %>%
  select(gauci_biome, n, mean_lai, median_lai, sd_lai) %>%
  rename(n_ILO = n, mean_lai_ILO = mean_lai, median_lai_ILO = median_lai, sd_lai_ILO = sd_lai)

# Manual merge using full_join
comparison_table <- full_join(asner_stats, ilo_stats, by = "gauci_biome") %>%
  mutate(
    n_Asner = replace_na(n_Asner, 0),
    n_ILO = replace_na(n_ILO, 0),
    mean_diff = case_when(
      !is.na(mean_lai_ILO) & !is.na(mean_lai_Asner) ~ round(mean_lai_ILO - mean_lai_Asner, 2),
      TRUE ~ NA_real_
    ),
    median_diff = case_when(
      !is.na(median_lai_ILO) & !is.na(median_lai_Asner) ~ round(median_lai_ILO - median_lai_Asner, 2),
      TRUE ~ NA_real_
    ),
    n_total = n_Asner + n_ILO,
    has_both = n_Asner > 0 & n_ILO > 0
  ) %>%
  arrange(desc(n_total))

cat("\nDEBUG: Manual merge results:\n")
cat(sprintf("Total rows in comparison table: %d\n", nrow(comparison_table)))
cat(sprintf("Biomes with data in both datasets: %d\n", sum(comparison_table$has_both, na.rm = TRUE)))

# Show biomes with both datasets
both_datasets <- comparison_table %>% filter(has_both)
if (nrow(both_datasets) > 0) {
  cat("\nBiomes with data in BOTH datasets:\n")
  for(i in 1:nrow(both_datasets)) {
    cat(sprintf("  %s: Asner=%d, ILO=%d\n", 
                both_datasets$gauci_biome[i],
                both_datasets$n_Asner[i], 
                both_datasets$n_ILO[i]))
  }
} else {
  cat("No biomes with both datasets found!\n")
}

# Create complete comparison table
complete_comparison <- biome_stats %>%
  pivot_wider(
    names_from = dataset,
    values_from = c(n, mean_lai, median_lai, sd_lai),
    names_sep = "_"
  ) %>%
  mutate(
    n_Asner = replace_na(n_Asner, 0),
    n_ILO = replace_na(n_ILO, 0),
    mean_diff = case_when(
      !is.na(mean_lai_ILO) & !is.na(mean_lai_Asner) ~ round(mean_lai_ILO - mean_lai_Asner, 2),
      TRUE ~ NA_real_
    ),
    median_diff = case_when(
      !is.na(median_lai_ILO) & !is.na(median_lai_Asner) ~ round(median_lai_ILO - median_lai_Asner, 2),
      TRUE ~ NA_real_
    ),
    n_total = n_Asner + n_ILO,
    in_both = (n_Asner > 0 & n_ILO > 0)
  ) %>%
  arrange(desc(n_total))

cat("\nCOMPARISON TABLE - OVERLAPPING BIOMES ONLY:\n")
if (nrow(comparison_table) > 0) {
  print(kable(comparison_table %>% 
                select(gauci_biome, n_Asner, n_ILO, n_total,
                       mean_lai_Asner, mean_lai_ILO, mean_diff,
                       median_lai_Asner, median_lai_ILO, median_diff),
              col.names = c("Biome", "n_Asner", "n_ILO", "n_Total",
                            "Mean_Asner", "Mean_ILO", "Mean_Diff",
                            "Median_Asner", "Median_ILO", "Median_Diff"),
              format = "simple"))
} else {
  cat("No overlapping biomes found!\n")
}

cat("\nCOMPLETE COMPARISON TABLE - ALL BIOMES:\n")
print(kable(complete_comparison %>% 
              select(gauci_biome, n_Asner, n_ILO, n_total, in_both,
                     mean_lai_Asner, mean_lai_ILO, mean_diff),
            col.names = c("Biome", "n_Asner", "n_ILO", "n_Total", "In_Both",
                          "Mean_Asner", "Mean_ILO", "Mean_Diff"),
            format = "simple"))

# =============================================================================
# 4. DESCRIPTIVE ANALYSIS (NO STATISTICAL TESTING)
# =============================================================================

cat("\nDescriptive analysis of biome differences...\n")

# Analyze biomes with overlapping data
if (nrow(both_datasets) > 0) {
  cat("\nDESCRITIVE SUMMARY FOR OVERLAPPING BIOMES:\n")
  
  descriptive_summary <- both_datasets %>%
    mutate(
      larger_dataset = case_when(
        n_Asner > n_ILO ~ "Asner",
        n_ILO > n_Asner ~ "ILO", 
        TRUE ~ "Equal"
      ),
      mean_agreement = case_when(
        abs(mean_diff) < 0.5 ~ "Very Similar",
        abs(mean_diff) < 1.0 ~ "Similar",
        abs(mean_diff) < 2.0 ~ "Different",
        TRUE ~ "Very Different"
      ),
      median_agreement = case_when(
        abs(median_diff) < 0.5 ~ "Very Similar",
        abs(median_diff) < 1.0 ~ "Similar", 
        abs(median_diff) < 2.0 ~ "Different",
        TRUE ~ "Very Different"
      ),
      ilo_higher = mean_diff > 0
    ) %>%
    select(gauci_biome, n_Asner, n_ILO, larger_dataset, 
           mean_lai_Asner, mean_lai_ILO, mean_diff, mean_agreement,
           median_lai_Asner, median_lai_ILO, median_diff, median_agreement, ilo_higher)
  
  print(kable(descriptive_summary,
              col.names = c("Biome", "n_Asner", "n_ILO", "Larger_Dataset",
                            "Mean_Asner", "Mean_ILO", "Mean_Diff", "Mean_Agreement",
                            "Median_Asner", "Median_ILO", "Median_Diff", "Median_Agreement", "ILO_Higher"),
              format = "simple"))
  
  # Summary statistics
  cat("\nSUMMARY OF DIFFERENCES:\n")
  cat(sprintf("Biomes where ILO has higher mean LAI: %d\n", sum(descriptive_summary$ilo_higher, na.rm = TRUE)))
  cat(sprintf("Biomes where Asner has higher mean LAI: %d\n", sum(!descriptive_summary$ilo_higher, na.rm = TRUE)))
  cat(sprintf("Mean absolute difference: %.2f\n", mean(abs(descriptive_summary$mean_diff), na.rm = TRUE)))
  cat(sprintf("Median absolute difference: %.2f\n", median(abs(descriptive_summary$mean_diff), na.rm = TRUE)))
  cat(sprintf("Largest difference: %.2f (%s)\n", 
              max(abs(descriptive_summary$mean_diff), na.rm = TRUE),
              descriptive_summary$gauci_biome[which.max(abs(descriptive_summary$mean_diff))]))
  
  # Agreement levels
  agreement_summary <- descriptive_summary %>%
    count(mean_agreement) %>%
    mutate(percentage = round(100 * n / sum(n), 1))
  
  cat("\nMEAN AGREEMENT LEVELS:\n")
  print(kable(agreement_summary,
              col.names = c("Agreement Level", "Count", "Percentage"),
              format = "simple"))
  
} else {
  cat("No overlapping biomes found - cannot perform comparative analysis.\n")
}

# =============================================================================
# 5. VISUALIZATIONS
# =============================================================================

cat("\nCreating visualizations...\n")

# 1. Side-by-side boxplots
p1 <- ggplot(combined_data, aes(x = fct_reorder(gauci_biome, LAI_value, median), 
                                y = LAI_value, fill = dataset)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("Asner" = "#2E86AB", "ILO" = "#A23B72")) +
  coord_flip() +
  labs(
    title = "LAI Distribution Comparison by Biome",
    subtitle = "Asner vs ILO Datasets (LAI ≤ 20)",
    x = "Gauci Biome",
    y = "Leaf Area Index (LAI)",
    fill = "Dataset"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    legend.position = "bottom"
  )

print(p1)
# CHANGED: Save to outputs folder
ggsave("data/outputs/lai_comparison_boxplots.png", p1, width = 14, height = 10, dpi = 300, bg = "white")

# 2. Mean comparison scatter plot (only for overlapping biomes)
if (nrow(both_datasets) > 0) {
  mean_comparison <- both_datasets %>%
    select(gauci_biome, mean_lai_Asner, mean_lai_ILO)
  
  p2 <- ggplot(mean_comparison, aes(x = mean_lai_Asner, y = mean_lai_ILO)) +
    geom_point(size = 3, alpha = 0.7, color = "#2E86AB") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(method = "lm", se = TRUE, color = "#A23B72", alpha = 0.3) +
    ggrepel::geom_text_repel(aes(label = str_wrap(gauci_biome, 15)), 
                             size = 2.5, max.overlaps = 10) +
    labs(
      title = "Mean LAI Comparison: Asner vs ILO",
      subtitle = "Each point represents a biome (LAI ≤ 20)",
      x = "Mean LAI (Asner Dataset)",
      y = "Mean LAI (ILO Dataset)",
      caption = "Red dashed line = perfect agreement"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  print(p2)
  # CHANGED: Save to outputs folder
  ggsave("data/outputs/lai_mean_comparison_scatter.png", p2, width = 10, height = 8, dpi = 300, bg = "white")
}

# 3. Median comparison scatter plot (only for overlapping biomes)
if (nrow(both_datasets) > 0) {
  median_comparison <- both_datasets %>%
    select(gauci_biome, median_lai_Asner, median_lai_ILO)
  
  p3 <- ggplot(median_comparison, aes(x = median_lai_Asner, y = median_lai_ILO)) +
    geom_point(size = 3, alpha = 0.7, color = "#2E86AB") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(method = "lm", se = TRUE, color = "#A23B72", alpha = 0.3) +
    ggrepel::geom_text_repel(aes(label = str_wrap(gauci_biome, 15)), 
                             size = 2.5, max.overlaps = 10) +
    labs(
      title = "Median LAI Comparison: Asner vs ILO",
      subtitle = "Each point represents a biome (robust to outliers, LAI ≤ 20)",
      x = "Median LAI (Asner Dataset)",
      y = "Median LAI (ILO Dataset)",
      caption = "Red dashed line = perfect agreement"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  print(p3)
  # CHANGED: Save to outputs folder
  ggsave("data/outputs/lai_median_comparison_scatter.png", p3, width = 10, height = 8, dpi = 300, bg = "white")
}

# 4. Difference visualization (only for overlapping biomes)
if (nrow(both_datasets) > 0) {
  p4 <- ggplot(both_datasets, aes(x = fct_reorder(gauci_biome, mean_diff), y = mean_diff)) +
    geom_col(aes(fill = mean_diff > 0), alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_fill_manual(values = c("TRUE" = "#A23B72", "FALSE" = "#2E86AB"),
                      labels = c("Asner Higher", "ILO Higher")) +
    coord_flip() +
    labs(
      title = "Mean LAI Differences by Biome",
      subtitle = "ILO - Asner (positive = ILO higher)",
      x = "Gauci Biome",
      y = "Difference in Mean LAI",
      fill = "Higher Dataset"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
  
  print(p4)
  # CHANGED: Save to outputs folder
  ggsave("data/outputs/lai_mean_differences.png", p4, width = 10, height = 8, dpi = 300, bg = "white")
}

# =============================================================================
# 6. EXPORT RESULTS
# =============================================================================

cat("\nSaving results...\n")

# Save comparison table
# CHANGED: Save to outputs folder
write_csv(comparison_table, "data/outputs/lai_biome_comparison_table.csv")

# Save complete comparison
# CHANGED: Save to outputs folder
write_csv(complete_comparison, "data/outputs/lai_complete_comparison_table.csv")

# Save descriptive summary if overlapping biomes exist
if (exists("descriptive_summary")) {
  # CHANGED: Save to outputs folder
  write_csv(descriptive_summary, "data/outputs/lai_descriptive_comparison.csv")
}

# =============================================================================
# 7. SUMMARY
# =============================================================================

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("ANALYSIS COMPLETE!\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")

cat(sprintf("Datasets compared: Asner (%d records) vs ILO (%d records)\n", 
            sum(combined_data$dataset == "Asner"),
            sum(combined_data$dataset == "ILO")))
cat(sprintf("LAI filtering: Values > 0 and ≤ 20 retained\n"))
cat(sprintf("Biomes with data in both datasets: %d\n", length(overlap)))

if (exists("descriptive_summary")) {
  cat(sprintf("Mean absolute difference across overlapping biomes: %.2f LAI units\n", 
              mean(abs(descriptive_summary$mean_diff), na.rm = TRUE)))
}

cat("\nFiles created:\n")
cat("- lai_comparison_boxplots.png\n")
if (exists("p2")) cat("- lai_mean_comparison_scatter.png\n")
if (exists("p3")) cat("- lai_median_comparison_scatter.png\n")
if (exists("p4")) cat("- lai_mean_differences.png\n")
cat("- lai_biome_comparison_table.csv\n")
cat("- lai_complete_comparison_table.csv\n")
if (exists("descriptive_summary")) cat("- lai_descriptive_comparison.csv\n")

cat("\nNOTE: Statistical testing removed by design.\n")
cat("Only descriptive comparisons and visualizations provided.\n")
cat("Analysis complete!\n")