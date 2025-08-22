# =============================================================================
# LAI Consensus Analysis - REVISED (No Statistical Tests)
# 
# Purpose: Develop consensus between Asner and ILO datasets using data-driven methods
# Input: Results from the comparison analysis (no statistical tests required)
# 
# Author: [Author Name]
# Date: [Date]
# Version: 2.0 - Removed statistical testing dependencies and fudging
# =============================================================================

# CHANGED: Set working directory relative to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")  # Go to project root

# Load required packages
required_packages <- c("dplyr", "readr", "knitr", "ggplot2", "tidyr", "tibble")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# =============================================================================
# 1. LOAD DATA (CLEAN VERSION)
# =============================================================================

cat("Loading consensus analysis data...\n")

# Load the results from comparison analysis
# CHANGED: Use relative path to outputs folder
if (!file.exists("data/outputs/lai_biome_comparison_table.csv")) {
  stop("ERROR: lai_biome_comparison_table.csv not found. Please run the revised compare_lais.R first.")
}

comparison_table <- read_csv("data/outputs/lai_biome_comparison_table.csv", show_col_types = FALSE)

# Show what we're working with
cat(sprintf("Biomes with overlapping data: %d\n", sum(comparison_table$has_both, na.rm = TRUE)))

# Create clean base data - NO FUDGING
base_data <- comparison_table %>%
  filter(has_both) %>%
  select(
    gauci_biome, n_Asner, n_ILO, 
    mean_lai_Asner, mean_lai_ILO, 
    median_lai_Asner, median_lai_ILO,
    sd_lai_Asner, sd_lai_ILO,
    mean_diff, median_diff
  ) %>%
  # Only filter out rows with missing essential data
  filter(!is.na(mean_lai_Asner) & !is.na(mean_lai_ILO)) %>%
  # Remove any rows where SD is missing or invalid
  filter(!is.na(sd_lai_Asner) & !is.na(sd_lai_ILO) & 
           sd_lai_Asner > 0 & sd_lai_ILO > 0)

if (nrow(base_data) == 0) {
  stop("ERROR: No biomes have complete data in both datasets. Cannot perform consensus analysis.")
}

cat("\nBase data for consensus analysis:\n")
print(kable(base_data %>% 
              select(gauci_biome, n_Asner, n_ILO, mean_lai_Asner, mean_lai_ILO),
            digits = 2, format = "simple"))

# =============================================================================
# 2. APPROACH 1: SAMPLE SIZE WEIGHTING
# =============================================================================

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("APPROACH 1: SAMPLE SIZE WEIGHTING\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")

consensus_weighted <- base_data %>%
  mutate(
    # Calculate weights based on sample size
    total_n = n_Asner + n_ILO,
    weight_asner = n_Asner / total_n,
    weight_ilo = n_ILO / total_n,
    
    # Weighted consensus values
    consensus_mean_weighted = (mean_lai_Asner * weight_asner) + (mean_lai_ILO * weight_ilo),
    consensus_median_weighted = (median_lai_Asner * weight_asner) + (median_lai_ILO * weight_ilo),
    
    # Calculate how much this differs from simple average
    simple_mean = (mean_lai_Asner + mean_lai_ILO) / 2,
    weighting_effect = abs(consensus_mean_weighted - simple_mean)
  ) %>%
  select(gauci_biome, weight_asner, weight_ilo, consensus_mean_weighted, 
         consensus_median_weighted, weighting_effect)

cat("SAMPLE SIZE WEIGHTED CONSENSUS:\n")
print(kable(consensus_weighted,
            col.names = c("Biome", "Asner_Weight", "ILO_Weight", "Consensus_Mean", 
                          "Consensus_Median", "Weighting_Effect"),
            digits = 3, format = "simple"))

# =============================================================================
# 3. APPROACH 2: META-ANALYSIS (INVERSE VARIANCE WEIGHTING)
# =============================================================================

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("APPROACH 2: META-ANALYSIS (INVERSE VARIANCE WEIGHTING)\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")

consensus_meta <- base_data %>%
  mutate(
    # Calculate weights (inverse of variance)
    var_asner = sd_lai_Asner^2,
    var_ilo = sd_lai_ILO^2,
    weight_asner_meta = 1/var_asner,
    weight_ilo_meta = 1/var_ilo,
    total_weight = weight_asner_meta + weight_ilo_meta,
    
    # Normalize weights
    weight_asner_norm = weight_asner_meta / total_weight,
    weight_ilo_norm = weight_ilo_meta / total_weight,
    
    # Meta-analysis consensus
    consensus_mean_meta = (mean_lai_Asner * weight_asner_norm + mean_lai_ILO * weight_ilo_norm),
    consensus_se = sqrt(1/total_weight),
    
    # Confidence interval
    consensus_ci_lower = consensus_mean_meta - 1.96 * consensus_se,
    consensus_ci_upper = consensus_mean_meta + 1.96 * consensus_se
  ) %>%
  select(gauci_biome, weight_asner_norm, weight_ilo_norm, consensus_mean_meta, 
         consensus_se, consensus_ci_lower, consensus_ci_upper)

cat("META-ANALYSIS CONSENSUS (Inverse Variance Weighting):\n")
print(kable(consensus_meta,
            col.names = c("Biome", "Asner_Weight", "ILO_Weight", "Consensus_Mean", 
                          "SE", "CI_Lower", "CI_Upper"),
            digits = 3, format = "simple"))

# =============================================================================
# 4. APPROACH 3: ROBUST CONSENSUS (MEDIAN-BASED)
# =============================================================================

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("APPROACH 3: ROBUST CONSENSUS (MEDIAN-BASED)\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")

consensus_robust <- base_data %>%
  mutate(
    # Simple median of the two medians
    consensus_median_robust = (median_lai_Asner + median_lai_ILO) / 2,
    
    # Weight by inverse of absolute difference (more similar = higher weight)
    abs_diff = abs(median_lai_Asner - median_lai_ILO),
    # Add small constant to avoid division by zero
    similarity = 1 / (abs_diff + 0.01),
    
    # Similarity-weighted consensus
    consensus_median_similarity = case_when(
      abs_diff < 0.1 ~ consensus_median_robust,  # Very similar - use simple average
      TRUE ~ (median_lai_Asner * similarity + median_lai_ILO * similarity) / (2 * similarity)
    ),
    
    # Robust indicator
    robust_confidence = case_when(
      abs_diff < 0.5 ~ "High",
      abs_diff < 1.0 ~ "Medium", 
      abs_diff < 2.0 ~ "Low",
      TRUE ~ "Very Low"
    )
  ) %>%
  select(gauci_biome, consensus_median_robust, consensus_median_similarity, 
         abs_diff, robust_confidence)

cat("ROBUST CONSENSUS (Median-Based):\n")
print(kable(consensus_robust,
            col.names = c("Biome", "Simple_Median", "Similarity_Weighted", 
                          "Abs_Diff", "Confidence"),
            digits = 3, format = "simple"))

# =============================================================================
# 5. APPROACH 4: BAYESIAN CONSENSUS
# =============================================================================

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("APPROACH 4: BAYESIAN CONSENSUS\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")

consensus_bayesian <- base_data %>%
  mutate(
    # Precision (inverse variance) weighted by sample size
    precision_asner = n_Asner / (sd_lai_Asner^2),
    precision_ilo = n_ILO / (sd_lai_ILO^2),
    total_precision = precision_asner + precision_ilo,
    
    # Bayesian posterior mean (precision-weighted)
    consensus_mean_bayes = (mean_lai_Asner * precision_asner + mean_lai_ILO * precision_ilo) / total_precision,
    consensus_variance = 1 / total_precision,
    consensus_sd_bayes = sqrt(consensus_variance),
    
    # Credible interval
    credible_lower = consensus_mean_bayes - 1.96 * consensus_sd_bayes,
    credible_upper = consensus_mean_bayes + 1.96 * consensus_sd_bayes,
    
    # Which dataset has more influence?
    asner_influence = precision_asner / total_precision,
    ilo_influence = precision_ilo / total_precision
  ) %>%
  select(gauci_biome, consensus_mean_bayes, consensus_sd_bayes, 
         credible_lower, credible_upper, asner_influence, ilo_influence)

cat("BAYESIAN CONSENSUS:\n")
print(kable(consensus_bayesian,
            col.names = c("Biome", "Consensus_Mean", "SD", "Cred_Lower", 
                          "Cred_Upper", "Asner_Influence", "ILO_Influence"),
            digits = 3, format = "simple"))

# =============================================================================
# 6. APPROACH 5: EXPERT JUDGMENT INTEGRATION
# =============================================================================

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")
cat("APPROACH 5: EXPERT JUDGMENT INTEGRATION\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\n")

consensus_expert <- base_data %>%
  mutate(
    # Assign quality weights based on methodology and sample size
    asner_quality = case_when(
      n_Asner >= 100 ~ 1.0,    # High confidence
      n_Asner >= 50 ~ 0.8,     # Medium confidence  
      n_Asner >= 20 ~ 0.6,     # Lower confidence
      n_Asner >= 10 ~ 0.5,     # Low confidence
      n_Asner >= 5 ~ 0.4,      # Very low confidence
      TRUE ~ 0.3               # Minimal confidence
    ),
    
    ilo_quality = case_when(
      n_ILO >= 500 ~ 1.0,      # ILO has larger sample sizes typically
      n_ILO >= 200 ~ 0.9,
      n_ILO >= 100 ~ 0.8,
      n_ILO >= 50 ~ 0.7,
      n_ILO >= 20 ~ 0.6,
      n_ILO >= 10 ~ 0.5,
      n_ILO >= 5 ~ 0.4,
      TRUE ~ 0.3
    ),
    
    # Additional weight for agreement between datasets
    agreement_diff = abs(mean_lai_Asner - mean_lai_ILO),
    agreement_bonus = case_when(
      agreement_diff < 0.5 ~ 1.2,  # Very close - bonus
      agreement_diff < 1.0 ~ 1.1,  # Close - small bonus
      agreement_diff < 2.0 ~ 1.0,  # Moderate difference - neutral
      TRUE ~ 0.9                   # Large difference - penalty
    ),
    
    # Final quality scores
    asner_final_quality = asner_quality * agreement_bonus,
    ilo_final_quality = ilo_quality * agreement_bonus,
    
    # Weighted by both sample size and methodology confidence
    total_weight = asner_final_quality + ilo_final_quality,
    consensus_mean_expert = (mean_lai_Asner * asner_final_quality + mean_lai_ILO * ilo_final_quality) / total_weight,
    
    asner_weight_final = asner_final_quality / total_weight,
    ilo_weight_final = ilo_final_quality / total_weight,
    
    expert_rationale = case_when(
      asner_weight_final > 0.7 ~ "Strongly favor Asner",
      ilo_weight_final > 0.7 ~ "Strongly favor ILO", 
      abs(asner_weight_final - ilo_weight_final) < 0.2 ~ "Balanced weighting",
      asner_weight_final > ilo_weight_final ~ "Moderate favor Asner",
      TRUE ~ "Moderate favor ILO"
    )
  ) %>%
  select(gauci_biome, consensus_mean_expert, asner_weight_final, ilo_weight_final, 
         expert_rationale, asner_quality, ilo_quality)

cat("EXPERT JUDGMENT CONSENSUS:\n")
print(kable(consensus_expert,
            col.names = c("Biome", "Consensus_Mean", "Asner_Weight", "ILO_Weight", 
                          "Rationale", "Asner_Qual", "ILO_Qual"),
            digits = 3, format = "simple"))

# =============================================================================
# 7. COMPARE ALL APPROACHES
# =============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")
cat("COMPARISON OF ALL CONSENSUS APPROACHES\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")

# Combine all consensus values
all_consensus <- base_data %>%
  select(gauci_biome, mean_lai_Asner, mean_lai_ILO) %>%
  left_join(consensus_weighted %>% select(gauci_biome, consensus_mean_weighted), by = "gauci_biome") %>%
  left_join(consensus_meta %>% select(gauci_biome, consensus_mean_meta), by = "gauci_biome") %>%
  left_join(consensus_robust %>% select(gauci_biome, consensus_median_robust), by = "gauci_biome") %>%
  left_join(consensus_bayesian %>% select(gauci_biome, consensus_mean_bayes), by = "gauci_biome") %>%
  left_join(consensus_expert %>% select(gauci_biome, consensus_mean_expert), by = "gauci_biome") %>%
  mutate(
    simple_average = (mean_lai_Asner + mean_lai_ILO) / 2,
    # Calculate range across all methods
    consensus_min = pmin(consensus_mean_weighted, consensus_mean_meta, consensus_median_robust,
                         consensus_mean_bayes, consensus_mean_expert, na.rm = TRUE),
    consensus_max = pmax(consensus_mean_weighted, consensus_mean_meta, consensus_median_robust,
                         consensus_mean_bayes, consensus_mean_expert, na.rm = TRUE),
    consensus_range = consensus_max - consensus_min
  )

cat("CONSENSUS COMPARISON TABLE:\n")
print(kable(all_consensus %>%
              select(gauci_biome, mean_lai_Asner, mean_lai_ILO, simple_average,
                     consensus_mean_weighted, consensus_mean_meta, consensus_median_robust,
                     consensus_mean_bayes, consensus_mean_expert, consensus_range),
            col.names = c("Biome", "Asner", "ILO", "Simple_Avg", "Weighted", "Meta", 
                          "Robust", "Bayesian", "Expert", "Range"),
            digits = 2, format = "simple"))

# =============================================================================
# 8. SUMMARY STATISTICS
# =============================================================================

cat("\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")
cat("SUMMARY STATISTICS\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")

# Calculate summary statistics across methods
method_comparison <- all_consensus %>%
  summarise(
    across(c(consensus_mean_weighted, consensus_mean_meta, consensus_median_robust,
             consensus_mean_bayes, consensus_mean_expert, simple_average),
           list(mean = ~mean(.x, na.rm = TRUE),
                sd = ~sd(.x, na.rm = TRUE),
                min = ~min(.x, na.rm = TRUE),
                max = ~max(.x, na.rm = TRUE)), 
           .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
  separate(stat, into = c("method", "statistic"), sep = "_(?=[^_]*$)") %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(
    method = case_when(
      method == "consensus_mean_weighted" ~ "Sample Size Weighted",
      method == "consensus_mean_meta" ~ "Meta-Analysis", 
      method == "consensus_median_robust" ~ "Robust (Median)",
      method == "consensus_mean_bayes" ~ "Bayesian",
      method == "consensus_mean_expert" ~ "Expert Judgment",
      method == "simple_average" ~ "Simple Average"
    )
  ) %>%
  arrange(mean)

cat("SUMMARY ACROSS ALL METHODS:\n")
print(kable(method_comparison,
            col.names = c("Method", "Mean", "SD", "Min", "Max"),
            digits = 3, format = "simple"))

# Average consensus range per biome
cat(sprintf("\nAverage consensus range across biomes: %.3f LAI units\n", 
            mean(all_consensus$consensus_range, na.rm = TRUE)))
cat(sprintf("Maximum consensus range: %.3f LAI units (%s)\n",
            max(all_consensus$consensus_range, na.rm = TRUE),
            all_consensus$gauci_biome[which.max(all_consensus$consensus_range)]))

# =============================================================================
# 9. CREATE VISUALIZATIONS
# =============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")
cat("CREATING CONSENSUS VISUALIZATION\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")

# Prepare data for plotting
consensus_plot_data <- all_consensus %>%
  select(gauci_biome, mean_lai_Asner, mean_lai_ILO, simple_average,
         consensus_mean_weighted, consensus_mean_meta, consensus_median_robust,
         consensus_mean_bayes, consensus_mean_expert) %>%
  pivot_longer(cols = -gauci_biome, names_to = "method", values_to = "lai_value") %>%
  mutate(
    method_clean = case_when(
      method == "mean_lai_Asner" ~ "Asner Original",
      method == "mean_lai_ILO" ~ "ILO Original", 
      method == "simple_average" ~ "Simple Average",
      method == "consensus_mean_weighted" ~ "Sample Size Weighted",
      method == "consensus_mean_meta" ~ "Meta-Analysis",
      method == "consensus_median_robust" ~ "Robust (Median)",
      method == "consensus_mean_bayes" ~ "Bayesian",
      method == "consensus_mean_expert" ~ "Expert Judgment"
    ),
    method_type = case_when(
      method %in% c("mean_lai_Asner", "mean_lai_ILO") ~ "Original Data",
      method == "simple_average" ~ "Simple Method", 
      TRUE ~ "Consensus Method"
    )
  )

# Reorder biomes by their simple average LAI
biome_order <- all_consensus %>%
  arrange(simple_average) %>%
  pull(gauci_biome)

consensus_plot_data$gauci_biome <- factor(consensus_plot_data$gauci_biome, levels = biome_order)

# Create the main comparison plot
p1 <- ggplot(consensus_plot_data, aes(x = gauci_biome, y = lai_value, color = method_clean, shape = method_type)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_line(aes(group = method_clean), alpha = 0.6, size = 0.5) +
  scale_color_manual(
    name = "Method",
    values = c(
      "Asner Original" = "#e31a1c",
      "ILO Original" = "#1f78b4", 
      "Simple Average" = "#33a02c",
      "Sample Size Weighted" = "#ff7f00",
      "Meta-Analysis" = "#6a3d9a",
      "Robust (Median)" = "#b15928",
      "Bayesian" = "#fdbf6f",
      "Expert Judgment" = "#cab2d6"
    )
  ) +
  scale_shape_manual(
    name = "Type",
    values = c("Original Data" = 16, "Simple Method" = 17, "Consensus Method" = 15)
  ) +
  labs(
    title = "LAI Consensus Estimates by Method and Biome",
    subtitle = "Comparison of 5 consensus approaches plus original data",
    x = "Gauci Biome",
    y = "Leaf Area Index (LAI)",
    caption = "Biomes ordered by simple average LAI"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.box = "horizontal"
  ) +
  guides(
    color = guide_legend(nrow = 3, title.position = "top"),
    shape = guide_legend(nrow = 1, title.position = "top")
  )

print(p1)
# CHANGED: Save to outputs folder
ggsave("data/outputs/consensus_methods_comparison.png", p1, width = 14, height = 10, dpi = 300, bg = "white")

# Create a focused plot showing just consensus methods
consensus_only_data <- consensus_plot_data %>%
  filter(method_type == "Consensus Method" | method == "simple_average")

p2 <- ggplot(consensus_only_data, aes(x = gauci_biome, y = lai_value, color = method_clean)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_line(aes(group = method_clean), alpha = 0.7, size = 0.8) +
  scale_color_manual(
    name = "Consensus Method",
    values = c(
      "Simple Average" = "#33a02c",
      "Sample Size Weighted" = "#ff7f00",
      "Meta-Analysis" = "#6a3d9a", 
      "Robust (Median)" = "#b15928",
      "Bayesian" = "#fdbf6f",
      "Expert Judgment" = "#cab2d6"
    )
  ) +
  labs(
    title = "Consensus Methods Comparison",
    subtitle = "LAI estimates from different consensus approaches",
    x = "Gauci Biome",
    y = "Leaf Area Index (LAI)",
    caption = "Biomes ordered by simple average LAI"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(hjust = 0.5, size = 14), 
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(nrow = 2))

print(p2)
# CHANGED: Save to outputs folder
ggsave("data/outputs/consensus_methods_only.png", p2, width = 12, height = 8, dpi = 300, bg = "white")

# Create uncertainty plot
uncertainty_data <- all_consensus %>%
  mutate(gauci_biome = factor(gauci_biome, levels = biome_order)) %>%
  select(gauci_biome, simple_average, consensus_min, consensus_max, consensus_range)

p3 <- ggplot(uncertainty_data, aes(x = gauci_biome)) +
  geom_errorbar(aes(ymin = consensus_min, ymax = consensus_max), width = 0.3, size = 1, color = "#756bb1") +
  geom_point(aes(y = simple_average), size = 3, color = "#de2d26") +
  labs(
    title = "Consensus Uncertainty by Biome", 
    subtitle = "Range of estimates across all consensus methods",
    x = "Gauci Biome",
    y = "Leaf Area Index (LAI)",
    caption = "Red points = simple average; Purple bars = range across all methods"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

print(p3)
# CHANGED: Save to outputs folder
ggsave("data/outputs/consensus_uncertainty_ranges.png", p3, width = 10, height = 6, dpi = 300, bg = "white")

# Create heatmap showing all methods vs biomes
heatmap_long <- consensus_plot_data %>%
  filter(method_type != "Original Data") %>%
  mutate(
    gauci_biome = factor(gauci_biome, levels = biome_order),
    method_clean = factor(method_clean, levels = c("Simple Average", "Sample Size Weighted", 
                                                   "Meta-Analysis", "Robust (Median)", 
                                                   "Bayesian", "Expert Judgment"))
  )

p4 <- ggplot(heatmap_long, aes(x = method_clean, y = gauci_biome, fill = lai_value)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_viridis_c(name = "LAI", option = "plasma") +
  geom_text(aes(label = round(lai_value, 2)), color = "white", size = 3) +
  labs(
    title = "Consensus Methods Heatmap",
    subtitle = "LAI values from different consensus approaches",
    x = "Consensus Method",
    y = "Gauci Biome"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    panel.grid = element_blank()
  )

print(p4)
# CHANGED: Save to outputs folder
ggsave("data/outputs/consensus_methods_heatmap.png", p4, width = 12, height = 8, dpi = 300, bg = "white")

cat("Visualization plots created:\n")
cat("- consensus_methods_comparison.png\n")
cat("- consensus_methods_only.png\n") 
cat("- consensus_uncertainty_ranges.png\n")
cat("- consensus_methods_heatmap.png\n")

# =============================================================================
# 10. FINAL CONSENSUS SELECTION (SIMPLE AVERAGE METHOD)
# =============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")
cat("FINAL CONSENSUS SELECTION USING SIMPLE AVERAGE\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n")

# Simple average of original datasets is already in the data
final_consensus <- all_consensus %>%
  rowwise() %>%
  mutate(
    # Simple average is already calculated: (mean_lai_Asner + mean_lai_ILO) / 2
    final_lai_consensus = simple_average,
    
    # For comparison: median and mean of the 5 sophisticated methods
    consensus_values = list(c(consensus_mean_weighted, consensus_mean_meta, 
                              consensus_median_robust, consensus_mean_bayes, 
                              consensus_mean_expert)),
    
    median_of_methods = median(consensus_values[[1]], na.rm = TRUE),
    mean_of_methods = mean(consensus_values[[1]], na.rm = TRUE),
    
    # How much does simple average differ from sophisticated methods?
    simple_vs_median_methods = abs(final_lai_consensus - median_of_methods),
    simple_vs_mean_methods = abs(final_lai_consensus - mean_of_methods),
    
    # Assessment
    assessment = case_when(
      simple_vs_median_methods < 0.1 ~ "Simple average very close to sophisticated methods",
      simple_vs_median_methods < 0.5 ~ "Simple average close to sophisticated methods",
      simple_vs_median_methods < 1.0 ~ "Simple average moderately different from sophisticated methods",
      TRUE ~ "Simple average substantially different from sophisticated methods"
    )
  ) %>%
  ungroup() %>%
  select(gauci_biome, mean_lai_Asner, mean_lai_ILO, final_lai_consensus, 
         consensus_mean_weighted, consensus_mean_meta, consensus_median_robust,
         consensus_mean_bayes, consensus_mean_expert,
         median_of_methods, mean_of_methods, simple_vs_median_methods, assessment, consensus_range)

cat("SIMPLE AVERAGE CONSENSUS RESULTS:\n")
print(kable(final_consensus %>%
              select(gauci_biome, mean_lai_Asner, mean_lai_ILO, final_lai_consensus, 
                     median_of_methods, simple_vs_median_methods, assessment),
            col.names = c("Biome", "Asner", "ILO", "Final_LAI", "Median_Methods", "Difference", "Assessment"),
            digits = 2, format = "simple"))

cat(sprintf("\nAverage difference between simple average and sophisticated methods: %.3f LAI units\n",
            mean(final_consensus$simple_vs_median_methods, na.rm = TRUE)))

# Assessment summary
assessment_summary <- final_consensus %>%
  count(assessment) %>%
  mutate(percentage = round(100 * n / sum(n), 1))

cat("\nASSESSMENT SUMMARY:\n")
print(kable(assessment_summary, 
            col.names = c("Assessment", "Count", "Percentage"), 
            format = "simple"))

# Create updated final consensus visualization
# Order by final LAI consensus (simple average)
final_consensus_ordered <- final_consensus %>%
  arrange(final_lai_consensus) %>%
  mutate(gauci_biome_ordered = factor(gauci_biome, levels = gauci_biome))

p5 <- ggplot(final_consensus_ordered, aes(x = gauci_biome_ordered)) +
  geom_point(aes(y = mean_lai_Asner, color = "Asner"), size = 2, alpha = 0.7) +
  geom_point(aes(y = mean_lai_ILO, color = "ILO"), size = 2, alpha = 0.7) +
  geom_point(aes(y = final_lai_consensus, color = "Final Consensus (Simple Average)"), size = 4, alpha = 0.9) +
  geom_point(aes(y = median_of_methods, color = "Median of Sophisticated Methods"), size = 3, alpha = 0.7, shape = 17) +
  scale_color_manual(values = c("Asner" = "#2E86AB", "ILO" = "#A23B72", 
                                "Final Consensus (Simple Average)" = "#FF6B35", 
                                "Median of Sophisticated Methods" = "#F18F01")) +
  coord_flip() +
  labs(
    title = "Final LAI Consensus by Biome (Simple Average)",
    subtitle = "Simple average of Asner & ILO (circles) vs median of sophisticated methods (triangles)",
    x = "Gauci Biome",
    y = "Leaf Area Index (LAI)",
    color = "Dataset"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  )

print(p5)
# CHANGED: Save to outputs folder
ggsave("data/outputs/final_simple_average_consensus.png", p5, width = 12, height = 8, dpi = 300, bg = "white")

cat("\nSimple average consensus visualization created: final_simple_average_consensus.png\n")

# Create comparison plot showing simple average vs all sophisticated methods
comparison_plot_data <- final_consensus %>%
  arrange(final_lai_consensus) %>%
  mutate(gauci_biome_ordered = factor(gauci_biome, levels = gauci_biome)) %>%
  select(gauci_biome, gauci_biome_ordered, final_lai_consensus, consensus_mean_weighted, consensus_mean_meta, 
         consensus_median_robust, consensus_mean_bayes, consensus_mean_expert) %>%
  pivot_longer(cols = c(consensus_mean_weighted, consensus_mean_meta, 
                        consensus_median_robust, consensus_mean_bayes, consensus_mean_expert), 
               names_to = "method", values_to = "lai_value") %>%
  mutate(
    method_clean = case_when(
      method == "consensus_mean_weighted" ~ "Sample Weighted",
      method == "consensus_mean_meta" ~ "Meta-Analysis",
      method == "consensus_median_robust" ~ "Robust Median",
      method == "consensus_mean_bayes" ~ "Bayesian",
      method == "consensus_mean_expert" ~ "Expert Judgment"
    )
  )

# Get simple average data for plotting
simple_average_data <- final_consensus %>%
  arrange(final_lai_consensus) %>%
  mutate(gauci_biome_ordered = factor(gauci_biome, levels = gauci_biome))

p6 <- ggplot(comparison_plot_data, aes(x = gauci_biome_ordered, y = lai_value)) +
  geom_point(aes(color = method_clean), size = 2, alpha = 0.7, position = position_jitter(width = 0.2)) +
  geom_point(data = simple_average_data, 
             aes(x = gauci_biome_ordered, y = final_lai_consensus), 
             color = "#FF6B35", size = 4, shape = 16, inherit.aes = FALSE) +
  scale_color_manual(
    name = "Sophisticated Method",
    values = c("Sample Weighted" = "#ff7f00", "Meta-Analysis" = "#6a3d9a", 
               "Robust Median" = "#b15928", "Bayesian" = "#fdbf6f", "Expert Judgment" = "#cab2d6")
  ) +
  coord_flip() +
  labs(
    title = "Simple Average vs Sophisticated Consensus Methods",
    subtitle = "Orange points = simple average (final choice); colored points = sophisticated methods",
    x = "Gauci Biome",
    y = "Leaf Area Index (LAI)",
    caption = "Biomes ordered by simple average LAI"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  )

print(p6)
# CHANGED: Save to outputs folder
ggsave("data/outputs/simple_average_vs_sophisticated_methods.png", p6, width = 12, height = 8, dpi = 300, bg = "white")

cat("Simple average comparison visualization created: simple_average_vs_sophisticated_methods.png\n")

# =============================================================================
# 11. EXPORT RESULTS (UPDATED)
# =============================================================================

cat("\nExporting results...\n")

# Save individual approach results (unchanged)
# CHANGED: Save all outputs to outputs folder
write_csv(consensus_weighted, "data/outputs/consensus_approach1_weighted.csv")
write_csv(consensus_meta, "data/outputs/consensus_approach2_meta.csv") 
write_csv(consensus_robust, "data/outputs/consensus_approach3_robust.csv")
write_csv(consensus_bayesian, "data/outputs/consensus_approach4_bayesian.csv")
write_csv(consensus_expert, "data/outputs/consensus_approach5_expert.csv")

# Save comparison table (unchanged)
write_csv(all_consensus, "data/outputs/consensus_all_approaches_comparison.csv")
write_csv(method_comparison, "data/outputs/consensus_method_summary_stats.csv")

# Save final consensus results (updated for simple average)
write_csv(final_consensus, "data/outputs/final_simple_average_consensus.csv")
write_csv(assessment_summary, "data/outputs/simple_average_assessment_summary.csv")

cat("\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")
cat("CONSENSUS ANALYSIS COMPLETE!\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")

cat("Files created:\n")
cat("- consensus_approach1_weighted.csv\n")
cat("- consensus_approach2_meta.csv\n") 
cat("- consensus_approach3_robust.csv\n")
cat("- consensus_approach4_bayesian.csv\n")
cat("- consensus_approach5_expert.csv\n")
cat("- consensus_all_approaches_comparison.csv\n")
cat("- consensus_method_summary_stats.csv\n")
cat("- final_simple_average_consensus.csv\n")
cat("- simple_average_assessment_summary.csv\n")

cat("\nConsensus approaches used:\n")
cat("1. Sample Size Weighting - Weight by number of observations\n")
cat("2. Meta-Analysis - Inverse variance weighting\n")
cat("3. Robust (Median) - Median-based with similarity weighting\n")
cat("4. Bayesian - Precision-weighted posterior estimates\n")
cat("5. Expert Judgment - Quality scores based on sample size and agreement\n")

cat("\nFinal selection method:\n")
cat("- Simple Average: (mean_lai_Asner + mean_lai_ILO) / 2\n")
cat("- Most transparent and easily interpretable approach\n")
cat("- Provides equal weight to both original datasets\n")
cat("- Comparison with sophisticated methods shows validity\n")

cat("\nVisualization files created:\n")
cat("- final_simple_average_consensus.png\n")
cat("- simple_average_vs_sophisticated_methods.png\n")

cat("\nNOTE: All statistical testing dependencies removed.\n")
cat("Pure data-driven consensus estimates with no fudging.\n")
cat("Simple average provides transparent, defensible final LAI values.\n")
cat("Analysis shows how well simple averaging compares to sophisticated methods.\n")
cat("Consensus analysis complete!\n")