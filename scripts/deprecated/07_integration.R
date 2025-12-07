# =============================================================================
# Biome LAI Integration and Surface Area Calculations
# 
# Purpose: Add consensus LAI values to Gauci biome table and calculate:
#          - 1-sided and 2-sided leaf surface areas by forest cover type
#          - Woody:leaf surface area ratios
# 
# Author: [Author Name]
# Date: [Date]
# Version: 1.2 - Fixed biome name matching and full name display
# =============================================================================

# Set working directory relative to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")  # Go to project root

# Load required packages
required_packages <- c("dplyr", "readr", "knitr", "stringr", "ggplot2", "forcats")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

if (!require("ggrepel", quietly = TRUE)) {
  install.packages("ggrepel")
  library(ggrepel)
}
if (!require("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
  library(tidyr)
}
set.seed(42)
options(stringsAsFactors = FALSE, scipen = 999)

# =============================================================================
# 1. DATA LOADING
# =============================================================================

cat("Loading biome table and consensus LAI data...\n")

# Load Gauci biome table
biome_table <- read_csv("data/inputs/biome_table_gauci.csv", show_col_types = FALSE)

# Load consensus LAI results
consensus_file <- "data/outputs/final_simple_average_consensus.csv"
if (!file.exists(consensus_file)) {
  stop("ERROR: Consensus LAI data not found. Please run 06_consensus.R first.")
}

consensus_lai <- read_csv(consensus_file, show_col_types = FALSE)

cat(sprintf("Loaded biome table: %d biomes\n", nrow(biome_table)))
cat(sprintf("Loaded consensus LAI: %d biomes with LAI data\n", nrow(consensus_lai)))

# =============================================================================
# 2. CREATE FULL BIOME NAMES MAPPING
# =============================================================================

# Create a mapping from abbreviated names to full WWF biome names
full_biome_names <- data.frame(
  short_name = c(
    "Boreal Forests/Taiga",
    "Deserts & Xeric Shrublands", 
    "Mediterranean Forests, Woodlands & Scrub",
    "Temperate Broadleaf & Mixed Forests",
    "Temperate Conifer Forests",
    "Temperate Grasslands, Savannas &Shrublands",
    "Trop. & Subtrop. Dry Broadleaf Forests",
    "Trop. & Subtrop. Grasslands, Savannas &",
    "Tropical & Subtropical Moist Broadleaf Forests"
  ),
  full_name = c(
    "Taiga and Boreal Forest",
    "Deserts and Xeric Shrublands", 
    "Mediterranean Forests, Woodlands, and Scrub",
    "Temperate Broadleaf and Mixed Forests",
    "Temperate Coniferous Forests",
    "Temperate Grasslands, Savannas, and Shrublands",
    "Tropical and Subtropical Dry Broadleaf Forests",
    "Tropical and Subtropical Grasslands, Savannas, and Shrublands",
    "Tropical and Subtropical Moist Broadleaf Forests"
  ),
  stringsAsFactors = FALSE
)

# =============================================================================
# 3. CLEAN AND MATCH BIOME NAMES
# =============================================================================

cat("\nCleaning and matching biome names...\n")

# Clean biome names to handle encoding issues and extra characters
biome_table <- biome_table %>%
  mutate(Biome_clean = str_trim(str_replace_all(Biome, "[«»]", "")))

consensus_lai <- consensus_lai %>%
  mutate(biome_clean = str_trim(str_replace_all(gauci_biome, "[«»]", "")))

# Create comprehensive mapping table using cleaned names
biome_mapping <- data.frame(
  gauci_table_name = c(
    # Exact matches first
    "Boreal Forests/Taiga",
    "Deserts & Xeric Shrublands", 
    "Mediterranean Forests, Woodlands & Scrub",
    "Temperate Broadleaf & Mixed Forests",
    "Temperate Conifer Forests",
    "Temperate Grasslands, Savannas &Shrublands",
    "Trop. & Subtrop. Dry Broadleaf Forests",
    "Trop. & Subtrop. Grasslands, Savannas &",
    # The problematic one with encoding issue
    "Tropical & Subtropical Moist Broadleaf Forests «"
  ),
  consensus_lai_name = c(
    "Boreal Forests/Taiga",
    "Deserts & Xeric Shrublands", 
    "Mediterranean Forests, Woodlands & Scrub",
    "Temperate Broadleaf & Mixed Forests",
    "Temperate Conifer Forests",
    "Temperate Grasslands, Savannas &Shrublands",
    "Trop. & Subtrop. Dry Broadleaf Forests",
    "Trop. & Subtrop. Grasslands, Savannas &",
    "Tropical & Subtropical Moist Broadleaf Forests"
  ),
  stringsAsFactors = FALSE
)

# Add full names to mapping
biome_mapping <- biome_mapping %>%
  left_join(full_biome_names %>% rename(consensus_lai_name = short_name), 
            by = "consensus_lai_name") %>%
  # For any that don't match, use the consensus name as full name
  mutate(full_name = ifelse(is.na(full_name), consensus_lai_name, full_name))

# Verify all mappings exist
cat("Verifying biome mappings:\n")
for (i in 1:nrow(biome_mapping)) {
  gauci_exists <- biome_mapping$gauci_table_name[i] %in% biome_table$Biome
  lai_exists <- biome_mapping$consensus_lai_name[i] %in% consensus_lai$gauci_biome
  
  if (!gauci_exists) {
    cat(sprintf("WARNING: '%s' not found in Gauci table\n", biome_mapping$gauci_table_name[i]))
  }
  if (!lai_exists) {
    cat(sprintf("WARNING: '%s' not found in LAI consensus\n", biome_mapping$consensus_lai_name[i]))
  }
  if (gauci_exists && lai_exists) {
    cat(sprintf("✓ Matched: '%s' -> '%s'\n", 
                biome_mapping$gauci_table_name[i], 
                biome_mapping$full_name[i]))
  }
}

# Join the data
biome_with_lai <- biome_table %>%
  left_join(
    biome_mapping %>% rename(Biome = gauci_table_name),
    by = "Biome"
  ) %>%
  left_join(
    consensus_lai %>% 
      select(gauci_biome, final_lai_consensus) %>%
      rename(consensus_lai_name = gauci_biome, LAI_1sided = final_lai_consensus),
    by = "consensus_lai_name"
  ) %>%
  # Add the full biome name for display
  mutate(Biome_Full = ifelse(is.na(full_name), Biome, full_name))

# Check results
matched_count <- sum(!is.na(biome_with_lai$LAI_1sided))
cat(sprintf("\nSUCCESS: %d out of %d expected biomes matched!\n", matched_count, nrow(consensus_lai)))

if (matched_count != nrow(consensus_lai)) {
  cat("Missing biomes:\n")
  missing_lai <- biome_with_lai %>% filter(is.na(LAI_1sided))
  print(missing_lai$Biome)
} else {
  cat("Perfect 1:1 match achieved!\n")
}

# =============================================================================
# 4. CALCULATE 2-SIDED LAI
# =============================================================================

cat("\nCalculating 2-sided LAI...\n")

biome_with_lai <- biome_with_lai %>%
  mutate(
    LAI_2sided = LAI_1sided * 2  # 2-sided LAI is approximately 2x 1-sided LAI
  )

# =============================================================================
# 5. CALCULATE LEAF SURFACE AREAS
# =============================================================================

cat("\nCalculating leaf surface areas...\n")

biome_with_lai <- biome_with_lai %>%
  mutate(
    # 1-sided leaf surface areas (Mha)
    MODIS_leaf_surf_1sided_Mha = LAI_1sided * `Biome area MHa` * `MODIS Mean Forest Cover Proportion`,
    Cnsus_leaf_surf_1sided_Mha = LAI_1sided * `Biome area MHa` * `Cnsus Mean Forest Cover Proportion`,
    `Cnsus+Shrubs_leaf_surf_1sided_Mha` = LAI_1sided * `Biome area MHa` * `Cnsus+Shrubs Mean Forest Cover Proportion`,
    
    # 2-sided leaf surface areas (Mha) 
    MODIS_leaf_surf_2sided_Mha = LAI_2sided * `Biome area MHa` * `MODIS Mean Forest Cover Proportion`,
    Cnsus_leaf_surf_2sided_Mha = LAI_2sided * `Biome area MHa` * `Cnsus Mean Forest Cover Proportion`,
    `Cnsus+Shrubs_leaf_surf_2sided_Mha` = LAI_2sided * `Biome area MHa` * `Cnsus+Shrubs Mean Forest Cover Proportion`
  )

# =============================================================================
# 6. CALCULATE WOODY:LEAF SURFACE AREA RATIOS
# =============================================================================

cat("\nCalculating woody:leaf surface area ratios...\n")

biome_with_lai <- biome_with_lai %>%
  mutate(
    # Woody:leaf ratios (1-sided)
    MODIS_woody_leaf_ratio_1sided = `MODIS wood surf. area Mha` / MODIS_leaf_surf_1sided_Mha,
    Cnsus_woody_leaf_ratio_1sided = `Cnsus wood surf. area Mha` / Cnsus_leaf_surf_1sided_Mha,
    `Cnsus+Shrubs_woody_leaf_ratio_1sided` = `Cnsus+Shrubs wood surface area Mha` / `Cnsus+Shrubs_leaf_surf_1sided_Mha`,
    
    # Woody:leaf ratios (2-sided)
    MODIS_woody_leaf_ratio_2sided = `MODIS wood surf. area Mha` / MODIS_leaf_surf_2sided_Mha,
    Cnsus_woody_leaf_ratio_2sided = `Cnsus wood surf. area Mha` / Cnsus_leaf_surf_2sided_Mha,
    `Cnsus+Shrubs_woody_leaf_ratio_2sided` = `Cnsus+Shrubs wood surface area Mha` / `Cnsus+Shrubs_leaf_surf_2sided_Mha`
  )

# =============================================================================
# 7. DISPLAY RESULTS WITH FULL NAMES
# =============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""))
cat("\n")
cat("BIOME LAI AND SURFACE AREA RESULTS\n")
cat(paste(rep("=", 80), collapse = ""))
cat("\n")

# Show LAI values for matched biomes only - using full names
cat("LAI VALUES BY BIOME:\n")
lai_summary <- biome_with_lai %>%
  filter(!is.na(LAI_1sided)) %>%
  select(Biome_Full, LAI_1sided, LAI_2sided) %>%
  arrange(desc(LAI_1sided))

print(kable(lai_summary,
            col.names = c("Biome", "1-sided LAI", "2-sided LAI"),
            digits = 2, format = "simple"))

# Show 1-sided leaf surface areas - using full names
cat("\n1-SIDED LEAF SURFACE AREAS (Mha):\n")
leaf_1sided_summary <- biome_with_lai %>%
  filter(!is.na(LAI_1sided)) %>%
  select(Biome_Full, MODIS_leaf_surf_1sided_Mha, Cnsus_leaf_surf_1sided_Mha, `Cnsus+Shrubs_leaf_surf_1sided_Mha`) %>%
  arrange(desc(MODIS_leaf_surf_1sided_Mha))

print(kable(leaf_1sided_summary,
            col.names = c("Biome", "MODIS", "Census", "Census+Shrubs"),
            digits = 1, format = "simple"))

# Show 2-sided leaf surface areas - using full names
cat("\n2-SIDED LEAF SURFACE AREAS (Mha):\n")
leaf_2sided_summary <- biome_with_lai %>%
  filter(!is.na(LAI_1sided)) %>%
  select(Biome_Full, MODIS_leaf_surf_2sided_Mha, Cnsus_leaf_surf_2sided_Mha, `Cnsus+Shrubs_leaf_surf_2sided_Mha`) %>%
  arrange(desc(MODIS_leaf_surf_2sided_Mha))

print(kable(leaf_2sided_summary,
            col.names = c("Biome", "MODIS", "Census", "Census+Shrubs"),
            digits = 1, format = "simple"))

# Show woody:leaf ratios (1-sided) - using full names
cat("\nWOODY:LEAF SURFACE AREA RATIOS (1-sided LAI):\n")
ratio_1sided_summary <- biome_with_lai %>%
  filter(!is.na(LAI_1sided)) %>%
  select(Biome_Full, MODIS_woody_leaf_ratio_1sided, Cnsus_woody_leaf_ratio_1sided, `Cnsus+Shrubs_woody_leaf_ratio_1sided`) %>%
  arrange(desc(MODIS_woody_leaf_ratio_1sided))

print(kable(ratio_1sided_summary,
            col.names = c("Biome", "MODIS", "Census", "Census+Shrubs"),
            digits = 2, format = "simple"))

# Show woody:leaf ratios (2-sided) - using full names
cat("\nWOODY:LEAF SURFACE AREA RATIOS (2-sided LAI):\n")
ratio_2sided_summary <- biome_with_lai %>%
  filter(!is.na(LAI_1sided)) %>%
  select(Biome_Full, MODIS_woody_leaf_ratio_2sided, Cnsus_woody_leaf_ratio_2sided, `Cnsus+Shrubs_woody_leaf_ratio_2sided`) %>%
  arrange(desc(MODIS_woody_leaf_ratio_2sided))

print(kable(ratio_2sided_summary,
            col.names = c("Biome", "MODIS", "Census", "Census+Shrubs"),
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

# Global totals (only for biomes with LAI data)
biomes_with_data <- biome_with_lai %>% filter(!is.na(LAI_1sided))

global_totals <- biomes_with_data %>%
  summarise(
    # LAI stats
    n_biomes = n(),
    mean_lai_1sided = round(mean(LAI_1sided, na.rm = TRUE), 2),
    mean_lai_2sided = round(mean(LAI_2sided, na.rm = TRUE), 2),
    total_biome_area_Mha = sum(`Biome area MHa`, na.rm = TRUE),
    
    # 1-sided leaf surface totals
    total_MODIS_leaf_1sided_Mha = round(sum(MODIS_leaf_surf_1sided_Mha, na.rm = TRUE), 1),
    total_Cnsus_leaf_1sided_Mha = round(sum(Cnsus_leaf_surf_1sided_Mha, na.rm = TRUE), 1),
    total_CnsusShrubs_leaf_1sided_Mha = round(sum(`Cnsus+Shrubs_leaf_surf_1sided_Mha`, na.rm = TRUE), 1),
    
    # 2-sided leaf surface totals
    total_MODIS_leaf_2sided_Mha = round(sum(MODIS_leaf_surf_2sided_Mha, na.rm = TRUE), 1),
    total_Cnsus_leaf_2sided_Mha = round(sum(Cnsus_leaf_surf_2sided_Mha, na.rm = TRUE), 1),
    total_CnsusShrubs_leaf_2sided_Mha = round(sum(`Cnsus+Shrubs_leaf_surf_2sided_Mha`, na.rm = TRUE), 1),
    
    # Woody surface totals
    total_MODIS_wood_Mha = sum(`MODIS wood surf. area Mha`, na.rm = TRUE),
    total_Cnsus_wood_Mha = sum(`Cnsus wood surf. area Mha`, na.rm = TRUE),
    total_CnsusShrubs_wood_Mha = sum(`Cnsus+Shrubs wood surface area Mha`, na.rm = TRUE),
    
    # Global woody:leaf ratios
    global_MODIS_woody_leaf_ratio_1sided = round(total_MODIS_wood_Mha / total_MODIS_leaf_1sided_Mha, 2),
    global_Cnsus_woody_leaf_ratio_1sided = round(total_Cnsus_wood_Mha / total_Cnsus_leaf_1sided_Mha, 2),
    global_CnsusShrubs_woody_leaf_ratio_1sided = round(total_CnsusShrubs_wood_Mha / total_CnsusShrubs_leaf_1sided_Mha, 2),
    
    global_MODIS_woody_leaf_ratio_2sided = round(total_MODIS_wood_Mha / total_MODIS_leaf_2sided_Mha, 2),
    global_Cnsus_woody_leaf_ratio_2sided = round(total_Cnsus_wood_Mha / total_Cnsus_leaf_2sided_Mha, 2),
    global_CnsusShrubs_woody_leaf_ratio_2sided = round(total_CnsusShrubs_wood_Mha / total_CnsusShrubs_leaf_2sided_Mha, 2)
  )

cat("GLOBAL TOTALS AND AVERAGES:\n")
cat(sprintf("Biomes with LAI data: %d out of %d total biomes\n", global_totals$n_biomes, nrow(consensus_lai)))
cat(sprintf("Total biome area: %.0f Mha\n", global_totals$total_biome_area_Mha))
cat(sprintf("Mean 1-sided LAI: %.2f\n", global_totals$mean_lai_1sided))
cat(sprintf("Mean 2-sided LAI: %.2f\n", global_totals$mean_lai_2sided))

cat("\nGLOBAL LEAF SURFACE AREAS:\n")
cat("1-sided (Mha):\n")
cat(sprintf("  MODIS: %.1f\n", global_totals$total_MODIS_leaf_1sided_Mha))
cat(sprintf("  Census: %.1f\n", global_totals$total_Cnsus_leaf_1sided_Mha))
cat(sprintf("  Census+Shrubs: %.1f\n", global_totals$total_CnsusShrubs_leaf_1sided_Mha))

cat("2-sided (Mha):\n")
cat(sprintf("  MODIS: %.1f\n", global_totals$total_MODIS_leaf_2sided_Mha))
cat(sprintf("  Census: %.1f\n", global_totals$total_Cnsus_leaf_2sided_Mha))
cat(sprintf("  Census+Shrubs: %.1f\n", global_totals$total_CnsusShrubs_leaf_2sided_Mha))

cat("\nGLOBAL WOODY SURFACE AREAS (Mha):\n")
cat(sprintf("  MODIS: %.0f\n", global_totals$total_MODIS_wood_Mha))
cat(sprintf("  Census: %.0f\n", global_totals$total_Cnsus_wood_Mha))
cat(sprintf("  Census+Shrubs: %.0f\n", global_totals$total_CnsusShrubs_wood_Mha))

cat("\nGLOBAL WOODY:LEAF RATIOS:\n")
cat("1-sided LAI basis:\n")
cat(sprintf("  MODIS: %.2f\n", global_totals$global_MODIS_woody_leaf_ratio_1sided))
cat(sprintf("  Census: %.2f\n", global_totals$global_Cnsus_woody_leaf_ratio_1sided))
cat(sprintf("  Census+Shrubs: %.2f\n", global_totals$global_CnsusShrubs_woody_leaf_ratio_1sided))

cat("2-sided LAI basis:\n")
cat(sprintf("  MODIS: %.2f\n", global_totals$global_MODIS_woody_leaf_ratio_2sided))
cat(sprintf("  Census: %.2f\n", global_totals$global_Cnsus_woody_leaf_ratio_2sided))
cat(sprintf("  Census+Shrubs: %.2f\n", global_totals$global_CnsusShrubs_woody_leaf_ratio_2sided))

# =============================================================================
# 9. CREATE WOODY:LEAF RATIO PLOTS WITH FULL NAMES
# =============================================================================

cat("\nCreating woody:leaf ratio plots...\n")

# Prepare data for plotting (MODIS 2-sided ratios only) - using full names
plot_data <- biome_with_lai %>%
  filter(!is.na(LAI_1sided)) %>%
  select(Biome_Full, MODIS_woody_leaf_ratio_2sided) %>%
  arrange(MODIS_woody_leaf_ratio_2sided)

# Create the main plot with green-to-brown color ramp and full names
p1 <- plot_data %>%
  ggplot(aes(x = fct_reorder(Biome_Full, MODIS_woody_leaf_ratio_2sided), 
             y = MODIS_woody_leaf_ratio_2sided,
             fill = MODIS_woody_leaf_ratio_2sided)) +
  geom_col(alpha = 0.9, width = 0.7) +
  scale_fill_gradient2(
    low = "#228B22",      # Forest green (leaf dominant)
    mid = "#DAA520",      # Golden rod (balanced) 
    high = "#8B4513",     # Saddle brown (wood dominant)
    midpoint = 1,
    guide = "none"        # Remove legend
  ) +
  coord_flip() +
  labs(
    title = "Woody:Leaf Surface Area Ratios by Biome",
    subtitle = "2-sided LAI basis",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.text.y = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

print(p1)
ggsave("data/outputs/woody_leaf_ratios_modis_color_ramp.png", p1, 
       width = 14, height = 8, dpi = 300, bg = "white")

# Also create a summary table showing the values with full names
cat("\nWOODY:LEAF RATIOS (MODIS, 2-sided LAI) - FULL BIOME NAMES:\n")
ratio_table <- plot_data %>%
  mutate(
    interpretation = case_when(
      MODIS_woody_leaf_ratio_2sided > 1.1 ~ "Wood Dominant",
      MODIS_woody_leaf_ratio_2sided < 0.9 ~ "Leaf Dominant", 
      TRUE ~ "Balanced"
    )
  ) %>%
  arrange(desc(MODIS_woody_leaf_ratio_2sided))

print(kable(ratio_table,
            col.names = c("Biome", "Woody:Leaf Ratio", "Interpretation"),
            digits = 2, format = "simple"))

cat("Woody:leaf ratio plot created:\n")
cat("- woody_leaf_ratios_modis_color_ramp.png\n")

# =============================================================================
# 10. EXPORT RESULTS WITH FULL NAMES
# =============================================================================

cat("\nExporting enhanced biome table...\n")

# Reorder columns for better organization, including full names
final_biome_table <- biome_with_lai %>%
  select(
    # Original and full biome names
    Biome, Biome_Full,
    
    # Original columns
    `MAT degC weighted`, `Biome area MHa`,
    `MODIS Mean Forest Cover Proportion`, `Cnsus Mean Forest Cover Proportion`, `Cnsus+Shrubs Mean Forest Cover Proportion`,
    `MODIS wood surf. area Mha`, `Cnsus wood surf. area Mha`, `Cnsus+Shrubs wood surface area Mha`,
    
    # New LAI columns
    LAI_1sided, LAI_2sided,
    
    # 1-sided leaf surface areas
    MODIS_leaf_surf_1sided_Mha, Cnsus_leaf_surf_1sided_Mha, `Cnsus+Shrubs_leaf_surf_1sided_Mha`,
    
    # 2-sided leaf surface areas
    MODIS_leaf_surf_2sided_Mha, Cnsus_leaf_surf_2sided_Mha, `Cnsus+Shrubs_leaf_surf_2sided_Mha`,
    
    # Woody:leaf ratios (1-sided)
    MODIS_woody_leaf_ratio_1sided, Cnsus_woody_leaf_ratio_1sided, `Cnsus+Shrubs_woody_leaf_ratio_1sided`,
    
    # Woody:leaf ratios (2-sided)
    MODIS_woody_leaf_ratio_2sided, Cnsus_woody_leaf_ratio_2sided, `Cnsus+Shrubs_woody_leaf_ratio_2sided`
  )

# Save enhanced biome table
write_csv(final_biome_table, "data/outputs/biome_table_enhanced_with_lai.csv")

# Save global summary
write_csv(global_totals, "data/outputs/global_surface_area_summary.csv")

# Save mapping table for reference (with full names)
write_csv(biome_mapping, "data/outputs/biome_name_mapping_reference.csv")

# Save summary tables with full names for easy reference
write_csv(lai_summary, "data/outputs/lai_values_by_biome_full_names.csv")
write_csv(ratio_table, "data/outputs/woody_leaf_ratios_full_names.csv")

cat("\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")
cat("ANALYSIS COMPLETE!\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")

cat("Files created:\n")
cat("- biome_table_enhanced_with_lai.csv\n")
cat("- global_surface_area_summary.csv\n")
cat("- biome_name_mapping_reference.csv\n")
cat("- lai_values_by_biome_full_names.csv\n")
cat("- woody_leaf_ratios_full_names.csv\n")

cat("\nColumns added to biome table:\n")
cat("- Biome_Full: Full WWF terrestrial biome names\n")
cat("- LAI_1sided: Consensus 1-sided LAI values\n")
cat("- LAI_2sided: 2-sided LAI (2x 1-sided)\n")
cat("- MODIS/Cnsus/Cnsus+Shrubs_leaf_surf_1sided_Mha: 1-sided leaf surface areas\n")
cat("- MODIS/Cnsus/Cnsus+Shrubs_leaf_surf_2sided_Mha: 2-sided leaf surface areas\n")
cat("- MODIS/Cnsus/Cnsus+Shrubs_woody_leaf_ratio_1sided: Woody:leaf ratios (1-sided basis)\n")
cat("- MODIS/Cnsus/Cnsus+Shrubs_woody_leaf_ratio_2sided: Woody:leaf ratios (2-sided basis)\n")

cat("\nInterpretation notes:\n")
cat("- LAI values from consensus analysis of Asner + ILO datasets\n")
cat("- Surface areas = LAI × Biome Area × Forest Cover Proportion\n")
cat("- Woody:leaf ratios = Woody surface area ÷ Leaf surface area\n")
cat("- Higher ratios = more woody relative to leaf surface\n")
cat("- 2-sided ratios are half the 1-sided ratios (same woody, 2x leaf surface)\n")
cat("- Full biome names follow WWF terrestrial biome classification\n")

if (matched_count == nrow(consensus_lai)) {
  cat("\nSUCCESS: Perfect 1:1 biome matching achieved!\n")
} else {
  cat(sprintf("\nWARNING: Only %d out of %d biomes matched. Check biome names.\n", matched_count, nrow(consensus_lai)))
}

cat("\nBiome LAI integration complete with full biome names!\n")