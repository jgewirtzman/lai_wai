# =============================================================================
# End-to-End Workflow (FOREST-ONLY):
#  - Iio & Ito (2014) LAI database -> Forest biome LAI
#  - Merge with Gauci biome table (forest biomes only)
#  - Compute woody:leaf surface area ratios per m² of woody ground (forest only)
#

# =============================================================================

# =============================================================================
# 0. SETUP
# =============================================================================

required_packages <- c(
  "dplyr", "readr", "stringr", "ggplot2", "forcats",
  "tidyr", "sf", "knitr"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

if (!require("RColorBrewer", quietly = TRUE)) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

if (!require("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
  library(patchwork)
}

sf::sf_use_s2(FALSE)
set.seed(42)
options(stringsAsFactors = FALSE, scipen = 999)

# Directory layout (adjust if needed)
lai_csv_path   <- "data/inputs/LAI_Woody_Plants_1231/data/LAI_Woody_Plants_Database.csv"
wwf_shapefile  <- "data/inputs/official/wwf_terr_ecos.shp"
gauci_path     <- "data/inputs/biome_table_gauci.csv"
out_dir        <- "data/outputs"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# =============================================================================
# 1. LOAD & CLEAN IIO LAI DATABASE
# =============================================================================

cat("Loading and cleaning Iio LAI database...\n")

# Structure of the Iio CSV:
#  - Rows 1–5: metadata header
#  - Row 6: column names
#  - Row 7: units ("Year", "m2 m-2", "Decimal Degree", etc.)
#  - Row 8+: data
iio_raw <- read_csv(
  lai_csv_path,
  skip = 5,            # start at row with column names
  show_col_types = FALSE
)

# Drop the units row (first row after header)
iio_data <- iio_raw %>%
  slice(-1)

# Clean column names (trim whitespace)
names(iio_data) <- trimws(names(iio_data))

# Convert coordinates to numeric
iio_data <- iio_data %>%
  mutate(
    Latitude  = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  )

# Replace -9999 with NA in numeric columns
iio_data <- iio_data %>%
  mutate(across(where(is.numeric), ~na_if(., -9999)))

cat(sprintf("Iio data: %d records, %d columns\n", nrow(iio_data), ncol(iio_data)))

coords_complete <- sum(!is.na(iio_data$Latitude) & !is.na(iio_data$Longitude))
cat(sprintf("Records with coordinates: %d (%.1f%%)\n",
            coords_complete, 100 * coords_complete / nrow(iio_data)))

# =============================================================================
# 2. CHOOSE LAI VARIABLE (CORRECTED WHERE AVAILABLE)
# =============================================================================
# Use Corrected_total_LAI_HSA when available, otherwise fall back to Total_LAI_HSA.
# Both are on an HSA (half-surface-area) basis, which we treat as "1-sided" LAI.

cat("\nConstructing unified LAI variable (Corrected_total_LAI_HSA preferred)...\n")

iio_data <- iio_data %>%
  mutate(
    Total_LAI_HSA           = as.numeric(Total_LAI_HSA),
    Corrected_total_LAI_HSA = as.numeric(Corrected_total_LAI_HSA),
    LAI_HSA_1sided          = ifelse(
      !is.na(Corrected_total_LAI_HSA),
      Corrected_total_LAI_HSA,
      Total_LAI_HSA
    )
  )

lai_complete <- sum(!is.na(iio_data$LAI_HSA_1sided))
cat(sprintf("Records with usable LAI_HSA_1sided: %d (%.1f%%)\n",
            lai_complete, 100 * lai_complete / nrow(iio_data)))

# Basic sanity filter on LAI values
iio_data <- iio_data %>%
  filter(!is.na(LAI_HSA_1sided) & LAI_HSA_1sided > 0 & LAI_HSA_1sided < 50)

cat(sprintf("Records after LAI filter: %d\n", nrow(iio_data)))

# =============================================================================
# 3. MAP IIO POINTS TO WWF BIOIMES → GAUCI BIOMES
# =============================================================================

cat("\nLoading WWF ecoregions shapefile...\n")

if (!file.exists(wwf_shapefile)) {
  stop("ERROR: WWF ecoregions shapefile not found at: ", wwf_shapefile)
}

wwf_ecos <- st_read(wwf_shapefile, quiet = TRUE) %>%
  st_make_valid()

# Filter to valid coordinates
coords_valid <- !is.na(iio_data$Latitude) & !is.na(iio_data$Longitude) &
  iio_data$Latitude >= -90 & iio_data$Latitude <= 90 &
  iio_data$Longitude >= -180 & iio_data$Longitude <= 180

cat(sprintf("Records with valid coordinates: %d\n", sum(coords_valid)))

iio_pts <- st_as_sf(
  iio_data[coords_valid, ],
  coords = c("Longitude", "Latitude"),
  crs = 4326,
  remove = FALSE
)

cat("Spatially joining Iio points to WWF ecoregions...\n")
joined <- st_join(iio_pts, wwf_ecos, join = st_within, left = TRUE)

# WWF BIOME → Gauci / WWF short names
wwf_to_gauci <- c(
  "1"  = "Tropical & Subtropical Moist Broadleaf Forests",
  "2"  = "Trop. & Subtrop. Dry Broadleaf Forests",
  "3"  = "Tropical & Subtropical Coniferous Forests",
  "4"  = "Temperate Broadleaf & Mixed Forests",
  "5"  = "Temperate Conifer Forests",
  "6"  = "Boreal Forests/Taiga",
  "7"  = "Trop. & Subtrop. Grasslands, Savannas &",
  "8"  = "Temperate Grasslands, Savannas &Shrublands",
  "9"  = "Flooded Grasslands & Savannas",
  "10" = "Montane Grasslands & Shrublands",
  "11" = "Tundra",
  "12" = "Mediterranean Forests, Woodlands & Scrub",
  "13" = "Deserts & Xeric Shrublands",
  "14" = "Mangroves"
)

joined$gauci_biome <- wwf_to_gauci[as.character(joined$BIOME)]

no_match <- sum(is.na(joined$gauci_biome))
cat(sprintf("Points with no WWF biome match: %d (%.1f%%)\n",
            no_match, 100 * no_match / nrow(joined)))

joined$gauci_biome[is.na(joined$gauci_biome)] <- "ambiguous"

iio_mapped <- joined %>%
  st_drop_geometry()

# Quick mapping summary
mapping_summary <- iio_mapped %>%
  count(gauci_biome, sort = TRUE, name = "n_records") %>%
  mutate(percentage = round(100 * n_records / nrow(iio_mapped), 1))

cat("\nMAPPING SUMMARY BY GAUCI BIOME (Iio data):\n")
print(mapping_summary)

# =============================================================================
# 4. PER-FOREST-BIOME LAI STATISTICS (WITH IQR FILTER)
# =============================================================================

cat("\nComputing per-forest-biome LAI statistics with IQR outlier filtering...\n")

# Define forest biomes (WWF/Gauci naming)
forest_biomes <- c(
  "Boreal Forests/Taiga",
  "Temperate Broadleaf & Mixed Forests",
  "Temperate Conifer Forests",
  "Mediterranean Forests, Woodlands & Scrub",
  "Tropical & Subtropical Coniferous Forests",
  "Trop. & Subtrop. Dry Broadleaf Forests",
  "Tropical & Subtropical Moist Broadleaf Forests",
  "Mangroves"
)

iio_classified <- iio_mapped %>%
  filter(gauci_biome %in% forest_biomes) %>%   # FORESTS ONLY
  mutate(
    gauci_biome = factor(gauci_biome),
    LAI_numeric = LAI_HSA_1sided
  ) %>%
  filter(!is.na(LAI_numeric) & LAI_numeric > 0 & LAI_numeric < 50)

cat(sprintf("Analyzing %d records with valid LAI and forest biome classifications\n",
            nrow(iio_classified)))
cat(sprintf("LAI range (forest): %.2f - %.2f\n",
            min(iio_classified$LAI_numeric), max(iio_classified$LAI_numeric)))
cat(sprintf("Forest biomes represented: %d\n",
            length(unique(iio_classified$gauci_biome))))

# IQR bounds per forest biome
iio_with_bounds <- iio_classified %>%
  group_by(gauci_biome) %>%
  mutate(
    q1          = quantile(LAI_numeric, 0.25, na.rm = TRUE),
    q3          = quantile(LAI_numeric, 0.75, na.rm = TRUE),
    iqr         = q3 - q1,
    lower_bound = q1 - 1.5 * iqr,
    upper_bound = q3 + 1.5 * iqr,
    is_outlier  = LAI_numeric < lower_bound | LAI_numeric > upper_bound
  ) %>%
  ungroup()

iqr_summary <- iio_with_bounds %>%
  summarise(
    total = n(),
    kept = sum(!is_outlier),
    removed = sum(is_outlier),
    pct_removed = round(100 * removed / total, 1)
  )
print(iqr_summary)

iio_iqr_filtered <- iio_with_bounds %>%
  filter(!is_outlier)

# Outlier summary
outlier_summary <- iio_with_bounds %>%
  group_by(gauci_biome) %>%
  summarise(
    total        = n(),
    within_iqr   = sum(!is_outlier),
    low_outliers = sum(LAI_numeric < lower_bound),
    high_outliers= sum(LAI_numeric > upper_bound),
    percent_kept = round(100 * within_iqr / total, 1),
    .groups      = "drop"
  ) %>%
  arrange(desc(total))

cat("\nOutlier filtering summary by forest biome:\n")
print(outlier_summary)

# IQR-filtered LAI stats per forest biome
stats_iqr <- iio_iqr_filtered %>%
  group_by(gauci_biome) %>%
  summarise(
    n          = n(),
    mean_lai   = mean(LAI_numeric, na.rm = TRUE),
    sd_lai     = sd(LAI_numeric, na.rm = TRUE),
    median_lai = median(LAI_numeric, na.rm = TRUE),
    q25        = quantile(LAI_numeric, 0.25, na.rm = TRUE),
    q75        = quantile(LAI_numeric, 0.75, na.rm = TRUE),
    min_lai    = min(LAI_numeric, na.rm = TRUE),
    max_lai    = max(LAI_numeric, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  arrange(desc(median_lai))

# Canonical LAI per forest biome
lai_by_forest_biome <- stats_iqr %>%
  mutate(
    gauci_biome = as.character(gauci_biome),
    final_lai   = median_lai   # switch to mean_lai if preferred
  )

write_csv(lai_by_forest_biome, file.path(out_dir, "forest_final_lai_by_biome.csv"))

cat("\nSaved per-forest-biome LAI to forest_final_lai_by_biome.csv\n")

# =============================================================================
# 5. LOAD GAUCI BIOME TABLE & MERGE LAI (FORESTS ONLY)
# =============================================================================

cat("\nLoading Gauci biome table and merging forest LAI...\n")

biome_table <- read_csv(gauci_path, show_col_types = FALSE)

lai_data <- read_csv(
  file.path(out_dir, "forest_final_lai_by_biome.csv"),
  show_col_types = FALSE
)

# Clean the text a bit (remove odd quotes, spaces)
biome_table <- biome_table %>%
  mutate(Biome = str_trim(str_replace_all(Biome, "[«»]", "")))

lai_data <- lai_data %>%
  mutate(gauci_biome = str_trim(str_replace_all(gauci_biome, "[«»]", "")))

# We only care about forest biomes defined above
forest_biome_list <- forest_biomes

# Mapping table: Gauci table -> LAI table (forest biomes)
biome_mapping <- data.frame(
  gauci_table_name = forest_biome_list,
  lai_biome_name   = forest_biome_list,  # same labels in lai_data$gauci_biome
  stringsAsFactors = FALSE
)

# Full names for plotting / captions
full_biome_names <- data.frame(
  lai_biome_name = c(
    "Boreal Forests/Taiga",
    "Temperate Broadleaf & Mixed Forests",
    "Temperate Conifer Forests",
    "Mediterranean Forests, Woodlands & Scrub",
    "Tropical & Subtropical Coniferous Forests",
    "Trop. & Subtrop. Dry Broadleaf Forests",
    "Tropical & Subtropical Moist Broadleaf Forests",
    "Mangroves"
  ),
  full_name = c(
    "Taiga and Boreal Forest",
    "Temperate Broadleaf and Mixed Forests",
    "Temperate Coniferous Forests",
    "Mediterranean Forests, Woodlands, and Scrub",
    "Tropical and Subtropical Coniferous Forests",
    "Tropical and Subtropical Dry Broadleaf Forests",
    "Tropical and Subtropical Moist Broadleaf Forests",
    "Mangroves"
  ),
  stringsAsFactors = FALSE
)

biome_mapping <- biome_mapping %>%
  left_join(full_biome_names, by = "lai_biome_name")

cat("\nVerifying forest biome mappings (Gauci table vs LAI data):\n")
for (i in seq_len(nrow(biome_mapping))) {
  gauci_exists <- biome_mapping$gauci_table_name[i] %in% biome_table$Biome
  lai_exists   <- biome_mapping$lai_biome_name[i]   %in% lai_data$gauci_biome
  
  status <- paste0(
    ifelse(gauci_exists, "Gauci OK", "Gauci MISSING"), " / ",
    ifelse(lai_exists,   "LAI OK",   "LAI MISSING")
  )
  cat(sprintf("  %-60s : %s\n", biome_mapping$full_name[i], status))
}

# Join Gauci table with LAI (FORESTS ONLY)
biome_with_lai <- biome_table %>%
  filter(Biome %in% forest_biome_list) %>%
  left_join(
    biome_mapping %>% rename(Biome = gauci_table_name),
    by = "Biome"
  ) %>%
  left_join(
    lai_data %>%
      select(gauci_biome, final_lai, n, sd_lai) %>%
      rename(lai_biome_name = gauci_biome,
             LAI_1sided     = final_lai,
             n_observations = n),
    by = "lai_biome_name"
  ) %>%
  mutate(
    Biome_Full = ifelse(is.na(full_name), Biome, full_name),
    LAI_2sided = LAI_1sided * 2
  )

cat(sprintf("\nForest biomes with LAI data: %d\n",
            sum(!is.na(biome_with_lai$LAI_1sided))))

# =============================================================================
# 6. ASSIGN SINGLE WAI PER FOREST BIOME & COMPUTE WOODY:LEAF RATIOS
# =============================================================================

cat("\nAssigning WAI and computing woody:leaf ratios for forests only...\n")

# Single WAI per forest biome:
#  - Tropical moist broadleaf: 4.12
#  - All other forest biomes: 3.07

biome_with_lai <- biome_with_lai %>%
  mutate(
    WAI = case_when(
      Biome == "Tropical & Subtropical Moist Broadleaf Forests" ~ 4.12,
      TRUE                                                      ~ 3.07
    ),
    ratio_woody_leaf_1sided = WAI / LAI_1sided,
    ratio_woody_leaf_2sided = WAI / LAI_2sided
  )

cat("\nForest woody:leaf ratios (2-sided, per m² of woody ground):\n")
print(
  biome_with_lai %>%
    select(Biome_Full, LAI_2sided, WAI, ratio_woody_leaf_2sided) %>%
    arrange(desc(ratio_woody_leaf_2sided))
)

# Simple global summary (plot-weighted by number of Iio plots in each biome)
forest_ratio_summary <- biome_with_lai %>%
  filter(!is.na(LAI_1sided)) %>%
  mutate(weight = ifelse(is.na(n_observations), 1, n_observations)) %>%
  summarise(
    n_biomes              = n(),
    mean_ratio_1sided     = weighted.mean(ratio_woody_leaf_1sided, w = weight, na.rm = TRUE),
    mean_ratio_2sided     = weighted.mean(ratio_woody_leaf_2sided, w = weight, na.rm = TRUE)
  )

cat("\nGLOBAL MEAN FOREST WOODY:LEAF RATIOS (per m² of woody ground):\n")
print(forest_ratio_summary)

# =============================================================================
# 7. PLOT: FOREST WOODY:LEAF RATIOS (2-SIDED)
# =============================================================================

cat("\nCreating forest woody:leaf ratio plot (2-sided)...\n")

plot_data <- biome_with_lai %>%
  filter(!is.na(LAI_1sided), !is.na(ratio_woody_leaf_2sided)) %>%
  select(Biome_Full, ratio_woody_leaf_2sided)

p_ratio_forest <- plot_data %>%
  ggplot(aes(
    x    = fct_reorder(Biome_Full, ratio_woody_leaf_2sided),
    y    = ratio_woody_leaf_2sided
  )) +
  geom_col(alpha = 0.9, width = 0.7, fill = "#8B4513") +
  coord_flip() +
  labs(
    title    = "Woody-to-Leaf Surface Area Ratios in Forest Biomes",
    subtitle = "Single WAI per biome (4.12 for tropical moist, 3.07 otherwise)\nvs. Iio LAI (2-sided, HSA basis)",
    x        = NULL,
    y        = "Woody : Leaf Surface Area Ratio (2-sided, per m² of woody ground)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(size = 10, hjust = 0.5),
    axis.text.y      = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

print(p_ratio_forest)

ggsave(
  filename = file.path(out_dir, "forest_woody_leaf_ratios_2sided.png"),
  plot     = p_ratio_forest,
  width    = 10,
  height   = 6,
  dpi      = 300,
  bg       = "white"
)

# Compact table
forest_ratio_table <- plot_data %>%
  transmute(
    Biome = Biome_Full,
    woody_leaf_ratio_2sided = ratio_woody_leaf_2sided
  ) %>%
  arrange(desc(woody_leaf_ratio_2sided))

cat("\nFOREST WOODY:LEAF RATIOS (2-sided):\n")
print(knitr::kable(
  forest_ratio_table,
  col.names = c("Biome", "Woody:Leaf Ratio (2-sided)"),
  digits    = 2,
  format    = "simple"
))

# =============================================================================
# 8. EXPORT RESULTS (FORESTS ONLY)
# =============================================================================

cat("\nExporting forest-only results...\n")

final_forest_biome_table <- biome_with_lai %>%
  select(
    Biome, Biome_Full,
    `MAT degC weighted`, `Biome area MHa`,
    LAI_1sided, LAI_2sided,
    n_observations, sd_lai,
    WAI,
    ratio_woody_leaf_1sided,
    ratio_woody_leaf_2sided
  )

write_csv(final_forest_biome_table,
          file.path(out_dir, "forest_biome_table_with_lai_and_ratios.csv"))
write_csv(lai_by_forest_biome,
          file.path(out_dir, "forest_lai_values_by_biome.csv"))
write_csv(forest_ratio_table,
          file.path(out_dir, "forest_woody_leaf_ratios_2sided.csv"))

cat("\n==================================================\n")
cat("FOREST-ONLY ANALYSIS COMPLETE!\n")
cat("Files created in data/outputs/:\n")
cat("  - forest_final_lai_by_biome.csv\n")
cat("  - forest_lai_values_by_biome.csv\n")
cat("  - forest_biome_table_with_lai_and_ratios.csv\n")
cat("  - forest_woody_leaf_ratios_2sided.csv\n")
cat("  - forest_woody_leaf_ratios_2sided.png\n")
cat("==================================================\n")


library(stringr)
library(forcats)
library(ggplot2)

# -------------------------------------------------------------------------
# Forest-only plot data
# -------------------------------------------------------------------------

plot_data <- biome_with_lai %>%
  filter(!is.na(LAI_1sided), !is.na(ratio_woody_leaf_2sided)) %>%
  select(Biome_Full, ratio_woody_leaf_2sided)

plot_data_insert <- plot_data %>%
  mutate(
    biome_wrapped = str_wrap(Biome_Full, width = 36)  # Limit to ~2 lines
  )

# -------------------------------------------------------------------------
# Forest-only woody:leaf ratio plot (2-sided)
# -------------------------------------------------------------------------

p_insert <- plot_data_insert %>%
  ggplot(aes(
    x    = fct_reorder(biome_wrapped, ratio_woody_leaf_2sided),
    y    = ratio_woody_leaf_2sided,
    fill = ratio_woody_leaf_2sided
  )) +
  geom_col(alpha = 1, width = 0.6) +  # thin bars for white space
  geom_text(
    aes(label = sprintf("%.2f", ratio_woody_leaf_2sided)),
    hjust = 1.1, vjust = 0.5,
    size = 3.5, fontface = "bold", color = "white"
  ) +
  scale_fill_gradient2(
    low  = "#6aa078",   # leaf-dominant
    mid  = "#f7cb62",   # balanced
    high = "#5d412a",   # wood-dominant
    midpoint = 0.5,    # forest ratios are ~0.28–0.65, so 0.4 is a nice "balanced" point
    guide = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title   = "Woody:Leaf Surface Area Ratio in Forest Biomes",
    x       = NULL,
    y       = NULL,
    caption = "Calculated from field-observed leaf area (Iio & Ito, 2014)\n
and TLS-derived woody area indices (Gauci et al.)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title   = element_text(size = 12, hjust = 0.5, face = "bold", margin = margin(b = 5)),
    plot.caption = element_text(size = 10, hjust = 0.5, face = "italic"),
    axis.text.y  = element_text(
      size   = 11,
      face   = "plain",
      margin = margin(r = 8),
      lineheight = 0.75
    ),
    axis.text.x      = element_blank(),
    axis.ticks       = element_blank(),
    plot.margin      = margin(t = 8, r = 25, b = 8, l = 8),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 1)
  )

print(p_insert)

ggsave(
  filename = file.path(out_dir, "forest_woody_leaf_ratios_insert_2sided.png"),
  plot     = p_insert,
  width    = 8,
  height   = 5,
  dpi      = 300,
  bg       = "white"
)




# Diagnostic script to check what processing is actually needed
# Run this after loading the Iio data but before filtering

library(dplyr)
library(readr)

# Adjust path as needed
lai_csv_path <- "data/inputs/LAI_Woody_Plants_1231/data/LAI_Woody_Plants_Database.csv"

# Load data (same as main script)
iio_raw <- read_csv(lai_csv_path, skip = 5, show_col_types = FALSE)
iio_data <- iio_raw %>% slice(-1)
names(iio_data) <- trimws(names(iio_data))

iio_data <- iio_data %>%
  mutate(
    Total_LAI_HSA = as.numeric(Total_LAI_HSA),
    Corrected_total_LAI_HSA = as.numeric(Corrected_total_LAI_HSA)
  )

# ============================================================
# Q1: Is the fallback ever used?
# ============================================================
cat("=" , rep("=", 60), "\n", sep = "")
cat("Q1: FALLBACK FROM CORRECTED TO TOTAL_LAI_HSA\n")
cat("=" , rep("=", 60), "\n", sep = "")

has_corrected <- sum(!is.na(iio_data$Corrected_total_LAI_HSA))
has_total_only <- sum(is.na(iio_data$Corrected_total_LAI_HSA) & !is.na(iio_data$Total_LAI_HSA))
has_neither <- sum(is.na(iio_data$Corrected_total_LAI_HSA) & is.na(iio_data$Total_LAI_HSA))

cat(sprintf("Records with Corrected_total_LAI_HSA: %d (%.1f%%)\n", 
            has_corrected, 100 * has_corrected / nrow(iio_data)))
cat(sprintf("Records needing fallback to Total_LAI_HSA: %d (%.1f%%)\n", 
            has_total_only, 100 * has_total_only / nrow(iio_data)))
cat(sprintf("Records with neither (dropped): %d (%.1f%%)\n", 
            has_neither, 100 * has_neither / nrow(iio_data)))

if (has_total_only == 0) {
  cat("\n>>> FALLBACK NOT NEEDED - all records have Corrected_total_LAI_HSA\n")
} else {
  cat(sprintf("\n>>> FALLBACK IS USED for %d records\n", has_total_only))
}

# ============================================================
# Q2: Does the physiological filter (0 < LAI < 50) remove anything?
# ============================================================
cat("\n")
cat("=" , rep("=", 60), "\n", sep = "")
cat("Q2: PHYSIOLOGICAL BOUNDS FILTER (0 < LAI < 50)\n")
cat("=" , rep("=", 60), "\n", sep = "")

iio_data <- iio_data %>%
  mutate(
    LAI_HSA_1sided = ifelse(!is.na(Corrected_total_LAI_HSA), 
                            Corrected_total_LAI_HSA, 
                            Total_LAI_HSA)
  )

has_lai <- sum(!is.na(iio_data$LAI_HSA_1sided))
below_zero <- sum(iio_data$LAI_HSA_1sided <= 0, na.rm = TRUE)
above_50 <- sum(iio_data$LAI_HSA_1sided >= 50, na.rm = TRUE)
within_bounds <- sum(iio_data$LAI_HSA_1sided > 0 & iio_data$LAI_HSA_1sided < 50, na.rm = TRUE)

cat(sprintf("Records with LAI value: %d\n", has_lai))
cat(sprintf("Records with LAI <= 0: %d\n", below_zero))
cat(sprintf("Records with LAI >= 50: %d\n", above_50))
cat(sprintf("Records within bounds (0-50): %d\n", within_bounds))

if (below_zero == 0 && above_50 == 0) {
  cat("\n>>> PHYSIOLOGICAL FILTER NOT NEEDED - all values within bounds\n")
} else {
  cat(sprintf("\n>>> PHYSIOLOGICAL FILTER REMOVES %d records\n", below_zero + above_50))
}

# ============================================================
# Q3: Does IQR filtering remove records (forest biomes only)?
# ============================================================
cat("\n")
cat("=" , rep("=", 60), "\n", sep = "")
cat("Q3: IQR OUTLIER FILTERING (per forest biome)\n")
cat("=" , rep("=", 60), "\n", sep = "")

# This requires spatial mapping first - simplified check using all data
# In practice, run this AFTER spatial mapping in the main script

cat("(Run after spatial mapping to check IQR filtering impact)\n")
cat("Add this code after line ~180 in main script:\n\n")

cat('
# After iio_with_bounds is created:
iqr_summary <- iio_with_bounds %>%
  summarise(
    total = n(),
    kept = sum(!is_outlier),
    removed = sum(is_outlier),
    pct_removed = round(100 * removed / total, 1)
  )
print(iqr_summary)

if (iqr_summary$removed == 0) {
  cat(">>> IQR FILTER NOT NEEDED - no outliers detected\\n")
} else {
  cat(sprintf(">>> IQR FILTER REMOVES %d records (%.1f%%)\\n", 
              iqr_summary$removed, iqr_summary$pct_removed))
}
')