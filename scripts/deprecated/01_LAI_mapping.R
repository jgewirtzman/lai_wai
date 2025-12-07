# =============================================================================
# LAI to Gauci Biome Mapping Analysis
# 
# Purpose: Map LAI biome categories to standardized Gauci biome classifications
# Method: Two-step mapping strategy using Biomecover (primary) and Biome (fallback)
# 
# Author: [Author Name]
# Date: [Date]
# Version: 1.0
# =============================================================================

# CHANGED: Set working directory relative to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")  # Go to project root

# Load required packages
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(readr)) {
  install.packages("readr")
  library(readr)
}
if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}
if (!require(knitr)) {
  install.packages("knitr")
  library(knitr)
}
if (!require(sf)) {
  install.packages("sf")
  library(sf)
}

# Set options for reproducible output
set.seed(42)
options(stringsAsFactors = FALSE, scipen = 999)

# Turn off spherical geometry to avoid topology errors
sf_use_s2(FALSE)

# =============================================================================
# 1. DATA LOADING AND INSPECTION
# =============================================================================

# Load LAI data
cat("Loading LAI dataset...\n")
# CHANGED: Use relative path to inputs folder
lai_data <- read_csv("data/inputs/LAI_data_asner.csv", show_col_types = FALSE)

# Load Gauci biome reference data
cat("Loading Gauci biome reference data...\n")
# CHANGED: Use relative path to inputs folder
gauci_data <- read_csv("data/inputs/biome_table_gauci.csv", show_col_types = FALSE)

# Data inspection
cat("Dataset dimensions:\n")
cat(sprintf("LAI data: %d records, %d columns\n", nrow(lai_data), ncol(lai_data)))
cat(sprintf("Gauci data: %d biomes, %d columns\n", nrow(gauci_data), ncol(gauci_data)))

# Examine key columns
cat("\nUnique Biomecover values:\n")
biomecover_values <- lai_data$Biomecover %>% 
  str_trim() %>% 
  unique() %>% 
  sort()
print(biomecover_values)

cat("\nData completeness:\n")
biomecover_complete <- sum(!is.na(lai_data$Biomecover) & str_trim(lai_data$Biomecover) != "")
biome_complete <- sum(!is.na(lai_data$Biome) & str_trim(lai_data$Biome) != "")
cat(sprintf("Records with Biomecover: %d (%.1f%%)\n", 
            biomecover_complete, 100 * biomecover_complete / nrow(lai_data)))
cat(sprintf("Records with Biome: %d (%.1f%%)\n", 
            biome_complete, 100 * biome_complete / nrow(lai_data)))

# =============================================================================
# 3. SPATIAL BIOME MAPPING (Step 3)
# =============================================================================

# Function to map WWF biome codes to Gauci biomes
map_wwf_to_gauci <- function(wwf_biome_code) {
  # WWF biome code to Gauci biome mapping
  wwf_to_gauci <- c(
    "1" = "Tropical & Subtropical Moist Broadleaf Forests",   # Tropical moist forests
    "2" = "Trop. & Subtrop. Dry Broadleaf Forests",          # Tropical dry forests
    "3" = "Tropical & Subtropical Coniferous Forests",        # Tropical coniferous forests
    "4" = "Temperate Broadleaf & Mixed Forests",              # Temperate broadleaf forests
    "5" = "Temperate Conifer Forests",                        # Temperate coniferous forests
    "6" = "Boreal Forests/Taiga",                             # Boreal forests
    "7" = "Trop. & Subtrop. Grasslands, Savannas &",         # Tropical grasslands
    "8" = "Temperate Grasslands, Savannas &Shrublands",      # Temperate grasslands
    "9" = "Flooded Grasslands & Savannas",                   # Flooded grasslands
    "10" = "Montane Grasslands & Shrublands",                # Montane grasslands
    "11" = "Tundra",                                          # Tundra
    "12" = "Mediterranean Forests, Woodlands & Scrub",       # Mediterranean
    "13" = "Deserts & Xeric Shrublands",                     # Deserts
    "14" = "Mangroves"                                        # Mangroves
    # Note: codes 98 (lakes) and 99 (rock_and_ice) don't map to Gauci biomes
  )
  
  return(wwf_to_gauci[as.character(wwf_biome_code)])
}

# Step 3: Spatial mapping for ambiguous cases
map_spatial_to_gauci <- function(lai_data_with_coords) {
  
  cat("Loading WWF ecoregions for spatial mapping...\n")
  
  # CHANGED: Use relative path to official data
  wwf_shapefile <- "data/inputs/official/wwf_terr_ecos.shp"
  if (!file.exists(wwf_shapefile)) {
    cat("WARNING: WWF ecoregions shapefile not found at:", wwf_shapefile, "\n")
    return(rep("ambiguous", nrow(lai_data_with_coords)))
  }
  
  # Read the shapefile (following your original code pattern)
  wwf_ecos <- st_read(wwf_shapefile, quiet = TRUE)
  
  # Fix any invalid geometries (from your original code)
  wwf_ecos <- st_make_valid(wwf_ecos)
  
  # Create sf points from LAI coordinates
  lai_pts <- st_as_sf(
    lai_data_with_coords, 
    coords = c("Longitude", "Latitude"), 
    crs = 4326,
    remove = FALSE
  )
  
  # Spatial join with WWF ecoregions (following your original pattern)
  cat("Spatially joining LAI points to WWF ecoregions...\n")
  joined <- st_join(lai_pts, wwf_ecos, join = st_within, left = TRUE)
  
  # Diagnostic: check how many points didn't match any ecoregion
  no_match <- sum(is.na(joined$BIOME))
  cat(sprintf("Points with no WWF ecoregion match: %d\n", no_match))
  
  # Diagnostic: check unique WWF biome codes found
  unique_codes <- unique(joined$BIOME[!is.na(joined$BIOME)])
  cat(sprintf("Unique WWF biome codes found: %s\n", paste(sort(unique_codes), collapse = ", ")))
  
  # Map WWF biome codes to Gauci biomes
  spatial_gauci <- map_wwf_to_gauci(joined$BIOME)
  
  # Diagnostic: check unmapped codes
  unmapped_codes <- unique(joined$BIOME[!is.na(joined$BIOME) & is.na(spatial_gauci)])
  if (length(unmapped_codes) > 0) {
    cat(sprintf("WWF codes not mapped to Gauci biomes: %s\n", paste(unmapped_codes, collapse = ", ")))
  }
  
  # Return spatial mapping results (NA for points outside biome boundaries)
  return(ifelse(is.na(spatial_gauci), "ambiguous", spatial_gauci))
}

# =============================================================================
# 4. BIOME MAPPING FUNCTIONS
# =============================================================================
gauci_biomes <- c(
  "Boreal Forests/Taiga",
  "Deserts & Xeric Shrublands", 
  "Flooded Grasslands & Savannas",
  "Mangroves",
  "Mediterranean Forests, Woodlands & Scrub",
  "Montane Grasslands & Shrublands",
  "Rock and Ice",
  "Temperate Broadleaf & Mixed Forests",
  "Temperate Conifer Forests",
  "Temperate Grasslands, Savannas &Shrublands",
  "Tropical & Subtropical Coniferous Forests",
  "Trop. & Subtrop. Dry Broadleaf Forests",
  "Trop. & Subtrop. Grasslands, Savannas &",
  "Tropical & Subtropical Moist Broadleaf Forests",
  "Tundra"
)

cat("\nTarget Gauci biomes:\n")
for (i in seq_along(gauci_biomes)) {
  cat(sprintf("%2d. %s\n", i, gauci_biomes[i]))
}

# Step 1: Biomecover mapping function (vectorized)
map_biomecover_to_gauci <- function(biomecover) {
  # Vectorized function - handle each element
  sapply(biomecover, function(x) {
    # Handle missing or NA values
    if (is.na(x) | str_trim(x) == "" | str_to_upper(str_trim(x)) == "N/A") {
      return(NA_character_)  # Triggers fallback to Biome column
    }
    
    clean_cover <- str_to_lower(str_trim(x))
    
    # Direct mappings for standardized Biomecover values
    if (clean_cover == "tundra") {
      return("Tundra")
    }
    
    # Boreal forests
    if (str_detect(clean_cover, "forest / bo")) {
      return("Boreal Forests/Taiga")
    }
    
    # Temperate forests
    if (str_detect(clean_cover, "forest / te")) {
      if (str_detect(clean_cover, "enl|ebl")) {
        return("Temperate Conifer Forests")
      }
      if (str_detect(clean_cover, "dbl|dnl")) {
        return("Temperate Broadleaf & Mixed Forests")
      }
      return("ambiguous")
    }
    
    # Tropical forests
    if (str_detect(clean_cover, "forest / tr")) {
      if (str_detect(clean_cover, "ebl")) {
        return("Tropical & Subtropical Moist Broadleaf Forests")
      }
      if (str_detect(clean_cover, "dbl")) {
        return("Trop. & Subtrop. Dry Broadleaf Forests")
      }
      return("ambiguous")
    }
    
    # Desert
    if (clean_cover == "desert") {
      return("Deserts & Xeric Shrublands")
    }
    
    # Categories requiring additional context
    if (clean_cover %in% c("grassland", "shrub", "wetland", "crops", "plantation")) {
      return("ambiguous")
    }
    
    return("ambiguous")
  }, USE.NAMES = FALSE)
}

# Step 2: Biome fallback mapping function (vectorized)
map_biome_to_gauci <- function(biome) {
  # Vectorized function - handle each element
  sapply(biome, function(x) {
    # Handle missing or NA values
    if (is.na(x) | str_trim(x) == "" | str_to_upper(str_trim(x)) == "N/A") {
      return("ambiguous")
    }
    
    clean_biome <- str_to_lower(str_trim(x))
    
    # Tundra
    if (str_detect(clean_biome, "tundra|arctic|antarctic")) {
      return("Tundra")
    }
    
    # Boreal forests
    if (str_detect(clean_biome, "boreal|boenl|bodbl|bodnl")) {
      return("Boreal Forests/Taiga")
    }
    
    # Temperate conifer forests
    if ((str_detect(clean_biome, "temp|temperate")) & 
        str_detect(clean_biome, "conifer|evergreen|eg")) {
      return("Temperate Conifer Forests")
    }
    if (str_detect(clean_biome, "teenl|teebl")) {
      return("Temperate Conifer Forests")
    }
    
    # Temperate broadleaf forests
    if ((str_detect(clean_biome, "temp|temperate")) & 
        str_detect(clean_biome, "deciduous|mixed|hardwood|beech")) {
      return("Temperate Broadleaf & Mixed Forests")
    }
    if (str_detect(clean_biome, "tedbl|tednl|temxd|teebl")) {
      return("Temperate Broadleaf & Mixed Forests")
    }
    
    # Tropical moist forests
    if ((str_detect(clean_biome, "tropical|trop")) & 
        str_detect(clean_biome, "rain|moist|montane rain")) {
      return("Tropical & Subtropical Moist Broadleaf Forests")
    }
    if (str_detect(clean_biome, "trebl|trenl")) {
      return("Tropical & Subtropical Moist Broadleaf Forests")
    }
    
    # Tropical dry forests
    if ((str_detect(clean_biome, "tropical|trop")) & 
        str_detect(clean_biome, "dry|deciduous|seasonal")) {
      return("Trop. & Subtrop. Dry Broadleaf Forests")
    }
    if (str_detect(clean_biome, "trdbl|trbld")) {
      return("Trop. & Subtrop. Dry Broadleaf Forests")
    }
    
    # Tropical coniferous forests
    if ((str_detect(clean_biome, "tropical|trop")) & 
        str_detect(clean_biome, "conifer|pine")) {
      return("Tropical & Subtropical Coniferous Forests")
    }
    
    # Mediterranean
    if (str_detect(clean_biome, "mediterranean|maquis|chaparral|mebl|mebld")) {
      return("Mediterranean Forests, Woodlands & Scrub")
    }
    
    # Mangroves
    if (str_detect(clean_biome, "mangrove")) {
      return("Mangroves")
    }
    
    # Deserts
    if (str_detect(clean_biome, "desert|desrt|sagebrush") | 
        (str_detect(clean_biome, "arid") & str_detect(clean_biome, "shrub"))) {
      return("Deserts & Xeric Shrublands")
    }
    
    # Flooded grasslands - check BEFORE general grassland patterns
    if (str_detect(clean_biome, "wet") & str_detect(clean_biome, "meadow")) {
      return("Flooded Grasslands & Savannas")
    }
    if (str_detect(clean_biome, "marsh")) {
      return("Flooded Grasslands & Savannas")
    }
    
    # Temperate grasslands (after flooded grasslands check)
    if ((str_detect(clean_biome, "grassland|prairie")) & 
        !str_detect(clean_biome, "tropical")) {
      return("Temperate Grasslands, Savannas &Shrublands")
    }
    if (clean_biome == "grass") {
      return("Temperate Grasslands, Savannas &Shrublands")
    }
    
    # Tropical grasslands
    if (str_detect(clean_biome, "grassland") & str_detect(clean_biome, "tropical")) {
      return("Trop. & Subtrop. Grasslands, Savannas &")
    }
    
    # Savannas
    if (str_detect(clean_biome, "savanna")) {
      if (str_detect(clean_biome, "tropical|trop")) {
        return("Trop. & Subtrop. Grasslands, Savannas &")
      } else {
        return("Temperate Grasslands, Savannas &Shrublands")
      }
    }
    
    # Floodplain without clear vegetation type - ambiguous (could be forest or grassland)
    if (str_detect(clean_biome, "flood") & !str_detect(clean_biome, "grassland|meadow|savanna|marsh")) {
      return("ambiguous")
    }
    
    # General wetland without specific vegetation context - ambiguous
    if (str_detect(clean_biome, "wetland|heath") & !str_detect(clean_biome, "grassland|meadow|marsh")) {
      return("ambiguous")
    }
    
    # Montane
    if (str_detect(clean_biome, "montane|alpine|subalpine")) {
      return("Montane Grasslands & Shrublands")
    }
    
    return("ambiguous")
  }, USE.NAMES = FALSE)
}

# Three-step mapping function (vectorized)
map_lai_to_gauci_threestep <- function(biome, biomecover, spatial_result = NULL) {
  # Step 1: Try Biomecover mapping
  biomecover_result <- map_biomecover_to_gauci(biomecover)
  
  # Step 2: Try Biome mapping for ambiguous/NA cases from Step 1
  biome_result <- map_biome_to_gauci(biome)
  
  # Combine Step 1 and Step 2 results
  two_step_result <- ifelse(
    !is.na(biomecover_result) & biomecover_result != "ambiguous",
    biomecover_result,
    biome_result
  )
  
  # Step 3: Use spatial mapping for remaining ambiguous cases
  if (!is.null(spatial_result)) {
    final_result <- ifelse(
      two_step_result == "ambiguous" & !is.na(spatial_result) & spatial_result != "ambiguous",
      spatial_result,
      two_step_result
    )
  } else {
    final_result <- two_step_result
  }
  
  return(final_result)
}

# =============================================================================
# 5. APPLY MAPPING TO DATASET
# =============================================================================

cat("\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")
cat("APPLYING THREE-STEP BIOME MAPPING\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")

# Check for required coordinate columns
if (!all(c("Latitude", "Longitude") %in% names(lai_data))) {
  cat("WARNING: Latitude and Longitude columns not found in LAI data.\n")
  cat("Spatial mapping (Step 3) will be skipped.\n")
  use_spatial <- FALSE
} else {
  use_spatial <- TRUE
}

# Apply spatial mapping for ambiguous cases (if coordinates available)
if (use_spatial) {
  cat("Applying spatial mapping for coordinate-based classification...\n")
  
  # Get only records with valid coordinates for spatial mapping
  coords_available <- !is.na(lai_data$Latitude) & !is.na(lai_data$Longitude) &
    lai_data$Latitude >= -90 & lai_data$Latitude <= 90 &
    lai_data$Longitude >= -180 & lai_data$Longitude <= 180
  
  # Initialize spatial results vector
  spatial_results <- rep(NA_character_, nrow(lai_data))
  
  if (sum(coords_available) > 0) {
    # Apply spatial mapping only to records with coordinates
    spatial_results[coords_available] <- map_spatial_to_gauci(lai_data[coords_available, ])
  }
  
  # Apply three-step mapping
  lai_mapped <- lai_data %>%
    mutate(
      gauci_biome = map_lai_to_gauci_threestep(Biome, Biomecover, spatial_results),
      biomecover_step = map_biomecover_to_gauci(Biomecover),
      biome_step = map_biome_to_gauci(Biome),
      spatial_step = spatial_results
    )
} else {
  # Apply two-step mapping only
  lai_mapped <- lai_data %>%
    mutate(
      gauci_biome = map_lai_to_gauci_threestep(Biome, Biomecover, NULL),
      biomecover_step = map_biomecover_to_gauci(Biomecover),
      biome_step = map_biome_to_gauci(Biome),
      spatial_step = NA_character_
    )
}

# =============================================================================
# 6. RESULTS AND DIAGNOSTICS
# =============================================================================

# Calculate success rates
total_records <- nrow(lai_mapped)
ambiguous_count <- sum(lai_mapped$gauci_biome == "ambiguous", na.rm = TRUE)
successfully_mapped <- total_records - ambiguous_count
success_rate <- 100 * successfully_mapped / total_records
ambiguous_rate <- 100 * ambiguous_count / total_records

cat(sprintf("\nMAPPING RESULTS:\n"))
cat(sprintf("Total records: %d\n", total_records))
cat(sprintf("Successfully mapped: %d (%.1f%%)\n", successfully_mapped, success_rate))
cat(sprintf("Ambiguous: %d (%.1f%%)\n", ambiguous_count, ambiguous_rate))

# Summary by Gauci biome
mapping_summary <- lai_mapped %>%
  count(gauci_biome, sort = TRUE, name = "n_records") %>%
  mutate(
    percentage = 100 * n_records / total_records,
    gauci_biome = factor(gauci_biome, levels = c(gauci_biomes, "ambiguous"))
  ) %>%
  arrange(gauci_biome)

cat("\nMAPPING SUMMARY BY GAUCI BIOME:\n")
print(kable(mapping_summary, 
            col.names = c("Gauci Biome", "Records", "Percentage"),
            digits = 1,
            format = "simple"))

# Analyze three-step process effectiveness
step_analysis <- lai_mapped %>%
  summarise(
    biomecover_clear = sum(!is.na(biomecover_step) & biomecover_step != "ambiguous"),
    biomecover_ambiguous = sum(is.na(biomecover_step) | biomecover_step == "ambiguous"),
    rescued_by_biome = sum((is.na(biomecover_step) | biomecover_step == "ambiguous") & 
                             !is.na(biome_step) & biome_step != "ambiguous"),
    rescued_by_spatial = if(use_spatial) {
      sum((is.na(biomecover_step) | biomecover_step == "ambiguous") & 
            (is.na(biome_step) | biome_step == "ambiguous") &
            !is.na(spatial_step) & spatial_step != "ambiguous")
    } else { 0 },
    final_ambiguous = sum(gauci_biome == "ambiguous", na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("\nTHREE-STEP PROCESS ANALYSIS:\n"))
cat(sprintf("Step 1 (Biomecover) clear results: %d (%.1f%%)\n", 
            step_analysis$biomecover_clear, 
            100 * step_analysis$biomecover_clear / total_records))
cat(sprintf("Step 1 (Biomecover) ambiguous: %d (%.1f%%)\n", 
            step_analysis$biomecover_ambiguous,
            100 * step_analysis$biomecover_ambiguous / total_records))
cat(sprintf("Step 2 (Biome) rescued ambiguous cases: %d (%.1f%%)\n", 
            step_analysis$rescued_by_biome,
            100 * step_analysis$rescued_by_biome / total_records))

if (use_spatial) {
  cat(sprintf("Step 3 (Spatial) rescued remaining cases: %d (%.1f%%)\n", 
              step_analysis$rescued_by_spatial,
              100 * step_analysis$rescued_by_spatial / total_records))
} else {
  cat("Step 3 (Spatial) skipped - no coordinate data available\n")
}

cat(sprintf("Final ambiguous cases: %d (%.1f%%)\n", 
            step_analysis$final_ambiguous,
            100 * step_analysis$final_ambiguous / total_records))

# Examples of successful mappings
cat("\nEXAMPLES OF SUCCESSFUL MAPPINGS:\n")
examples <- lai_mapped %>%
  filter(gauci_biome != "ambiguous") %>%
  select(Biome, Biomecover, gauci_biome) %>%
  group_by(gauci_biome) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  arrange(gauci_biome)

print(kable(examples,
            col.names = c("Original Biome", "Biomecover", "Mapped Gauci Biome"),
            format = "simple"))

# Examples of ambiguous cases
cat("\nEXAMPLES OF AMBIGUOUS CASES:\n")
ambiguous_examples <- lai_mapped %>%
  filter(gauci_biome == "ambiguous") %>%
  count(Biome, Biomecover, sort = TRUE) %>%
  head(10) %>%
  select(-n)

print(kable(ambiguous_examples,
            col.names = c("Original Biome", "Biomecover"),
            format = "simple"))

# =============================================================================
# 7. EXPORT RESULTS
# =============================================================================

# Save mapped dataset
# CHANGED: Save to outputs folder
output_file <- "data/outputs/lai_data_with_gauci_mapping.csv"
write_csv(lai_mapped, output_file)
cat(sprintf("\nMapped dataset saved to: %s\n", output_file))

# Save mapping summary
# CHANGED: Save to outputs folder
summary_file <- "data/outputs/gauci_mapping_summary.csv"
write_csv(mapping_summary, summary_file)
cat(sprintf("Mapping summary saved to: %s\n", summary_file))

cat("\nAnalysis complete!\n")

# =============================================================================
# 8. SESSION INFO FOR REPRODUCIBILITY
# =============================================================================

cat("\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")
cat("SESSION INFORMATION\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")
print(sessionInfo())