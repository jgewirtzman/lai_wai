# =============================================================================
# ILO LAI to Gauci Biome Mapping Analysis
# 
# Purpose: Map ILO LAI dataset to standardized Gauci biome classifications
# Method: Three-step mapping strategy using PFT (primary), climate+geography 
#         (secondary), and spatial WWF data (fallback)
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

# Load ILO LAI data
cat("Loading ILO LAI dataset...\n")
# CHANGED: Use relative path to inputs folder
ilo_data <- read_csv("data/inputs/lai_data_ilo.csv", show_col_types = FALSE)

# Load Gauci biome reference data (from your original analysis)
cat("Loading Gauci biome reference data...\n")
# CHANGED: Use relative path to inputs folder
gauci_data <- read_csv("data/inputs/biome_table_gauci.csv", show_col_types = FALSE)

# Data inspection
cat("Dataset dimensions:\n")
cat(sprintf("ILO data: %d records, %d columns\n", nrow(ilo_data), ncol(ilo_data)))
cat(sprintf("Gauci data: %d biomes, %d columns\n", nrow(gauci_data), ncol(gauci_data)))

# Examine key columns for mapping
cat("\nUnique PFT (Plant Functional Type) values:\n")
pft_values <- ilo_data$PFT %>% 
  str_trim() %>% 
  unique() %>% 
  sort()
print(pft_values)

cat("\nUnique Vegetation_status values:\n")
veg_status_values <- ilo_data$Vegetation_status %>% 
  str_trim() %>% 
  unique() %>% 
  sort()
print(veg_status_values)

cat("\nData completeness:\n")
pft_complete <- sum(!is.na(ilo_data$PFT) & str_trim(ilo_data$PFT) != "")
coords_complete <- sum(!is.na(ilo_data$Latitude) & !is.na(ilo_data$Longitude))
climate_complete <- sum(!is.na(ilo_data$`MAT_(Literature value)`) & !is.na(ilo_data$`MAP_(Literature value)`))

cat(sprintf("Records with PFT: %d (%.1f%%)\n", 
            pft_complete, 100 * pft_complete / nrow(ilo_data)))
cat(sprintf("Records with coordinates: %d (%.1f%%)\n", 
            coords_complete, 100 * coords_complete / nrow(ilo_data)))
cat(sprintf("Records with climate data: %d (%.1f%%)\n", 
            climate_complete, 100 * climate_complete / nrow(ilo_data)))

# =============================================================================
# 2. DEFINE TARGET BIOMES (same as your original)
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

# =============================================================================
# 3. SPATIAL BIOME MAPPING FUNCTION (reuse from your code)
# =============================================================================

# Function to map WWF biome codes to Gauci biomes (same as your original)
map_wwf_to_gauci <- function(wwf_biome_code) {
  wwf_to_gauci <- c(
    "1" = "Tropical & Subtropical Moist Broadleaf Forests",
    "2" = "Trop. & Subtrop. Dry Broadleaf Forests",
    "3" = "Tropical & Subtropical Coniferous Forests",
    "4" = "Temperate Broadleaf & Mixed Forests",
    "5" = "Temperate Conifer Forests",
    "6" = "Boreal Forests/Taiga",
    "7" = "Trop. & Subtrop. Grasslands, Savannas &",
    "8" = "Temperate Grasslands, Savannas &Shrublands",
    "9" = "Flooded Grasslands & Savannas",
    "10" = "Montane Grasslands & Shrublands",
    "11" = "Tundra",
    "12" = "Mediterranean Forests, Woodlands & Scrub",
    "13" = "Deserts & Xeric Shrublands",
    "14" = "Mangroves"
  )
  
  return(wwf_to_gauci[as.character(wwf_biome_code)])
}

# Spatial mapping function (adapted from your original)
map_spatial_to_gauci <- function(ilo_data_with_coords) {
  cat("Loading WWF ecoregions for spatial mapping...\n")
  
  # CHANGED: Use relative path to official data
  wwf_shapefile <- "data/inputs/official/wwf_terr_ecos.shp"
  if (!file.exists(wwf_shapefile)) {
    cat("WARNING: WWF ecoregions shapefile not found at:", wwf_shapefile, "\n")
    return(rep("ambiguous", nrow(ilo_data_with_coords)))
  }
  
  wwf_ecos <- st_read(wwf_shapefile, quiet = TRUE)
  wwf_ecos <- st_make_valid(wwf_ecos)
  
  ilo_pts <- st_as_sf(
    ilo_data_with_coords, 
    coords = c("Longitude", "Latitude"), 
    crs = 4326,
    remove = FALSE
  )
  
  cat("Spatially joining ILO points to WWF ecoregions...\n")
  joined <- st_join(ilo_pts, wwf_ecos, join = st_within, left = TRUE)
  
  no_match <- sum(is.na(joined$BIOME))
  cat(sprintf("Points with no WWF ecoregion match: %d\n", no_match))
  
  spatial_gauci <- map_wwf_to_gauci(joined$BIOME)
  
  return(ifelse(is.na(spatial_gauci), "ambiguous", spatial_gauci))
}

# =============================================================================
# 4. ILO-SPECIFIC BIOME MAPPING FUNCTIONS
# =============================================================================

# Step 1: PFT-based mapping with climate context
map_pft_to_gauci <- function(pft, mat, map, latitude) {
  sapply(seq_along(pft), function(i) {
    current_pft <- str_trim(pft[i])
    current_mat <- mat[i]
    current_map <- map[i]
    current_lat <- latitude[i]
    
    # Handle missing values
    if (is.na(current_pft) | current_pft == "" | current_pft == "N/A") {
      return(NA_character_)
    }
    
    # Use climate and latitude to refine PFT-based classification
    is_tropical <- !is.na(current_mat) && current_mat > 20
    is_temperate <- !is.na(current_mat) && current_mat >= 0 && current_mat <= 20
    is_boreal <- !is.na(current_mat) && current_mat < 0
    is_arid <- !is.na(current_map) && current_map < 500
    is_wet <- !is.na(current_map) && current_map > 2000
    is_high_latitude <- !is.na(current_lat) && abs(current_lat) > 60
    
    # PFT-based mapping with climate context
    if (current_pft == "DB") {  # Deciduous Broadleaf
      if (is_tropical) {
        if (is_arid) {
          return("Trop. & Subtrop. Dry Broadleaf Forests")
        } else {
          return("Tropical & Subtropical Moist Broadleaf Forests")
        }
      } else if (is_temperate) {
        return("Temperate Broadleaf & Mixed Forests")
      } else if (is_boreal || is_high_latitude) {
        return("Boreal Forests/Taiga")
      } else {
        return("Temperate Broadleaf & Mixed Forests")  # Default for moderate climates
      }
    }
    
    if (current_pft == "DC") {  # Deciduous Conifer
      if (is_boreal || is_high_latitude) {
        return("Boreal Forests/Taiga")
      } else {
        return("Temperate Conifer Forests")
      }
    }
    
    if (current_pft == "EB") {  # Evergreen Broadleaf
      if (is_tropical) {
        if (is_arid) {
          return("Trop. & Subtrop. Dry Broadleaf Forests")
        } else {
          return("Tropical & Subtropical Moist Broadleaf Forests")
        }
      } else if (is_temperate && !is.na(current_lat) && abs(current_lat) < 40) {
        # Subtropical/Mediterranean region
        if (is_arid) {
          return("Mediterranean Forests, Woodlands & Scrub")
        } else {
          return("Temperate Broadleaf & Mixed Forests")
        }
      } else {
        return("Temperate Broadleaf & Mixed Forests")
      }
    }
    
    if (current_pft == "EC") {  # Evergreen Conifer
      if (is_boreal || is_high_latitude) {
        return("Boreal Forests/Taiga")
      } else if (is_tropical) {
        return("Tropical & Subtropical Coniferous Forests")
      } else {
        return("Temperate Conifer Forests")
      }
    }
    
    if (current_pft == "Mix") {  # Mixed forests
      if (is_tropical) {
        return("Tropical & Subtropical Moist Broadleaf Forests")
      } else if (is_boreal || is_high_latitude) {
        return("Boreal Forests/Taiga")
      } else {
        return("Temperate Broadleaf & Mixed Forests")
      }
    }
    
    if (current_pft == "sDB") {  # Shrub Deciduous Broadleaf
      if (is_tropical) {
        return("Trop. & Subtrop. Grasslands, Savannas &")
      } else if (is_arid) {
        return("Deserts & Xeric Shrublands")
      } else {
        return("Temperate Grasslands, Savannas &Shrublands")
      }
    }
    
    return("ambiguous")
  }, USE.NAMES = FALSE)
}

# Step 2: Species-based refinement
map_species_to_gauci <- function(dominant_species, country) {
  sapply(seq_along(dominant_species), function(i) {
    species <- str_to_lower(str_trim(dominant_species[i]))
    country_name <- str_to_lower(str_trim(country[i]))
    
    if (is.na(species) | species == "" | species == "n/a") {
      return("ambiguous")
    }
    
    # Mediterranean indicators
    if (str_detect(species, "quercus|oak") && 
        str_detect(country_name, "spain|portugal|italy|greece|france")) {
      return("Mediterranean Forests, Woodlands & Scrub")
    }
    
    # Mangrove indicators
    if (str_detect(species, "rhizophora|avicennia|mangrove")) {
      return("Mangroves")
    }
    
    # Boreal indicators
    if (str_detect(species, "picea|abies|pinus") && 
        str_detect(country_name, "canada|russia|finland|sweden|norway")) {
      return("Boreal Forests/Taiga")
    }
    
    # Tropical forest indicators
    if (str_detect(species, "cecropia|ficus|dipterocarp") ||
        (str_detect(country_name, "brazil|ecuador|colombia|malaysia|indonesia") && 
         str_detect(species, "forest|tree"))) {
      return("Tropical & Subtropical Moist Broadleaf Forests")
    }
    
    # Desert/arid indicators
    if (str_detect(species, "acacia|prosopis|desert") && 
        str_detect(country_name, "botswana|namibia|australia|niger")) {
      return("Deserts & Xeric Shrublands")
    }
    
    return("ambiguous")
  }, USE.NAMES = FALSE)
}

# Step 3: Climate-based classification for remaining ambiguous cases
map_climate_to_gauci <- function(mat, map, latitude) {
  sapply(seq_along(mat), function(i) {
    current_mat <- mat[i]
    current_map <- map[i]
    current_lat <- latitude[i]
    
    # Handle missing climate data
    if (is.na(current_mat) || is.na(current_map)) {
      return("ambiguous")
    }
    
    # Tundra: very cold, any precipitation
    if (current_mat < -5) {
      return("Tundra")
    }
    
    # Boreal: cold, moderate precipitation
    if (current_mat >= -5 && current_mat < 2) {
      return("Boreal Forests/Taiga")
    }
    
    # Desert: hot and very dry OR cold and dry
    if (current_map < 300) {
      if (current_mat > 18) {
        return("Deserts & Xeric Shrublands")
      } else {
        return("Temperate Grasslands, Savannas &Shrublands")  # Cold steppe
      }
    }
    
    # Tropical climates (MAT > 20°C)
    if (current_mat > 20) {
      if (current_map > 1500) {
        return("Tropical & Subtropical Moist Broadleaf Forests")
      } else if (current_map > 800) {
        return("Trop. & Subtrop. Dry Broadleaf Forests")
      } else {
        return("Trop. & Subtrop. Grasslands, Savannas &")
      }
    }
    
    # Temperate climates (MAT 2-20°C)
    if (current_mat >= 2 && current_mat <= 20) {
      # Mediterranean climate: warm, dry summers (approximation)
      if (!is.na(current_lat) && abs(current_lat) > 30 && abs(current_lat) < 45 && 
          current_map > 400 && current_map < 1000) {
        return("Mediterranean Forests, Woodlands & Scrub")
      }
      
      # Wet temperate
      if (current_map > 1200) {
        return("Temperate Broadleaf & Mixed Forests")
      } 
      # Moderate temperate
      else if (current_map > 500) {
        return("Temperate Conifer Forests")
      } 
      # Dry temperate
      else {
        return("Temperate Grasslands, Savannas &Shrublands")
      }
    }
    
    return("ambiguous")
  }, USE.NAMES = FALSE)
}

# Three-step mapping function for ILO data
map_ilo_to_gauci_threestep <- function(pft, mat, map, latitude, dominant_species, country, spatial_result = NULL) {
  # Step 1: PFT-based mapping with climate context
  pft_result <- map_pft_to_gauci(pft, mat, map, latitude)
  
  # Step 2: Species-based refinement for ambiguous/NA cases from Step 1
  species_result <- map_species_to_gauci(dominant_species, country)
  
  # Step 3: Climate-based classification for remaining ambiguous cases
  climate_result <- map_climate_to_gauci(mat, map, latitude)
  
  # Combine results with priority: PFT > Species > Climate > Spatial
  step1_result <- ifelse(
    !is.na(pft_result) & pft_result != "ambiguous",
    pft_result,
    ifelse(
      !is.na(species_result) & species_result != "ambiguous",
      species_result,
      climate_result
    )
  )
  
  # Step 4: Use spatial mapping for remaining ambiguous cases
  if (!is.null(spatial_result)) {
    final_result <- ifelse(
      step1_result == "ambiguous" & !is.na(spatial_result) & spatial_result != "ambiguous",
      spatial_result,
      step1_result
    )
  } else {
    final_result <- step1_result
  }
  
  return(final_result)
}

# =============================================================================
# 5. APPLY MAPPING TO ILO DATASET
# =============================================================================

cat("\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")
cat("APPLYING ILO THREE-STEP BIOME MAPPING\n")
cat(paste(rep("=", 50), collapse = ""))
cat("\n")

# Check for required coordinate columns
if (!all(c("Latitude", "Longitude") %in% names(ilo_data))) {
  cat("WARNING: Latitude and Longitude columns not found in ILO data.\n")
  cat("Spatial mapping (Step 4) will be skipped.\n")
  use_spatial <- FALSE
} else {
  use_spatial <- TRUE
}

# Apply spatial mapping for ambiguous cases (if coordinates available)
if (use_spatial) {
  cat("Applying spatial mapping for coordinate-based classification...\n")
  
  # Get only records with valid coordinates for spatial mapping
  coords_available <- !is.na(ilo_data$Latitude) & !is.na(ilo_data$Longitude) &
    ilo_data$Latitude >= -90 & ilo_data$Latitude <= 90 &
    ilo_data$Longitude >= -180 & ilo_data$Longitude <= 180
  
  # Initialize spatial results vector
  spatial_results <- rep(NA_character_, nrow(ilo_data))
  
  if (sum(coords_available) > 0) {
    # Apply spatial mapping only to records with coordinates
    spatial_results[coords_available] <- map_spatial_to_gauci(ilo_data[coords_available, ])
  }
  
  # Apply four-step mapping
  ilo_mapped <- ilo_data %>%
    mutate(
      gauci_biome = map_ilo_to_gauci_threestep(
        PFT, 
        `MAT_(Literature value)`, 
        `MAP_(Literature value)`, 
        Latitude, 
        Dominant_species, 
        Country, 
        spatial_results
      ),
      pft_step = map_pft_to_gauci(PFT, `MAT_(Literature value)`, `MAP_(Literature value)`, Latitude),
      species_step = map_species_to_gauci(Dominant_species, Country),
      climate_step = map_climate_to_gauci(`MAT_(Literature value)`, `MAP_(Literature value)`, Latitude),
      spatial_step = spatial_results
    )
} else {
  # Apply three-step mapping only (without spatial)
  ilo_mapped <- ilo_data %>%
    mutate(
      gauci_biome = map_ilo_to_gauci_threestep(
        PFT, 
        `MAT_(Literature value)`, 
        `MAP_(Literature value)`, 
        Latitude, 
        Dominant_species, 
        Country, 
        NULL
      ),
      pft_step = map_pft_to_gauci(PFT, `MAT_(Literature value)`, `MAP_(Literature value)`, Latitude),
      species_step = map_species_to_gauci(Dominant_species, Country),
      climate_step = map_climate_to_gauci(`MAT_(Literature value)`, `MAP_(Literature value)`, Latitude),
      spatial_step = NA_character_
    )
}

# =============================================================================
# 6. RESULTS AND DIAGNOSTICS
# =============================================================================

# Calculate success rates
total_records <- nrow(ilo_mapped)
ambiguous_count <- sum(ilo_mapped$gauci_biome == "ambiguous", na.rm = TRUE)
successfully_mapped <- total_records - ambiguous_count
success_rate <- 100 * successfully_mapped / total_records
ambiguous_rate <- 100 * ambiguous_count / total_records

cat(sprintf("\nILO MAPPING RESULTS:\n"))
cat(sprintf("Total records: %d\n", total_records))
cat(sprintf("Successfully mapped: %d (%.1f%%)\n", successfully_mapped, success_rate))
cat(sprintf("Ambiguous: %d (%.1f%%)\n", ambiguous_count, ambiguous_rate))

# Summary by Gauci biome
mapping_summary <- ilo_mapped %>%
  count(gauci_biome, sort = TRUE, name = "n_records") %>%
  mutate(
    percentage = 100 * n_records / total_records,
    gauci_biome = factor(gauci_biome, levels = c(gauci_biomes, "ambiguous"))
  ) %>%
  arrange(gauci_biome)

cat("\nILO MAPPING SUMMARY BY GAUCI BIOME:\n")
print(kable(mapping_summary, 
            col.names = c("Gauci Biome", "Records", "Percentage"),
            digits = 1,
            format = "simple"))

# Analyze step-by-step process effectiveness
step_analysis <- ilo_mapped %>%
  summarise(
    pft_clear = sum(!is.na(pft_step) & pft_step != "ambiguous"),
    pft_ambiguous = sum(is.na(pft_step) | pft_step == "ambiguous"),
    rescued_by_species = sum((is.na(pft_step) | pft_step == "ambiguous") & 
                               !is.na(species_step) & species_step != "ambiguous"),
    rescued_by_climate = sum((is.na(pft_step) | pft_step == "ambiguous") & 
                               (is.na(species_step) | species_step == "ambiguous") &
                               !is.na(climate_step) & climate_step != "ambiguous"),
    rescued_by_spatial = if(use_spatial) {
      sum((is.na(pft_step) | pft_step == "ambiguous") & 
            (is.na(species_step) | species_step == "ambiguous") &
            (is.na(climate_step) | climate_step == "ambiguous") &
            !is.na(spatial_step) & spatial_step != "ambiguous")
    } else { 0 },
    final_ambiguous = sum(gauci_biome == "ambiguous", na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("\nSTEP-BY-STEP PROCESS ANALYSIS:\n"))
cat(sprintf("Step 1 (PFT+Climate) clear results: %d (%.1f%%)\n", 
            step_analysis$pft_clear, 
            100 * step_analysis$pft_clear / total_records))
cat(sprintf("Step 1 (PFT+Climate) ambiguous: %d (%.1f%%)\n", 
            step_analysis$pft_ambiguous,
            100 * step_analysis$pft_ambiguous / total_records))
cat(sprintf("Step 2 (Species) rescued ambiguous cases: %d (%.1f%%)\n", 
            step_analysis$rescued_by_species,
            100 * step_analysis$rescued_by_species / total_records))
cat(sprintf("Step 3 (Climate only) rescued remaining cases: %d (%.1f%%)\n", 
            step_analysis$rescued_by_climate,
            100 * step_analysis$rescued_by_climate / total_records))

if (use_spatial) {
  cat(sprintf("Step 4 (Spatial WWF) rescued final cases: %d (%.1f%%)\n", 
              step_analysis$rescued_by_spatial,
              100 * step_analysis$rescued_by_spatial / total_records))
} else {
  cat("Step 4 (Spatial) skipped - no coordinate data available\n")
}

cat(sprintf("Final ambiguous cases: %d (%.1f%%)\n", 
            step_analysis$final_ambiguous,
            100 * step_analysis$final_ambiguous / total_records))

# Examples of successful mappings by PFT
cat("\nEXAMPLES OF SUCCESSFUL MAPPINGS BY PFT:\n")
pft_examples <- ilo_mapped %>%
  filter(gauci_biome != "ambiguous") %>%
  select(PFT, Country, `MAT_(Literature value)`, `MAP_(Literature value)`, gauci_biome) %>%
  group_by(PFT, gauci_biome) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  arrange(PFT, gauci_biome)

print(kable(pft_examples,
            col.names = c("PFT", "Country", "MAT", "MAP", "Mapped Gauci Biome"),
            format = "simple"))

# Examples of ambiguous cases
cat("\nEXAMPLES OF REMAINING AMBIGUOUS CASES:\n")
ambiguous_examples <- ilo_mapped %>%
  filter(gauci_biome == "ambiguous") %>%
  count(PFT, Vegetation_status, sort = TRUE) %>%
  head(10) %>%
  select(-n)

print(kable(ambiguous_examples,
            col.names = c("PFT", "Vegetation Status"),
            format = "simple"))

# Summary by original PFT
cat("\nMAPPING SUCCESS BY ORIGINAL PFT:\n")
pft_success <- ilo_mapped %>%
  group_by(PFT) %>%
  summarise(
    total = n(),
    mapped = sum(gauci_biome != "ambiguous"),
    success_rate = 100 * mapped / total,
    .groups = "drop"
  ) %>%
  arrange(desc(success_rate))

print(kable(pft_success,
            col.names = c("PFT", "Total Records", "Successfully Mapped", "Success Rate (%)"),
            digits = 1,
            format = "simple"))

# =============================================================================
# 7. EXPORT RESULTS
# =============================================================================

# Save mapped dataset
# CHANGED: Save to outputs folder
output_file <- "data/outputs/ilo_data_with_gauci_mapping.csv"
write_csv(ilo_mapped, output_file)
cat(sprintf("\nMapped ILO dataset saved to: %s\n", output_file))

# Save mapping summary
# CHANGED: Save to outputs folder
summary_file <- "data/outputs/ilo_gauci_mapping_summary.csv"
write_csv(mapping_summary, summary_file)
cat(sprintf("ILO mapping summary saved to: %s\n", summary_file))

# Save PFT-specific success analysis
# CHANGED: Save to outputs folder
pft_summary_file <- "data/outputs/ilo_pft_success_summary.csv"
write_csv(pft_success, pft_summary_file)
cat(sprintf("PFT success analysis saved to: %s\n", pft_summary_file))

cat("\nILO to Gauci biome mapping analysis complete!\n")

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