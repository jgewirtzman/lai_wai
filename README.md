# LAI Biome Mapping and Consensus Analysis

This project maps Leaf Area Index (LAI) data from two sources (Asner and Iio datasets) to standardized biome classifications and develops consensus LAI values for global biomes.

## Project Overview

This project integrates Leaf Area Index (LAI) data with woody surface area data from Gauci et al. (2024) to quantify global forest surface areas and woody:leaf ratios. The analysis enables calculation of total leaf surface area and leaf:woody surface area  by biome.

The analysis workflow consists of 7 R scripts that process, map, analyze, and integrate LAI data:

1. **Biome Mapping**: Map original LAI observation biome classifications to standardized biomes (WWF defined, as used in Gauci et al.)
2. **Statistical Analysis**: Apply IQR filtering to LAI observations and calculate distributions by biome
3. **Comparison**: Compare LAI values between datasets
4. **Consensus Development**: Develop consensus LAI values using multiple approaches
5. **Integration**: Calculate leaf surface areas and woody:leaf ratios using Gauci et al. woody surface data

## Directory Structure

```
├── data/
│   ├── inputs/                    # Raw input data
│   │   ├── LAI_data_asner.csv    # Asner LAI dataset
│   │   ├── lai_data_ilo.csv      # ILO LAI dataset
│   │   ├── biome_table_gauci.csv # Gauci biome reference table
│   │   └── official/             # Official reference data
│   │       └── wwf_terr_ecos.shp # WWF ecoregions shapefile
│   └── outputs/                  # Generated results and figures
├── scripts/                      # Analysis scripts (01-07)
└── README.md                     # This file
```

## Workflow Scripts

### 1. Asner LAI Mapping (`01_LAI_mapping.R`)
- Maps Asner dataset biome categories to standardized Gauci biomes
- Uses 3-step mapping: Biomecover → Biome → Spatial (WWF)
- **Outputs**: `lai_data_with_gauci_mapping.csv`, `gauci_mapping_summary.csv`

### 2. Asner LAI Analysis (`02_LAI_calcs.R`)
- Applies IQR filtering to remove outliers within each biome
- Generates distribution plots and statistical summaries
- **Outputs**: `lai_data_iqr_filtered.csv`, distribution plots

### 3. Iio LAI Mapping (`03_ilo_mapping.R`)
- Maps Iio dataset to Gauci biomes using PFT, climate, and spatial data
- Uses 4-step mapping: PFT+Climate → Species → Climate → Spatial
- **Outputs**: `ilo_data_with_gauci_mapping.csv`, `ilo_gauci_mapping_summary.csv`

### 4. Iio LAI Analysis (`04_ilo_calcs.R`)
- Applies IQR filtering and analyzes ILO LAI distributions
- Includes PFT-specific analysis and climate relationships
- **Outputs**: `ilo_lai_data_iqr_filtered.csv`, distribution plots

### 5. Dataset Comparison (`05_compare_lais.R`)
- Compares LAI values between Asner and ILO datasets by biome
- Creates comparison visualizations and summary tables
- **Outputs**: `lai_biome_comparison_table.csv`, comparison plots

### 6. Consensus Development (`06_consensus.R`)
- Develops consensus LAI values using 5 different approaches:
  - Sample size weighting
  - Meta-analysis (inverse variance)
  - Robust (median-based)
  - Bayesian
  - Expert judgment
- Selects simple average as final consensus method
- **Outputs**: `final_simple_average_consensus.csv`, consensus plots

### 7. Integration (`07_integration.R`)
- Integrates consensus LAI values with Gauci biome table
- Calculates 1-sided and 2-sided leaf surface areas
- Computes woody:leaf surface area ratios by forest cover type
- **Outputs**: `biome_table_enhanced_with_lai.csv`, ratio plots

## Key Outputs

### Final Products
- **`biome_table_enhanced_with_lai.csv`**: Complete biome table with consensus LAI values and surface area calculations
- **`final_simple_average_consensus.csv`**: Consensus LAI values for each biome
- **`global_surface_area_summary.csv`**: Global totals and woody:leaf ratios

### Visualizations
- Distribution plots for each dataset
- Comparison plots between datasets
- Consensus method comparisons
- Woody:leaf ratio visualizations with color-coded interpretations

## Data Sources

### Input Datasets
1. **Asner LAI Data** (`LAI_data_asner.csv`)
   - Based on Asner et al. (2003) global synthesis of 1,008 LAI measurements from ~400 field sites
   - Contains LAI measurements with Biome and Biomecover classifications
   - Spans global biomes from 1932-2000 publication period
   - Available from ORNL DAAC (Short Name: LAI_Woody_Plants_1231)
   - Includes coordinates for spatial mapping

2. **Iio LAI Data** (`lai_data_ilo.csv`)
   - Based on Iio et al. (2014) compilation of 2,606 field-observed LAI values from 554 literature sources
   - Published between 1932-2011, representing woody species globally
   - Contains Plant Functional Type (PFT) classifications (DB, EB, EC, DC, sDB, Mix)
   - Includes climate data (MAT, MAP), species information, and coordinates
   - Uses standardized definition of LAI as half of total surface area (HSA)

3. **Gauci Biome Table** (`biome_table_gauci.csv`)
   - Reference table with standardized WWF terrestrial biome classifications (Olson & Dinerstein)
   - Contains forest cover proportions from MODIS, Census, and Census+Shrubs datasets
   - Includes woody surface areas from Gauci et al. (2024) terrestrial laser scanning analysis

4. **WWF Terrestrial Ecoregions** (`wwf_terr_ecos.shp`)
   - Terrestrial Ecoregions of the World (TEOW) biogeographic regionalization
   - 867 terrestrial ecoregions classified into 14 biomes (forests, grasslands, deserts, etc.)
   - Represents original distribution of distinct assemblages of species and communities
   - Used for coordinate-based spatial mapping when other biome classifications are ambiguous
   - Available from: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world

## Methodology

### Biome Mapping Strategy
- **Asner**: Biomecover (primary) → Biome (fallback) → Spatial WWF (last resort)
- **ILO**: PFT+Climate (primary) → Species (refinement) → Climate only → Spatial WWF

### Statistical Approach
- IQR filtering within each biome to remove outliers
- No statistical testing (by design) - purely descriptive analysis
- Multiple consensus methods compared for robustness

### Consensus Selection
- Simple average chosen as final method: `(Asner_LAI + ILO_LAI) / 2`
- Most transparent and easily interpretable
- Validated against 5 sophisticated consensus methods



## Running the Analysis

### Full Workflow
```r
# Set working directory to project root
setwd("path/to/project")

# Run scripts in order
source("scripts/01_LAI_mapping.R")
source("scripts/02_LAI_calcs.R") 
source("scripts/03_ilo_mapping.R")
source("scripts/04_ilo_calcs.R")
source("scripts/05_compare_lais.R")
source("scripts/06_consensus.R")
source("scripts/07_integration.R")
```

### Individual Scripts
Each script can be run independently, but later scripts depend on outputs from earlier ones.







## Citation and Data Use

When using this analysis or derived products, please cite:
- Asner, G.P., Scurlock, J.M.O., Hicke, J.A. (2003). Global synthesis of leaf area index observations: implications for ecological and remote sensing studies. *Global Ecology and Biogeography*, 12(3), 191-205. https://doi.org/10.1046/j.1466-822X.2003.00026.x
- Iio, A., Hikosaka, K., Anten, N.P.R., Nakagawa, Y., Ito, A. (2014). Global dependence of field-observed leaf area index in woody species on climate: a systematic review. *Global Ecology and Biogeography*, 23(3), 274-285. https://doi.org/10.1111/geb.12133
- Gauci, V., Pangala, S.R., Shenkin, A. et al. Global atmospheric methane uptake by upland tree woody surfaces. *Nature* **631**, 796–800 (2024). https://doi.org/10.1038/s41586-024-07592-w
- Olson, D.M., Dinerstein, E., Wikramanayake, E.D., Burgess, N.D., Powell, G.V.N., Underwood, E.C., D'Amico, J.A., Itoua, I., Strand, H.E., Morrison, J.C., Loucks, C.J., Allnutt, T.F., Ricketts, T.H., Kura, Y., Lamoreux, J.F., Wettengel, W.W., Hedao, P., Kassem, K.R. (2001). Terrestrial ecoregions of the world: a new map of life on Earth. *Bioscience* 51(11):933-938.
- This LAI analysis workflow

## Contact and Support

For questions about the analysis or to report issues:
- Check script comments for methodology details
- Review output CSV files for data validation
- Examine diagnostic messages in script outputs

## Version History

- **v1.0**: Initial biome mapping and analysis
- **v2.0**: Added ILO dataset integration  
- **v3.0**: Implemented consensus development (removed statistical testing)
- **v4.0**: Enhanced with surface area calculations and full integration

---

*This analysis integrates LAI data with woody surface measurements from Gauci et al. (2024) to quantify global forest surface areas and woody:leaf ratios across WWF terrestrial biomes. The work complements the discovery that tree woody surfaces constitute a major atmospheric methane sink, providing a comprehensive view of forest surface area dynamics.*
