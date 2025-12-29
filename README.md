# Forest Biome LAI and Woody:Leaf Surface Area Ratios

This project calculates woody-to-leaf surface area ratios for global forest biomes by combining field-measured leaf area index (LAI) data with terrestrial laser scanning estimates of wood area index (WAI).

## Overview

The analysis integrates:
- **LAI data** from the Iio & Ito (2014) global woody plant database (ORNL DAAC)
- **WAI data** from Gauci et al. (2024) terrestrial laser scanning measurements
- **Biome classification** from WWF Terrestrial Ecoregions of the World

## Directory Structure

```
├── data/
│   ├── inputs/
│   │   ├── LAI_Woody_Plants_1231/
│   │   │   └── data/
│   │   │       └── LAI_Woody_Plants_Database.csv   # Iio LAI database
│   │   ├── biome_table_gauci.csv                   # Gauci biome reference table
│   │   └── official/
│   │       └── wwf_terr_ecos.shp                   # WWF ecoregions shapefile
│   └── outputs/                                     # Generated results
├── scripts/
│   └── 01_wai.R                                    # Main analysis script
└── README.md
```

## Workflow

The analysis is contained in a single script (`01_wai.R`) that:

1. **Loads Iio LAI database** - 2,653 site-specific maximum LAI measurements from 554 published studies (1932–2011)

2. **Extracts clumping-corrected LAI** - Uses `Corrected_total_LAI_HSA` values, which account for non-random foliage distribution

3. **Maps to WWF biomes** - Spatial intersection of measurement coordinates with WWF Terrestrial Ecoregions (99.5% classification success)

4. **Restricts to forest biomes** - Eight closed-canopy forest biomes where WAI estimates exist:
   - Boreal Forests/Taiga
   - Temperate Broadleaf & Mixed Forests
   - Temperate Coniferous Forests
   - Mediterranean Forests, Woodlands & Scrub
   - Tropical & Subtropical Coniferous Forests
   - Tropical & Subtropical Dry Broadleaf Forests
   - Tropical & Subtropical Moist Broadleaf Forests
   - Mangroves

5. **Applies IQR filtering** - Removes outliers beyond 1.5 × IQR per biome (removes ~2% of records)

6. **Calculates median LAI** - Per-biome median as canonical estimate

7. **Assigns WAI values** - From Gauci et al. (2024):
   - Tropical moist broadleaf: 4.12 m² m⁻²
   - All other forest biomes: 3.07 m² m⁻²

8. **Computes woody:leaf ratios** - WAI / (LAI × 2), using two-sided LAI for like-for-like comparison of total surface areas

## Key Outputs

- `forest_final_lai_by_biome.csv` - Median LAI per forest biome
- `forest_biome_table_with_lai_and_ratios.csv` - Complete results table
- `forest_woody_leaf_ratios_2sided.csv` - Woody:leaf ratios by biome
- `forest_woody_leaf_ratios_2sided.png` - Visualization

## Data Sources

### Iio LAI Database
- **Citation**: Iio, A., and A. Ito. 2014. A Global Database of Field-observed Leaf Area Index in Woody Plant Species, 1932-2011. ORNL DAAC. doi:10.3334/ORNLDAAC/1231
- **Reference**: Iio et al. (2014) Global Ecology and Biogeography 23:274–285. doi:10.1111/geb.12133
- Site-specific maximum LAI values; excludes measurements affected by drought, disturbance, or immature/declining vegetation
- All values standardized to half-surface-area (HSA) basis with clumping corrections

### Gauci WAI Data
- **Citation**: Gauci, V. et al. (2024) Global atmospheric methane uptake by upland tree woody surfaces. Nature 631:796–800. doi:10.1038/s41586-024-07592-w
- Terrestrial laser scanning measurements from closed-canopy forests

### WWF Ecoregions
- **Citation**: Olson, D.M. et al. (2001) Terrestrial ecoregions of the world: a new map of life on Earth. BioScience 51:933–938.
- https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world

## Requirements

R packages: `dplyr`, `readr`, `stringr`, `ggplot2`, `forcats`, `tidyr`, `sf`, `knitr`, `RColorBrewer`, `patchwork`

## Notes

The woody:leaf ratios presented here are preliminary estimates given that only two WAI values are currently available (one for tropical moist forests, one for all others). These values indicate that woody-to-leaf ratios vary among forest biomes, but more widespread WAI measurements will be necessary to characterize this variation precisely. The lowest ratios likely occur in herbaceous/non-woody biomes and the highest in xeric ecosystems, but WAI data do not yet exist for these vegetation types.

## Additional Literature Informing This Work
Because the published manuscript is restricted to a reference limit, the following works are provided here rather than cited in-text. These studies informed the broader scientific framing of woody surface ecology, gas exchange, nutrient cycling, and forest structural complexity.

### Trace Gases
1. Teskey, R. O., Saveyn, A., Steppe, K. & McGuire, M. A. Origin, fate and significance of CO₂ in tree stems. *New Phytol.* 177, 17–32 (2008).
2. Pangala, S. R. et al. Large emissions from floodplain trees close the Amazon methane budget. *Nature* 552, 230–234 (2017).
3. Barba, J. et al. Methane emissions from tree stems: a new frontier in the global carbon cycle. *New Phytol.* 222, 18–28 (2019).
4. Jeffrey, L. C. et al. Bark-dwelling methanotrophic bacteria decrease methane emissions from trees. *Nat. Commun.* 12, 2127 (2021).
5. Gorgolewski, A. S., Vantellingen, J., Caspersen, J. P. & Thomas, S. C. Overlooked sources of methane emissions from trees: branches and wounds. *Can. J. For. Res.* 52, 1165–1175 (2022).

### Nutrient Cycling and Deposition
6. Tukey, H. B., Ticknor, R. L., Hinsvark, O. N. & Wittwer, S. H. Absorption of nutrients by stems and branches of woody plants. *Science* 116, 167–168 (1952).
7. Doty, S. L. et al. Variable nitrogen fixation in wild *Populus*. *PLoS One* 11, e0155979 (2016).
8. Barrett, T. E., Ponette-González, A. G., Rindy, J. E. & Weathers, K. C. Wet deposition of black carbon: A synthesis. *Atmos. Environ.* 213, 558–567 (2019).

### Forest Structural Measurements
9. Asner, G. P., Scurlock, J. M. O. & Hicke, J. A. Global synthesis of leaf area index observations: implications for ecological and remote sensing studies. *Glob. Ecol. Biogeogr.* 12, 191–205 (2003).
10. Calders, K. et al. Terrestrial laser scanning in forest ecology: expanding the horizon. *Remote Sens. Environ.* 251, 112102 (2020).
11. Liu, X. et al. Enhancing ecosystem productivity and stability with increasing canopy structural complexity in global forests. *Sci. Adv.* 10, eadl1947 (2024).

### Forest Surface Area Studies
12. Gavrikov, V. L. Stem Surface Area as Subject of Study. In: *Stem Surface Area in Modeling of Forest Stands* (ed. Gavrikov, V. L.) 1–13 (Springer International Publishing, Cham, 2017).
