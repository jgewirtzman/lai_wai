# Tree Surface Area Calculations for Truncated Cones
# 26m tall tree with 40cm DBH, assuming conical stem geometry

# Tree parameters
height <- 26  # meters total height
dbh <- 0.40   # meters (40cm diameter at breast height)
dbh_height <- 1.37  # meters above ground where DBH is measured
base_radius <- dbh / 2  # radius at breast height

# Linear taper from DBH (base_radius) at 1.37m to apex (0 radius) at 26m
height_above_dbh <- height - dbh_height  # height from DBH to tree top

cat("Tree parameters:\n")
cat("Total height:", height, "m\n")
cat("DBH:", dbh * 100, "cm\n")
cat("DBH height:", dbh_height, "m\n")
cat("Radius at DBH:", base_radius, "m\n")
cat("Height above DBH:", height_above_dbh, "m\n\n")

# Function to get radius at any height above groun d
# Linear taper from DBH radius at 1.37m to 0 at tree top (26m)
get_radius_at_height <- function(height_above_ground) {
  if (height_above_ground >= height) {
    return(0)  # At or above tree top
  }
  
  # Linear taper: radius decreases from base_radius at DBH to 0 at tree top
  # radius = base_radius * (height - height_above_ground) / height_above_dbh
  radius <- base_radius * (height - height_above_ground) / height_above_dbh
  return(max(0, radius))
}

# Function to calculate lateral surface area of truncated cone
# Formula: π * (r1 + r2) * slant_height
truncated_cone_lateral_area <- function(r1, r2, h) {
  slant_height <- sqrt(h^2 + (r1 - r2)^2)
  area <- pi * (r1 + r2) * slant_height
  return(area)
}

# Calculate lateral surface areas for different sampling heights
sampling_heights <- c(1.37, 2, 10, 20, 26)  # from ground to sampling height
results <- data.frame(
  sampling_height = sampling_heights,
  surface_area = numeric(length(sampling_heights)),
  percent_of_stem = numeric(length(sampling_heights)),
  percent_of_tree = numeric(length(sampling_heights))
)

# Calculate lateral surface areas
cat("Lateral surface area calculations:\n")
for (i in 1:length(sampling_heights)) {
  h <- sampling_heights[i]
  
  # Radius at ground (0m) and at sampling height
  r_bottom <- get_radius_at_height(0)
  r_top <- get_radius_at_height(h)
  
  # Lateral surface area from ground to sampling height
  area <- truncated_cone_lateral_area(r_bottom, r_top, h)
  results$surface_area[i] <- area
  
  cat("Sampling to", h, "m height:\n")
  cat("  Radius at ground:", round(r_bottom, 3), "m\n")
  cat("  Radius at", h, "m:", round(r_top, 3), "m\n")
  cat("  Lateral surface area:", round(area, 2), "m²\n\n")
}

# Full stem surface area
full_stem_area <- results$surface_area[results$sampling_height == 26]

# Calculate percentages of stem sampled
results$percent_of_stem <- (results$surface_area / full_stem_area) * 100

cat("Stem surface area summary:\n")
print(results[,1:3])
cat("\n")

# Now calculate total tree surface area using Whittaker & Woodwell (1967)
stem_bark_per_m2 <- 0.45  # m² stem bark per m² ground
branch_bark_per_m2 <- 1.7  # m² branch bark per m² ground

# Ground area represented by this tree
ground_area <- full_stem_area / stem_bark_per_m2
branch_surface_area <- ground_area * branch_bark_per_m2
total_tree_surface_area <- full_stem_area + branch_surface_area

cat("\nTotal tree surface area calculations:\n")
cat("Full stem surface area:", round(full_stem_area, 2), "m²\n")
cat("Ground area represented:", round(ground_area, 2), "m²\n")
cat("Branch surface area:", round(branch_surface_area, 2), "m²\n")
cat("Total tree surface area:", round(total_tree_surface_area, 2), "m²\n\n")

# Calculate percentages of total tree sampled
results$percent_of_tree <- (results$surface_area / total_tree_surface_area) * 100

cat("Final results:\n")
print(results)

# Verification of truncated cone formula
cat("\n=== FORMULA VERIFICATION ===\n")
cat("Testing truncated cone lateral surface area formula:\n")
cat("Formula: π × (r₁ + r₂) × √(h² + (r₁ - r₂)²)\n\n")

# Test with known values: r1=2, r2=1, h=3
test_r1 <- 2; test_r2 <- 1; test_h <- 3
test_area <- truncated_cone_lateral_area(test_r1, test_r2, test_h)
expected_area <- pi * (test_r1 + test_r2) * sqrt(test_h^2 + (test_r1 - test_r2)^2)

cat("Test case: r₁ =", test_r1, ", r₂ =", test_r2, ", h =", test_h, "\n")
cat("Calculated:", round(test_area, 6), "\n")
cat("Expected:", round(expected_area, 6), "\n")
cat("Formula verified:", abs(test_area - expected_area) < 1e-10, "\n")