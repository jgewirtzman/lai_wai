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

# Function to get radius at any height above ground
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
sampling_heights <- c(2, 10, 26)  # from ground to sampling height
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
cat("Full stem surface area:", round(full_stem_area, 1), "m²\n")
cat("Ground area represented:", round(ground_area, 1), "m²\n")
cat("Branch surface area:", round(branch_surface_area, 1), "m²\n")
cat("Total tree surface area:", round(total_tree_surface_area, 1), "m²\n\n")

# Calculate percentages of total tree sampled
results$percent_of_tree <- (results$surface_area / total_tree_surface_area) * 100

cat("Final results:\n")
print(results)

# Create visualizations
library(ggplot2)
library(dplyr)
library(patchwork)

# Create a comprehensive tree profile with sampling zones and capture curves
create_tree_visualization <- function() {
  
  # 1. Tree profile with shaded sampling zones (horizontally stretched)
  create_tree_profile <- function() {
    # Create points for tree outline with horizontal stretching
    heights <- seq(0, 26, 0.1)
    radii <- sapply(heights, get_radius_at_height)
    
    # Horizontal stretch factor to make the cone more visible (reduced to 5x)
    stretch_factor <- 5
    
    # Create zones for different sampling heights
    # Zone 1: 0-2m (red)
    h1 <- seq(0, 2, 0.05)
    r1 <- sapply(h1, get_radius_at_height) * stretch_factor
    zone1 <- data.frame(
      height = c(h1, rev(h1)),
      radius = c(r1, -rev(r1)),
      zone = "0-2m sampling"
    )
    
    # Zone 2: 2-10m (yellow)
    h2 <- seq(2, 10, 0.05)
    r2 <- sapply(h2, get_radius_at_height) * stretch_factor
    zone2 <- data.frame(
      height = c(h2, rev(h2)),
      radius = c(r2, -rev(r2)),
      zone = "2-10m sampling"
    )
    
    # Zone 3: 10-26m (green)
    h3 <- seq(10, 26, 0.05)
    r3 <- sapply(h3, get_radius_at_height) * stretch_factor
    zone3 <- data.frame(
      height = c(h3, rev(h3)),
      radius = c(r3, -rev(r3)),
      zone = "10-26m sampling"
    )
    
    # Combine all zones
    all_zones <- rbind(zone1, zone2, zone3)
    
    # Tree outline for border
    tree_outline <- data.frame(
      height = c(heights, rev(heights)),
      radius = c(radii * stretch_factor, -rev(radii * stretch_factor))
    )
    
    # Create the plot
    p1 <- ggplot() +
      # Colored sampling zones
      geom_polygon(data = all_zones, 
                   aes(x = radius, y = height, fill = zone), 
                   alpha = 0.8) +
      
      # Tree outline
      geom_path(data = tree_outline, 
                aes(x = radius, y = height), 
                color = "black", linewidth = 1.5) +
      
      # Sampling height lines across the width
      geom_hline(yintercept = c(2, 10), 
                 color = "black", linewidth = 1, linetype = "dashed", alpha = 0.7) +

      
      # Formatting with nicer colors and better proportions
      scale_fill_manual(values = c("0-2m sampling" = "#d73027",    # Nice red
                                   "2-10m sampling" = "#fee08b",   # Warm yellow  
                                   "10-26m sampling" = "#4575b4")) + # Nice blue-green
      coord_fixed(ratio = 0.25) +  # Adjust for 5x stretch
      scale_x_continuous(limits = c(-max(radii) * stretch_factor * 1.1, 
                                    max(radii) * stretch_factor * 1.1),
                         name = "Trunk width (5x stretched for visibility)") +
      scale_y_continuous(breaks = seq(0, 26, 5),
                         limits = c(0, 27),  # Start from 0 to match right plot
                         name = "Height above ground (m)") +
      ggtitle("Tree Profile: Sampling Zones") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.position = "none",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_text(size = 11),
            axis.title.y = element_text(size = 11),
            plot.margin = margin(10, 10, 10, 10),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
    
    return(p1)
  }
  
  # 2. Capture percentage vs height curve
  create_capture_curve <- function() {
    # Calculate percent capture for many height increments
    heights_detailed <- seq(1, 26, 0.5)
    capture_data <- data.frame(
      height = heights_detailed,
      stem_percent = numeric(length(heights_detailed)),
      tree_percent = numeric(length(heights_detailed))
    )
    
    # Calculate capture percentages for each height
    for (i in 1:length(heights_detailed)) {
      h <- heights_detailed[i]
      r_bottom <- get_radius_at_height(0)
      r_top <- get_radius_at_height(h)
      area <- truncated_cone_lateral_area(r_bottom, r_top, h)
      
      capture_data$stem_percent[i] <- (area / full_stem_area) * 100
      capture_data$tree_percent[i] <- (area / total_tree_surface_area) * 100
    }
    
    # Add our specific sampling points with colors matching the left plot zones
    sampling_points <- data.frame(
      height = c(2, 10, 26),
      stem_percent = results$percent_of_stem,
      tree_percent = results$percent_of_tree,
      label = c("2m", "10m", "Full"),
      zone_color = c("#d73027", "#fee08b", "#4575b4")  # Match left plot colors
    )
    
    # Create the plot
    p2 <- ggplot() +
      # Capture curves with meaningful colors and line types
      geom_line(data = capture_data, 
                aes(x = stem_percent, y = height, color = "% of Stem", linetype = "% of Stem"), 
                linewidth = 3, alpha = 0.8) +
      geom_line(data = capture_data, 
                aes(x = tree_percent, y = height, color = "% of Tree", linetype = "% of Tree"), 
                linewidth = 3, alpha = 0.8) +
      
      # Sampling points with colors matching the left plot zones
      geom_point(data = sampling_points, 
                 aes(x = stem_percent, y = height, fill = zone_color), 
                 size = 5, color = "black", 
                 shape = 21, stroke = 2) +
      geom_point(data = sampling_points, 
                 aes(x = tree_percent, y = height, fill = zone_color), 
                 size = 5, color = "black",
                 shape = 21, stroke = 2) +
      
      # Use manual fill scale for points
      scale_fill_identity() +
      
      # Point labels with better positioning - check for valid positions
      geom_text(data = sampling_points, 
                aes(x = pmax(stem_percent + 8, 8), y = height, label = label), 
                hjust = 0, color = "black", fontface = "bold", size = 4) +
      
      # Horizontal lines at sampling heights  
      geom_hline(yintercept = c(2, 10), 
                 color = "#969696", linewidth = 0.5, linetype = "dashed", alpha = 0.7) +
      
      # Formatting with meaningful colors and line types
      scale_color_manual(values = c("% of Stem" = "black", "% of Tree" = "black"),  # Brown and Green
                         name = "") +  
      scale_linetype_manual(values = c("% of Stem" = "solid", "% of Tree" = "dashed"),
                            name = "") +
      scale_x_continuous(breaks = seq(0, 100, 20),
                         limits = c(0, 105),
                         name = "Percent Surface Area Captured (%)") +
      scale_y_continuous(breaks = seq(0, 26, 5),
                         limits = c(0, 27),  # Match left plot limits exactly
                         name = "Height above ground (m)") +
      ggtitle("Surface Area Capture vs Height") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.position = "bottom",
            legend.text = element_text(size = 10),
            axis.title.x = element_text(size = 11),
            axis.title.y = element_text(size = 11),
            plot.margin = margin(10, 10, 10, 10),
            panel.grid.minor = element_blank())
    
    return(p2)
  }
  
  # Create both plots with matching dimensions
  p1 <- create_tree_profile()
  p2 <- create_capture_curve()
  
  # Combine using patchwork with equal heights and proper alignment
  combined <- p1 + p2 + 
    plot_layout(ncol = 2, widths = c(1, 1)) +  # Equal widths for better balance
    plot_annotation(
      title = "Tree Surface Area Analysis: 26m Tree with 40cm DBH",
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    )
  
  # Display the combined plot
  print(combined)
  
  return(combined)
}

# Create the visualization
cat("Creating tree visualization...\n")

# Print debug info
cat("Results data:\n")
print(results)
cat("\nFull stem area:", full_stem_area, "\n")
cat("Total tree surface area:", total_tree_surface_area, "\n")

tree_viz <- create_tree_visualization()

# Print key insights
cat("\nKEY INSIGHTS FROM VISUALIZATION:\n")
cat("=================================\n")
cat("• Tree profile shows how sampling zones capture different portions\n")
cat("• Capture curves show exponential relationship between height and coverage\n")
cat("• Red curve (% of tree) shows dramatic impact of including branches\n")
cat("• Standard 2m sampling captures < 15% of stem, < 4% of total tree\n")