# Create a comprehensive tree profile with sampling zones and capture curves using red-blue gradient
create_tree_visualization <- function() {
  
  # 1. Tree profile with gradient sampling zones (horizontally stretched)
  create_tree_profile <- function() {
    # Create points for tree outline with horizontal stretching
    heights <- seq(0, 26, 0.1)
    radii <- sapply(heights, get_radius_at_height)
    
    # Horizontal stretch factor to make the cone more visible
    stretch_factor <- 5
    
    # Create gradient zones with smooth transitions
    # Define many small height segments for smooth gradient
    n_segments <- 50
    height_segments <- seq(0, 26, length.out = n_segments + 1)
    
    # Create color palette from red to blue
    gradient_colors <- colorRampPalette(c("#d73027", "#4575b4"))(n_segments)
    
    # Create data frame for all segments
    all_zones <- data.frame()
    
    for (i in 1:n_segments) {
      h_start <- height_segments[i]
      h_end <- height_segments[i + 1]
      h_seq <- seq(h_start, h_end, length.out = 20)
      r_seq <- sapply(h_seq, get_radius_at_height) * stretch_factor
      
      segment_data <- data.frame(
        height = c(h_seq, rev(h_seq)),
        radius = c(r_seq, -rev(r_seq)),
        zone = paste0("segment_", i),
        color = gradient_colors[i]
      )
      
      all_zones <- rbind(all_zones, segment_data)
    }
    
    # Tree outline for border
    tree_outline <- data.frame(
      height = c(heights, rev(heights)),
      radius = c(radii * stretch_factor, -rev(radii * stretch_factor))
    )
    
    # Create the plot
    p1 <- ggplot() +
      # Gradient zones
      geom_polygon(data = all_zones, 
                   aes(x = radius, y = height, fill = color, group = zone), 
                   alpha = 0.9) +
      
      # Tree outline
      geom_path(data = tree_outline, 
                aes(x = radius, y = height), 
                color = "black", linewidth = 1.5) +
      
      # Sampling height lines across the width
      geom_hline(yintercept = c(2, 10), 
                 color = "black", linewidth = 1, linetype = "dashed", alpha = 0.7) +
      
      # Use identity fill scale for gradient colors
      scale_fill_identity() +
      coord_fixed(ratio = 0.25) +  # Adjust for 5x stretch
      scale_x_continuous(limits = c(-max(radii) * stretch_factor * 1.1, 
                                    max(radii) * stretch_factor * 1.1),
                         name = "Trunk width (5x stretched for visibility)") +
      scale_y_continuous(breaks = seq(0, 26, 5),
                         limits = c(0, 27),
                         name = "Height above ground (m)") +
      ggtitle("Tree Profile: Height Gradient (Red→Blue)") +
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
  
  # 2. Capture percentage vs height curve with corresponding gradient colors
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
    
    # Function to get gradient color based on height
    get_gradient_color <- function(height) {
      # Normalize height to 0-1 range
      normalized_height <- height / 26
      # Interpolate between red and blue
      colorRampPalette(c("#d73027", "#4575b4"))(100)[pmax(1, pmin(100, round(normalized_height * 100)))]
    }
    
    # Add our specific sampling points with gradient colors
    sampling_points <- data.frame(
      height = c(2, 10, 26),
      stem_percent = results$percent_of_stem,
      tree_percent = results$percent_of_tree,
      label = c("2m", "10m", "Full"),
      gradient_color = sapply(c(2, 10, 26), get_gradient_color)
    )
    
    # Create the plot
    p2 <- ggplot() +
      # Capture curves
      geom_line(data = capture_data, 
                aes(x = stem_percent, y = height, color = "% of Stem", linetype = "% of Stem"), 
                linewidth = 3, alpha = 0.8) +
      geom_line(data = capture_data, 
                aes(x = tree_percent, y = height, color = "% of Tree", linetype = "% of Tree"), 
                linewidth = 3, alpha = 0.8) +
      
      # Sampling points with gradient colors matching the left plot
      geom_point(data = sampling_points, 
                 aes(x = stem_percent, y = height, fill = gradient_color), 
                 size = 5, color = "black", 
                 shape = 21, stroke = 2) +
      geom_point(data = sampling_points, 
                 aes(x = tree_percent, y = height, fill = gradient_color), 
                 size = 5, color = "black",
                 shape = 21, stroke = 2) +
      
      # Use manual fill scale for points
      scale_fill_identity() +
      
      # Point labels with better positioning
      geom_text(data = sampling_points, 
                aes(x = pmax(stem_percent + 8, 8), y = height, label = label), 
                hjust = 0, color = "black", fontface = "bold", size = 4) +
      
      # Horizontal lines at sampling heights  
      geom_hline(yintercept = c(2, 10), 
                 color = "#969696", linewidth = 0.5, linetype = "dashed", alpha = 0.7) +
      
      # Line colors and types
      scale_color_manual(values = c("% of Stem" = "black", "% of Tree" = "black"),
                         name = "") +  
      scale_linetype_manual(values = c("% of Stem" = "solid", "% of Tree" = "dashed"),
                            name = "") +
      scale_x_continuous(breaks = seq(0, 100, 20),
                         limits = c(0, 105),
                         name = "Percent Surface Area Captured (%)") +
      scale_y_continuous(breaks = seq(0, 26, 5),
                         limits = c(0, 27),
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
  
  # Create both plots
  p1 <- create_tree_profile()
  p2 <- create_capture_curve()
  
  # Combine using patchwork
  combined <- p1 + p2 + 
    plot_layout(ncol = 2, widths = c(1, 1)) +
    plot_annotation(
      title = "Tree Surface Area Analysis: 26m Tree with 40cm DBH (Red-Blue Gradient)",
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    )
  
  return(combined)
}

# Create the updated visualization
cat("Creating tree visualization with red-blue gradient...\n")
tree_viz_gradient <- create_tree_visualization()
print(tree_viz_gradient)

cat("\nKEY FEATURES OF GRADIENT VISUALIZATION:\n")
cat("=====================================\n")
cat("• Smooth red-to-blue gradient shows height progression\n")
cat("• Point colors on right plot correspond to tree gradient\n")
cat("• Red (bottom) = low height sampling\n")
cat("• Blue (top) = full tree sampling\n")
cat("• Intermediate heights show interpolated colors\n")