# Create a comprehensive tree profile with sampling zones and capture curves using area-based gradient
create_tree_visualization <- function() {
  
  # 1. Tree profile with gradient sampling zones based on % area captured
  create_tree_profile <- function() {
    # Create points for tree outline
    heights <- seq(0, 26, 0.1)
    radii <- sapply(heights, get_radius_at_height)
    
    # Stretch factor to make the cone wider and more visible
    stretch_factor <- 5
    
    # Calculate area percentages for coloring
    all_heights <- seq(0, 26, 0.1)
    area_percentages <- numeric(length(all_heights))
    
    for (i in 1:length(all_heights)) {
      h <- all_heights[i]
      if (h == 0) {
        area_percentages[i] <- 0
      } else {
        r_bottom <- get_radius_at_height(0)
        r_top <- get_radius_at_height(h)
        area <- truncated_cone_lateral_area(r_bottom, r_top, h)
        area_percentages[i] <- (area / full_stem_area) * 100
      }
    }
    
    # Create gradient zones with smooth transitions
    n_segments <- 50
    height_segments <- seq(0, 26, length.out = n_segments + 1)
    
    # Calculate area percentage for each segment
    segment_areas <- numeric(n_segments)
    for (i in 1:n_segments) {
      h_mid <- (height_segments[i] + height_segments[i + 1]) / 2
      if (h_mid == 0) {
        segment_areas[i] <- 0
      } else {
        r_bottom <- get_radius_at_height(0)
        r_top <- get_radius_at_height(h_mid)
        area <- truncated_cone_lateral_area(r_bottom, r_top, h_mid)
        segment_areas[i] <- (area / full_stem_area) * 100
      }
    }
    
    # Create color palette
    max_area <- max(segment_areas)
    min_area <- min(segment_areas)
    normalized_areas <- (segment_areas - min_area) / (max_area - min_area)
    gradient_colors <- colorRampPalette(c("#d73027", "#4575b4"))(100)
    segment_colors <- gradient_colors[pmax(1, pmin(100, round(normalized_areas * 99) + 1))]
    
    # Create the cone as a simple triangle with gradient fill that extends to bottom of ellipses
    all_zones <- data.frame()
    
    # Calculate how far down the ellipses extend
    max_ellipse_extension <- max(radii) * stretch_factor * 0.3  # 30% of max radius
    
    for (i in 1:n_segments) {
      h_start <- height_segments[i]
      h_end <- height_segments[i + 1]
      h_seq <- seq(h_start, h_end, length.out = 20)
      r_seq <- sapply(h_seq, get_radius_at_height) * stretch_factor
      
      # Extend the bottom segment to cover the ellipse area
      if (h_start == 0) {
        # For the bottom segment, extend down to cover the base ellipse
        h_seq_extended <- c(seq(-max_ellipse_extension, h_start, length.out = 10), h_seq)
        r_seq_extended <- c(rep(r_seq[1], 10), r_seq)  # Use base radius for extended part
      } else {
        h_seq_extended <- h_seq
        r_seq_extended <- r_seq
      }
      
      # Create triangle segments
      segment_data <- data.frame(
        height = c(h_seq_extended, rev(h_seq_extended)),
        radius = c(r_seq_extended, -rev(r_seq_extended)),
        zone = paste0("segment_", i),
        color = segment_colors[i],
        area_percent = segment_areas[i]
      )
      
      all_zones <- rbind(all_zones, segment_data)
    }
    
    # Tree outline - simple triangle cone
    tree_outline <- data.frame(
      height = c(heights, rev(heights)),
      radius = c(radii * stretch_factor, -rev(radii * stretch_factor))
    )
    
    # Create elliptical cross-sections (perspective view) and base curve
    create_ellipse <- function(height, n_points = 100) {
      r <- get_radius_at_height(height) * stretch_factor
      theta <- seq(0, 2*pi, length.out = n_points)
      
      # Create full ellipse with perspective (flattened vertically for 3D effect)
      ellipse_width <- r
      ellipse_height <- r * 0.3  # Flatten to show perspective
      
      x <- ellipse_width * cos(theta)
      y <- rep(height, n_points) + ellipse_height * sin(theta)
      
      return(data.frame(x = x, y = y))
    }
    
    # Create base curve (just the bottom arc)
    create_base_curve <- function(height = 0, n_points = 50) {
      r <- get_radius_at_height(height) * stretch_factor
      # Only create the bottom portion of the ellipse (from 180° to 360°)
      theta <- seq(pi, 2*pi, length.out = n_points)
      
      ellipse_width <- r
      ellipse_height <- r * 0.3  # Same flattening as ellipses
      
      x <- ellipse_width * cos(theta)
      y <- rep(height, n_points) + ellipse_height * sin(theta)
      
      return(data.frame(x = x, y = y))
    }
    
    # Create ellipses and base curve
    ellipse_2m <- create_ellipse(2)
    ellipse_10m <- create_ellipse(10)
    base_curve <- create_base_curve(0)
    
    # Create the plot
    p1 <- ggplot() +
      # Cone with gradient (semi-transparent triangle)
      geom_polygon(data = all_zones, 
                   aes(x = radius, y = height, fill = color, group = zone), 
                   alpha = 0.7) +
      
      # Cone outline (triangle)
      geom_path(data = tree_outline, 
                aes(x = radius, y = height), 
                color = "black", linewidth = 1.5) +
      
      # Cross-sectional ellipses with smaller dashes and base curve
      geom_path(data = ellipse_2m, aes(x = x, y = y), 
                color = "black", linewidth = 0.7, linetype = "dashed", 
                lineend = "round", linejoin = "round") +
      geom_path(data = ellipse_10m, aes(x = x, y = y), 
                color = "black", linewidth = 0.7, linetype = "dashed",
                lineend = "round", linejoin = "round") +
      geom_path(data = base_curve, aes(x = x, y = y), 
                color = "black", linewidth = 1.5) +
      
      # Use identity fill scale for gradient colors
      scale_fill_identity() +
      coord_fixed(ratio = 0.5) +
      scale_x_continuous(limits = c(-max(radii) * stretch_factor * 1.1, 
                                    max(radii) * stretch_factor * 1.1),
                         name = NULL) +
      scale_y_continuous(breaks = seq(0, 26, 5),
                         limits = c(-0.5, 27),  # Extend below 0 to show full ellipses
                         name = "Height above ground (m)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.position = "none",
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 16),
            axis.ticks.x = element_blank(),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            plot.margin = margin(10, 5, 10, 10),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
    
    return(list(plot = p1, max_area = max_area, min_area = min_area))
  }
  
  # 2. Capture percentage vs height curve with corresponding gradient colors
  create_capture_curve <- function(area_range) {
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
    
    # Function to get gradient color based on area percentage
    get_gradient_color <- function(area_percent) {
      # Normalize area percentage using the same range as the tree profile
      normalized_area <- (area_percent - area_range$min_area) / (area_range$max_area - area_range$min_area)
      normalized_area <- pmax(0, pmin(1, normalized_area))  # Clamp to 0-1
      # Interpolate between red and blue based on area percentage
      gradient_colors <- colorRampPalette(c("#d73027", "#4575b4"))(100)
      return(gradient_colors[pmax(1, pmin(100, round(normalized_area * 99) + 1))])
    }
    
    # Add our specific sampling points with area-based gradient colors
    sampling_points <- data.frame(
      height = c(2, 10, 26),
      stem_percent = results$percent_of_stem,
      tree_percent = results$percent_of_tree,
      label = c("2m", "10m", "Full"),
      gradient_color = sapply(results$percent_of_stem, get_gradient_color)
    )
    
    # Create the plot
    p2 <- ggplot() +
      # Capture curves
      geom_line(data = capture_data, 
                aes(x = stem_percent, y = height), 
                linewidth = 2, alpha = 0.8, color = "black") +
      geom_line(data = capture_data, 
                aes(x = tree_percent, y = height), 
                linewidth = 2, alpha = 0.8, color = "black", linetype = "longdash") +
      
      # Line annotations
      annotate("text", x = 80, y = 21, label = "% of \nStem", 
               fontface = "bold", size = 5, color = "black") +
      annotate("text", x = 40, y = 24, label = "% of \nTree", 
               fontface = "bold", size = 5, color = "black") +
      
      # Sampling points with gradient colors matching the left plot (based on area %)
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
      
      # Point labels with better positioning and area percentage info
      geom_text(data = sampling_points, 
                aes(x = pmax(stem_percent + 12, 12), y = height, 
                    label = paste0(label, "\n(", round(stem_percent, 1), "%)")), 
                hjust = 0, color = "black", fontface = "bold", size = 5) +
      
      # vertical lines at 100%  
      geom_vline(xintercept = 100, 
                 color = "#4575b4", linewidth = 1, linetype = "dashed", alpha = 0.5) +
      
      scale_x_continuous(breaks = c(0, 25, 50, 75, 100),
                         limits = c(0, 105),
                         name = "Percent Surface Area Captured") +
      scale_y_continuous(breaks = seq(0, 26, 5),
                         limits = c(-0.5, 27),  # Match left panel limits
                         name = NULL) +  # Remove y-axis title
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.position = "none",
            axis.text.x = element_text(size = 12),
            axis.text.y = element_blank(),  # Remove y-axis tick labels
            axis.ticks.y = element_blank(),  # Remove y-axis tick marks
            axis.title.x = element_text(size = 14),
            axis.title.y = element_blank(),  # Remove y-axis title
            plot.margin = margin(10, 10, 10, 5),
            panel.grid.minor = element_blank())
    
    return(p2)
  }
  
  # Create both plots
  tree_result <- create_tree_profile()
  p1 <- tree_result$plot
  area_range <- list(max_area = tree_result$max_area, min_area = tree_result$min_area)
  p2 <- create_capture_curve(area_range)
  
  # Combine using patchwork with reduced spacing
  combined <- p1 + p2 + 
    plot_layout(ncol = 2, widths = c(1, 2)) +
    plot_annotation(
      theme = theme(plot.margin = margin(0, 0, 0, 0))
    )
  
  return(combined)
}

# Create the updated visualization
cat("Creating tree visualization with custom x-axis breaks...\n")
tree_viz_area_gradient <- create_tree_visualization()
print(tree_viz_area_gradient)