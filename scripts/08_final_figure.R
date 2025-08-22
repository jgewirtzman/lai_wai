# =============================================================================
# ADD THIS SECTION TO SCRIPT 07_integration.R AFTER THE EXISTING WOODY:LEAF RATIO PLOTS
# (Around line 380, after the existing woody:leaf ratio visualization)
# =============================================================================

# Create compact insert version based on existing p1 plot
cat("\nCreating compact woody:leaf ratio insert for figure panels...\n")

# Create wrapped biome names for the insert (keep full names but wrap for readability)
plot_data_insert <- plot_data %>%
  mutate(
    biome_wrapped = str_wrap(Biome_Full, width = 30)  # Longer width for better line breaks
  )

# Create insert plot with same structure as p1 but wrapped names
p_insert <- plot_data_insert %>%
  ggplot(aes(x = fct_reorder(biome_wrapped, MODIS_woody_leaf_ratio_2sided), 
             y = MODIS_woody_leaf_ratio_2sided,
             fill = MODIS_woody_leaf_ratio_2sided)) +
  geom_col(alpha = 0.9, width = 0.6) +  # Thinner bars to create more white space between them
  geom_text(aes(label = sprintf("%.2f", MODIS_woody_leaf_ratio_2sided)), 
            hjust = 1.1, vjust = 0.5, size = 3.5, fontface = "bold", color = "white") +
  scale_fill_gradient2(
    low = "#228B22",      # Forest green (leaf dominant)
    mid = "#DAA520",      # Golden rod (balanced) 
    high = "#8B4513",     # Saddle brown (wood dominant)
    midpoint = 1,
    guide = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +  # More space for text labels
  coord_flip() +
  labs(
    title = "Woody:Leaf Ratios",
    x = NULL,
    y = NULL,
    caption = "6-fold variation across biomes"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold", margin = margin(b = 5)),
    plot.caption = element_text(size = 10, hjust = 0.5, face = "italic", margin = margin(t = 5)),
    axis.text.y = element_text(size = 9, face = "bold", 
                               margin = margin(r = 8),
                               lineheight = 1.1),  # Tight line spacing within labels
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(t = 8, r = 25, b = 8, l = 8),  # More right margin for text labels
    panel.background = element_rect(fill = "white", color = "black", linewidth = 1),
    plot.background = element_rect(fill = "white", color = "black", linewidth = 1)
  )

print(p_insert)

# Display the insert
print(p_insert)

# Save the insert at different sizes for flexibility
ggsave("data/outputs/woody_leaf_ratio_insert_large.png", p_insert, 
       width = 4, height = 4, dpi = 300, bg = "white")

ggsave("data/outputs/woody_leaf_ratio_insert_medium.png", p_insert, 
       width = 3.5, height = 3.5, dpi = 300, bg = "white")

ggsave("data/outputs/woody_leaf_ratio_insert_small.png", p_insert, 
       width = 3, height = 3, dpi = 300, bg = "white")

# Create an ultra-compact version with minimal text
p_insert_minimal <- ggplot(insert_data, aes(x = biome_short, y = MODIS_woody_leaf_ratio_2sided)) +
  geom_col(aes(fill = insert_color), width = 0.7, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.2f", MODIS_woody_leaf_ratio_2sided)), 
            hjust = -0.1, 
            size = 4,
            fontface = "bold") +
  scale_fill_identity() +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(insert_data$MODIS_woody_leaf_ratio_2sided) * 1.15), 
                     expand = c(0, 0)) +
  labs(
    title = "Woody:Leaf Ratios",
    x = NULL, y = NULL,
    caption = "6-fold variation across biomes"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0.5, face = "italic"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(2, 10, 2, 2),
    panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    plot.background = element_rect(fill = "white", color = "black", linewidth = 0.5)
  )

print(p_insert_minimal)

# Save minimal version
ggsave("data/outputs/woody_leaf_ratio_insert_minimal.png", p_insert_minimal, 
       width = 3, height = 3, dpi = 300, bg = "white")

# Print summary for manuscript
cat("\nCOMPACT INSERT SUMMARY:\n")
cat(sprintf("Created woody:leaf ratio insert showing %.1f-fold variation\n", 
            max(insert_data$MODIS_woody_leaf_ratio_2sided) / min(insert_data$MODIS_woody_leaf_ratio_2sided)))
cat(sprintf("Range: %.2f (%s) to %.2f (%s)\n",
            min(insert_data$MODIS_woody_leaf_ratio_2sided),
            insert_data$biome_short[which.min(insert_data$MODIS_woody_leaf_ratio_2sided)],
            max(insert_data$MODIS_woody_leaf_ratio_2sided),
            insert_data$biome_short[which.max(insert_data$MODIS_woody_leaf_ratio_2sided)]))

cat("\nInsert files created:\n")
cat("- woody_leaf_ratio_insert_large.png (4×4 inches)\n")
cat("- woody_leaf_ratio_insert_medium.png (3.5×3.5 inches)\n") 
cat("- woody_leaf_ratio_insert_small.png (3×3 inches)\n")
cat("- woody_leaf_ratio_insert_minimal.png (3×3 inches, minimal text)\n")

cat("\nSuggested manuscript text:\n")
cat("'Woody:leaf surface area ratios vary substantially across forest biomes,\n")
cat("spanning a 6-fold range from leaf-dominated temperate coniferous forests\n")
cat("(ratio = 0.19) to wood-dominated Mediterranean systems (ratio = 1.08).'\n")

# =============================================================================
# UPDATE THE FILES CREATED SECTION AT THE END OF SCRIPT 07
# =============================================================================

# Add these lines to the existing "Files created:" section:
cat("- woody_leaf_ratio_insert_large.png\n")
cat("- woody_leaf_ratio_insert_medium.png\n")
cat("- woody_leaf_ratio_insert_small.png\n")
cat("- woody_leaf_ratio_insert_minimal.png\n")