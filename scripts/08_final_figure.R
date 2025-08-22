# =============================================================================
# ADD THIS SECTION TO SCRIPT 07_integration.R AFTER THE EXISTING WOODY:LEAF RATIO PLOTS
# (Around line 380, after the existing woody:leaf ratio visualization)
# =============================================================================
# Create compact insert version based on existing p1 plot
cat("\nCreating compact woody:leaf ratio insert for figure panels...\n")
# Create wrapped biome names for the insert (keep full names but wrap for readability)
plot_data_insert <- plot_data %>%
  mutate(
    biome_wrapped = str_wrap(Biome_Full, width = 36)  # Longer width to limit to ~2 lines
  )
# Create insert plot with same structure as p1 but wrapped names
p_insert <- plot_data_insert %>%
  ggplot(aes(x = fct_reorder(biome_wrapped, MODIS_woody_leaf_ratio_2sided), 
             y = MODIS_woody_leaf_ratio_2sided,
             fill = MODIS_woody_leaf_ratio_2sided)) +
  geom_col(alpha = 2, width = 0.6) +  # Thinner bars to create more white space between them
  geom_text(aes(label = sprintf("%.2f", MODIS_woody_leaf_ratio_2sided)), 
            hjust = 1.1, vjust = 0.5, size = 3.5, fontface = "bold", color = "white") +
  scale_fill_gradient2(
    low = "#6aa078",      # Forest green (leaf dominant)
    mid = "#f7cb62",      # Golden rod (balanced) 
    high = "#5d412a",     # Saddle brown (wood dominant)
    midpoint = 0.75,
    guide = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +  # More space for text labels
  coord_flip() +
  labs(
    title = "Woody:Leaf Surface Area Ratio",
    x = NULL,
    y = NULL,
    #caption = "6-fold variation across biomes",
    caption = "Calculated from field-observed leaf area \nand TLS-derived woody area indices"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold", margin = margin(b = 5)),
    plot.caption = element_text(size = 12, hjust = 0.5, face = "italic"),  # Match Boyce style
    #plot.caption = element_text(size = 10, hjust = 0.5, face = "italic", margin = margin(t = 5)),
    axis.text.y = element_text(size = 11, face = "plain", 
                               margin = margin(r = 8),
                               lineheight = 0.75),  # Very tight line spacing, smaller non-bold text
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    #panel.grid = element_blank(),
    plot.margin = margin(t = 8, r = 25, b = 8, l = 8),  # More right margin for text labels
    panel.background = element_rect(fill = "white", color = "black", linewidth = 1)
  )
print(p_insert)
# Display the insert
print(p_insert)
# Save the insert at different aspect ratios
ggsave("data/outputs/woody_leaf_ratio_insert_square.png", p_insert, 
       width = 4, height = 4, dpi = 300, bg = "white")
# Create a wide version (2:1 ish aspect ratio)
ggsave("data/outputs/woody_leaf_ratio_insert_wide.png", p_insert, 
       width = 6.5, height = 3.6, dpi = 600, bg = "white")
cat("\nInsert files created:\n")
cat("- woody_leaf_ratio_insert_square.png (4×4 inches)\n")
cat("- woody_leaf_ratio_insert_wide.png (6×3 inches)\n")