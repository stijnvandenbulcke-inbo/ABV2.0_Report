library(terra)
library(ggplot2)
plot_focal_heatmap <- function(kernel, r_template) {
  cell_size <- terra::res(r_template)
  cells <- expand.grid(row = seq_len(nrow(kernel)), col = seq_len(ncol(kernel)))
  cells$x <- (cells$col - (ncol(kernel) + 1) / 2) * cell_size[1]
  cells$y <- ((nrow(kernel) + 1) / 2 - cells$row) * cell_size[2]
  cells$weight <- as.vector(kernel) / sum(kernel, na.rm = TRUE)

  circles <- expand.grid(
    angle = seq(0, 2 * pi, length.out = 361),
    radius = c(25, 50, 75, 100)
  )
  circles$x <- circles$radius * cos(circles$angle)
  circles$y <- circles$radius * sin(circles$angle)
  labels <- data.frame(radius = c(25, 50, 75, 100))

  ggplot(cells, aes(x = x, y = y)) +
    geom_tile(aes(fill = weight), width = cell_size[1], height = cell_size[2]) +
    
    # 1. Halo backing line for high contrast across all colors
    geom_path(
      data = circles, 
      aes(group = radius), 
      linetype = "solid", 
      color = "white", 
      linewidth = 0.8,
      alpha = 0.7
    ) +
    # 2. Black dotted ring on top (visible on yellow, green, and purple)
    geom_path(
      data = circles, 
      aes(group = radius), 
      linetype = "dotted", 
      color = "black", 
      linewidth = 0.5
    ) +
    
    # 3. Crisp, clear distance labels
    geom_label(
      data = labels,
      aes(x = 0, y = radius, label = paste0(radius, " m")),
      inherit.aes = FALSE, 
      size = 3.2,
      fontface = "bold",
      color = "black",
      fill = "white",
      label.size = 0.25,                  # Subtle border frame
      label.padding = unit(0.2, "lines")
    ) +
    
    # 4. Full-width, thicker colorbar matching the raster square exactly
    scale_fill_viridis_c(
      name = NULL,
      labels = function(x) sprintf("%.2f%%", 100 * x),
      guide = guide_colorbar(
        barwidth = unit(0.5, "cm"),       # Matches 100% of the raster square width
        barheight = unit(0.6, "npc"),     # Fatter bar
        ticks = TRUE,
        ticks.colour = "black",
        ticks.linewidth = 0.5,
        frame.colour = "grey30",         # Clean outer frame around the bar
        frame.linewidth = 0.5
      )
    ) +
    coord_equal() +
    theme_void() +
    theme(
      legend.position = "right",
      legend.margin = margin(t = 12)
    )
}


