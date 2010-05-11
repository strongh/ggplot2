# Build ggplot for rendering
# This function is the powerhouse that converts the plot specification into something that's ready to be rendered on screen
# 
# @keywords internal
ggplot_build <- function(plot) {
  if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)
  
  plot <- plot_clone(plot)
  layers <- plot$layers
  scales <- plot$scales
  facet <- plot$facet
  cs <- plot$coordinates
  # Apply function to layer and matching data
  dlapply <- function(f) mlply(cbind(d = data, p = layers), f)

  # Facet - adding extra data for margins & missing facetting variables,
  # and adding on a PANEL variable
  layer_data <- lapply(layers, function(y) y$data)
  facet$train(c(list(plot$data), layer_data))
  data <- facet$map(layer_data, plot$data)
  
  # Compute aesthetics to produce data with generalised variable names
  data <- dlapply(function(d, p) p$compute_aesthetics(d, plot))
  
  # Transform all scales
  data <- lapply(data, scales$transform_df)
  
  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  facet$position_train(data, scales)
  data <- facet$position_map(data)
  
  # Apply and map statistics, then reparameterise geoms that need it
  data <- dlapply(function(d, p) facet$calc_statistics(d, p)) 
  data <- dlapply(function(d, p) p$map_statistic(d, plot)) 
  data <- dlapply(function(d, p) p$reparameterise(d))

  # Adjust position
  data <- dlapply(function(d, p) p$adjust_position(d, scales))
  
  npscales <- scales$non_position_scales()
  
  # Train and map, for final time
  if (npscales$n() > 0) {
    lapply(data, npscales$train_df)
    data <- lapply(data, npscales$map_df)
  }
  facet$position_train(data, scales)
  data <- facet$position_map(data)

  # Produce grobs
  grobs <- dlapply(function(d, p) facet$make_grobs(d, p, cs))
  
  grobs3d <- matrix(unlist(grobs, recursive=FALSE), ncol = length(layers))
  panels <- aaply(grobs3d, 1, splat(grobTree), .drop = FALSE)
  
  list(
    data = data,
    plot = plot,
    scales = npscales,
    cs = cs,
    panels = panels,
    facet = facet
  )
}

