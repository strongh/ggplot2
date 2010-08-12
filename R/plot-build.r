# Build ggplot for rendering
# This function is the powerhouse that converts the plot specification into something that's ready to be rendered on screen
# 
# @keywords internal
ggplot_build <- function(plot) {
  if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)
  
  plot <- plot_clone(plot)
  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$data)
  scales <- plot$scales

  # Apply function to layer and matching data
  dlapply <- function(f) mlply(cbind(d = data, p = layers), f)

  # Initialise panels, add extra data for margins & missing facetting
  # variables, and add on a PANEL variable to data
  panels <- Panels$clone(scales, plot$coordinates, plot$facet)
  panels$train(layer_data, plot$data)
  browser()
  data <- panels$map(layer_data, plot$data)
  
  # Compute aesthetics to produce data with generalised variable names
  data <- dlapply(function(d, p) p$compute_aesthetics(d, plot))
  
  # Transform all scales
  data <- lapply(data, scales$transform_df)
  
  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  panels$train_scales(data, scales)
  data <- panels$map_scales(data)
  
  # Apply and map statistics
  data <- panels$calc_statistics(data)
  data <- dlapply(function(d, p) p$map_statistic(d, plot)) 
  
  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- dlapply(function(d, p) p$reparameterise(d))

  # Apply position adjustments
  data <- dlapply(function(d, p) p$adjust_position(d, scales))
   
  # Re-train and re-map position scales post stat and position adjustment 
  panels$train_scales(data, scales)
  data <- panels$map_scales(data)
  
  # Train and map non-position scales
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, npscales$train_df)
    data <- lapply(data, npscales$map_df)
  }  
  
  list(data = data, panels = panels)
}

