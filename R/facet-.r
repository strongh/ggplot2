Facet <- proto(TopLevel, {
  scales <- list()
  
  clone <- function(.) {
    as.proto(.$as.list(all.names=TRUE), parent=.) 
  }
  objname <- "Facet"
  class <- function(.) "facet"
  
  html_returns <- function(.) {
    ps(
      "<h2>Returns</h2>\n",
      "<p>This function returns a facet object.</p>"
    )
  }
  
  parameters <- function(.) {
    params <- formals(get("new", .))
    params[setdiff(names(params), c(".","variable"))]
  }
  
  xlabel <- function(., theme) {
    nulldefault(.$scales$x[[1]]$name, theme$labels$x)
  }
    
  ylabel <- function(., theme) 
    nulldefault(.$scales$y[[1]]$name, theme$labels$y)
  
  
  # Data is mapped after training to ensure that all layers have extra
  # copies of data for margins and missing facetting variables, and 
  # has a PANEL variable that tells which panel it belongs to.
  # 
  # @param data a list of data frames (one for each layer)  
  map <- function(., layer_data, plot_data) {
    lapply(layer_data, function(data) {
      if (empty(data)) data <- plot_data
      .$map_layer(data)
    })
  }

    
  # Position scales ----------------------------------------------------------

  position_train <- function(., data, scales) {
    x_scales <- unique(.$panel_info$SCALE_X)
    y_scales <- unique(.$panel_info$SCALE_Y)

    # Initialise scales if needed and if available
    if (is.null(.$scales$x) && scales$has_scale("x")) {
      .$scales$x <- rlply(length(x_scales), scales$get_scales("x")$clone())
    }
    if (is.null(.$scales$y) && scales$has_scale("y")) {
      .$scales$y <- rlply(length(y_scales), scales$get_scales("y")$clone())
    }

    # For each layer of data
    l_ply(data, function(l) .$position_train_layer(l))
  }

  position_train_layer <- function(., data) {
    scale_x <- .$panel_info$SCALE_X[match(data$PANEL, .$panel_info$PANEL)]
    l_ply(unique(scale_x), function(i) {
      raw <- data[scale_x == i, , ]
      .$scales$x[[i]]$train_df(raw, drop = .$free$x)
    })

    scale_y <- .$panel_info$SCALE_Y[match(data$PANEL, .$panel_info$PANEL)]
    l_ply(unique(scale_y), function(i) {
      raw <- data[scale_y == i, , ]
      .$scales$y[[i]]$train_df(raw, drop = .$free$y)
    })
  }

  position_map <- function(., layer_data) {
    lapply(layer_data, function(data) .$position_map_layer(data))
  }

  position_map_layer <- function(., data) {
    scale_x <- .$panel_info$SCALE_X[match(data$PANEL, .$panel_info$PANEL)]
    data <- ldply(unique(scale_x), function(i) {
      old <- data[scale_x == i, , ]
      new <- .$scales$x[[i]]$map_df(old)
      cunion(new, old)
    })

    scale_y <- .$panel_info$SCALE_Y[match(data$PANEL, .$panel_info$PANEL)]
    data <- ldply(unique(scale_y), function(i) {
      old <- data[scale_y == i, , ]
      new <- .$scales$y[[i]]$map_df(old)
      cunion(new, old)
    })

    data
  }

  calc_statistics <- function(., data, layer) {
    ddply(data, "PANEL", function(panel_data) {
      scales <- .$panel_scales(data$PANEL[1])

      layer$calc_statistic(panel_data, scales)
    })
  }

  make_grobs <- function(., data, layer, coord) {
    dlply(data, "PANEL", .drop = FALSE, function(panel_data) {
      scales <- .$panel_scales(data$PANEL[1])

      details <- coord$compute_ranges(scales)
      layer$make_grob(panel_data, details, coord)
    })
  }

  panel_scales <- function(., panel) {
    scale_x <- .$scales$x[[subset(.$panel_info, PANEL == panel)$SCALE_X]]
    scale_y <- .$scales$y[[subset(.$panel_info, PANEL == panel)$SCALE_Y]]

    list(x = scale_x, y = scale_y)
  }
})