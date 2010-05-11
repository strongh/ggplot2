FacetGrid <- proto(Facet, {
  new <- function(., facets, margins = FALSE, scales = "fixed", space = "fixed", labeller = "label_value", as.table = TRUE) {
    scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
    free <- list(
      x = any(scales %in% c("free_x", "free")),
      y = any(scales %in% c("free_y", "free"))
    )
    space <- match.arg(space, c("fixed", "free"))
    
    # Facets can either be a formula, a string, or a list of things to be
    # convert to quoted
    if (is.character(facets)) {
      facets <- as.formula(facets)
    }
    if (is.formula(facets)) {
      rows <- as.quoted.formula(facets[[2]])
      rows <- rows[!sapply(rows, identical, as.name("."))]
      cols <- as.quoted.formula(facets[[3]])
      cols <- cols[!sapply(cols, identical, as.name("."))]
    }
    if (is.list(facets)) {
      rows <- as.quoted(facets[[1]])
      cols <- as.quoted(facets[[2]])
    }
    if (length(rows) + length(cols) == 0) {
      stop("Must specify at least one variable to facet by", call. = FALSE)
    }
    
    .$proto(
      rows = rows, cols = cols, margins = margins,
      free = free, space_is_free = (space == "free"),
      labeller = labeller, as.table = as.table
    )
  }
  
  # Train facetter with data from plot and all layers.  
  # 
  # This creates the panel_info data frame which maps from data values to
  # panel coordinates: ROW, COL and PANEL. It also records the panels that
  # contribute to each x and y scale.
  # 
  # @param data a list of data frames (one for the plot and one for each
  #   layer)
  train <- function(., data) { 
    panels <- layout_grid(data, .$rows, .$cols, .$margins)

    # Relax constraints, if necessary
    if (.$free$x) panels$SCALE_X <- panels$ROW
    if (.$free$y) panels$SCALE_Y <- panels$COL
    
    .$panel_info <- panels
    invisible(NULL)
  }

  map_layer <- function(., data) {
    # Compute facetting variables
    all <- c(.$rows, .$cols)
    facet_vals <- as.data.frame(compact(
      eval.quoted(all, data, emptyenv(), try = TRUE)))

    data <- add_margins(data, names(.$rows), names(.$cols), .$margins)
          
    # If any facetting variables are missing, add them in by 
    # duplicating the data
    missing_facets <- setdiff(names(all), names(facet_vals))
    if (length(missing_facets) > 0) {
      to_add <- unique(.$panel_info[missing_facets])
      
      data_rep <- rep.int(1:nrow(data), nrow(to_add))
      facet_rep <- rep(1:nrow(to_add), each = nrow(data))

      data <- data[data_rep, ]
      facet_vals <- cbind(
        facet_vals[data_rep, ,  drop = FALSE], 
        to_add[facet_rep, , drop = FALSE])
    }
    
    # Add PANEL variable
    if (nrow(facet_vals) == 0) {
      # Special case of no facetting
      data$PANEL <- 1
    } else {
      facet_vals[] <- lapply(facet_vals[], as.factor)
      keys <- join.keys(facet_vals, .$panel_info, by = names(all))

      data$PANEL <- .$panel_info$PANEL[match(keys$x, keys$y)]      
    }
    
    data
  }

  # Create grobs for each component of the panel guides
  add_guides <- function(., panels_grob, coord, theme) {
    coord_details <- llply(.$panel_info$PANEL, function(i) {
      coord$compute_ranges(.$panel_scales(i))
    })
    
    axes <- .$build_axes(coord, coord_details, theme)
    strips <- .$build_strips(coord_details, theme)
    panels <- .$build_panels(panels_grob, coord, coord_details, theme)
    # legend
    # labels
    
    # Combine components into complete plot
    centre <- (axes$l$clone())$cbind(panels)$cbind(strips$r)
    top <- (strips$t$clone())$
      add_cols(strips$r$widths)$
      add_cols(axes$l$widths, pos = 0)
    bottom <- (axes$b$clone())$
      add_cols(strip$r$widths)$
      add_cols(axes$l$widths, pos = 0)
      
    complete <- centre$clone()$
      rbind(top, pos = 0)$
      rbind(bottom)
    complete$respect <- panels$respect
    complete$name <- "layout"
    
    complete
  }
  
  build_strips <- function(., coord_details, theme) {
    col_vars <- ddply(.$panel_info, "COL", uniquecols)
    row_vars <- ddply(.$panel_info, "ROW", uniquecols)

    list(
      r = .$build_strip(row_vars, theme, "r"), 
      t = .$build_strip(col_vars, theme, "t")
    )
  }
  
  build_strip <- function(., label_df, theme, side = "right") {
    side <- match.arg(side, c("t", "l", "b", "r"))
    horizontal <- side %in% c("t", "b")
    labeller <- match.fun(.$labeller)
    
    label_df <- label_df[setdiff(names(label_df), 
      c("PANEL", "COL", "ROW", "SCALE_X", "SCALE_Y"))]
    
    # No labelling data, so return empty row/col
    if (empty(label_df)) {
      if (horizontal) {
        widths <- unit(rep(1, max(.$panel_info$COL)), "null")
        return(layout_empty_row(widths))
      } else {
        heights <- unit(rep(1, max(.$panel_info$ROW)), "null")
        return(layout_empty_col(heights))
      }
    }
    
    # Create matrix of labels
    labels <- matrix(list(), nrow = nrow(label_df), ncol = ncol(label_df))
    for (i in seq_len(ncol(label_df))) {
      labels[, i] <- labeller(names(label_df)[i], label_df[, i])
    }
    
    # Render as grobs
    grobs <- aaply(labels, c(1,2), ggstrip, theme = theme, 
      horizontal = horizontal, .drop = FALSE)
    
    # Create layout
    name <- paste("strip", side, sep = "-")
    if (horizontal) {
      grobs <- t(grobs)
      
      # Each row is as high as the highest as a wide as the panel
      row_height <- function(row) max(laply(row, height_cm))
      heights <- unit(apply(grobs, 1, row_height), "cm")
      widths <- unit(rep(1, ncol(grobs)), "null")
    } else {
      # Each row is wide as the widest and as high as the panel
      col_width <- function(col) max(laply(col, width_cm))
      widths <- unit(apply(grobs, 2, col_width), "cm")
      heights <- unit(rep(1, nrow(grobs)), "null")
    }
    strips <- layout_matrix(name, grobs, heights = heights, widths = widths)
    
    if (horizontal) {
      strips$add_col_space(theme$panel.margin)
    } else {
      strips$add_row_space(theme$panel.margin)
    }
    strips
  }
  
  build_axes <- function(., coord, coord_details, theme) {
    axes <- list()
    
    # Horizontal axes
    cols <- which(.$panel_info$ROW == 1)
    grobs <- lapply(coord_details[cols], coord$guide_axis_h, theme)
    axes$b <- layout_row("axis-b", grobs)$add_col_space(theme$panel.margin)
    
    # Vertical axes
    rows <- which(.$panel_info$COL == 1)
    grobs <- lapply(coord_details[rows], coord$guide_axis_v, theme)
    axes$l <- layout_col("axis-l", grobs)$add_row_space(theme$panel.margin)

    axes
  }
  build_panels <- function(., panels_grob, coord, coord_details, theme) {
    aspect_ratio <- theme$aspect.ratio
    
    # If user hasn't set aspect ratio, and we have fixed scales, then
    # ask the coordinate system if it wants to specify one
    if (is.null(aspect_ratio) && !.$free$x && !.$free$y) {
      aspect_ratio <- coord$compute_aspect(coord_details[[1]])
    }
    
    if (is.null(aspect_ratio)) {
      aspect_ratio <- 1
      respect <- FALSE
    } else {
      respect <- TRUE
    }
    
    # Add background and foreground to panels
    panels <- .$panel_info$PANEL    
    panel_grobs <- lapply(panels, function(i) {
      fg <- coord$guide_foreground(coord_details[[i]], theme)
      bg <- coord$guide_background(coord_details[[i]], theme)
      grobTree(bg, panels_grob[[i]], fg)      
    })
    nrow <- max(.$panel_info$ROW)
    ncol <- max(.$panel_info$COL)
    dim(panel_grobs) <- c(nrow, ncol)

    if(.$space_is_free) {
      size <- function(y) unit(diff(y$output_expand()), "null")
      x_scales <- .$panel_info$scale_x[.$panel_info$ROW == 1]
      y_scales <- .$panel_info$scale_y[.$panel_info$COL == 1]

      panel_widths <- do.call("unit.c", llply(.$scales$x, size))[x_scales]
      panel_heights <- do.call("unit.c", llply(.$scales$y, size))[y_scales]
    } else {
      panel_widths <- rep(unit(1, "null"), ncol)
      panel_heights <- rep(unit(1 * aspect_ratio, "null"), nrow)
    }
  

    panels <- layout_matrix("panel", panel_grobs, panel_widths, panel_heights)
    panels$respect <- respect
    panels$add_col_space(theme$panel.margin)
    panels$add_row_space(theme$panel.margin)
    panels
  }


  # Documentation ------------------------------------------------------------

  objname <- "grid"
  desc <- "Lay out panels in a rectangular/tabular manner."
  
  desc_params <- list(
    facets = "a formula with the rows (of the tabular display) on the LHS and the columns (of the tabular display) on the RHS; the dot in the formula is used to indicate there should be no faceting on this dimension (either row or column); the formula can also be entered as a string instead of a classical formula object",
    margins = "logical value, should marginal rows and columns be displayed"
  )
    
  seealso <- list(
    # "cast" = "the formula and margin arguments are the same as those used in the reshape package"
  )  
  
  icon <- function(.) {
    gTree(children = gList(
      rectGrob(0, 1, width=0.95, height=0.05, hjust=0, vjust=1, gp=gpar(fill="grey60", col=NA)),
      rectGrob(0.95, 0.95, width=0.05, height=0.95, hjust=0, vjust=1, gp=gpar(fill="grey60", col=NA)),
      segmentsGrob(c(0, 0.475), c(0.475, 0), c(1, 0.475), c(0.475, 1))
    ))
  }  
  
  examples <- function(.) {
    # faceting displays subsets of the data in different panels
    p <- ggplot(diamonds, aes(carat, ..density..)) +
     geom_histogram(binwidth = 0.2)
    
    # With one variable
    p + facet_grid(. ~ cut)
    p + facet_grid(cut ~ .)

    # With two variables
    p + facet_grid(clarity ~ cut)
    p + facet_grid(cut ~ clarity)
    p + facet_grid(cut ~ clarity, margins=TRUE)
    
    qplot(mpg, wt, data=mtcars, facets = . ~ vs + am)
    qplot(mpg, wt, data=mtcars, facets = vs + am ~ . )
    
    # You can also use strings, which makes it a little easier
    # when writing functions that generate faceting specifications
    # p + facet_grid("cut ~ .")
    
    # see also ?plotmatrix for the scatterplot matrix
    
    # If there isn't any data for a given combination, that panel 
    # will be empty
    qplot(mpg, wt, data=mtcars) + facet_grid(cyl ~ vs)
    
    # If you combine a facetted dataset with a dataset that lacks those
    # facetting variables, the data will be repeated across the missing
    # combinations:
    p <- qplot(mpg, wt, data=mtcars, facets = vs ~ cyl)

    df <- data.frame(mpg = 22, wt = 3)
    p + geom_point(data = df, colour="red", size = 2)
    
    df2 <- data.frame(mpg = c(19, 22), wt = c(2,4), vs = c(0, 1))
    p + geom_point(data = df2, colour="red", size = 2)

    df3 <- data.frame(mpg = c(19, 22), wt = c(2,4), vs = c(1, 1))
    p + geom_point(data = df3, colour="red", size = 2)

    
    # You can also choose whether the scales should be constant
    # across all panels (the default), or whether they should be allowed
    # to vary
    mt <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
    
    mt + facet_grid(. ~ cyl, scales = "free")
    # If scales and space are free, then the mapping between position
    # and values in the data will be the same across all panels
    mt + facet_grid(. ~ cyl, scales = "free", space = "free")
    
    mt + facet_grid(vs ~ am, scales = "free")
    mt + facet_grid(vs ~ am, scales = "free_x")
    mt + facet_grid(vs ~ am, scales = "free_y")
    mt + facet_grid(vs ~ am, scales = "free", space="free")

    # You may need to set your own breaks for consitent display:
    mt + facet_grid(. ~ cyl, scales = "free_x", space="free") + 
      scale_x_continuous(breaks = seq(10, 36, by = 2))
    # Adding scale limits override free scales:
    last_plot() + xlim(10, 15)

    # Free scales are particularly useful for categorical variables
    qplot(cty, model, data=mpg) + 
      facet_grid(manufacturer ~ ., scales = "free", space = "free")
    # particularly when you reorder factor levels
    mpg <- within(mpg, {
      model <- reorder(model, cty)
      manufacturer <- reorder(manufacturer, cty)
    })
    last_plot() %+% mpg + opts(strip.text.y = theme_text())
  }
  
  pprint <- function(., newline=TRUE) {
    cat("facet_", .$objname, "(", .$facets, ", ", .$margins, ")", sep="")
    if (newline) cat("\n")
  }
  
})

# List of scales
# Make a list of scales, cloning if necessary
# 
# @arguments input scale
# @arguments number of scales to produce in output
# @arguments should the scales be free (TRUE) or fixed (FALSE)
# @keywords internal
scales_list <- function(scale, n, free = TRUE) {
  if (free) {
    rlply(n, scale$clone())  
  } else {
    rep(list(scale), n)  
  }
}