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
      scales = NULL, labeller = list(labeller), as.table = as.table
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
    all <- c(.$rows, .$cols)
    
    # Here we need to build up a complete list of all variables used for 
    # facetting in different layers, ideally in such a way that we don't
    # create combinations that don't exist somewhere in the raw data. The
    # complication is that some layers might not have all facetting variables,
    # so this goal might not be achievable (and the code currently doesn't
    # manage to do that, instead including all combinations)
        
    # Get all possible values
    layers <- lapply(data, eval.quoted, expr = all, emptyenv(), try = TRUE)
    # Rearrange so instead of variables nested inside layers,
    # have layers nested inside lists    
    vars <- lapply(names(all), function(var) lapply(layers, "[[", var))
    vars <- lapply(vars, function(x) unique(concat(x)))
    names(vars) <- names(all)
    
    levels <- do.call("expand.grid", vars)  
    levels[] <- lapply(levels, as.factor)

    # Add margins
    margins <- .$margins
    if (is.logical(margins)) {
      if (margins) {
        margins <- c(names(all), "grand_row", "grand_col")
      } else {
        margins <- c()
      }
    }
    margin_vals <- margin.vars(list(names(.$rows), names(.$cols)), margins)
    
    if (length(margin_vals) > 0) {
      all_marg <- as.data.frame(rep(list("(all)"), ncol(levels)))
      names(all_marg) <- names(levels)

      levels <- rbind(levels, ldply(margin_vals, function(var) {
        cunion(unique(levels[var]), all_marg)
      }))      
    }
    
    # Create panel info dataset
    panel <- ninteraction(levels)
    panel <- factor(panel, levels = seq_len(attr(panel, "n")))
    
    panels <- cbind(
      PANEL = panel,
      ROW = ninteraction(levels[names(.$rows)]) %||% 1,
      COL = ninteraction(levels[names(.$cols)]) %||% 1,
      SCALE_X = 1,
      SCALE_Y = 1,
      levels
    )
    panels <- unrowname(panels[order(panels$PANEL), ])
    
    # Relax constraints, if necessary
    if (.$free$x) df$SCALE_X <- panels$ROW
    if (.$free$y) df$SCALE_Y <- panels$COL
    
    .$panel_info <- panels
    invisible(NULL)
  }

  map_layer <- function(., data) {
    # Add extra data for margins
        
    # Compute facetting variables
    all <- c(.$rows, .$cols)
    facet_vals <- as.data.frame(compact(
      eval.quoted(all, data, emptyenv(), try = TRUE)))
          
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

    aspect_ratio <- theme$aspect.ratio
    
    # If user hasn't set aspect ratio, and we have fixed scales, then
    # ask the coordinate system if it wants to specify one
    if (is.null(aspect_ratio) && !.$free$x && !.$free$y) {
      ranges <- coord$compute_ranges(.$panel_scales(1))
      aspect_ratio <- coord$compute_aspect(ranges)
    }
    
    if (is.null(aspect_ratio)) {
      aspect_ratio <- 1
      respect <- FALSE
    } else {
      respect <- TRUE
    }

    panels <- .$panel_info$PANEL
    cols <- which(.$panel_info$ROW == 1)
    rows <- which(.$panel_info$COL == 1)
    
    coord_details <- llply(panels, function(i) {
      coord$compute_ranges(.$panel_scales(i))
    })
    
    # Horizontal axes
    axes_h <- lapply(coord_details[cols], coord$guide_axis_h, theme)
    axes_h_height <- do.call("max2", llply(axes_h, grobHeight))
    axeshGrid <- grobGrid(
      "axis_h", axes_h, nrow = 1, ncol = length(cols),
      heights = axes_h_height, clip = "off"
    )
    
    # Vertical axes
    axes_v <- lapply(coord_details[rows], coord$guide_axis_v, theme)
    axes_v_width <- do.call("max2", llply(axes_v, grobWidth))
    axesvGrid <- grobGrid(
      "axis_v", axes_v, nrow = length(rows), ncol = 1,
      widths = axes_v_width, as.table = .$as.table, clip = "off"
    )
    
    # Strips
    labels <- .$labels_default(.$shape, theme)
    
    strip_widths <- llply(labels$v, grobWidth)
    strip_widths <- do.call("unit.c", llply(1:ncol(strip_widths), 
      function(i) do.call("max2", strip_widths[, i])))
    stripvGrid <- grobGrid(
      "strip_v", labels$v, nrow = nrow(labels$v), ncol = ncol(labels$v),
      widths = strip_widths, as.table = .$as.table
    )
      
    strip_heights <- llply(labels$h, grobHeight)
    strip_heights <- do.call("unit.c", llply(1:nrow(strip_heights),
       function(i) do.call("max2", strip_heights[i, ])))
    striphGrid <- grobGrid(
      "strip_h", labels$h, nrow = nrow(labels$h), ncol = ncol(labels$h),
      heights = strip_heights
    )
    
    # Add background and foreground to panels
    panels_grob <- lapply(panels, function(i) {
      fg <- coord$guide_foreground(coord_details[[i]], theme)
      bg <- coord$guide_background(coord_details[[i]], theme)
      grobTree(bg, panels_grob[[i]], fg)      
    })
    dim(panels_grob) <- c(length(rows), length(cols))

    if(.$space_is_free) {
      size <- function(y) unit(diff(y$output_expand()), "null")
      panel_widths <- do.call("unit.c", llply(.$scales$x, size))
      panel_heights <- do.call("unit.c", llply(.$scales$y, size))
    } else {
      panel_widths <- unit(1, "null")
      panel_heights <- unit(1 * aspect_ratio, "null")
    }

    panelGrid <- grobGrid(
      "panel", t(panels_grob), ncol = length(cols), nrow = length(rows),
      widths = panel_widths, heights = panel_heights, as.table = .$as.table,
      respect = respect
    )
       
    # Add gaps and compute widths and heights
    fill_tl <- spacer(nrow(labels$h), 1)
    fill_tr <- spacer(nrow(labels$h), ncol(labels$v))
    fill_bl <- spacer(1, 1)
    fill_br <- spacer(1, ncol(labels$v))
    
    all <- rbind(
      cbind(fill_tl,   striphGrid, fill_tr),
      cbind(axesvGrid, panelGrid,  stripvGrid),
      cbind(fill_bl,   axeshGrid,  fill_br) 
    )
    # theme$panel.margin, theme$panel.margin
    
    # from left to right
    hgap_widths <- do.call("unit.c", compact(list(
      unit(0, "cm"), # no gap after axis
      rep.unit2(theme$panel.margin, length(cols) - 1), # gap after all panels except last
      unit(rep(0, ncol(stripvGrid) + 1), "cm") # no gap after strips 
    )))
    hgap <- grobGrid("hgap", 
      ncol = ncol(all), nrow = nrow(all),
      widths = hgap_widths, 
    )
    
    # from top to bottom
    vgap_heights <- do.call("unit.c", compact(list(
      unit(rep(0, nrow(striphGrid) + 1), "cm"), # no gap after strips 
      rep.unit2(theme$panel.margin, length(rows) - 1), # gap after all panels except last
      unit(0, "cm") # no gap after axis
    )))
    
    vgap <- grobGrid("vgap",
      nrow = nrow(all), ncol = ncol(all) * 2,
      heights = vgap_heights
    )
    
    rweave(cweave(all, hgap), vgap)
  }


  labels_default <- function(., gm, theme) {
    col_vars <- ddply(.$panel_info, "COL", uniquecols)
    row_vars <- ddply(.$panel_info, "ROW", uniquecols)

    list(
      h = t(.$make_labels(col_vars, theme)), 
      v = .$make_labels(row_vars, theme, horizontal = FALSE)
    )
  }
  
  make_labels <- function(., label_df, theme, ...) {
    labeller <- match.fun(.$labeller[[1]])
    
    label_df <- label_df[setdiff(names(label_df), 
      c("PANEL", "COL", "ROW", "SCALE_X", "SCALE_Y"))]
  
    labels <- matrix(list(), nrow = nrow(label_df), ncol = ncol(label_df))
    for (i in seq_len(ncol(label_df))) {
      labels[, i] <- labeller(names(label_df)[i], label_df[, i])
    }
    
    if (nrow(label_df) == 1) {
      grobs <- matrix(list(zeroGrob()))
    } else {
      grobs <- apply(labels, c(1,2), ggstrip, theme = theme, ...)
    }
    grobs
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