#' Layout facets in a grid
#' 
#' @params data list of data frames, one for each layer
#' @params rows variables that form the rows
#' @params cols variables that form the columns
#' @return a data frame with columns \code{PANEL}, \code{ROW} and \code{COL},
#'   that match the facetting variable values up with their position in the 
#'   grid
#' @keywords internal
layout_grid <- function(data, rows = NULL, cols = NULL, margins = NULL) {
  if (length(rows) == 0 && length(cols) == 0) return(layout_null())
  rows <- as.quoted(rows)
  cols <- as.quoted(cols)
  
  # For each layer, compute the facet values
  values <- compact(llply(data, quoted_df, vars = c(rows, cols)))

  # Form the base data frame which contains all combinations of facetting
  # variables that appear in the data
  has_all <- unlist(llply(values, length)) == length(rows) + length(cols)
  if (!any(has_all)) {
    stop("At least one layer must contain all variables used for facetting")
  }
  base <- unique(ldply(values[has_all]))
  
  # Systematically add on missing combinations
  for (value in values[!has_all]) {
    if (empty(value)) next;
    
    old <- base[setdiff(names(base), names(value))]
    new <- value[intersect(names(base), names(value))]
    
    base <- rbind(base, expand.grid.df(old, new))
  }

  # Add margins
  base <- add_margins(base, names(rows), names(cols), margins)
  base[] <- lapply(base[], as.factor)

  # Create panel info dataset
  panel <- ninteraction(base)
  panel <- factor(panel, levels = seq_len(attr(panel, "n")))
  
  panels <- cbind(
    PANEL = panel,
    ROW = ninteraction(base[names(rows)]) %||% 1,
    COL = ninteraction(base[names(cols)]) %||% 1,
    base
  )
  arrange(panels, PANEL)
}

layout_null <- function(data) { 
   data.frame(PANEL = 1, ROW = 1, COL = 1)
}

#' Take single layer of data and combine it with panel information to split
#' data into different panels.  Adds in extra data for missing facetting
#' levels and for margins.
#'
#' @params data a data frame
locate_grid <- function(data, panels, rows = NULL, cols = NULL, margins = FALSE) {
  rows <- as.quoted(rows)
  cols <- as.quoted(cols)
  vars <- c(names(rows), names(cols))
  
  # Compute facetting values and add margins
  data <- add_margins(data, names(rows), names(cols), margins)
  facet_vals <- quoted_df(data, c(rows, cols))
  
  # If any facetting variables are missing, add them in by 
  # duplicating the data
  missing_facets <- setdiff(vars, names(facet_vals))
  if (length(missing_facets) > 0) {
    to_add <- unique(panels[missing_facets])
    
    data_rep <- rep.int(1:nrow(data), nrow(to_add))
    facet_rep <- rep(1:nrow(to_add), each = nrow(data))
    
    data <- unrowname(data[data_rep, , drop = FALSE])
    facet_vals <- unrowname(cbind(
      facet_vals[data_rep, ,  drop = FALSE], 
      to_add[facet_rep, , drop = FALSE]))
  }
  
  # Add PANEL variable
  if (nrow(facet_vals) == 0) {
    # Special case of no facetting
    data$PANEL <- 1
  } else {
    facet_vals[] <- lapply(facet_vals[], as.factor)
    keys <- join.keys(facet_vals, panels, by = vars)

    data$PANEL <- panels$PANEL[match(keys$x, keys$y)]
  }
  
  arrange(data, PANEL)
}

quoted_df <- function(data, vars) {
  values <- eval.quoted(data, expr = vars, emptyenv(), try = TRUE)
  as.data.frame(compact(values))
}