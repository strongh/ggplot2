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
  facet_values <- function(layer_df, vars) {
    values <- eval.quoted(layer_df, expr = vars, emptyenv(), try = TRUE)
    as.data.frame(compact(values))
  }
  values <- compact(llply(data, facet_values, vars = c(rows, cols)))

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