GeomErrorbarh <- proto(Geom, {
  objname <- "errorbarh"
  desc <- "Horizontal error bars"
  icon <- function(.) {
    gTree(children=gList(
      segmentsGrob(c(0.5, 0.3), c(0.70, 0.30), c(0.9, 0.7), c(0.70, 0.30)),
      segmentsGrob(c(0.5, 0.3), c(0.55, 0.15), c(0.5, 0.3), c(0.85, 0.45)),
      segmentsGrob(c(0.9, 0.7), c(0.55, 0.15), c(0.9, 0.7), c(0.85, 0.45))
    ))
  }
  
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour = "black", size=0.5, linetype=1, width=0.5, alpha = 1)
  guide_geom <- function(.) "path"
  required_aes <- c("x", "xmin", "xmax")
  
  reparameterise <- function(., df, params) {
    df$height <- df$height %||% 
      params$height %||% (resolution(df$y, FALSE) * 0.9)
        
    transform(df,
      ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  }

  seealso <- list(
    "geom_errorbar" = "vertical error bars"
  )

  draw <- function(., data, scales, coordinates, width = NULL, ...) {
    GeomPath$draw(with(data, data.frame( 
      x = as.vector(rbind(xmax, xmax, NA, xmax, xmin, NA, xmin, xmin)),
      y = as.vector(rbind(ymin, ymax, NA, y,    y,    NA, ymin, ymax)), 
      colour = rep(colour, each = 8),
      alpha = rep(alpha, each = 8),
      size = rep(size, each = 8),
      linetype = rep(linetype, each = 8),
      group = rep(1:(nrow(data)), each = 8),
      stringsAsFactors = FALSE, 
      row.names = 1:(nrow(data) * 8)
    )), scales, coordinates, ...)
  }
  
  examples <- function(.) {
    df <- data.frame(
      trt = factor(c(1, 1, 2, 2)), 
      resp = c(1, 5, 3, 4), 
      group = factor(c(1, 2, 1, 2)), 
      se = c(0.1, 0.3, 0.3, 0.2)
    )
    
    # Define the top and bottom of the errorbars
    
    p <- ggplot(df, aes(resp, trt, fill = group))
    p + geom_point() +
      geom_errorbarh(aes(xmax = resp + se, xmin = resp - se))
      
    last_plot() + aes(colour = group)
      
  }
})
