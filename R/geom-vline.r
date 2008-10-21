GeomVline <- proto(Geom, {
  new <- function(., ...) {
    .super$new(., ..., ignore.extra = TRUE)
  }
  
  reparameterise <- function(., df, params) {
    intercept <- params$intercept

    if (is.null(intercept)) {
      # Intercept comes from data, default to 0 if not set
      if (is.null(df$intercept)) df$intercept <- 0
      
    } else if (is.numeric(intercept)) {
      # Intercept is a numeric vector of positions
      df <- df[rep(1, length(intercept)), ]
      df$intercept <- intercept
      
    } else if (is.character(intercept) || is.function(intercept)) {
      # Intercept is a function
      f <- match.fun(intercept)
      trans <- function(df) transform(df, intercept = f(x))
      df <- ddply(df, .(group), trans)
    } else {
      stop("Invalid intercept type: should be a numeric vector, a function", 
           ", or a name of a function", call. = FALSE)
    }
    
    unique(within(df, {
      x <- intercept
      xend <- intercept
      y <- NULL
    }))
  }
  
  draw <- function(., data, intercept = NULL, munched, ...) {
    data <- within(data, {
      y <- 0
      yend <- 1
    })
          
    GeomSegment$draw(data, range, munched)
  }

  objname <- "vline"
  desc <- "Line, vertical"
  icon <- function(.) linesGrob(c(0.5, 0.5), c(0, 1))
  details <- "<p>This geom allows you to annotate the plot with vertical lines (see geom_hline and geom_abline for other types of lines)</p>\n\n<p>There are two ways to use it.  You can either specify the intercept of the line in the call to the geom, in which case the line will be in the same position in every panel.  Alternatively, you can supply a different intercept for each panel using a data.frame.  See the examples for the differences</p>"
  
  default_stat <- function(.) StatIdentity
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1)
  guide_geom <- function(.) "vline"

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data, 
      ggname(.$my_name(), segmentsGrob(0.5, 0, 0.5, 1, default.units="npc",
      gp=gpar(col=colour, lwd=size * .pt, lty=linetype, lineend="butt")))
    )
  }

  seealso <- list(
    geom_hline = "for horizontal lines",
    geom_abline = "for lines defined by a slope and intercept",
    geom_segment = "for a more general approach"
  )
  
  examples <- function(.) {
    # Fixed lines
    p <- ggplot(mtcars, aes(x = wt, y=mpg)) + geom_point()
    p + geom_vline(intercept=5)
    p + geom_vline(intercept=1:5)
    p + geom_vline(intercept=1:5, colour="green")
    p + geom_vline(intercept="mean", size=2, colour = alpha("red", 0.2))
    
    last_plot() + coord_equal()
    last_plot() + coord_flip()
    
    # Lines from data
    p <- ggplot(mtcars, aes(x = wt, y=mpg)) + geom_point()
    p + geom_vline(intercept="mean") + facet_grid(. ~ cyl)
    p + geom_vline(aes(colour = factor(cyl)), intercept="mean")
    p + geom_vline(aes(intercept = wt))
  }  
})
