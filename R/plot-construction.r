# Plot construction
# The elements of a ggplot plot are combined together with addition.
# 
# \itemize{
#   \item \code{data.frame}: replace default data.frame (must use \code{\%+\%})
#   \item \code{uneval}: replace default aesthetics
#   \item \code{layer}: add new layer
#   \item \code{options}: update plot options
#   \item \code{scale}: replace default scale
#   \item \code{coord}: override default coordinate system
#   \item \code{facet}: override default coordinate faceting
# }
#
# @arguments plot object
# @argument object to add
# @seealso \code{\link{set_last_plot}}, \code{\link{ggplot}}
# @keyword internal
# @alias \%+\%
"+.ggplot" <- function(p, object) {
  p <- plot_clone(p)

  if (is.data.frame(object)) {
    p$data <- object
  } else if (inherits(object, "options")) {
    object$labels <- defaults(object$labels, p$options$labels)
    p$options <- defaults(object, p$options)
  } else if(inherits(object, "labels")) {
      p <- update_labels(p, object)
  } else if(inherits(object, "uneval")) {
      p$mapping <- defaults(object, p$mapping)
      
      labels <- lapply(object, deparse)
      names(labels) <- names(object)
      p <- update_labels(p, labels)
  } else if(is.list(object)) {
    for (o in object) {
      p <- p + o
    }
  } else {
    p <- switch(object$class(),
      layer  = {
        p$layers <- append(p$layers, object)
        
        # Add any new labels
        mapping <- as.list(as.character(object$mapping))
        default <- as.list(as.character(object$stat$default_aes()))
        
        new_labels <- defaults(mapping, default)
        p$options$labels <- defaults(p$options$labels, new_labels)
        p
      },
      coord = {
        p$coordinates <- object
        p
      },
      facet = {
        p$facet <- object
        p
      },
      scale = {
        p$scales$add(object)
        p
      }
    )
  }
  set_last_plot(p)
  p
}
"%+%" <- `+.ggplot`