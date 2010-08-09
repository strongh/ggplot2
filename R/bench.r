#' Benchmark plot creation time.
#' Broken down into construct, build, render and draw times.
#'
#' @param x code to create ggplot2 plot
#' @export
#' @internal
#' @examples
#' benchplot(qplot(mpg, wt, data = mtcars))
#' benchmark(qplot(mpg, wt, data = mtcars) + facet_grid(.~ cyl))
benchplot <- function(x) {
  
  construct <- system.time(force(x))
  stopifnot(inherits(x, "ggplot"))
  
  build <- system.time(pieces <- ggplot_build(x))
  render <- system.time(grob <- ggplotGrob(x, pieces))
  draw <- system.time(grid.draw(grob))
  
  times <- rbind(construct, build, render, draw)[, 1:3]
  
  unrowname(data.frame(
    step = c("construct", "build", "render", "draw", "TOTAL"),
    rbind(times, colSums(times)))) 
}