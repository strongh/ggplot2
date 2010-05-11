context("Facet panel location") 

test_that("two col cases with no missings", {
  vscyl <- layout_grid(list(mtcars), "cyl", "vs")
  loc <- locate_grid(mtcars, vscyl, "cyl", "vs")  
  
  expect_that(nrow(loc), equals(nrow(mtcars)))
  expect_that(ncol(loc), equals(ncol(mtcars) + 1))
  
})

