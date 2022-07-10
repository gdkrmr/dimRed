test_that("HLLE", {
  if(requireNamespace(dimRed:::getMethodDependencies("HLLE"), quietly = TRUE))
    expect_error(embed(iris[1:4], "HLLE", ndim = 1, .mute = c("message", "output")),
                 "ndim must be 2 or larger.")
})
