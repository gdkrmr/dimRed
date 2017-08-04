

context("drr")


test_that("drr forward and backward passes", {
    spiral <- loadDataSet("Helix", n = 500)

    drr_spiral <- embed(spiral, "DRR", ndim = 3)

    expect_equal(3, getNDim(drr_spiral))
    dsa <- drr_spiral@apply(spiral)
    dsi <- drr_spiral@inverse(dsa)

    expect_equal(dsi, spiral)
})
