

context("drr")


test_that("drr forward and backward passes", {
    spiral <- loadDataSet("Helix", n = 500)

    drr.spiral <- embed(spiral, "DRR", ndim = 3)

    dsa <- drr.spiral@apply(spiral)
    dsi <- drr.spiral@inverse(dsa)

    expect_equal(dsi, spiral)
})
