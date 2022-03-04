test_that("pars matching", {
    for (m in dimRedMethodList()) {
        mo <- getMethodObject(m)
        expect(
            all.equal(
                mo@stdpars,
                matchPars(mo, list())
            ),
            paste("par matching for", m, "failed")
        )
    }

    expect_warning(
      embed(iris[1:4], "PCA", asdf = 1234),
      "Parameter matching: asdf is not a standard parameter, ignoring."
    )
})
