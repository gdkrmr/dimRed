


context("dimRedMethod-class")

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
})
