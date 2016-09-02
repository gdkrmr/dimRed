


context("dimRedMethod-class")

test_that(paste('match empty pars Pars', mo), {
    for(m in dimRedMethodList()) {
        mo <- getMethodObject(m)
        expect(
            all.equal( mo@stdpars, matchPars(mo, list()) )
        )
    }
})

