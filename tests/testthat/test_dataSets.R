context("dataSets")


test_that("datasets load", {
    for (d in dataSetList()) {
        ds <- loadDataSet(d)
        expect(inherits(ds, "dimRedData"), "must be of class 'dimRedData'")
    }
})
