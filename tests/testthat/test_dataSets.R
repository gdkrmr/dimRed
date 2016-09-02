context("dataSets")


test_that(paste( "dataset: ", d),{
    for(d in dataSetList()) {
        ds <- loadDataSet(d)
        expect(inherits(ds, "dimRedData"), "must be of class 'dimRedData")
    }
})
