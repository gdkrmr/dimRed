
data(iris)
context("PCA")

test_that("general data conversions", {
    irisData <- as(iris[,1:4], 'dimRedData')
    expect_equal(class(irisData)[1], 'dimRedData')
    
    irisParsCS <- new('dimRedMethodPars', pars = list(center = TRUE,  scale = TRUE))
    irisParsC  <- new('dimRedMethodPars', pars = list(center = TRUE,  scale = FALSE))
    irisParsS  <- new('dimRedMethodPars', pars = list(center = FALSE, scale = TRUE))
    irisPars   <- new('dimRedMethodPars', pars = list(center = FALSE, scale = FALSE))
    expect_equal(class(irisParsCS)[1], 'dimRedMethodPars')
    expect_equal(class(irisParsC)[1],  'dimRedMethodPars')
    expect_equal(class(irisParsS)[1],  'dimRedMethodPars')
    expect_equal(class(irisPars)[1],   'dimRedMethodPars')

    irisDRcs <- new('dimRed', data = irisData, method = pca, pars = irisParsCS)
    irisDRs  <- new('dimRed', data = irisData, method = pca, pars = irisParsS)
    irisDRc  <- new('dimRed', data = irisData, method = pca, pars = irisParsC)
    irisDR   <- new('dimRed', data = irisData, method = pca, pars = irisPars)
    expect_equal(class(irisDRcs)[1], 'dimRed')
    expect_equal(class(irisDRs)[1],  'dimRed')
    expect_equal(class(irisDRc)[1],  'dimRed')
    expect_equal(class(irisDR)[1],   'dimRed')
    
    irisResCS <- fit(irisDRcs)
    irisResS  <- fit(irisDRs)
    irisResC  <- fit(irisDRc)
    irisRes   <- fit(irisDR)
    expect_equal(class(irisResCS)[1], "dimRedResult")
    expect_equal(class(irisResS)[1],  "dimRedResult")
    expect_equal(class(irisResC)[1],  "dimRedResult")
    expect_equal(class(irisRes)[1],   "dimRedResult")

    expect_equal(irisResCS@apply(irisData), irisResCS@data)
    expect_equal(irisResS@apply(irisData),  irisResS@data)
    expect_equal(irisResC@apply(irisData),  irisResC@data)
    expect_equal(irisRes@apply(irisData),   irisRes@data)

    expect_equal(irisResCS@inverse(irisResCS@data), irisData)
    expect_equal(irisResS@inverse(irisResS@data),   irisData)
    expect_equal(irisResC@inverse(irisResC@data),   irisData)
    expect_equal(irisRes@inverse(irisRes@data),     irisData)
})

