

context("quality")

test_that('quality', {

    irisData <- loadDataSet("Iris")

    parsPCA <- new('dimRedMethodPars', pars = list(center = TRUE, scale = TRUE))
    drPCA <- new('dimRed', data = irisData, method = pca, pars = parsPCA)
    resPCA <- fit(drPCA)

    resQual <- list(
        Q_local(resPCA),
        Q_global(resPCA),
        mean_R_NX(resPCA),
        total_correlation(resPCA),
        cophenetic_correlation(resPCA),
        distance_correlation(resPCA),
        reconstruction_rmse(resPCA)
    )
    lapply(resQual, function(x) expect_true(is.numeric(x)))
})
