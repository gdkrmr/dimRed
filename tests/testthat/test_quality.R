

context("quality")

test_that('quality', {

    irisData <- loadDataSet("Iris")

    parsPCA <- list(center = TRUE, scale. = TRUE)
    resPCA <- do.call(function(...) embed(irisData, "pca", ...), parsPCA)

    suppressWarnings(
        resQual <- list(
            Q_local(resPCA),
            Q_global(resPCA),
            mean_R_NX(resPCA),
            total_correlation(resPCA),
            cophenetic_correlation(resPCA),
            distance_correlation(resPCA),
            reconstruction_rmse(resPCA)
        )
    )
    
    lapply(resQual, function(x) expect_true(is.numeric(x)))
})
