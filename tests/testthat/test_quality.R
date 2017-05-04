

context("quality")

test_that("quality", {

    irisData <- loadDataSet("Iris")

    parsPCA <- list(center = TRUE, scale. = TRUE)
    resPCA <- do.call(function(...) embed(irisData, "PCA", ...), parsPCA)

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

test_that("rmse_by_ndim", {

  ir <- loadDataSet("Iris")
  ir.drr <- embed(ir, "DRR", ndim = ndims(ir))
  ir.pca <- embed(ir, "PCA", ndim = ndims(ir))

  rmse_res <- data.frame(
    drr = reconstruction_error(ir.drr),
    pca = reconstruction_error(ir.pca)
  )

  for (i in 1:length(rmse_res$pca)) {
    expect_true(rmse_res$pca[i] - rmse_res$drr[i] + 1e-12 > 0, info = paste0(
                                                           "ndim = ", i,
                                                           ", rmse pca = ", rmse_res$pca[i],
                                                           ", rmse drr = ", rmse_res$drr[i]
                                                         ))
  }
  # expect_true(all((rmse_res$pca - rmse_res$drr) + 1e-12 > 0))

  expect_error(reconstruction_error(ir.pca, 5))
  expect_error(reconstruction_error(ir.pca, 0))
})
