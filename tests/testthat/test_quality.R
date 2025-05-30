test_that("quality", {

    irisData <- loadDataSet("Iris")
    # remove duplicates
    irisData <- irisData[!duplicated(irisData@data), ]

    parsPCA <- list(center = TRUE, scale. = TRUE)
    resPCA <- do.call(function(...) embed(irisData, "PCA", ...), parsPCA)

    if(requireNamespace("coRanking", quietly = TRUE))
      expect_true(is.numeric(Q_local(resPCA)))
    if(requireNamespace("coRanking", quietly = TRUE))
      expect_true(is.numeric(Q_global(resPCA)))
    if(requireNamespace("coRanking", quietly = TRUE))
      expect_true(is.numeric(mean_R_NX(resPCA)))
    if(requireNamespace("optimx", quietly = TRUE))
      expect_true(is.numeric(total_correlation(resPCA)))
    expect_true(is.numeric(cophenetic_correlation(resPCA)))
    if (requireNamespace("energy", quietly = TRUE))
      expect_true(is.numeric(distance_correlation(resPCA)))
    expect_true(is.numeric(reconstruction_rmse(resPCA)))

    ## suppressWarnings(
    ##     resQual <- list(
    ##         Q_local(resPCA),
    ##         Q_global(resPCA),
    ##         mean_R_NX(resPCA),
    ##         total_correlation(resPCA),
    ##         cophenetic_correlation(resPCA),
    ##         distance_correlation(resPCA),
    ##         reconstruction_rmse(resPCA)
    ##     )
    ## )

    ## lapply(resQual, function(x) expect_true(is.numeric(x)))
})

test_that("Q_local ndim", {
  if(!requireNamespace("coRanking", quietly = TRUE)) {
    skip("coRanking not available")
  }

  irisData <- loadDataSet("Iris")
  irisData <- irisData[!duplicated(irisData@data)]

  parsPCA <- list(center = TRUE, scale. = FALSE, ndim = 4)
  resPCA <- do.call(function(...) embed(irisData, "PCA", ...), parsPCA)

  tmp <- sapply(1:4, function(x) quality(resPCA, "Q_local", ndim = x))
  expect_equal(rank(tmp), 1:4)
})

test_that("rmse_by_ndim", {
  if(!requireNamespace("DRR", quietly = TRUE)) {
    skip("DRR not available")
  }

  ir <- loadDataSet("Iris")

  set.seed(1)
  ir.drr <- embed(ir, "DRR", .mute = c("message", "output"), ndim = ndims(ir))
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

test_that("AUC_lnK_R_NX", {
  if(!requireNamespace("coRanking", quietly = TRUE)) {
    skip("coRanking not available")
  }

  irisData <- loadDataSet("Iris")
  irisData <- irisData[!duplicated(irisData@data)]

  parsPCA <- list(center = TRUE, scale. = TRUE, ndim = 4)
  resPCA <- do.call(function(...) embed(irisData, "PCA", ...), parsPCA)

  expect_true(length(AUC_lnK_R_NX(resPCA, weight = "inv"))   == 1)
  expect_true(length(AUC_lnK_R_NX(resPCA, weight = "log"))   == 1)
  expect_true(length(AUC_lnK_R_NX(resPCA, weight = "ln"))    == 1)
  expect_true(length(AUC_lnK_R_NX(resPCA, weight = "log10")) == 1)

  expect_true(AUC_lnK_R_NX(resPCA, weight = "log") == AUC_lnK_R_NX(resPCA, weight = "ln"))
  expect_error(AUC_lnK_R_NX(resPCA, weight = "asdf"))

})
