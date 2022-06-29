skip_if_no_NMF <- function() {
  if (!requireNamespace("NMF", quietly = TRUE) &&
      Sys.getenv("BNET_FORCE_NNMF_TESTS") != "1")
    skip("NMF not available for testing")
}

## if we don't load the library explicitly, the predict function does not work
## (sometimes...).
## library(NMF)

ints_trn <- matrix(seq(0, 98, by = 2), ncol = 5)
input_trn <- dimRedData(as.data.frame(ints_trn))
input_tst <- dimRedData(ints_trn[1:3,] + 1)

test_that("2D projection", {
  skip_if_no_NMF()

  dim_2_defaults <- embed(input_trn, "NNMF", seed = 13, nrun = 1)

  expect_equal(dim_2_defaults@method, "NNMF")

  ## Expected results from
  ## tmp <- NMF::nmf(t(ints_trn), rank = 2, nrun = 1, seed = 13)
  ## coefs <- basis(tmp)
  ## rownames(coefs) <- paste0("V", 1:5)
  ## colnames(coefs) <- paste0("NNMF", 1:2)
  ## coefs
  ## dput(coefs)

  dim_2_coef <- structure(
    c(18.807241710186, 30.2191667888959,
      32.1069052462692, 9.53490906878683,
      164.109205703974, 0.00064246562138093,
      24.3924277525021, 56.4301459918642,
      108.103923297376, 17.566220349863),
    .Dim = c(5L, 2L),
    .Dimnames = list(c("V1", "V2", "V3", "V4", "V5"),
                     c("NNMF1", "NNMF2")))

  expect_equal(dim_2_defaults@other.data$w, dim_2_coef, ignore_attr = TRUE)

  dim_2_apply <- dim_2_defaults@apply(input_tst)@data
  dim_2_pred <- predict(dim_2_defaults, input_tst)@data

  ## Expected results from
  ## t(solve(crossprod(basis(tmp)), t(input_tst@data %*% basis(tmp))))
  ## preds <- getData(input_tst) %*% t(MASS::ginv(basis(tmp)))
  ## getData(getDimRedData(dim_2_defaults))
  ## colnames(preds) <- paste0("NNMF", 1:2)
  ## dput(preds)

  dim_2_exp <- structure(
    c(0.427476458116875, 0.440237021147746, 0.452997584178617,
      0.512256378881175, 0.5332094651398, 0.554162551398426),
    .Dim = c(3L, 2L),
    .Dimnames = list(NULL, c("NNMF1", "NNMF2"))
  )

  expect_equal(dim_2_apply, dim_2_exp, tolerance = 0.01, ignore_attr = TRUE)
  expect_equal(dim_2_pred,  dim_2_exp, tolerance = 0.01, ignore_attr = TRUE)
})

test_that("other arguments", {
  skip_if_no_NMF()

  dim_3_args <- embed(input_trn, "NNMF", seed = 13, nrun = 10,
                      ndim = 3, method = "KL",
                      options = list(.pbackend = NULL))

  ## Expected results from
  ## tmp <- NMF::nmf(t(ints_trn), rank = 3, nrun = 10, seed = 13,
  ##                 method = "KL", .pbackend = NULL)
  ## coefs <- t(NMF::coef(tmp))
  ## colnames(coefs) <- paste0("NNMF", 1:ncol(coefs))
  ## coefs
  ## dput(coefs)
  ## rot <- NMF::basis(tmp)
  ## rownames(rot) <- paste0("V", 1:nrow(rot))
  ## dput(rot)

  dim_3_rot <- structure(
    c(11.624951277152, 31.2554213278975, 50.8858913786408,
      70.5163614293837, 90.1468314801264, 2.22044604925031e-16,
      36.4357899711133, 72.8715799422292, 109.307369913346,
      145.743159884462, 22.4019808842378, 42.1081005773292,
      61.8142202704197, 81.52033996351, 101.2264596566),
    .Dim = c(5L, 3L),
    .Dimnames = list(c("V1", "V2", "V3", "V4", "V5"), NULL)
  )
  dim_3_pred <- structure(
    c(2.22044604925031e-16, 0.0731742704517501, 0.194863499580201,
      0.50224638618713, 0.557517908619563, 0.197219538171418,
      0.0860784848917408, 0.159094934700865, 0.10366866301249,
      0.216483929440989, 0.54891083782883, 0.481738298195276, 0.40204352636632,
      0.274419226004639, 0.211867578024856, 0.256578985276104,
      0.236980211423017, 0.16984840699324, 0.135869049278152,
      0.0584647425861749, 2.22044604925031e-16, 0.0513058500137363,
      0.0774360678481537, 0.00720517673339281, 0.0678012129377125,
      0.344046917890136, 0.49099862480747, 0.542386371921862, 0.660426277478513,
      0.691161417731563),
    .Dim = c(10L, 3L),
    .Dimnames = list(NULL, c("NNMF1", "NNMF2", "NNMF3"))
  )

  expect_equal(dim_3_args@other.data$w, dim_3_rot, ignore_attr = TRUE)
  expect_equal(getData(getDimRedData(dim_3_args)), dim_3_pred, ignore_attr = TRUE)

  dim_3_apply <- dim_3_args@apply(input_tst)@data
  dim_3_pred <- predict(dim_3_args, input_tst)@data

  ## Expected results from
  ## crossprod(basis(tmp)) does not have full rank!!! This needs to be considered
  ## w <- getOtherData(dim_3_args)$w
  ## preds <- t(solve(crossprod(w), t(input_trn@data %*% w)))
  ## preds <- t(qr.solve(crossprod(w), t(input_trn@data %*% w)))
  ## preds <- getData(input_tst) %*% t(MASS::ginv(w))
  ## preds
  ## dput(preds)
  ## getData(getDimRedData(dim_3_args))
  ## preds - getData(getDimRedData(dim_3_args))
  ## input_trn@data
  ## input_tst@data %*% basis(tmp)
  ## colnames(preds) <- paste0("NNMF", 1:3)

  dim_3_exp <- structure(
    c(0.118730450278164, 0.144080695556738, 0.169430940835312,
      0.494122495652466, 0.439293850852014, 0.384465206051563,
      -0.0169733070286198, 0.0591496323928872, 0.135272571814394),
    .Dim = c(3L, 3L)
  )

  expect_equal(dim_3_apply, dim_3_exp, tolerance = 0.01, ignore_attr = TRUE)
  expect_equal(dim_3_pred,  dim_3_exp, tolerance = 0.01, ignore_attr = TRUE)
})


test_that("Bad args", {
  skip_if_no_NMF()

  expect_error(embed(iris, "NNMF"))
  expect_error(embed(iris[, 1], "NNMF"),
               "`ndim` should be less than the number of columns")
  expect_error(embed(iris[1:4], "NNMF", method = c("a", "b")),
               "only supply one `method`")
  expect_error(embed(scale(iris[1:4]), "NNMF"), "negative entries")

})


test_that("Full_rank", {
  skip_if_no_NMF()

  dim_2_full_rank_example <- embed(input_trn, "NNMF", ndim = ncol(input_trn@data))
  dim_2_recon  <- inverse(dim_2_full_rank_example, dim_2_full_rank_example@data@data)

  expect_equal(dim_2_recon, input_trn, ignore_attr = TRUE, tolerance = 1e-2)
})
