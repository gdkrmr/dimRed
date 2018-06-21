
context("NNMF")

ints_trn <- matrix(seq(0, 98, by = 2), ncol = 5)
input_trn <- dimRedData(as.data.frame(ints_trn))
input_tst <- dimRedData(ints_trn[1:3,] + 1)

test_that("2D projection", {

  dim_2_defaults <- embed(input_trn, "NNMF", seed = 13, nrun = 10)

  # Expected results from
  ## tmp <- NMF::nmf(t(ints_trn), rank = 2, nrun = 10, seed = 13)
  # coefs <- t(basis(tmp))
  # colnames(coefs) <- paste0("V", 1:5)

  dim_2_coef <- structure(
    c(
      24.8398963516693,
      2.22044604925031e-16,
      35.5902150792044,
      29.2373463142194,
      46.3405338067393,
      58.4746926284389,
      57.0908525342742,
      87.7120389426585,
      67.8411712618091,
      116.949385256878
    ),
    .Dim = c(2L, 5L),
    .Dimnames = list(NULL, c("V1", "V2", "V3", "V4", "V5"))
  )

  expect_equivalent(dim_2_defaults@other.data$w, t(dim_2_coef))

  dim_2_apply <- dim_2_defaults@apply(input_tst)@data
  dim_2_pred <- predict(dim_2_defaults, input_tst)@data

  # Expected results from
  ## dput(solve(crossprod(basis(tmp)), t(input_tst@data %*% basis(tmp))))
  # preds <- input_tst@data %*% basis(tmp)
  # colnames(preds) <- paste0("NNMF", 1:2)

  dim_2_exp <- t(structure(
    c(0.0402578169346043, 0.669254159607562, 0.120773450803807,
      0.639649259171627, 0.201289084673012, 0.610044358735691),
    .Dim = 2:3,
    .Dimnames = list(c("NNMF1", "NNMF2"), NULL)
  ))

  ## dim_2_exp <-
  ##   structure(
  ##     c(11649.8731758885, 12113.2785139559, 12576.6838520233,
  ##       17834.7812516739, 18419.5281779583, 19004.2751042427),
  ##     .Dim = 3:2,
  ##     .Dimnames = list(NULL, c("NNMF1", "NNMF2"))
  ##   )

  expect_equivalent(dim_2_apply, dim_2_exp, tolerance = 0.01)
  expect_equivalent(dim_2_pred,  dim_2_exp, tolerance = 0.01)
})

test_that("other arguments", {

  dim_3_args <- embed(input_trn, "NNMF", seed = 13, nrun = 10,
                      ndim = 3, method = "KL",
                      options = list(.pbackend = NULL))

  # Expected results from
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
      70.5163614293837, 90.1468314801264, 2.22044604925031e-16, 36.4357899711133,
      72.8715799422292, 109.307369913346, 145.743159884462, 22.4019808842378,
      42.1081005773292, 61.8142202704197, 81.52033996351, 101.2264596566),
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

  expect_equivalent(dim_3_args@other.data$w, dim_3_rot)
  expect_equivalent(getData(getDimRedData(dim_3_args)), dim_3_pred)

  dim_3_apply <- dim_3_args@apply(input_tst)@data
  dim_3_pred <- predict(dim_3_args, input_tst)@data

  # Expected results from
  ## crossprod(basis(tmp)) does not have full rank!!! This needs to be considered
  preds <- t(solve(t(basis(tmp)) %*% basis(tmp), t(input_trn@data %*% basis(tmp))))
  # input_tst@data %*% basis(tmp)
  # colnames(preds) <- paste0("NNMF", 1:3)

  dim_3_exp <-
    structure(
      c(14357.7017427699, 14866.5606565563, 15375.4195703427,
        22225.8318823803, 22954.5476818026, 23683.2634812249,
        16613.1390940541, 17231.2812967583, 17849.4234994625),
      .Dim = c(3L, 3L),
      .Dimnames = list(NULL, c("NNMF1", "NNMF2", "NNMF3"))
    )

  expect_equivalent(dim_3_apply, dim_3_exp, tolerance = 0.01)
  expect_equivalent(dim_3_pred,  dim_3_exp, tolerance = 0.01)
})


test_that("Bad args", {

  expect_error(embed(iris, "NNMF"))
  expect_error(embed(iris[, 1], "NNMF"),
               "`ndim` should be less than the number of columns")
  expect_error(embed(iris[1:4], "NNMF", method = c("a", "b")),
               "only supply one `method`")
  expect_error(embed(scale(iris[1:4]), "NNMF"), "negative entries")

})
