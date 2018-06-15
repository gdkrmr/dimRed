
context("NNMF")
library(NMF)

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
  # tmp <- NMF::nmf(t(ints_trn), rank = 3, nrun = 10, seed = 13,
  #                 method = "KL", .pbackend = NULL)
  # coefs <- t(basis(tmp))
  # colnames(coefs) <- paste0("V", 1:5)

  dim_2_coef <-
    structure(
      c(11.624951277152, 2.22044604925031e-16, 22.4019808842378,
        31.2554213278975, 36.4357899711133, 42.1081005773292, 50.8858913786408,
        72.8715799422292, 61.8142202704197, 70.5163614293837, 109.307369913346,
        81.52033996351, 90.1468314801264, 145.743159884462, 101.2264596566
      ),
      .Dim = c(3L, 5L),
      .Dimnames = list(NULL, c("V1", "V2", "V3", "V4", "V5"))
    )

  expect_equivalent(dim_3_args@other.data$proj, dim_2_coef)

  dim_3_apply <- dim_3_args@apply(input_tst)@data
  dim_3_pred <- predict(dim_3_args, input_tst)@data

  # Expected results from
  # preds <- input_tst@data %*% basis(tmp)
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
  expect_error(embed(iris[,1], "NNMF"))
})
