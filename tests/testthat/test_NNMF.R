
context("NNMF")

ints_trn <- matrix(seq(0, 98, by = 2), ncol = 5)
input_trn <- dimRedData(as.data.frame(ints_trn))
input_tst <- dimRedData(ints_trn[1:3,] + 1)

test_that("2D projection", {

  set.seed(6569)
  dim_2_defaults <- embed(input_trn, "NNMF")

  # Expected results from
  # set.seed(6569)
  # tmp <- NNLM::nnmf(ints_trn, k = 2, verbose = 0)

  dim_2_W <-
    structure(
      c(
        2.42572749239833e-07,
        842.09976067527,
        1684.1995209438,
        2526.2992812106,
        3368.39904147689,
        4210.49880174279,
        5052.59856200863,
        5894.69832225697,
        6736.79808251701,
        7578.89784277869,
        9214.12987669352,
        8801.71261456881,
        8389.29535258438,
        7976.87809060143,
        7564.46082861892,
        7152.04356663674,
        6739.62630465461,
        6327.20904268744,
        5914.79178071026,
        5502.37451873169
      ),
      .Dim = c(10L, 2L)
    )
  dim_2_H <-
  structure(
    c(
      0.00237501551979808,
      0,
      0.00343805393051527,
      0.00217057934569909,
      0.00450109234110581,
      0.00434115869149624,
      0.00556413075169635,
      0.00651173803729339,
      0.00662716916228689,
      0.00868231738309053
    ),
    .Dim = c(2L, 5L)
  )

  expect_equivalent(dim_2_defaults@other.data$H, dim_2_H)

  set.seed(5197)
  dim_2_proj <- dim_2_defaults@apply(input_tst)

  # Expected results from
  # set.seed(9770)
  # predict(tmp, ints_trn[1:3,] + 1, which = "W")$coefficients

  dim_2_pred <-
    structure(
      c(
        421.049880386134,
        1263.14964068643,
        2105.24940098787,
        9007.92124569017,
        8595.503983678,
        8183.08672166484
      ),
      .Dim = c(3L,
               2L)
    )
  expect_equivalent(dim_2_proj@data, dim_2_pred, tolerance = 0.01)
  expect_equivalent(getData(getDimRedData(dim_2_defaults)), dim_2_W,
                    tolerance = 0.01)

  expect_equivalent(inverse(dim_2_defaults, dim_2_W), input_trn)
  expect_equivalent(inverse(dim_2_defaults, dimRedData(dim_2_pred)), input_tst)

})


test_that("3D projection via KL div", {

  set.seed(2106)
  dim_3_defaults <- embed(input_trn, "NNMF",
                          ndim = 3, loss = "mkl",
                          rel.tol = 1e-10)

  # Expected results from
  # set.seed(2106)
  # tmp <- NNLM::nnmf(ints_trn, k = 3, loss = "mkl", verbose = 0, rel.tol = 1e-10)

  dim_3_W <-
    structure(
      c(
        5.18063005099169,
        3.61928482599978,
        3.23874387453718,
        3.79740487024406,
        3.69872376193331,
        2.99237501197329,
        1.95823119713848,
        2.25481853296027,
        0.62561468275278,
        1.17359023793198,
        4.45166623232659,
        6.59192039569594,
        6.77618627417522,
        5.40467525804366,
        5.12204364677915,
        5.84600628218366,
        7.11295755771834,
        6.1755684532383,
        8.42822962506597,
        7.07441891452535,
        0,
        1.48872378516316,
        2.97744757032668,
        4.46617135549625,
        5.95489514066389,
        7.44361892582682,
        8.93234271098553,
        10.4210664961581,
        11.909790281308,
        13.3985140664849
      ),
      .Dim = c(10L, 3L)
    )

  dim_3_H <-
    structure(
      c(
        0,
        0,
        1.34343255607936,
        2.54192843793978,
        1.53452859904805,
        1.80325365438537,
        5.083856875899,
        3.06905719807887,
        2.2630747526993,
        7.62578531385728,
        4.60358579711049,
        2.72289585101288,
        10.1677137518434,
        6.13811439611691,
        3.18271694933827
      ),
      .Dim = c(3L, 5L)
    )

  expect_equivalent(dim_3_defaults@other.data$H, dim_3_H)

  set.seed(2141)
  dim_3_proj <- dim_3_defaults@apply(input_tst)

  # Expected results from
  # set.seed(2141)
  # predict(tmp, ints_trn[1:3,] + 1, which = "W")$coefficients

  dim_3_pred <-
    structure(
      c(
        4.62380751248276,
        4.05189652465168,
        3.43947500335433,
        5.15098831939291,
        5.65225639510719,
        6.2206296894075,
        0.744361892582574,
        2.23308567774797,
        3.72180946290805
      ),
      .Dim = c(3L, 3L)
    )

  expect_equivalent(dim_3_proj@data, dim_3_pred, tolerance = 0.01)

  expect_equivalent(inverse(dim_3_defaults, dim_3_W), input_trn)
  expect_equivalent(inverse(dim_3_defaults, dimRedData(dim_3_pred)), input_tst)
})

test_that("Bad args", {

  expect_error(embed(iris, "NNMF"))
  expect_error(embed(iris[,1], "NNMF"))

})
