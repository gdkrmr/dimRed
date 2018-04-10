
data(iris)
context("kPCA")

test_that("general data conversions", {

    irisData <- loadDataSet("Iris")
    expect_equal(class(irisData)[1], "dimRedData")

    irisPars <- list()
    irisPars[[length(irisPars) + 1]] <-
        list(kernel = "rbfdot",
             kpar = list(sigma = 0.1))
    irisPars[[length(irisPars) + 1]] <-
        list(kernel = "rbfdot",
             kpar = list(sigma = 1))
    irisPars[[length(irisPars) + 1]] <-
        list(kernel = "polydot",
             kpar = list(degree = 3))
    irisPars[[length(irisPars) + 1]] <-
        list(kernel = "vanilladot",
             kpar = list())
    irisPars[[length(irisPars) + 1]] <-
        list(kernel = "laplacedot",
             kpar = list(sigma = 1))
    irisPars[[length(irisPars) + 1]] <-
        list(kernel = "laplacedot",
             kpar = list(sigma = 0.1))
    irisPars[[length(irisPars) + 1]] <-
        list(kernel = "besseldot",
             kpar = list(sigma = 0.1,
                         order = 1,
                         degree = 1))
    irisPars[[length(irisPars) + 1]] <-
        list(kernel = "besseldot",
             kpar = list(sigma = 1,
                         order = 2,
                         degree = 3))
    irisPars[[length(irisPars) + 1]] <-
        list(kernel = "splinedot",
             kpar = list())

    irisRes <- lapply(irisPars, function(x)
        do.call(
          function(...) tryCatch(embed(.data = irisData,
                                       .method = "kPCA", ...),
                                   error = function(e) as.character(e)),
            x
        ) )

    for (i in 1:length(irisRes)) {
        if (inherits(irisRes[[i]], "character")){
            expect(grepl("singular", irisRes[[i]]),
                   "singular")
        } else {
            expect(inherits(irisRes[[i]], "dimRedResult"),
                   'should be of class "dimRedResult"')
        }
    }

    ## This test fails with multithreaded blas
    ## for (i in 1:length(irisRes)){
    ##     if (inherits(irisRes[[i]], "dimRedResult")){
    ##       expect_equal(irisRes[[i]]@apply(irisData)@data[, 1:2],
    ##                    irisRes[[i]]@data@data)

    ##       expect_equal(2, getNDim(irisRes[[i]]))

    ##         ## the reverse is an approximate:

    ##       expect_less_than(
    ##           max(
    ##               irisRes[[i]]@inverse(irisRes[[i]]@data)@data - irisData@data
    ##           ), 300,
    ##           ## paste0("inverse of kpca is an approximate, ",
    ##           ##        "so this may fail due to numerical inaccuracy")
    ##         )
    ##     }
    ## }

    ## This one cannot calculate an inverse:
    kpca.fit <- embed(loadDataSet("3D S", n = 200),
                      "kPCA", kernel = "splinedot", kpar = list())
    expect( is.na(kpca.fit@inverse(1)), "The inverse should return NA" )
})
