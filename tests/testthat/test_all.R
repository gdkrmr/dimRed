

context("high level functions")


test_that("high level functions working?", {
    embed_methods <- dimRedMethodList()
    quality_methods <- dimRedQualityList()
    scurve <- loadDataSet("3D S Curve", n = 500)

    quality_results <- matrix(NA, length(embed_methods),
                              length(quality_methods),
                              dimnames = list(embed_methods, quality_methods))
    embedded_data <- list()

    for (e in embed_methods) {
        message("embedding: ", e)
        suppressWarnings(
          embedded_data[[e]] <- embed(scurve, e,
                                      .mute = c("message", "output")))
        for (q in quality_methods) {
            message("  quality: ", q)
            quality_results[e, q] <- tryCatch(
                suppressWarnings(quality(embedded_data[[e]], q,
                                         .mute = c("message", "output"))),
                error = function (e) NA
            )
        }
    }

    expect(inherits(quality_results, "matrix"), "should be matrix")
    expect(storage.mode(quality_results) == "double",
           'storage should be "double"')
})
