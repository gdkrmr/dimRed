#! /bin/bash

R --slave << END_FILE

    library(rhub)
    archs <- rhub::platforms()[["docker-image"]]
    archs <- na.exclude(archs)
    sapply(
        archs,
        function(x) try(rhub::check(platform = x, show_status = FALSE))
    )

END_FILE
