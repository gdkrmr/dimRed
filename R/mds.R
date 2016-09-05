

#' Metric dimensional scaling
#'
#' Fit a metric dimensional scaling
#'
#' for details see \code{\link[stats]{cmdscale}}
#'
#' @examples
#' dat <- loadDataSet("3D S Curve")
#' emb <- mds@fun(dat)
#'
#' plot(emb@data@data)
#'
#' @include dimRedResult-class.R
#' @include dimRedMethod-class.R
#' 
#' @export
mds <- new('dimRedMethod',
           stdpars = list(d = dist, ndim = 2),
           fun = function (data, pars,
                           keep.org.data = TRUE) {
    ##
    meta <- data@meta
    orgdata <- if (keep.org.data) data@data else NULL
    indata <- data@data

    ## there are only efficient implementations for euclidean
    ## distances: extra efficient implementations for euclidean
    ## distances are possible, D is quared several times, it would be
    ## much faster to compute the squared distance right away.
    has.apply <- identical(all.equal(pars$d, dist), TRUE) # == TRUE
                                                          # necessary,
                                                          # because
                                                          # all.equal
                                                          # returns
                                                          # TRUE or an
                                                          # error
                                                          # string!!!!
   
    D <- as.matrix(pars$d(indata))
    if (has.apply) mD2 <- mean(D^2)
    
    ## cmdscale square the matrix internally
    res <- stats::cmdscale(D, k = pars$ndim)
    outdata <- res
               
    D <- NULL  # Untested: remove that from environment before creating
               # appl function, else it will stay in its environment
               # forever

    appl <- if(!has.apply) function(x) NA else function(x) {
        browser()
        appl.meta <- if(inherits(x, 'dimRedData')) x@meta else data.frame() 
        proj <- if(inherits(x, 'dimRedData')) x@data else x

        ## double center new data with respect to old: TODO: optimize
        ## this method, according to the de Silva, Tenenbaum(2004)
        ## paper. Need an efficient method to calculate the distance
        ## matrices between different point sets and arbitrary
        ## distances.
        Kab <- as.matrix(pars$d(proj)^2)
        Exa <- colMeans(pdist2(indata, proj))
        Kab <- sweep(Kab, 1, Exa)       #, '-')
        Kab <- sweep(Kab, 2, Exa)       #, '-')
        Kab <- -0.5 * (Kab + mD2)

        ## Eigenvalue decomposition
        tmp  <- eigen(Kab, symmetric = TRUE)
        ev   <- tmp$values[seq_len(pars$ndim)]
        evec <- tmp$vectors[, seq_len(pars$ndim), drop = FALSE]
        
        k1 <- sum(ev > 0)
        if (k1 < pars$ndim) {
            warning(gettextf("only %d of the first %d eigenvalues are > 0", 
                             k1, k), domain = NA)
            evec <- evec[, ev > 0, drop = FALSE]
            ev <- ev[ev > 0]
        }
        points <- evec * rep(sqrt(ev), each = nrow(proj))
        dimnames(points) <- list(NULL, paste0("MDS", seq_len(ncol(points))))
            
        new('dimRedData', data = points, meta = appl.meta)
    }
    
    

    colnames(outdata) <- paste0("MDS", seq_len(ncol(outdata)))

    return(new(
        'dimRedResult',
        data         = new('dimRedData',
                           data = outdata,
                           meta = meta),
        org.data     = orgdata,
        apply        = appl,
        has.org.data = keep.org.data,
        has.apply    = has.apply,
        method       = "mds",
        pars         = pars
    ))
})
