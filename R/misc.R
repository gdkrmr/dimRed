if(!isClassUnion('missingORnumeric'))   setClassUnion('missingORnumeric',   c('numeric', 'missing'))
if(!isClassUnion('missingORcharacter')) setClassUnion('missingORcharacter', c('character', 'missing'))
if(!isClassUnion('missingORlogical'))   setClassUnion('missingORlogical',   c('logical', 'missing'))
if(!isClassUnion('missingORfunction'))  setClassUnion('missingORfunction',  c('function', 'missing'))

# Squared euclidean distance between points in A and B
# taken from http://blog.felixriedel.com/2013/05/pairwise-distances-in-r/ 

pdist2 <- function (A, B) {
    an = rowSums(A^2) # apply(A, 1, function(rvec) crossprod(rvec,rvec))
    bn = rowSums(B^2) # apply(B, 1, function(rvec) crossprod(rvec,rvec))
 
    m = nrow(A)
    n = nrow(B)
 
    matrix(rep(an, n), nrow=m) +
        matrix(rep(bn, m), nrow=m, byrow=TRUE) -
        2 * tcrossprod(A,B)
}


## a + b ~ c + d
## becomes
## ~ c + d + 0
rhs <- function (formula) {
    fs <- as.character(formula)[3]
    stats::as.formula(paste("~", fs, "+ 0"))
}

## a + b ~ c + d
## becomes
## ~ a + b + 0
lhs <- function (formula) {
    fs <- as.character(formula)[2]
    stats::as.formula(paste("~", fs, "+ 0"))
}

## check if a package is installed
chckpkg <- function (pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(paste0("require '", pkg,
                    "' package, install it using install.packages('",
                    pkg, "')"))
    }
}
