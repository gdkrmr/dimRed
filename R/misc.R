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
