

testdat <- loadDataSet("3D S Curve", n = 20)
testdat2 <- loadDataSet("3D S Curve", n = 120)
context("isomap")

library(microbenchmark)

bm <- microbenchmark(
    i1 <- isomap2@fun(testdat, pars = list(knn = 7, ndim = 2))
   ,
    i2 <- isomap2@fun(testdat, pars = list(knn = 50, ndim = 2, eps = 0.1)),
    i3 <- isomap2@fun(testdat, pars = list(knn = 50, ndim = 2, eps = 0.5)),
    i4 <- isomap2@fun(testdat, pars = list(knn = 50, ndim = 2, eps = 2.0)),
    times = 1L
)

plot(
t(t(0.5 * cmdout$points / cmdout$eig[seq_len(pars$ndim)]) %*% t((mean(geodist^2) - (lgeodist^2))))
)

0.5 * cmdout$points / cmdout$eig[seq_len(pars$ndim)] 

    (mean(geodist^2) - (lgeodist^2)) %*% (0.5 * cmdout$points / cmdout$eig[seq_len(pars$ndim)])

print(bm)

testdat <- loadDataSet("3D S Curve", n = 200)
testdat2 <- loadDataSet("3D S Curve", n = 1200)

i1 <- isomap2@fun(testdat, pars = list(knn = 7, ndim = 2))
i1.app <- i1@apply(testdat@data[1,, drop =F])
i1.app.2 <- i1@apply(testdat2)

i2 <- isomap2@fun(testdat2, pars = list(knn = 20, ndim = 2))

i1@data@data[1,]
i1.app@data[1,]

plot(testdat, type = "3vars")
plot(i1, type = "2vars")
plot(i1.app.2, type = "2vars")
points(i1@data@data, add = TRUE)
points(x = i1.app.2@data[,1],
       y = i1.app.2@data[,2],
       add = TRUE)
points(i1.app.2@data, add = TRUE)
plot(pca@fun(i1.app.2), type = "2vars")

plot(i1)
plot(i2, type = "2vars")
plot(i3)
plot(i4)


-0.5 * (t(Lsharp) %*% t(dammu))

plot(-0.5 * (dammu %*% Lsharp[dim(Lsharp)[1]:1,2:1]))
plot(-0.5 * (dammu %*% cmdout$points[20:1,2:1]))
out <- -0.5 * (dammu %*% Lsharp[20:1,2:1, drop = F])

plot(i1, type = "2vars")
points(out, add = TRUE)

plot(out)
plot(dammu)
testdat3 <- loadDataSet("3D S Curve", n = 5)

RANN::nn2(matrix3366), matrix(1:2))

RANN::nn2(matrix(1:4, nrow = 4), searchtype = "standard", eps =20)
