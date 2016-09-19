using RCall
R"library(dimRed)"

"""
Fill xout with cumulative sum of the quality criterium for
different dimensions of the method appied on xin
"""
function wrapper!(xout::AbstractArray, xin::AbstractArray, method::AbstractString, quality::AbstractString)
    dmax = size(xin, 2)
    @rput xin
    @rput method
    @rput quality
    @rput dmax
    R"""
    res <- numeric(dmax)
    in.drdata <- as(xin, 'dimRedData')
    # cat("class in.drdata: ",    class(in.drdata), "\n")
    # cat("class res: ",          class(res),       "\n")
    # cat("length res: ",         length(res),      "\n")
    # cat("class xin: ",          class(xin),       "\n")
    # cat("class method: ",       class(method),    "\n")
    # cat("class dmax: ",         class(dmax),      "\n")
    # cat("class quality: ",      class(quality),   "\n")
    out.drresult <- embed(in.drdata, method, ndim = dmax)
    # cat("class out.drresult: ", class(out.drresult), "\n")
    # str(out.drresult)
    for(i in seq_len(dmax)){
        tmp.out.drresult <- out.drresult
        tmp.out.drresult@data@data <- tmp.out.drresult@data@data[,seq_len(i), drop = FALSE]
        res[i] <- quality(tmp.out.drresult, quality)
    }
    """
    @rget res
    println(typeof(res))
    xout[:] = res
    return xout
end




#=
# test

data = [x^2 + randn() for x in collect(1:500)/50, y in 1:15]
xpca = Array{Float64}(15)
xdrr = Array{Float64}(15)
xiso = Array{Float64}(15)
xlle = Array{Float64}(15)
xkpca = Array{Float64}(15)
@time wrapper!(xpca, data, "pca",    "Q_local")
@time wrapper!(xdrr, data, "drr",    "Q_local")
@time wrapper!(xiso, data, "isomap", "Q_local")
@time wrapper!(xlle, data, "lle",    "Q_local")
@time wrapper!(xkpca, data, "kpca",  "Q_local")
xpca
xdrr
xiso
xlle
xkpca

@time wrapper!(xpca, data, "pca",    "coph")
@time wrapper!(xdrr, data, "drr",    "coph")
@time wrapper!(xiso, data, "isomap", "coph")
@time wrapper!(xlle, data, "lle",    "coph")
@time wrapper!(xkpca, data, "kpca",  "coph")


@rput xpca
@rput xdrr
@rput xiso
@rput xlle
@rput xkpca

R"plotdata <- cbind(xpca, xdrr, xiso, xlle, xkpca)"
R"matplot(plotdata, type = 'l')"
R"""
legend(x = 'topleft', legend = colnames(plotdata),
         col = seq_len(ncol(plotdata)),
         lty = 1)
"""


# some stuff if you want to test stuff
# check dimensionality reduction methods:
R"dimRedMethodList()"
R"dimRedQualityList()"
R"kpca@stdpars"

=#
