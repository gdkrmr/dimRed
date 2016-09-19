using ClusterBGC
#Adds processors provided by openlava
addprocs_cluster()
using CABLAB
CABLABdir("/Net/Groups/BGI/scratch/gkraemer/cablabresults/") #sets the working directory
println(CABLABdir())
CABLAB.DAT.init_DATworkers()  #initializes the workers


c     = Cube("/Net/Groups/BGI/scratch/fgans/cubecopy/datacube/")
vars  = ["GPP","Rg","t2m","fpar_fluxcom"];
#cdata = getCubeData(c,latitude=(36, 48), longitude=(6,18),variable=vars);
cdata = getCubeData(c,latitude=(50, 51), longitude=(30,32),variable=vars);

@time cube_filled = mapCube(gapFillMSC, cdata);
@time cubeanom    = mapCube(removeMSC,  cube_filled)


@everywhere using RCall
@everywhere begin
R"library(dimRed)"

"""
Fill xout with cumulative sum of the quality criterium for
different dimensions of the method appied on xin
"""
function wrapper!(xout::AbstractArray, xin::AbstractArray,
method::AbstractString, quality::AbstractString)
    dmax = size(xin, 2)
    @rput xin
    @rput method
    @rput quality
    @rput dmax
    if (mapreduce(isnan,+,0,xin)) > 0
        xout[:]=NaN
        return xout
    end
    R"""
    res <- rep(NaN,dmax)
    in.drdata <- as(xin, 'dimRedData')
    # cat("class in.drdata: ",    class(in.drdata), "\n")
    # cat("class res: ",          class(res),       "\n")
    # cat("length res: ",         length(res),      "\n")
    # cat("class xin: ",          class(xin),       "\n")
    # cat("class method: ",       class(method),    "\n")
    # cat("class dmax: ",         class(dmax),      "\n")
    # cat("class quality: ",      class(quality),   "\n")
    # print(dim(xin))
    out.drresult <- embed(in.drdata, method, ndim = dmax, scale. = TRUE)
    # cat("class out.drresult: ", class(out.drresult), "\n")
    # str(out.drresult)
    for(i in seq_len(dmax)){
        tmp.out.drresult <- out.drresult
        tmp.out.drresult@data@data <- tmp.out.drresult@data@data[,seq_len(i), drop = FALSE]
        try(res[i] <- quality(tmp.out.drresult, quality))
    }
    # print(res)
    """
    @rget res
    xout[:] = res
    return xout
end
end
registerDATFunction(wrapper!,(TimeAxis,VariableAxis),(VariableAxis,),inmissing=(:nan,),outmissing=:nan,no_ocean=1)

@time qualitypca=mapCube(wrapper!,cubeanom,"pca","mean_R_NX");
@time qualityiso=mapCube(wrapper!,cubeanom,"iso","mean_R_NX");

saveCube(qualityiso,"ISO")
saveCube(qualitypca,"PCA")
