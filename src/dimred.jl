using MultivariateStats

function sufficient_dimensions{T}(xout::AbstractArray{T}, xin::AbstractArray{T}, expl_var::Float64 = 0.95)
    npoint, nvar = size(xin)
    for i in 1:nvar
        _sum  = _ssum = zero(eltype(xin))
        for j in 1:npoint
            _sum  += xin[j,i]
            _ssum += xin[j,i]^2
        end
        _mean  = _sum / npoint
        _smean = _ssum / npoint
        _std = sqrt(_smean - _mean^2)
        if _std == 0
            _std = 1.0
            warn("zero variance in column $i")
        end
        for j in 1:npoint
            xin[j,i] -= _mean
            xin[j,i] /= _std
        end
    end
    pca = fit(PCA, xin, pratio = 0.999, method = :svd)
    xout[1]  =
        findfirst(cumsum(principalvars(pca)) / tprincipalvar(pca) .> expl_var)
    xout[1]
end

data = [x^2 + randn() for x in collect(1:500)/50, y in 1:15]
data[:,15] = 1
res = collect(1.0)
a,b = size(data)
@code_native sufficient_dimensions(res, data)
@time sufficient_dimensions(res, data)




function globalPCA{tp, N}(datacube::AbstractArray{tp, N}, expl_var::Float64 = 0.95)
    if(expl_var > 1.0 || expl_var < 0.0) print("Stop: expl_var has to be within range(0.0, 1.0) but is $expl_var")
    else
    newsize = zeros(Int64, length(size(datacube)))
    dims = 1
    for i = 1:(N-1) # multiply dimensions except the last one and save as dims
        dims = size(datacube, i) * dims
        newsize[i] = size(datacube, i)
    end
    X = reshape(datacube, dims, size(datacube, N)) # reshape according to dims
    X = permutedims(X, [2,1]) # permute as PCA expectes other input format
    M = fit(PCA, X, pratio = 0.999, method = :svd)
    Y = transform(M, X)
    Y = permutedims(Y, [2,1])
    num_expl_comp = findfirst(cumsum(principalvars(M))/tprincipalvar(M) .>= expl_var)
    Y = Y[:,1:num_expl_comp]
    newsize[N] = num_expl_comp
    Xout = similar(datacube)
    Xout = Xout[:,:,:,1:num_expl_comp]
    Xout[:] = Y[:]
    Xout
    end
    return(Xout, principalvars(M)/tprincipalvar(M), cumsum(principalvars(M))/tprincipalvar(M), num_expl_comp)
end
