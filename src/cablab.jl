using RCall
using CABLAB

c     = Cube("/Net/Groups/BGI/scratch/fgans/cubecopy/datacube/")
vars  = ["GPP","Rg","t2m","fpar_fluxcom"];
cdata = getCubeData(c,latitude=(36, 48), longitude=(6,18),variable=vars);


cube_filled = mapCube(gapFillMSC, cdata);
cubeanom=mapCube(removeMSC,cube_filled)

@time qualitypca=mapCube(wrapper!,cubeanom,"pca","mean_R_NX");
@time qualityiso=mapCube(wrapper!,cubeanom,"iso","mean_R_NX");


󠁺plotXY(qualitypca,xaxis=VariableAxis)
plotMAP(qualitypca)
plotMAP(qualityiso)
