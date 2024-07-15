library(MFPCA)
library(SOP)
library(ks)
options(error=NULL)
xtimes=unique(sofa_l2$maxT)
cov_sofa<-gam(kprod~ s(stime,ttime,maxT), dat= sofa_cov)
covfx=cov_sofa

nsub<-length(xtimes)

Cov_T=vector(mode = "list",length = nsub)

for(i in 1:nsub){
  maxt=xtimes[i]
  argvals=if(includezero){seq(0,maxt,by=1/Hz)}else{seq(0,maxt,by=1/Hz)[-1]}

  #predict covar mtx for subject i (based on maxT = M)
  Cov_T[[i]]<-pcov_m(covfx, times=maxt,argvals=argvals,  upper=F, includezero=includezero)[[1]]
}

y=vec(Cov_T[[11]])

# aux=unique(sofa_l2$maxT)
# pc_grid_sofa_aux<-get_pcs_M(aux, cov_sofa, Hz=1, includezero=F, npcs=5)


E_values=matrix(nrow = nsub-3, ncol = npcs)
E_functions=matrix(nrow = nsub-3, ncol = npcs)

for (ind in 1:(nsub-3)) {

  E_values[ind,] = pc_grid_sofa_aux$evalues[[ind+3]]


}


uniExpansions <- lapply(pc_grid_sofa_aux, function (mark) {
  list(type = "given", functions = funData::funData(
    argvals = mark$argvals, X = t(mark$efunctions)))
})

MFPCA()



