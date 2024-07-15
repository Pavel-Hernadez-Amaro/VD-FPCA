MFPCA_VD=function(Data, M_grid=NULL, m_npcs=NULL){

aux=sapply(Data, nrow)

stopifnot("Same sample size for all variables required"=all(aux==aux[1]))

n_variables=length(Data)
N=nrow(Data[[1]])
npcs=univ_efunctions=vector(mode = "list", length = n_variables)
EE=NULL

for (ind in 1:n_variables) {

  # THIS ARE GELLAR'S STEPS

  Variable_long<-Data[[ind]]  %>% melt(na.rm=T) %>%
    rename(id=Var1,time=Var2, y=value) %>%
    arrange(id,time) %>%
    filter(!is.na(y))

  #set up data frame for Variable ind
  ndays_df<-ddply(Variable_long,~id,summarize, maxT=length(y)) %>%
    arrange(maxT) %>%
    filter(!(maxT == 1))
  ndays_df$newid<-1:dim(ndays_df)[1]
  Variable_l2<-inner_join(Variable_long,ndays_df,by="id") %>% select(-id) %>% 									dplyr::rename(id=newid) %>%
    select(id, maxT, time, y) %>%
    arrange(id, time)

  #add mean, ytilde
  fit_Variable<-gam(y~s(time,maxT), data= Variable_l2, method="REML")
  Variable_l2$mean<-as.vector(predict(fit_Variable))
  Variable_l2$ydiff<-Variable_l2$y-Variable_l2$mean
  Variable_cov<-datsetup_cov(Variable_l2)

  #fit covar function
  cov_temp<-gam(kprod~ s(stime,ttime,maxT), dat= Variable_cov)

  if(!is.null(M_grid)){
    gridM=M_grid

  }else{

    # THIS IS TO DETERMINE THE NUMBER OF OBSERVATION OF EVERY SAMPLE CURVE IN VARIABLE ind (POSIBLE DOMAIN)
    gridM=apply(X = Data[[ind]], MARGIN = 1, FUN = function(x) sum(!is.na(x)) )

  }


  #Get PC's at times in gridM
  pc_grid<-get_pcs_M(gridM, cov_temp, Hz=1, includezero=F, pve = 0.9) # pve=0.9 means that the number
                                                                      # of PC for the univariate basis will be selected
                                                                      # as the minimum for a 90% of variance explained


  univ_efunctions[[i]]=pc_grid$efunctions

  aux=sapply(pc_grid$evalues, length)

  stopifnot("Same number of principal components required for all the sample curves"=all(aux==aux[1]))

  npcs[[ind]]=length(pc_grid$evalues[[1]])

  ee=matrix(nrow = N, ncol=npcs[[ind]])

  for (i in 1:N) {
    for (j in 1:npcs[[ind]]) {

      # aux=which(gridM==ndays_df$maxT[i])

      SIGMA=matrix(Variable_cov$kprod[ifelse(isempty(ndays_df$maxT[i-1]),1,sum((ndays_df$maxT[1:i-1])^2)+1):sum((ndays_df$maxT[1:i])^2)],nrow=ndays_df$maxT[i],ncol=ndays_df$maxT[i])

      Y_demeaned=as.matrix(Variable_l2$ydiff[(ifelse(isempty(ndays_df$maxT[i-1]),1,sum(ndays_df$maxT[1:i-1])+1)):(sum(ndays_df$maxT[1:i]))])

      # THESE ARE THE SCORES BY Yao et al 2005
      ee[i,j]=pc_grid$evalues[[i]][j] %*% pc_grid$efunctions[[i]][,j] %*% SIGMA %*% Y_demeaned

    }}

  EE=cbind(EE,ee)

  }


Cov_e=lapply(1:nrow(EE), function(x) outer(EE[x,],EE[x,]))

for (ind in 1:length(Cov_e)) {

  Cov_e[[ind]][which(Cov_e[[ind]]<0)]=0

}

for (i in 1:nrow(Cov_e[[1]])) {
  for (j in 1:i) {

    assign(paste0("y_",i,j), sapply(1:nrow(EE),function(x) Cov_e[[x]][i,j]) )

    df=data.frame(y=get(paste0("y_",i,j)), M=gridM)

    # res=sop(formula = y~f(M,nseg = 30),data = df)

    assign(paste0("res_",i,j),gam(y~s(M, bs="ps",k=20), dat= df))

    # assign(paste0("theta_",i,j),c(res$b.fixed,res$b.random))

    assign(paste0("theta_",i,j),res$coefficients)

  }}


model_aux=list()

for (i in 11:paste0(rep(nrow(Cov_e[[1]]),2),collapse = "")) {

  result <- tryCatch({
    get(paste0("res_",i))
  }, error = function(e) {
    return(FALSE)  # Return FALSE on error
  })

  if (!isFALSE(result)) {

    model_aux[[i]]=get(paste0("res_",i))

  }}

model=list()
tag=0
for (i in 11:length(model_aux)) {

  if(!isempty(model_aux[[i]])){

    tag=tag+1

    model[[tag]]=model_aux[[i]]

  }
}

# THIS FUNCTION HAS THE LIMITATION OF NEED THAT EVERY VARIABLE HAVE THE SAME DOMAIN LENGTH ACRROSS MEDICAL VARIABLES

###

# HERE WE EVALUATE THE FUNCTIONAL COVARIATE MATRIX IN A GRID OF DOMAINS AS DEFAULT WE HAVE THE ORIGINAL GRID gridM


Cov_T=vector(mode = "list",length = length(gridM))

for (ind in 1:length(gridM)) {

  aux=rep(0,length(model))

  for (i in 1:length(model)) {

    aux[i]=predict.gam(model[[i]],data.frame(M=gridM[[ind]]))
    if (aux[i]<0) {
      aux[i]=0
    }
  }

  Cov_T[[ind]]=diag(x = 0,nrow = nrow(Cov_e[[1]]))

  for (i in 1:nrow(Cov_T[[ind]])) {
    Cov_T[[ind]][i, i:nrow(Cov_T[[ind]])] <- aux[1:(11-i)]
    Cov_T[[ind]][i:nrow(Cov_T[[ind]]),i] <- aux[1:(11-i)]
    aux <- aux[-(1:(11-i))]  # Remove used elements
  }

}

####

v_m=c_m=vector(mode = "list",length = length(Cov_T))

  for (i in 1:length(Cov_T)) {

    aux=eigen(Cov_T[[i]])

    sub_npcs = ifelse(is.null(m_npcs), min(which(cumsum(aux$values)/sum(aux$values) > 0.9)), m_npcs)
    sub_npcs = min(sub_npcs, gridM[i])

    c_m[[i]]=aux$vectors[,1:sub_npcs]
    v_m[[i]]=aux$values[1:sub_npcs]

  }



score_m=Phi_m=vector(mode = "list",length = length(gridM))

for (i in 1:length(gridM)) {

  score_m[[i]] = t(as.matrix(EE[i,])) %*% c_m[[i]]

  univ_efunctions_aux=NULL
  for (ind in 1:n_variables) {

    univ_efunctions_all=cbind(univ_efunctions[[ind]][[i]])

  }

  Phi_m[[i]] = univ_efunctions_all %*% as.matrix(c_m[[i]])


}


return(list(
            score_m=score_m,
            Phi_m=Phi_m,
            univ_efunctions=univ_efunctions,
            EE=EE,
            npcs=npcs
            ))


}
