#######################################################
#######################################################
##Load required libraries
library(reshape2)
library(plyr)
library(dplyr)
library(caTools)
library(refund)
library(mgcv)
library(SOP)
library(pracma)
options(error=NULL)
# #Load required functions
# path<-"C:/Users/user/Desktop/Trabajo/Escuela/Doctorado/Pavel/Estancia/Codigo/UCGS_1604373_Supplementary Files/"
# source(paste0(path,"vdFunctions.R"))

#######################################################
##ICU data - refund package
# sofa_dat<-sofa$SOFA
# sofa_long<-sofa_dat  %>% melt(na.rm=T) %>%
#   rename(id=Var1,time=Var2, y=value) %>%
#   arrange(id,time) %>%
#   filter(!is.na(y))
#
# #set up data frame for SOFA
# ndays_df<-ddply(sofa_long,~id,summarize, maxT=length(y)) %>%
#   arrange(maxT) %>%
#   filter(!(maxT == 1))
# ndays_df$newid<-1:dim(ndays_df)[1]
# sofa_l2<-inner_join(sofa_long,ndays_df,by="id") %>% select(-id) %>% 									dplyr::rename(id=newid) %>%
#   select(id, maxT, time, y) %>%
#   arrange(id, time)

# #add mean, ytilde
# fit_sofa<-gam(y~s(time,maxT), data= sofa_l2, method="REML")
# sofa_l2$mean<-as.vector(predict(fit_sofa))
# sofa_l2$ydiff<-sofa_l2$y-sofa_l2$mean
# sofa_cov<-datsetup_cov(sofa_l2)
#
# #fit covar function
# cov_sofa<-gam(kprod~ s(stime,ttime,maxT), dat= sofa_cov)
#
# #Get PC's at times in gridM
# gridM= seq(2,170, by=.5)
# pc_grid_sofa<-get_pcs_M(gridM, cov_sofa, Hz=1, includezero=F, npcs=5)
#
# aux=unique(sofa_l2$maxT)
# pc_grid_sofa_aux<-get_pcs_M(aux, cov_sofa, Hz=1, includezero=F, npcs=5)

##################################### Temperature

a_obs=1
b_obs=219 # liimite que me permite el código de Gellar
npcs=5

Temperature_dat<-Temperature[,a_obs:b_obs]
Temperature_long<-Temperature_dat  %>% melt(na.rm=T) %>%
  rename(id=Var1,time=Var2, y=value) %>%
  arrange(id,time) %>%
  filter(!is.na(y))

#set up data frame for Temperature
ndays_df<-ddply(Temperature_long,~id,summarize, maxT=length(y)) %>%
  arrange(maxT) %>%
  filter(!(maxT == 1))
ndays_df$newid<-1:dim(ndays_df)[1]
Temperature_l2<-inner_join(Temperature_long,ndays_df,by="id") %>% select(-id) %>% 									dplyr::rename(id=newid) %>%
  select(id, maxT, time, y) %>%
  arrange(id, time)

#add mean, ytilde
fit_Temperature<-gam(y~s(time,maxT), data= Temperature_l2, method="REML")
Temperature_l2$mean<-as.vector(predict(fit_Temperature))
Temperature_l2$ydiff<-Temperature_l2$y-Temperature_l2$mean
Temperature_cov<-datsetup_cov(Temperature_l2)

#fit covar function
cov_temp<-gam(kprod~ s(stime,ttime,maxT), dat= Temperature_cov)

#Get PC's at times in gridM
gridM = M_temp #unique(Temperature_l2$maxT) # ndays_df$maxT
# gridM_aux= seq(npcs,b_obs, by=1)
pc_grid_temp<-get_pcs_M(gridM, cov_temp, Hz=1, includezero=F, npcs=npcs)

ee_temp=matrix(nrow = nrow(Temperature), ncol=npcs)

for (i in 1:nrow(Temperature)) {
  for (j in 1:npcs) {

  # aux=which(gridM==ndays_df$maxT[i])

  SIGMA=matrix(Temperature_cov$kprod[ifelse(isempty(ndays_df$maxT[i-1]),1,sum((ndays_df$maxT[1:i-1])^2)+1):sum((ndays_df$maxT[1:i])^2)],nrow=ndays_df$maxT[i],ncol=ndays_df$maxT[i])

  Y_demeaned=as.matrix(Temperature_l2$ydiff[(ifelse(isempty(ndays_df$maxT[i-1]),1,sum(ndays_df$maxT[1:i-1])+1)):(sum(ndays_df$maxT[1:i]))])

  ee_temp[i,j]=pc_grid_temp$evalues[[i]][j] %*% pc_grid_temp$efunctions[[i]][,j] %*% SIGMA %*% Y_demeaned

}}

############################### Capilar

Capilar_dat<-Capilar[,a_obs:b_obs]
Capilar_long<-Capilar_dat  %>% melt(na.rm=T) %>%
  rename(id=Var1,time=Var2, y=value) %>%
  arrange(id,time) %>%
  filter(!is.na(y))

#set up data frame for Capilar
ndays_df<-ddply(Capilar_long,~id,summarize, maxT=length(y)) %>%
  arrange(maxT) %>%
  filter(!(maxT == 1))
ndays_df$newid<-1:dim(ndays_df)[1]
Capilar_l2<-inner_join(Capilar_long,ndays_df,by="id") %>% select(-id) %>% 									dplyr::rename(id=newid) %>%
  select(id, maxT, time, y) %>%
  arrange(id, time)

#add mean, ytilde
fit_Capilar<-gam(y~s(time,maxT), data= Capilar_l2, method="REML")
Capilar_l2$mean<-as.vector(predict(fit_Capilar))
Capilar_l2$ydiff<-Capilar_l2$y-Capilar_l2$mean
Capilar_cov<-datsetup_cov(Capilar_l2)

#fit covar function
cov_capilar<-gam(kprod~ s(stime,ttime,maxT), dat= Capilar_cov)

#Get PC's at times in gridM
gridM = M_capilar #unique(Capilar_l2$maxT) # ndays_df$maxT
# gridM_aux= seq(npcs,b_obs, by=1)
pc_grid_capilar<-get_pcs_M(gridM, cov_capilar, Hz=1, includezero=F, npcs=npcs)

ee_cap=matrix(nrow = nrow(Temperature), ncol=npcs)

for (i in 1:nrow(Capilar)) {
  for (j in 1:npcs) {

    # aux=which(gridM==ndays_df$maxT[i])

    SIGMA=matrix(Capilar_cov$kprod[ifelse(isempty(ndays_df$maxT[i-1]),1,sum((ndays_df$maxT[1:i-1])^2)+1):sum((ndays_df$maxT[1:i])^2)],nrow=ndays_df$maxT[i],ncol=ndays_df$maxT[i])

    Y_demeaned=as.matrix(Capilar_l2$ydiff[(ifelse(isempty(ndays_df$maxT[i-1]),1,sum(ndays_df$maxT[1:i-1])+1)):(sum(ndays_df$maxT[1:i]))])

    ee_cap[i,j]=pc_grid_capilar$evalues[[i]][j] %*% pc_grid_capilar$efunctions[[i]][,j] %*% SIGMA %*% Y_demeaned

  }}


EE=cbind(ee_temp,ee_cap)

Cov_e=lapply(1:nrow(EE), function(x) outer(EE[x,],EE[x,]))

for (ind in 1:length(Cov_e)) {

  Cov_e[[ind]][which(Cov_e[[ind]]<0)]=0

}

for (i in 1:nrow(Cov_e[[1]])) {

print(paste0("i = ",i))

for (j in 1:i) {

print(paste0("j = ",j))

assign(paste0("y_",i,j), sapply(1:nrow(EE),function(x) Cov_e[[x]][i,j]) )

df=data.frame(y=get(paste0("y_",i,j)),M=M_temp)

# res=sop(formula = y~f(M,nseg = 30),data = df)

assign(paste0("res_",i,j),gam(y~s(M, bs="ps",k=20), dat= df))

# assign(paste0("theta_",i,j),c(res$b.fixed,res$b.random))

assign(paste0("theta_",i,j),res$coefficients)

}}

model_aux=list()

for (i in 11:1010) {

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

#mean(abs(res$fitted.values-df$y))

# THIS FUNCTION HAS THE LIMITATION OF NEED THAT EVERY VARIABLE HAVE THE SAME DOMAIN LENGTH ACRROSS MEDICAL VARIABLES
Cov_T_spec = function(model,Domain,npcs,n_var){

Cov_T=vector(mode = "list",length = length(Domain)) #matrix(nrow = npcs*2, ncol = npcs*2)

for (ind in 1:length(Domain)) {

aux=rep(0,length(model))

for (i in 1:length(model)) {

  aux[i]=predict.gam(model[[i]],data.frame(M=Domain[[ind]]))
    if (aux[i]<0) {
      aux[i]=0
    }
  }

Cov_T[[ind]]=matrix(nrow = npcs*n_var, ncol = npcs*n_var)

for (i in 1:nrow(Cov_T[[ind]])) {
  Cov_T[[ind]][i, i:nrow(Cov_T[[ind]])] <- aux[1:(11-i)]
  Cov_T[[ind]][i:nrow(Cov_T[[ind]]),i] <- aux[1:(11-i)]
  aux <- aux[-(1:(11-i))]  # Remove used elements
}

  }

Cov_T

}


Cov_temp_cap=Cov_T_spec(model,M_temp,5,2)


middle_eigen=function(Cov_matrices,m_npcs){

  N=length(Cov_matrices)

  v_m=c_m=vector(mode = "list",length = N)

  for (i in 1:N) {

    c_m[[i]]=eigen(Cov_matrices[[i]])$vectors[,1:m_npcs]
    v_m[[i]]=eigen(Cov_matrices[[i]])$values[1:m_npcs]

  }

  return(list(c_m=c_m,v_m=v_m))

}


aux=middle_eigen(Cov_matrices = Cov_temp_cap,m_npcs = 3)

score_m=Phi_m=vector(mode = "list",length = length(M_temp))

for (i in 1:length(M_temp)) {

  score_m[[i]] = t(as.matrix(EE[i,])) %*% aux$c_m[[i]]

  Phi_m[[i]] = cbind(pc_grid_temp$efunctions[[i]],pc_grid_capilar$efunctions[[i]]) %*% as.matrix(aux$c_m[[i]])


}

Phi_m[[i]] %*% t(score_m[[i]])

i=40

pc_grid_temp$efunctions[[i]] %*% as.matrix(EE[i,1:npcs])

Temperature_l2$y[which(Temperature_l2$id==i)]

Temperature_l2$ydiff[which(Temperature_l2$id==i)]
