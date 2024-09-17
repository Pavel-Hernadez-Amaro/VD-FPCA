# FIRST SIMULATION TRY FOR MFPCA_VD
options(error=NULL)
library(plyr)
library(dplyr)
library(pracma)
library(mgcv)

# N=250
#
# sigma_sqared=0
#
# M_1=M_2=8
#
# t=seq(0,1,len=100)

######### GELLAR BASED

eigen_fun_real_1=function(M, K=10){

  Phi_real=vector(mode = "list",length = length(M))


  for (ind in 1:length(M)) {

    t=1:M[ind]

    Phi_real[[ind]]=array(dim = c(length(t),K))

    for (j in 1:5) {

      Phi_real[[ind]][,(j*2)-1]=sin(2*j*pi*t/M[ind])*sqrt(2)/sqrt(M[ind])
      Phi_real[[ind]][,j*2]=cos(2*j*pi*t/M[ind])*sqrt(2)/sqrt(M[ind])

    }

  }

  Phi_real

}

eigen_fun_real_2=function(M, K=10){

  Phi_real=vector(mode = "list",length = length(M))


  for (ind in 1:length(M)) {

    t=1:M[ind]

    W=pnorm(M[ind], 30, 10)

    Phi_real[[ind]]=array(dim = c(length(t),K))

    for (j in 1:K) {

      Phi_real[[ind]][,j]=W*sin(2*j*pi*t/M[ind])*sqrt(2)/sqrt(M[ind]) + (1-W)*cos(2*j*pi*t/M[ind])*sqrt(2)/sqrt(M[ind])

    }

  }

  Phi_real

}

set.seed(42)

N=100
K=10
sigma=0.01
epsilon=rnorm(N,0,sigma)

M=sort(round(runif(N,10,80)))

t=2:max(M)

mu=0.0001*(t-120)^2+3*sin(pi*t/60)

eigen_val_real=sapply(1:K, FUN = function(x) 0.5^(x-1) )

Phi_real_1=eigen_fun_real_1(M)
Phi_real_2=eigen_fun_real_2(M)

X_1=X_2=matrix(nrow = N, ncol = max(M))

scores_real_1=scores_real_2=matrix(nrow = N, ncol = K)

for (i in 1:N) {

  scores_real_1[i,]=rnorm(K,0,eigen_val_real)
  scores_real_2[i,]=rnorm(K,0,eigen_val_real)

  X_1[i,1:M[i]] = mu[1:M[i]] + Phi_real_1[[i]] %*% scores_real_1[i,] + epsilon[i]
  X_2[i,1:M[i]] = mu[1:M[i]] + Phi_real_2[[i]] %*% scores_real_2[i,] + epsilon[i]

}

case=81

plot(X_1[case,1:M[case]])
plot(X_2[case,1:M[case]])

plot(Phi_real_1[[1]][,1], type="l")
plot(Phi_real_2[[1]][,1], type="l")


Data_sim=list(X_1=X_1,X_2=X_2)

res_sim=MFPCA_VD_int(Data_sim, m_npcs = 3)

X_1_hat=X_2_hat=matrix(nrow = N, ncol=max(M))

for (i in 1:N) {

  X_1_hat[i,1:M[i]]=res_sim$Phi_m[[i]][[1]] %*% res_sim$score_m[i,]

  X_2_hat[i,1:M[i]]=res_sim$Phi_m[[i]][[2]] %*% res_sim$score_m[i,]

}


case=8

plot(X_1[case,1:M[case]])
plot(X_1_hat[case,1:M[case]])

plot(X_2[case,1:M[case]])
plot(X_2_hat[case,1:M[case]])

X_1_hat_uni=X_2_hat_uni=matrix(nrow = N, ncol=max(M))

for (i in 1:N) {

  X_1_hat_uni[i,1:M[i]]=res_sim$univ_efunctions[[1]][[i]] %*% res_sim$EE[i,1:5]

  X_2_hat_uni[i,1:M[i]]=res_sim$univ_efunctions[[2]][[i]] %*% res_sim$EE[i,6:10]

}

case=100

plot(X_1[case,1:M[case]])
plot(X_1_hat_uni[case,1:M[case]])
lines(X_1_hat[case,1:M[case]])

plot(X_2[case,1:M[case]])
plot(X_2_hat_uni[case,1:M[case]])
lines(X_2_hat[case,1:M[case]])
