#######################################################
#Load required functions
path<-"C:/Users/user/Desktop/Trabajo/Escuela/Doctorado/Pavel/Estancia/Codigo/UCGS_1604373_Supplementary Files/"
source(paste0(path,"vdFunctions.R"))

#######################################################
##ICU data - refund package
sofa_dat<-sofa$SOFA
sofa_long<-sofa_dat  %>% melt(na.rm=T) %>%
						 rename(id=Var1,time=Var2, y=value) %>%		
						 arrange(id,time) %>% 
						 filter(!is.na(y)) 

#set up data frame for SOFA
ndays_df<-ddply(sofa_long,~id,summarize, maxT=length(y)) %>% 
				arrange(maxT) %>% 
				filter(!(maxT == 1))
ndays_df$newid<-1:dim(ndays_df)[1]
sofa_l2<-inner_join(sofa_long,ndays_df,by="id") %>% select(-id) %>% 									dplyr::rename(id=newid) %>% 
					select(id, maxT, time, y) %>% 
					arrange(id, time)

#add mean, ytilde
fit_sofa<-gam(y~s(time,maxT), data= sofa_l2, method="REML")
sofa_l2$mean<-as.vector(predict(fit_sofa))
sofa_l2$ydiff<-sofa_l2$y-sofa_l2$mean
sofa_cov<-datsetup_cov(sofa_l2)

#fit covar function
cov_sofa<-gam(kprod~ s(stime,ttime,maxT), dat= sofa_cov)

#Get PC's at times in gridM
gridM= seq(2,170, by=.5)
pc_grid_sofa<-get_pcs_M(gridM, cov_sofa, Hz=1, includezero=F, npcs=5)


#######################################################
#Scaled method
sofa_l<-melt(sofa$SOFA,na.rm=T)
colnames(sofa_l)<-c(".id",".index",".value")
sofa_l<-filter(sofa_l,!is.na(.value))
time_vec<-1:max(sofa_l$.index)

#exclude those with only one day of data
llim=1
lengths_icu<-ddply(sofa_l,~.id,summarize, n=length(.value))
shortids<-which(lengths_icu$n>llim)
sofa_lgt1<-filter(sofa_l,.id %in% shortids)

samp_rateICU<-30
time_out<-seq(0,1,length.out=samp_rateICU)
sofa_interp<-matrix(NA,ncol=samp_rateICU,nrow=length(shortids))
for(i in 1:length(shortids)){
	sub_dat<-filter(sofa_lgt1,.id==shortids[i])
	sofa_interp[i,]<-approx(translate(sub_dat$.index),sub_dat$.value,xout=time_out)$y
}
sofa_fpca_stretch<-fpca.sc(sofa_interp)

#######################################################
#Weighted method
sofa_fpca_sc<-fpca.sc(sofa_dat)

#######################################################
#######################################################
##Accelerometry data
#######################################################

########Load chair stand data
homedir<-"/homedir/path/"
load(paste0(homedir,"stand_both.RData"))
names(standonly_list)<-c("time", "magnitude","fb","ud","lr")
ttime<-lengths(standonly_list[[1]])/80
maxlength<-max(lengths(standonly_list[[1]]))

padna<-function(x,maxlength){
	x<-unlist(x)
	navec<-rep(NA,(maxlength - length(x)))
	new_vec<-c(x,navec)
	return(new_vec)
}

ttime_ord<-ttime[order(ttime)]

#pad accel data (fb axis)
fb_napad<-rep(NA,length(maxlength))
for(i in 1:length(ttime)){
	temp<-padna(standonly_list[[3]][i],maxlength)
	fb_napad<-rbind(fb_napad,temp)
}
fb_napad<-fb_napad[-1,]
fb_naorder<-fb_napad[order(ttime),]

##modify dataframe
rownames(fb_naorder)<-NULL
colnames(fb_naorder)<-standonly_list$time[[53]]
fb_df<-as.data.frame(fb_naorder)
fb_df$obs<-1:81
fb_df$maxT<-ttime_ord

#melt to long format
fb_long<-melt(fb_df,id=c("obs","maxT"))
colnames(fb_long)[3:4]<-c("time","y")
fb_long$time<-as.numeric(levels(fb_long$time))[fb_long$time]
fb2<-fb_long[-which(is.na(fb_long$y)),]
fb_ord<-fb2[order(fb2$obs),]
names(fb_ord)[1]="id"

######################################################
#Variable method
fit1<-gam(y~s(time,maxT),data= fb_ord,method="REML")
meanfb<-as.vector(predict(fit1))
fb_ord$mean<-meanfb
fb_ord$ydiff<-fb_ord$y-fb_ord$mean
fb_cov<-datsetup_cov(fb_ord)

#fit covar function
cov_fb<-gam(kprod~ s(stime,ttime,maxT), dat= fb_cov)

#Get PC's at times in gridM
gridM_fb= seq(0.5,2.5, by=.025)
pc_grid_fb<-get_pcs_M(gridM_fb, cov_fb, Hz=80,includezero=T)


######################################################
#Scaled method
samp_rate_accel<-100
fb_dat<-select(fb_ord,id,time,y)
colnames(fb_dat)<-c(".id",".index",".value")
accel_ids<-unique(fb_dat$.id)

time_vec<-seq(0, max(fb_dat$.index),length.out=100)
time_out<-seq(0,1,length.out=samp_rate_accel)
fb_interp<-matrix(NA,ncol= samp_rate_accel,nrow=length(accel_ids))
for(i in 1:length(accel_ids)){
	sub_dat<-filter(fb_dat,.id == accel_ids[i])
	fb_interp[i,]<-approx(translate(sub_dat$.index),sub_dat$.value, xout=time_out)$y
}
fb_sc<-refund::fpca.sc(Y=fb_interp,cov.est.method=2,pve=0.95)

######################################################
#Weighted method
fb_dat<-select(fb_ord,id,time,y)
colnames(fb_dat)<-c(".id",".index",".value")
fb_fpca_w<-refund::fpca.sc(ydata=fb_dat,pve=0.95)
