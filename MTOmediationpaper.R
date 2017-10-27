###########################################################
### Code to replicate analyses for:                     ###
### Rudolph et al., Mediation of neighborhood effects   ### 
### on adolescent substance use by the school and peer  ###
### environments in a large-scale housing voucher       ###
### experiment. Epidemiology. 2017                      ###
### Note that code for the TMLE SDE/SIE estimator       ###
### function that can be applied generally can be found ###
### here: https://arxiv.org/abs/1707.09021
### What follows is code specific for this analysis     ###
###########################################################

###########################################################
### CODE TO GENERATE RESULTS FOR BOYS FOR MARIJUANA USE ###
###########################################################

library(mice)
library(ggplot2)
library(MASS)
library(glmnet)
library(doParallel)
set.seed(42394)

source("C:/Users/rudol181/Documents/cvControl.R")

#load data
for(i in 1:30){
  assign(paste0("someintoutimp",i),complete(imp4,action=i, include=FALSE))
}
impdat <- list(someintoutimp1, someintoutimp2, someintoutimp3, someintoutimp4, someintoutimp5, someintoutimp6, someintoutimp7, someintoutimp8, someintoutimp9, someintoutimp10, 
               someintoutimp11, someintoutimp12, someintoutimp13, someintoutimp14, someintoutimp15, someintoutimp16, someintoutimp17, someintoutimp18, someintoutimp19, someintoutimp20, 
               someintoutimp21, someintoutimp22, someintoutimp23, someintoutimp24, someintoutimp25, someintoutimp26, someintoutimp27, someintoutimp28, someintoutimp29, someintoutimp30)

######################################
### TESTING WHICH SITES TO COMBINE ###
######################################

##pot use
fitbigger<-lm(pot30~ voucher +I(ra_site==2) + I(ra_site==3) + I(ra_site==5) + voucher:I(ra_site==2) + voucher:I(ra_site==3) + voucher:I(ra_site==5), data=ch2dat[ch2dat$svy_gender==0,])
fitsmaller<-lm(pot30~ voucher +I(ra_site==2) + I(ra_site==5) + voucher:I(ra_site==2) + voucher:I(ra_site==5), data=ch2dat[ch2dat$svy_gender==0,])
pchisq(2*(logLik(fitbigger) - logLik(fitsmaller)), df=2, lower.tail=FALSE)
#'log Lik.' 0.07125706 (df=9), can combine chi and la but may not be a good idea bc small p value if the other is serving as the reference
fitsmallerer<-lm(pot30~ voucher+I(ra_site==2)  + voucher:I(ra_site==2), data=ch2dat[ch2dat$svy_gender==0,])
pchisq(2*(logLik(fitsmaller) - logLik(fitsmallerer)), df=2, lower.tail=FALSE)
#'log Lik.' 0.01002419 (df=7)
fitbigger<-lm(pot30~ voucher +I(ra_site==2) + I(ra_site==3) + I(ra_site==4) + voucher:I(ra_site==2) + voucher:I(ra_site==3) + voucher:I(ra_site==4), data=ch2dat[ch2dat$svy_gender==0,])
fitsmaller<-lm(pot30~ voucher +I(ra_site==3) + I(ra_site==4) + voucher:I(ra_site==3) + voucher:I(ra_site==4), data=ch2dat[ch2dat$svy_gender==0,])
pchisq(2*(logLik(fitbigger) - logLik(fitsmaller)), df=2, lower.tail=FALSE)
fitsmallerer<-lm(pot30~ voucher+I(ra_site==3)  + voucher:I(ra_site==3), data=ch2dat[ch2dat$svy_gender==0,])
pchisq(2*(logLik(fitsmaller) - logLik(fitsmallerer)), df=2, lower.tail=FALSE)
pchisq(2*(logLik(fitbigger) - logLik(fitsmallerer)), df=4, lower.tail=FALSE)
fitsmallererer<-lm(cig30~ voucher, data=ch2dat[ch2dat$svy_gender==1,])
pchisq(2*(logLik(fitsmallerer) - logLik(fitsmallererer)), df=2, lower.tail=FALSE)
pchisq(2*(logLik(fitbigger) - logLik(fitsmallererer)), df=6, lower.tail=FALSE)
#males: combine boston, LA, NYC

######################################
### TOTAL EFFECT ESTIMATION ##########
######################################
desm<-svydesign(id=~1,weights=~wt_totsvy, data=ch2dat[ch2dat$svy_gender==0& ch2dat$svy_age_imp<18,])
totaleffect<-summary(svyglm(pot30~ voucher, design=desm, subset=ra_site%in%c(2,4,5)))$coefficients[2,1:2]

######################################
### ESTIMATION OF G_M ################
######################################

#only do this for the first imputed dataset, because g_M are treated as known
i<-1
datlist<-impdat[[1]][impdat[[1]]$ra_site %in% c(2,4,5)  & impdat[[1]]$svy_gender==0 & impdat[[1]]$svy_age_imp<18,]

covlist<-c("ra_site", "svy_ethrace", "svy_age_imp")

#baseline covariates
wcols<-colnames(datlist[,c(3,5,6,  10:17,73:96,100,116:127, 130, 132:137, 139:140, 142:144, 146:148, 150:152, 154:155, 179,181:244, 246:343, 345:364, 366:373, 375:411,413:415, 419:428, 430:432,434,435)])

#covariates interacted with adherence
wintzcols<-colnames(datlist[,c(128,129,156,158,159)])

tmpdatw<-tmpdatwintz<-tmpdatwintm<-tmpdat<-datlist

#limit to those with complete mediator/outcome information
tmpdat<-datlist[datlist$radid_person %in% ch2dat[!is.na(ch2dat$pot30) & !is.na(ch2dat$sn_peer_drugs),]$radid_person,]

tmpdatza1<-tmpdatza0<-tmpdatz<-tmpdat
tmpdatza1$voucher<-1
tmpdatza0$voucher<-0

pfac<-rep(1, nrow(tmpdat[,c("voucher", wcols)]))
pfac[which( colnames(tmpdat[,c("voucher", wcols)]) %in% c("voucher", covlist) )]<-0
cl<-makePSOCKcluster(4)
registerDoParallel(cl)

cvfit<-cv.glmnet(x=data.matrix(tmpdat[,c("voucher", wcols)]), y=tmpdat$svy_cmove,  
                 family="binomial", weights=tmpdat$wt_totsvy, penalty.factor=pfac, parallel=TRUE)
za1<-predict(cvfit, newx=data.matrix(tmpdatza1[,c("voucher", wcols)]), s="lambda.1se", type="response")
za0<-predict(cvfit, newx=data.matrix(tmpdatza0[,c("voucher", wcols)]), s="lambda.1se", type="response")

tmpdatmz1<-tmpdatmz0<-tmpdatm<-tmpdat[,c("svy_cmove", wcols, wintzcols)]
tmpdatmz1$svy_cmove<-1
tmpdatmz0$svy_cmove<-0

pfac<-rep(1, ncol(tmpdatm))
pfac[which( colnames(tmpdatm) %in% c("svy_cmove", covlist) )]<-0
cvfit<-cv.glmnet(x=data.matrix(tmpdatm), y=tmpdat[,"sn_peer_drugs"], family="binomial", weights=tmpdat$wt_totsvy, 
                 penalty.factor=pfac, parallel=TRUE)

stopCluster(cl)
mz1<-predict(cvfit, type="response", newx=data.matrix(tmpdatmz1), s="lambda.1se")
mz0<-predict(cvfit, type="response", newx=data.matrix(tmpdatmz0), s="lambda.1se")

gm<-(mz1*za0) + (mz0*(1-za0))
gma1<-(mz1*za1) + (mz0*(1-za1))

##############################################################
### ESTIMATION OF DIRECT AND INDIRECT EFFECTS ################
##############################################################
#covariates interacted with mediator
wintmcols<-colnames(datlist[,c(245,374,412)])

resdat<-rep(list(c(1:4)), 30)

for(i in 1:30){
  
  print(i)
  datlist<-impdat[[i]][impdat[[i]]$ra_site %in% c(2,4,5)  & impdat[[i]]$svy_gender==0 & impdat[[i]]$svy_age_imp<18,]

  tmpdatw<-tmpdatwintz<-tmpdatwintm<-tmpdat<-datlist
  
  #limit to those with nonmissing mediator and outcome
  #tmpdat<-datlist[datlist$radid_person %in% ch2dat[!is.na(ch2dat[,"pot30"]),]$radid_person,]
  #640
  tmpdat<-datlist[datlist$radid_person %in% ch2dat[!is.na(ch2dat[,"pot30"]) & !is.na(ch2dat[,"sn_peer_drugs"]),]$radid_person,]
  tmpdat[,"pot30"]<-as.numeric(as.character(tmpdat[,"pot30"]))
  tmpdat[,"sn_peer_drugs"]<-as.numeric(as.character(tmpdat[,"sn_peer_drugs"]))
  
  tmpdatm<-tmpdat[,c("svy_cmove", wcols, wintzcols)]
  pfac<-rep(1, ncol(tmpdatm))
  pfac[which( colnames(tmpdatm) %in% c("svy_cmove", covlist) )]<-0
  cl<-makePSOCKcluster(4)
  registerDoParallel(cl)
  cvfit<-cv.glmnet(x=data.matrix(tmpdatm), y=tmpdat[,"sn_peer_drugs"], family="binomial", weights=tmpdat$wt_totsvy, 
                   penalty.factor=pfac, parallel=TRUE)
  mz<-predict(cvfit, type="response", newx=data.matrix(tmpdatm), s="lambda.1se")
  
  psa1<-I(tmpdat$voucher==1)/mean(tmpdat$voucher)
  psa0<-I(tmpdat$voucher==0)/mean((1-tmpdat$voucher))
  
  psm<-(mz*tmpdat[,"sn_peer_drugs"]) + ((1-mz)*(1-tmpdat[,"sn_peer_drugs"]))
  
  #clever covariate
  tmpdat$ha1a1<-((tmpdat[,"sn_peer_drugs"]*gma1 + (1-tmpdat[,"sn_peer_drugs"])*(1-gma1))/psm) * psa1 * tmpdat$wt_totsvy
  tmpdat$ha1a0<-((tmpdat[,"sn_peer_drugs"]*gm + (1-tmpdat[,"sn_peer_drugs"])*(1-gm))/psm) * psa1 * tmpdat$wt_totsvy
  tmpdat$ha0a0<-((tmpdat[,"sn_peer_drugs"]*gm + (1-tmpdat[,"sn_peer_drugs"])*(1-gm))/psm) * psa0 * tmpdat$wt_totsvy
  
  #make folds that are balanced by the outcome
  N<-nrow(tmpdat)
  id<-NULL
  Y<-as.numeric(as.character(tmpdat[,"pot30"]))
  cvControl<-list(V=5, stratifyCV=TRUE, shuffle=TRUE)
  
  foldsout<-CVFolds(N=N, id=id, Y=Y, cvControl=cvControl)
  tmpdat$folds<-5
  tmpdat$folds[foldsout[[1]]]<-1
  tmpdat$folds[foldsout[[2]]]<-2
  tmpdat$folds[foldsout[[3]]]<-3
  tmpdat$folds[foldsout[[4]]]<-4
  tmpdat$folds[foldsout[[5]]]<-5
  
  tmpdatym0<-tmpdatym1<-tmpdaty<-tmpdat[,c("svy_cmove", "sn_peer_drugs", wcols, wintzcols, wintmcols)]
  tmpdatym0[,"sn_peer_drugs"]<-0
  tmpdatym1[,"sn_peer_drugs"]<-1
  
  #get initial Qy
  pfac<-rep(1, ncol(tmpdaty))
  pfac[which( colnames(tmpdaty) %in% c("svy_cmove", "sn_peer_drugs", covlist) )]<-0
  cvfit<-cv.glmnet(x=data.matrix(tmpdaty), y=tmpdat[,"pot30"], family="binomial", weights=tmpdat$wt_totsvy, penalty.factor=pfac, parallel=TRUE, foldid = tmpdat$folds)
  
  tmpdat$ym<-predict(cvfit, type="response", newx=data.matrix(tmpdaty), s="lambda.1se")
  tmpdat$ym1<-predict(cvfit, type="response", newx=data.matrix(tmpdatym1), s="lambda.1se")
  tmpdat$ym0<-predict(cvfit, type="response", newx=data.matrix(tmpdatym0), s="lambda.1se")
  
  #get update
  epsilona1a1<-coef(glm(formula= paste("pot30", "1", sep="~") , weights=tmpdat$ha1a1, offset=(qlogis(ym)), family="quasibinomial", data=tmpdat[,c("pot30", "ym")]))
  epsilona1a0<-coef(glm(formula= paste("pot30", "1", sep="~") , weights=tmpdat$ha1a0, offset=(qlogis(ym)), family="quasibinomial", data=tmpdat[,c("pot30", "ym")]))
  epsilona0a0<-coef(glm(formula= paste("pot30", "1", sep="~") , weights=tmpdat$ha0a0, offset=(qlogis(ym)), family="quasibinomial", data=tmpdat[,c("pot30", "ym")]))
  
  #updated Qy
  q1upa1a1<-plogis(cbind(qlogis(tmpdat$ym0) + epsilona1a1,  qlogis(tmpdat$ym1) + epsilona1a1))
  q1upa1a0<-plogis(cbind(qlogis(tmpdat$ym0) + epsilona1a0,  qlogis(tmpdat$ym1) + epsilona1a0))
  q1upa0a0<-plogis(cbind(qlogis(tmpdat$ym0) + epsilona0a0,  qlogis(tmpdat$ym1) + epsilona0a0))
  
  #integrate out m
  tmpdat$qma1a1<-q1upa1a1[,1]*(1-gma1) + q1upa1a1[,2]*gma1
  tmpdat$qma1a0<-q1upa1a0[,1]*(1-gm) + q1upa1a0[,2]*gm
  tmpdat$qma0a0<-q1upa0a0[,1]*(1-gm) + q1upa0a0[,2]*gm
  
  #get inital Qz
  tmpdatzfita1<-tmpdat[tmpdat$voucher==1,c( wcols)]
  tmpdatzfita0<-tmpdat[tmpdat$voucher==0,c( wcols)]
  pfac<-rep(1, ncol(tmpdatzfita1))
  pfac[which( colnames(tmpdatzfita1) %in% c( covlist) )]<-0
  cvfit<-cv.glmnet(x=data.matrix(tmpdatzfita1), y=qlogis(tmpdat[tmpdat$voucher==1,]$qma1a1), penalty.factor=pfac, parallel=TRUE)
  #cvfit<-cv.glmnet(x=data.matrix(tmpdatzfita1), y=tmpdat[tmpdat$voucher==1,]$qma1a1, family="quasibinomial", penalty.factor=pfac, parallel=TRUE)
  tmpdat$qza1a1<-predict(cvfit, newx=data.matrix(tmpdat[wcols]), s="lambda.1se")
  cvfit<-cv.glmnet(x=data.matrix(tmpdatzfita1), y=qlogis(tmpdat[tmpdat$voucher==1,"qma1a0"]), penalty.factor=pfac, parallel=TRUE)
  tmpdat$qza1a0<-predict(cvfit,newx=data.matrix(tmpdat[wcols]), s="lambda.1se")
  cvfit<-cv.glmnet(x=data.matrix(tmpdatzfita0), y=qlogis(tmpdat[tmpdat$voucher==0,]$qma0a0), penalty.factor=pfac, parallel=TRUE)
  tmpdat$qza0a0<-predict(cvfit, newx=data.matrix(tmpdat[wcols]), s="lambda.1se")
  stopCluster(cl)
  
  #get update
  epsilonza1a1<-coef(glm(qma1a1 ~ 1, weights=psa1*tmpdat$wt_totsvy, offset=tmpdat$qza1a1, family="quasibinomial", data=tmpdat))
  epsilonza1a0<-coef(glm(qma1a0 ~ 1, weights=psa1*tmpdat$wt_totsvy, offset=tmpdat$qza1a0, family="quasibinomial", data=tmpdat))
  epsilonza0a0<-coef(glm(qma0a0 ~ 1, weights=psa0*tmpdat$wt_totsvy, offset=tmpdat$qza0a0, family="quasibinomial", data=tmpdat))
  
  #updated Qz
  tmpdat$qzupa1a1<-plogis(tmpdat$qza1a1 + epsilona1a1)
  tmpdat$qzupa1a0<-plogis(tmpdat$qza1a0 + epsilona1a0)
  tmpdat$qzupa0a0<-plogis(tmpdat$qza0a0 + epsilona0a0)
  
  #estimate
  tmlea1m1<-sum(tmpdat$qzupa1a1*tmpdat$wt_totsvy)/sum(tmpdat$wt_totsvy)
  tmlea1m0<-sum(tmpdat$qzupa1a0*tmpdat$wt_totsvy)/sum(tmpdat$wt_totsvy)
  tmlea0m0<-sum(tmpdat$qzupa0a0*tmpdat$wt_totsvy)/sum(tmpdat$wt_totsvy)
  
  #components of eic
  eic1a1a1<-tmpdat$ha1a1*(tmpdat$sn_peer_drugs - (plogis(qlogis(tmpdat$ym) + epsilona1a1)))
  eic2a1a1<-psa1*tmpdat$wt_totsvy*(tmpdat$qma1a1 - tmpdat$qzupa1a1)
  eic3a1a1<-tmpdat$qzupa1a1-tmlea1m1
  eica1a1<-eic1a1a1 + eic2a1a1 + eic3a1a1
  
  eic1a1a0<-tmpdat$ha1a0*(tmpdat$sn_peer_drugs - (plogis(qlogis(tmpdat$ym) + epsilona1a0)))
  eic2a1a0<-psa1*tmpdat$wt_totsvy*(tmpdat$qma1a0 - tmpdat$qzupa1a0)
  eic3a1a0<-tmpdat$qzupa1a0-tmlea1m0
  eica1a0<-eic1a1a0 + eic2a1a0 + eic3a1a0
  
  eic1a0a0<-tmpdat$ha0a0*(tmpdat$sn_peer_drugs - (plogis(qlogis(tmpdat$ym) + epsilona0a0)))
  eic2a0a0<-psa0*tmpdat$wt_totsvy*(tmpdat$qma0a0 - tmpdat$qzupa0a0)
  eic3a0a0<-tmpdat$qzupa0a0-tmlea0m0
  eica0a0<-eic1a0a0 + eic2a0a0 + eic3a0a0
  
  ndeeic<-eica1a0 - eica0a0
  vareic<-var(ndeeic)/nrow(tmpdat)
  
  nieeic<-eica1a1 - eica1a0
  varnieeic<-var(nieeic)/nrow(tmpdat)
  
  resdat[[i]]<-list("nde"=tmlea1m0-tmlea0m0, "ndevar"=vareic, "nie"=tmlea1m1-tmlea1m0, "nievar"=varnieeic)
}