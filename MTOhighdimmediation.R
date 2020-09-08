library(SuperLearner)
library(mice)
library(mitools)
library(data.table)

source("slfunctions.R")
source("estimsl.R")

#read in imputed datasets
implist<-lapply(Sys.glob("imputednointupdatedjuly2020*.csv"), read.csv)

Wnames=c("RA_SITE", "re0v1", "re2v1", "re3v1", "F_SVY_AGE_BL_IMP", "X_F_C1_BEHPRB", "X_F_C1_GIFTED", "X_F_AD_EDGRADHS", "X_F_AD_NEVMARR", "X_F_AD_PARENTU18", "X_F_AD_WORKING", "X_F_HH_AFDC", "X_F_HH_DISABL", "X_F_HH_SIZE2", "X_F_HH_SIZE3", "X_F_HH_SIZE4", "X_F_HOOD_UNSAFENIT", "X_F_HOOD_VERYDISSAT", "X_F_HOUS_MOV3TM", "X_F_HOUS_MOVSCHL", "X_F_HOUS_SEC8BEF", "F_C9010T_PERPOV_BL")

A<-"RA_POOLGRP_EXPS8"
Z<-"F_SVY_CMOVE"
Y<-"F_MH_ANY_Y_YT"
weights<-"F_WT_TOTSVY"
allmediators<-c("F_C9010T_PERPOV_DW", "F_SC_S_FRRDLUNCH_DUR", "F_SC_TITLE1_ALL_DUR", "F_SC_RANK_DUR", "F_SC_RATIO_PUPTCH_DUR", "F_SH_CNT_SCH", "F_SPL_MOVES_N" "F_SH_BLDIST_RCNT", "F_SH_CNT_YR_SCHCHG")

for(k in 1:10){
	datimp<-implist[[k]]
	alldat<-datimp[datimp$F_SVY_GENDER==0 & datimp$RA_SITE!=1, c(Wnames,A,Z,allmediators, Y, weights)]
	setnames(alldat, old=c(A,Z, Y, weights), new=c("A", "Z", "Y", "weights"))
	newWnames<-c()
	for(g in 1:length(Wnames)){
		newWnames[g]<-paste0("W", g)
	}
	newMnames<-c()
	for(m in 1:length(allmediators)){
		newMnames[m]<-paste0("M",m)
	}
	setnames(alldat, old=c(Wnames, allmediators), new=c(newWnames, newMnames))
	A<-alldat[,"A"]
	M<-alldat[,substr(names(alldat), 1, 1)=="M"]
	Z<-alldat[,"Z"]
	Y<-alldat[,"Y"]
	W<-alldat[substr(names(alldat),1,1)=="W"]
	candidates<-c("SL.glm", "SL.mean", "SL.gam", "SL.earth", "SL.glm.interaction", "SL.bayesglm")
	res<-mediation(alldat, nfolds=3, candidates=candidates, family.outcome="binomial")

	betassieos[[k]]<-res[,1]
	varssieos[[k]]<-res[,5]^2
	betasdeos[[k]]<-res[,3]
	varssdeos[[k]]<-res[,7]^2
	betassietmle[[k]]<-res[,2]
	varssietmle[[k]]<-res[,6]^2
	betassdetmle[[k]]<-res[,4]
	varssdetmle[[k]]<-res[,8]^2
}

betassieos2<-lapply(betassieos, function(y) ifelse(y==1,NA,y))
varssieos2<-lapply(varssieos, function(y) ifelse(y==1,NA,y))
betassdeos2<-lapply(betassdeos, function(y) ifelse(y==1,NA,y))
varssdeos2<-lapply(varssdeos, function(y) ifelse(y==1,NA,y))
betassietmle2<-lapply(betassietmle, function(y) ifelse(y==1,NA,y))
varssietmle2<-lapply(varssietmle, function(y) ifelse(y==1,NA,y))
betassdetmle2<-lapply(betassdetmle, function(y) ifelse(y==1,NA,y))
varssdetmle2<-lapply(varssdetmle, function(y) ifelse(y==1,NA,y))

boysmedall<-rbind(as.data.frame(summary(MIcombine(betassieos2, varssieos2))), as.data.frame(summary(MIcombine(betassietmle2, varssietmle2))), as.data.frame(summary(MIcombine(betassdeos2, varssdeos2))), as.data.frame(summary(MIcombine(betassdetmle2, varssdetmle2))))
