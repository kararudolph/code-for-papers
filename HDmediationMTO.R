files.sources<-c("c.r", "b.r", "crossfit.r", "e.r", "folds.r", "g.r", "h_m.r", "h_z.r", "mediation.R", "u.r", "utils.r", "v.r", "npsem.r", "not_transported.R", "transported.R")
sapply(files.sources, source)

Wnames=c( "RA_SITE" , "re0v1", "re2v1", "re3v1", "F_SVY_AGE_BL_IMP",  "X_F_C1_BEHPRB" , "X_F_C1_GIFTED","X_F_AD_EDGRADHS",  "X_F_AD_NEVMARR","X_F_AD_PARENTU18",
          "X_F_AD_WORKING", "X_F_HH_AFDC" , "X_F_HH_DISABL", "X_F_HH_SIZE2", "X_F_HH_SIZE3", "X_F_HH_SIZE4",
          "X_F_HOOD_UNSAFENIT",  "X_F_HOOD_VERYDISSAT", "X_F_HOUS_MOV3TM","X_F_HOUS_MOVSCHL", "X_F_HOUS_SEC8BEF" , "F_C9010T_PERPOV_BL"  )

A<-"RA_POOLGRP_EXPS8"
Z<-"F_SVY_CMOVE.x"
weights<-"F_WT_TOTSVY"

#group mediators

##Neighborhood
Mnhoodpov<-"F_C9010T_PERPOV_DW"

##school environment
Mse1<-"F_SC_S_FRRDLUNCH_DUR"
Mse2<-"F_SC_TITLE1_ALL_DUR"
Mse3<-"F_SC_RANK_DUR"
Mse4<-"F_SC_RATIO_PUPTCH_DUR"
Mse<-c(Mse1, Mse2, Mse3, Mse4)
Mse<-c(Mse2, Mse3)

##school/home instability
Minstab1<-"F_SH_CNT_SCH"
Minstab2<-"F_SPL_MOVES_N"
Minstab<-c(Minstab1, Minstab2)

allmediators<-c(Mnhoodpov, Mse, Minstab)

Y<- "F_MH_MOOD_Y_YT"

alldat<-datimp[datimp$F_SVY_GENDER==0 & datimp$RA_SITE!=1, c(Wnames,A, Z,  allmediators, Y, weights) ]

#no transport
pi<-1/alldat[,weights]
alldat$weights<-alldat[,weights]/mean(alldat[,weights])

library(data.table)
library("earth")
library("xgboost")
library("glmnet")
library("BART")
slres<-c("SL.glm", "SL.mean", "SL.glmnet", "SL.earth", "SL.xgboost")

set.seed(408)
nottrans_all<-not_transported(data=alldat, A="RA_POOLGRP_EXPS8", W=Wnames, Z=c("F_SVY_CMOVE.x"), M=allmediators,  Y=Y, weights="weights", family="binomial", folds=10, learners_g = slres,
                                    learners_e = slres,
                                    learners_b = slres,
                                    learners_hz = slres,
                                    learners_u =slres,
                                    learners_ubar = slres,
                                    learners_v = slres,
                                    learners_vbar = slres)

set.seed(408)
nottrans_school<-not_transported(data=alldat, A="RA_POOLGRP_EXPS8", W=Wnames, Z=c("F_SVY_CMOVE.x", Minstab, Mnhoodpov), M=Mse,  Y=Y, weights="weights", family="binomial", folds=10, learners_g = slres,
                              learners_e = slres,
                              learners_b = slres,
                              learners_hz = slres,
                              learners_u =slres,
                              learners_ubar = slres,
                              learners_v = slres,
                              learners_vbar = slres)

set.seed(408)
nottrans_move<-not_transported(data=alldat, A="RA_POOLGRP_EXPS8", W=Wnames, Z=c("F_SVY_CMOVE.x", Mnhoodpov), M=Minstab,  Y=Y,weights="weights", family="binomial", folds=10, learners_g = slres,
                                   learners_e = slres,
                                   learners_b = slres,
                                   learners_hz = slres,
                                   learners_u =slres,
                                   learners_ubar = slres,
                                   learners_v = slres,
                                   learners_vbar = slres)

set.seed(408)
nottrans_pov<-not_transported(data=alldat, A="RA_POOLGRP_EXPS8", W=Wnames, Z=c("F_SVY_CMOVE.x", Minstab), M=Mnhoodpov,  Y=Y, weights="weights",family="binomial", folds=10, learners_g = slres,
                learners_e = slres,
                learners_b = slres,
                learners_hz = sldebug,
                learners_u =slres,
                learners_ubar = slres,
                learners_v = slres,
                learners_vbar = slres)
