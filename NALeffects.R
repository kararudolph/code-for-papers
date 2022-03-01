library(tidyverse)

#load in longitudinal data, call it lmtpdat

a <- names(lmtpdat[, paste0("bin_have_NAL_3.", 2007:2017)])

baselinenameroot <- c( "med_earn.", "unemp_rate.", "pov." ,"male." ,"hisp."  , "white.","black.","age0_19." , "age20_24." ,"age25_44.","age45_64.","age65p." , "total_pop.", "Pop_Density.")

#identify state-level baseline and time-varying covariates 
makelvar<-function(x) paste0(x, 2007:2017, ".sum")
lsum1<-sapply(baselinenameroot, makelvar)
lsum1v<-as.vector(t(lsum1))
makelvarltp<-function(x) paste0(x, "." ,2007:2017)
lawroots <- c("prop_yr_GSL", "MML_prop_yr_sup", "prop_yr_PMC", "PDMP_new")
lsum2<-sapply(lawroots, makelvarltp)
lsum2v<-as.vector(t(lsum2))
makeloutvarsum<-function(x) paste0(x, 2006:2016, ".sum")
loutsum<-sapply(c("all_od_crude_rate_100k."), makeloutvarsum)
loutvsum<-as.vector(t(loutsum))

#identify county-level baseline and time-varying covariates 
baselinenameroot_cnty <- c(  "pFamInPov","pUnemployed",  "MEDHHINC_10K" ,"pWhite","pBlack","pHisp" ,"PopDens1k","pAge0_19","pAge20_24", "pAge25_44" , "pAge45_64","pAge65p" ,"pMale" )
l<-sapply(baselinenameroot_cnty, makelvarltp)
lv<-as.vector(t(l))
makeloutvar<-function(x) paste0(x, ".", 2006:2016)
lout<-sapply(c("OpioidDeathRate"), makeloutvar)
loutv<-as.vector(t(lout))

timevary <- list(trt = list(
  c(loutvsum[1],lsum1v[1:14], lsum2v[1:4]),
  c(loutvsum[2],lsum1v[15:28], lsum2v[5:8]),
  c(loutvsum[3],lsum1v[29:42], lsum2v[9:12]),
  c(loutvsum[4],lsum1v[43:56], lsum2v[13:16]),
  c(loutvsum[5],lsum1v[57:70], lsum2v[17:20]),
  c(loutvsum[6],lsum1v[71:84], lsum2v[21:24]),
  c(loutvsum[7],lsum1v[85:98], lsum2v[25:28]),
  c(loutvsum[8],lsum1v[99:112], lsum2v[29:32]),
  c(loutvsum[9],lsum1v[113:126], lsum2v[33:36]),
  c(loutvsum[10],lsum1v[127:140], lsum2v[37:40]),
  c(loutvsum[11],lsum1v[141:154], lsum2v[41:44])
  ), outcome= list(
  c(loutv[1],lv[1:13], lsum2v[1:4]),
  c(loutv[2],lv[14:26], lsum2v[5:8]),
  c(loutv[3],lv[27:39], lsum2v[9:12]),
  c(loutv[4],lv[40:52], lsum2v[13:16]),
  c(loutv[5],lv[53:65], lsum2v[17:20]),
  c(loutv[6],lv[66:78], lsum2v[21:24]),
  c(loutv[7],lv[79:91], lsum2v[25:28]),
  c(loutv[8],lv[92:104], lsum2v[29:32]),
  c(loutv[9],lv[105:117], lsum2v[33:36]),
  c(loutv[10],lv[118:130], lsum2v[37:40]),
  c(loutv[11],lv[131:143], lsum2v[41:44])
  ))

y <- names(lmtpdat[, paste0("OpioidDeathRate.", c(2007:2016,2018))])

lmtpdat1<-lmtpdat[,c(a,loutvsum, lsum1v, lsum2v, loutv, lv, "OpioidDeathRate.2018", "geoid")]
lmtpdat2<-lmtpdat1[complete.cases(lmtpdat1),]

library(future)
options(mc.cores=3)
getOption("mc.cores")
plan(multicore)
set.seed(7235)

library(sl3)
library(lmtp)
library(progressr)

lmtpdat2[,loutvsum]<-apply(lmtpdat2[,loutvsum], 2, function(x) as.numeric(x))
lmtpdat3<-lmtpdat2[complete.cases(lmtpdat2),]

#implement shift
latefullup<-lmtpdat3
for(i in 1:nrow(lmtpdat3)){
  for(j in 2:11){

    if(lmtpdat3[i,a[j]]==1 & lmtpdat3[i,a[j-1]]==0 & I(j+1)<13)
   {
    latefullup[i,a[j]]=0
  } 
}}


learners<-make_learner_stack(
  Lrnr_glm_fast, 
  Lrnr_glmnet,
  Lrnr_mean, 
  Lrnr_earth
)

set.seed(7235)
with_progress(psi.tmle.mtp.county.shift<-lmtp_sdr(lmtpdat3, trt = a, outcome="OpioidDeathRate.2018", time_vary=timevary, cens = NULL, shifted=latefullup, shift = NULL, id="geoid" , outcome_type="continuous", learners_outcome = learners, learners_trt = learners, folds = 5, intervention_type = "mtp"))
with_progress(psi.tmle.mtp.county.ref<-lmtp_sdr(lmtpdat3, trt = a, outcome="OpioidDeathRate.2018", time_vary=timevary, cens = NULL, shift = NULL, id="geoid" , outcome_type="continuous", learners_outcome = learners, learners_trt = learners, folds = 5, intervention_type = "mtp"))
