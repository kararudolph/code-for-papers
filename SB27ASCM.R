library(tidyverse)

dat<-dat%>% arrange(year,month)
dat$cummonth<-dat$month+((dat$year-2013)*12)
dat$numstate<-as.numeric(as.factor(dat$state))

devtools::install_github("ebenmichael/augsynth")
library(augsynth)
library(magrittr)
library(dplyr)
library(data.table)

subdat<-data.table(dat) %>% group_by(numstate, cummonth, mdr_category)%>% summarize(orgs_tested_new=sum(orgs_tested), orgs_ns_new=sum(orgs_ns), white=first(nhw), black=first(nhb), asian=first(nhasian), hispanic=first(hispanic), 
                                                                                    agecat0_14=first(age_0_14), agecat15_24=first(age_15_24), agecat25_44=first(age_25_44), agecat45_64=first(age_45_64), 
                                                                                    agecat65_84=first(age_65_84), agecat85plus=first(age_85plus), female=first(propfemale), CAFOs=first(CAFOs), poultry=first(Poultry), 
                                                                                    Slaughterhouse=first(Slaughterhouse), type2dm=first(proptype2diabet), urban=first(urban), X100_300=first(X100_300), gt_300=first(gt_300), 
                                                                                    limited=first(limited), major=first(major), no_affiliation=first(no_affiliation), graduate=first(graduate))

subdat<-as.data.frame(subdat)
subdat$outcome[subdat$orgs_tested==0]<-0
subdat$outcome[subdat$orgs_tested>0]<-subdat[subdat$orgs_tested>0,]$orgs_ns/subdat[subdat$orgs_tested>0,]$orgs_tested

#uncomment the outcome of interest
#subdat_quin<-subdat[subdat$mdr_category=="amino_ir",]
subdat_quin<-subdat[subdat$mdr_category=="esc4_ir",]
#subdat_quin<-subdat[subdat$mdr_category=="quin_ir",]
#subdat_quin<-subdat[subdat$mdr_category=="tetracycl_ir",]

#cut out pandemic time and time that is too early
moddat<-subdat_quin[subdat_quin$cummonth<88 & subdat_quin$cummonth>18,]
moddat$trt<-ifelse(moddat$numstate==3 & moddat$cummonth>60,1,0)

covasyn <- augsynth(outcome ~ trt |  white + hispanic + agecat15_24 +  agecat45_64 + agecat65_84 + agecat85plus + female
                    + log(Slaughterhouse) + type2dm + urban +  gt_300 +  major + graduate, numstate, cummonth, data=moddat, progfunc="ridge", scm=T, fixedeff=T)

summary(covasyn, stat_func=function(x) abs(sum(x)))
plot(covasyn)
