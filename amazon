devtools::install_github("ebenmichael/augsynth")
library(augsynth)
library(tidyverse)
library(vroom)

####################################
#### COMMUTING ZONE PRIMARY ANALYSIS
####################################

#read in data              
czdat<-read.csv("commuting_zone_with_county_stats_weightedmean_pop_up.csv")

#scale covariates
disabscaled_cz<- czdat %>% mutate_at(covar, ~(scale(.) %>% as.vector))

#using lasso to identify covariates
x<-model.matrix(~ p_male + p_white + p_black + p_hisp + p_age5_17 + p_age18_24 + p_age25_34 + p_age35_49 + p_age50_64+ p_age65p + lths + hs+ collplus+ p_est_not_labor + p_unemp + p_est_ind_manufact + p_est_nev_married + p_est_married + p_est_vet_status_civilian + med_earn + density + abs_up_mobility, disabscaled_cz)
              
library(glmnet)
cvfit<-cv.glmnet(x[,-1], disabscaled_cz$tot_denials)
              
nonzerocovar<- c("p_male", "p_white", "p_hisp", "p_age5_17",  "p_age25_34", "p_age50_64", "p_age65p", "lths", "hs", "collplus", "p_unemp", "p_est_ind_manufact", "p_est_nev_married", "p_est_married", "p_est_vet_status_civilian", "med_earn",  "abs_up_mobility")
              
## applications              
disabscaled_cz$logapplications<-log(disabscaled_cz$tot_applications)
formcov <- as.formula(glue::glue("logapplications ~ has_fc | {paste0(nonzerocovar, collapse = ' + ')}"))
              
nevertreated<- disabscaled_cz %>% group_by(czone) %>% filter(sum(has_fc)==0)
evertreated<- disabscaled_cz %>% group_by(czone) %>% filter(has_fc>0)
disabscaled_cz$everfc <- ifelse(disabscaled_cz$czone %in% evertreated$czone, 1, 0)

applicationsfit_cz <- multisynth(
                              formcov, czone, year, disabscaled_cz[#!disabscaled_cz$czone %in% c(158) & 
                                disabscaled_cz$tot_applications<100,], 
                              fixedeff = TRUE, 
                              time_cohort = TRUE,
                              lambda=.5,
                              n_leads = 3 
                              #n_lags = 10
                            )
                            
summary(applicationsfit_cz)
                              
plot(applicationsfit_cz, levels = "Average")

## denials       
disabscaled_cz$logdenials<-log(disabscaled_cz$tot_denials)
formcov <- as.formula(glue::glue("logdenials ~ has_fc | {paste0(nonzerocovar, collapse = ' + ')}"))
                            
denialsfit_cz <- multisynth(
                              formcov, czone, year, disabscaled_cz[disabscaled_cz$tot_applications<100, ], 
                              fixedeff = TRUE, 
                              time_cohort = TRUE,
                              lambda=.5,
                              n_leads = 3 
                              #n_lags = 10
                            )

summary(denialsfit_cz)

plot(denialsfit_cz, levels = "Average")

## allowances
disabscaled_cz$logallowances<-log(disabscaled_cz$tot_allowances)
formcov <- as.formula(glue::glue("logallowances ~ has_fc | {paste0(nonzerocovar, collapse = ' + ')}"))
                            
allowancesfit_cz <- multisynth(
                              formcov, czone, year, disabscaled_cz[disabscaled_cz$tot_applications<100, ], 
                              fixedeff = TRUE, 
                              time_cohort = TRUE,
                              lambda=.5,
                              n_leads = 3 
                              #n_lags = 10
                            )
                            

summary(allowancesfit_cz)

plot(allowancesfit_cz, levels = "Average")

### sensitivity analysis 1: effects by year

#run the following for each of the above code, for each year
print(allowancesfit_cz, level = 2006)

### sensitivity analysis 2: different pooling parameters
#repeat the above code but change the value of lambda

### sensitivity analysis 3: placebo check
formcovfake <- as.formula(glue::glue("logapplications ~ fake_has_fc | {paste0(nonzerocovar, collapse = ' + ')}"))

applicationsfitsens3_cv <- multisynth(
  formcovfake, czone, year, disabscaled_cz[!disabscaled_cz$czone %in% c(26, 72, 158
  ) & disabscaled_cz$tot_applications<100,], 
  fixedeff = TRUE, 
  time_cohort = TRUE,
  lambda=.5,
  n_leads = 3 
  #n_lags = 10
)

#note that this will be exactly 0 if lambda=1
summary(applicationsfitsens3_cv)
plot(applicationsfitsens3_cv, levels = "Average")

###sensitivity analysis 4: county level
#scale covariates
disabscaled<- disabnonmisout %>% mutate_at(covar, ~(scale(.) %>% as.vector))

#identify covariates that matter
x<-model.matrix(~ p_male + p_white + p_black + p_hisp + p_age5_17 + p_age18_24 + p_age25_34 + p_age35_49 + p_age50_64+ p_age65p + lths + hs+ collplus+ p_est_not_labor + p_unemp + p_est_ind_manufact + p_est_nev_married + p_est_married + p_est_vet_status_civilian + med_earn + density + abs_up_mobility, disabscaled_cz)
cvfit<-cv.glmnet(x[,-1], disabscaled_cz$tot_denials)
coef(cvfit, s=cvfit$lambda.1se)

nonzerocovar<- c("p_male", "p_white", "p_hisp", "p_age5_17",  "p_age25_34", "p_age50_64", "p_age65p", "lths", "hs", "collplus", "p_unemp", "p_est_ind_manufact", "p_est_nev_married", "p_est_married", "p_est_vet_status_civilian", "med_earn",  "abs_up_mobility")

disabscaled$logapplications<-log(disabscaled$tot_applications)

formcov <- as.formula(glue::glue("logapplications ~ has_fc | {paste0(nonzerocovar, collapse = ' + ')}"))

applicationsfit <- multisynth(
  formcov, geoid2, year, disabscaled[disabscaled$geoid2 !=24510 & disabscaled$tot_applications<100,], 
  fixedeff = TRUE, 
  time_cohort = TRUE,
  lambda=.5,
  n_leads = 3 
  #n_lags = 10
)

summary(applicationsfit)
plot(applicationsfit, levels = "Average")

disabscaled$logdenials<-log(disabscaled$tot_denials)

formcov <- as.formula(glue::glue("logdenials ~ has_fc | {paste0(nonzerocovar, collapse = ' + ')}"))

denialsfit <- multisynth(
  formcov, geoid2, year, disabscaled[disabscaled$geoid2 !=24510 & disabscaled$tot_denials<50,], 
  fixedeff = TRUE, 
  time_cohort = TRUE,
  lambda=.5,
  n_leads = 3 
  #n_lags = 10
)

summary(denialsfit)
plot(denialsfit, levels = "Average")

disabscaled$logallowances<-log(disabscaled$tot_allowances)

formcov <- as.formula(glue::glue("logallowances ~ has_fc | {paste0(nonzerocovar, collapse = ' + ')}"))

allowancesfit <- multisynth(
  formcov, geoid2, year, disabscaled[disabscaled$geoid2 !=24510 & disabscaled$tot_allowances<100,], 
  fixedeff = TRUE, 
  time_cohort = TRUE,
  lambda=.5,
  n_leads = 3 
  #n_lags = 10
)

summary(allowancesfit)
plot(allowancesfit, levels = "Average")


###sensitivity analysis 5: SSDI current beneficiaries

setwd("~/Documents/amazon")
cztofips<-read.csv("cz00_fips.csv")[,c(1,2)]
names(cztofips)<-c("geoid2", "czone")
tmp1<-merge(cztofips, disabscaled[,c(1,2,4:7)], by="geoid2", all.x=FALSE, all.y=TRUE)
tmp1[,c(5:7)]<-lapply(tmp1[,c(5:7)], function(x) x/1000 * tmp1$pop12p)
cztmp<-tmp1 %>% group_by(czone, year) %>% summarise(pop12p = sum(pop12p), tot_applications=sum(tot_applications), tot_allowances=sum(tot_allowances), tot_denials=sum(tot_denials))
cztmp2<-merge(cztmp, disabscaled_cz, by=c("czone", "year"), all.x=FALSE, all.y=TRUE)
datoth<-read.csv("disability_webscrape_commuting_zone.csv")
cztmp3<-merge(cztmp2, datoth, by=c("czone", "year"), all=FALSE)

#per100
cztmp3$logdisabledworkerrate<-log(cztmp3$disabled_workers/cztmp3$pop12p*100)
formcov <- as.formula(glue::glue("logdisabledworkerrate ~ has_fc | {paste0(nonzerocovar, collapse = ' + ')}"))

currentbenefit_cz <- multisynth(
  formcov, czone, year, cztmp3,
  fixedeff = TRUE, 
  time_cohort = TRUE,
  lambda=1,
  n_leads = 3 
  #n_lags = 10
)

summary(currentbenefit_cz)
plot(currentbenefit_cz, levels = "Average")

#placebo check for this outcome
formcovfake <- as.formula(glue::glue("logdisabledworkerrate ~ fake_has_fc | {paste0(nonzerocovar, collapse = ' + ')}"))

currentbenefitsens3_cv <- multisynth(
  formcovfake, czone, year,cztmp3[!cztmp3$czone %in% c(26, 72, 158  ),],
  fixedeff = TRUE, 
  time_cohort = TRUE,
  lambda=.5,
  n_leads = 3 
  #n_lags = 10
)

#note that this will be exactly 0 if lambda=1
summary(currentbenefitsens3_cv)
plot(currentbenefitsens3_cv, levels = "Average")

### sensitivity analysis 6: CZs ever having an FC opening

fulldis <- readRDS("~/Documents/amazon/fc_all.rds")

fulldis <- fulldis |>
  rename(fips = county) |>
  mutate(fips = str_remove(fips, "^0+"))

library(readxl)
czone <- read_excel("~/Downloads/commuting_zones.xls")
czone <- czone[,c(1,2)]
names(czone)<-c("fips", "czone")
fulldis <- fulldis |>
  rename(fips = county) 
fulldis1<- inner_join(czone, fulldis, by="fips")

disabscaled_cz$everfc[disabscaled_cz$czone %in% fulldis1$czone]<-1

disabscaled_cz$logapplications<-log(disabscaled_cz$tot_applications)
formcov <- as.formula(glue::glue("logapplications ~ has_fc | {paste0(nonzerocovar, collapse = ' + ')}"))

applicationsfit_cz_fc <- multisynth(
  formcov, czone, year, disabscaled_cz[disabscaled_cz$everfc==1 & 
                                         disabscaled_cz$tot_applications<100,], 
  fixedeff = TRUE, 
  time_cohort = TRUE,
  lambda=.1,
  n_leads = 3 
  #n_lags = 10
)

summary(applicationsfit_cz_fc)
plot(applicationsfit_cz_fc, levels = "Average")

disabscaled_cz$logdenials<-log(disabscaled_cz$tot_denials)
formcov <- as.formula(glue::glue("logdenials ~ has_fc | {paste0(nonzerocovar, collapse = ' + ')}"))

denialsfit_cz_fc <- multisynth(
  formcov, czone, year, disabscaled_cz[disabscaled_cz$everfc==1 & disabscaled_cz$tot_denials<100,], 
  fixedeff = TRUE, 
  time_cohort = TRUE,
  lambda=.1,
  n_leads = 3 
  #n_lags = 10
)

summary(denialsfit_cz_fc)

plot(denialsfit_cz_fc, levels = "Average")

disabscaled_cz$logallowances<-log(disabscaled_cz$tot_allowances)
formcov <- as.formula(glue::glue("logallowances ~ has_fc | {paste0(nonzerocovar, collapse = ' + ')}"))

allowancesfit_cz_fc <- multisynth(
  formcov, czone, year, disabscaled_cz[disabscaled_cz$everfc==1 & disabscaled_cz$tot_allowances<100,], 
  fixedeff = TRUE, 
  time_cohort = TRUE,
  lambda=.1,
  n_leads = 3 
  #n_lags = 10
)

summary(allowancesfit_cz_fc)
plot(allowancesfit_cz_fc, levels = "Average")


### sensitivity analysis 7: DID estimate

library(did)
diddat <-disabscaled_cz[,1:33] %>% 
  group_by(czone) %>%
  filter(has_fc==1) %>%
  mutate(first_treated = min(year))
diddat2<- left_join(disabscaled_cz[disabscaled_cz$czone %in% diddat$czone,1:33], unique(diddat[,c("czone", "first_treated")]), by="czone")
disabscaled_cz$first_treated<-0
diddat3 <-rbind(as.data.frame(diddat2), as.data.frame(disabscaled_cz[!disabscaled_cz$czone %in% diddat$czone,]))

applications_attgt <- att_gt(yname = "logapplications",
                        tname = "year",
                        idname = "czone",
                        gname = "first_treated",
                        xformla = ~ p_male  + p_hisp + p_age65p + lths+ med_earn + density  + abs_up_mobility +
                          p_est_vet_status_civilian ,
                        # xformla = ~ 1,
                        data = diddat3
)

summary(applications_attgt)

agg.es <- aggte(applications_attgt, type = "dynamic")
summary(agg.es)

ggdid(agg.es)


app.dyn.balance <- aggte(applications_attgt, type = "dynamic", balance_e=2)
summary(app.dyn.balance)

ggdid(app.dyn.balance)


denials_attgt <- att_gt(yname = "logdenials",
                        tname = "year",
                        idname = "czone",
                        gname = "first_treated",
                        xformla = ~ p_male  + p_hisp + p_age65p + lths+ med_earn + density  + abs_up_mobility +
                          p_est_vet_status_civilian,
                        data = diddat3
)

agg.denials <- aggte(denials_attgt, type = "dynamic")
summary(agg.denials)
ggdid(agg.denials)


denials.balance <- aggte(denials_attgt, type = "dynamic", balance_e=2)
summary(denials.balance)
ggdid(denials.balance)

allowances_attgt <- att_gt(yname = "logallowances",
                           tname = "year",
                           idname = "czone",
                           gname = "first_treated",
                           xformla = ~ p_male  + p_hisp + p_age65p + lths+ med_earn + density  + abs_up_mobility +
                             p_est_vet_status_civilian,
                           data = diddat3
)

agg.allow <- aggte(allowances_attgt, type = "dynamic")
summary(agg.allow)
ggdid(agg.allow)


allowances.balance <- aggte(allowances_attgt, type = "dynamic", balance_e=2)
summary(allowances.balance)
ggdid(allowances.balance)
