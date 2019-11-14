###########################################################
### Code to replicate analyses for:                     ###
### Rudolph et al., The reletive economy and drug       ###
### overdose deaths. Epidemiology. 2019                 ###
###########################################################

###########################################################
### CODE TO GENERATE RESULTS FOR WHITE MALES, FOR THE   ###
### LONGITUDINAL, 15-YEAR CUMULATIVE ASSOCIATION WITH   ###
### GREATER LABOR FORCE NONPARTICIPATION RATIOS         ###
###########################################################
library(stats)
library(MASS)

#read in data

## functions
changefun<-function(data, var, yearend, yearbegin){
  data[data[,"year"]==yearend, var] - data[data[,"year"]==yearbegin, var] 
}

trncfunc<-function(x){
  pmin(pmax(x, quantile(x, .01, na.rm=TRUE)), quantile(x, .99, na.rm=TRUE))
}

varstotpop<-c("page0_19", "page20_24", "page25_44", "page45_64", "page65p", "lthighschool", "highschool", "aa", "ba", "outoflabor", "earnings")
varswm<-c("wmpage0_19", "wmpage20_24", "wmpage25_44", "wmpage45_64", "wmpage65p", "wmlthighschool", "wmhighschool", "wmaa", "wmba", "wmoutoflabor", "wmearnings")
denom<-lapply(dat[,varstotpop], function(x) 
      ifelse(x==0, 0.01, x))
denom2<-as.data.frame(do.call(cbind, denom))
colnames(denom2)<-colnames(dat[,varstotpop])
ratiodata<-dat[,varswm]/denom2 
colnames(ratiodata)<-c("ratioage0_19", "ratioage20_24", "ratioage25_44", "ratioage45_64", "ratioage65", "ratiolths", "ratiohs", "ratioaa", "ratioba", "oofratio", "earningsratio")

ratiodata2<-as.data.frame(apply(ratiodata, 2, trncfunc))

ratiodata3<-cbind(ratiodata2, dat)

ratiodata3$oofratiocen<-ratiodata3$oofratio - 0.8
ratiodata3$outoflaborcen<-ratiodata3$outoflabor -40

datwide<-reshape(ratiodata3[,c("fips2", "year", "state", "oddeathcount", "oddeathcountwm", "outoflabor", "wmoutoflabor", "oofratiocen", "outoflaborcen", "pmale", "pwhite", "pblack", "wmpage0_19", "wmpage65p", "page0_19", "page65p", "rucc_2013", "wmalepop" , "ratioage0_19", "ratioage20_24", "ratioage25_44", "ratioage45_64", "ratioage65", "ratiolths", "ratiohs", "ratioaa", "ratioba", "earningsratio")], idvar = "fips2", timevar = "year", direction = "wide")

urbdatwide<-datwide[!is.na(datwide$outoflabor.2015) & datwide$outoflabor.2015>33 & datwide$wmalepop.2015>5000,]

y2<-glm.nb(oddeathcountwm.2015~ oofratiocen.2015*outoflaborcen.2015 + pwhite.2015 + pmale.2015 + wmpage0_19.2015 + wmpage65p.2015 +ratioage0_19.2015 + ratioage65.2015+ oddeathcountwm.2009 + oofratiocen.2009*outoflaborcen.2009 + pwhite.2009 + pmale.2009 + wmpage0_19.2009 + wmpage65p.2009 +ratioage0_19.2009 + ratioage65.2009 + oddeathcountwm.2000 + oofratiocen.2000*outoflaborcen.2000 + pwhite.2000 + pmale.2000 + wmpage0_19.2000 + wmpage65p.2000 +ratioage0_19.2000 + ratioage65.2000 + offset(log(wmalepop.2015)), data = urbdatwide)

q2obs<-predict(y2, newdata=urbdatwide, type="response")

datwide111<-urbdatwide
datwide111$oofratiocen.2015<-datwide111$oofratiocen.2015+.05
datwide111$oofratiocen.2009<-datwide111$oofratiocen.2009+.05
datwide111$oofratiocen.2000<-datwide111$oofratiocen.2000+.05

q2a1b1c1<-predict(y2, newdata=datwide111, type="response")

y1<-suppressWarnings(glm.nb(q2obs ~  oofratiocen.2009*outoflaborcen.2009 + pwhite.2009 + pmale.2009 + wmpage0_19.2009 + wmpage65p.2009 +ratioage0_19.2009 + ratioage65.2009 + oddeathcountwm.2000 + oofratiocen.2000*outoflaborcen.2000 + pwhite.2000 + pmale.2000 + wmpage0_19.2000 + wmpage65p.2000 +ratioage0_19.2000 + ratioage65.2000 + offset(log(wmalepop.2009)),  data = urbdatwide))

q1obs<-predict(y1, newdata=urbdatwide, type="response")

y1a1b1c1<-suppressWarnings(glm.nb(q2a1b1c1 ~  oofratiocen.2009*outoflaborcen.2009 + pwhite.2009 + pmale.2009 + wmpage0_19.2009 + wmpage65p.2009 +ratioage0_19.2009 + ratioage65.2009 + oddeathcountwm.2000+ oofratiocen.2000*outoflaborcen.2000 + pwhite.2000 + pmale.2000 + wmpage0_19.2000 + wmpage65p.2000 +ratioage0_19.2000 + ratioage65.2000 + offset(log(wmalepop.2009)),  data = urbdatwide))

q1a1b1c1<-predict(y1a1b1c1, newdata=datwide111, type="response")

y0<-suppressWarnings(glm.nb(q1obs ~  oofratiocen.2000*outoflaborcen.2000 + pwhite.2000 + pmale.2000 + wmpage0_19.2000 + wmpage65p.2000 +ratioage0_19.2000 + ratioage65.2000 + offset(log(wmalepop.2000)),  data = urbdatwide))

q0obs<-predict(y0, newdata=urbdatwide, type="response")

y0a1b1c1<-suppressWarnings(glm.nb(q1a1b1c1 ~ oofratiocen.2000*outoflaborcen.2000 + pwhite.2000 + pmale.2000 + wmpage0_19.2000 + wmpage65p.2000 +ratioage0_19.2000 + ratioage65.2000 + offset(log(wmalepop.2000)),  data = urbdatwide))

q0a1b1c1<-predict(y0a1b1c1, newdata=datwide111, type="response")

#long-term effect
cumeffect<-q0a1b1c1 - q0obs
