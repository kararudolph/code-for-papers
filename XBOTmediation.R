library(dplyr)
library(data.table)
library(SuperLearner)
#the following files are found in this same repo
#estim.r is the function that returns the direct and indirect effect estimates
source('slfunctions.r')
source('estim.r')

#read in imputed datasets
testdat1<-read.csv("testdat1.csv")
testdat2<-read.csv("testdat2.csv")
testdat3<-read.csv("testdat3.csv")
testdat4<-read.csv("testdat4.csv")
testdat5<-read.csv("testdat5.csv")
testdat6<-read.csv("testdat6.csv")
testdat7<-read.csv("testdat7.csv")
testdat8<-read.csv("testdat8.csv")
testdat9<-read.csv("testdat9.csv")
testdat10<-read.csv("testdat10.csv")
testdat<-list(testdat1, testdat2, testdat3, testdat4, testdat5, testdat6, testdat7, testdat8, testdat9, testdat10)

homelesseffects<-nonhomelesseffects<-rep( list(list()), 10 )

#apply function on all datasests
#files sourced above are found in same repo
for(i in 1:10){

tdat2<-testdat[[i]] %>% dplyr:::select(-M4, -M5)
homelessdat<-subset(tdat2, homeless==1)[,-1]
nonhomelessdat<-subset(tdat2, homeless==0)[,-1]

#estimate direct and indirect effects
#using the lite version of the package
#candidates <- c('SL.glm', 'SL.mean')

candidates <- c('SL.glm', 'SL.caretRF', 'SL.caretXGB', 'SL.glmnet', 'SL.earth')

homelesseffects[[i]] <- mediation(homelessdat, weights=rep(1,nrow(homelessdat)),
                     candidatesg = candidates,
                     candidatese = candidates,
                     candidatesm = candidates,
                     candidatesr = candidates,
                     candidatesq = candidates,
                     candidatesu = candidates,
                     candidatesv = candidates,
                     nfolds = 3,
                     family.outcome =binomial())
 rm(homelessdat)

#run for nonhomeless
nonhomelesseffects[[i]] <- mediation(nonhomelessdat, weights=rep(1,nrow(nonhomelessdat)),
                     candidatesg = candidates,
                     candidatese = candidates,
                     candidatesm = candidates,
                     candidatesr = candidates,
                     candidatesq = candidates,
                     candidatesu = candidates,
                     candidatesv = candidates,
                     nfolds = 3,
                     family.outcome =binomial())

rm(nonhomelessdat)

}

#collect results in one dataframe
grphdat<-as.data.frame(matrix(rep(0,8*4), ncol=4))
#indirect effect
#os
grphdat[1,]<-summary(MIcombine(results=lapply(homelesseffects, function(x) x$effects[1,1]), variances=lapply(homelesseffects, function(x) (x$ses[1,1])^2)))
#tmle
grphdat[2,]<-summary(MIcombine(results=lapply(homelesseffects, function(x) x$effects[1,2]), variances=lapply(homelesseffects, function(x) (x$ses[1,2])^2)))

#direct
grphdat[3,]<-summary(MIcombine(results=lapply(homelesseffects, function(x) x$effects[2,1]), variances=lapply(homelesseffects, function(x) (x$ses[2,1])^2)))

grphdat[4,]<-summary(MIcombine(results=lapply(homelesseffects, function(x) x$effects[2,2]), variances=lapply(homelesseffects, function(x) (x$ses[2,2])^2)))

#indirect
grphdat[5,]<-summary(MIcombine(results=lapply(nonhomelesseffects, function(x) x$effects[1,1]), variances=lapply(nonhomelesseffects, function(x) (x$ses[1,1])^2)))

#direct
grphdat[6,]<-summary(MIcombine(results=lapply(nonhomelesseffects, function(x) x$effects[1,2]), variances=lapply(nonhomelesseffects, function(x) (x$ses[1,2])^2)))

#indirect
grphdat[7,]<-summary(MIcombine(results=lapply(nonhomelesseffects, function(x) x$effects[2,1]), variances=lapply(nonhomelesseffects, function(x) (x$ses[2,1])^2)))

#direct
grphdat[8,]<-summary(MIcombine(results=lapply(nonhomelesseffects, function(x) x$effects[2,2]), variances=lapply(nonhomelesseffects, function(x) (x$ses[2,2])^2)))
