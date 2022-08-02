###########################
### CLEAN DATA#############
###########################

#trial data
baselinedatup<-read.csv("baselinedatup.csv")[,-1]

wnames<-c("project", "site", "age", "sex","isHispanic", "xrace", "hasBipolar", "hasSchiz", "hasMajorDep", "hasAnxPan", "hasBrainDamage", "hasEpilepsy", "hcows", "ivdrug", "edu", "mar", "bamphetamine30_base", "bcannabis30_base", "bbenzo30_base", "falcohol", "fdrug", "alcdisorder", "cocdisorder", "trt")

dat<-baselinedatup[,c("who", wnames)]

#teds data
tedsdat<-read.csv("tedsa_puf_2015_2017.csv")[,-1]
teds <- subset(tedsdat, select = c(CASEID, GENDER, AGE, ETHNIC, RACE, EDUC, PREG, MARSTAT, METHUSE, 
	# so limit to METHUSE=1
	SUB1, SUB2, SUB3,
	# to limit to 5,6,7,
	ROUTE1, ROUTE2, ROUTE3, 
	#value of 4 is injection drug use
	#should concur with IDU=1
	FREQ1, FREQ2, FREQ3,
	# amount of use in the past month so limit to values 2,3 (nonzero use)
    IDU,
    #ALCFLG, COKEFLG, 
    #MARFLG, 
    #aligned with bcannabis30_base
    HERFLG, METHFLG,OPSYNFLG,
    #the above 3 should be the same as OUD
	#MTHAMFLG, AMPHFLG,
	#the above 2 are amphetamine use
	#aligned with bamphetamine30_base
	#BENZFLG,
	#alligned with  bbenzo30_base
	DSMCRIT))
	#==1 | 4 | 9 aligned with alcdisorder
	#==6 | 13 aligned with cocdisorder
	#== 16 aligned with has Schiz
	#== 15 aligned with hasMajorDep
	#== 17 aligned with hasBipolar
	#== 14 aligned with hasAnxPan
	#==5 | 12 means has OUD
                                              

#subset so that it's only participants who will receive MAT
teds <- teds[teds$METHUSE == 1, ]

#AMPHETAMINES (amph)
#set new column to 99 
teds$bamphetamine30_base <- 99
#primary, secondary or tertiary substance is amph and has been used in past month is coded as 1 
teds$bamphetamine30_base[(teds$SUB1 %in% c(10,11) & teds$FREQ1 != 1) | (teds$SUB2 %in% c(10,11) & teds$FREQ2!=1) | (teds$SUB3 %in% c(10,11) & teds$FREQ3!=1)] <- 1
teds$bamphetamine30_base[(teds$SUB1 %in% c(10,11) & teds$FREQ1==1) | (teds$SUB2 %in% c(10,11) & teds$FREQ2==1) | (teds$SUB3 %in% c(10,11) & teds$FREQ3==1)] <- 0
teds$bamphetamine30_base[!teds$SUB1 %in% c(10,11) & !teds$SUB2 %in% c(10,11) & !teds$SUB3 %in% c(10,11)] <- 0
teds$bamphetamine30_base[teds$SUB1== -9 & teds$SUB2== -9 & teds$SUB3== -9] <- NA

teds$bbenzo30_base <- 99
teds$bbenzo30_base[(teds$SUB1==13 & teds$FREQ1!= 1) | (teds$SUB2==13 & teds$FREQ2!=1) | (teds$SUB3==13 & teds$FREQ3!=1)] <- 1
#teds$bbenzo30_base[(teds$SUB1==13 & teds$FREQ1!= 1) | (teds$SUB==13 & teds$FREQ2!=1) | (teds$SUB3==13 & teds$FREQ3!=1)] <- 1
teds$bbenzo30_base[(teds$SUB1==13 & teds$FREQ1==1) | (teds$SUB2==13 & teds$FREQ2==1) | (teds$SUB3==13 & teds$FREQ3==1)] <- 0
teds$bbenzo30_base[teds$SUB1!=13 & teds$SUB2!=13 & teds$SUB3!=13] <- 0
teds$bbenzo30_base[teds$SUB1== -9 & teds$SUB2== -9 & teds$SUB3== -9] <- NA

teds$bcannabis30_base <- 99
teds$bcannabis30_base[(teds$SUB1==4 & teds$FREQ1 != 1) | (teds$SUB2==4 & teds$FREQ2!=1) | (teds$SUB3==4 & teds$FREQ3!=1)] <- 1
teds$bcannabis30_base[(teds$SUB1==4 & teds$FREQ1==1) | (teds$SUB2==4 & teds$FREQ2==1) | (teds$SUB3==4 & teds$FREQ3==1)] <- 0
teds$bcannabis30_base[teds$SUB1!=4 & teds$SUB2!=4 & teds$SUB3!=4] <- 0
teds$bcannabis30_base[teds$SUB1== -9 & teds$SUB2== -9 & teds$SUB3== -9] <- NA

teds$sex <- 0
#anyone who is coded as male in GENDER is coded as 1
teds$sex[teds$GENDER == 1] <- 1
#anyone who is missing in GENDER is coded as NA
teds$sex[teds$GENDER == -9] <- NA

#1=married, 2=div/sep/widow, 3=single
teds$mar <- 99
#code married observations as 1
teds$mar[teds$MARSTAT == 2] <- 1
#coding missing values as NA
teds$mar[teds$MARSTAT == -9] <- NA
teds$mar[teds$MARSTAT %in% c(3,4)] <- 2
teds$mar[teds$MARSTAT == 1] <- 3

teds$edu<-99
teds$edu[teds$EDUC %in% c(1,2)] <- 1
teds$edu[teds$EDUC==3] <- 2
teds$edu[teds$EDUC %in% c(4,5)] <- 3
teds$edu[teds$EDUC == -9] <- NA

teds$xrace<-99
teds$xrace[teds$RACE == 5 & teds$ETHNIC %in% c(4,-9)] <- 1
teds$xrace[teds$RACE == 4 & teds$ETHNIC %in% c(4,-9)] <- 2
teds$xrace[teds$ETHNIC %in% c(1,2,3,5)] <- 3
teds$xrace[teds$RACE %in% c(1,2,3,6,7,8,9) & teds$ETHNIC %in% c(4,-9)] <- 4
#code those who are missing both race and ethnicity as NA
teds$xrace[teds$RACE == -9 & teds$ETHNIC == -9] <- NA
teds$xrace[teds$xrace == 99] <- NA

teds$ivdrug1 <- teds$IDU
teds$ivdrug1[teds$ivdrug1 == -9] <- NA
teds$ivdrug2<-ifelse(teds$ROUTE1==4 | teds$ROUTE2==4 | teds$ROUTE3==4, 1, 0)
teds$ivdrug<-0
teds$ivdrug[is.na(teds$ivdrug1) & teds$ivdrug1==1 | teds$ivdrug2==1]<-1

teds$age<-teds$AGE

teds$hasOUD1 <- ifelse(teds$DSMCRIT %in% c(5,12), 1, 0)
teds$hasOUD2 <- ifelse(teds$HERFLG==1 | teds$METHFLG==1 | teds$OPSYNFLG==1, 1, 0)

#limit to those either with DSMOUD or with illicit opioid usein the past month
teds<-teds[teds$hasOUD1==1 | teds$hasOUD2==1, ]

teds$trialdata<-0

teds$PREG[teds$PREG== -9]<- NA
teds$PREG[teds$PREG== 2]<- 0
teds$PREG<-ifelse(!is.na(teds$sex) & teds$sex==1, 0,  teds$PREG)
teds$PREG<-as.factor(teds$PREG)
teds_final <- subset(teds, select = c(CASEID, sex, age, xrace, edu, mar, PREG, bamphetamine30_base, bbenzo30_base, bcannabis30_base, ivdrug, trialdata))
#teds_final <- subset(teds, select = c(CASEID, sex, age, xrace, edu, mar, PREG, bamphetamine30_base, bbenzo30_base, bcannabis30_base, ivdrug, trialdata, RACE))

trial <- subset(dat, select = c(who, age, sex, xrace, ivdrug, edu, mar,bamphetamine30_base, bcannabis30_base, bbenzo30_base))

trial$PREG<-0
trial$trialdata<-1
trial$ivdrug[trial$ivdrug == -9] <- NA
trial$bamphetamine30_base[trial$bamphetamine30_base == -9] <- NA
trial$bbenzo30_base[trial$bbenzo30_base == -9] <- NA
trial$bcannabis30_base[trial$bcannabis30_base == -9] <- NA

#convert female to 0 so sex is a binary column
trial$sex[trial$sex == 2] <- 0 

trial$edu[trial$edu == -9] <- NA
trial$mar[trial$mar < 0] <- NA

#create age categories
trial$age1<-0
trial$age1[trial$age< 15]<-1
trial$age1[trial$age>14 & trial$age<18]<-2
trial$age1[trial$age>17 & trial$age<21]<-3
trial$age1[trial$age>20 & trial$age<25]<-4
trial$age1[trial$age>24 & trial$age<30]<-5
trial$age1[trial$age>29 & trial$age<35]<-6
trial$age1[trial$age>34 & trial$age<40]<-7
trial$age1[trial$age>39 & trial$age<45]<-8
trial$age1[trial$age>44 & trial$age<50]<-9
trial$age1[trial$age>49 & trial$age<55]<-10
trial$age1[trial$age>54 & trial$age<65]<-11
trial$age1[trial$age>64]<-12
trial$age<-trial$age1

stacked<-rbind(trial[,c("sex","age" , "xrace","edu", "mar"  , "PREG" , "bamphetamine30_base", "bbenzo30_base" ,"bcannabis30_base","ivdrug","trialdata" )], teds_final[,c("sex","age" , "xrace","edu", "mar"  , "PREG" , "bamphetamine30_base", "bbenzo30_base" ,"bcannabis30_base","ivdrug","trialdata") ])

stacked$sex<-as.factor(stacked$sex)
stacked$PREG<-as.factor(stacked$PREG)
stacked$xrace<-as.factor(stacked$xrace)
stacked$mar<-as.factor(stacked$mar)
stacked$mar<-droplevels(stacked$mar)
stacked$edu <-factor(stacked$edu, ordered = TRUE, levels = c("1", "2", "3"))
stacked$bamphetamine30_base<-as.factor(stacked$bamphetamine30_base)
stacked$bbenzo30_base<-as.factor(stacked$bbenzo30_base)
stacked$bcannabis30_base[stacked$bcannabis30_base==99]<-NA
stacked$bcannabis30_base<-as.factor(stacked$bcannabis30_base)
stacked$ivdrug<-as.factor(stacked$ivdrug)

###########################
### IMPUTE ################
###########################
library(mice)
pred_teds <- quickpred(stacked, mincor = 0.01)
ini_teds <- mice(stacked, maxit = 0, pri = F)
meth_teds <- ini_teds$meth
meth_teds = c("logreg", "", "polyreg", "polr", "polyreg", "logreg", "logreg", "logreg", "logreg", "logreg",  "")
meth_trials = c("", "", "", "polr", "polyreg", "", "logreg", "logreg", "logreg", "logreg",  "")

imp_dat_trial <- mice(stacked[stacked$trialdata==1,], pred = pred_teds, meth = meth_trials, maxit = 10, m = 5, seed = 92385)
imp_list_trial <- complete(imp_dat_trial, "all")

imp_dat_teds <- mice(stacked[stacked$trialdata==0,], pred = pred_teds, meth = meth_teds, maxit = 10, m = 5, seed = 92385)
imp_list_teds <- complete(imp_dat_teds, "all")

stacked_list <- list()
for (i in 1:5) {
  stacked_list[[i]] <- rbind(imp_list_teds[[i]], imp_list_trial[[i]])
}

load("stacked_list.RData")

# make larger age categories -- <18, 18-35, 35-50, 50-64, 65+

for(i in 1:5){
	stacked_list[[i]]$agecat<-9
	stacked_list[[i]][stacked_list[[i]]$age %in% c(1,2),]$agecat<-0
	stacked_list[[i]][stacked_list[[i]]$age %in% c(3:6),]$agecat<-1
	stacked_list[[i]][stacked_list[[i]]$age %in% c(7:9),]$agecat<-2
	stacked_list[[i]][stacked_list[[i]]$age %in% c(10,11),]$agecat<-3
	stacked_list[[i]][stacked_list[[i]]$age ==12,]$agecat<-4
	factor(stacked_list[[i]]$agecat, ordered = TRUE, levels = c("0", "1", "2", "3", "4"))
}


for(i in 1:5){
	stacked_list[[i]]$CASEID<-c(trial$who, teds_final$CASEID)
}


popdata<-stacked_list[[1]][stacked_list[[1]]$trialdata==0,]
samppopdata<-popdata[sample(nrow(popdata), 20000),] 
  
newdat<-as.data.frame(rbind(stacked_list[[1]][stacked_list[[1]]$trialdata==1,], samppopdata))

library(dplyr)

  library(rpart.plot)
# plot tree
only_count <- function(x, labs, digits, varlen)
{
  paste(x$frame$n)
}


###########################
### ANALYZE DATA ##########
###########################

represented<-notrepresented<-underrepresented<-representednopreg<-notrepresentednopreg<-underrepresentednopreg<-representednomar<-notrepresentednomar<-representednomarnoedu<-notrepresentednomarnoedu<-representednopregnomarnoedu<-notrepresentednopregnomarnoedu<-underrepresentednopregnomarnoedu<-list()
#gives subset of teds that is seen in trials
for(i in 1:5){
	print(i)
#i<-1
	represented[[i]]<-dplyr::semi_join(stacked_list[[i]][stacked_list[[i]]$trialdata==0, c("sex","xrace" ,"edu","mar" , "PREG","bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )], stacked_list[[i]][stacked_list[[i]]$trialdata==1, c("sex","xrace" ,"edu","mar" , "PREG","bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )])
	notrepresented[[i]]<-dplyr::anti_join(stacked_list[[i]][stacked_list[[i]]$trialdata==0, c("sex","xrace" ,"edu","mar" , "PREG","bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )], stacked_list[[i]][stacked_list[[i]]$trialdata==1,c("sex","xrace" ,"edu","mar" , "PREG","bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )])
	print(nrow(notrepresented[[i]])/nrow(stacked_list[[i]][stacked_list[[i]]$trialdata==0,]))

	#count number of trial participants in each represented subgroup
noduprep<-represented[[i]] %>% distinct
trialrep<-dplyr::left_join(noduprep, stacked_list[[i]][stacked_list[[i]]$trialdata==1,c("sex","xrace" ,"edu","mar" , "PREG","bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )] )

tallyd<-trialrep%>% 
 group_by(sex, xrace , edu, mar , PREG,bamphetamine30_base ,bbenzo30_base, bcannabis30_base ,ivdrug,agecat) %>%
 tally()  %>% arrange(desc(n)) %>% filter(n<5)

	#if number <5 ,move to not represented
newnonrep<-dplyr::left_join(tallyd[,-11], represented[[i]])
underrepresented[[i]]<-rbind(notrepresented[[i]], newnonrep)

noduprep<-trialrep<-tallyd<-newnonrep<-NULL

	representednopreg[[i]]<-dplyr::semi_join(stacked_list[[i]][stacked_list[[i]]$trialdata==0, c("sex","xrace" ,"edu","mar" , "bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )], stacked_list[[i]][stacked_list[[i]]$trialdata==1, c("sex","xrace" ,"edu","mar" , "bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )])
	notrepresentednopreg[[i]]<-dplyr::anti_join(stacked_list[[i]][stacked_list[[i]]$trialdata==0, c("sex","xrace" ,"edu","mar" , "bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )], stacked_list[[i]][stacked_list[[i]]$trialdata==1,c("sex","xrace" ,"edu","mar" , "bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )])
	print(nrow(notrepresentednopreg[[i]])/nrow(stacked_list[[i]][stacked_list[[i]]$trialdata==0,]))

noduprep<-representednopreg[[i]] %>% distinct
trialrep<-dplyr::left_join(noduprep, stacked_list[[i]][stacked_list[[i]]$trialdata==1,c("sex","xrace" ,"edu","mar" , "bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )] )

tallyd<-trialrep%>% 
 group_by(sex, xrace , edu, mar ,bamphetamine30_base ,bbenzo30_base, bcannabis30_base ,ivdrug,agecat) %>%
 tally()  %>% arrange(desc(n)) %>% filter(n<5)

	#if number <5 ,move to not represented
newnonrep<-dplyr::left_join(tallyd[,c("sex","xrace" ,"edu","mar" , "bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat")], representednopreg[[i]])
underrepresentednopreg[[i]]<-rbind(notrepresentednopreg[[i]], newnonrep)

noduprep<-trialrep<-tallyd<-newnonrep<-NULL

	representednomar[[i]]<-dplyr::semi_join(stacked_list[[i]][stacked_list[[i]]$trialdata==0, c("sex","xrace" ,"edu", "PREG","bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )], stacked_list[[i]][stacked_list[[i]]$trialdata==1, c("sex","xrace" ,"edu", "PREG","bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )])
	notrepresentednomar[[i]]<-dplyr::anti_join(stacked_list[[i]][stacked_list[[i]]$trialdata==0, c("sex","xrace" ,"edu", "PREG","bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )], stacked_list[[i]][stacked_list[[i]]$trialdata==1, c("sex","xrace" ,"edu", "PREG","bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )])
	print(nrow(notrepresentednomar[[i]])/nrow(stacked_list[[i]][stacked_list[[i]]$trialdata==0,]))

	representednomarnoedu[[i]]<-dplyr::semi_join(stacked_list[[i]][stacked_list[[i]]$trialdata==0, c("sex","xrace" ,"PREG","bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )], stacked_list[[i]][stacked_list[[i]]$trialdata==1, c("sex","xrace" ,"PREG","bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )])
	notrepresentednomarnoedu[[i]]<-dplyr::anti_join(stacked_list[[i]][stacked_list[[i]]$trialdata==0, c("sex","xrace" ,"PREG","bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )], stacked_list[[i]][stacked_list[[i]]$trialdata==1, c("sex","xrace" ,"PREG","bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )])
	print(nrow(notrepresentednomarnoedu[[i]])/nrow(stacked_list[[i]][stacked_list[[i]]$trialdata==0,]))

	representednopregnomarnoedu[[i]]<-dplyr::semi_join(stacked_list[[i]][stacked_list[[i]]$trialdata==0, c("sex","xrace" ,"bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )], stacked_list[[i]][stacked_list[[i]]$trialdata==1, c("sex","xrace" ,"PREG","bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )])
	notrepresentednopregnomarnoedu[[i]]<-dplyr::anti_join(stacked_list[[i]][stacked_list[[i]]$trialdata==0, c("sex","xrace" ,"bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )], stacked_list[[i]][stacked_list[[i]]$trialdata==1, c("sex","xrace" ,"bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )])
	print(nrow(notrepresentednopregnomarnoedu[[i]])/nrow(stacked_list[[i]][stacked_list[[i]]$trialdata==0,]))

#uncomment for primary analysis
#	represented[[i]]$notrepresented<-0
#	notrepresented[[i]]$notrepresented<-1
#	tedsrecomb<-rbind(represented[[i]],notrepresented[[i]])
#	tedsrecomb$notrepresented<-as.factor(tedsrecomb$notrepresented)
noduprep<-representednopregnomarnoedu[[i]] %>% distinct
trialrep<-dplyr::left_join(noduprep, stacked_list[[i]][stacked_list[[i]]$trialdata==1,c("sex","xrace" ,"bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )] )

tallyd<-trialrep%>% 
 group_by(sex, xrace , bamphetamine30_base ,bbenzo30_base, bcannabis30_base ,ivdrug,agecat) %>%
 tally()  %>% arrange(desc(n)) %>% filter(n<5)

	#if number <5 ,move to not represented
newnonrep<-dplyr::left_join(tallyd[,c("sex","xrace" ,"bamphetamine30_base" ,"bbenzo30_base", "bcannabis30_base" ,"ivdrug","agecat" )], representednopregnomarnoedu[[i]])
underrepresentednopregnomarnoedu[[i]]<-rbind(notrepresentednopregnomarnoedu[[i]], newnonrep)

noduprep<-trialrep<-tallyd<-newnonrep<-NULL

}

representednopreg[[i]]$notrepresented<-0
	notrepresentednopreg[[i]]$notrepresented<-1
	tedsrecomb<-rbind(representednopreg[[i]],notrepresentednopreg[[i]])
	tedsrecomb$notrepresented<-as.factor(tedsrecomb$notrepresented)

set.seed(98735)
#uncomment for primary analysis
	#fit_inter <- rpart(notrepresented ~ ., method = "class", control = rpart.control(maxdepth = 10, minsplit=10, xval=10, cp=0), data = tedsrecomb[,c("sex" , "xrace" ,"PREG", "bamphetamine30_base","bbenzo30_base","bcannabis30_base" , "ivdrug","agecat", "notrepresented"   )])
fit_inter <- rpart(notrepresented ~ ., method = "class", control = rpart.control(maxdepth = 10, minsplit=10, xval=10, cp=0), data = tedsrecomb[,c("sex" , "xrace" ,"bamphetamine30_base","bbenzo30_base","bcannabis30_base" , "ivdrug","agecat", "notrepresented"   )])
	printcp(fit_inter)

	#tree.pruned <- prune(fit_inter, cp = .02)
	tree.pruned <- prune(fit_inter, cp = .03)
	printcp(tree.pruned)

boxcols <- c("pink", "palegreen3")[tree.pruned$frame$yval]

pdf(paste0("tree_nopreg",i))
par(xpd=TRUE)
prp(tree.pruned, faclen = 0, cex = 0.8, node.fun=only_count, box.col = boxcols)
legend("topleft", legend = c("represented","notrepresented"), fill = c("pink", "palegreen3", bty="n"),
       title = "Group")
dev.off()

print(rpart.rules(tree.pruned))

}


notrepresentedinanyimputations4<-dplyr::semi_join(notrepresented[[1]], notrepresented[[2]])
notrepresentedinanyimputations3<-dplyr::semi_join(notrepresentedinanyimputations4, notrepresented[[3]])
notrepresentedinanyimputations2<-dplyr::semi_join(notrepresentedinanyimputations3, notrepresented[[4]])
notrepresentedinanyimputations<-dplyr::semi_join(notrepresentedinanyimputations2, notrepresented[[5]])
nrow(notrepresentedinanyimputations)/nrow(stacked_list[[i]][stacked_list[[i]]$trialdata==0,])
#0.1074465


#what are the most frequent under-represented combinations?
library(plyr)
imp_freq <- plyr::ddply(notrepresentedinanyimputations, .(sex, agecat, xrace, PREG, edu, mar, bamphetamine30_base,bbenzo30_base, bcannabis30_base, ivdrug), nrow)

#sort by newly created column V1 which is the number of times that combination appears
imp_freq <- imp_freq[order(-imp_freq$V1),]
library(xtable)
print(xtable(head(imp_freq, 10), type="latex"))

underrepresentedinanyimputations4<-dplyr::semi_join(underrepresented[[1]], underrepresented[[2]])
underrepresentedinanyimputations3<-dplyr::semi_join(underrepresentedinanyimputations4, underrepresented[[3]])
underrepresentedinanyimputations2<-dplyr::semi_join(underrepresentedinanyimputations3, underrepresented[[4]])
underrepresentedinanyimputations<-dplyr::semi_join(underrepresentedinanyimputations2, underrepresented[[5]])
nrow(underrepresentedinanyimputations)/nrow(stacked_list[[i]][stacked_list[[i]]$trialdata==0,])
# 0.451476

#what are the most frequent under-represented combinations?
library(plyr)
imp_freq <- plyr::ddply(underrepresentedinanyimputations, .(sex, agecat, xrace, PREG, edu, mar, bamphetamine30_base,bbenzo30_base, bcannabis30_base, ivdrug), nrow)

#sort by newly created column V1 which is the number of times that combination appears
imp_freq <- imp_freq[order(-imp_freq$V1),]
library(xtable)
print(xtable(head(imp_freq, 10), type="latex"))
 

notrepresentedinanyimputationsnopreg4<-dplyr::semi_join(notrepresentednopreg[[1]], notrepresentednopreg[[2]])
notrepresentedinanyimputationsnopreg3<-dplyr::semi_join(notrepresentedinanyimputationsnopreg4, notrepresentednopreg[[3]])
notrepresentedinanyimputationsnopreg2<-dplyr::semi_join(notrepresentedinanyimputationsnopreg3, notrepresentednopreg[[4]])
notrepresentedinanyimputationsnopreg<-dplyr::semi_join(notrepresentedinanyimputationsnopreg2, notrepresentednopreg[[5]])
nrow(notrepresentedinanyimputationsnopreg)/nrow(stacked_list[[i]][stacked_list[[i]]$trialdata==0,])
#0.0834875

#what are the most frequent under-represented combinations?
library(plyr)
imp_freq <- plyr::ddply(notrepresentedinanyimputationsnopreg, .(sex, agecat, xrace, edu, mar, bamphetamine30_base,bbenzo30_base, bcannabis30_base, ivdrug), nrow)

#sort by newly created column V1 which is the number of times that combination appears
imp_freq <- imp_freq[order(-imp_freq$V1),]
library(xtable)
print(xtable(head(imp_freq, 10), type="latex"))

underrepresentedinanyimputationsnopreg4<-dplyr::semi_join(underrepresentednopreg[[1]], underrepresentednopreg[[2]])
underrepresentedinanyimputationsnopreg3<-dplyr::semi_join(underrepresentedinanyimputationsnopreg4, underrepresentednopreg[[3]])
underrepresentedinanyimputationsnopreg2<-dplyr::semi_join(underrepresentedinanyimputationsnopreg3, underrepresentednopreg[[4]])
underrepresentedinanyimputationsnopreg<-dplyr::semi_join(underrepresentedinanyimputationsnopreg2, underrepresentednopreg[[5]])
nrow(underrepresentedinanyimputationsnopreg)/nrow(stacked_list[[i]][stacked_list[[i]]$trialdata==0,])
#0.4352858

#what are the most frequent under-represented combinations?
library(plyr)
imp_freq <- plyr::ddply(underrepresentedinanyimputationsnopreg, .(sex, agecat, xrace, edu, mar, bamphetamine30_base,bbenzo30_base, bcannabis30_base, ivdrug), nrow)

#sort by newly created column V1 which is the number of times that combination appears
imp_freq <- imp_freq[order(-imp_freq$V1),]
print(xtable(head(imp_freq, 10), type="latex"))

notrepresentedinanyimputationsnopregnomarnoedu4<-dplyr::semi_join(notrepresentednopregnomarnoedu[[1]], notrepresentednopregnomarnoedu[[2]])
notrepresentedinanyimputationsnopregnomarnoedu3<-dplyr::semi_join(notrepresentedinanyimputationsnopregnomarnoedu4, notrepresentednopregnomarnoedu[[3]])
notrepresentedinanyimputationsnopregnomarnoedu2<-dplyr::semi_join(notrepresentedinanyimputationsnopregnomarnoedu3, notrepresentednopregnomarnoedu[[4]])
notrepresentedinanyimputationsnopregnomarnoedu<-dplyr::semi_join(notrepresentedinanyimputationsnopregnomarnoedu2, notrepresentednopregnomarnoedu[[5]])
nrow(notrepresentedinanyimputationsnopregnomarnoedu)/nrow(stacked_list[[i]][stacked_list[[i]]$trialdata==0,])
#0.02571164

#what are the most frequent under-represented combinations?
imp_freq <- plyr::ddply(notrepresentedinanyimputationsnopregnomarnoedu, .(sex, agecat, xrace, bamphetamine30_base,bbenzo30_base, bcannabis30_base, ivdrug), nrow)

#sort by newly created column V1 which is the number of times that combination appears
imp_freq <- imp_freq[order(-imp_freq$V1),]

print(xtable(head(imp_freq, 10), type="latex"))

underrepresentedinanyimputationsnopregnomarnoedu4<-dplyr::semi_join(underrepresentednopregnomarnoedu[[1]], underrepresentednopregnomarnoedu[[2]])
underrepresentedinanyimputationsnopregnomarnoedu3<-dplyr::semi_join(underrepresentedinanyimputationsnopregnomarnoedu4, underrepresentednopregnomarnoedu[[3]])
underrepresentedinanyimputationsnopregnomarnoedu2<-dplyr::semi_join(underrepresentedinanyimputationsnopregnomarnoedu3, underrepresentednopregnomarnoedu[[4]])
underrepresentedinanyimputationsnopregnomarnoedu<-dplyr::semi_join(underrepresentedinanyimputationsnopregnomarnoedu2, underrepresentednopregnomarnoedu[[5]])
nrow(underrepresentedinanyimputationsnopregnomarnoedu)/nrow(stacked_list[[i]][stacked_list[[i]]$trialdata==0,])
#0.08795633

#what are the most frequent under-represented combinations?
imp_freq <- plyr::ddply(underrepresentedinanyimputationsnopregnomarnoedu, .(sex, agecat, xrace, bamphetamine30_base,bbenzo30_base, bcannabis30_base, ivdrug), nrow)

#sort by newly created column V1 which is the number of times that combination appears
imp_freq <- imp_freq[order(-imp_freq$V1),]

print(xtable(head(imp_freq, 10), type="latex"))
