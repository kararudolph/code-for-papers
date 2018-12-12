library(MatchIt)
library(ltmle)
library(data.table)
library(parallel)


analyzeImputedData = function(dt, dichotomize, outcome, discard, regionType){
  #Subset data by ubrancat and positive weight
  if(regionType == "urban"){
    dt = dt[dt$urbancat == 1 & dt$final_weight != 0,]
  }
  else if(regionType == "suburban"){
    dt = dt[dt$suburb == 1 & dt$final_weight != 0,]
  }
  else if(regionType == "rural"){
    dt = dt[dt$urbancat == 0 & dt$suburb == 0 & dt$final_weight != 0,]
  }
  else{stop("Invalid Region Type")}
  
  if(outcome %in% c("saq_add_impair_NIMH2", "cp_CdOddh_NIMH2.add")){
    dt = dt[!is.na(dt$final_weight_psaq),]
  }
  
  
  if(outcome == "mddys_or_anxiety"){
    dt$mddys_or_anxiety = as.numeric(dt$dsm_mdddys_NIMH2|dt$dsm_anxiety_NIMH2)
  }
  
  #Determine Dichotomization type
  if(dichotomize == "cluster"){
    d = dist(dt[,c("ldnmean", "leqmean", "nightmean", "ldnmax", "leqmax", "nightmax")])
    hc = hclust(d)
    clusts = cutree(hc, k = 2)
    m1 = mean(dt$ldnmean[clusts == 1])
    m2 = mean(dt$ldnmean[clusts == 2])
    if(m1 > m2){
      dt$noise = (clusts - 2) * -1
    }
    else{
      dt$noise = clusts - 1
    }
  }
  else if(is.numeric(dichotomize) & !is.na(dichotomize)){
    dt$noise = as.numeric(dt$ldnmean > dichotomize)
  }
  else if(dichotomize == "none"){
    dt$noise = dt$ldnmean
  }
  else{
    stop("Error: Invalid dichotomization option")
  }
  
  #Remove variables with missing values
  dt2 = dt[, sapply(dt, function(x){sum(is.na(x))==0})]
  
  #Fit propensity score matching
  if(dichotomize != "none"){
    ps = matchit(noise ~ SEXF + age + factor(racecat) + Language + 
                   lninc + ImgGen + poly(mage,2) + fath + moth + meducat + 
                   pc_psych_minor + pp_pa_minor + pp_pa_severe + 
                   pc_pa_minor + pc_pa_severe + CH33 + factor(emp) + score + 
                   factor(Pmarital_4cat) + factor(region) + no2_2000 +
                   factor(religion) + citizen +  ndvi01 + tmax_mean + 
                   popden, data = dt2, 
                 method = "full", 
                 replace=TRUE, discard=discard, 
                 #exact=c("SEXF", "urbancat", "suburb"), 
                 exact=c("SEXF"),
                 mahvars = c("score", "lninc"), 
                 caliper=0.2, 
                 reestimate=FALSE)
    
    #Grab matched data
    psData = match.data(ps)
    #Grab propensity scores
    if(discard != "none"){
      pscore = ps$distance[!ps$discarded]
    }
    else{
      pscore = ps$distance
    }
    
    
    #Truncate data with extreme propensity scores
    if(discard != "none"){
      dc1 = quantile(pscore[ps$treat[!ps$discarded] == 1], .01)
      dc2 = quantile(pscore[ps$treat[!ps$discarded] == 0], .99)
      
      psData2 = psData[pscore > dc1 & pscore < dc2,]
      pscore2 = pscore[pscore > dc1 & pscore < dc2]
    }
    else{
      psData2 = psData
      pscore2 = pscore
    }
    
    
    
    #Compute new weights
    if(outcome %in% c("saq_add_impair_NIMH2", "cp_CdOddh_NIMH2.add")){
      wt = psData2$weights * (psData2$final_weight_psaq / mean(psData2$final_weight_psaq))
    }
    else{
      wt = psData2$weights * (psData2$final_weight / mean(psData2$final_weight))
    }
    
    
    #Create data set for ltmle
    psData3 = psData2[,c("SEXF", "age", "racecat", "Language",
                         "lninc", "ImgGen", "mage", "fath", "moth",
                         "meducat", "pc_psych_minor", "pp_pa_minor",
                         "pp_pa_severe", "CH33", "emp", "score", "no2_2000",
                         "Pmarital_4cat", "region", "religion", "citizen",
                         "ndvi01", "tmax_mean", "popden", "noise", 
                         gsub("\\.([a-z]*)", "", outcome))]
    SL.library <- c("SL.glm",  "SL.mean", "SL.bayesglm", "SL.earth", "SL.gam")
    
    gform = array(c(pscore2, 1 - pscore2), dim = c(length(pscore2), 1, 2))
    
    #Run ltmle
    gf = ltmle(data = psData3, Anodes = "noise", Lnodes = NULL, 
               Ynodes = gsub("\\.([a-z]*)", "", outcome),
               gform = gform, abar = list(1,0), SL.library = SL.library,
               observation.weights = wt)
    
    return(gf)
  }
  
  else{
    wt = dt2$final_weight / mean(dt2$final_weight)
    Y = dt2[, gsub("\\.([a-z]*)", "", outcome)]
    X = dt2[, c("SEXF", "age", "racecat", "Language",
                "lninc", "ImgGen", "mage", "fath", "moth",
                "meducat", "pc_psych_minor", "pp_pa_minor",
                "pp_pa_severe", "CH33", "emp", "score", "no2_2000",
                "Pmarital_4cat", "region", "religion", "citizen",
                "ndvi01", "tmax_mean", "popden", "noise")]
    SL.library <- c("SL.glm",  "SL.mean", "SL.bayesglm", "SL.earth", "SL.gam")
    newX = rbind(X, X)
    newX$noise[(nrow(X) + 1):(2*nrow(X))] = X$noise + sd(X$noise)
    SLfit = SuperLearner(Y = Y, X = X, newX = newX, obsWeights = wt, 
                         SL.library = SL.library)
    as.numeric(t(SLfit$SL.predict) %*% rep(c(1,-1), each = nrow(X)))
  }
  
  
  
}

#Function takes ltmle results as an input and returns estimates
grabPointEsts = function(r){
  rslts = unlist(summary(r))
  if(any(grepl("RR", names(rslts)))){
    pointEsts = rslts[grepl("estimate", names(rslts))][1:5]
    names(pointEsts) = c("TreatmentEstimate", "ControlEstimate", 
                         "AdditiveTreatmentEffect", "RelativeRisk",
                         "OddsRatio")
  }
  else{
    pointEsts = unlist(rslts[grepl("estimate", names(rslts))][1:3])
    names(pointEsts) = c("TreatmentEstimate", "ControlEstimate", 
                         "AdditiveTreatmentEffect")
  }
  unlist(pointEsts)
}

#Function takes ltmle results as an input and returns estimates
grabStdErrs = function(r){
  rslts = unlist(summary(r))
  if(any(grepl("RR", names(rslts)))){
    rslts = unlist(summary(r))
    stdErrs = rslts[grepl("std.dev", names(rslts))]
    names(stdErrs) = c("TreatmentEstimate", "ControlEstimate", 
                       "AdditiveTreatmentEffect", "RelativeRisk",
                       "OddsRatio")
  }
  else{
    stdErrs = unlist(rslts[grepl("std.dev", names(rslts))])
    names(stdErrs) = c("TreatmentEstimate", "ControlEstimate", 
                       "AdditiveTreatmentEffect")
  }
  unlist(stdErrs)
}

#Function takes:
#    dataList := list of multiply imputed data sets
#    dichotomize := method to dichotomize data.  Options are a numeric value
#                    to split along ldnmean, or "clustering" to use hierarchical
#                    clustering on all mean and max noise measures.
#    outcome := name of the outcome variable to analyze.

analyzeImputedDataList = function(dataList, regionType = "urban", dichotomize = 55, outcome, 
                                  alpha = 0.05, discard = TRUE){
  #Get ltmle results for all data sets
  resultsList = mclapply(dataList, analyzeImputedData, regionType = regionType, dichotomize = dichotomize,
                         outcome = outcome, discard = discard, mc.cores = 1)
  #Get point estimates from all ltmle results
  resultsPointEsts = do.call(rbind, lapply(resultsList, grabPointEsts))
  if(ncol(resultsPointEsts) == 5){
    resultsPointEsts[, "RelativeRisk"] = log(resultsPointEsts[, "RelativeRisk"])
    resultsPointEsts[, "OddsRatio"] = log(resultsPointEsts[, "OddsRatio"])
  }
  #Get standard errors from all ltmle results
  resultsStdErrs = do.call(rbind, lapply(resultsList, grabStdErrs))
  #Combine results using Ruben's Rule
  Qbar = colMeans(resultsPointEsts)
  Ubar = colMeans(resultsStdErrs^2)
  B = sapply(1:ncol(resultsPointEsts), function(i, pe, q){
    (1/(nrow(pe) - 1)) * sum((pe[,i] - q[i])^2)
  }, pe  = resultsPointEsts, q = Qbar)
  T = sqrt(Ubar + (1 + 1/nrow(resultsPointEsts)) * B)
  if(length(Qbar) == 5){
    Qbar[4] = exp(Qbar[4])
    Qbar[5] = exp(Qbar[5])
  }
  rbind(data.table(t(Qbar)), data.table(t(T)))
  return(list(estimates = Qbar, stdErrs = T))
}


# Outcomes: 
#   •	sleep (variable names: 
#            o	CC19_1 - hours of sleep on weeknight
#            o	CC19_3 - hours of sleep weekendnight
#            o	btwk - bedtime on weeknight
#            o	btwkndc - bedtime on weekend night
#   •	mental health (variable names: 
#            o	cp_CdOddh_NIMH2 - conduct or oppositional defiant disorder
#            o	dsm_substance_NIMH2 
#            o  dsm_mddys_NIMH2 OR dsm_anxiety_NIMH2
#            o  saq_add_impair_NIMH2 - severe adhd

#Load imputed data sets
load("~/dat.Rdata")

outcomes = c("CC19_1", "CC19_3", "btwk", "btwknd",             
            "dsm_substance_NIMH2", "mddys_or_anxiety", 
             "saq_add_impair_NIMH2", "cp_CdOddh_NIMH2.add")

allOutcomes = mclapply(outcomes, analyzeImputedDataList, dataList = foraarondat, regionType = "urban", 
                       dichotomize = 55, discard = "none", mc.cores = 12)
