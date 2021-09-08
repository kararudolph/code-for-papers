
EPIIEd.tmle = function( data,
                       learners = sl3::Lrnr_glm_fast$new(),
                       uvlearners = sl3::Lrnr_hal9001$new(max_degree=5),
                       V=c('W1','W2'),
                       folds,
                       family.outcome){

    g_learners<-b_learners<-learners

    A <- data[, "A"]
    Y <- data[, "Y"]
    W <- data[, substr(names(data), 1, 1) == "W"]

    obs_weights = rep(1, length(Y))
    weights<-data[, "weights"]

    data <- data.table::as.data.table(cbind(Y, A, W, obs_weights))
    w_names <- paste("W", seq_len(dim(data.table::as.data.table(W))[2]),
                     sep = "_"
                     )

    data.table::setnames(data, c("Y", "A", w_names, "obs_weights"))

                                        # bound outcome Y in unit interval
    min_y <- min(data[["Y"]])
    max_y <- max(data[["Y"]])
    if(min_y<0 | max_y>1){
        data.table::set(data, j = "Y", value = scale_to_unit(data[["Y"]]))
    }

    ##get CV eifpiie

    eif1<-est_onestep(data=data, contrast=rep(1,nrow(data)), g_learners=g_learners, b_learners=b_learners, w_names=w_names,
                      y_bounds=c(min_y, max_y), ext_weights=weights, folds = folds)
    eif0<-est_onestep(data=data, contrast=rep(0,nrow(data)), g_learners=g_learners,  b_learners=b_learners, w_names=w_names,
                      y_bounds=c(min_y, max_y), ext_weights=weights, cv_folds = 3)

    eifate<-eif1$eif-eif0$eif

                                        #add to data table as new variable named D1
    data<-data %>%
        mutate(D1 = eifate)

    SL.fit.D1<-D_eif(data=data, uvlearners=uvlearners, w_names=w_names, folds=folds)

    d.est.Dpi = as.numeric(SL.fit.D1<0)

                                        #get CV eifdd - CV eifd0
    eifd<-est_onestep(data=data, contrast=d.est.Dpi, g_learners=g_learners, b_learners=b_learners, w_names=w_names,
                      y_bounds=c(min_y, max_y), ext_weights=weights, folds)

                                        #eif of y contrast
    ify<-data$Y - mean(data$Y)
    eifatecontrastdest<- eifd$eif - data$Y

    os.est_d = eifd$theta
    ci.widths_d = c(qnorm(0.975)*sd(eifd$eif*weights)/sqrt(nrow(data)))

    os.est_obs = mean(data$Y)
    ci.widths_obs = c(qnorm(0.975)*sd(ify*weights)/sqrt(nrow(data)))

    os.est_1 = mean(eif1$eif)
    ci.widths_1=c(qnorm(0.975)*sd(eif1$eif*weights)/sqrt(nrow(data)))

    os.est_0 = mean(eif0$eif)
    ci.widths_0=c(qnorm(0.975)*sd(eif0$eif*weights)/sqrt(nrow(data)))

    os.contrast.est<-stats::weighted.mean(eifatecontrastdest, weights)
    ci.widths.cont = c(qnorm(0.975)*sd(eifatecontrastdest*weights)/sqrt(nrow(data)))

    os.contrast.est_1<-stats::weighted.mean(eifd$eif - eif1$eif, weights)
    ci.widths.cont_1 = c(qnorm(0.975)*sd((eifd$eif - eif1$eif)*weights)/sqrt(nrow(data)))

    os.contrast.est_0<-stats::weighted.mean(eifd$eif - eif0$eif, weights)
    ci.widths.cont_0 = c(qnorm(0.975)*sd((eifd$eif - eif0$eif)*weights)/sqrt(nrow(data)))

    return(list(EYd.est=os.est_d,EYd.ci.widths=ci.widths_d,EY.est=os.est_obs,EY.ci.widths=ci.widths_obs,
                Econtrast.est=os.contrast.est,contrast.ci.widths=ci.widths.cont, eif1=eif1, eif0=eif0, eifd=eifd, os.est_1= os.est_1, os.est_0=os.est_0, ci.widths_1=ci.widths_1, ci.widths_0=ci.widths_0 , os.contrast.est_1=os.contrast.est_1, os.contrast.est_0=os.contrast.est_0, ci.widths.cont_1=ci.widths.cont_1, ci.widths.cont_0=ci.widths.cont_0))

}


EPIIEdclassification.tmle = function( data,
                                     learners = sl3::Lrnr_glm_fast$new(),
                                     V=c('W1','W2'),
                                     folds,
                                     family.outcome){

    g_learners<-b_learners<-learners

    A <- data[, "A"]
    Y <- data[, "Y"]
    W <- data[, substr(names(data), 1, 1) == "W"]

    obs_weights = rep(1, length(Y))
    weights<-data[, "weights"]

    data <- data.table::as.data.table(cbind(Y, A, W, obs_weights))
    w_names <- paste("W", seq_len(dim(data.table::as.data.table(W))[2]),
                     sep = "_"
                     )

    data.table::setnames(data, c("Y", "A", w_names, "obs_weights"))

                                        # bound outcome Y in unit interval
    min_y <- min(data[["Y"]])
    max_y <- max(data[["Y"]])
    if(min_y<0 | max_y>1){
        data.table::set(data, j = "Y", value = scale_to_unit(data[["Y"]]))
    }

                                        #get CV eifpiie
    eif1<-est_onestep(data=data, contrast=rep(1,nrow(data)), g_learners=g_learners, b_learners=b_learners, w_names=w_names, y_bounds=c(min_y, max_y), ext_weights=weights, folds)
    eif0<-est_onestep(data=data, contrast=rep(0,nrow(data)), g_learners=g_learners,  b_learners=b_learners, w_names=w_names, y_bounds=c(min_y, max_y), ext_weights=weights, folds)

    eifate<-eif1$eif-eif0$eif

                                        #add to data table as new variable named D1
    data<-data %>%
        mutate(D = eifate, obs_weights=abs(eifate), Dclass=as.numeric(eifate < 0))

    SL.fit.D1<-Dclass_eif(data=data, learners=learners, w_names=w_names, folds)

    d.est.Dpi = as.numeric(SL.fit.D1 > 0.5)

    data$obs_weights = rep(1, length(Y))
                                        #get CV eifdd - CV eifd0
    eifd<-est_onestep(data=data, contrast=d.est.Dpi, g_learners=g_learners, b_learners=b_learners, w_names=w_names, y_bounds=c(min_y, max_y), ext_weights=weights, folds=folds)

                                        #eif of y contrast
    ify<-data$Y - mean(data$Y)
    eifatecontrastdest<- eifd$eif - data$Y

    os.est_d = eifd$theta
    ci.widths_d = c(qnorm(0.975)*sd(eifd$eif*weights)/sqrt(nrow(data)))

    os.est_obs = mean(data$Y)
    ci.widths_obs = c(qnorm(0.975)*sd(ify*weights)/sqrt(nrow(data)))

    os.contrast.est<-stats::weighted.mean(eifatecontrastdest, weights)
    ci.widths.cont = c(qnorm(0.975)*sd(eifatecontrastdest*weights)/sqrt(nrow(data)))

    return(list(EYd.est=os.est_d,EYd.ci.widths=ci.widths_d,EY.est=os.est_obs,EY.ci.widths=ci.widths_obs,Econtrast.est=os.contrast.est,contrast.ci.widths=ci.widths.cont, eif1=eif1, eif0=eif0, eifd=eifd ))

}

ATElassod.tmle = function( data, learners = sl3::Lrnr_glm_fast$new(),
                       uvlearners = sl3::Lrnr_hal9001$new(max_degree=5),
                       V=c('W1','W2'),
                       folds,
                       family.outcome){

    g_learners<-b_learners<-learners

    A <- data[, "A"]
    Y <- data[, "Y"]
    W <- data[, substr(names(data), 1, 1) == "W"]

    obs_weights = rep(1, length(Y))
    weights<-data[, "weights"]

    data <- data.table::as.data.table(cbind(Y, A, W, obs_weights))
    w_names <- paste("W", seq_len(dim(data.table::as.data.table(W))[2]),
                     sep = "_"
                     )

    data.table::setnames(data, c("Y", "A", w_names, "obs_weights"))

                                        # bound outcome Y in unit interval
    min_y <- min(data[["Y"]])
    max_y <- max(data[["Y"]])
    if(min_y<0 | max_y>1){
        data.table::set(data, j = "Y", value = scale_to_unit(data[["Y"]]))
    }

    ##get CV eifpiie

    eif1<-est_onestep(data=data, contrast=rep(1,nrow(data)), g_learners=g_learners, b_learners=b_learners, w_names=w_names,
                      y_bounds=c(min_y, max_y), ext_weights=weights, folds = folds)
    eif0<-est_onestep(data=data, contrast=rep(0,nrow(data)), g_learners=g_learners,  b_learners=b_learners, w_names=w_names,
                      y_bounds=c(min_y, max_y), ext_weights=weights, folds = folds)

    eifate<-eif1$eif-eif0$eif

    d.est.Dpi = as.numeric(eifate<0)
    data<-as.data.frame(data)
    datlim<-data[,w_names]
    datamatrix<-model.matrix( ~ .^2, datlim)[,-1]
    #datamatrix<-model.matrix( ~ (W1+W2+W3+W4+W5+W6+W7+W8+W9+W10+W11+W12+W13+W14+W15+W16+W17+W18+W19+W20+W21+W22 + W23+W24+W25+W26+W27+W28+W29+W30+W31+W32+W33+W34+W35)^2, data)[,-1]
    newdat<-as.data.frame(datamatrix)
    newdat$cate<-d.est.Dpi

    newdat<-as.data.table(newdat)

    lassorule<-est_adaptlasso(data=newdat, family="binomial", folds=folds)
                                        #get CV eifdd - CV eifd0
 
    eifd<-est_onestep(data=data, contrast=lassorule, g_learners=g_learners, b_learners=b_learners, w_names=w_names,
                      y_bounds=c(min_y, max_y), ext_weights=weights, folds)

                                        #eif of y contrast
    ify<-data$Y - mean(data$Y)
    eifatecontrastdest<- eifd$eif - data$Y

    os.est_d = eifd$theta
    ci.widths_d = c(qnorm(0.975)*sd(eifd$eif*weights)/sqrt(nrow(data)))

    os.est_obs = mean(data$Y)
    ci.widths_obs = c(qnorm(0.975)*sd(ify*weights)/sqrt(nrow(data)))

    os.contrast.est<-stats::weighted.mean(eifatecontrastdest, weights)
    ci.widths.cont = c(qnorm(0.975)*sd(eifatecontrastdest*weights)/sqrt(nrow(data)))

    return(list(EYd.est=os.est_d,EYd.ci.widths=ci.widths_d,EY.est=os.est_obs,EY.ci.widths=ci.widths_obs,
                Econtrast.est=os.contrast.est,contrast.ci.widths=ci.widths.cont, eif1=eif1, eif0=eif0, eifd=eifd ))

}

ATElassocontd.tmle = function( data, learners = sl3::Lrnr_glm_fast$new(),
                       uvlearners = sl3::Lrnr_hal9001$new(max_degree=5),
                       V=c('W1','W2'),
                       folds,
                       family.outcome){

    g_learners<-b_learners<-learners

    A <- data[, "A"]
    Y <- data[, "Y"]
    W <- data[, substr(names(data), 1, 1) == "W"]

    obs_weights = rep(1, length(Y))
    weights<-data[, "weights"]

    data <- data.table::as.data.table(cbind(Y, A, W, obs_weights))
    w_names <- paste("W", seq_len(dim(data.table::as.data.table(W))[2]),
                     sep = "_"
                     )

    data.table::setnames(data, c("Y", "A", w_names, "obs_weights"))

                                        # bound outcome Y in unit interval
    min_y <- min(data[["Y"]])
    max_y <- max(data[["Y"]])
    if(min_y<0 | max_y>1){
        data.table::set(data, j = "Y", value = scale_to_unit(data[["Y"]]))
    }

    ##get CV eifpiie

    eif1<-est_onestep(data=data, contrast=rep(1,nrow(data)), g_learners=g_learners, b_learners=b_learners, w_names=w_names,
                      y_bounds=c(min_y, max_y), ext_weights=weights, folds = folds)
    eif0<-est_onestep(data=data, contrast=rep(0,nrow(data)), g_learners=g_learners,  b_learners=b_learners, w_names=w_names,
                      y_bounds=c(min_y, max_y), ext_weights=weights, folds = folds)

    eifate<-eif1$eif-eif0$eif

    d.est.Dpi = eifate
    data<-as.data.frame(data)
    datlim<-data[,w_names]
    datamatrix<-model.matrix( ~ .^2, datlim)[,-1]
    #datamatrix<-model.matrix( ~ (W1+W2+W3+W4+W5+W6+W7+W8+W9+W10+W11+W12+W13+W14+W15+W16+W17+W18+W19+W20+W21+W22 + W23+W24+W25+W26+W27+W28+W29+W30+W31+W32+W33+W34+W35)^2, data)[,-1]
    newdat<-as.data.frame(datamatrix)
    newdat$cate<-d.est.Dpi

    newdat<-as.data.table(newdat)

    lassoest<-est_adaptlasso_cont(data=newdat, family="gaussian", folds=folds)
                                        #get CV eifdd - CV eifd0
    lassorule = as.numeric(lassoest<0)
    eifd<-est_onestep(data=data, contrast=lassorule, g_learners=g_learners, b_learners=b_learners, w_names=w_names,
                      y_bounds=c(min_y, max_y), ext_weights=weights, folds)

                                        #eif of y contrast
    ify<-data$Y - mean(data$Y)
    eifatecontrastdest<- eifd$eif - data$Y

    os.est_d = eifd$theta
    ci.widths_d = c(qnorm(0.975)*sd(eifd$eif*weights)/sqrt(nrow(data)))

    os.est_obs = mean(data$Y)
    ci.widths_obs = c(qnorm(0.975)*sd(ify*weights)/sqrt(nrow(data)))

    os.contrast.est<-stats::weighted.mean(eifatecontrastdest, weights)
    ci.widths.cont = c(qnorm(0.975)*sd(eifatecontrastdest*weights)/sqrt(nrow(data)))

    return(list(EYd.est=os.est_d,EYd.ci.widths=ci.widths_d,EY.est=os.est_obs,EY.ci.widths=ci.widths_obs,
                Econtrast.est=os.contrast.est,contrast.ci.widths=ci.widths.cont, eif1=eif1, eif0=eif0, eifd=eifd ))

}

singlevard.tmle = function( data, learners = sl3::Lrnr_glm_fast$new(),
                       uvlearners = sl3::Lrnr_hal9001$new(max_degree=5),
                       V=c('W1','W2'),
                       folds,
                       family.outcome){

    g_learners<-b_learners<-learners

    A <- data[, "A"]
    Y <- data[, "Y"]
    W <- data[, substr(names(data), 1, 1) == "W"]

    obs_weights = rep(1, length(Y))
    weights<-data[, "weights"]

    data <- data.table::as.data.table(cbind(Y, A, W, obs_weights))
    w_names <- paste("W", seq_len(dim(data.table::as.data.table(W))[2]),
                     sep = "_"
                     )

    data.table::setnames(data, c("Y", "A", w_names, "obs_weights"))

                                        # bound outcome Y in unit interval
    min_y <- min(data[["Y"]])
    max_y <- max(data[["Y"]])
    if(min_y<0 | max_y>1){
        data.table::set(data, j = "Y", value = scale_to_unit(data[["Y"]]))
    }

    ##get CV eifpiie

    eif1<-est_onestep(data=data, contrast=rep(1,nrow(data)), g_learners=g_learners, b_learners=b_learners, w_names=w_names,
                      y_bounds=c(min_y, max_y), ext_weights=weights, folds = folds)
    eif0<-est_onestep(data=data, contrast=rep(0,nrow(data)), g_learners=g_learners,  b_learners=b_learners, w_names=w_names,
                      y_bounds=c(min_y, max_y), ext_weights=weights, folds = folds)

    eifate<-eif1$eif-eif0$eif

    data<-data %>%
        mutate(D1 = eifate)

    wnamesnosite<-c("W_1", "W_2", "W_3", "W_11", "W_12", "W_13", "W_14", "W_15","W_16", "W_17", "W_18", "W_19", "W_20", "W_21", "W_22", "W_23", "W_24", "W_25","W_26", "W_27", "W_28", "W_29","W_30", "W_31", "W_32", "W_33", "W_34", "W_35")
    SL.fit.D1<-D_eif_singlevar(data=data, uvlearners=uvlearners, w_names=wnamesnosite, folds=folds)

    d.est.Dpi = as.numeric(SL.fit.D1[[1]]<0)

                                        #get CV eifdd - CV eifd0
    eifd<-est_onestep(data=data, contrast=d.est.Dpi, g_learners=g_learners, b_learners=b_learners, w_names=w_names,
                      y_bounds=c(min_y, max_y), ext_weights=weights, folds)

                                        #eif of y contrast
    ify<-data$Y - mean(data$Y)
    eifatecontrastdest<- eifd$eif - data$Y

    os.est_d = eifd$theta
    ci.widths_d = c(qnorm(0.975)*sd(eifd$eif*weights)/sqrt(nrow(data)))

    os.est_obs = mean(data$Y)
    ci.widths_obs = c(qnorm(0.975)*sd(ify*weights)/sqrt(nrow(data)))

    os.contrast.est<-stats::weighted.mean(eifatecontrastdest, weights)
    ci.widths.cont = c(qnorm(0.975)*sd(eifatecontrastdest*weights)/sqrt(nrow(data)))

    return(list(EYd.est=os.est_d,EYd.ci.widths=ci.widths_d,EY.est=os.est_obs,EY.ci.widths=ci.widths_obs,
                Econtrast.est=os.contrast.est,contrast.ci.widths=ci.widths.cont, eif1=eif1, eif0=eif0, eifd=eifd , whichvar=SL.fit.D1[[2]]))
}






#    d.est.Dpi = as.numeric(eifate<0)
#    newdat<-as.data.frame(data)[,c("W_1", "W_2", "W_3", "W_11", "W_12", "W_13", "W_14", "W_15","W_16", "W_17", "W_18", "W_19", "W_20", "W_21", "W_22", "W_23", "W_24", "W_25","W_26", "W_27", "W_28", "W_29","W_30", "W_31", "W_32", "W_33", "W_34", "W_35")]
#    newdat$cate<-d.est.Dpi

#    newdat<-as.data.table(newdat)
    #covariate<-c("W1", "W2", "W3", "W11", "W12", "W13", "W14", "W15","W16", "W17", "W18", "W19", "W20", "W21", "W22", "W23", "W24", "W25","W26", "W27", "W28", "W29","W30", "W31", "W32", "W33", "W34", "W35")
    #always include W4:W10
   
#    singled<-est_singled(data=newdat, family="binomial", folds=folds)
                                        #get CV eifdd - CV eifd0

esteval<-function(data, contrast, g_learners, b_learners, w_names, y_bounds, ext_weights, folds){

    eifd<-est_onestep(data=data, contrast=lassorule, g_learners=g_learners, b_learners=b_learners, w_names=w_names,
                      y_bounds=c(min_y, max_y), ext_weights=weights, folds)

                                        #eif of y contrast
    ify<-data$Y - mean(data$Y)
    eifatecontrastdest<- eifd$eif - data$Y

    os.est_d = eifd$theta
    ci.widths_d = c(qnorm(0.975)*sd(eifd$eif*weights)/sqrt(nrow(data)))

    os.est_obs = mean(data$Y)
    ci.widths_obs = c(qnorm(0.975)*sd(ify*weights)/sqrt(nrow(data)))

    os.contrast.est<-stats::weighted.mean(eifatecontrastdest, weights)
    ci.widths.cont = c(qnorm(0.975)*sd(eifatecontrastdest*weights)/sqrt(nrow(data)))

    return(list(EYd.est=os.est_d,EYd.ci.widths=ci.widths_d,EY.est=os.est_obs,EY.ci.widths=ci.widths_obs,
                Econtrast.est=os.contrast.est,contrast.ci.widths=ci.widths.cont, eif1=eif1, eif0=eif0, eifd=eifd ))

}

