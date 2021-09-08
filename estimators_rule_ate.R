est_adaptlasso<-function(data, 
                          family="binomial", 
                          folds){
  data<-as.data.table(data)

  # estimate the EIF on a per-fold basis
  cv_adapt_lasso <- origami::cross_validate(
    cv_fun = cv_adaptlasso,
    folds = folds,
    data_in = data,
    use_future = FALSE,
    .combine = FALSE
  )

  # get estimated efficient influence function
  cv_rule_est <- do.call(c, lapply(cv_adapt_lasso[[1]], `[[`, "lasso_rule"))
  obs_valid_idx <- do.call(c, lapply(folds, `[[`, "validation_set"))
  cv_rule_est <- cv_rule_est[order(obs_valid_idx)]

}

est_adaptlasso_cont<-function(data, 
                          family="gaussian", 
                          folds){
  data<-as.data.table(data)

  # estimate the EIF on a per-fold basis
  cv_adapt_lasso <- origami::cross_validate(
    cv_fun = cv_adaptlasso_cont,
    folds = folds,
    data_in = data,
    use_future = FALSE,
    .combine = FALSE
  )

  # get estimated efficient influence function
  cv_rule_est <- do.call(c, lapply(cv_adapt_lasso[[1]], `[[`, "lasso_rule"))
  obs_valid_idx <- do.call(c, lapply(folds, `[[`, "validation_set"))
  cv_rule_est <- cv_rule_est[order(obs_valid_idx)]

}


est_singled<-function(data, 
                          family="binomial", 
                          folds){
  data<-as.data.table(data)

  # estimate the EIF on a per-fold basis
  cv_single_d <- origami::cross_validate(
    cv_fun = cv_singled,
    folds = folds,
    data_in = data,
    use_future = FALSE,
    .combine = FALSE
  )

  # get estimated efficient influence function
  cv_rule_est <- do.call(c, lapply(cv_single_d[[1]], `[[`, "wname"))
  obs_valid_idx <- do.call(c, lapply(folds, `[[`, "validation_set"))
  cv_rule_est <- cv_rule_est[order(obs_valid_idx)]

}


cv_adaptlasso <- function(fold,
                   data_in) {
  # make training and validation data
  train_data <- origami::training(data_in)
  valid_data <- origami::validation(data_in)

  # 1) fit regression for propensity score regression
  lasso_out <- fit_cate_mech(
    train_data = train_data,
    valid_data = valid_data
  )

  # extract components; NOTE: only do this for observations in validation set
  lasso_rule <- lasso_out$estrule


  # output list
  out <- list(data.table::data.table(
    # components necessary for fluctuation step of TMLE
    lasso_rule = lasso_rule, fold = origami::fold_index()
  ), cov=lasso_out$cov.picked)
  return(out)
}

cv_adaptlasso_cont <- function(fold,
                   data_in) {
  # make training and validation data
  train_data <- origami::training(data_in)
  valid_data <- origami::validation(data_in)

  # 1) fit regression for propensity score regression
  lasso_out <- fit_cate_mech_cont(
    train_data = train_data,
    valid_data = valid_data
  )

  # extract components; NOTE: only do this for observations in validation set
  lasso_rule <- lasso_out$estrule


  # output list
  out <- list(data.table::data.table(
    # components necessary for fluctuation step of TMLE
    lasso_rule = lasso_rule, fold = origami::fold_index()
  ), cov=lasso_out$cov.picked)
  return(out)
}

#make glearners and b_learners 
#screen.corRankBest plus


cv_singled <- function(fold,
                   data_in,
                   contrast,
                   g_learners,
                   b_learners,
                   w_names) {
  # make training and validation data
  train_data <- origami::training(data_in)
  valid_data <- origami::validation(data_in)

  # 1) fit regression for propensity score regression
  g_out <- fit_treat_mech(
    train_data = train_data,
    valid_data = valid_data,
    contrast = contrast,
    learners = g_learners,
    w_names = w_names
  )

  # 3) fit outcome regression
  b_out <- fit_out_mech(
    train_data = train_data,
    valid_data = valid_data,
    contrast = contrast,
    learners = b_learners,
    w_names = w_names
  )

  # extract components; NOTE: only do this for observations in validation set
  b_prime <- b_out$b_est_valid$b_pred_A_prime
  #b_natural <- b_out$b_est_valid$b_pred_A_natural
  g_prime <- g_out$treat_est_valid$treat_pred_A_prime

  # create inverse probability weights
  C1<-valid_data$C1
  ipw_a_prime <- as.numeric(valid_data$A == C1) / g_prime

  # compute uncentered efficient influence function components
  eif_y <- ipw_a_prime * (valid_data$Y - b_prime) + b_prime
  eif <- eif_y

  # output list
  out <- list(data.table::data.table(
    # components necessary for fluctuation step of TMLE
    g_prime = g_prime,
    b_prime = b_prime,
    # efficient influence function and fold IDs
    D_star = eif, fold = origami::fold_index()
  ))
  return(out)
}


fit_best <- function(train_data,
                           valid_data = NULL) {

  fit <- rpart(cate ~ . , method="class", data=train_data)
  
  summary(fit)$used

  valid_data$cate<-NULL
  valid.data<-as.matrix(valid_data)
  suboptrulelasso<-predict(mstarfit, s="lambda.min", gamma=c(1), 
    relax=TRUE, newx=valid.data, type="class")

    ## output
    out <- list(estrule=as.numeric(suboptrulelasso), cov.picked=vsecondorderint)
  return(out)
}

fit_cate_mech <- function(train_data,
                           valid_data = NULL) {

  #res<-lm(cate ~ ., data=train_data)
#ker 6/29/2021 should it be the following?
  res<-glm(cate ~ ., data=train_data)
  pfacm<-(1/abs(res$coefficients))
  pfacm<-ifelse(is.na(pfacm), 2000, pfacm)

  #lastcol<-ncol(train_data)
  mat1<-train_data
  mat1$cate<- NULL
  mstarfit<-cv.glmnet(as.matrix(mat1), train_data$cate, family="binomial", 
    penalty.factor=pfacm, nfolds=5)

  vsecondorderint<-rownames(coef(mstarfit, s="lambda.min"))[coef(mstarfit, s="lambda.min")[,1]!=0]

  valid_data$cate<-NULL
  valid.data<-as.matrix(valid_data)
  suboptrulelasso<-predict(mstarfit, s="lambda.min", gamma=c(1), 
    relax=TRUE, newx=valid.data, type="class")

    ## output
    out <- list(estrule=as.numeric(suboptrulelasso), cov.picked=vsecondorderint)
  return(out)
}

fit_cate_mech_cont <- function(train_data,
                           valid_data = NULL) {

  res<-lm(cate ~ ., data=train_data)
  pfacm<-(1/abs(res$coefficients))
  pfacm<-ifelse(is.na(pfacm), 2000, pfacm)

  #lastcol<-ncol(train_data)
  mat1<-train_data
  mat1$cate<- NULL
  mstarfit<-cv.glmnet(as.matrix(mat1), train_data$cate, family="gaussian", 
    penalty.factor=pfacm, nfolds=5)

  vsecondorderint<-rownames(coef(mstarfit, s="lambda.min"))[coef(mstarfit, s="lambda.min")[,1]!=0]

  valid_data$cate<-NULL
  valid.data<-as.matrix(valid_data)
  suboptrulelasso<-predict(mstarfit, s="lambda.min", gamma=c(1), 
    relax=TRUE, newx=valid.data)

    ## output
    out <- list(estrule=as.numeric(suboptrulelasso), cov.picked=vsecondorderint)
  return(out)
}


cv_eif <- function(fold,
                   data_in,
                   contrast,
                   g_learners,
                   b_learners,
                   w_names) {
  # make training and validation data
  train_data <- origami::training(data_in)
  valid_data <- origami::validation(data_in)

  # 1) fit regression for propensity score regression
  g_out <- fit_treat_mech(
    train_data = train_data,
    valid_data = valid_data,
    contrast = contrast,
    learners = g_learners,
    w_names = w_names
  )

  # 3) fit outcome regression
  b_out <- fit_out_mech(
    train_data = train_data,
    valid_data = valid_data,
    contrast = contrast,
    learners = b_learners,
    w_names = w_names
  )

  # extract components; NOTE: only do this for observations in validation set
  b_prime <- b_out$b_est_valid$b_pred_A_prime
  #b_natural <- b_out$b_est_valid$b_pred_A_natural
  g_prime <- g_out$treat_est_valid$treat_pred_A_prime

  # create inverse probability weights
  C1<-valid_data$C1
  ipw_a_prime <- as.numeric(valid_data$A == C1) / g_prime

  # compute uncentered efficient influence function components
  eif_y <- ipw_a_prime * (valid_data$Y - b_prime) + b_prime
  eif <- eif_y

  # output list
  out <- list(data.table::data.table(
    # components necessary for fluctuation step of TMLE
    g_prime = g_prime,
    b_prime = b_prime,
    # efficient influence function and fold IDs
    D_star = eif, fold = origami::fold_index()
  ))
  return(out)
}

Dclass_eif<-function(data,
                learners,
                w_names,
                folds){

  # make sure that more than one fold is specified
  ## assertthat::assert_that(cv_folds > 1)

  # create cross-validation folds
  ## folds <- origami::make_folds(data,
  ##   fold_fun = origami::folds_vfold,
  ##   V = cv_folds
  ## )

# estimate the EIF on a per-fold basis
  cv_d_eif_results <- origami::cross_validate(
    cv_fun = cv_dclass_eif,
    folds = folds,
    data_in = data,
    learners = learners,
    w_names = w_names,
    use_future = FALSE,
    .combine = FALSE
  )

  # get estimated efficient influence function
  cv_d_eif_est <- do.call(c, lapply(cv_d_eif_results[[1]], `[[`, "D_star"))
  obs_valid_idx <- do.call(c, lapply(folds, `[[`, "validation_set"))
  cv_d_eif_est <- cv_d_eif_est[order(obs_valid_idx)]

  d_eif_est_out<-cv_d_eif_est

  return(d_eif_est_out)


}


D_eif<-function(data,
                uvlearners,
                w_names,
                folds){

  # make sure that more than one fold is specified
  ## assertthat::assert_that(cv_folds > 1)

  # create cross-validation folds
  ## folds <- origami::make_folds(data,
  ##   fold_fun = origami::folds_vfold,
  ##   V = cv_folds
  ## )

# estimate the EIF on a per-fold basis
  cv_d_eif_results <- origami::cross_validate(
    cv_fun = cv_d_eif,
    folds = folds,
    data_in = data,
    uvlearners = uvlearners,
    w_names = w_names,
    use_future = FALSE,
    .combine = FALSE
  )

  # get estimated efficient influence function
  cv_d_eif_est <- do.call(c, lapply(cv_d_eif_results[[1]], `[[`, "D_star"))
  obs_valid_idx <- do.call(c, lapply(folds, `[[`, "validation_set"))
  cv_d_eif_est <- cv_d_eif_est[order(obs_valid_idx)]

  d_eif_est_out<-cv_d_eif_est

  return(d_eif_est_out)


}

cv_d_eif <- function(fold,
                   data_in,
                   uvlearners,
                   w_names) {

  # make training and validation data
  train_data <- origami::training(data_in)
  valid_data <- origami::validation(data_in)

d_out <- fit_rule_mech(
    train_data = train_data,
    valid_data = valid_data,
    learners = uvlearners,
    w_names = w_names
  )

  # extract components; NOTE: only do this for observations in validation set
  d_est <- d_out$treat_est_valid$treat_pred

  # output list
  out <- list(data.table::data.table(
    # components necessary for fluctuation step of TMLE
        D_star = d_est, fold = origami::fold_index()
  ))
  return(out)
}

D_eif_singlevar<-function(data,
                uvlearners,
                w_names,
                folds){

  # make sure that more than one fold is specified
  ## assertthat::assert_that(cv_folds > 1)

  # create cross-validation folds
  ## folds <- origami::make_folds(data,
  ##   fold_fun = origami::folds_vfold,
  ##   V = cv_folds
  ## )

# estimate the EIF on a per-fold basis
  cv_d_eif_results <- origami::cross_validate(
    cv_fun = cv_d_eif_singlevar,
    folds = folds,
    data_in = data,
    uvlearners = sluv,
    w_names = w_names,
    use_future = FALSE,
    .combine = FALSE
  )

  # get estimated efficient influence function
  cv_d_eif_est <- do.call(c, lapply(cv_d_eif_results[[1]], `[[`, "D_star"))
  obs_valid_idx <- do.call(c, lapply(folds, `[[`, "validation_set"))
  cv_d_eif_est <- cv_d_eif_est[order(obs_valid_idx)]

  d_eif_est_out<-cv_d_eif_est

  cv_name <- do.call(c, lapply(cv_d_eif_results[[1]], `[[`, "whichvar"))
  obs_valid_idx <- do.call(c, lapply(folds, `[[`, "validation_set"))
  cv_name <- cv_name[order(obs_valid_idx)]

  varname<-cv_name

  return(list(d_eif_est_out, varname))


}

cv_d_eif_singlevar <- function(fold,
                   data_in,
                   uvlearners,
                   w_names) {

  # make training and validation data
  train_data <- origami::training(data_in)
  valid_data <- origami::validation(data_in)
  

d_out <- fit_rule_mech_singlevar(
    train_data = train_data,
    valid_data = valid_data,
    learners = uvlearners,
    w_names = w_names
  )

  # extract components; NOTE: only do this for observations in validation set
  d_est <- d_out$treat_est_valid$treat_pred
  whichvar <- rep(d_out$whichvar, length(d_est))
  # output list
  out <- list(data.table::data.table(
    # components necessary for fluctuation step of TMLE
        D_star = d_est, whichvar=whichvar, fold = origami::fold_index()
  ))
  return(out)
}

fit_rule_mech_singlevar <- function(train_data,
                           valid_data = NULL,
                           learners=sluv,
                           w_names) {
  cov_names <- w_names

  ## construct task for treatment mechanism fit
  treat_task <- sl3::sl3_Task$new(
    data = train_data,
    weights = NULL,
    covariates = cov_names,
    outcome = "D1",
    outcome_type = "continuous"
  )

  ## fit and predict treatment mechanism
  treat_fit <- learners$train(treat_task)
  treat_pred <- treat_fit$predict()
  #selected_vars <- treat_fit$learner_fits[[1]]$learner_fits[[1]]
  whichvartmp<-screen.corRankBest(train_data$D1, as.data.frame(train_data)[,cov_names], method = "kendall", rank = 1)
  whichvar<-names(whichvartmp[whichvartmp==TRUE ])

  ## use full data for prediction if no validation data provided
  if (is.null(valid_data)) {
    out_treat_est <- data.table::as.data.table(treat_pred)
    data.table::setnames(out_treat_est, c(
      "treat_pred"
    ))

    ## output
    out <- list(
      treat_est = out_treat_est,
      treat_fit = treat_fit
    )
  } else {
    out_treat_est <- lapply(
      list(train_data, valid_data),
      function(data) {
        ## create task to generate contrast-specific predictions
        treat_task <- sl3::sl3_Task$new(
          data = data,
          weights = NULL,
          covariates = cov_names,
          outcome = "D1",
          outcome_type = "continuous"
        )

        ## predictions for training data
        treat_pred <- treat_fit$predict(treat_task)

        ## bounding to numerical precision and for positivity considerations

        out_treat_est <- treat_pred

        out_treat_est <- data.table::as.data.table(out_treat_est)
        data.table::setnames(out_treat_est, c(
          "treat_pred"
        ))
       })

    ## output
    out <- list(
      treat_est_train = out_treat_est[[1]],
      treat_est_valid = out_treat_est[[2]],
      treat_fit = treat_fit,
      whichvar = whichvar
    )
  }
  return(out)
}


cv_dclass_eif <- function(fold,
                   data_in,
                   learners,
                   w_names) {

  # make training and validation data
  train_data <- origami::training(data_in)
  valid_data <- origami::validation(data_in)

d_out <- fit_ruleclass_mech(
    train_data = train_data,
    valid_data = valid_data,
    learners = learners,
    w_names = w_names
  )

  # extract components; NOTE: only do this for observations in validation set
  d_est <- d_out$treat_est_valid$treat_pred

  # output list
  out <- list(data.table::data.table(
    # components necessary for fluctuation step of TMLE
        D_star = d_est, fold = origami::fold_index()
  ))
  return(out)
}


est_onestep <- function(data,
                        contrast,
                        g_learners,
                        b_learners,
                        w_names,
                        y_bounds,
                        ext_weights = NULL,
                        folds) {

  # make sure that more than one fold is specified
    # make sure that more than one fold is specified
  #assertthat::assert_that(folds > 1)

  # create cross-validation folds
  #folds <- origami::make_folds(data,
  #  fold_fun = origami::folds_vfold,
  #  V = folds
  #)

  data<-as.data.table(data %>%
  mutate(C1 = contrast))

  # estimate the EIF on a per-fold basis
  cv_eif_results <- origami::cross_validate(
    cv_fun = cv_eif,
    folds = folds,
    data_in = data,
    contrast = contrast,
    g_learners = g_learners,
    b_learners = b_learners,
    w_names = w_names,
    use_future = FALSE,
    .combine = FALSE
  )

  # get estimated efficient influence function
  cv_eif_est <- do.call(c, lapply(cv_eif_results[[1]], `[[`, "D_star"))
  obs_valid_idx <- do.call(c, lapply(folds, `[[`, "validation_set"))
  cv_eif_est <- cv_eif_est[order(obs_valid_idx)]

  # re-scale efficient influence function
  eif_est_rescaled <- cv_eif_est
  #%>%
  #  scale_from_unit(y_bounds[2], y_bounds[1])

  # compute one-step estimate and variance from efficient influence function
  if (is.null(ext_weights)) {
    os_est <- mean(eif_est_rescaled)
    eif_est_out <- eif_est_rescaled
  } else {
    # compute a re-weighted one-step, with re-weighted influence function
    os_est <- stats::weighted.mean(eif_est_rescaled, ext_weights)
    eif_est_out <- eif_est_rescaled * ext_weights
  }
  os_var <- stats::var(eif_est_out) / length(eif_est_out)

  # output
  os_est_out <- list(
    theta = os_est,
    var = os_var,
    eif = eif_est_out,
    type = "onestep"
  )
  return(os_est_out)
}
