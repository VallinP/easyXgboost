

##########################################################################################
#' Evaluate a xgboost model through cross validation
#'
#' Patrice Vallin, easyXgboost, Apr 2020
#'
#########################################################################################
#'
#' @param data_DM an xgboost pointer
#' @param xgb_pars a matrix
#'
#' @import magrittr
#' @import stats
#' @import xgboost
#'
#' @return ErrorsHyperparameters
#'
xgboost_cv <- function(data_DM, xgb_pars){

  set.seed(xgb_pars[1, "seed"])
  currentObjective <- xgb_pars[1, "objective"]
  currentEval_metric <- xgb_pars[1, "eval_metric"]

  # run grid search
  system.time(
    ErrorsHyperparameters <- apply(xgb_pars, 1, function(parameterList){

      #Extract Parameters to test
      currentEta <- parameterList[["eta"]]
      currentMinChild <- parameterList[["min_child"]]
      currentDepth <- parameterList[["max_depth"]]
      currentGamma <- parameterList[["gamma"]]
      currentSubsampleRate <- parameterList[["subsample"]]
      currentColsampleRate <- parameterList[["colsample_bytree"]]
      currentLambda <- parameterList[["lambda"]]
      currentAlpha <- parameterList[["alpha"]]

      currentnIter <- parameterList[["nIter"]]

      xgboostModelCV <- xgboost::xgb.cv(data =  data_DM,
                                        nrounds = currentnIter,
                                        nfold = 5,
                                        showsd = TRUE,

                                        "eta" = currentEta,

                                        "min_child_weight" = currentMinChild,
                                        "max.depth" = currentDepth,

                                        "gamma" = currentGamma,

                                        "subsample" = currentSubsampleRate,
                                        "colsample_bytree" = currentColsampleRate,

                                        "lambda" = currentLambda,
                                        "alpha " = currentAlpha,

                                        metrics = currentEval_metric,
                                        eval_metric = currentEval_metric,
                                        objective = currentObjective, #"reg:linear",
                                        booster = "gbtree",
                                        early_stopping_rounds = 10,
                                        print_every_n = 10,
                                        verbose = 0
      )

      xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)

      if(currentEval_metric == "rmse"){
        train_eval <- tail(xvalidationScores$test_rmse_mean, 1)
        test_eval <- tail(xvalidationScores$train_rmse_mean,1)
      } #end if rmse

      if(currentEval_metric == "auc"){
        train_eval <- tail(xvalidationScores$test_auc_mean, 1)
        test_eval <- tail(xvalidationScores$train_auc_mean,1)
      } #end if auc

      bestIter <- xgboostModelCV$best_iteration
      sum_eval <- (train_eval + test_eval)
      output <- return(c(train_eval, test_eval, sum_eval, bestIter,
                         currentEta, currentMinChild, currentDepth,
                         currentGamma, currentSubsampleRate, currentColsampleRate,
                         currentLambda, currentAlpha))
    }) # end xgboost gridsearchcv
  ) # end sys.time

  {
    # xvalidationScores
    ErrorsHyperparameters <- as.data.frame(cbind(t(ErrorsHyperparameters), NA, NA))
    colnames(ErrorsHyperparameters) <- c("train_eval", "test_eval", "sum_eval", "bestIter",
                                         "eta", "min_child", "max_depth",
                                         "gamma", "subsample", "colsample_bytree",
                                         "lambda", "alpha",
                                         "eval_metric", "objective")

    if(currentEval_metric == "rmse"){
      ErrorsHyperparameters <- ErrorsHyperparameters[order(ErrorsHyperparameters[,"sum_eval"]),]
    } # end rmse

    if(currentEval_metric == "auc"){
      ErrorsHyperparameters <- ErrorsHyperparameters[order(ErrorsHyperparameters[,"sum_eval"], decreasing = T),]
    } # end auc

    ErrorsHyperparameters[, "objective"] <- currentObjective
    ErrorsHyperparameters[, "eval_metric"] <- currentEval_metric

  } # end xvalidationscores

  return(ErrorsHyperparameters)

} # end function xgboost_cv


##########################################################################################
#' Construct a xgboost model based on cross validation
#'
#' Patrice Vallin, easyXgboost, Apr 2020
#'
#########################################################################################
#'
#' @param dataset_stim a matrix
#' @param outcome_var an array
#' @param ErrorsHyperparameters a matrix
#' @param classif_type a character string
#' @param seed a numeric value
#' @param k a numeric value
#'
#' @import magrittr
#' @import stats
#' @import caret
#' @import xgboost
#' @importFrom pROC roc multiclass.roc auc
#'
#' @return xgb
#'
prediction_model <- function(dataset_stim, outcome_var,
                             ErrorsHyperparameters, classif_type,
                             seed, k){

  # split train and test
  inTrain <- caret::createDataPartition(y = outcome_var ,
                                        p = as.numeric(levels(ErrorsHyperparameters[1, "subsample"])),
                                        times = k, list = T)

  xgb <- vector(mode = "list", length = k)
  model <- vector(mode = "list", length = k)
  xgb_roc_obj <- vector(mode = "list", length = k)
  pred_score <- rep(NA, times = k)

  for(x1 in c(1:k)){

    # data to train the model
    training_matrix <- data.matrix(dataset_stim[inTrain[[x1]], ])
    Y_train <- outcome_var[inTrain[[x1]] ]
    training_DM <- xgboost::xgb.DMatrix(data = training_matrix, label = Y_train)

    # subset the rest to test
    testing_matrix <- data.matrix(dataset_stim[-inTrain[[x1]], ])
    Y_testing <- outcome_var[-inTrain[[x1]] ]
    testing_DM <- xgboost::xgb.DMatrix(data = testing_matrix, label = Y_testing)

    # set xgbosst pars
    default_param <- list(eta = as.numeric(levels(ErrorsHyperparameters[1, "eta"]))[ErrorsHyperparameters[1,"eta"]],

                          min_child_weight = as.numeric(levels(ErrorsHyperparameters[1, "min_child"]))[ErrorsHyperparameters[1,"min_child"]],
                          max.depth = as.numeric(levels(ErrorsHyperparameters[1, "max_depth"]))[ErrorsHyperparameters[1,"max_depth"]],

                          gamma = as.numeric(levels(ErrorsHyperparameters[1, "gamma"]))[ErrorsHyperparameters[1,"gamma"]],

                          subsample = as.numeric(levels(ErrorsHyperparameters[1, "subsample"]))[ErrorsHyperparameters[1,"subsample"]],
                          colsample_bytree = as.numeric(levels(ErrorsHyperparameters[1, "colsample_bytree"]))[ErrorsHyperparameters[1,"colsample_bytree"]],

                          lambda = as.numeric(levels(ErrorsHyperparameters[1, "lambda"]))[ErrorsHyperparameters[1,"lambda"]],
                          alpha = as.numeric(levels(ErrorsHyperparameters[1, "alpha"]))[ErrorsHyperparameters[1,"alpha"]],

                          #eval_metric = levels(ErrorsHyperparameters[1, "eval_metric"])[ErrorsHyperparameters[1,"eval_metric"]],
                          objective = levels(ErrorsHyperparameters[1, "objective"])[ErrorsHyperparameters[1,"objective"]],
                          booster = "gbtree",
                          nthread = 4,
                          seed = seed
    )


    # train the model using the best iteration found by cross validation
    suppressWarnings(
      xgb[[x1]] <- xgboost::xgb.train(data = training_DM,
                                      params = default_param,
                                      eval_metric = "merror",
                                      nrounds = as.numeric(levels(ErrorsHyperparameters[1,"bestIter"]))[ErrorsHyperparameters[1,"bestIter"]],
                                      print_every_n = 10,
                                      verbose = 0
                                      # num_class = 12,
                                      # save_name = paste0("xgboost.model_",filterPars[["outcome"]][x1], "_", filterPars[["stimulationValues"]][x3])
      ) # end xgb.train
    ) # end suppresswarnings


    # predict values in test set
    xgb_preds <- stats::predict(xgb[[x1]], testing_matrix)

    if(classif_type == "binary"){
      suppressMessages(xgb_roc_obj[[x1]] <- pROC::roc(Y_testing, round(xgb_preds)) )
      pred_score[x1] <- pROC::auc(xgb_roc_obj[[x1]])
    }
    if(classif_type == "multi"){
      suppressMessages(xgb_roc_obj[[x1]] <- pROC::multiclass.roc(Y_testing, xgb_preds) )
      pred_score[x1] <- pROC::auc(xgb_roc_obj[[x1]])
    }
    if(classif_type == "regression"){
      suppressMessages(pred_score[x1] <- caret::RMSE(Y_testing, xgb_preds) )
    }

  } # end for x1

  pred_score_mean <- mean(pred_score)

  if(classif_type == "binary" || classif_type == "multi"){
    cat("XGB mean AUC ", pred_score_mean, "\n")
    best_k <- which(pred_score == max(pred_score))[1]
    cat("XGB max AUC ", pred_score[best_k])
  }

  if(classif_type == "regression"){
    cat("XGB RMSE ", pred_score_mean, "\n")
    best_k <- which(pred_score == min(pred_score))[1]
    cat("XGB min RMSE ", pred_score[best_k])
  }
  message("\n", "\n")

  return(xgb[[best_k]])

} # end function prediction model





##########################################################################################
#' SSE and findElbow function are adapted from FlowSOM R package
#' Sofie Van Gassen et al., Cytometry Part A, 2015
#' https://onlinelibrary.wiley.com/doi/full/10.1002/cyto.a.22625
#' http://bioconductor.org/packages/release/bioc/html/FlowSOM.html
#########################################################################################
#'
#' @param data a array of numeric values
#'
#' @import stats
#'
#' @return optimal
#'
findElbow <- function (data)
{
  n <- length(data)
  data <- as.data.frame(cbind(1:n, data))
  colnames(data) <- c("X", "Y")
  min_r <- Inf
  optimal <- 1
  for (i in 2:(n - 1)) {
    f1 <- stats::lm(Y ~ X, data[1:(i - 1), ])
    f2 <- stats::lm(Y ~ X, data[i:n, ])
    r <- sum(abs(c(f1$residuals, f2$residuals)))
    if (r < min_r) {
      min_r <- r
      optimal <- i
    }
  }
  return(optimal)
}






##################################################################







##########################################################################################
#' Find optimal xgboost parameters through hypertuning
#'
#' Patrice Vallin, easyXgboost, Apr 2020
#'
#########################################################################################
#'
#' @param data_DM an xgboost pointer
#' @param xgb_pars a matrix
#'
#' @import magrittr
#' @import stats
#' @import xgboost
#'
xgboost_hypertuning <- function(data_DM, xgb_pars){

  message("\n", "Hypertuning in progress...")

  # Fix learning rate and number of estimators for tuning tree-based parameters
  tuneHyperpars <- expand.grid(eta = c(.05, .1, .2, .3 ),
                               min_child = 1,
                               max_depth = 6,
                               gamma = 0,
                               subsample = .8,
                               colsample_bytree = 1,
                               lambda = 1,
                               alpha = 1,
                               nIter = 300,
                               objective = xgb_pars[1, "objective"],
                               eval_metric = xgb_pars[1, "eval_metric"],
                               max_feats = xgb_pars[1, "max_feats"],
                               seed = xgb_pars[1, "seed"],
                               explain = xgb_pars[1, "explain"])
  tuneHyperpars


  ErrorsHyperparameters <- xgboost_cv(data_DM, tuneHyperpars)
  ErrorsHyperparameters

  best_eta <- as.numeric(levels(droplevels(ErrorsHyperparameters[1, "eta"])) )

  message(paste0("Best eta (initial) : ", best_eta))


  # Tune max_depth and min_child_weight
  fact_min_child <- 2
  seq_min_child <- seq(2, 8, fact_min_child)
  fact_max_depth <- 2
  seq_max_depth <- seq(3, 9, fact_max_depth)

  for(i in 1:2){
    tuneHyperpars <- expand.grid(eta = best_eta,
                                 min_child = seq_min_child,
                                 max_depth = seq_max_depth,
                                 gamma = 0,
                                 subsample = .8,
                                 colsample_bytree = 1,
                                 lambda = 1,
                                 alpha = 1,
                                 nIter = 300,
                                 objective = xgb_pars[1, "objective"],
                                 eval_metric = xgb_pars[1, "eval_metric"],
                                 max_feats = xgb_pars[1, "max_feats"],
                                 seed = xgb_pars[1, "seed"],
                                 explain = xgb_pars[1, "explain"])
    tuneHyperpars

    ErrorsHyperparameters <- xgboost_cv(data_DM, tuneHyperpars)
    ErrorsHyperparameters

    best_minchild <- as.numeric(levels(droplevels(ErrorsHyperparameters[1, "min_child"])) )
    best_maxdepth <- as.numeric(levels(droplevels(ErrorsHyperparameters[1, "max_depth"])) )
    fact_min_child <- fact_min_child / 2
    fact_max_depth <- fact_max_depth / 2

    seq_min_child <- seq(best_minchild - fact_min_child,
                         best_minchild, best_minchild + fact_min_child)
    seq_max_depth <- seq(best_maxdepth - fact_max_depth,
                         best_maxdepth, best_maxdepth + fact_max_depth)

  } # end for

  message(paste0("Best minchild : ", best_minchild))
  message(paste0("Best maxdepth : ", best_maxdepth))


  # Tune gamma
  fact_gamma <- .5
  seq_gamma <- seq(0, 5, fact_gamma)

  for(i in 1:2){
    tuneHyperpars <- expand.grid(eta = best_eta,
                                 min_child = best_minchild,
                                 max_depth = best_maxdepth,
                                 gamma = seq_gamma,
                                 subsample = .8,
                                 colsample_bytree = 1,
                                 lambda = 1,
                                 alpha = 1,
                                 nIter = 300,
                                 objective = xgb_pars[1, "objective"],
                                 eval_metric = xgb_pars[1, "eval_metric"],
                                 max_feats = xgb_pars[1, "max_feats"],
                                 seed = xgb_pars[1, "seed"],
                                 explain = xgb_pars[1, "explain"])
    tuneHyperpars

    ErrorsHyperparameters <- xgboost_cv(data_DM, tuneHyperpars)
    ErrorsHyperparameters

    best_gamma <- as.numeric(levels(droplevels(ErrorsHyperparameters[1, "gamma"])) )
    fact_gamma <- fact_gamma / 2

    seq_gamma <- c(best_gamma - fact_gamma, best_gamma - (fact_gamma/2),
                     best_gamma, best_gamma + (fact_gamma/2),
                     best_gamma + fact_gamma)

    if(seq_gamma[1] < 0){seq_gamma <- seq_gamma - seq_gamma[1]}

  } # end for

  message(paste0("Best gamma : ", best_gamma))


  # Tune subsample and colsample_bytree
  fact_subsample <- fact_colsample_bytree <- .1
  seq_subsample <- seq(.6, .8, fact_subsample)
  seq_colsample_bytree <- seq(.6, .8, fact_colsample_bytree)

  for(i in 1:1){
    tuneHyperpars <- expand.grid(eta = best_eta,
                                 min_child = best_minchild,
                                 max_depth = best_maxdepth,
                                 gamma = best_gamma,
                                 subsample = seq_subsample,
                                 colsample_bytree = seq_colsample_bytree,
                                 lambda = 1,
                                 alpha = 1,
                                 nIter = 300,
                                 objective = xgb_pars[1, "objective"],
                                 eval_metric = xgb_pars[1, "eval_metric"],
                                 max_feats = xgb_pars[1, "max_feats"],
                                 seed = xgb_pars[1, "seed"],
                                 explain = xgb_pars[1, "explain"])
    tuneHyperpars

    ErrorsHyperparameters <- xgboost_cv(data_DM, tuneHyperpars)
    ErrorsHyperparameters

    best_subsample <- as.numeric(levels(droplevels(ErrorsHyperparameters[1, "subsample"])) )
    best_colsample_bytree <- as.numeric(levels(droplevels(ErrorsHyperparameters[1, "colsample_bytree"])) )

    fact_subsample <- fact_subsample / 2
    fact_colsample_bytree <- fact_colsample_bytree / 2

    seq_subsample <- c(best_subsample / fact_subsample,
                       best_subsample, best_subsample * fact_subsample)
    seq_colsample_bytree <- c(best_colsample_bytree / fact_colsample_bytree,
                              best_colsample_bytree, best_colsample_bytree * fact_colsample_bytree)
    seq_subsample <- as.numeric(levels(as.factor(seq_subsample)))
    seq_colsample_bytree <- as.numeric(levels(as.factor(seq_colsample_bytree)))

  } # end for


  message(paste0("Best subsample : ", best_subsample))
  message(paste0("Best colsample_bytree : ", best_colsample_bytree))


  # Tune alpha & lambda Regularization Parameter
  #seq_alpha <- c(0, 1e-3) #, 1e-2, 0.1, 1)
  #fact_alpha <- fact_lambda <- 10
  #for(i in 1:3){seq_alpha[i+2] <- seq_alpha[i+1] * fact_alpha}
  #seq_lambda <- seq_alpha

  seq_alpha <- seq_lambda <- c(0, 1e-3, 5e-3, 1e-2, 5e-2, 0.1, 0.5, 1, 5)
  fact_alpha <- fact_lambda <- 5

  for(i in 1:3){
    tuneHyperpars <- expand.grid(eta = best_eta,
                                 min_child = best_minchild,
                                 max_depth = best_maxdepth,
                                 gamma = best_gamma,
                                 subsample = best_subsample,
                                 colsample_bytree = best_colsample_bytree,
                                 alpha = seq_alpha,
                                 lambda = seq_lambda,
                                 nIter = 300,
                                 objective = xgb_pars[1, "objective"],
                                 eval_metric = xgb_pars[1, "eval_metric"],
                                 max_feats = xgb_pars[1, "max_feats"],
                                 seed = xgb_pars[1, "seed"],
                                 explain = xgb_pars[1, "explain"])
    tuneHyperpars

    ErrorsHyperparameters <- xgboost_cv(data_DM, tuneHyperpars)
    ErrorsHyperparameters

    best_alpha <- as.numeric(levels(droplevels(ErrorsHyperparameters[1, "alpha"])) )
    best_lambda <- as.numeric(levels(droplevels(ErrorsHyperparameters[1, "lambda"])) )

    fact_alpha <- fact_alpha / 2
    fact_lambda <- fact_lambda / 2

    seq_alpha <- c(best_alpha / fact_alpha, (2*best_alpha) / fact_alpha,
                   best_alpha, best_alpha * (fact_alpha/2),
                   best_alpha * fact_alpha)
    seq_lambda <- c(best_lambda / fact_lambda, (2*best_lambda) / fact_lambda,
                    best_lambda, best_lambda * (fact_alpha/2),
                    best_lambda * fact_lambda)
    seq_alpha <- as.numeric(levels(as.factor(seq_alpha)))
    seq_lambda <- as.numeric(levels(as.factor(seq_lambda)))

  } # end for

  message(paste0("Best alpha : ", best_alpha))
  message(paste0("Best lambda : ", best_lambda))


  # Tune Learning Rate
  seq_eta <- c(.1, .2, .3 )
  fact_eta <- .1

  for(i in 1:3){

    tuneHyperpars <- expand.grid(eta = seq_eta,
                                 min_child = best_minchild,
                                 max_depth = best_maxdepth,
                                 gamma = best_gamma,
                                 subsample = best_subsample,
                                 colsample_bytree = best_colsample_bytree,
                                 alpha = best_alpha,
                                 lambda = best_lambda,
                                 nIter = 300,
                                 objective = xgb_pars[1, "objective"],
                                 eval_metric = xgb_pars[1, "eval_metric"],
                                 max_feats = xgb_pars[1, "max_feats"],
                                 seed = xgb_pars[1, "seed"],
                                 explain = xgb_pars[1, "explain"])
    tuneHyperpars

    ErrorsHyperparameters <- xgboost_cv(data_DM, tuneHyperpars)
    ErrorsHyperparameters

    best_eta <- as.numeric(levels(droplevels(ErrorsHyperparameters[1, "eta"])) )
    fact_eta <- fact_eta / 2

    seq_eta <- c(best_eta - fact_eta, best_eta, best_eta + fact_eta)

  } # end for

  message(paste0("Best eta (final) : ", best_eta))

  message("Last cross-validation result :")
  print(head(ErrorsHyperparameters[1,]))


  return(ErrorsHyperparameters)

} # End xgboost_hypertuning








