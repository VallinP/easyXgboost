##########################################################################################
#' Analyse the dataset with xgboost
#'
#' Patrice Vallin, easyXgboost, Apr 2020
#' .
#' SSE and findElbow function are adapted from FlowSOM R package
#' Sofie Van Gassen et al., Cytometry Part A, 2015
#' https://onlinelibrary.wiley.com/doi/full/10.1002/cyto.a.22625
#' http://bioconductor.org/packages/release/bioc/html/FlowSOM.html
#########################################################################################
#'
#' @param dataFile a character string specifying the location of the project dataset
#'
#' @import magrittr
#' @import stats
#' @import caret
#' @import xgboost
#'
xgboost_analysis2 <- function(dataFile){

  message("\n", "\n", rep("#", times = 20))
  message("# Dataset analysis #")
  message(rep("#", times = 20), "\n")


  ###################################################
  #  Step 1 : Load data & settings
  ###################################################

  library("caret")
  library("nlme")
  library("magrittr")
  if(F){
    #library(Rmisc)
    #library(ggplot2)
    #library(RColorBrewer)  #Palettes graphiques : utiliser la commande display.brewer.all()
    #library(lme4)
    #library(arm)
    #library(AICcmodavg)
    #library(nlme)
    #library(car)

    #library(xgboost)
    #library(readr)
    #library(stringr)
    #library(dplyr)
    #library(Ckmeans.1d.dp)
    #library(pROC)
    #library(lattice)

  } # end FALSE

  # Select a dataset
  message("Loading dataset & parameters...", "\n")
  projectDir <- dirname(dataFile)
  filename <- basename(dataFile)
  load(paste0(projectDir, "/easyXgboost/xgboost_analysis.Rdata"))

  # Set pars
  max_feats <- xgb_pars[1, "max_feats"]
  objective <- levels(droplevels(xgb_pars[1, "objective"]))
  eval_metric <- levels(droplevels(xgb_pars[1, "eval_metric"]))
  seed <- xgb_pars[1, "seed"]
  explain <- levels(droplevels(xgb_pars[1, "explain"]))
  set.seed(seed)


  if(xgb_pars[1, "objective"] == "binary:logistic"){
    classif_type <- "binary"
  } else if(xgb_pars[1, "objective"] == "reg:squarederror"){
    classif_type <- "regression"
  } else {
    stop("Only binary:logistic and reg:squarederror objectives are supported currently.")
  }

  ################################################################################
  # Create a training and testing group object
  ################################################################################
  {
    #inTrain <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    #names(inTrain) <- filterPars[["outcome"]]
    xgb_ID <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(xgb_ID) <- filterPars[["outcome"]]
    outcome_var <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(outcome_var) <- filterPars[["outcome"]]

    feat_n0 <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(feat_n0) <- filterPars[["outcome"]]
    feat_names0 <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(feat_names0) <- filterPars[["outcome"]]

    xgb1 <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(xgb1) <- filterPars[["outcome"]]
    model1 <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(model1) <- filterPars[["outcome"]]
    feat_n1 <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(feat_n1) <- filterPars[["outcome"]]
    feat_names1 <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(feat_names1) <- filterPars[["outcome"]]

    xgb2 <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(xgb2) <- filterPars[["outcome"]]
    model2 <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(model2) <- filterPars[["outcome"]]
    feat_n2 <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(feat_n2) <- filterPars[["outcome"]]
    feat_names2 <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(feat_names2) <- filterPars[["outcome"]]

    xgb3 <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(xgb3) <- filterPars[["outcome"]]
    model3 <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(model3) <- filterPars[["outcome"]]

    feat_n_max <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(feat_n_max) <- filterPars[["outcome"]]
    feat_names_max <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(feat_names_max) <- filterPars[["outcome"]]

    elbow_point_res <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(elbow_point_res) <- filterPars[["outcome"]]

    xgbcv_performance1 <- xgbcv_performance2 <- xgbcv_performance3 <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(xgbcv_performance1) <- names(xgbcv_performance2) <- names(xgbcv_performance3) <- filterPars[["outcome"]]

    xgb_explainer <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(xgb_explainer) <- filterPars[["outcome"]]
    xgb_pred_breakdown <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(xgb_pred_breakdown) <- filterPars[["outcome"]]
    xgb_waterfall <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(xgb_waterfall) <- filterPars[["outcome"]]
    xgb_breakpoint <- vector(mode = "list", length = length(filterPars[["outcome"]]))
    names(xgb_breakpoint) <- filterPars[["outcome"]]
  }


  ##########################################################
  # Compute the model and validate it for each filter keep
  ##########################################################
  # set loop parameters
  x1 <- 1 # variable detude
  x3 <- 1 # stimulation/biological condition
  nLoop <- 1

  for (x1 in 1:length(filterPars[["outcome"]])) {

    # set names of lists inTrain, xgb and model
    {
      #inTrain[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      #names(inTrain[[x1]]) <- filterPars[["stimulationValues"]]

      xgb_ID[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(xgb_ID[[x1]]) <- filterPars[["stimulationValues"]]
      outcome_var[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(outcome_var[[x1]]) <- filterPars[["stimulationValues"]]

      feat_n0[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(feat_n0[[x1]]) <- filterPars[["stimulationValues"]]
      feat_names0[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(feat_names0[[x1]]) <- filterPars[["stimulationValues"]]

      xgb1[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(xgb1[[x1]]) <- filterPars[["stimulationValues"]]
      model1[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(model1[[x1]]) <- filterPars[["stimulationValues"]]
      feat_n1[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(feat_n1[[x1]]) <- filterPars[["stimulationValues"]]
      feat_names1[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(feat_names1[[x1]]) <- filterPars[["stimulationValues"]]

      xgb2[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(xgb2[[x1]]) <- filterPars[["stimulationValues"]]
      model2[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(model2[[x1]]) <- filterPars[["stimulationValues"]]
      feat_n2[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(feat_n2[[x1]]) <- filterPars[["stimulationValues"]]
      feat_names2[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(feat_names2[[x1]]) <- filterPars[["stimulationValues"]]

      xgb3[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(xgb3[[x1]]) <- filterPars[["stimulationValues"]]
      model3[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(model3[[x1]]) <- filterPars[["stimulationValues"]]

      feat_n_max[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(feat_n_max[[x1]]) <- filterPars[["stimulationValues"]]
      feat_names_max[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(feat_names_max[[x1]]) <- filterPars[["stimulationValues"]]

      elbow_point_res[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(elbow_point_res[[x1]]) <- filterPars[["stimulationValues"]]

      xgbcv_performance1[[x1]] <- xgbcv_performance2[[x1]] <- xgbcv_performance3[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(xgbcv_performance1[[x1]]) <- names(xgbcv_performance2[[x1]]) <- names(xgbcv_performance3[[x1]]) <- filterPars[["stimulationValues"]]

      xgb_explainer[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(xgb_explainer[[x1]]) <- filterPars[["stimulationValues"]]
      xgb_pred_breakdown[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(xgb_pred_breakdown[[x1]]) <- filterPars[["stimulationValues"]]
      xgb_waterfall[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(xgb_waterfall[[x1]]) <- filterPars[["stimulationValues"]]
      xgb_breakpoint[[x1]] <- vector(mode = "list", length = length(filterPars[["stimulationValues"]]))
      names(xgb_breakpoint[[x1]]) <- filterPars[["stimulationValues"]]
    }


    for (x3 in  1:length(filterPars[["stimulationValues"]])) {

      message(rep("-", times = 30))
      message(paste0("Variable ", filterPars[["outcome"]][x1]))
      message(paste0("Biological condition ", filterPars[["stimulationValues"]][x3]))
      message(rep("-", times = 30), "\n")

      xgbcv_performance1[[x1]][[x3]] <-
        xgbcv_performance2[[x1]][[x3]] <-
        xgbcv_performance3[[x1]][[x3]] <-
        as.data.frame(matrix(data = NA, ncol = 14))

      colnames(xgbcv_performance1[[x1]][[x3]]) <-
        colnames(xgbcv_performance2[[x1]][[x3]]) <-
        colnames(xgbcv_performance3[[x1]][[x3]]) <-
        c("train_eval", "test_eval", "sum_eval",
          "bestIter", "eta", "min_child", "max_depth",
          "gamma", "subsample", "colsample_bytree",
          "lambda", "alpha", "eval_metric", "objective")

      xgb_explainer[[x1]][[x3]] <- vector(mode = "list", length = 3)
      names(xgb_explainer[[x1]][[x3]]) <- c("model1", "model2", "model3")
      xgb_pred_breakdown[[x1]][[x3]] <- vector(mode = "list", length = 3)
      names(xgb_pred_breakdown[[x1]][[x3]]) <- c("model1", "model2", "model3")
      xgb_waterfall[[x1]][[x3]] <- vector(mode = "list", length = 3)
      names(xgb_waterfall[[x1]][[x3]]) <- c("model1", "model2", "model3")
      xgb_breakpoint[[x1]][[x3]] <- vector(mode = "list", length = 3)
      names(xgb_breakpoint[[x1]][[x3]]) <- c("model1", "model2", "model3")


      ##########################################################
      # Select stimulation rows & remove dumy vars & empty cols
      ##########################################################
      {
        # Select rows matching the current stimulation value
        filter_rows <- df_all_combined$stimulation == filterPars[["stimulationValues"]][x3]
        filter_rows[is.na(filter_rows)] <- 0
        if(TRUE %in% levels(as.factor(is.na(filter_rows)))){ stop("NA values detected during the sort of one or more variables")}

        # Store primary ID (& featnames ?)
        xgb_ID[[x1]][[x3]] <- df_all_combined[filter_rows == 1, filterPars[["ID"]]]
        # !colnames(df_all_combined) %in% filterPars[["filter_all"]]

        # Remove dummy vars
        dataset_stim <- data.frame(df_all_combined[filter_rows == 1, !colnames(df_all_combined) %in% filterPars[["filter_all"]]])
        suppressWarnings(rm(filter_rows))

        # Remove empty columns (again)
        currentNcol <- dim(dataset_stim)[2]
        dataset_stim <- Filter(function(x)!all(is.na(x)), dataset_stim)
        cat(paste0(currentNcol - dim(dataset_stim)[2], " empty features removed (at this step)."), "\n")
        cat(paste0("Dataset dimension : ", dim(dataset_stim)[1], " rows, ", dim(dataset_stim)[2], " features."), "\n", "\n")

      } # End select stimulation rows and remove dummy vars


      {
        # store outcome feature
        outcome_var[[x1]][[x3]] <- dataset_stim[, filterPars[["outcome"]][x1]]
        dataset_stim <- dataset_stim[, !colnames(dataset_stim) %in% filterPars[["outcome"]][x1]]
        tail(colnames(dataset_stim))
      }


      ####################################################
      # Step 5: Run an initial Xgboost model
      ####################################################

      feat_n0[[x1]][[x3]] <- ncol(dataset_stim)
      feat_names0[[x1]][[x3]] <- colnames(dataset_stim)
      if(max_feats == "auto"){max_feats <- 0}

      {
        # Create a xgboost matrix containing all the data
        suppressWarnings(rm(list = c("data_matrix", "data_DM")))
        data_matrix <- data.matrix(dataset_stim[, feat_names0[[x1]][[x3]]])
        Y_data <- outcome_var[[x1]][[x3]]
        data_DM <- xgboost::xgb.DMatrix(data = data_matrix, label = Y_data)
      }

      # Hypertuning
      message(paste0("Model 1 : ", feat_n0[[x1]][[x3]], " features"), "\n")
      ErrorsHyperparameters <- easyXgboost:::xgboost_hypertuning(data_DM, xgb_pars)
      xgbcv_performance1[[x1]][[x3]] <- ErrorsHyperparameters

      # Compute k Classification/Regression model and retrieve the one with the best prediction score
      message("Cross validation with bests parameters...")
      easyXgboost:::prediction_model(dataset_stim[, feat_names0[[x1]][[x3]]], outcome_var[[x1]][[x3]],
                                     ErrorsHyperparameters, classif_type,
                                     seed, k = 20) %>%
        xgboost::xgb.Booster.complete(.) ->
        xgb1[[x1]][[x3]]

      # Lets start with finding what the actual tree looks like
      model1[[x1]][[x3]] <- xgboost::xgb.dump(xgb1[[x1]][[x3]], with_stats = T)

      # Compute feature importance matrix
      message("Extraction of the important features")
      importance_matrix <- xgboost::xgb.importance(colnames(dataset_stim), model = xgb1[[x1]][[x3]])

      # keep most important features in the new model
      xgboost::xgb.ggplot.importance(importance_matrix = importance_matrix, top_n = feat_n0[[x1]][[x3]])
      feat.order <- rev(order(importance_matrix$Importance))[c(1:feat_n0[[x1]][[x3]])]

      feat_names1[[x1]][[x3]] <- importance_matrix$Feature[feat.order]
      feat_n1[[x1]][[x3]] <- length(importance_matrix$Importance)
      feat_names1[[x1]][[x3]] <- feat_names1[[x1]][[x3]][c(1:feat_n1[[x1]][[x3]])]


      #####################################################################
      # Step 6: Tune number of features
      #####################################################################
      message("Determining the Elbow point")
      elbow_point_res[[x1]][[x3]] <- importance_matrix$Importance[c(1:feat_n1[[x1]][[x3]])]

      smooth <- 0.2
      for (i in 2:(feat_n1[[x1]][[x3]] - 1)) {
        elbow_point_res[[x1]][[x3]][i] <- (1 - smooth) * elbow_point_res[[x1]][[x3]][i] +
          (smooth/2) * elbow_point_res[[x1]][[x3]][i - 1] +
          (smooth/2) * elbow_point_res[[x1]][[x3]][i + 1]
      }

      elbow_point <- easyXgboost:::findElbow(elbow_point_res[[x1]][[x3]])
      if(max_feats == 0){max_feats <- elbow_point}

      #plot(elbow_point_res[[x1]][[x3]])
      #abline(v = elbow_point, col = "red")

      feat_n2[[x1]][[x3]] <- elbow_point
      feat_names2[[x1]][[x3]] <- feat_names1[[x1]][[x3]][c(1:elbow_point)]
      feat_n2[[x1]][[x3]] <- length(feat_names2[[x1]][[x3]])


      #################################################################################
      # Step 7: Run Xgboost on eblow_point defined number of features, CV_parameters
      #################################################################################
      {
        # Create a xgboost matrix containing all the data
        suppressWarnings(rm(list = c("data_matrix", "data_DM")))
        data_matrix <- data.matrix(dataset_stim[, feat_names2[[x1]][[x3]]])
        data_DM <- xgboost::xgb.DMatrix(data = data_matrix, label = Y_data)
      }

      # Hypertuning
      message(paste0("Model 2 : ", feat_n2[[x1]][[x3]], " features"), "\n")
      ErrorsHyperparameters <- easyXgboost:::xgboost_hypertuning(data_DM, xgb_pars)
      xgbcv_performance2[[x1]][[x3]] <- ErrorsHyperparameters

      # Compute k Classification/Regression model and retrieve the one with the best prediction score
      easyXgboost:::prediction_model(dataset_stim[, feat_names2[[x1]][[x3]]], outcome_var[[x1]][[x3]],
                                     ErrorsHyperparameters, classif_type,
                                     seed, k = 20) %>%
        xgboost::xgb.Booster.complete(.) ->
        xgb2[[x1]][[x3]]

      # Lets start with finding what the actual tree looks like
      model2[[x1]][[x3]] <- xgboost::xgb.dump(xgb2[[x1]][[x3]], with_stats = T)

      # Compute feature importance matrix
      importance_matrix <- xgboost::xgb.importance(feat_names2[[x1]][[x3]], model = xgb2[[x1]][[x3]])

      # keep most important features in the new model
      xgboost::xgb.ggplot.importance(importance_matrix = importance_matrix, top_n = feat_n2[[x1]][[x3]])
      feat.order <- rev(order(importance_matrix$Importance))[c(1:feat_n2[[x1]][[x3]])]
      feat_names3 <- importance_matrix$Feature[feat.order]
      feat_n4 <- feat_n3 <- length(importance_matrix$Importance)
      feat_names3 <- feat_names3[c(1:feat_n3)]
      feat_names4 <- feat_names3[c(1:max_feats)]


      #################################################################################
      # Step 8: Run Xgboost on max_feats defined number of features, CV_parameters
      #################################################################################
      if(feat_n4 > max_feats){
        {
          # Create a xgboost matrix containing all the data
          suppressWarnings(rm(list = c("data_matrix", "data_DM")))
          data_matrix <- data.matrix(dataset_stim[, feat_names4])
          data_DM <- xgboost::xgb.DMatrix(data = data_matrix, label = Y_data)
        }

        # Hypertuning
        message(paste0("Model 3 : ", max_feats, " features"), "\n")
        ErrorsHyperparameters <- easyXgboost:::xgboost_hypertuning(data_DM, xgb_pars)
        xgbcv_performance3[[x1]][[x3]] <- ErrorsHyperparameters

        # Compute k Classification/Regression model and retrieve the one with the best prediction score
        easyXgboost:::prediction_model(dataset_stim[, feat_names4], outcome_var[[x1]][[x3]],
                                       ErrorsHyperparameters, classif_type,
                                       seed, k = 20) %>%
          xgboost::xgb.Booster.complete(.) ->
          xgb3[[x1]][[x3]]

        # Lets start with finding what the actual tree looks like
        model3[[x1]][[x3]] <- xgboost::xgb.dump(xgb3[[x1]][[x3]], with_stats = T)

        # Compute feature importance matrix
        importance_matrix <- xgboost::xgb.importance(feat_names4, model = xgb3[[x1]][[x3]])

        # keep most important features in the new model
        xgboost::xgb.ggplot.importance(importance_matrix = importance_matrix, top_n = feat_n4)
        feat.order <- rev(order(importance_matrix$Importance))[c(1:feat_n4)]
        feat_names5 <- importance_matrix$Feature[feat.order]
        feat_n5 <- length(importance_matrix$Importance)
        feat_names5 <- feat_names5[c(1:feat_n5)]

        feat_names_max[[x1]][[x3]] <- feat_names5
        feat_n_max[[x1]][[x3]] <- feat_n5

      } else {

        xgbcv_performance3[[x1]][[x3]] <- xgbcv_performance2[[x1]][[x3]]
        xgb3[[x1]][[x3]] <- xgb2[[x1]][[x3]]
        model3[[x1]][[x3]] <- model2[[x1]][[x3]]
        feat_n_max[[x1]][[x3]] <- feat_n5 <- feat_n4
        feat_names_max[[x1]][[x3]] <- feat_names5 <- feat_names4

      } # end if feat_n4 > max_feats

    } #end for filter x3

  } #end for filter x1


  message("\n", paste0("Saving data..."))
  suppressWarnings(
    save(list = c("dataFile", "df_all_combined", "outcome_var",
                  "filterPars", "xgb_pars", "xgb_ID",
                  "xgb1", "xgb2", "xgb3",
                  "model1", "model2", "model3",
                  "feat_n0", "feat_n1", "feat_n2", "feat_n_max",
                  "feat_names0", "feat_names1", "feat_names2", "feat_names_max", "max_feats",
                  "xgbcv_performance1", "xgbcv_performance2", "xgbcv_performance3",
                  "elbow_point_res", "xgb_explainer",
                  "xgb_pred_breakdown", "xgb_waterfall", "xgb_breakpoint"),
         file = paste0(projectDir, "/easyXgboost/xgboost_analysis.Rdata"),
         compress = "gzip")
  )


} # end xgboost_analysis2
