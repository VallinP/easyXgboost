##########################################################################################
#' Explain xgboost models
#'
#' Patrice Vallin, easyXgboost, Apr 2020
#'
#########################################################################################
#'
#' @param dataFile
#'
#' @import stats
#' @import caret
#' @import xgboost
#' @import xgboostExplainer
#'
xgboost_explain <- function(dataFile){

  library("magrittr")

  cat('\n')
  message(rep("#", times = 28))
  message("# Resolve xgboostExplainer #")
  message(rep("#", times = 28))
  cat("\n")

  # Select a dataset
  message("Loading dataset & parameters...", "\n")
  projectDir <- dirname(dataFile)
  filename <- basename(dataFile)
  load(paste0(projectDir, "/easyXgboost/xgboost_analysis.Rdata"))


  # Xgboost explainer
  if(xgb_pars[1, "objective"] == "binary:logistic" ||xgb_pars[1, "objective"] == "reg:squarederror"){
    if(as.logical(xgb_pars[1, "explain"])){

      message("Explain xgboost model...")

      if(xgb_pars[1, "objective"] == "binary:logistic"){
        classif_type <- "binary"
      } else if(xgb_pars[1, "objective"] == "reg:squarederror"){
        classif_type <- "regression"
      } else {
        stop("Only binary:logistic and reg:squarederror objectives are supported currently.")
      }

      x1 <- 1 # variable detude
      x3 <- 1 # stimulation/biological condition
      x4 <- 1

      pdf(file = paste0(projectDir, "/easyXgboost/xgboostExplainer.pdf"), width = 20, height = 6)

      for (x1 in 1:length(filterPars[["outcome"]])) {

        for (x3 in  1:length(filterPars[["stimulationValues"]])) {

          message(rep("-", times = 30))
          message(paste0("Variable ", filterPars[["outcome"]][x1]))
          message(paste0("Biological condition ", filterPars[["stimulationValues"]][x3]))
          message(rep("-", times = 30))
          message(" ")

          for(x4 in 1:3){

            message("Model ", x4)
            if(x4 == 1){
              feat_n <- feat_n0[[x1]][[x3]]
              feat_names <- feat_names0[[x1]][[x3]]
              xgb <- xgb1[[x1]][[x3]]
              xgbcv_performance <- xgbcv_performance1[[x1]][[x3]]
            }
            if(x4 == 2){
              feat_n <- feat_n2[[x1]][[x3]]
              feat_names <- feat_names2[[x1]][[x3]]
              xgb <- xgb2[[x1]][[x3]]
              xgbcv_performance <- xgbcv_performance2[[x1]][[x3]]
            }
            if(x4 == 3){
              feat_n <- feat_n_max[[x1]][[x3]]
              feat_names <- feat_names_max[[x1]][[x3]]
              xgb <- xgb3[[x1]][[x3]]
              xgbcv_performance <- xgbcv_performance3[[x1]][[x3]]
            }

            # dataset
            dataset_stim <- df_all_combined[
              df_all_combined[, filterPars[["ID"]] ] %in% xgb_ID[[x1]][[x3]],
              feat_names]

            # split train and test
            inTrain <- caret::createDataPartition(y = outcome_var[[x1]][[x3]],
                                                  p = as.numeric(levels(xgbcv_performance[1, "subsample"])),
                                                  times = 1, list = FALSE)

            # data to train the model
            training_matrix <- data.matrix(dataset_stim[inTrain, xgb$feature_names]) #feat_names])
            Y_train <- outcome_var[[x1]][[x3]][inTrain]
            training_DM <- xgboost::xgb.DMatrix(data = training_matrix, label = Y_train)

            # subset the rest to test
            testing_matrix <- data.matrix(dataset_stim[-inTrain, xgb$feature_names]) #feat_names])
            Y_testing <- outcome_var[[x1]][[x3]][-inTrain]
            testing_DM <- xgboost::xgb.DMatrix(data = testing_matrix, label = Y_testing)

            xgb_explainer[[x1]][[x3]][[x4]] <- xgboostExplainer::buildExplainer(xgb.model = xgb,
                                                              trainingData = training_DM,
                                                              type = classif_type, #"binary",
                                                              base_score = 0.5
                                                              #n_first_tree = 5
            )

            xgb_pred_breakdown[[x1]][[x3]][[x4]] <- xgboostExplainer::explainPredictions(xgb.model = xgb,
                                                                       explainer = xgb_explainer[[x1]][[x3]][[x4]],
                                                                       data = testing_DM)


            cat('Breakdown Complete','\n')
            weights = rowSums(xgb_pred_breakdown[[x1]][[x3]][[x4]])
            pred.xgb = 1/(1+exp(-weights))
            xgb.preds = predict(xgb, testing_DM)
            cat(max(xgb.preds-pred.xgb),'\n')

            ####### IMPACT AGAINST VARIABLE VALUE
            if(x4 == 3){
              i <- 1
              for(i in 1:feat_n){
                cr <- colorRamp(c("blue", "red"))
                plot(testing_matrix[,feat_names[i]], as.data.frame(xgb_pred_breakdown[[x1]][[x3]][[x4]])[, feat_names[i]],
                     #col = rgb(cr(round(testing_matrix[,feat_names[i]])), max=255),
                     cex=0.4, pch=16,
                     xlab = paste0(feat_names[i]),
                     ylab = paste0(feat_names[i], " impact on log-odds"))

              } # end for i

            } # end if x4 == 3


            xgb_waterfall[[x1]][[x3]][[x4]] <- xgboostExplainer::showWaterfall(xgb.model = xgb,
                                                             explainer = xgb_explainer[[x1]][[x3]][[x4]],
                                                             DMatrix = testing_DM,
                                                             data.matrix = testing_matrix,
                                                             idx = 2,
                                                             type = classif_type
            )
            xgb_waterfall[[x1]][[x3]][[x4]][["data"]][["x"]]
            xgb_waterfall[[x1]][[x3]][[x4]][["data"]][["y"]]


            # Extract breakpoint
            breakpointVar <- colnames(testing_matrix)
            xgb_breakpoint[[x1]][[x3]][[x4]] <- array(data = NA, dim = length(breakpointVar))
            names(xgb_breakpoint[[x1]][[x3]][[x4]]) <- breakpointVar

            for(i in 1:length(breakpointVar)){
              valIndex <- grep(paste(breakpointVar[i]), xgb_waterfall[[x1]][[x3]][[x4]][["data"]][["x"]])[1]
              suppressWarnings(xgb_breakpoint[[x1]][[x3]][[x4]][i] <- as.numeric(gsub(paste0(breakpointVar[i], "..."), "", as.character(xgb_waterfall[[x1]][[x3]][[x4]][["data"]][["x"]][valIndex]), perl = F)))
            }

            message("xgb_breakpoint first 6 values :")
            print(head(xgb_breakpoint[[x1]][[x3]][[x4]]))
            cat("\n")

          } # end for x4

        } # end for x3

      } # end for x1

      dev.off()


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


    } # end if xgboost_explain

  } else {
    message(" ")
    message(rep("#", times = 81))
    message("#", rep(" ", times = 21), "Xgboost explainer was not performed.", rep(" ", times = 22), "#")
    message("# Only binary:logistic and reg:squarederror objectives are supported currently. #")
    message(rep("#", times = 81))
    message(" ")

  } # end if test classif type


} # end xgboost explainer function
