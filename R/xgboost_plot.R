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
#' @import ggplot2
#' @import RColorBrewer
#' @import xgboost
#'
xgboost_plot <- function(dataFile){

  library("magrittr")

  cat('\n')
  message(rep("#", times = 29))
  message("# Print xgboost plot report #")
  message(rep("#", times = 29))
  cat("\n")

  # Select a dataset
  message("Loading dataset & parameters...", "\n")
  projectDir <- dirname(dataFile)
  filename <- basename(dataFile)
  load(paste0(projectDir, "/easyXgboost/xgboost_analysis.Rdata"))

  # initialize pdf printer
  pdf(file = paste0(projectDir, "/easyXgboost/xgboost_plot.pdf"), width = 20, height = 6)

  x1 <- 1 # variable detude
  x3 <- 1 # stimulation/biological condition
  x4 <- 3

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
          feat_n <- length(colnames(df_all_combined)) #feat_n1[[x1]][[x3]]
          feat_names <- colnames(df_all_combined) #feat_names1[[x1]][[x3]]
          xgb <- xgb1[[x1]][[x3]]
          model <- model1[[x1]][[x3]]
          xgbcv_performance <- xgbcv_performance1[[x1]][[x3]]
        }
        if(x4 == 2){
          feat_n <- feat_n2[[x1]][[x3]]
          feat_names <- feat_names2[[x1]][[x3]]
          xgb <- xgb2[[x1]][[x3]]
          model <- model2[[x1]][[x3]]
          xgbcv_performance <- xgbcv_performance2[[x1]][[x3]]
        }
        if(x4 == 3){
          feat_n <- feat_n_max[[x1]][[x3]]
          feat_names <- feat_names_max[[x1]][[x3]]
          xgb <- xgb3[[x1]][[x3]]
          model <- model3[[x1]][[x3]]
          xgbcv_performance <- xgbcv_performance3[[x1]][[x3]]
        }

        # dataset
        dataset_stim <- df_all_combined[
          df_all_combined[, filterPars[["ID"]] ] %in% xgb_ID[[x1]][[x3]],
          feat_names]

        # Compute feature importance matrix
        importance_matrix <- xgboost::xgb.importance(feat_names, model = xgb)
        # keep most important features in the new model
        xgboost::xgb.ggplot.importance(importance_matrix = importance_matrix, top_n = feat_n)
        # Nice graph
        #xgb.plot.importance(importance_matrix[1:10,])
        plot <- xgboost::xgb.ggplot.importance(importance_matrix = importance_matrix, top_n = feat_n)
        plot <- plot +
          ggtitle(paste0(filterPars[["outcome"]][x1], " ", filterPars[["stimulationValues"]][x3], " Model ", x4)) +
          ggplot2::aes(width = 0.5)
        print(plot)

        #In case last step does not work for you because of a version issue, you can try following :
        #barplot(importance_matrix[,1])

        # Plot the tree
        if(x4 == 3){
        #print(xgboost::xgb.plot.tree(model = xgboost::xgb.Booster.complete(xgb)))
        } # end x4 == 2

        if(xgb_pars[1,"explain"] == TRUE){plot(xgb_waterfall[[x1]][[x3]][[x4]])}


        # Plot Elbow point
        if(x4 == 2){
          plot(elbow_point_res[[x1]][[x3]])
          abline(v = feat_n, col = "red")
        } # end x4 == 2

      } # end for x4

    } # end for x3

  } # end for x1

  dev.off()

} # end xgboost_plot
