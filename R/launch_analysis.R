#' Launch xgboost analysis workflow
#'
#' @import magrittr
#'
#' @export
#'
launch_analysis <- function(){

  caption.value <- "Select your project directory"
  dataFile <- easyXgboost:::choose_dataset(caption.value)

  easyXgboost:::import_parameters(dataFile)

  easyXgboost:::prepare_dataset(dataFile)

  easyXgboost:::xgboost_analysis2(dataFile)

  # to do : seperate plot by outcome, stimulation
  easyXgboost:::xgboost_explain(dataFile)

  easyXgboost:::xgboost_plot(dataFile)

  # statistic analysis (pam, sam, glmnet, ridge reg, LMM)

  # report (prediction plots with auc, xgboost explainer plot, violin plot with pval, )

  message("\n", "\n", rep("=", times = 30), "\n",
          "# xgboost analysis completed #", "\n",
          rep("=", times = 30), "\n", "\n")

} #end launch_analysis function
