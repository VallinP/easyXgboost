#' Load & save analysis parameters
#'
#' @param dataFile a character string specifying the location of the project dataset
#'
#' @import magrittr
#'
import_parameters <- function(dataFile){

  ###################################################
  #  Section 2 : Parametres dExtraction des donnees
  ###################################################

  #library("magrittr")

  message(" ")
  message(" ")
  message(rep("#", times = 21))
  message("# Import parameters #")
  message(rep("#", times = 21))
  message(" ")

  projectDir <- dirname(dataFile)

  # Import the dataset
  message("Reading dataset...")
  if(grepl(".csv", dataFile) && !suppressMessages(class(try(data.frame(read.csv(dataFile)), silent=TRUE)) == "try-error")){
    dataset <- suppressMessages(read.csv(file=paste0(dataFile)))
  } else if(grepl(".xlsx", dataFile) && !suppressMessages(class(try(data.frame(readxl::read_xlsx(dataFile)), silent=TRUE)) == "try-error")){
    dataset <- suppressWarnings(as.data.frame(readxl::read_xlsx(dataFile)))
  } else {
    stop("Only csv and xlsx files are accepted.")
  }

  # load metadata
  data2 <- paste0(dirname(dataFile), "/easyXgboost/attachments/xgboost_metadata.csv")
  metadata <- suppressWarnings(as.data.frame(read.csv(file=data2)))
  row.names(metadata) <- metadata[, "parameter"]

  # filter parameters
  filterPars <- vector(mode = "list", length = 12)
  names(filterPars) <- c("ID", "patient_ID", "timepoint", "excludeVars",
                         "outcome", "cohort","valid", "filter_all",
                         "stimulationValues", "stimulationVar", "bin", "ohe")

  filterPars[["ID"]] <- levels(droplevels(metadata[!is.na(metadata[,"ID"]), "parameter"]))
  filterPars[["patient_ID"]] <- levels(droplevels(metadata[!is.na(metadata[,"patient_ID"]), "parameter"]))
  filterPars[["timepoint"]] <- levels(droplevels(metadata[!is.na(metadata[,"timepoint"]), "parameter"]))

  # Outcome variables (as specified in column names of the dataset)
  filterPars[["outcome"]] <- levels(droplevels(metadata[!is.na(metadata[,"filter_outcome"]), "parameter"]))

  # Biological stimulations used for in vitro/ex vivo tests (use the column named "stimulation")
  filterPars[["stimulationValues"]] <- levels(as.factor(dataset[,!is.na(metadata[,"filter_stimulation"])]))

  # filter stimulation column name
  filterPars[["stimulationVar"]] <- levels(droplevels(metadata[!is.na(metadata[,"filter_stimulation"]), "parameter"]))
  if(is.na(filterPars[["stimulationVar"]])){
    stop("No stimulationVar feature defined. Please select one.")
  } else if(length(filterPars[["stimulationVar"]]) >= 2){
    stop("More than one primary ID defined. Please select only one.")
  }

  # Train/Test split
  filterPars[["cohort"]] <- levels(droplevels(metadata[!is.na(metadata[,"filter_cohort"]), "parameter"]))

  # Specify a column name containing logical value to remove indesired raws
  filterPars[["valid"]] <- levels(droplevels(metadata[!is.na(metadata[,"filter_valid"]), "parameter"]))

  # Which col are bin
  lapply(dataset, as.factor) %>%
    lapply(., levels) %>%
    lapply(., length) ->
    len_fact
  bin_test <- (sapply(dataset, is.character) & len_fact == 2)

  # Exclude vars
  #filterPars[["excludeVars"]] <- levels(droplevels(metadata[!is.na(metadata[, "exclude"]),"parameter"]))
  levels(droplevels(metadata[!is.na(metadata[, "exclude"]),"parameter"])) %>%
    .[!. %in% c(filterPars[["ID"]], filterPars[["patient_ID"]], filterPars[["timepoint"]],
                filterPars[["outcome"]], filterPars[["stimulationVar"]], filterPars[["cohort"]],
                filterPars[["valid"]])] ->
    filterPars[["excludeVars"]]

  # which columns to ohe
  levels(droplevels(metadata[metadata$ohe == 1,"parameter"])) %>%
    .[!. %in% filterPars[["excludeVars"]]] ->
    filterPars[["ohe"]]

  # Correct bin
  filterPars[["bin"]] <- names(bin_test[bin_test == TRUE])
  filterPars[["bin"]] <- filterPars[["bin"]][!filterPars[["bin"]] %in% filterPars[["excludeVars"]] ]


  # Check metadata input
  if(is.na(filterPars[["ID"]])){
    stop("No primary ID defined. Please select one.")
  } else if(length(filterPars[["ID"]]) >= 2){
    stop("More than one primary ID defined. Please select only one.")
  } else {
    message(paste0("primary ID feat defined : ", filterPars[["ID"]]))
  }

  if(is.na(filterPars[["patient_ID"]])){
    stop("No patient_ID feature defined. Please select one.")
  } else if(length(filterPars[["patient_ID"]]) >= 2){
    stop("More than one patient_ID feature defined. Please select only one.")
  } else {
    message(paste0("patient ID feat defined : ", filterPars[["patient_ID"]]))
  }

  if(is.na(filterPars[["timepoint"]])){
    message("No timepoint feature defined. Please select one.")
  } else if(length(filterPars[["timepoint"]]) >= 2){
    stop("More than one timepoint feature defined. Please select only one.")
  } else {
    message(paste0("timepoint feat defined : ", filterPars[["timepoint"]]))
  }

  if(is.na(filterPars[["valid"]])){
    stop("No filter_valid feature defined. Please select one.")
  } else if(length(filterPars[["valid"]]) >= 2){
    stop("More than one valid feature defined. Please select only one.")
  } else {
    message(paste0("filter_valid feat defined : ", filterPars[["valid"]]))
  }

  if(is.na(filterPars[["stimulationVar"]])){
    stop("No stimulation feature defined. Please select one.")
  } else if(length(filterPars[["stimulationVar"]]) >= 2){
    stop("More than one stimulation feature defined. Please select only one.")
  } else if(length(filterPars[["stimulationValues"]]) == 0){
    stop("No stimulation value defined for this feature. Please add one level.")
  } else {
    message("stimulation feature defined : ")
    cat(filterPars[["stimulationVar"]], sep = ",")
    message(" ")
    message("stimulation levels defined : ")
    cat(filterPars[["stimulationValues"]], sep = ",")
    message(" ")
  }

  if(sum(is.na(filterPars[["outcome"]])) != 0){
    stop("No outcome feature defined. Please select one.")
  } else {
    message("Outcome feats defined : ")
    cat(filterPars[["outcome"]], sep = ",")
    cat("\n")
  }

  message(paste0("Number of excluded feat defined : ", length(filterPars[["excludeVars"]])))

  message(" ")
  message(" ")


  # All filter column names
  filterPars[["filter_all"]] <- c(filterPars[["ID"]], filterPars[["patient_ID"]], filterPars[["valid"]], filterPars[["cohort"]], filterPars[["stimulationVar"]]) # timepoint,


  # load hyperparameters
  data3 <- paste0(dirname(dataFile),"/easyXgboost/attachments/xgboost_grid_parameters.csv")
  xgbGridPars <- suppressWarnings(as.data.frame(read.csv(file=data3)))
  rownames(xgbGridPars) <- xgbGridPars[,"parameter"]

  xgbGridPars["nIter", c(3:ncol(xgbGridPars))] %>%
    .[!is.na(.)] %>%
    gsub(",", ".", .) %>%
    as.numeric(.) ->
    nIter

  xgbGridPars["seed", c(3:ncol(xgbGridPars))] %>%
    .[!is.na(.)] %>%
    gsub(",", ".", .) %>%
    as.numeric(.) ->
    seed

  xgbGridPars["max_feats", c(3:ncol(xgbGridPars))] %>%
    .[!is.na(.)] %>%
    gsub(",", ".", .) %>%
    as.numeric(.) ->
    max_feats

  xgbGridPars["objective", c(3:ncol(xgbGridPars))] %>%
    .[!is.na(.)] ->
    xgb_objective

  xgbGridPars["eval_metric", c(3:ncol(xgbGridPars))] %>%
    .[!is.na(.)] ->
    xgb_evalmetric

  xgbGridPars["xgboost_explain", c(3:ncol(xgbGridPars))] %>%
    .[!is.na(.)] ->
    xgboost_explain


  # set gridcv pars
  xgb_pars <- as.data.frame(matrix(data = NA, ncol = 6, nrow = 1))
  colnames(xgb_pars) <- c("nIter", "objective", "eval_metric", "max_feats", "seed", "explain")
  xgb_pars$nIter <- nIter
  xgb_pars$objective <- xgb_objective
  xgb_pars$eval_metric <- xgb_evalmetric
  xgb_pars$max_feats <- max_feats
  xgb_pars$seed <- seed
  xgb_pars$explain <- xgboost_explain

  message(paste0("Saving data..."))
  suppressWarnings(
    save(list = c("filterPars", "xgb_pars"),
       file = paste0(projectDir, "/easyXgboost/xgboost_pars.Rdata"),
       compress = "gzip")
  )

}
