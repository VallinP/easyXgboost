#' Prepare the dataset
#'
#' @param dataFile a character string specifying the location of the project dataset
#'
#' @import magrittr
#' @import stats
#' @import caret
#' @import mice
#' @import VIM
#'
#'
prepare_dataset <- function(dataFile){

  {
    message("\n", "\n", rep("#", times = 23))
    message("# Prepare the dataset #")
    message(rep("#", times = 23), "\n")


    ###################################################
    #  Section 2 : Parametres dExtraction des donnees
    ###################################################

    library("caret")
    library("nlme")
    library("magrittr")
    library("mice")

    # Select a dataset
    projectDir <- dirname(dataFile)
    filename <- basename(dataFile)

    # Import the dataset
    message("Reading dataset...")
    if(grepl(".csv", dataFile) && !suppressMessages(class(try(data.frame(read.csv(dataFile)), silent=TRUE)) == "try-error")){
      dataset <- suppressMessages(read.csv(file=paste0(dataFile)))
    } else if(grepl(".xlsx", dataFile) && !suppressMessages(class(try(data.frame(readxl::read_xlsx(dataFile)), silent=TRUE)) == "try-error")){
      dataset <- suppressWarnings(as.data.frame(readxl::read_xlsx(dataFile)))
    } else {
      stop("Provided csv file invalid")
    }

    cat(paste0("Dataset dimension : ", dim(dataset)[1], " rows, ", dim(dataset)[2], " features."), "\n", "\n")

    # Load parameters
    load(paste0(projectDir, "/easyXgboost/xgboost_pars.Rdata"))

    # filter parameters
    filterPars

    # xgboost parameters
    xgb_pars

  } # End initialization


  ######################################
  # Data Cleaning & Feature Engineering
  ######################################
  {
    message("Cleaning dataset")

    #  Sort data by ID       ### (ie. #sort by var1 (ascending) and var2 (descending) newdata <- olddata[order(var1, -var2),] )
    dataset <- as.data.frame(dataset[order(dataset[,filterPars[["ID"]]]),])

    # remove excludeVars cols from the dataset
    cat(paste0("Removing "), summary(as.logical(colnames(dataset) %in% filterPars[["excludeVars"]]))[3], " features (as defined in metadata)", "\n")
    dataset <- data.frame(dataset[,!colnames(dataset) %in% filterPars[["excludeVars"]] ])

    # remove rows based on the selected feature in metadata : filter_valid
    filter_rows <- dataset[,filterPars[["valid"]]] == 1
    filter_rows[is.na(filter_rows)] <- 0
    if(TRUE %in% levels(as.factor(is.na(filter_rows)))){ stop("NA values detected during the sort of one or more variables")}

    dataset <- data.frame(dataset[filter_rows == 1,])
    cat(paste0("Removing ", summary(as.logical(filter_rows))[2], " rows (as defined by the feature ", filterPars[["valid"]], ")"), "\n")
    cat(paste0("Dataset dimension : ", dim(dataset)[1], " rows, ", dim(dataset)[2], " features."), "\n", "\n")

  } # end remove excluded cols and rows from the dataset

  ##################################################
  ## Recode binary features
  ##################################################
  # one-hot-encoding categorical features
  bin_feats <- filterPars[["bin"]]
  bin_feats


  # Check there is more at least two levels in each categorical feat. (otherwise they are excluded)
  i <-  length(bin_feats)
  if(length(bin_feats) > 0){
    message("Converting binary features...")

    dataset2 <- as.data.frame(dataset[, filterPars[["bin"]] ])
    dataset2 <- apply(dataset2, 2,
                      function(x) {
                        lev <- levels(as.factor(x))
                        test0 <- (x == lev[1])
                        test1 <- (x == lev[2])
                        x[test0] <- 0
                        x[test1] <- 1
                        x <- as.numeric(x)
                        return(x)
                      })
    dataset[, colnames(dataset2)] <- dataset2
    suppressWarnings(rm(dataset2))

    cat(bin_feats, "\n")

  } else {
    message("No binary features to convert...")
  } # end convert bin feats


  ##################################################
  ## Convert categorical features using ohe method
  ##################################################
  {
    # one-hot-encoding categorical features
    filterPars[["ohe"]] %>%
      .[!. %in% filterPars[["bin"]]] ->
      ohe_feats
    ohe_feats


    # Check there is more at least two levels in each categorical feat. (otherwise they are excluded)
    i <-  length(ohe_feats)
    if(length(ohe_feats) > 0){

      # Check there is 3+ levels in categorical feat (exclude the feat from categorical list, if not the case)
      message("Converting categorical features...")
      i <- 2
      ohe_feats2 <- ohe_feats
      for (i in length(ohe_feats2):1){
        if(length(levels(factor(dataset[,ohe_feats2[i]]))) < 3 ){
          dataset[,ohe_feats2[i]] <- NULL
          ohe_feats2 <- ohe_feats2[!ohe_feats2 %in% ohe_feats2[i]]
        } # end if
      } # end for

      if(length(ohe_feats) > length(ohe_feats2)){
        message("Feature(s) not OHE : ")
        print(c(ohe_feats[!ohe_feats %in% ohe_feats2]))
        message("because binary/null type. Please consider to revise csv file metadata description.", "\n")
      }

      ohe_feats <- ohe_feats2

      if(length(ohe_feats) > 0){
        # Proceed to ohe
        formula <- stats::as.formula(paste("~", ohe_feats[1], "+", paste(ohe_feats[c(2:length(ohe_feats))], collapse = " + ")))
        df_all <- dataset

        dummies <- caret::dummyVars(formula, data = df_all)
        df_all_ohe <- as.data.frame(stats::predict(dummies, newdata = df_all))
        df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)
        message("Features OHE :")
        cat(paste0(ohe_feats, "\n"))

      } else {
        # if zero categorical variable, keep the original dataset
        message("No categorical features to convert...", "\n")
        df_all_combined <- dataset

      } # end if length ohe_feats >0 2nd test

    } else {
      # if zero categorical variable, keep the original dataset
      message("No categorical features to convert...", "\n")
      df_all_combined <- dataset

    } # end if length ohe_feats > 0 1st test

    suppressWarnings(rm(dataset, df_all_ohe, df_all, dummies))
    cat(paste0("Dataset dimension : ", dim(df_all_combined)[1], " rows, ", dim(df_all_combined)[2], " features."), "\n", "\n")

  } # end ohe


  {
    # Remove rows containing NA values in filter features
    df <- as.data.frame(df_all_combined[,filterPars[["filter_all"]]])
    filter_NA <- df[,filterPars[["ID"]]] %in% na.omit(df)[,filterPars[["ID"]]]
    df_all_combined <- df_all_combined[ filter_NA,]

    # Todo : same for other filterPars : ID_patient, Timepoint, stimulation, outcome ....
    # Todo : print rows index removed

    cat(paste0(length(filter_NA[!filter_NA]), " rows removed, due to NA values in filter features."), "\n", "\n")
    dim(df_all_combined)
    suppressWarnings(rm(df, filter_NA))
  } # end Remove rows containing NA values in filter features


  {
    # Remove empty columns
    currentNcol <- dim(df_all_combined)[2]
    currentColnames <- colnames(df_all_combined)
    df_all_combined <- Filter(function(x)!all(is.na(x)), df_all_combined)
    cat(paste0(currentNcol - dim(df_all_combined)[2], " empty features removed."), "\n")

    if(length(currentColnames) != length(colnames(df_all_combined))){
      cat("Features removed: ")
      cat(paste0(currentColnames[!currentColnames %in% colnames(df_all_combined)], " , "), "\n")
    }

    cat("\n")

  } # end Remove empty columns


  # Impute Missing data
  {

    # Search for na values
    sink(tempfile())
    suppressWarnings(mice_plot <- VIM::aggr(df_all_combined, col=c('navyblue','yellow'),
                                            numbers=TRUE, sortVars=TRUE,
                                            labels=names(df_all_combined), cex.axis=.7,
                                            gap=3, ylab=c("Missing data","Pattern"))
    )
    sink()

    na_col <- mice_plot$missings$Variable[mice_plot$missings$Count > 0]
    length(na_col)

    study_col <- colnames(df_all_combined)[!colnames(df_all_combined) %in% c(filterPars$ID, filterPars$patient_ID, filterPars$valid, filterPars$stimulationVar)]
    filter_col <- c(filterPars$ID, filterPars$patient_ID, filterPars$valid, filterPars$stimulationVar)
    study_df <- df_all_combined[, study_col]
    filter_df <- df_all_combined[, filter_col]

    message("Computing features correlation...")

    # Compute correlations
    suppressWarnings({
      corr_res <- stats::cor(study_df[,na_col],
                             study_df[,!study_col %in% na_col],
                             use = "pairwise.complete.obs")
    })
    head(summary(corr_res))[, c(1:6)]
    #heatmap(corr_res)

    # define functions
    colMax <- function(data) sapply(data, max, na.rm = TRUE)
    colMin <- function(data) sapply(data, min, na.rm = TRUE)

    # Find strongest correlated features - TRUE if correlation > 0.8
    corr_max <- colMax(data = as.data.frame(corr_res))
    corr_min <- colMin(data = as.data.frame(corr_res))
    corr_abs <- sort(abs(c(corr_max, corr_min)))

    # Retrieve the correlated feat names
    corr_feat <- unique(names(corr_abs[corr_abs > .3]))
    length(corr_feat)

    if(length(corr_feat) > 30){
      corr_feat <- corr_feat[1:30]
    }

    # Intialize MICE
    message("Imputing dataset...")
    #library(mice)
    init = mice(df_all_combined, maxit=0)
    meth = init$method
    predM = init$predictorMatrix

    #To impute the missing values, mice package use an algorithm in a such a way that use information from other variables in dataset to predict and impute the missing values. Therefore, you may not want to use certain variable as predictors. For example the ID variable does not have any predictive value.
    #The code below will remove the variable as predictor but still will be imputed. Just for illustration purposes I select the BMI variable to not be included as predictor during imputation.
    predM[, colnames(df_all_combined)[!colnames(df_all_combined) %in% c(na_col, corr_feat)]] = 0

    #If you want to skip a variable from imputation use the code below. Keep in mind that this variable will be used for prediction.
    #meth[c("Age")]=""

    #Now let specify the methods for imputing the missing values. There are specific methods for continues, binary and ordinal variables. I set different methods for each variable. You can add more than one variable in each methods.
    meth[names(meth)[names(meth) %in% na_col]]="pmm" # "norm
    meth[names(meth)[(names(meth) %in% na_col) & (names(meth) %in% bin_feats)]]="logreg"
    meth[names(meth)[(names(meth) %in% na_col) & (names(meth) %in% ohe_feats)]]="polyreg"

    #Now it is time to run the multiple (m=5) imputation.
    set.seed(123456)
    suppressWarnings({
      imputed = mice(df_all_combined, method=meth, predictorMatrix=predM, m=5, maxit = 5)
    })


    # Create a dataset after imputation.
    suppressWarnings(imputed_df <- complete(imputed))
    dim(df_all_combined)
    dim(imputed_df)
    colnames(imputed_df)[!colnames(imputed_df) %in% colnames(df_all_combined)]


    # Check for missings in the imputed dataset.
    message(paste0("Initial dataset contains :", sum(sapply(df_all_combined, function(x) sum(is.na(x)))), " NA values"))
    sum(sapply(study_df, function(x) sum(is.na(x))))
    #sum(sapply(imputed, function(x) sum(is.na(x))))
    message(paste0("Imputed dataset contains :", sum(sapply(imputed_df, function(x) sum(is.na(x)))), " NA values", "\n"))


    dim(df_all_combined)
    dim(imputed_df)

    df_all_combined <- imputed_df

    # Todo : if missing values still present, replace by mean

  } # end impute


  # Data normalization
  if(F){
    message("Normalizing numeric features...")
    grepFold <- function (foldIndex, folds, featNames){featId <- grep(folds[[foldIndex]], featNames)}

    folds <- as.list(c(ohe_feats, bin_feats))
    featIds <- sapply(1:length(folds), grepFold,
                      folds = folds,
                      featNames=colnames(imputed_df))

    colNotScaled <- c(filterPars$ID,
                      filterPars$patient_ID,
                      filterPars$timepoint,
                      filterPars$outcome,
                      filterPars$cohort,
                      filterPars$valid,
                      filterPars$stimulationVar,
                      colnames(imputed_df)[featIds])
    colToScale <- colnames(df_all_combined)[!colnames(df_all_combined) %in% colNotScaled]

    dfNotScaled <- imputed_df[, colNotScaled]

    dfScaled <- colScale(x = imputed_df[, colToScale],
                         center = TRUE,
                         scale = TRUE,
                         add_attr = TRUE,
                         rows = NULL,
                         cols = NULL)

    df_all_combined2 <- cbind(dfNotScaled, dfScaled)
    row.names(df_all_combined2) <- row.names(df_all_combined)

    df_all_combined <- df_all_combined2
    head(df_all_combined[, c(1:6)])

    cat("\n")
  } # end normalization


  message(paste0("Saving data..."))
  suppressWarnings(
    save(list = c("dataFile", "df_all_combined",
                "filterPars", "xgb_pars"),
       file = paste0(projectDir, "/easyXgboost/xgboost_analysis.Rdata"),
       compress = "gzip")
  )

}

