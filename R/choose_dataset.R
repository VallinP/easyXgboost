#'#########################################################################################
#' Function to choose a project dataset
#'
#' Patrice Vallin, easyXgboost, Apr 2020.
#'########################################################################################
#'
#' @import magrittr
#' @import utils
#' @importFrom tcltk tk_choose.dir
#'
#' @param caption a character string
#'
#' @return dir

choose_dataset <- function(caption = "Select a dataset") {
  setwd("~/")

  if (exists('choose.files', where='package:utils', mode='function')) {
    dir <- utils::choose.files(caption)
  } else {
    dir <- tcltk::tk_choose.files(caption)
  }
  return(dir)
}
