#'#########################################################################################
#' scale dataset
#'
#' Adapted from : https://www.r-bloggers.com/a-faster-scale-function/
#'########################################################################################
#'
#' @param dataFile a character string specifying the location of the project dataset
#'
#' @import matrixStats
#'
colScale = function(x,
                    center = TRUE,
                    scale = TRUE,
                    add_attr = TRUE,
                    rows = NULL,
                    cols = NULL) {

  library(matrixStats)

  if (!is.null(rows) && !is.null(cols)) {
    x <- x[rows, cols, drop = FALSE]
  } else if (!is.null(rows)) {
    x <- x[rows, , drop = FALSE]
  } else if (!is.null(cols)) {
    x <- x[, cols, drop = FALSE]
  }

  ################
  # Get the column means
  ################
  cm = colMeans(x, na.rm = TRUE)
  ################
  # Get the column sd
  ################
  if (scale) {
    csd = colSds(x, center = cm)
  } else {
    # just divide by 1 if not
    csd = rep(1, length = length(cm))
  }
  if (!center) {
    # just subtract 0
    cm = rep(0, length = length(cm))
  }
  x = t( (t(x) - cm) / csd )
  if (add_attr) {
    if (center) {
      attr(x, "scaled:center") <- cm
    }
    if (scale) {
      attr(x, "scaled:scale") <- csd
    }
  }
  return(x)
}
