#' @title Triplet for Data Analysis
#' 
#' @description Create a triplet object: data, row metric, column metric
#' 
#' @param x a numetric matrix or data frame
#' @param w_row vector of row weights
#' @param w_col vector of column weights
#' @return an object of class \code{"triplet"}
#' @export
#' @examples
#' set.seed(55)
#' X = matrix(runif(50), 10, 5)
#' triplet(X)
triplet <- function(x, w_row = NULL, w_col = NULL)
{
  if (!is_numeric_tabular(x))
    stop("\n'triplet' requires a numeric matrix or data frame")
  
  # check vector of row weights
  if (!is.null(w_row)) {
    if (length(w_row) != nrow(x))
      stop("\nlength of 'w_row' differs from nrow(x)")
  } else {
    w_row = rep(1, nrow(x)) / nrow(x)
  }
  # check vector of column weights
  if (!is.null(w_col)) {
    if (length(w_col) != ncol(x))
      stop("\nlength of 'w_col' differs from ncol(x)")
  } else {
    w_col = rep(1, ncol(x))
  }
  
  # output
  structure(
    list(data = x, w_row = w_row, w_col = w_col),
    class = "triplet")
}
