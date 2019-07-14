#' @title Procrustes Similarity Index
#' 
#' @description Calculates the Procrustes Similarity Index between two 
#' configuration matrices
#' @details matrices \code{x} and \code{y} must have the same number of rows
#' @param x a numeric matrix or data frame
#' @param y a numeric matrix or data frame
#' @param center whether to apply column centering 
#' @param scale whether to apply column scaling
#' @return value of procrustes similarity
#' @export
#' @seealso \code{\link{RV_simple}}
#' @examples
#' set.seed(33)
#' A = matrix(rnorm(35), 7, 5)
#' B = matrix(rnorm(49), 7, 7)
#' 
#' # PSI
#' procrustes_similarity(A, B)
procrustes_similarity <- function(x, y, center = TRUE, scale = FALSE)
{
  if (!is_numeric_tabular(x))
    stop("\n'x' must be a numeric matrix or data frame")
  if (is_one_dim(x))
    stop("\n'x' cannot be a one dimensional object")
  if (has_missing(x))
    stop("\n'x' contains missing values")  
  if (!is_numeric_tabular(y))
    stop("\n'y' must be a numeric matrix or data frame")
  if (is_one_dim(y))
    stop("\n'y' cannot be a one dimensional object")
  if (has_missing(y))
    stop("\n'y' contains missing values")
  if (different_nrow(x, y))
    stop("\n'x' and 'y' must have same number of rows")
  
  X = scale(x, center = center, scale = scale)
  Y = scale(y, center = center, scale = scale)
  
  # add columns of zeros to the smaller matrix
  if (ncol(X) != ncol(Y))
  {
    if (ncol(x) > ncol(y)) {
      Y = cbind(Y, zeros(nrow(Y), ncol(X) - ncol(Y)))
    } else {
      X = cbind(X, zeros(nrow(X), ncol(Y) - ncol(X)))
    }
  }
  
  # optimal rotation matrix H
  SVD = svd(t(Y) %*% X)
  H = SVD$u %*% t(SVD$v)
  # Procrustes Similarity Index
  numer = tr(t(X) %*% Y %*% H)
  trace_XtX = sum(apply(X, 2, var) * (ncol(X)-1))
  trace_YtY = sum(apply(Y, 2, var) * (ncol(Y)-1))
  denom = sqrt(trace_XtX * trace_YtY)
  # output
  numer / denom
}
