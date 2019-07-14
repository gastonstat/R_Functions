#' @title Inner Product Correlation of 2 matrices
#' 
#' @description 
#' Computes the common measure of matrix correlation based on the inner
#' product of two given matrices
#' 
#' @details
#' Matrices \code{X} and \code{Y} must have the same number of rows
#' 
#' @param X a numeric matrix (or data frame)
#' @param Y a numeric matrix (or data frame)
#' @export
#' @examples
#' set.seed(11)
#' A = matrix(runif(12), 4, 4)
#' B = matrix(runif(16), 4, 4)
#' 
#' IPC(A, B)
IPC <- function(X, Y, scale = TRUE)
{
  # check X, Y compatibility
  if (!is.matrix(X)) {X = as.matrix(X)}
  if (!is.matrix(Y)) {Y = as.matrix(Y)}
  check_compatibility(X, Y)
  
  if (scale) {
    X = scale(X, scale = scale)
    Y = scale(Y, scale = scale)
  }
  ab = tr(crossprod(X, Y))
  aa = mnorm(X)
  bb = mnorm(Y)
  inner_prod_corr = ab / (aa * bb)
  inner_prod_corr
}

IPC <- function(X, Y, metricx = NULL, metricy = NULL)
{
  ab = tr(t(X) %*% Mx %*% Y %*% My)
  aa = t(X) %*% Mx %*% X
  bb = t(Y) %*% My %*% Y
  inner_prod_corr = ab / (aa * bb)
  inner_prod_corr
}


#' @title Check Compatibility of two matrices
#' @description Checks compatibility of two matrices
#' @param X a numeric matrix
#' @param Y a numeric matrix
#' @return whether \code{X} is compatible with \code{Y}
#' @example
#' @keywords internal
#' @examples
#' m1 = matrix(1:6, 3, 2)
#' m2 = matrix(7:12, 3, 2)
#' 
#' check_compatibility(m1, m2)  # TRUE
check_compatibility <- function(X, Y) 
{
  if (!is_numeric_tabular(X))
    stop("\n'X' must be a numeric matrix")
  if (!is_numeric_tabular(Y))
    stop("\n'Y' must be a numeric matrix")
  
  if (different_nrow(X, Y))
    stop("\n'X' and 'Y' must have the same number of rows")
  
  # else
  TRUE
}



#' @title Add columns of zeros
#' @description Adds columns of zeros to a given matrix
#' @param x a numeric matrix
#' @param num_col number of columns to add
#' @export
#' @keywords internal
#' @examples
#' M = matrix(1:6, 3, 2)
#' add_column_zeros(M, 2)
add_column_zeros <- function(x, num_col) {
  cbind(x, zeros(nrow(x), num_col))
}


