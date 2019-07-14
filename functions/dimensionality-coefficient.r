#' @title Dimensionality Coefficient (Beta Coefficient)
#' 
#' @description Calculates the BETA coefficient, that can be taken as an
#' estimation of the dimensionality of a dataset
#' @param x a numeric matrix (or data frame) or an object of class 
#' \code{"triplet"} (see \code{\link{triplet}})
#' @param center whether to center columns
#' @param scale whether to scale columns
#' @export
#' @examples
#' set.seed(9)
#' M = matrix(rnorm(35), 5, 7)
#' 
#' # with a matrix
#' dimensionality(M)
#' 
#' # with a triplet
#' dimensionality(triplet(M))
dimensionality <- function(x, center = TRUE, scale = FALSE) {
  UseMethod("dimensionality", x)
}

#' @S3method dimensionality default
dimensionality.default <- function(x, center = TRUE, scale = FALSE) 
{
  if (!is_numeric_tabular(x))
    stop("\n'dimensionality()' requires a numeric matrix or data frame")    
}

#' @S3method dimensionality matrix
dimensionality.matrix <- function(x, center = TRUE, scale = FALSE) 
{
  if (!is_numeric_tabular(x))
    stop("\n'dimensionality()' requires a numeric matrix or data frame")
  # BETA coeff
  dimensionality_coefficient(x, center = center, scale = scale)
}

#' @S3method dimensionality data.frame
dimensionality.data.frame <- function(x, center = TRUE, scale = FALSE) 
{
  if (!is_numeric_tabular(x))
    stop("\n'dimensionality()' requires a numeric matrix or data frame")
  # BETA coeff
  dimensionality_coefficient(x, center = center, scale = scale)
}

#' @S3method dimensionality triplet
dimensionality.triplet <- function(x, center = TRUE)
{
  # extract datasets
  X = x$data  
  # center datasets
  if (center) {
    X = scale(X, scale = FALSE)
  }
  # associated metrics to X
  Qx = diag(x$w_col, ncol(X), ncol(X))
  Dx = diag(x$w_row, nrow(X), nrow(X))  
  # matrix scalar product
  Wx = X %*% Qx %*% t(X) %*% Dx
  # BETA coeff
  beta_numer = (tr(Wx)) ^ 2
  beta_denom = tr(Wx %*% Wx)
  beta_numer / beta_denom
}

# internal function
dimensionality_coefficient <- function(x, center = TRUE, scale = FALSE)
{
  if (has_missing(x))
    stop("\n'x' contains missing values")
  if (is_one_dim(x)) return(1)
  # eigendecomp
  evs = eigen_values(x, center = center, scale = scale)
  beta_denom = sum(evs^2)
  E = tcrossprod(evs)
  beta_numer = 2 * sum(E[lower.tri(E)])
  # beta coefficient (unidimensinoality coefficient)
  1 + beta_numer / beta_denom
}

