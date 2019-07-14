#' @title Inertia
#' 
#' @description Calculates the inertia of a matrix
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
#' inertia(M)
#' 
#' # with a triplet
#' inertia(triplet(M))
inertia <- function(x, center = TRUE, scale = FALSE) {
  UseMethod("inertia", x)
}

#' @S3method inertia default
inertia.default <- function(x, center = TRUE, scale = FALSE) 
{
  if (!is_numeric_tabular(x))
    stop("\n'inertia()' requires a numeric matrix or data frame")    
}

#' @S3method inertia matrix
inertia.matrix <- function(x, center = TRUE, scale = FALSE)
{
  if (!is_numeric_tabular(x))
    stop("\n'inertia()' requires a numeric matrix or data frame")    

  X = scale(x, center = center, scale = scale)

  # --------------------------
  ## inertia (matrix algebra formula)
  # Sx = tcrossprod(X)
  # inertia = tr(Sx) / nrow(X)
  # --------------------------
  
  # correction factor
  correction = (nrow(x) - 1) / nrow(x)
  # inertia (faster formula)
  sum(apply(x, 2, var) * correction)
}

#' @S3method inertia data.frame
inertia.data.frame <- function(x, center = TRUE, scale = FALSE)
{
  if (!is_numeric_tabular(x))
    stop("\n'inertia()' requires a numeric matrix or data frame")    

  X = scale(x, center = center, scale = scale)
  # --------------------------
  ## inertia (matrix algebra formula)
  # Sx = tcrossprod(X)
  # inertia = tr(Sx) / nrow(X)
  # --------------------------

  # correction factor
  correction = (nrow(x) - 1) / nrow(x)
  # inertia (faster formula)
  sum(apply(x, 2, var) * correction)
}

#' @S3method inertia triplet
inertia.triplet <- function(x, center = TRUE)
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
  # inertia
  tr(Wx)
}
