#' @title RV Coefficient (Simple version)
#' 
#' @description The RV Coefficient is a measure of similarity between 
#' two matrices (same number of rows, different number of columns)
#' 
#' @param X numeric matrix of dimension \code{n,p}
#' @param Y numeric matrix of dimension \code{n,q}
#' @param scale whether to scale matrices \code{X} and \code{Y}
#' @return RV coefficient
#' @seealso \code{\link{RV_full}}
#' @export RV_simple RV
#' @aliases RV_simple RV
#' @examples
#' set.seed(33)
#' A = matrix(runif(12), 4, 3)
#' B = matrix(rnorm(12), 4, 3)
#' 
#' RV_simple(A, B)
#' RV_simple(A, A)  # equal to 1
#' 
#' # synonym function
#' RV(A, B)
RV_simple <- function(X, Y, scale = FALSE)
{
  if (is_one_dim(X)) X = as.matrix(X)
  if (missing(Y))
    stop("\n'Y' is missing for 'RV_simple()'")
  if (is_one_dim(Y)) Y = as.matrix(Y)
  
  if (!is_numeric_tabular(X))
    stop("\n'RV_simple()' requires 'X' to be a numeric matrix")
  if (!is_numeric_tabular(Y))
    stop("\n'RV_simple()' requires 'Y' to be a numeric matrix")
  if (nrow(X) != nrow(Y))
    stop("\n'RV_simple()' requires 'X' and 'Y' to have same number of rows")
  
  # center data
  X = scale(X, scale = scale)
  Y = scale(Y, scale = scale)
  
  # outer products
  # 'tcrossprod()' slightly faster than 'X %*% t(X)'
  Sx = tcrossprod(X)
  Sy = tcrossprod(Y)
  
  # RV coefficient
  RV_numerator = tr(Sx %*% Sy)
  RV_denom = sqrt(tr(Sx %*% Sx) * tr(Sy %*% Sy))
  # output
  RV_numerator / RV_denom
}

RV <- function(X, Y, scale = FALSE) {
  RV_simple(X, Y, scale = scale)
}


#' @title RV Coefficient (Full Version)
#' 
#' @description This function calculates an RV coefficient based on two provided
#' triplets: (X, Qx, Dx) and (Y, Qy, Dy)
#' 
#' @param x_triplet an object of class \code{"triplet"} 
#' (see \code{\link{triplet}})
#' @param y_triplet an object of class \code{"triplet"}
#' (see \code{\link{triplet}})
#' @return RV coefficient
#' @seealso \code{\link{RV_simple}}
#' @export
#' @examples
#' set.seed(33)
#' A = matrix(runif(12), 4, 3)
#' B = matrix(rnorm(12), 4, 3)
#' 
#' # synonym function
#' RV_full(triplet(A), triplet(B))
RV_full <- function(x_triplet, y_triplet)
{
  if (class(x_triplet) != "triplet")
    stop("\n'RV_full()' requires 'x_triplet' to be of class 'triplet'")
  if (missing(y_triplet))
    stop("\n'y_triplet' is missing for 'RV_full()'")
  if (class(y_triplet) != "triplet")
    stop("\n'RV_full()' requires 'y_triplet' to be of class 'triplet'")
  
  # extract datasets
  X = x_triplet$data
  Y = y_triplet$data
  
  # compatibility between X and Y?
  if (nrow(X) != nrow(Y))
    stop("\n'x_triplet' is not compatible with 'y_triplet'")
  
  # center datasets
  X = scale(X, scale = FALSE)
  Y = scale(Y, scale = FALSE)
  
  # associated metrics to X
  Qx = diag(x_triplet$w_col, ncol(X), ncol(X))
  Dx = diag(x_triplet$w_row, nrow(X), nrow(X))
  # associated metrics to Y
  Qy = diag(y_triplet$w_col, ncol(Y), ncol(Y))
  Dy = diag(y_triplet$w_row, nrow(Y), nrow(Y))
  
  # Escouffier operators (i.e. matrix scalar products)
  Wx = X %*% Qx %*% t(X) %*% Dx
  Wy = Y %*% Qy %*% t(Y) %*% Dy
  
  # RV coefficient
  tr(Wx %*% Wy) / sqrt(tr(Wx %*% Wx) * tr(Wy %*% Wy)) 
}


#' @title Scalar-Valued Variance 
#' @description Scalar-Valued Variance of a matrix
#' @param X a numeric matrix
#' @param scale whether to scale the columns of \code{X}
#' @return The scalar-valued variance
#' @seealso \code{\link{RV}}, \code{\link{COVV}}
#' @export
#' @examples
#' set.seed(33)
#' A = matrix(runif(12), 4, 3)
#' 
#' # get VAV 
#' VAV(A)
VAV <- function(X, scale = TRUE) 
{
  if (!is_numeric_matrix(X))
    stop("\n'VAV()' requires 'X' to be a numeric matrix")
  
  X = scale(X, scale = scale)
  # outer product
  # 'tcrossprod()' slightly faster than 'X %*% t(X)'
  XX = tcrossprod(X)
  # output
  tr(XX %*% XX)
}


#' @title Scalar-Valued Covariance 
#' @description Scalar-Valued Covariance of two matrices
#' @param X a numeric matrix
#' @param Y a numeric matrix
#' @param scale whether to scale columns of \code{X} and \code{Y}
#' @return The scalar-valued Covariance 
#' @export
#' @seealso \code{\link{RV}}, \code{\link{VAV}}
#' @examples
#' set.seed(33)
#' A = matrix(runif(12), 4, 3)
#' B = matrix(rnorm(12), 4, 3)
#' 
#' # get COVV 
#' COVV(A, B)
COVV <- function(X, Y, scale = TRUE) 
{
  if (!is_numeric_matrix(X))
    stop("\n'COVV()' requires 'X' to be a numeric matrix")
  if (!is_numeric_matrix(Y))
    stop("\n'COVV()' requires 'Y' to be a numeric matrix")
  if (nrow(X) != nrow(Y))
    stop("\n'COVV()' requires 'X' and 'Y' to have same number of rows")
  
  X = scale(X, scale = scale)
  Y = scale(Y, scale = scale)
  
  # outer product
  # 'crossprod(X,Y)' slightly faster than 't(X) %*% Y'
  XY = crossprod(X, Y)
  YX = crossprod(Y, X)
  # output
  tr(XY %*% YX)
}
