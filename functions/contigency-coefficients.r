#' @title Pearson contingency coefficient
#' @description Calculates the Perason coefficient for a contingency table
#' between two categorical variables
#' @param x a table or matrix
#' @return calculated coefficient
#' @export
#' @seealso \code{\link{tschuprow_coef}}, \code{\link{cramer_coef}}
pearson_coef <- function(x)
{
  check_crosstable(x)
  n_total = sum(x)
  chi2d = chi2_stat(x)
  sqrt(chi2d^2 / (n_total + chi2d^2))
}


#' @title Tschuprow coefficient
#' @description Calculates the Tschuprow coefficient for a contingency table
#' between two categorical variables
#' @param x a table or matrix
#' @return calculated coefficient
#' @export
#' @seealso \code{\link{pearson_coef}}, \code{\link{cramer_coef}}
tschuprow_coef <- function(x)
{
  check_crosstable(x)
  n_total = sum(x)
  num_rows = nrow(x)
  num_cols = ncol(x)

  chi2d = chi2_stat(x)
  t_denom = n_total * sqrt((num_rows - 1) * (num_cols - 1))
  sqrt(chi2d^2 / t_denom)
}


#' @title Cramer coefficient
#' @description Calculates the Cramer coefficient for a contingency table
#' between two categorical variables
#' @param x a table or matrix
#' @return calculated coefficient
#' @export
#' @seealso \code{\link{pearson_coef}}, \code{\link{tschuprow_coef}}
cramer_coef <- function(x)
{
  check_crosstable(x)
  n_total = sum(x)
  num_rows = nrow(x)
  num_cols = ncol(x)
  
  chi2d = chi2_stat(x)
  c_denom = n_total * min(num_rows - 1, num_cols - 1)
  (chi2d^2 / c_denom)
}
