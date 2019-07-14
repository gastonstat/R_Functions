#' @title RV Matrix
#' 
#' @description Compute the matrix of RV coefficients
#' 
#' @param x a list of matrices
#' @seealso \code{\link{RV}}
#' @export
#' @examples
#' # a list of matrices
#' A = list(
#'   A1 = matrix(rnorm(n * 3), n, 3),
#'   A2 = matrix(rnorm(n * 4), n, 4),
#'   A3 = matrix(rnorm(n * 2), n, 2))
#'   
#' RV_matrix(A)
RV_matrix <- function(x) 
{
  # how many elements in x
  n = length(x)
  # initialize matrix to store RVs
  RVS = matrix(1, n, n)
  # compute RV coefficients
  for (j in 1L:(n-1)) {
    for (i in (j+1):n) {
      RV_ij = RV(x[[i]], x[[j]])
      RVS[i,j] = RV_ij
      RVS[j,i] = RV_ij
    }
  }
  # output
  RVS
}
