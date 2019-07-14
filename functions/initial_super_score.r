#' @title Initial Super Score
#' 
#' @description Computes starting values for a super score
#' 
#' @param value optional vector provided by the user for the arbitrary initial 
#' super score. If \code{NULL} then a vector of ones is used. 
#' If \code{NA} then a random normal vector is used.
#' @param num_obs number of observations
#' @export
#' @keywords internal
#' @examples
#' # vector of ones
#' initial_super_score(NULL, 5)
#' 
#' # random normal
#' initial_super_score(NA, 5)
#' 
#' # user provided
#' initial_super_score(1:5, 5)
initial_super_score <- function(value, num_obs)
{
  if (is.null(value)) {
    # vector of ones
    unit_score = rep(1, num_obs)
    super_score = normalize(unit_score)
  } else {
    if (is.na(value)) {
      # random normal vector
      random_score = rnorm(num_obs)
      super_score = normalize(random_score) 
    } else {
      # user provided vector
      if (length(value) != num_obs)
        stop("\n'initial' values don't match number of rows in Data")
      super_score = normalize(value)      
    }
  }
  # output
  super_score
}
