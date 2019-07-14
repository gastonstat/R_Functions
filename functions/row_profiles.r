#' @title row and column profiles
#' 
#' @description get the row profiles of a contingency table
#' @param x a two-way tabe (contingency table of frequency)
#' @aliases row_profiles col_profiles
#' @export row_profiles col_profiles
#' @examples
#' # contingency table
#' eduread = edu_read()
#' 
#' # row profiles
#' rp = row_profiles(eduread)
#' rp
#' rowSums(rp)
#' 
#' # column profiles
#' cp = col_profiles(eduread)
#' cp
#' colSums(cp)
row_profiles <- function(x) 
{
  check_crosstable(x)
    
  if (!is.matrix(x))
    x = as.matrix(x)
  
  # row profiles
  prop.table(x, margin = 1)  
}

col_profiles <- function(x) 
{
  check_crosstable(x)
  
  if (!is.matrix(x))
    x = as.matrix(x)
  
  # column profiles
  prop.table(x, margin = 2)  
}


freq_table <- function(x) 
{
  check_crosstable(x)
  
  if (!is.matrix(x))
    x = as.matrix(x)
  
  # column profiles
  prop.table(x)  
}


#' @title Check contigency table
#' 
#' @description Check that an object is a good contigency table
#' @param x an R object
#' @return TRUE if a is a contingency table
#' @export
#' @keywords internal
check_crosstable <- function(x) 
{
  if (is_not_tabular(x))
    stop("\n'x' must be a matrix, table, or data frame")
  if (is_one_dim(x))
    stop("\n'x' is a one-dimensional object")
  
  if (any(x < 0) || any(is.na(x))) 
    stop("\nthere are nonnegative and/or finite entries")
  if (sum(x) == 0) 
    stop("\n'sum(x)' is zero")
  # else
  TRUE
}
