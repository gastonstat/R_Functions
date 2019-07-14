#' @title Education vs Reader
#' 
#' @description Contingency table: rows are levels of education,
#' columns are type of readers
#' 
#' @return contingency table
#' @export
#' @seealso \code{\link{independence_table}}, \code{\link{chi2_table}},
#' \code{\link{chi2_contrib}}, \code{\link{sign_table}}
#' @examples
#' # education level -vs- type of reader
#' eduread = edu_read()
#' eduread
edu_read <- function()
{
  # values for contingency table
  edu_lev = c(
    c(5, 18, 19, 12, 3),
    c(7, 46, 29, 40, 7),
    c(2, 20, 39, 49, 16))
  
  # dimnames
  dn = list(Education = paste("E", 1:5, sep = ''),
            Reader = paste("C", 1:3, sep = ''))
  
  # output
  array(edu_lev, dim = c(5, 3), dimnames = dn)
}


#' @title Independence table
#' 
#' @description Calculates the expected values under independence hypothesis
#' of a contingency table
#' 
#' @param x contingency table (or matrix)
#' @return value
#' @export
#' @seealso \code{\link{chi2_table}}, \code{\link{sign_table}},
#' \code{\link{chi2_contrib}}
#' @examples
#' # contingency table
#' eduread = edu_read()
#' 
#' independence_table(eduread)
independence_table <- function(x)
{
  check_crosstable(x)
  n_total = sum(x)  
  row_margin = rowSums(x)
  col_margin = colSums(x)
  expected = outer(row_margin, col_margin, "*") / n_total
  # output
  expected
}


#' @title Chi-Square Contribution table
#' 
#' @description Calculates the values that contribute for the chi-square 
#' statistic of a contingency table
#' 
#' @param x contingency table (or matrix)
#' @return value
#' @export
#' @seealso \code{\link{independence_table}}, \code{\link{chi2_table}},
#' \code{\link{sign_table}}
#' @examples
#' # contingency table
#' eduread = edu_read()
#' 
#' chi2_contrib(eduread)
chi2_contrib <- function(x) 
{
  expected = independence_table(x)
  n_total = sum(x)
  chi2d = sum((x - expected)^2 / expected)
  # output
  (n_total / chi2d) * (x - expected)^2 / expected
}


#' @title Sign table
#' 
#' @description Calculates the sign values 
#' of a contingency table
#' 
#' @param x contingency table (or matrix)
#' @return value
#' @export
#' @seealso \code{\link{independence_table}}, \code{\link{chi2_table}},
#' \code{\link{chi2_contrib}}
#' @examples
#' # contingency table
#' eduread = edu_read()
#' 
#' sign_table(eduread)
sign_table <- function(x)
{
  expected = independence_table(x)
  # output
  sign(x - expected)
}


#' @title Chi-Square table
#' 
#' @description Calculates the chi-square values 
#' of a contingency table
#' 
#' @param x contingency table (or matrix)
#' @return value
#' @export
#' @seealso \code{\link{independence_table}}, \code{\link{sign_table}},
#' \code{\link{chi2_contrib}}
#' @examples
#' # contingency table
#' eduread = edu_read()
#' 
#' chi2_table(eduread)
chi2_table <- function(x)
{
  expected = independence_table(x)
  # output
  (x - expected)^2 / expected
}


#' @title Chi-Square Statistic
#' 
#' @description Calculates the chi-square statistic 
#' of a contingency table
#' 
#' @param x contingency table (or matrix)
#' @return chi-square statistic
#' @export
#' @seealso \code{\link{independence_table}}, \code{\link{sign_table}},
#' \code{\link{chi2_contrib}}
#' @examples
#' # contingency table
#' eduread = edu_read()
#' 
#' chi2_stat(eduread)
chi2_stat <- function(x) 
{
  expected = independence_table(x)
  chi2_statistic = sum((x - expected)^2 / expected)
  # output
  chi2_statistic
}


#' @title Chi-Square Inertia
#' 
#' @description Inertia of a contingency table
#' 
#' @param x contingency table (or matrix)
#' @return inertia
#' @export
#' @seealso \code{\link{independence_table}}, \code{\link{sign_table}},
#' \code{\link{chi2_contrib}}
#' @examples
#' # contingency table
#' eduread = edu_read()
#' 
#' chi2_inertia(eduread)
chi2_inertia <- function(x) 
{
  expected = independence_table(x)
  n_total = sum(x)
  chi2_statistic = sum((x - expected)^2 / expected)
  # output
  chi2_statistic / n_total
}


#file_dir = "/Users/Gaston/Documents/Teaching/Datasets/"
#vaca = read.csv(paste(file_dir, "vacations.csv", sep=""), 
#                row.names=1, header=TRUE)

