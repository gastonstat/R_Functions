# =========================================================================
# Function:     tryTolower
# Author:       Gaston Sanchez
# Date:         May, 2012
# Description:  Sometimes, when we apply function "tolower" to a given
#               text we may get an error message about an invalid input.
#               To overcome this issue we can use the function "tryCatch"
#               to catch possible errors. The function "tryTolower" 
#               will return an "NA" value when an error occurs.
#
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
# =========================================================================


tryTolower <- function(x)
{
   # x: character string

   # create empty 'NA' object
   # this is where the returned value will be
   y = NA
   # tryCatch error
   try_error = tryCatch(tolower(x), error = function(e) e)
   # if not an error
   if (!inherits(try_error, "error))
       y = tolower(x)
   # result
   return(y)
}

# tryTolower a character vector:
# use "sapply" when working with a character vector
sapply(text_vector, function(x) tryTolower(x))

