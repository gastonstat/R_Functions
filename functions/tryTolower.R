# =========================================================================
# Name:         tryTolower
# Author:       Gaston Sanchez
# Description:  This function helps to catch possible errors when
#               applying "tolower" to a given text string
#               If there's an error, it returns NA instead of 
#               an error message
#
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
# =========================================================================

tryTolower <- function(x)
{
    # x: character string
    
    y = NA
    # tryCatch error when tolower
    try_error = tryCatch(tolower(x), error = function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
        y = tolower(y)
    # results
    return(y)
}


# use "tryTolower" with "sapply" in case of having a character vector
sapply(some_text, function(x) tryTolower(x))
