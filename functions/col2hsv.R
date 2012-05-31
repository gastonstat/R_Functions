# =========================================================================
# Name:         col2hsv
# Author:       Gaston Sanchez
# Description:  Color to HSV conversion
#               R color to HSV (hue/saturation/value) conversion
#              
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
# =========================================================================

col2hsv <- function(col, alpha=0)
{
    # col: a vector of any kind of R colors
    # alpha: decimal number (0-1) indicating the opacity value
    
    # how many colors
    nc = length(col)

    # convert to RGB and then to HSV
    hsv_aux = rgb2hsv(col2rgb(col))
        
    # reexpress hsv_col in hexadecimal
    hsv_col = rep("", nc)
    for (i in 1:nc)
    {
        hsv_col[i] = hsv(
            h = hsv_aux[1,i], s = hsv_aux[2,i], 
            v = hsv_aux[3,i], alpha = alpha)
    }
    
    # results
    return(hsv_col)
}


## testing "col2hsv"
# generate some colors
some_colors = rainbow(5)

# apply col2hsv
col2hsv(some_colors)

# another example
col2hsv(c("blue", "red", "orange", "green"))
