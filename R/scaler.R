## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
#     Copyright (C) 2020  Reza Mohammadi & Kevin Burke                         |
#                                                                              |
#     This file is part of 'liver' package.                                    |
#                                                                              |
#     liver is free software: you can redistribute it and/or modify it under   |
#     the terms of the GNU General Public License as published by the Free     |
#     Software Foundation; see <https://cran.r-project.org/web/licenses/GPL-3>.|
#                                                                              |
#     Maintainer: Reza Mohammadi <a.mohammadi@uva.nl>                          |
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
#     Z-score normalization
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

scaler <- function(x, scale = c("minmax", "zscore"), col = "auto", 
                   par1 = NULL, par2 = NULL, na.rm = FALSE)
{
    scale <- match.arg(scale)
    
    if(scale == "minmax") {
        scaled_data <- liver::minmax(x = x, col = col, min = par1, max = par2, na.rm = na.rm)
    } else if (scale == "zscore") {
        scaled_data <- liver::zscore(x = x, col = col, mean = par1, sd = par2, na.rm = na.rm)
    }
    
    return(scaled_data)
}
  
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |








