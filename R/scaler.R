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

scaler = function(x, method = c("minmax", "zscore"), columns = NULL, na.rm = FALSE) 
{
    method = match.arg(method)
    
    if(method == "minmax") 
		x_trans = liver::minmax(x = x, columns = columns, na.rm = na.rm)
		
    if(method == "zscore") 
		x_trans = liver::zscore(x = x, columns = columns, na.rm = na.rm)
    
    return(x_trans)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |








