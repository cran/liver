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
#     Skim a data frame to get useful summary statistics
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

skim = function( data, hist = TRUE, ... )
{
    if( hist == TRUE ) 
        skimr::skim( data = data, ... )
    else
        skimr::skim_without_charts( data = data, ... )
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |








