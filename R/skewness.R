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
#     Compute a skewness of each field
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

skewness = function( x, na.rm = FALSE ) 
{
    if( !is.vector( x ) & !is.matrix( x ) & !is.data.frame( x ) ) stop( " x must be a vector, matrix, or dataframe" )
    if( is.data.frame( x ) ) x = data.matrix( x )

    if( any( is.na( x ) ) & ( na.rm == FALSE ) ) na.rm = TRUE
    
    if( is.vector( x ) ){
        skew = 3 * ( mean( x, na.rm = na.rm ) - median( x, na.rm = na.rm ) ) / sd( x, na.rm = na.rm )
    }
    
    if( is.matrix( x ) ){
        skew = 3 * ( apply( x, 2, mean, na.rm = na.rm ) - apply( x, 2, median, na.rm = na.rm ) ) / apply( x, 2, sd, na.rm = na.rm )
     }
    
    return( skew )
}
   
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

