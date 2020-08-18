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
#     Min-Max normalization
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

minmax = function( x, na.rm = FALSE ) 
{
#    if( !methods::is( x )[ 1 ] %in% c( "integer", "numeric" ) ) stop( "Categorical variable not support" )

    if( !is.vector( x ) & !is.matrix( x ) & !is.data.frame( x ) ) stop( " x must be a vector, matrix, or dataframe" )
    if( is.data.frame( x ) ) x = data.matrix( x )
    
    if( any( is.na( x ) ) & ( na.rm == FALSE ) )
        na.rm = TRUE
    
    if( is.vector( x ) ){
        x_mm = ( x - min( x, na.rm = na.rm ) ) / ( max( x, na.rm = na.rm ) - min( x, na.rm = na.rm ) )
    }
    
    if( is.matrix( x ) ){
        x_mm = t( ( t( x ) - apply( x, 2, min, na.rm = na.rm ) ) / ( apply( x, 2, max, na.rm = na.rm ) - apply( x, 2, min, na.rm = na.rm ) ) )
    }

    return( x_mm )
}
   
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |









