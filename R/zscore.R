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

zscore = function( x, na.rm = FALSE ) 
{
#    if( !methods::is( x )[ 1 ] %in% c( "integer", "numeric" ) ) stop( "Categorical variable not support" )

    if( !is.vector( x ) & !is.matrix( x ) & !is.data.frame( x ) ) stop( " x must be a vector, matrix, or dataframe" )

    data_frame = FALSE
    if( is.data.frame( x ) ){
        data_frame = TRUE
        x = data.matrix( x )
    }
    
    if( any( is.na( x ) ) & ( na.rm == FALSE ) )
        na.rm = TRUE
    
    if( is.vector( x ) ){
        z = ( x - mean( x, na.rm = na.rm ) ) / sd( x, na.rm = na.rm )
    }
    
    if( is.matrix( x ) ){
        if( nrow( x ) == 1 ) stop( " x, for the case of matrix, must have more than 1 row." )
        
        z = t( ( t( x ) - apply( x, 2, mean, na.rm = na.rm ) ) / apply( x, 2, stats::sd, na.rm = na.rm ) )
        
        z[ is.na( z ) ] = 0
    }
    
    if( data_frame == TRUE ) z = as.data.frame( z )

    return( z )
}
   
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |








