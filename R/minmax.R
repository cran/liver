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
    
    data_frame = FALSE
    if( is.data.frame( x ) ){
        data_frame = TRUE
        x = data.matrix( x )
    }
    
    if( any( is.na( x ) ) & ( na.rm == FALSE ) ) na.rm = TRUE
    
    if( is.vector( x ) ){
        x_mm = ( x - min( x, na.rm = na.rm ) ) / ( max( x, na.rm = na.rm ) - min( x, na.rm = na.rm ) )
    }
    
    if( is.matrix( x ) ){
        if( nrow( x ) == 1 ) stop( " x, for the case of matrix, must have more than 1 row." )
        
        x_mm = t( ( t( x ) - apply( x, 2, min, na.rm = na.rm ) ) / ( apply( x, 2, max, na.rm = na.rm ) - apply( x, 2, min, na.rm = na.rm ) ) )
        
        x_mm[ is.na( x_mm ) ] = x[ is.na( x_mm ) ]
        
        x_mm[ x_mm > 1 ] = 1
        x_mm[ x_mm < 0 ] = 0
    }
    
    if( data_frame == TRUE ) x_mm = as.data.frame( x_mm )
    
    return( x_mm )
}
   
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |







