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
#     Compute a Mean Square Error (MSE)
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

mse = function( pred, actual, weight = 1, na.rm = FALSE ) 
{
    if( length( pred ) != length( actual ) )
        stop( "prod & actual must have the same length" )

    if( !is.numeric( weight ) )
        stop( "weight must be numeric" )

    if( length( weight ) > 1 & length( weight ) != length( pred ) )
        stop( "prod & weight must have the same length" )
    
    if( length( weight ) == 1 ) weight = rep( weight, length( pred ) )
    
    if( !is.numeric( pred   ) ) pred   = as.numeric( pred   )
    if( !is.numeric( actual ) ) actual = as.numeric( actual )
    
    mse_value = stats::weighted.mean( ( pred - actual ) ^ 2, w = weight, na.rm = na.rm )

    return( mse_value )
}
   
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
