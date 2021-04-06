## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
#     Copyright (C) 2020 - 2021  Reza Mohammadi & Kevin Burke                  |
#                                                                              |
#     This file is part of 'liver' package.                                    |
#                                                                              |
#     liver is free software: you can redistribute it and/or modify it under   |
#     the terms of the GNU General Public License as published by the Free     |
#     Software Foundation; see <https://cran.r-project.org/web/licenses/GPL-3>.|
#                                                                              |
#     Maintainer: Reza Mohammadi <a.mohammadi@uva.nl>                          |
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
#     Plot a Confusion Matrix
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

conf.mat.plot = function( pred, actual, cutoff = NULL, conf.level = 0, 
                          margin = 1, color = c( "#ff83a8", "#83ff9b" ), ... )
{
    if( length( pred ) != length( actual ) )
        stop( "prod & actual must have the same length" )
    
    if( !is.null( cutoff ) ){
        if( ( cutoff < 0 ) || ( cutoff > 1 ) ) stop( " Value of 'cutoff' must be between 0 and 1." )
        
        levels = base::levels( as.factor( actual ) )
        if( length( levels )  < 2 ) stop( " 'actual' must have more than two levels." )
        if( length( levels ) != 2 ) stop( " For the case 'cutoff!=NULL', 'actual' must have two levels." )
        
        if( ( levels[ 1 ] == 1 ) | ( levels[ 1 ] == "no" ) | ( levels[ 1 ] == "No" ) | ( levels[ 1 ] == FALSE ) )
            levels = c( levels[ 2 ], levels[ 1 ] ) 
        
        cat( paste( c( "Setting levels: control = \"", levels[ 1 ], "\", case = \"", levels[ 2 ],"\"  \n" ), collapse = "" ) ) 
        
        pred = ifelse( pred >= cutoff, levels[ 1 ], levels[ 2 ] )
        
        pred   = factor( pred  , levels = levels )
        actual = factor( actual, levels = levels )
    }
    
    prediction = pred
    graphics::fourfoldplot( table( prediction, actual ), conf.level = conf.level, 
                            margin = margin, color = color, ... )
}
   
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |




