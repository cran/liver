## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
#     Copyright (C) 2020  Reza Mohammadi & Kevin Burke                         |
#                                                                              |
#     This file is part of liver package.                                    |
#                                                                              |
#     liver is free software: you can redistribute it and/or modify it under   |
#     the terms of the GNU General Public License as published by the Free     |
#     Software Foundation; see <https://cran.r-project.org/web/licenses/GPL-3>.|
#                                                                              |
#     Maintainer: Reza Mohammadi <a.mohammadi@uva.nl>                          |
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
#     Find missing values (NA)
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

find.na = function( x ) 
{
    if( !is.vector( x ) & !is.matrix( x ) & !is.data.frame( x ) ) stop( " x must be a vector, matrix, or dataframe" )
    
    ind = which( is.na( x ), arr.ind = TRUE )
    
    if( length( ind ) == 0 ) print( "There is no any missing values (NA)." )
    
    return( ind )
}
   
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

