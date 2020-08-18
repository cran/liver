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
#     Partition a dataset
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

partition = function( data, prob = c( 0.7, 0.3 ) )
{
    if( !is.matrix( data ) & !is.data.frame( data ) ) stop( " data must be a matrix, or dataframe" )
    
    length_prob = length( prob )
    
    if( length_prob > nrow( data ) ) stop( "length of prob must be smaller or equal to number of observations." )

    ind = sample( length_prob, nrow( data ), replace = TRUE, prob = prob )
    
    if( length_prob == 1 ){
        result = data[ ind == 1, ]
    }
    
    if( length_prob == 2 ){
        part1 = data[ ind == 1, ]
        part2 = data[ ind == 2, ]
        
        result = list( part1 = part1, part2 = part2 )
    }
        
    if( length_prob == 3 ){
        part1 = data[ ind == 1, ]
        part2 = data[ ind == 2, ]
        part3 = data[ ind == 3, ]
        
        result = list( part1 = part1, part2 = part2, part3 = part3 )
    }
    
    if( length_prob == 4 ){
        part1 = data[ ind == 1, ]
        part2 = data[ ind == 2, ]
        part3 = data[ ind == 3, ]
        part4 = data[ ind == 4, ]
        
        result = list( part1 = part1, part2 = part2, part3 = part3, part4 = part4 )
    }

    if( length_prob == 5 ){
        part1 = data[ ind == 1, ]
        part2 = data[ ind == 2, ]
        part3 = data[ ind == 3, ]
        part4 = data[ ind == 4, ]
        part5 = data[ ind == 5, ]
        
        result = list( part1 = part1, part2 = part2, part3 = part3, part4 = part4, part5 = part5 )
    }
    
    return( result )    
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |








