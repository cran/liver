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

partition = function( data, prob = c( 0.7, 0.3 ), set.seed = NULL )
{
    if( !is.matrix( data ) & !is.data.frame( data ) ) stop( " data must be a matrix, or dataframe" )
    
    if( !is.null( set.seed ) ) set.seed( set.seed )
    
    length_prob = length( prob )
    
    if( length_prob > nrow( data ) ) stop( "length of prob must be smaller or equal to number of observations." )
    
    ind = sample( length_prob, nrow( data ), replace = TRUE, prob = prob )
    
    if( length_prob == 1 ){
        partitions = data[ ind == 1, ]
    }else{
        name_list = vector( length = length_prob )
        partitions = list()
        
        for( i in 1:length_prob ){
            partitions[[ i ]] = data[ ind == i, ]
            
            name_list[ i ] = paste( c( "part", i ), collapse = "" )
        }
        
        names( partitions ) = name_list
    }
    
    return( partitions )    
}
   
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |








