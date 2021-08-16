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
#     k-Nearest Neighbour Classification using formula interface
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

kNN = function( formula, train, test, k = 1, transform = FALSE, l = 0, 
                prob = FALSE, use.all = TRUE, na.rm = FALSE ) 
{
    formula = stats::as.formula( formula )
    
    model_frame_train  = stats::model.frame( formula, data = train )
    model_train        = attr( model_frame_train, "terms" )
    model_train_no_res = stats::delete.response( model_train )
    
    train_label = stats::model.response( model_frame_train )
    
    train = stats::model.matrix( model_train, model_frame_train )[ , -1 ]
    test  = stats::model.matrix( model_train_no_res, test )[ , -1 ]
    
    if( transform == TRUE ) transform = "minmax"
    
    if( transform != FALSE )
    {
        if( !is.vector( train ) ){
            data_all = rbind( train, test )
            data_all = liver::transform( data_all, method = transform )
            
            train = data_all[    1:nrow( train )  , ]
            test  = data_all[ -( 1:nrow( train ) ), ]
        }else{
            data_all = c( train, test )
            data_all = liver::transform( data_all, method = transform )
            
            train = data_all[    1:length( train )   ]
            test  = data_all[ -( 1:length( train ) ) ]
        }
    }
    
    output_knn = class::knn( train = train, test = test, cl = train_label, k = k, 
                             l = l, prob = prob, use.all = use.all )
    
    if( prob == TRUE )
    {
        levels_train = levels( output_knn )
        levels_size  = length( levels_train )
        
        row_size = ifelse( !is.vector( test ), nrow( test ), 1 )
        
        prob_all = matrix( ncol = levels_size, nrow = row_size, 
                           dimnames = list( rownames( test ), levels_train ) )
        
        prob_knn = attr( output_knn, "prob" )
        
        for( i in 1:levels_size )
        {
            ind_i = which( output_knn == levels_train[ i ] )
            
            prob_all[ ind_i,  i ] = prob_knn[ ind_i ]
            prob_all[ ind_i, -i ] = ( 1 - prob_knn[ ind_i ] ) / ( levels_size - 1 )
        }
        
        output_knn = prob_all
    }
    
    return( output_knn )
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |








