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

kNN = function( formula, train, test, k = 1, transform = FALSE, l = 0, prob = FALSE, use.all = TRUE ) 
{
    formula = stats::as.formula( formula )
    
    mf  = stats::model.frame( formula, data = train )
    mt  = attr( mf, "terms" )
    mt2 = stats::delete.response( mt )
    
    cl = stats::model.response( mf )
    
    learn = stats::model.matrix( mt, mf    )[ , -1 ]
    valid = stats::model.matrix( mt2, test )[ , -1 ]
    
    if( transform == TRUE ) transform = "minmax"
    
    if( transform != FALSE )
    {
        if( !is.vector( learn ) ){
            data_all = rbind( learn, valid )
            data_all = liver::transform( data_all, method = transform )
            
            learn = data_all[    1:nrow( learn )  , ]
            valid = data_all[ -( 1:nrow( learn ) ), ]
        }else{
            data_all = c( learn, valid )
            data_all = liver::transform( data_all, method = transform )
            
            learn = data_all[    1:length( learn )   ]
            valid = data_all[ -( 1:length( learn ) ) ]
        }
    }
    
    class::knn( train = learn, test = valid, cl = cl, k = k, l = l, 
                prob = prob, use.all = use.all )
}
    
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |








