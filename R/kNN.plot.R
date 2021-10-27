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

kNN.plot = function( formula, train, test, k.max = 10, transform = FALSE, 
                     base = "error", set.seed = NULL, ... ) 
{
    if( k.max < 2 ) stop( "  k.max must be > 1" )
    
    if( !is.null( set.seed ) ) set.seed( set.seed )
    
    formula = stats::as.formula( formula )
    
    model_frame_train = stats::model.frame( formula, data = train )
    model_train       = attr( model_frame_train, "terms" )
    
    train_label = stats::model.response( model_frame_train )
    train       = stats::model.matrix( model_train, model_frame_train )[ , -1 ]
    
    model_frame_test = stats::model.frame( formula, data = test )
    model_test       = attr( model_frame_test, "terms" )
    
    test_label = stats::model.response( model_frame_test )
    test       = stats::model.matrix( model_test, model_frame_test )[ , -1 ]
    
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
    
    k_list   = 1:k.max
    base_list = vector( length = k.max  )
    
    for( k in k_list ){
        knn_k = class::knn( train = train, test = test, cl = train_label, k = k, ... )
        #knn_k = class::knn( train = train, test = test, cl = train_label, k = k )
        
        base_list[ k ] = liver::accuracy( knn_k, test_label )
    }
    
    if( ( base == "accuracy" ) | ( base == "Accuracy" ) )
    {
        title = "Optimal value of k based on accuracy"
        y_lab = "Accuracy"
    }
    
    if( ( base == "MSE" ) | ( base == "mse" ) )
    {
        base_list = 1 - base_list
        
        title = "Optimal value of k based on MSE"
        y_lab = "Mean Square Error (MSE)"
    }
    
    if( ( base == "Error" ) | ( base == "error" ) )
    {
        base_list = 1 - base_list
        
        title = "Optimal value of k based on Error Rate"
        y_lab = "Error Rate"
    }
    
    df = data.frame( k_list = as.factor( k_list ), base_list = base_list, stringsAsFactors = TRUE)
    
    df_gg = data.frame( k_list = k_list, base_list = base_list )
    
    ggplot2::ggplot( df_gg, ggplot2::aes( x = k_list, y = base_list ) ) +
        ggplot2::geom_line( color = "#a0a0a0" ) + 
        ggplot2::geom_point( shape = 21, color = "#ff5085", fill = "#ff83a8", size = 2 ) + 
        ggplot2::theme_minimal() + 
        ggplot2::scale_x_continuous( breaks = k_list ) +
        ggplot2::ggtitle( title ) +
        ggplot2::labs( x = "Value of k", y = y_lab ) +
        ggplot2::theme( axis.line = ggplot2::element_line( size = 0.4, colour = "black" ),
                        panel.grid.major = ggplot2::element_line( colour = "#f2f2f2" ), panel.grid.minor = ggplot2::element_blank(),
                        panel.border = ggplot2::element_blank(), panel.background = ggplot2::element_blank() )
}
   
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |








