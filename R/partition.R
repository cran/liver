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
   
partition = function(data, prob = c(0.7, 0.3), set.seed = NULL)
{
    if(!is.matrix(data) & !is.data.frame(data)) 
        stop(" data must be a matrix, or dataframe")
    
    if(sum(prob) > 1) 
        stop(" Sum of the vector 'prob' must be smaller or equal to 1")
    else if(sum(prob) < 1)
        prob = c(prob, 1 - sum(prob))
    
    length_prob = length(prob)
    length_data = nrow(data)
    
    if(length_prob > nrow(data)) 
        stop(" length of prob must be smaller or equal to number of observations.")
    
    if(!is.null(set.seed)) 
        set.seed(set.seed)
    
    name_list = vector(length = length_prob)
    partitions = list()
    
    for(i in 1:(length_prob - 1))
    {
        ind_i = sample(1:length_data, size = round(prob[i] * length_data), replace = F)
        
        partitions[[i]] = data[ind_i,]
        
        data        = data[-ind_i,]
        length_data = nrow(data)
        name_list[i] = paste(c("part", i), collapse = "")
    }
    
    partitions[[length_prob]] = data
    name_list[length_prob] = paste(c("part", length_prob), collapse = "")
    
    names(partitions) = name_list
    
    return(partitions)    
}
   
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |








