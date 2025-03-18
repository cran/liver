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
#     Create a Confusion Matrix
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

conf.mat = function(pred, actual, cutoff = 0.5, reference = NULL, 
                    proportion = FALSE, dnn = c("Predict", "Actual"), ...)
{
    if(length(pred) != length(actual))
        stop("'pred' and 'actual' must have the same length.")
    
    if(!is.factor(actual)) 
        actual = factor(actual)
    
    levels = base::levels(actual)
    
    if(length(levels) < 2) 
        stop("'actual' must have at least two levels.")
    
    if(is.numeric(levels) && levels[1] != (length(levels) - 1)) 
        levels = c(max(levels), levels[-max(levels)])
    
    if(!is.null(reference))
    {
        if(length(reference) != 1) 
            stop(" 'reference' must have only one level.")
        
        if(!is.character(reference))
            reference = as.character(reference)
        
        if(!reference %in% levels)
            stop(" 'reference' must be one of the levels of 'actual'.")
        
        if(which(levels == reference) != 1) 
            levels = c(levels[which(levels == reference)], levels[-which(levels == reference)]) 
    }else{
        if(length(levels) == 2)
            cat(paste(c("Setting levels: reference = \"", levels[1], "\", case = \"", levels[2],"\"  \n"), collapse = ""))
        else
            cat(paste(c("Setting levels: reference = \"", levels[1],"\"  \n"), collapse = ""))
    }
    
    if(is.numeric(pred))
    {
        if(length(unique(pred)) != length(levels))
        {
            if((cutoff < 0) || (cutoff > 1)) 
                stop(" The value of 'cutoff' must be between 0 and 1.")
            
            if(length(levels) > 2)
            {
                Others = levels[levels != levels[1]]
                actual = forcats::fct_collapse(actual, "Others" = Others)
                levels = c(levels[1], "Others")
            }
            
            pred = ifelse(pred >= cutoff, levels[1], levels[2])
        }else{
            levels_pred = sort(unique(pred), decreasing = TRUE)
            for(i in 1:length(levels_pred))
                pred[pred == levels_pred[i]] = levels[i]
        }
    }
    
    pred   = factor(pred  , levels = levels)
    actual = factor(actual, levels = levels)
    
    conf_mat = table(pred, actual, dnn = dnn, ...)
    
    if(proportion == TRUE)
        conf_mat = round(conf_mat / sum(conf_mat), 3)
    
    return(conf_mat)
}
  
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |




