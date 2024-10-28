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

conf.mat = function(pred, actual, cutoff = NULL, reference = NULL, 
                    proportion = FALSE, dnn = c("Predict", "Actual"), ...)
{
    if(length(pred) != length(actual))
        stop("prod & actual must have the same length")
    
    if(!is.null(cutoff))
    {
        if((cutoff < 0) || (cutoff > 1)) 
			stop(" The value of 'cutoff' must be between 0 and 1.")
        
        levels = base::levels(as.factor(actual))
        
        if(length(levels)  < 2) 
			stop(" 'actual' must have more than two levels.")
			
        if(length(levels) != 2) 
			stop(" For the case 'cutoff != NULL', 'actual' must have two levels.")
        
        if(levels[1] == 0) 
			levels = c(levels[2], levels[1]) 
        
        if(!is.null(reference))
            if(which(levels == reference) == 2) 
				levels = c(levels[2], levels[1]) 

        if(is.null(reference))
            cat(paste(c("Setting levels: reference = \"", levels[1], "\", case = \"", levels[2],"\"  \n"), collapse = "")) 
        
        pred = ifelse(pred >= cutoff, levels[1], levels[2])
        
        pred   = factor(pred  , levels = levels)
        actual = factor(actual, levels = levels)
    }
    
    conf_mat = table(pred, actual, dnn = dnn, ...)
    
    if(proportion == TRUE)
        conf_mat = round(conf_mat / sum(conf_mat), 3)
    
    return(conf_mat)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |




