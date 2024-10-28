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
#     Compute average classification accuracy
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

accuracy = function(pred, actual, cutoff = NULL, reference = NULL)
{
    conf_mat = liver::conf.mat(pred = pred, actual = actual, cutoff = cutoff, reference = reference)
    
    accuracy_value = sum(diag(conf_mat)) / sum(conf_mat)
    
}
   
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |


