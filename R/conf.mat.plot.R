## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
#     Copyright (C) 2020 -   Reza Mohammadi & Kevin Burke                      |
#                                                                              |
#     This file is part of 'liver' package.                                    |
#                                                                              |
#     liver is free software: you can redistribute it and/or modify it under   |
#     the terms of the GNU General Public License as published by the Free     |
#     Software Foundation; see <https://cran.r-project.org/web/licenses/GPL-3>.|
#                                                                              |
#     Maintainer: Reza Mohammadi <a.mohammadi@uva.nl>                          |
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
#     Plot a Confusion Matrix
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

conf.mat.plot = function(pred, actual, cutoff = 0.5, reference = NULL, 
                         conf.level = 0, margin = c(1, 2), 
                         color = c("#F4A582", "#A6D854"), ...)
{
  conf_mat = liver::conf.mat(pred = pred, actual = actual, cutoff = cutoff, reference = reference)
  
  graphics::fourfoldplot(conf_mat, conf.level = conf.level, 
                         margin = margin, color = color, ...)
}   

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |




