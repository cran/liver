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
#     Confidence interval for mean using t-distribution.              |
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

t_conf = function(x, conf = 0.95, ...)
{
  conf = stats::t.test(x = x, 
           y = NULL, 
           alternative = "two.sided",
           mu = 0, 
           paired = FALSE, 
           var.equal = FALSE,
           conf.level = conf, ...)$"conf.int"
  
  return(as.vector(conf))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |








