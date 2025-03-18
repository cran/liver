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
#     one hot coding
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

one.hot = function(data, cols = "auto", sparsifyNAs = FALSE, naCols = FALSE, 
                   dropCols = TRUE, dropUnusedLevels = FALSE)
{ 
  if(!is.vector(data) & !is.data.frame(data) & !data.table::is.data.table(data))
    stop(" data must be a vector, data.frame, or data.table")
  
  class_data = class(data)
  
  if(is.vector(data))
    data = data.frame(data)
  
  if(is.data.frame(data))
    data = data.table::as.data.table(data)
  
  ind_chr = which(sapply(data, function(x) is.character(x)))
  for(i in ind_chr)
    data[[i]] = factor(data[[i]], levels = unique(data[[i]]))
  
  if(cols[1] == "auto") 
    cols <- colnames(data)[which(sapply(data, function(x) is.factor(x) & !is.ordered(x)))]
  
  output = mltools::one_hot(dt = data, cols = cols, sparsifyNAs = sparsifyNAs, naCols = naCols, 
                            dropCols = dropCols, dropUnusedLevels = dropUnusedLevels)
  
  if(class_data == "data.frame")
    output = as.data.frame(output)
  
  return(output)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |








