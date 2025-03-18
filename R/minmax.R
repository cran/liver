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
#     Min-Max normalization
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

minmax = function(x, col = "auto", min = NULL, max = NULL, 
                  na.rm = FALSE) 
{
  #    if(!methods::is(x)[1] %in% c("integer", "numeric")) stop("Categorical variable not support")
  
  if(!is.vector(x) & !is.matrix(x) & !is.data.frame(x)) 
    stop(" x must be a vector, matrix, or dataframe")
  
  # Handle NA removal flag internally
  na_option <- na.rm
  if(any(is.na(x)) & (na_option == FALSE)) 
    na_option = TRUE
  
  x_orig = x
  if(!is.vector(x))
  {
    if(length(col) == 1 && col == "auto")
      col <- colnames(x)[which(sapply(x, function(x) is.numeric(x)))]
    
    if(length(col) == 1 && col == "all") 
      col <- colnames(x)
    
    x = x[, col]
  }
  
  if(is.vector(x))
  {
    min_val <- if (is.null(min)) min(x, na.rm = na_option) else min
    max_val <- if (is.null(max)) max(x, na.rm = na_option) else max
    
    if(min_val >= max_val)
      stop("The 'min' value must be less than the 'max' value")
    
    x_mm = (x - min_val) / (max_val - min_val)
    
  }else{
    
    is_data_frame = is.data.frame(x)
    if(is_data_frame)
      x = data.matrix(x)
    
    # Define min and max as vectors for each column
    min_val <- if (is.null(min)) apply(x, 2, min, na.rm = na_option) else min
    max_val <- if (is.null(max)) apply(x, 2, max, na.rm = na_option) else max
    
    if(length(min_val) != length(max_val)) 
      stop("'min' and 'max' must have the same length")
    
    if(length(min_val) == 1)
      min_val = rep(min_val, ncol(x))
    
    if(length(max_val) == 1)
      max_val = rep(max_val, ncol(x))
    
    if(length(min_val) != ncol(x) | length(max_val) != ncol(x))
      stop("'min', 'max', and 'col' must have the same length")
    
    x_mm = t((t(x) - min_val) / (max_val - min_val))
    
    x_mm[is.na(x_mm)] = x[is.na(x_mm)]
    
    if(is_data_frame)
    {
      x_mm = as.data.frame(x_mm)
      colnames(x_mm) = colnames(x)
    }
  }
  
  if(!is.vector(x_orig))
  {
    x_orig[, col] = x_mm
    x_mm = x_orig
  }
  
  return(x_mm)
}
   
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |







