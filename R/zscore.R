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
#     Z-score normalization
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

zscore = function(x, col = "auto", mean = NULL, sd = NULL, 
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
    mean_val <- if (is.null(mean)) mean(x, na.rm = na_option) else mean
    sd_val   <- if (is.null(sd  )) sd(x,   na.rm = na_option) else sd
    
    if(sd_val == 0) 
      stop("Standard deviation is zero, cannot perform z-score normalization")
    
    z = (x - mean_val) / sd_val
    
  }else{
    
    is_data_frame = is.data.frame(x)
    if(is_data_frame)
      x = data.matrix(x)
    
    # Define mean and sd as vectors for each column
    mean_val <- if (is.null(mean)) apply(x, 2, mean, na.rm = na_option) else mean
    sd_val   <- if (is.null(sd  )) apply(x, 2, sd, na.rm = na_option  ) else sd
    
    if(length(mean_val) != length(sd_val)) 
      stop("'mean' and 'sd' must have the same length")
    
    if(length(mean_val) == 1)
      mean_val = rep(mean_val, ncol(x))
    
    if(length(sd_val) == 1)
      sd_val = rep(sd_val, ncol(x))
    
    if(length(mean_val) != ncol(x) | length(sd_val) != ncol(x))
      stop("'mean', 'sd', and 'col' must have the same length")
    
    z = t((t(x) - mean_val) / sd_val)
    
    z[is.na(z)] = 0
    
    if(is_data_frame)
    {
      z = as.data.frame(z)
      colnames(z) = colnames(x)
    }
  }
  
  if(!is.vector(x_orig))
  {
    x_orig[, col] = z
    z = x_orig
  }
  
  return(z)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |








