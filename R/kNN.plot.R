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

kNN.plot = function(formula, train, test, k.max = 10, scaler = FALSE, 
                    base = "accuracy", reference = NULL, cutoff = NULL, 
                    type = "class", report = FALSE, set.seed = NULL, ...) 
{
  if(any(is.na(train))) 
    stop("train dataset has NA")
  
  if(any(is.na(test)))  
    stop("test dataset has NA")
  
  if(length(k.max) == 1)
    if(k.max < 2)
      stop("  k.max must be > 1")
  
  if(max(k.max) > nrow(train))
    stop("  k.max must be less or equal to the number of observations in the training set")
  
  if(!is.null(set.seed)) 
    set.seed(set.seed)
  
  formula = stats::as.formula(formula)
  
  model_frame_train = stats::model.frame(formula, data = train)
  model_train       = attr(model_frame_train, "terms")
  
  train_label = stats::model.response(model_frame_train)
  train       = stats::model.matrix(model_train, model_frame_train)[, -1]
  
  model_frame_test = stats::model.frame(formula, data = test)
  model_test       = attr(model_frame_test, "terms")
  
  test_label = stats::model.response(model_frame_test)
  test       = stats::model.matrix(model_test, model_frame_test)[, -1]
  
  if(scaler == TRUE) 
    scaler = "minmax"
  
  if(scaler != FALSE)
  {
    na.rm = FALSE
    if(scaler == "minmax")
    {
      if(is.vector(train))
      {
        par1_train = min(train, na.rm = na.rm)
        par2_train = max(train, na.rm = na.rm)
      }else{
        par1_train = apply(train, 2, min, na.rm = na.rm)
        par2_train = apply(train, 2, max, na.rm = na.rm)
      }
    }
    
    if(scaler == "zscore")
    {
      if(is.vector(train))
      {
        par1_train = mean(train, na.rm = na.rm)
        par2_train = stats::sd(train, na.rm = na.rm)
      }else{
        par1_train = apply(train, 2, mean, na.rm = na.rm)
        par2_train = apply(train, 2, stats::sd, na.rm = na.rm)
      }
    }
    
    col = "all"
    
    train = liver::scaler(train, scale = scaler, col = col, par1 = par1_train, par2 = par2_train, na.rm = na.rm)
    test  = liver::scaler(test, scale = scaler, col = col, par1 = par1_train, par2 = par2_train, na.rm = na.rm)
  }
  
  if(is.null(dim(train))) 
    train = as.matrix(train)
  
  if(is.null(dim(test)))  
    test = as.matrix(test)
  
  if(length(k.max) == 1)
    k_list   = 1:k.max
  else 
    k_list = k.max
  
  base_list = vector(length = length(k_list))
  
  # - - - -
  if(is.factor(train_label))
    levels = base::levels(train_label)
  else
    levels = base::levels(factor(train_label))
  
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
  
  reference = levels[1]
  
  prob = ifelse(type == "prob", TRUE, FALSE)
  # - - - 
  
  for(k in 1:length(k_list))
  {
    knn_k = class::knn(train = train, test = test, cl = train_label, k = k_list[k], prob = prob, ...)
    
    base_list[k] = liver::accuracy(knn_k, test_label, reference = reference, cutoff = cutoff)
  }
  
  if((base == "accuracy") | (base == "Accuracy"))
  {
    title = "Accuracy for Different k Values"
    y_lab = "Accuracy"
  }
  
  if((base == "MSE") | (base == "mse"))
  {
    base_list = 1 - base_list
    
    title = "MSE for Different k Values"
    y_lab = "Mean Square Error (MSE)"
  }
  
  if((base == "Error") | (base == "error"))
  {
    base_list = 1 - base_list
    
    title = "Error Rate for Different k Values"
    y_lab = "Error Rate"
  }
  
  df = data.frame(k_list = as.factor(k_list), base_list = base_list, stringsAsFactors = TRUE)
  
  df_gg = data.frame(k_list = k_list, base_list = base_list)
  
  plot_knn = ggplot2::ggplot(df_gg, ggplot2::aes(x = k_list, y = base_list)) +
    ggplot2::geom_line(color = "#ffd1de") + 
    ggplot2::geom_point(shape = 21, color = "#b60009", fill = "#b60009", size = 2) + 
    ggplot2::theme_minimal() + 
    ggplot2::scale_x_continuous(breaks = k_list) +
    ggplot2::ggtitle(title) +
    ggplot2::labs(x = "Value of k", y = y_lab) +
    ggplot2::theme(axis.line = ggplot2::element_line(linewidth = 0.4, colour = "black"),
                   panel.grid.major = ggplot2::element_line(colour = "#f2f2f2"), 
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(), 
                   panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5)) 
  
  if(report == FALSE) 
    return(plot_knn)
  else
    return(list(base_list, plot_knn))
}
   
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
   