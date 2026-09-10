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
#     Confidence interval for mean using z-distribution.                 
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

z.conf = function(x, sigma = NULL, conf = 0.95)
{
  # Check input validity
  if (!is.numeric(x) || !is.vector(x)) {
    stop("x must be a numeric vector.")
  }
  
  if (length(x) == 0) {
    stop("x is empty. Provide at least one observation.")
  }
  
  if (!is.null(sigma)) {
    if (!is.numeric(sigma) || length(sigma) != 1 || sigma <= 0) {
      stop("sigma must be a single positive number.")
    }
  }
  
  if (!is.numeric(conf) || length(conf) != 1 || conf <= 0 || conf >= 1) {
    stop("conf must be a single number between 0 and 1 (exclusive).")
  }
  
  n = length(x)
  x_bar = mean(x)
  alpha = 1 - conf
  
  if (!is.null(sigma)) {
    # Known population standard deviation to use z-based CI
    z_crit = stats::qnorm(1 - alpha / 2)
    se = sigma / sqrt(n)
    margin = z_crit * se
  } else {
    # Unknown sigma to use sample SD and t-distribution
    if (n < 2) {
      stop("At least two observations are required when sigma is unknown.")
    }
    s = stats::sd(x)
    t_crit = stats::qt(1 - alpha / 2, df = n - 1)
    se = s / sqrt(n)
    margin = t_crit * se
  }
  
  lower = x_bar - margin
  upper = x_bar + margin
  
  return(c(lower, upper))
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |








