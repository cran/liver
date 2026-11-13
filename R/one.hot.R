## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |
#     Copyright (C) 2020  Reza Mohammadi                                       |
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
                   dropCols = FALSE, dropUnusedLevels = FALSE)
{
  flags = list(sparsifyNAs = sparsifyNAs, naCols = naCols,
               dropCols = dropCols, dropUnusedLevels = dropUnusedLevels)
  for (flag in names(flags)) {
    value = flags[[flag]]
    if (!is.logical(value) || length(value) != 1L || is.na(value))
      stop(flag, " must be TRUE or FALSE", call. = FALSE)
  }

  return_data_table = inherits(data, "data.table")
  if (return_data_table && !requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required for data.table input", call. = FALSE)

  if (is.data.frame(data) || is.matrix(data)) {
    data = as.data.frame(data, stringsAsFactors = FALSE)
  } else if (is.factor(data) || (is.atomic(data) && is.null(dim(data)))) {
    data = data.frame(data, stringsAsFactors = FALSE)
  } else {
    stop("data must be a vector, matrix, data.frame, or data.table", call. = FALSE)
  }

  column_names = names(data)
  if (anyNA(column_names) || any(!nzchar(column_names)) ||
      anyDuplicated(column_names))
    stop("data must have unique, non-missing, non-empty column names", call. = FALSE)

  # Preserve the existing conversion of character columns to factors.
  for (i in seq_along(data)) {
    if (is.character(data[[i]]))
      data[[i]] = factor(data[[i]], levels = unique(data[[i]]))
  }

  if (identical(cols, "auto")) {
    selected = which(vapply(data, function(x) is.factor(x) && !is.ordered(x),
                            logical(1)))
  } else if (is.null(cols)) {
    selected = integer(0)
  } else if (is.character(cols)) {
    selected = match(cols, column_names)
    if (anyNA(selected))
      stop("cols contains missing or unknown column names", call. = FALSE)
  } else if (is.numeric(cols)) {
    if (anyNA(cols) || any(!is.finite(cols)) || any(cols != floor(cols)) ||
        any(cols < 1L | cols > ncol(data)))
      stop("cols must contain valid positive integer column indices", call. = FALSE)
    selected = as.integer(cols)
  } else {
    stop("cols must be 'auto', column names, or column indices", call. = FALSE)
  }
  selected = unique(selected)
  if (any(!vapply(data[selected], is.factor, logical(1))))
    stop("Selected columns must be factors or character vectors", call. = FALSE)

  # Build each column's indicators in factor-level order, without changing data.
  output = list()
  output_names = character(0)
  for (i in seq_along(data)) {
    column = column_names[i]
    x = data[[i]]
    if (!(i %in% selected) || !dropCols) {
      output[length(output) + 1L] = list(x)
      output_names = c(output_names, column)
    }
    if (!(i %in% selected))
      next

    codes = as.integer(x)
    missing = is.na(codes)
    level_indices = seq_along(levels(x))
    if (dropUnusedLevels)
      level_indices = level_indices[level_indices %in% codes[!missing]]

    if (naCols && any(missing)) {
      output[length(output) + 1L] = list(as.integer(missing))
      output_names = c(output_names, paste0(column, "_NA"))
    }
    for (j in level_indices) {
      indicator = as.integer(codes == j)
      if (sparsifyNAs)
        indicator[missing] = 0L
      output[length(output) + 1L] = list(indicator)
      output_names = c(output_names, paste0(column, "_", levels(x)[j]))
    }
  }

  if (anyDuplicated(output_names))
    stop("Encoding creates duplicate column names; rename columns or factor levels",
         call. = FALSE)
  names(output) = output_names
  output = structure(output, class = "data.frame", row.names = attr(data, "row.names"))
  if (return_data_table)
    output = data.table::as.data.table(output)
  return(output)
}
