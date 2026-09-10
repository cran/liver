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
#     Partition a dataset
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |

partition = function(data, ratio = c(0.7, 0.3), strata = NULL, set.seed = NULL)
{
    if(!is.matrix(data) & !is.data.frame(data))
        stop(" data must be a matrix, or dataframe")

    if(nrow(data) == 0)
        stop(" data must contain at least one observation")

    if(!is.numeric(ratio) | any(!is.finite(ratio)) | any(ratio <= 0))
        stop(" 'ratio' must contain positive numeric values")

    if(sum(ratio) > 1)
        stop(" Sum of the vector 'ratio' must be smaller or equal to 1")

    if(sum(ratio) < 1)
        ratio = c(ratio, 1 - sum(ratio))

    length_ratio = length(ratio)
    length_data  = nrow(data)

    if(length_ratio > length_data)
        stop(" length of 'ratio' must be smaller or equal to number of observations.")

    if(!is.null(set.seed))
        base::set.seed(set.seed)


    # ------------------------------------------------------------
    # Determine the exact number of observations in each partition
    # ------------------------------------------------------------

    allocate_counts = function(n, ratio)
    {
        expected = n * ratio
        counts   = floor(expected)

        remainder = n - sum(counts)

        if(remainder > 0)
        {
            ind = order(expected - counts, decreasing = TRUE)
            counts[ind[seq_len(remainder)]] =
                counts[ind[seq_len(remainder)]] + 1
        }

        counts
    }

    partition_size = allocate_counts(length_data, ratio)


    # ------------------------------------------------------------
    # No stratification
    # ------------------------------------------------------------

    strata_expr = substitute(strata)

    if(missing(strata) || identical(strata_expr, quote(NULL)))
    {
        ind = sample.int(length_data)

        partition_id = rep(
            seq_len(length_ratio),
            times = partition_size
        )

        row_partition = integer(length_data)
        row_partition[ind] = partition_id
    }


    # ------------------------------------------------------------
    # Stratification
    # ------------------------------------------------------------

    else
    {
        # Allow both:
        # strata = income
        # strata = "income"

        if(is.character(strata_expr) && length(strata_expr) == 1)
            strata_name = strata_expr
        else if(is.symbol(strata_expr))
            strata_name = as.character(strata_expr)
        else
            stop(" 'strata' must be a column name")

        if(is.null(colnames(data)) || !strata_name %in% colnames(data))
            stop(" 'strata' must be a column in 'data'")

        strata_value = data[, strata_name]

        if(any(is.na(strata_value)))
            stop(" 'strata' must not contain missing values")


        # --------------------------------------------------------
        # Numerical strata
        # --------------------------------------------------------

        if(is.numeric(strata_value))
        {
            # Random values are used to randomize observations with ties
            ind = order(strata_value, stats::runif(length_data))

            # Construct an evenly distributed sequence of partition labels
            position = numeric(0)
            label    = integer(0)

            for(i in seq_len(length_ratio))
            {
                n_i = partition_size[i]

                if(n_i > 0)
                {
                    random_start = stats::runif(1)

                    position_i =
                        (seq_len(n_i) - random_start) / n_i

                    position = c(position, position_i)
                    label    = c(label, rep(i, n_i))
                }
            }

            label = label[order(position)]

            row_partition = integer(length_data)
            row_partition[ind] = label
        }


        # --------------------------------------------------------
        # Categorical strata
        # --------------------------------------------------------

        else if(is.factor(strata_value) ||
                is.character(strata_value) ||
                is.logical(strata_value))
        {
            strata_value = as.factor(strata_value)

            strata_ind  = split(seq_len(length_data), strata_value)
            strata_size = lengths(strata_ind)

            # Ideal number of observations from each stratum
            # in each partition
            expected = outer(strata_size, ratio)

            allocation = floor(expected)

            row_remaining =
                strata_size - rowSums(allocation)

            col_remaining =
                partition_size - colSums(allocation)

            fractional = expected - allocation

            # Allocate remaining observations while preserving both:
            # 1. stratum proportions
            # 2. total partition sizes
            while(sum(row_remaining) > 0)
            {
                possible =
                    outer(row_remaining > 0,
                          col_remaining > 0,
                          "&")

                score = fractional
                score[!possible] = -Inf

                pos = which(
                    score == max(score),
                    arr.ind = TRUE
                )[1, ]

                allocation[pos[1], pos[2]] =
                    allocation[pos[1], pos[2]] + 1

                row_remaining[pos[1]] =
                    row_remaining[pos[1]] - 1

                col_remaining[pos[2]] =
                    col_remaining[pos[2]] - 1

                fractional[pos[1], pos[2]] = -Inf
            }


            # Randomly assign observations within each stratum
            row_partition = integer(length_data)

            for(i in seq_along(strata_ind))
            {
                ind = sample(strata_ind[[i]])

                start = 1

                for(j in seq_len(length_ratio))
                {
                    n_j = allocation[i, j]

                    if(n_j > 0)
                    {
                        end = start + n_j - 1

                        row_partition[ind[start:end]] = j

                        start = end + 1
                    }
                }
            }
        }

        else
        {
            stop(" 'strata' must be a numeric or categorical variable")
        }
    }


    # ------------------------------------------------------------
    # Create partitions
    # ------------------------------------------------------------

    partitions = lapply(
        seq_len(length_ratio),
        function(i)
            data[row_partition == i, , drop = FALSE]
    )

    names(partitions) = paste0("part", seq_len(length_ratio))

    return(partitions)
}
   
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - |








