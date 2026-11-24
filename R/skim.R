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
# Summarize a data frame or matrix. Assign the result to access
# the full-precision statistics, or print it to display one table per type.
# In the package NAMESPACE, register S3method(print,liver_skim).
skim = function(data, hist = TRUE, ...)
{
    if (!is.data.frame(data) && !is.matrix(data))
        stop("'data' must be a data frame or a matrix.", call. = FALSE)
    if (!is.logical(hist) || length(hist) != 1L || is.na(hist))
        stop("'hist' must be TRUE or FALSE.", call. = FALSE)

    groups = if (inherits(data, "grouped_df")) attr(data, "groups")
    group_names = setdiff(names(groups), ".rows")
    data_name = deparse(substitute(data), nlines = 1L)
    if (is.call(substitute(data)))
        data_name = paste0(deparse(substitute(data)[[1L]])[1L], "(...)")
    data = as.data.frame(data, stringsAsFactors = FALSE)
    columns = seq_along(data)
    labels = names(data)
    selection = substitute(c(...))
    if (length(selection) > 1L) {
        if (requireNamespace("tidyselect", quietly = TRUE)) {
            columns = tidyselect::eval_select(selection, data, env = parent.frame())
            labels = names(columns)
        } else {
            columns = skim_base_columns(selection, labels, parent.frame())
            labels = labels[columns]
        }
    }

    keep = !names(data)[columns] %in% group_names
    columns = columns[keep]
    labels = labels[keep]
    types = vapply(columns, function(j) skim_column_type(data[[j]]), character(1L))
    order_by_type = order(types, seq_along(types))
    ngroups = if (is.null(groups)) 1L else nrow(groups)
    group_id = rep(seq_len(ngroups), times = length(columns))
    index = rep(order_by_type, each = ngroups)
    columns = unname(columns[index])
    types = unname(types[index])
    result = data.frame(skim_type = types, skim_variable = as.character(labels[index]),
                        stringsAsFactors = FALSE)
    for (key in group_names) result[[key]] = groups[[key]][group_id]
    result$n_missing = integer(length(types))
    result$complete_rate = double(length(types))

    for (type in unique(types)) {
        rows = which(types == type)
        summaries = lapply(rows, function(i) {
            x = data[[columns[i]]]
            if (!is.null(groups)) {
                selected = groups$.rows[[group_id[i]]]
                x = if (is.null(dim(x))) x[selected] else x[selected, , drop = FALSE]
            }
            skim_column(x, type, hist)
        })
        block = do.call(rbind, summaries)
        result$n_missing[rows] = block$n_missing
        result$complete_rate[rows] = block$complete_rate
        for (field in setdiff(names(block), c("n_missing", "complete_rate"))) {
            # NA indexing creates a full-length column of the correct class.
            target = block[[field]][rep(NA_integer_, length(types))]
            target[rows] = block[[field]]
            result[[paste(type, field, sep = ".")]] = target
        }
    }
    attr(result, "data_name") = data_name
    attr(result, "data_dim") = dim(data)
    attr(result, "group_names") = group_names
    class(result) = c("liver_skim", "data.frame")
    result
}

skim_column_type = function(x)
{
    if (!is.null(dim(x))) return("matrix")
    if (is.factor(x)) return("factor")
    if (inherits(x, "POSIXt")) return("POSIXct")
    if (inherits(x, c("Date", "difftime"))) return(class(x)[1L])
    if (is.numeric(x) && !is.complex(x)) return("numeric")
    typeof(x)
}

skim_column = function(x, type, hist)
{
    missing = is.na(x)
    if (!is.null(dim(missing))) missing = rowSums(missing) > 0L
    result = data.frame(n_missing = sum(missing), complete_rate = mean(!missing))
    if (type == "matrix") return(result)
    observed = x[!missing]
    unique_count = function() length(unique(observed))

    if (type == "numeric") {
        result$mean = mean(observed)
        result$sd = stats::sd(observed)
        result[c("p0", "p25", "p50", "p75", "p100")] =
            as.list(stats::quantile(observed, names = FALSE))
        if (hist) result$hist = skim_histogram(observed)
    } else if (type == "factor") {
        result$ordered = is.ordered(x)
        result$n_unique = unique_count()
        result$top_counts = skim_top_counts(x)
    } else if (type == "logical") {
        result$mean = mean(observed)
        result$count = skim_top_counts(x)
    } else if (type == "character") {
        lengths = nchar(observed, allowNA = TRUE)
        result$min = if (length(lengths)) min(lengths) else NA_integer_
        result$max = if (length(lengths)) max(lengths) else NA_integer_
        result$empty = sum(observed == "")
        result$n_unique = unique_count()
        result$whitespace = sum(grepl("^[[:space:]]+$", observed))
    } else if (type %in% c("Date", "POSIXct", "difftime")) {
        if (type == "POSIXct") observed = as.POSIXct(observed)
        result$min = if (length(observed)) min(observed) else observed[NA_integer_]
        result$max = if (length(observed)) max(observed) else observed[NA_integer_]
        result$median = stats::median(observed)
        result$n_unique = unique_count()
    } else if (type == "complex") {
        result$mean = mean(observed)
    } else if (type == "list") {
        sizes = lengths(x)
        result$n_unique = unique_count()
        result$min_length = if (length(sizes)) min(sizes) else NA_integer_
        result$max_length = if (length(sizes)) max(sizes) else NA_integer_
    }
    result
}

skim_top_counts = function(x)
{
    counts = table(x, useNA = "no")
    if (!length(counts)) return("")
    top = utils::head(order(as.integer(counts), decreasing = TRUE, method = "radix"), 4L)
    labels = names(counts)[top]
    labels[labels == ""] = "empty"
    paste0(substr(labels, 1L, 3L), ": ", as.integer(counts[top]), collapse = ", ")
}

skim_histogram = function(values)
{
    values = values[is.finite(values)]
    if (!length(values)) return(" ")
    limits = range(values)
    if (limits[1L] == limits[2L]) {
        counts = c(0, 0, length(values), 0, 0)
    } else {
        # Use R's right-closed bins. Rescale only at floating-point limits.
        span = diff(limits)
        scale = max(abs(limits))
        if (!is.finite(span) || span < 10 * .Machine$double.eps * scale ||
            !is.finite(scale + span / 1000) || span < .Machine$double.xmin) {
            if (!is.finite(span)) {
                values = values / 2
                limits = limits / 2
            }
            values = (values - limits[1L]) / diff(limits)
        }
        counts = tabulate(cut(values, 5L, labels = FALSE), nbins = 5L)
    }
    # The six glyphs have reliably distinguishable heights in console fonts.
    glyphs = c(0x2581L, 0x2582L, 0x2583L, 0x2585L, 0x2586L, 0x2587L)
    heights = cut(counts / max(counts), seq(0, 1, length.out = 7L),
                  include.lowest = TRUE, labels = FALSE)
    intToUtf8(glyphs[heights])
}

# Base R supports names, positions, ranges and negative positions.
# Installing the optional tidyselect package enables its full selection syntax.
skim_base_columns = function(selection, labels, caller)
{
    indices = seq_along(labels)
    mask = as.list(stats::setNames(indices, labels))
    expressions = as.list(selection)[-1L]
    if (any(nzchar(names(expressions))))
        stop("Install 'tidyselect' to rename columns in '...'.", call. = FALSE)

    as_positions = function(expr) {
        value = eval(expr, mask, caller)
        if (is.character(value)) {
            value = match(value, labels)
        }
        if (is.null(value))
            return(integer(0))
        if (!is.numeric(value) || any(!is.finite(value)) ||
            any(value != trunc(value)) || any(abs(value) > length(labels)))
            stop("Select existing columns by name or integer position.", call. = FALSE)
        as.integer(value)
    }

    selected = integer(0)
    for (i in seq_along(expressions)) {
        requested = as_positions(expressions[[i]])
        requested = requested[requested != 0L]
        if (any(requested < 0L)) {
            if (any(requested > 0L))
                stop("Supply inclusions and exclusions as separate arguments.",
                     call. = FALSE)
            if (i == 1L)
                selected = indices
            selected = setdiff(selected, -requested)
        } else {
            selected = union(selected, requested)
        }
    }
    selected
}

# Format decimal places for display only, keeping decimal points aligned.
skim_format_numbers = function(x, digits)
{
    text = vapply(x, function(value) {
        if (!is.finite(value)) return(as.character(value))
        if (value == 0) return("0")
        power = floor(log10(abs(value)))
        if (power < -5L || power > 12L)
            return(format(value, digits = digits, scientific = TRUE, trim = TRUE))
        decimals = max(0L, digits - 1L - power)
        fixed = formatC(value, format = "f", digits = decimals)
        if (value == round(value, decimals)) {
            if (decimals) fixed = sub("\\.?0+$", "", fixed)
        } else if (!decimals) {
            fixed = paste0(fixed, ".")
        }
        fixed
    }, character(1L), USE.NAMES = FALSE)
    text[is.na(text)] = "NA"
    before = nchar(sub("\\..*$", "", text))
    after = nchar(text) - before
    paste0(strrep(" ", max(before) - before), text, strrep(" ", max(after) - after))
}

print.liver_skim = function(x, ..., digits = 3L)
{
    if (!is.numeric(digits) || length(digits) != 1L || !is.finite(digits) ||
        digits < 1L || digits > 15L || digits != as.integer(digits))
        stop("'digits' must be an integer from 1 to 15.", call. = FALSE)
    dash = intToUtf8(0x2500L)
    heading = function(title, width) {
        prefix = paste0(dash, dash, " ", title, " ")
        cat(prefix, strrep(dash, max(0L, width - nchar(prefix))), "\n", sep = "")
    }

    heading("Data Summary", 40L)
    dims = attr(x, "data_dim")
    group_names = attr(x, "group_names")
    counts = table(x$skim_type[!duplicated(x[c("skim_type", "skim_variable")])])
    type_labels = if (length(counts)) paste0("  ", names(counts))
    labels = c("", "Name", "Number of rows", "Number of columns", strrep("_", 23L),
               "Column type frequency:", type_labels,
               strrep("_", 24L), "Group variables")
    values = c("Values", attr(x, "data_name"), dims, "", "", as.integer(counts), "",
               if (length(group_names)) paste(group_names, collapse = ", ") else "None")
    cat(paste(sprintf("%-26s", labels), format(values, justify = "left")), sep = "\n")

    for (type in names(counts)) {
        prefix = paste0(type, ".")
        fields = names(x)[startsWith(names(x), prefix)]
        fields = setdiff(fields, group_names)
        cols = c("skim_variable", group_names, "n_missing", "complete_rate", fields)
        section = as.data.frame(x)[x$skim_type == type, cols, drop = FALSE]
        names(section) = c("skim_variable", group_names, "n_missing", "complete_rate",
                           substring(fields, nchar(prefix) + 1L))
        cat("\n")
        heading(paste("Variable type:", type), getOption("width", 80L))
        # Include row numbers, and right-align numeric fields like a tibble.
        display = c(list(as.character(seq_len(nrow(section)))), as.list(section))
        titles = c("", names(section))
        formatted = lapply(seq_along(display), function(j) {
            column = display[[j]]
            numeric_column = is.numeric(column) && !is.object(column)
            body = if (numeric_column) skim_format_numbers(column, digits) else format(column, trim = TRUE)
            cells = c(titles[j], body)
            format(cells, justify = if (numeric_column || j == 1L) "right" else "left")
        })
        cat(do.call(paste, formatted), sep = "\n")
    }
    invisible(x)
}
