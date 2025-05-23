# ---
# repo: Yunuuuu/standalone
# file: standalone-tibble.R
# last-updated: 2024-11-14
# license: https://unlicense.org
# imports: [vctrs (>= 0.5.0), rlang]
# ---

# when developing R package, instead of depending on `dplyr`, `tidyr`, `tibble`.
# we prefer use the basic function from `vctrs`
#
# Note: these functions won't check arguments
#
# Since `vctrs` also depends on `rlang` and `cli`, it has no harm to use
# function from `rlang` and `cli`.
#
# Please initialize the package docs and import vctrs
# 1. run `usethis::use_package_doc()`
# 2. in package docs, please add #' @import vctrs

# ## Changelog
# 2025-03-12
# - Added `case_when()`
#
# 2024-11-14
# - Added `column_to_rownames()`
#
# 2024-11-13
# - fix wrong results in `coalesce()`: we should assign value in the missing
#   index
#
# 2024-11-12
# - Added `rename`
# - coalesce() now will return value immediately when no missing value exists.
#
# 2024-11-11:
# - Added `inner_join`
# - Added `left_join`
# - Added `right_join`
# - Added `cross_join`
# - Added `replace_na`
# - Added `coalesce`
# - Added `deframe`
# - Added `enframe`
# - Added `remove_rownames`
# - Added `rownames_to_column`
#
# 2024-11-10:
# - Added `full_join`
# - Added `if_else`
#
# nocov start

#' @importFrom rlang set_names
full_join <- function(
    x,
    y,
    by = vec_set_intersect(names(x), names(y)),
    by.x = by,
    by.y = by,
    suffix = c(".x", ".y")
) {
    loc <- vec_locate_matches(x[by.x], set_names(y[by.y], by.x), remaining = NA)
    x_slicer <- .subset2(loc, "needles")
    y_slicer <- .subset2(loc, "haystack")
    ans <- join_bind(
        vec_slice(x, x_slicer),
        # drop duplicated join column
        vec_slice(y[vec_set_difference(names(y), by.y)], y_slicer),
        suffix = suffix
    )
    new_rows <- which(vec_detect_missing(x_slicer)) # should come from `y`
    if (length(new_rows)) {
        ans[new_rows, by.x] <- vec_slice(y[by.y], y_slicer[new_rows])
    }
    ans
}

#' @importFrom rlang set_names
inner_join <- function(
    x,
    y,
    by = vec_set_intersect(names(x), names(y)),
    by.x = by,
    by.y = by,
    suffix = c(".x", ".y")
) {
    loc <- vec_locate_matches(
        x[by.x],
        set_names(y[by.y], by.x),
        no_match = "drop"
    )
    x_slicer <- .subset2(loc, "needles")
    y_slicer <- .subset2(loc, "haystack")
    join_bind(
        vec_slice(x, x_slicer),
        # drop duplicated join column
        vec_slice(y[vec_set_difference(names(y), by.y)], y_slicer),
        suffix = suffix
    )
}

#' @importFrom rlang set_names
left_join <- function(
    x,
    y,
    by = vec_set_intersect(names(x), names(y)),
    by.x = by,
    by.y = by,
    suffix = c(".x", ".y")
) {
    loc <- vec_locate_matches(x[by.x], set_names(y[by.y], by.x))
    x_slicer <- .subset2(loc, "needles")
    y_slicer <- .subset2(loc, "haystack") # can have NA value
    join_bind(
        vec_slice(x, x_slicer),
        # drop duplicated join column
        vec_slice(y[vec_set_difference(names(y), by.y)], y_slicer),
        suffix = suffix
    )
}

#' @importFrom rlang set_names
right_join <- function(
    x,
    y,
    by = vec_set_intersect(names(x), names(y)),
    by.x = by,
    by.y = by,
    suffix = c(".x", ".y")
) {
    loc <- vec_locate_matches(
        x[by.x],
        set_names(y[by.y], by.x),
        no_match = "drop",
        remaining = NA
    )
    x_slicer <- .subset2(loc, "needles") # can have NA value
    y_slicer <- .subset2(loc, "haystack")
    join_bind(
        # drop duplicated join column
        vec_slice(x[vec_set_difference(names(x), by.x)], x_slicer),
        vec_slice(y, y_slicer),
        suffix = suffix
    )
}

cross_join <- function(x, y, suffix = c(".x", ".y")) {
    x_size <- vec_size(x)
    y_size <- vec_size(y)
    x_out <- vec_rep_each(x, times = y_size)
    y_out <- vec_rep(y, times = x_size)
    join_bind(x_out, y_out, suffix)
}

join_bind <- function(x, y, suffix) {
    x_names <- names(x)
    y_names <- names(y)
    common <- vec_set_intersect(x_names, y_names)
    if (length(common)) {
        # add suffix to duplicated names
        index <- vec_match(common, x_names)
        names(x)[index] <- paste0(x_names[index], .subset(suffix, 1L))
        index <- vec_match(common, y_names)
        names(y)[index] <- paste0(y_names[index], .subset(suffix, 2L))
    }
    vec_cbind(x, y, .name_repair = "check_unique")
}

#' Rename elements in a list, data.frame or vector
#'
#' This is akin to `dplyr::rename` and `plyr::rename`. It renames elements given
#' as names in the `replace` vector to the values in the `replace` vector
#' without touching elements not referenced.
#'
#' @param x A data.frame or a named vector or list
#' @param replace A named character vector. The names identifies the elements in
#' `x` that should be renamed and the values gives the new names.
#'
#' @return `x`, with new names according to `replace`
#' @noRd
rename <- function(x, replace) {
    nms <- names(x)
    names(x) <- vec_assign(nms, match(names(replace), nms), replace)
    x
}

if_else <- function(condition, true, false, na = NULL) {
    # output size from `condition`
    size <- vec_size(condition)

    # output type from `true`/`false`/`na`
    ptype <- vec_ptype_common(true = true, false = false, na = na)

    args <- vec_recycle_common(
        true = true,
        false = false,
        na = na,
        .size = size
    )
    args <- vec_cast_common(!!!args, .to = ptype)

    out <- vec_init(ptype, size)

    loc_true <- condition
    loc_false <- !condition

    out <- vec_assign(out, loc_true, vec_slice(args$true, loc_true))
    out <- vec_assign(out, loc_false, vec_slice(args$false, loc_false))

    if (!is.null(na)) {
        loc_na <- vec_detect_missing(condition)
        out <- vec_assign(out, loc_na, vec_slice(args$na, loc_na))
    }

    out
}

case_when <- function(.default, ..., .ptype = NULL) {
    if (is.null(.ptype)) {
        .ptype <- vec_ptype(.default)
    } else {
        .default <- vec_cast(.default, .ptype)
    }
    env <- rlang::caller_env()
    dots <- rlang::list2(...)
    unused <- vec_rep(TRUE, times = vec_size(.default))
    for (i in seq_along(dots)) {
        if (!any(unused)) {
            break
        }
        dot <- .subset2(dots, i)
        loc <- unused & rlang::eval_tidy(rlang::f_lhs(dot), env = env)
        value <- rlang::eval_tidy(rlang::f_rhs(dot), env = env)
        value <- vec_cast(value, .ptype, x_arg = sprintf("`...` (%d)", i))
        if (length(value) > 1L) value <- vec_slice(value, loc)
        .default <- vec_assign(
            .default,
            loc,
            value,
            value_arg = sprintf("`...` (%d)", i)
        )
        unused[loc] <- FALSE
    }
    .default
}


#' Replace NAs with specified values
#' @param value A single value.
#' @noRd
replace_na <- function(x, value) {
    value <- vec_cast(x = value, to = x, x_arg = "value", to_arg = "x")
    vec_assign(x, vec_detect_missing(x), value)
}

#' Find the first non-missing element
#' @param ... A list of atomic vector (You shouldn't input `NULL`).
#' @noRd
coalesce <- function(...) {
    dots <- vec_recycle_common(...)
    out <- .subset2(dots, 1L)
    for (i in 2:length(dots)) {
        if (vec_any_missing(out)) {
            missing <- vec_detect_missing(out)
            out <- vec_assign(
                out,
                missing,
                vec_slice(.subset2(dots, i), missing)
            )
        } else {
            return(out)
        }
    }
    out
}

#' Convert values to `NA`
#' @noRd
na_if <- function(x, y) {
    y <- vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
    y <- vec_recycle(y, size = vec_size(x), x_arg = "y")
    na <- vec_init(x)
    vec_assign(x, vec_equal(x, y, na_equal = TRUE), na)
}

deframe <- function(x) {
    if (ncol(x) == 1L) {
        return(x[[1]])
    }
    vec_set_names(x[[1L]], x[[2]])
}

enframe <- function(x, name = "name", value = "value") {
    if (!is.null(names(x))) {
        data <- list(names(x), unname(x))
    } else {
        data <- list(seq_along(x), unname(x))
    }
    new_data_frame(vec_set_names(data, c(name, value)))
}

remove_rownames <- function(.data) {
    rownames(.data) <- NULL
    .data
}

rownames_to_column <- function(.data, var = "rowname") {
    if (!is.null(var_col <- rownames(.data))) {
        nms <- names(.data)
        .data[[var]] <- var_col
        .data <- .data[c(var, nms)]
    }
    .data
}

column_to_rownames <- function(.data, var = 1L) {
    rownames(.data) <- .subset2(.data, var)
    .data[[var]] <- NULL
    .data
}

# nocov end
