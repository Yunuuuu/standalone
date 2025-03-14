#' Create Series object
#'
#' @param value Series value.
#' @param name Series names
#' @param ... Additional attributes to be added.
#' @rdname Series
new_series <- function(value, name = "", ...) {
    structure(value, name = name, ..., class = "Series")
}

#' @include standalone-namespace.R
#' @name Series
Series <- make_methods(
    "Series",
    public = list( # Methods used by user
        #' @description Subset object
        #' @param offset Start 0-based index.
        #' @param length The length of the subset
        slice = function(offset, length = NULL) { # 0-based index
            if (is.null(length)) end <- self$len else end <- offset + length
            .subset(self, offset:end)
        }
    ),
    active = list( # fields (data property value) should be an active binding
        #' @field name Get the names of the Series
        name = function() attr(self, "name"),
        #' @field len Get the length of the Series
        len = function() length(self),
        #' @field dtype Get the data type of the Series
        dtype = function() typeof(self),
        #' @field struct Use struct methods
        struct = function() {
            if (!all_named(self)) {
                stop("data must be a `struct` data") # I suppose `struct` data is just a named series
            }
            class(self) <- "Series_Struct"
            invisible(self)
        }
    )
)

# For a sub-namespace, we just change the underlying class of the data value in
# the parent-namespace active field.
# Then add the methods
Struct <- make_methods(
    "Series_Struct",
    public = list(),
    active = list(fields = function() names(self))
)
