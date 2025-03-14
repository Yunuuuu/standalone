# ---
# repo: Yunuuuu/standalone
# file: standalone-namespace.R
# last-updated: 2025-03-14
# license: https://unlicense.org
# imports: [utils]
# ---

# ## Changelog
# 2025-03-14:
# - Add `make_methods()`
#
# nocov start

# Here, we split the R6 object between the object entity and methods or fields
# 1. We can use `self` to refer the input data (other methods or fields can
#    be directly acquired by `self$`)
# 2. We can add private methods and fields
make_methods <- function(Class, public = list(), private = list(),
                         active = list()) {
    methods <- new_namespace(
        Class,
        public = public,
        private = private, active = active
    )
    ns <- getNamespace(utils::packageName())
    subset <- namespace_bind_public(Class, methods)
    registerS3method("$", Class, subset, envir = ns)
    registerS3method("[[", Class, subset, envir = ns)
    registerS3method("[", Class, function(x, i) {
        stop("Please use `$` or `[[` method instead")
    }, envir = ns)
    registerS3method(".DollarNames", Class, function(x, pattern) {
        names <- lapply(
            c("public_fields", "public_methods", "active"),
            function(x) {
                if (!is.null(env <- .subset2(methods, "public_fields"))) {
                    names(env)
                }
            }
        )
        names <- unlist(names, recursive = FALSE, use.names = FALSE)
        names[grepl(pattern, names)]
    }, envir = ns)

    private_subset <- namespace_bind_private(Class, methods)
    private_class <- paste0(Class, "PrivateNamespace")
    registerS3method("$", private_class, private_subset, envir = ns)
    registerS3method("[[", private_class, private_subset, envir = ns)
    registerS3method("[", private_class, function(x, i) {
        stop("Please use `$` or `[[` method instead")
    }, envir = ns)
    registerS3method(".DollarNames", private_class, function(x, pattern) {
        names <- lapply(c("private_fields", "private_methods"), function(x) {
            if (!is.null(env <- .subset2(methods, x))) {
                names(env)
            }
        })
        names <- unlist(names, recursive = FALSE, use.names = FALSE)
        names[grepl(pattern, names)]
    }, envir = ns)
    methods
}

# Check that all elements of a list are named.
# NULL and empty lists return TRUE.
all_named <- function(x) {
    if (length(names(x)) != length(x) || any(names(x) == "")) {
        return(FALSE)
    }
    TRUE
}

# Check that all elements of a list are functions
all_functions <- function(x) {
    all(vapply(x, is.function, logical(1L), USE.NAMES = FALSE))
}

# Return all the functions in a list.
get_functions <- function(x) {
    funcs <- vapply(x, is.function, logical(1), USE.NAMES = FALSE)
    if (all(!funcs)) {
        return(NULL)
    }
    x[funcs]
}

# Return all the non-functions in a list.
get_nonfunctions <- function(x) {
    funcs <- vapply(x, is.function, logical(1), USE.NAMES = FALSE)
    if (all(funcs)) {
        return(NULL)
    }
    x[!funcs]
}

new_namespace <- function(Class, public = list(), private = list(),
                          active = list()) {
    if (!all_named(public) || !all_named(private) || !all_named(active)) {
        stop("All elements of public, private, and active must be named.")
    }
    if (anyDuplicated(c(names(public), names(active)))) {
        stop("All items in public, and active must have unique names.")
    }
    if (anyDuplicated(names(private))) {
        stop("All items in private must have unique names.")
    }
    if (any(c("self", "private") %in%                           # styler: off
            c(names(public), names(private), names(active)))) { # styler: off
        stop("Items cannot use reserved names 'self' or 'private'.")
    }
    if (!all_functions(active)) {
        stop("All items in active must be functions.")
    }
    namespace <- new.env(parent = emptyenv())

    # use the same name of `R6ClassGenerator` object.
    namespace$portable <- FALSE
    namespace$classname <- paste0(Class, "Namespace")
    namespace$cloneable <- FALSE
    # namespace$class <- FALSE
    namespace$lock_class <- TRUE
    # https://github.com/r-lib/roxygen2/blob/main/R/object-r6.R#L139
    # namespace$inherit <- NULL # must be NULL for roxygen2 work ?
    # attr(namespace, "name") <- paste0(Class, "Namespace")

    # assign methods into namespace
    public_fields <- get_nonfunctions(public)
    public_methods <- get_functions(public)
    if (length(public_fields)) {
        namespace$public_fields <- public_fields
    }
    if (length(public_methods)) {
        namespace$public_methods <- public_methods
    }

    private_fields <- get_nonfunctions(private)
    private_methods <- get_functions(private)
    if (length(private_fields)) {
        namespace$private_fields <- private_fields
    }
    if (length(private_methods)) {
        namespace$private_methods <- private_methods
    }
    if (length(active)) namespace$active <- active

    # we assign a `R6ClassGenerator` class, in this ways,
    # we can utilize roxygen2 R6 style document
    structure(namespace,
        class = c(paste0(Class, "Namespace"), "R6ClassGenerator")
    )
}

# function used to create `$.Class` and `[[.Class` method
namespace_bind_public <- function(.__class__, .__namespace__) {
    force(.__class__)
    force(.__namespace__)
    function(self, .__name__) {
        # 1. environment used to find `self`, and `private`
        # 2. `self` should be the data value
        # the parent environment of this environment is the package namespace
        # it's safe to just change the function environment in this package
        force(self)
        .__enclos_env__ <- environment()
        .__enclos_env__$private <- structure(list(self),
            class = paste0(.__class__, "PrivateNamespace")
        )
        # styler: off
        if (!is.null(.__public_env__ <- .subset2(.__namespace__,
                     "public_methods")) &&
            !is.null(.__fn__ <- .subset2(.__public_env__, .__name__))) {
            environment(.__fn__) <- .__enclos_env__
            .__fn__
        } else if (!is.null(.__public_env__ <- .subset2(.__namespace__,
                           "public_fields")) &&
            !is.null(.__fn__ <- .subset2(.__public_env__, .__name__))) {
            .__fn__
        } else if (!is.null(.__active_env__ <- .subset2(.__namespace__,
                            "active")) &&
            !is.null(.__fn__ <- .subset2(.__active_env__, .__name__))) {
            environment(.__fn__) <- .__enclos_env__
            makeActiveBinding(".__active_fn__", .__fn__, .__enclos_env__)
            .__active_fn__ # nolint
        } else {
            stop(sprintf("No method `%s` found for %s", .__name__, .__class__))
        }
        # styler: on
    }
}

utils::globalVariables(".__active_fn__")

namespace_bind_private <- function(.__class__, .__namespace__) {
    force(.__class__)
    force(.__namespace__)
    function(self, .__name__) {
        self <- .subset2(self, "self")
        .__enclos_env__ <- environment()
        .__enclos_env__$private <- structure(list(self),
            class = paste0(.__class__, "PrivateNamespace")
        )
        # styler: off
         if (!is.null(.__private_env__ <- .subset2(.__namespace__,
                     "private_methods")) &&
            !is.null(.__fn__ <- .subset2(.__private_env__, .__name__))) {
            environment(.__fn__) <- .__enclos_env__
            .__fn__
        } else if (!is.null(.__private_env__ <- .subset2(.__namespace__,
                           "private_fields")) &&
            !is.null(.__fn__ <- .subset2(.__private_env__, .__name__))) {
            .__fn__
        } else {
            stop(sprintf("No private method `%s` found for %s", .__name__, .__class__))
        }
        # styler: on
    }
}

# nocov end
