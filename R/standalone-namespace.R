# ---
# repo: Yunuuuu/standalone
# file: standalone-namespace.R
# last-updated: 2025-03-03
# license: https://unlicense.org
# imports: [utils]
# ---

# create New namespace, which is just an environment
# 1. For every new namespace, we must add a new S3 method `$.ClassName` and
#    `[[.ClassName` to bind the namespace into the special class
#    (dispatch_method).
# 2. We can use `self` to refer the input data (other methods or fields can
#    be directly acquired by `self$`)
# 3. For a sub-namespace, we must define an `active` function to change the
#    underlying class of the data value in the parent namepace. Then add a new
#    S3 method `$.ClassName` and `[[.ClassName` to bind the sub-namespace into
#    the new class
# 4. we can also check the data value match a special data type for the
#    sub-namespace
namespace <- function(Class, public = list(), active = list()) {
    ns <- new_namespace(public, active)
    method <- new_method(Class, ns)
    package <- utils::packageName()
    registerS3method("$", Class, method, envir = getNamespace(package))
    registerS3method("[[", Class, method, envir = getNamespace(package))
    ns
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

new_namespace <- function(public = list(), private = list(), active = list()) {
    if (!all_named(public) || !all_named(active)) {
        stop("All elements of public, and active must be named.")
    }
    allnames <- c(names(public), names(private), names(active))
    if (anyDuplicated(allnames)) {
        stop("All items in public, private, and active must have unique names.")
    }
    if (any(c("self", "private") %in% allnames)) {
        stop("Items cannot use reserved names 'self' or 'private'.")
    }
    if (!all_functions(public)) {
        stop("All items in public must be functions.")
    }
    if (!all_functions(private)) {
        stop("All items in private must be functions.")
    }
    if (!all_functions(active)) {
        stop("All items in active must be functions.")
    }
    namespace <- new.env(parent = emptyenv())
    namespace$public <- new.env(parent = emptyenv())
    namespace$private <- new.env(parent = emptyenv())
    namespace$active <- new.env(parent = emptyenv())
    list2env(public, namespace$public)
    list2env(private, namespace$private)
    list2env(active, namespace$active)
    namespace
}

# function used to create `$.ClassName` and `[[.ClassName` method
new_method <- function(.__class__, .__namespace__) {
    # we use special name to prevent from overriding other function (also known
    # as private methods) defined in this package
    force(.__class__)
    force(.__namespace__)
    function(self, .__name__) {
        force(self)
        private <- .subset2(.__namespace__, "private") # nolint

        # insert a new stack to the function, used to find `self`, `self` should
        # be the data value.
        .__public_env__ <- .subset2(.__namespace__, "public")
        .__active_env__ <- .subset2(.__namespace__, "active")
        if (exists(.__name__, envir = .__public_env__, inherits = FALSE)) {
            .__fn__ <- .subset2(.__public_env__, .__name__)
            environment(.__fn__) <- environment()
            .__fn__
        } else if (exists(.__name__, envir = .__active_env__,   # styler: off
                          inherits = FALSE)) {                  # styler: off
            .__fn__ <- .subset2(.__active_env__, .__name__)
            environment(.__fn__) <- environment()
            makeActiveBinding(".__active_fn__", .__fn__, environment())
            .__active_fn__ # nolint
        } else {
            stop(sprintf(
                "No method `%s()` found for Class `%s`", .__name__, .__class__
            ))
        }
    }
}

utils::globalVariables(".__active_fn__")
