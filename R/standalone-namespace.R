# ---
# repo: Yunuuuu/standalone
# file: standalone-namespace.R
# last-updated: 2025-03-03
# license: https://unlicense.org
# imports: [utils]
# ---

# create New namespace, which is just an environment
# 1. We can use `self` to refer the input data (other methods or fields can
#    be directly acquired by `self$`)
# 2. We can add private methods and fields
# 3. We must create new class to bind this namespace
namespace <- function(Class, public = list(), private = list(), active = list(),
                      parent_env = parent.frame()) {
    ns <- new_namespace(
        public = public, private = private,
        active = active, parent_env = parent_env
    )
    method <- subset_method(Class, ns)
    package <- utils::packageName()
    registerS3method("$", Class, method, envir = getNamespace(package))
    registerS3method("[[", Class, method, envir = getNamespace(package))
    registerS3method("[", Class, function(x, i) {
        stop("Please use `$` or `[[` method instead")
    }, envir = getNamespace(package))
    registerS3method(".DollarNames", Class, function(x, pattern) {
        names <- ls(ns, all.names = TRUE)
        names <- names[grepl(pattern, names)]
        setdiff(names, c("self", "private"))
    }, envir = getNamespace(package))
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

assign_func_envs <- function(objs, target_env) {
    if (is.null(target_env)) return(objs) # styler: off

    lapply(objs, function(f) {
        if (is.function(f)) environment(f) <- target_env
        f
    })
}

new_namespace <- function(public = list(), private = list(), active = list(),
                          parent_env = parent.frame()) {
    if (!all_named(public) || !all_named(private) || !all_named(active)) {
        stop("All elements of public, private, and active must be named.")
    }
    allnames <- c(names(public), names(private), names(active))
    if (anyDuplicated(allnames)) {
        stop("All items in public, private, and active must have unique names.")
    }
    if (any(c("self", "private") %in% allnames)) {
        stop("Items cannot use reserved names 'self' or 'private'.")
    }
    if (!all_functions(active)) {
        stop("All items in active must be functions.")
    }

    # used to bind `self` and `private` pointer
    enclos_env <- new.env(parent = parent_env)
    enclos_env$private <- new.env(parent = emptyenv())

    public_fields <- get_nonfunctions(public)
    public_methods <- get_functions(public)
    public_methods <- assign_func_envs(public_methods, enclos_env)
    if (length(public_fields)) list2env(public_fields, enclos_env)
    if (length(public_methods)) list2env(public_methods, enclos_env)

    private_fields <- get_nonfunctions(private)
    private_methods <- get_functions(private)
    private_methods <- assign_func_envs(private_methods, enclos_env)
    if (length(private_fields)) list2env(private_fields, enclos_env$private)
    if (length(private_methods)) {
        list2env(private_methods, enclos_env$private)
        for (name in names(private_methods)) {
            lockBinding(name, enclos_env$private)
        }
    }
    lockEnvironment(enclos_env$private)

    active <- assign_func_envs(active, enclos_env)
    for (i in seq_along(active)) {
        makeActiveBinding(
            names(active)[i], .subset2(active, i),
            enclos_env
        )
    }
    enclos_env
}

# function used to create `$.Class` and `[[.Class` method
subset_method <- function(.__class__, .__namespace__) {
    force(.__class__)
    force(.__namespace__)
    function(self, .__name__) {
        .__namespace__$self <- self
        on.exit(rm(list = "self", envir = .__namespace__, inherits = FALSE))
        if (exists(.__name__, envir = .__namespace__, inherits = FALSE)) {
            get(.__name__, envir = .__namespace__, inherits = FALSE)
        } else {
            stop(sprintf(
                "No method `%s()` found for Class `%s`", .__name__, .__class__
            ))
        }
    }
}
