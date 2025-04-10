# ---
# repo: Yunuuuu/standalone
# file: standalone-polars.R
# last-updated: 2025-02-26
# license: https://unlicense.org
# dependencies: [standalone-pkg.R]
# imports: [rlang]
# ---

# This file contains several helper functions for working with the polars
# package.

# Note: Since `polars` is not available on `CRAN`, if you plan to publish your
# package on `CRAN`, please include the following in your `DESCRIPTION` file:
# `Additional_repositories: https://community.r-multiverse.org` and run
# `usethis::use_package("polars", "Suggests")`. For each user-defined function
# that uses polars, be sure to add `use_polars()` as the first line in the
# function

# ## Changelog
# 2025-04-10
# - Remove `series_lapply`, please use other package like mirai instead
#
# 2025-02-26:
# - Add `install_polars`
# - Add `use_polars`
# - Add `series_lapply`
#
# nocov start

pl <- NULL

# Helper function to install `polars` package
#
# Helper function to install `polars` package from
# [`r-multiverse`](https://community.r-multiverse.org).
install_polars <- function() {
    orepos <- getOption("repos")
    options(repos = c("https://community.r-multiverse.org", orepos))
    on.exit(options(repos = orepos), add = TRUE)
    install_pkgs("polars")
}

use_polars <- function(reason) {
    if (is.null(pl)) {
        if (!is_installed("polars")) {
            if (missing(reason)) {
                reason <- sprintf("to use `%s` package", pkg_nm())
            }
            orepos <- getOption("repos")
            options(repos = c("https://community.r-multiverse.org", orepos))
            on.exit(options(repos = orepos), add = TRUE)
            rlang::check_installed("polars", reason = reason)
        }
        utils::assignInNamespace("pl",
            getExportedValue("polars", "pl"),
            ns = pkg_namespace()
        )
    }
}

# nocov end
