
# standalone

<!-- badges: start -->
<!-- badges: end -->

The `standalone` package offers helper scripts designed for use in other R
packages. You can integrate these scripts into your project with a single
command, making it easy to leverage utility functions across packages without
adding dependency overhead.

To use `standalone` scripts in your R package, simply run:

``` r
usethis::use_standalone("Yunuuuu/standalone")
```

# Helper Scripts Available

 - `assert`: Argument-checking helpers to ensure inputs are valid.
 - `cli`: Functions to format messages using the `cli` package if it is available.
 - `obj-type`: Functions to generate informative messages about an object's class.
 - `stringr`: Base R functions that mimic `stringr` syntax, enabling string manipulation without requiring `stringr` pacakge.
 - `vctrs`: Provides base implementations for functions from `dplyr`, `tidyr`,
   and `tibble` using `vctrs`, allowing data manipulation functionalities
   without needing these package dependencies.