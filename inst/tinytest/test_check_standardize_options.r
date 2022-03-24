## -------->>  [[file:../../harmonizer.src.org::*check_standardize_options][check_standardize_options:2]]
require("data.table")

check_standardize_options <- nstandr:::check_standardize_options
get_dots <- nstandr:::get_dots
standardize_options <- nstandr:::standardize_options


testing_check_arguments <- function(x, ...) {
    dots <- get_dots(standardize_options
                   , search_calls_with_formals = c("x", "...")
                   , search_depth = 5L
                   , search_up_to_call = c("standardize", "nstandr::standardize"))
    check_standardize_options(dots, x)
    return(TRUE)
}

expect_true(testing_check_arguments(data.table(a = c(1,2), b = c("a", "b")), output_col_name = "c"))
## bad output_placement name
expect_error(testing_check_arguments(data.table(a = c(1,2), b = c("a", "b")), output_placement = "omitted"))


testing_check_arguments <- function(x, ...) {
    dots <- get_dots(standardize_options
                   , search_calls_with_formals = c("x", "...")
                   , search_depth = 5L
                   , search_up_to_call = c("standardize", "nstandr::standardize"))
    check_standardize_options(dots, x
                          , check_name_duplicates = TRUE)
    return(TRUE)
}


## taken name
expect_error(testing_check_arguments(data.table(a = c(1,2), b = c("a", "b")), output_col_name = "b"))
## testing append copy prefix (taken name)
expect_error(testing_check_arguments(data.table(a = c(1,2), b = c("a", "b"), a_standardized = TRUE)))
## test rows
expect_error(testing_check_arguments(data.table(a = c(1,2), b = c("a", "b")), rows = c(1,1)))
## --------<<  check_standardize_options:2 ends here


