## -------->>  [[file:../../harmonizer.src.org::*check_harmonize_options][check_harmonize_options:2]]
require("data.table")

check_harmonize_options <- harmonizer:::check_harmonize_options
get_dots <- harmonizer:::get_dots
harmonize_options <- harmonizer:::harmonize_options


testing_check_arguments <- function(x, ...) {
    dots <- get_dots(harmonize_options
                   , search_calls_with_formals = c("x", "...")
                   , search_depth = 5L
                   , search_up_to_call = c("harmonize", "harmonizer::harmonize"))
    check_harmonize_options(dots, x)
    return(TRUE)
}

expect_true(testing_check_arguments(data.table(a = c(1,2), b = c("a", "b")), name = "c"))
## bad output name
expect_error(testing_check_arguments(data.table(a = c(1,2), b = c("a", "b")), output = "omitted"))


testing_check_arguments <- function(x, ...) {
    dots <- get_dots(harmonize_options
                   , search_calls_with_formals = c("x", "...")
                   , search_depth = 5L
                   , search_up_to_call = c("harmonize", "harmonizer::harmonize"))
    check_harmonize_options(dots, x
                          , check_name_duplicates = TRUE)
    return(TRUE)
}


## taken name
expect_error(testing_check_arguments(data.table(a = c(1,2), b = c("a", "b")), name = "b"))
## testing append copy prefix (taken name)
expect_error(testing_check_arguments(data.table(a = c(1,2), b = c("a", "b"), a_harmonized = TRUE)))
## test rows
expect_error(testing_check_arguments(data.table(a = c(1,2), b = c("a", "b")), rows = c(1,1)))
## --------<<  check_harmonize_options:2 ends here


