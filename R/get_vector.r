## -------->>  [[file:../harmonizer.src.org::*get_vector][get_vector:1]]
##' Gets vector by column and defactor if needed. Optionaly one can provide a fallback_value which will be returned if col is not specified.
##'
##' @param x Input data. Can be vector, data.frame or a data.table
##' @param col Column of interest in the input data `x`. The vector we would like to work with. Ignored if input `x` is a atomic vector.
##' @param rows Rows of interest
##' @param choices Optional set of choices that return value should be subset of. Do no check if it is NULL.
##' @param fallback_value If col is NULL then return this value (but see `fallback_value_ignored_if_col` argument). Also check if it is a character vector and same lenght as x. If it is of length 1 then replicate it to match x's length.
##' @param fallback_value_ignored_if_col A bolean toggle. When set if col is provided then fallback_value_is_simply ignored. Otherwise col is ignored when fallback_value is provided.
##' @param fallback_value_any_missing Toggle check if missing values are allowed in fallback_value
##' @param fallback_value_ensure_length Toggle if fallback_value should we make it same length as `x`
##' @param check_x_col_rows Toggle wheather to use check_x, check_col, check_rows?
##' @param which_call_to_report Which call to report if argument checks fail.
##' @return A vector. Factors in imput `data` are converted to string.
##'
##' @md
get_vector <- function(x, col
                     , rows = NULL
                     , choices = NULL
                     , fallback_value = NULL
                     , fallback_value_ignored_if_col = TRUE
                     , fallback_value_any_missing = TRUE
                     , fallback_value_ensure_length = TRUE
                     , check_x_col_rows = TRUE
                     , which_call_to_report = -1L) {
    assertion_fails <- checkmate::makeAssertCollection()
    ## arg checks
    if (check_x_col_rows) check_x(x, which_call_to_report)
    checkmate::assert_flag(fallback_value_ignored_if_col, add = assertion_fails)
    checkmate::assert_flag(fallback_value_any_missing, add = assertion_fails)
    checkmate::assert_flag(fallback_value_ensure_length, add = assertion_fails)
    ## get vector
    if ((!fallback_value_ignored_if_col || is.null(col))
        && !is.null(fallback_value)) {
        ## check choices
        if (!is.null(choices)) {
            checkmate::assert_subset(fallback_value
                                   , choices = choices
                                   , fmatch = TRUE
                                   , add = assertion_fails)
        }
        ## get fallback vector
        if (fallback_value_ensure_length &&
            length(fallback_value) == 1) {
            checkmate::assert_string(fallback_value
                                   , na.ok = fallback_value_any_missing
                                   , add = assertion_fails)
            v <- rep(fallback_value, x_length(x))
        } else {
            checkmate::assert_character(fallback_value
                                      , any.missing = fallback_value_any_missing
                                      , len = x_length(x)
                                      , add = assertion_fails)
            v <- fallback_value
        }
    } else {
        ## get column
        if (is.atomic(x)) {
            v <- harmonize_defactor(x)
        } else {
            if (check_x_col_rows) check_col(col, x, which_call_to_report)
            v <- harmonize_defactor(x[[col]])
        }
        ## check choices
        if (!is.null(choices)) {
            checkmate::assert_subset(v
                                   , choices = choices
                                   , fmatch = TRUE
                                   , .var.name =
                                         paste0("x[[", checkmate::vname(col), "]]")
                                   , add = assertion_fails)
        }
    }
    ## select rows
    if (!is.null(rows)) {
        if (check_x_col_rows) check_rows(rows, x, which_call_to_report = which_call_to_report)
        v <- v[rows]
    }
    report_arg_checks(assertion_fails, which_call_to_report)
    return(v)
}
## --------<<  get_vector:1 ends here


