## -------->>  [[file:../nstandr.src.org::*get_standardize_options][get_standardize_options:1]]
##' Gets `standardize_options` at point with consistent updates up through calling stack.
##'
##' Limited to max stack of 3 calls and calls that include at least `data` and `...` formals (`nstandr` functions specific) up to `.GlobalEnv` or `standardize` call.
##' 
##' @return Returns list of updated arguments specified in `standardize_options` function
##' 
##' 
##' @md 
get_standardize_options <- function() {
    evalq({
        get_dots(standardize_options
               , search_calls_with_formals = c("x", "...")
               , search_depth = 500L
               ## , search_up_to_call = c("standardize", "nstandr::standardize")
               , skip_checks_for_parent_call = FALSE)
    }, envir = parent.frame())
}


get_col_and_rows <- function() {
    evalq({
        get_dots(standardize_options
               , select_args = c("col", "rows")
               , search_calls_with_formals = c("x", "...")
               , search_depth = 500L
               ## , search_up_to_call = c("standardize", "nstandr::standardize")
               , skip_checks_for_parent_call = FALSE)
    }, envir = parent.frame())
}
## --------<<  get_standardize_options:1 ends here


