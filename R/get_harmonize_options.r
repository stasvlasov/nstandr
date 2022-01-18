## -------->>  [[file:../harmonizer.src.org::*get_harmonize_options][get_harmonize_options:1]]
##' Gets `harmonize_options` at point with consistent updates up through calling stack.
##'
##' Limited to max stack of 3 calls and calls that include at least `data` and `...` formals (`harmonizer` functions specific) up to `.GlobalEnv` or `harmonize` call.
##' 
##' @return Returns list of updated arguments specified in `harmonize_options` function
##' 
##' 
##' @md 
get_harmonize_options <- function() {
    evalq({
        get_dots(harmonize_options
               , search_while_calls_have_formals = c("x", "...")
               , search_up_nframes = 50L
               , search_up_to_call = c("harmonize", "harmonizer::harmonize")
               , skip_checks_for_parent_call = FALSE)
    }, envir = parent.frame())
}


get_col_and_rows <- function() {
    evalq({
        get_dots(harmonize_options
               , select_args = c("col", "rows")
               , search_while_calls_have_formals = c("x", "...")
               , search_up_nframes = 5L
               , search_up_to_call = c("harmonize", "harmonizer::harmonize")
               , skip_checks_for_parent_call = FALSE)
    }, envir = parent.frame())
}
## --------<<  get_harmonize_options:1 ends here


