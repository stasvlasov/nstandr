## -------->>  [[id:org:77wbsm80daj0][get_vector:1]]
##' Gets a target vector to harmonize.
##'
##' @param data Input data. Can be vector, data.frame or a data.table
##' @param col Column of interest in the input `data`. The vector we would like to work with. This parameter is ignored if input `data` is a vector (checked by `is.atomic`)
##' @param rows Rows of interest
##' @param ... Ignored arguments that are meant for `inset_vector`
##' @return A vector. Factors in imput `data` are converted to string.
##'
##' @md
get_vector <- function(x, ...) {
    dots <- get_dots(harmonize_options
                   , select_args = c("col", "rows")
                   , search_while_calls_have_formals = c("x", "...")
                   , search_up_nframes = 5L
                   , search_up_to_call = c("harmonize", "harmonizer::harmonize"))
    for (i in names(dots)) assign(i, dots[[i]])
    check_args_col_rows()
    harmonize_data_get_col(x, col)[rows]
}


## functions that only runs within get_vector and inset_vector
## --------------------------------------------------------------------------------

## Tests Arguments
check_args_col_rows <- function() {
    evalq({
        ## - check col
        if(harmonize_is_ok_col(col, x, required = TRUE)) {
            col %<>% ifelse(is.numeric(.), ., match(., names(x)))
        }
        ## - check rows
        if(!harmonize_is_ok_type(rows, harmonize_data_length(x), type = "logical")) {
            rows <- TRUE  # select all if rows NULL
        }
    }, envir = parent.frame())
}
## --------<<  get_vector:1 ends here


