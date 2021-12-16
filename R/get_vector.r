## -------->>  [[file:../harmonizer.src.org::*get_vector][get_vector:1]]
get_col_as_number <- function(col, x, placement = NULL) {
    if(is.character(col)) {
        which(names(x) %in% col)
    } else if(is.null(placement)) {
        col
    } else {
        ## adjust for prepened cols
        switch(
            placement
          , replace_col = col
          , append_to_col = col
          , prepend_to_col = col + 1
          , append_to_x = col
          , prepend_to_x = col + 1)
    }
}

get_target_name <- function(col, x, name, name_suffix) {
    if(!is.null(name)) {
        return(name)
    } else {
        return(paste0(names(x)[col], name_suffix))
    }
}


get_target_col <- function(col, x, placement) {
    switch(placement
         , append_to_col = col + 1
         , prepend_to_col = col - 1
         , append_to_x = ncol(x)
         , prepend_to_x = 1)
} 


get_moving_target <- function(dots, x) {
    with(dots, {
        ## if we replacing things then it is the same column
        if(placement == "replace_col") {
            return(get_col_as_number(col, x))
        } else {
            col_after_prepend <- get_col_as_number(col, x, placement)
            if(col_after_prepend > ncol(x)) {
                ## nothing was prepended then assume first run
                return(get_col_as_number(col, x))
            }
            target_col_candidate <- get_target_col(col_after_prepend, x, placement)
            if(target_col_candidate == 0 ||
               target_col_candidate > ncol(x)) {
                ## if target is not valid assume first run
                return(get_col_as_number(col, x))
            }
            target_name <- get_target_name(col_after_prepend, x, name, name_suffix)
            target_name_candidate <- names(x)[target_col_candidate]
            if(target_name_candidate != target_name) {
                ## case of the first call of get_vector
                return(get_col_as_number(col, x))
            } else {
                ## case of subsequent calls
                return(target_col_candidate)
            }
        }
    })
}



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
    with(dots <- get_harmonize_options(), {
        ## check arguments
        check_harmonize_options(dots, x)
        ## select col
        if(is.atomic(x)) {
            x_col <- harmonize_defactor(x)
        } else {
            moving_target <- get_moving_target(dots, x)
            x_col <- harmonize_defactor(x[[moving_target]])
        }
        ## select rows
        if(is.null(rows)) {
            return(x_col)
        } else {
            return(x_col[rows])
        }
    })
}




## functions that only runs within get_vector and inset_vector
## --------------------------------------------------------------------------------

## Tests Arguments
col_name_at_place = check_args_col_rows <- function() {
    .Deprecated("check_col")
    .Deprecated("check_rows")
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


