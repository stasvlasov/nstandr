## -------->>  [[file:../harmonizer.src.org::*get_target & inset_target][get_target & inset_target:1]]
get_col_as_number <- function(col, x) {
    if(is.character(col)) {
        which(names(x) %in% col)
    } else {
        col
    }
}

make_target_name <- function(col, x, name, name_suffix) {
    if(!is.null(name)) {
        return(name)
    } else {
        return(paste0(names(x)[col], name_suffix))
    }
}

infer_post_inset_col_from_pre_inset_col <- function(col, x, placement) {
    if(is.character(col)) {
        which(names(x) %in% col)
    } else {
        switch(
            placement
          , replace_col = col
          , append_to_col = col
          , prepend_to_col = col + 1
          , append_to_x = col
          , prepend_to_x = col + 1)
    }
}

infer_if_post_inset_col_possible <- function(col, x, placement) {
    col <- get_col_as_number(col, x)
    switch(
        placement
      , replace_col = TRUE
      , append_to_col = ifelse(col == ncol(x), FALSE, TRUE)
      , prepend_to_col = ifelse(col == 1, FALSE, TRUE)
      , append_to_x = ifelse(col == ncol(x), FALSE, TRUE)
      , prepend_to_x = ifelse(col == 1, FALSE, TRUE))
}


infer_moving_target_from_post_inset_col <- function(col, x, placement, as_name = FALSE) {
    col <- get_col_as_number(col, x)
    return_col <- switch(
        placement
      , replace_col = col
      , append_to_col = col + 1
      , prepend_to_col = col - 1
      , append_to_x = ncol(x)
      , prepend_to_x = 1)
    if(as_name) {
        names(x)(return_col)
    } else {
        return_col
    }
}


## this assumes that nothing else was never added...
infer_moving_target_from_pre_inset_col <- function(col, x, placement, as_name = FALSE) {
    col <- get_col_as_number(col, x)
    return_col <- switch(
        placement
      , replace_col = col
      , append_to_col = col + 1
      , prepend_to_col = col
      , append_to_x = ncol(x)
      , prepend_to_x = 1)
    if(as_name) {
        names(x)(return_col)
    } else {
        return_col
    }
}

## assume that other stuff is always append to x or col so inference will keep working
infer_moving_target_from_names <- function(dots, x, return_name_for_new_col = FALSE) {
    with(dots, {
        if(placement == "replace_col") {
            return(get_col_as_number(col, x))
        } else if(infer_if_post_inset_col_possible(col, x, placement)) {
            target_name_generated <-
                infer_post_inset_col_from_pre_inset_col(col, x, placement) |>
                make_target_name(x, name, name_suffix)
            if(target_name_generated %in% names(x)[target]) {
                ## case of subsequent calls
                return(get_col_as_number(target_name_generated, x))
            }
        }
        col_or_new_name <- ifelse(return_name_for_new_col
                                , make_target_name(col, x, name, name_suffix)
                                , get_col_as_number(col, x))
        return(col_or_new_name)
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
get_target <- function(x, ...) {
    with(dots <- get_harmonize_options(), {
        ## check arguments
        check_harmonize_options(dots, x)
        get_vector(x
                 , col = infer_moving_target_from_names(dots, x)
                 , rows = rows
                 , check_x_col_rows = FALSE)
    })
}



format_append_copy <- function(format, name = "") {
    procedure_number <- 
        get0("harmonizer_harmonize_procedure_number", ifnotfound = "", envir = parent.frame())
   procedure_name <-
        get0("harmonizer_harmonize_procedure_name", ifnotfound = "", envir = parent.frame())
    lowdash_procedure_number <-
        ifelse(procedure_number == "", "", paste0("_", procedure_number))
    lowdash_procedure_name <-
        ifelse(procedure_name == "", "", paste0("_", procedure_name))
    lowdash_name <- ifelse(name == "", "", paste0("_", name))
    name_lowdash <- ifelse(name == "", "", paste0(name, "_"))
    stringi::stri_replace_all_fixed(format
                                  , pattern = c("{col_name}", "{_col_name}", "{col_name_}"
                                              , "{procedure_number}", "{_procedure_number}"
                                              , "{procedure_name}", "{_procedure_name}")
                                  , replacement = c(name, lowdash_name, name_lowdash
                                                   , procedure_number, lowdash_procedure_number
                                                   , procedure_name, lowdash_procedure_name)
                                  , vectorise_all = FALSE)
}

##' Insets target vector back to input object (`x`)
##' 
##' @param vector Character vector to inset into the `x` object
##' @param x Data to harmonize. Character vector or data.frame or data.table
##' @return Data.table or character vector
##' @inheritDotParams harmonize_options
inset_target <- function(vector, x, ...) {
    vector <- harmonize_defactor_vector(vector)
    with(dots <- get_harmonize_options(), {
        ## check harmonize_options
        check_harmonize_options(dots, x)
        ## -----
        ## inset ommitted_rows_values if needed
        ## -----
        if(!is.null(rows)
           && ((is.logical(rows) && !all(rows))
               || (is.numeric(rows) && !setequal(rows, 1:x_length(x))))) {
            ## check vector lenth
            if(is.logical(rows)) {
                checkmate::assert_character(vector, len = sum(rows))
            } else if(is.numeric(rows)) {
                checkmate::assert_character(vector, len = length(rows))
            }
            ## process `ommitted_rows_values`
            ommitted_rows_values <- get_vector(x
                                             , col = infer_moving_target_from_names(dots, x)
                                             , fallback_value = ommitted_rows_values
                                             , fallback_value_ignored_if_col = FALSE
                                             , check_x_col_rows = FALSE)
            ## inject ommited rows
            vector <- `[<-`(ommitted_rows_values, rows, vector)

        } else {
            ## just check the vector length
            checkmate::assert_character(vector, len = x_length(x))
            if(is.numeric(rows)) {
                ## case of permutations for same length
                vector <- vector[rows]
            }
        }
        ## -----
        ## inset full vector
        ## -----
        if(placement != "omit") {
            if(is.atomic(x) && placement == "replace_col") {
                ## just replace x if it is atomic
                x <- vector
            } else {
                x <- harmonize_defactor(x, conv2dt = "all")
                width_pre_inset <- x_width(x)
                col_post_inset <- infer_post_inset_col_from_pre_inset_col(col, x, placement)
                col_or_name_if_new <-
                    infer_moving_target_from_names(dots, x, return_name_for_new_col = TRUE)
                ## fuckin data.table syntax is so cryptic
                x[, (col_or_name_if_new) := vector]
                ## now if we added new col
                if(x_width(x) == width_pre_inset + 1) {
                    ## if new col was added place last col into target posision
                    target <- infer_moving_target_from_post_inset_col(col_post_inset, x, placement)
                    cols_nums <-
                        1:width_pre_inset |>
                        append(width_pre_inset + 1, after = target - 1)
                    data.table::setcolorder(x, cols_nums)
                }
            }
        }
        ## -----
        ## apped copy
        ## -----
        if(append_copy) {
            x <- harmonize_defactor(x, conv2dt = "all")
            col_post_inset <- infer_post_inset_col_from_pre_inset_col(col, x, placement)
            append_copy_name <- format_append_copy(append_copy_name_format, name = names(x)[col_post_inset])
            checkmate::assert_names(append_copy_name)
            x[, (append_copy_name) := vector]
        }
        return(x)
    })
}
## --------<<  get_target & inset_target:1 ends here


