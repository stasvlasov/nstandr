## -------->>  [[file:../harmonizer.src.org::*get_vector & inset_vector][get_vector & inset_vector:1]]
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

infer_moving_target_from_names <- function(dots, x, return_name_for_new_col = FALSE) {
    with(dots, {
        if(placement == "replace_col") {
            return(get_col_as_number(col, x))
        }
        col_or_new_name <- ifelse(return_name_for_new_col
                                , make_target_name(col, x, name, name_suffix)
                                , get_col_as_number(col, x))
        if(infer_if_post_inset_col_possible(col, x, placement)) {
            target_name_generated <-
                infer_post_inset_col_from_pre_inset_col(col, x, placement) |>
                make_target_name(x, name, name_suffix)
            target_name_generated |>
                print()
            target <- infer_moving_target_from_pre_inset_col(col, x, placement)
            target |>
                print()
            if(names(x)[target] == target_name_generated) {
                ## case of subsequent calls
                return(target)
            }
        }
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
get_vector <- function(x, ...) {
    with(dots <- get_harmonize_options(), {
        ## check arguments
        check_harmonize_options(dots, x)
        ## select col
        if(is.atomic(x)) {
            x_col <- harmonize_defactor(x)
        } else {
            moving_target <- infer_moving_target_from_names(dots, x)
            moving_target |>
                print()
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




##' Insets target vector back to input object (`data`)
inset_vector <- function(vector, x, ...) {
    with(dots <- get_harmonize_options(), {
        ## check harmonize_options
        check_harmonize_options(dots, x)
        ## inset ommitted_rows_values if needed
        if(!is.null(rows)
           && ((is.logical(rows) && !all(rows))
               || (is.numeric(rows) && !setequal(rows, 1:x_length(x))))) {
            ## check vector lenth
            if(is.logical(rows)) {
                checkmate::assert_character(vector, len = sum(rows))
            } else if(is.numeric(rows)){
                checkmate::assert_character(vector, len = length(rows))
            }
            ## process `ommitted_rows_values`
            if(is.null(ommitted_rows_values)) {
                moving_target <- infer_moving_target_from_names(dots, x)
                ommitted_rows_values <- harmonize_defactor(x[[moving_target]])
            }
            if(length(ommitted_rows_values) != x_length(x)) {
                ## assume `ommitted_rows_values` length 1
                ommitted_rows_values <- rep(ommitted_rows_values, x_length(x))
            }
            ## inject ommited rows
            vector_full <- ommitted_rows_values
            vector_full[rows] <- vector
            vector <- vector_full
        } else {
            ## just check the vector length
            checkmate::assert_character(vector, len = x_length(c))
            if(is.numeric(rows)) {
                ## case of permutations for same length
                vector <- vector[rows]
            }
        }
        ## inset full vector
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
        ## apped copy
        ## TODO
        ## x[, (col_or_name_if_new) := vector]
    })
    return(x)
}
## --------<<  get_vector & inset_vector:1 ends here


