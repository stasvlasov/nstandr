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

infer_post_inset_col_from_pre_inset_col <- function(col, x, output) {
    if(is.character(col)) {
        which(names(x) %in% col)
    } else {
        switch(
            output
          , replace_col = col
          , append_to_col = col
          , prepend_to_col = col + 1
          , append_to_x = col
          , prepend_to_x = col + 1)
    }
}

infer_if_post_inset_col_possible <- function(col, x, output) {
    col <- get_col_as_number(col, x)
    switch(
        output
      , replace_col = TRUE
      , append_to_col = ifelse(col == x_width(x), FALSE, TRUE)
      , append_to_x = ifelse(col == x_width(x), FALSE, TRUE)
      , prepend_to_x = ifelse(col == 1, FALSE, TRUE)
      , prepend_to_col = ifelse(col == 1, FALSE, TRUE)
    )
}


infer_moving_target_from_post_inset_col <- function(col, x, output, as_name = FALSE) {
    col <- get_col_as_number(col, x)
    return_col <- switch(
        output
      , replace_col = col
      , append_to_col = col + 1
      , prepend_to_col = col - 1
      , append_to_x = x_width(x)
      , prepend_to_x = 1)
    if(as_name) {
        names(x)(return_col)
    } else {
        return_col
    }
}


## this assumes that nothing else was never added...
infer_moving_target_from_pre_inset_col <- function(col, x, output, as_name = FALSE) {
    col <- get_col_as_number(col, x)
    return_col <- switch(
        output
      , replace_col = col
      , append_to_col = col + 1
      , prepend_to_col = col
      , append_to_x = x_width(x)
      , prepend_to_x = 1)
    if(as_name) {
        names(x)(return_col)
    } else {
        return_col
    }
}

## assume that other stuff is always append to x or col so inference will keep working
infer_moving_target_from_names <- function(dots, x
                                         , return_null_for_new_col = FALSE
                                         , return_name_for_new_col = FALSE) {
    with(dots, {
        if(output == "replace_col") return(get_col_as_number(col, x))
        col_post_inset <- infer_post_inset_col_from_pre_inset_col(col, x, output)
        if(infer_if_post_inset_col_possible(col_post_inset, x, output)) {
            target_name_generated <-
                make_target_name(col_post_inset, x, name, name_suffix)
            if(target_name_generated %in% names(x)) {
                ## case of subsequent calls
                return(get_col_as_number(target_name_generated, x))
            }
        }
        if(return_null_for_new_col) return(NULL)
        if(return_name_for_new_col) return(make_target_name(col, x, name, name_suffix))
        get_col_as_number(col, x)
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
get_target <- function(x, return_null_for_new_col = FALSE, ...) {
    with(dots <- get_harmonize_options(), {
        ## check arguments
        check_harmonize_options(dots, x)
        col <- infer_moving_target_from_names(dots, x, return_null_for_new_col)
        if(is.null(col)) return(NULL)
        get_vector(x, col, rows, check_x_col_rows = FALSE)
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
##' @param x Data to harmonize. Character vector or data.frame or
##'     data.table
##' @param omitted_rows_values_for_new_col Alternative value
##'     `omitted_rows_values` to use in case we create new column in
##'     x. For example, it is use in insetting codes to avoid the
##'     default `omitted_rows_values` use initial `col` in which case
##'     codes will be mixed with input values
##' @param allow_na_in_vector Whether to allow NA in inset vector
##' @param which_call_to_report System call number (e.g., -2L) to
##'     include in report if arguments checks fails
##' @param return_only_target_col If toggled to TRUE then only return
##'     the vector to be inset (output argument is ignored)
##' @return Data.table or character vector
##' @inheritDotParams harmonize_options
inset_target <- function(vector, x
                       , omitted_rows_values_for_new_col = NULL
                       , allow_na_in_vector = TRUE
                       , which_call_to_report = -5L
                       , return_only_target_col = FALSE
                       , ...) {
    checkmate::assert_flag(allow_na_in_vector)
    checkmate::assert_flag(return_only_target_col)
    vector <- defactor_vector(vector)
    with(dots <- get_harmonize_options(), {
        ## check harmonize_options
        check_harmonize_options(dots, x)
        assertion_fails <- checkmate::makeAssertCollection()
        ## -----
        ## inset omitted_rows_values if needed
        ## -----
        checkmate::assert_multi_class(vector
                                    , classes = c("list", "character", "logical", "numeric")
                                    , add = assertion_fails)
        if(!is.null(rows)
           && ((is.logical(rows) && !all(rows))
               || (is.numeric(rows) && !setequal(rows, 1:x_length(x))))) {
            ## check vector lenth
            getFromNamespace(paste0("assert_", class(vector)), "checkmate")(
                vector
              , len = ifelse(is.numeric(rows), length(rows), sum(rows))
              , any.missing = allow_na_in_vector
              , add = assertion_fails
            )
            report_arg_checks(assertion_fails, which_call_to_report)
            ## process `omitted_rows_values`
            omitted_rows_values_col <-
                infer_moving_target_from_names(
                    dots
                  , x
                  , return_null_for_new_col =
                        !is.null(omitted_rows_values_for_new_col))
            if(is.null(omitted_rows_values_col) &&
               is.null(omitted_rows_values)) {
                omitted_rows_values <- omitted_rows_values_for_new_col
            }
            omitted_rows_values <-
                get_vector(x
                         , col = omitted_rows_values_col
                         , fallback_value = omitted_rows_values
                         , fallback_value_supersedes = TRUE
                         , check_x_col_rows = FALSE)
            ## inject ommited rows
            vector <- `[<-`(omitted_rows_values, rows, vector)
        } else {
            ## just check the vector length
            getFromNamespace(paste0("assert_", class(vector)), "checkmate")(
                vector
              , len = x_length(x)
              , any.missing = allow_na_in_vector
              , add = assertion_fails
            )
            report_arg_checks(assertion_fails, which_call_to_report)
            if(is.numeric(rows)) {
                ## case of permutations for same length
                vector <- vector[rows]
            }
        }
        ## -----
        ## inset full vector
        ## -----
        if(return_only_target_col) {
            x <- vector
        } else if(output != "omit") {
            if(is.atomic(x) && output == "replace_col") {
                ## just replace x if it is atomic
                x <- vector
            } else {
                x <- defactor(x, conv2dt = "all")
                width_pre_inset <- x_width(x)
                col_post_inset <- infer_post_inset_col_from_pre_inset_col(col, x, output)
                col_or_name_if_new <-
                    infer_moving_target_from_names(dots, x, return_name_for_new_col = TRUE)
                ## fuckin data.table syntax is so cryptic
                ## [] at the end ensures that returned DT is printed
                x[, (col_or_name_if_new) := vector][]
                ## x[[col_or_name_if_new]] <- vector
                ## now if we added new col
                if(x_width(x) == width_pre_inset + 1) {
                    ## if new col was added place last col into target posision
                    target <- infer_moving_target_from_post_inset_col(col_post_inset, x, output)
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
        if(append_copy & !return_only_target_col) {
            x <- defactor(x, conv2dt = "all")
            col_post_inset <- infer_post_inset_col_from_pre_inset_col(col, x, output)
            append_copy_name <- format_append_copy(append_copy_name_format, name = names(x)[col_post_inset])
            checkmate::assert_names(append_copy_name, add = assertion_fails)
            report_arg_checks(assertion_fails, which_call_to_report)
            ## [] at the end ensures that returned DT is printed
            x[, (append_copy_name) := vector][]
        }
        report_arg_checks(assertion_fails, which_call_to_report)
        return(x)
    })
}
## --------<<  get_target & inset_target:1 ends here


