## -------->>  [[file:../harmonizer.src.org::*get_target & inset_target][get_target & inset_target:2]]
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
    if(is.null(x)) return(NULL)
    with(dots <- get_harmonize_options(), {
        ## check arguments
        check_harmonize_options(dots, x)
        x_names <- if (is.atomic(x)) x_atomic_name else names(x)
        col <- infer_moving_target_from_names(x_names
                                            , col
                                            , output_col_name
                                            , output_placement
                                            , return_null_for_new_col)
        if(is.null(col)) return(NULL)
        get_vector(x, col, rows, check_x_col_rows = FALSE)
    })
}




##' Insets target vector back to input object (`x`)
##' 
##' @param vector Character vector to inset into the `x` object
##' @param x Data to harmonize. Character vector or data.frame or
##'     data.table
##' @param omitted_rows_value_for_new_col Alternative value
##'     `omitted_rows_value` to use in case we create new column in
##'     x. For example, it is use in insetting codes to avoid the
##'     default `omitted_rows_value` use initial `col` in which case
##'     codes will be mixed with input values
##' @param allow_na_in_vector Whether to allow NA in inset vector
##' @param which_call_to_report System call number (e.g., -2L) to
##'     include in report if arguments checks fails
##' @param return_only_target_col If toggled to TRUE then only return
##'     the vector to be inset (output_placement argument is ignored)
##' @return Data.table or character vector
##' @inheritDotParams harmonize_options
inset_target <- function(vector, x
                       , omitted_rows_value_for_new_col = NULL
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
        ## inset omitted_rows_value if needed
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
            ## process `omitted_rows_value`
            x_names <- if (is.atomic(x)) x_atomic_name else names(x)
            omitted_rows_value_col <-
                infer_moving_target_from_names(
                    x_names
                  , col
                  , output_col_name
                  , output_placement
                  , return_null_for_new_col =
                        !is.null(omitted_rows_value_for_new_col))
            if(is.null(omitted_rows_value_col) &&
               is.null(omitted_rows_value)) {
                omitted_rows_value <- omitted_rows_value_for_new_col
            }
            omitted_rows_value <-
                get_vector(x
                         , col = omitted_rows_value_col
                         , fallback_value = omitted_rows_value
                         , fallback_value_supersedes = TRUE
                         , check_x_col_rows = FALSE)
            ## inject ommited rows
            vector <- `[<-`(omitted_rows_value, rows, vector)
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
        } else if(output_placement != "omit") {
            if(is.atomic(x) && output_placement == "replace_col") {
                ## just replace x if it is atomic
                x <- vector
            } else {
                x <- defactor(x, conv2dt = "all", x_atomic_name)
                width_pre_inset <- x_width(x)
                col_post_inset <- infer_post_inset_col_from_pre_inset_col(col, names(x), output_placement)
                col_or_name_if_new <-
                    infer_moving_target_from_names(names(x)
                                                 , col
                                                 , output_col_name
                                                 , output_placement
                                                 , return_name_for_new_col = TRUE)
                ## fuckin data.table syntax is so cryptic
                ## [] at the end ensures that returned DT is printed
                x[, (col_or_name_if_new) := vector][]
                ## x[[col_or_name_if_new]] <- vector
                ## now if we added new col
                if(x_width(x) == width_pre_inset + 1) {
                    ## if new col was added place last col into target posision
                    target <- infer_moving_target_from_post_inset_col(col_post_inset, names(x), output_placement)
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
        if(append_output_copy & !return_only_target_col) {
            x <- defactor(x, conv2dt = "all", x_atomic_name)
            col_post_inset <- infer_post_inset_col_from_pre_inset_col(col, names(x), output_placement)
            append_output_copy_name <- 
                format_col_name(col_name_format = output_copy_col_name
                              , col_name = names(x)[col_post_inset]
                              , x_names = names(x))
            checkmate::assert_names(append_output_copy_name, add = assertion_fails)
            report_arg_checks(assertion_fails, which_call_to_report)
            ## [] at the end ensures that returned DT is printed
            x[, (append_output_copy_name) := vector][]
        }
        report_arg_checks(assertion_fails, which_call_to_report)
        return(x)
    })
}
## --------<<  get_target & inset_target:2 ends here


