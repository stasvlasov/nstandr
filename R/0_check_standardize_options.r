## -------->>  [[file:../nstandr.src.org::*check_standardize_options][check_standardize_options:1]]
report_arg_checks <- function (collection
                             , which_call_to_report = -2L
                             , call_to_report = NULL) {
    checkmate::assertClass(collection, "AssertCollection")
    if (!collection$isEmpty()) {
        msgs <- paste("-", collection$getMessages())
        context <- "nstandr :: %i argument checks failed in '%s' call:"
        if(is.call(try(
            call <- sys.call(which_call_to_report)
          , silent = TRUE))) {
            call_to_report <- deparse1(call)
        }
        err = c("\n", strwrap(sprintf(context, length(msgs), call_to_report))
              , strwrap(msgs, indent = 4, exdent = 6))
        stop(simpleError(paste0(err, collapse = "\n"), call = sys.call(1L)))
    }
    invisible(TRUE)
}

check_col <- function(col, x
                    , which_call_to_report = -1L
                    , missing_ok = FALSE
                    , null_ok = FALSE) {
    assertion_fails <- checkmate::makeAssertCollection()
    checkmate::assert_multi_class(col, c("character", "integer", "numeric"), add = assertion_fails)
    if (is.character(col)) {
        checkmate::assert_multi_class(x, c("data.frame", "data.table"), add = assertion_fails)
        checkmate::assert_character(col
                                  , min.chars = 1
                                  , len = 1
                                  , any.missing = missing_ok
                                  , null.ok = null_ok
                                  , add = assertion_fails)
        checkmate::assert_choice(col
                               , names(x)
                               , null.ok = null_ok
                               , add = assertion_fails)
    } else if(is.numeric(col)) {
        checkmate::assert_int(col
                            , lower = 1
                            , upper = x_width(x)
                            , na.ok = missing_ok
                            , null.ok = null_ok
                            , add = assertion_fails)
    }
    report_arg_checks(assertion_fails
                    , which_call_to_report)
}






check_x <- function(x, which_call_to_report = -1L) {
    assertion_fails <- checkmate::makeAssertCollection()
    checkmate::assert_multi_class(x, c("character", "data.frame", "data.table"), add = assertion_fails)
    report_arg_checks(assertion_fails
                    , which_call_to_report)
}


##' Assumes that rows (if logical) are same length as x
##' @param rows either numeric of logical vector
##' @param x The object to standardize
##' @param null_ok Whether NULL is valid value
##' @param na_ok Whether NA is valid value
##' @param which_call_to_report When reporting issues which function call to indicate for reference
##' @return nothing
check_rows <- function(rows, x
                     , which_call_to_report = -1L
                     , null_ok = TRUE
                     , na_ok = FALSE) {
    assertion_fails <- checkmate::makeAssertCollection()
    checkmate::assert_multi_class(rows
                                , classes = c("logical", "numeric")
                                , null.ok = null_ok
                                , add = assertion_fails)
    if(is.logical(rows)) {
        checkmate::assert_logical(rows
                                , any.missing = na_ok
                                , len = x_length(x)
                                , null.ok = null_ok
                                , add = assertion_fails)
    } else if(is.numeric(rows)) {
        checkmate::assert_integerish(rows
                                   , any.missing = na_ok
                                   , min.len = 1
                                   , max.len = x_length(x)
                                   , unique = TRUE
                                   , lower = 1
                                   , upper = x_length(x)
                                   , null.ok = null_ok
                                   , add = assertion_fails)
    }
    report_arg_checks(assertion_fails
                    , which_call_to_report)
}


check_standardize_options <- function(dots
                                  , x
                                  , which_call_to_report = -1L
                                  , check_name_duplicates = FALSE) {
    ## check own arguments
    checkmate::assert_int(which_call_to_report)
    checkmate::assert_flag(check_name_duplicates)
    ## check standardize_options
    assertion_fails <- checkmate::makeAssertCollection()
    with(dots, {
        ## check x
        check_x(x, which_call_to_report)
        ## check 'col'
        if(!is.atomic(x)) {
            check_col(col, x, which_call_to_report)
        }
        ## check 'rows'
        check_rows(rows, x, which_call_to_report)
        ## check 'omitted_rows_value'
        if(length(omitted_rows_value) == 1) {
            checkmate::assert_string(
                           omitted_rows_value
                         , na.ok = TRUE
                         , add = assertion_fails)
        } else {
            checkmate::assert_character(
                           omitted_rows_value
                         , null.ok = TRUE
                         , len = x_length(x)
                         , add = assertion_fails)
        }
        ## check 'placement'
        checkmate::assert_choice(
                       output_placement
                     , choices = c(
                           "replace_col"
                         , "prepend_to_col"
                         , "append_to_col"
                         , "prepend_to_x"
                         , "append_to_x"
                         , "omit")
                     , add = assertion_fails)
        ## check 'output_col_name'
        checkmate::assert_string(
                       output_col_name
                     , add = assertion_fails)
        if(check_name_duplicates && !is.atomic(x)) {
            output_col_name_formated <-
                infer_moving_target_from_names(names(x), col, output_col_name, output_placement)
            checkmate::assert_names(
                           output_col_name_formated
                         , type = "ids"
                         , what = "colnames"
                         , disjunct.from = names(x)
                         , add = assertion_fails)
        }
        ## check  x_atomic_name'
        checkmate::assert_string(
                       x_atomic_name
                     , add = assertion_fails)
        checkmate::assert_names(
                       x_atomic_name
                     , type = "ids"
                     , what = "colnames"
                     , add = assertion_fails)
        ## check 'output_col_name'
        checkmate::assert_string(
                       output_col_name
                     , add = assertion_fails)
        ## check if col_name + 'output_col_name' is distinct from names(x)
        if(check_name_duplicates && !is.atomic(x) && is.null(name)) {
            checkmate::assert_names(
                           paste0(names(x)[[col]], output_col_name)
                         , type = "ids"
                         , what = "colnames"
                         , disjunct.from = names(x)
                         , add = assertion_fails)
        }
        ## check 'append_output_copy'
        checkmate::assert_flag(
                       append_output_copy
                     , add = assertion_fails)
        ## check 'output_copy_col_name'
        checkmate::assert_string(
                       output_copy_col_name
                     , min.chars = 1
                     , add = assertion_fails)
    })
    report_arg_checks(assertion_fails
                    , which_call_to_report)
}
## --------<<  check_standardize_options:1 ends here


