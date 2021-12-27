## -------->>  [[file:../harmonizer.src.org::*check_harmonize_options][check_harmonize_options:1]]
report_arg_checks <- function (collection
                             , report_for_call = sys.call(which = -2)) {
    checkmate::assertClass(collection, "AssertCollection")
    if (!collection$isEmpty()) {
        msgs = paste("-", collection$getMessages())
        context = "Harmonizer :: %i argument checks failed for '%s' call:"
        err = c("\n", strwrap(sprintf(context, length(msgs), deparse1(report_for_call)))
              , strwrap(msgs, indent = 4, exdent = 6))
        stop(simpleError(paste0(err, collapse = "\n"), call = sys.call(1L)))
    }
    invisible(TRUE)
}



check_col <- function(col, x
                    , missing_ok = FALSE
                    , null_ok = FALSE
                    , report_for_call = sys.call(which = -1)) {
    assertion_fails <- checkmate::makeAssertCollection()
    checkmate::assert_multi_class(col, c("character", "numeric"), add = assertion_fails)
    if (is.character(col)) {
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
                            , upper = x_length(x)
                            , na.ok = missing_ok
                            , null.ok = null_ok
                            , add = assertion_fails)
    }
    report_arg_checks(assertion_fails
                    , report_for_call)
}






check_x <- function(x, report_for_call = sys.call(which = -1)) {
    assertion_fails <- checkmate::makeAssertCollection()
    checkmate::assert_multi_class(x, c("character", "data.frame", "data.table"), add = assertion_fails)
    report_arg_checks(assertion_fails
                    , report_for_call)
}


##' Assumes that rows (if logical) are same length as x
##' @param rows either numeric of logical vector
##' @param x The object to harmonize
##' @param null_ok Whether NULL is valid value
##' @param na_ok Whether NA is valid value
##' @param report_for_call When reporting issues which function call to indicate for reference
##' @return nothing
check_rows <- function(rows, x
                     , null_ok = TRUE
                     , na_ok = FALSE
                     , report_for_call = sys.call(which = -1)) {
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
                    , report_for_call)
}


check_harmonize_options <- function(dots
                                  , x
                                  , report_for_call = sys.call(which = -1)
                                  , check_name_duplicates = FALSE) {
    ## check own arguments
    checkmate::assert_class(report_for_call, classes = "call")
    checkmate::assert_flag(check_name_duplicates)
    ## check harmonize_options
    assertion_fails <- checkmate::makeAssertCollection()
    with(dots, {
        ## check 'col'
        check_col(col, x)
        ## check 'rows'
        check_rows(rows, x)
        ## check 'ommitted_rows_values'
        if(length(ommitted_rows_values) == 1) {
            checkmate::assert_string(
                           ommitted_rows_values
                         , add = assertion_fails)
        } else {
            checkmate::assert_character(
                           ommitted_rows_values
                         , null.ok = TRUE
                         , len = x_length(x)
                         , add = assertion_fails)
        }
        ## check 'placement'
        checkmate::assert_choice(
                       placement
                     , choices = c(
                           "replace_col"
                         , "prepend_to_col"
                         , "append_to_col"
                         , "prepend_to_x"
                         , "append_to_x"
                         , "omit")
                     , add = assertion_fails)
        ## check 'name'
        checkmate::assert_string(
                       name
                     , null.ok = TRUE
                     , add = assertion_fails)
        if(check_name_duplicates && !is.null(name) && !is.atomic(x)) {
            checkmate::assert_names(
                           name
                         , type = "ids"
                         , what = "colnames"
                         , disjunct.from = names(x)
                         , add = assertion_fails)
        }
        ## check 'name_for_x_atomic'
        checkmate::assert_string(
                       name_for_x_atomic
                     , add = assertion_fails)
        checkmate::assert_names(
                       name_for_x_atomic
                     , type = "ids"
                     , what = "colnames"
                     , add = assertion_fails)
        ## check 'name_suffix'
        checkmate::assert_string(
                       name_suffix
                     , add = assertion_fails)
        ## check if col_name + 'name_suffix' is distinct from names(x)
        if(check_name_duplicates && !is.atomic(x) && is.null(name)) {
            checkmate::assert_names(
                           paste0(names(x)[[col]], name_suffix)
                         , type = "ids"
                         , what = "colnames"
                         , disjunct.from = names(x)
                         , add = assertion_fails)
        }
        ## check 'append_copy'
        checkmate::assert_flag(
                       append_copy
                     , add = assertion_fails)
        ## check 'append_copy_name_format'
        checkmate::assert_string(
                       append_copy_name_format
                     , min.chars = 1
                     , add = assertion_fails)
    })
    report_arg_checks(assertion_fails
                    , report_for_call)
}
## --------<<  check_harmonize_options:1 ends here


