## -------->>  [[file:../harmonizer.src.org::*inset_vector][inset_vector:2]]
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
## --------<<  inset_vector:2 ends here


