## -------->>  [[file:../harmonizer.src.org::*detect_patterns][detect_patterns:1]]
##' transpose list of vectors
transpose_list_of_vectors <- function(l) {
    do.call(mapply, c(FUN = list(c), l, list(USE.NAMES = FALSE, SIMPLIFY = FALSE)))
}

and_rows <- function(rows_1, rows_2, x) {
    if(is.null(rows_1)) return(rows_2)
    if(is.null(rows_2)) return(rows_1)
    if(is.numeric(rows_1)) rows_1 <- 1:x_length(x) %in% rows_1
    if(is.numeric(rows_2)) rows_2 <- 1:x_length(x) %in% rows_2
    rows_1 & rows_2
}

or_rows <- function(rows_1, rows_2, x) {
    if(is.null(rows_1)) return(rows_1)
    if(is.null(rows_2)) return(rows_2)
    if(is.numeric(rows_1)) rows_1 <- 1:x_length(x) %in% rows_1
    if(is.numeric(rows_2)) rows_2 <- 1:x_length(x) %in% rows_2
    rows_1 | rows_2
}

#' Codes strings (e.g., organizational names) based on certain patterns
#'
#' @param x Vector or table to detect in.
#' @param patterns Accepts both vector or table. If patterns it is table can also include replacements column.
#' @param patterns_col If patterns is a table this specifies which column to use. Default is 1.
#' @param patterns_codes_col If patterns is table which column to use as codes column. Default is 2.
#' @param patterns_type Specifies kind(s) of patterns. Default is "fixed" (calling code{\link[stringi]{stri_replace_all_fixed}}). Other options are:
#' @param patterns_type_col Column in patterns table where you can specify types of patterns for each pattern. If set then `patterns.type` is ignored. Default is NULL.
#' @param patterns_codes If provided use it as codes. Should be the same length as patterns. Default is NULL.
#' @param output_codes_col_name 
#' @param codes_omitted_rows_value If `rows` is set. Use this value to fill the omitted rows. When we update existing codes column Default is NULL which means that we use initial codes values for omitted rows. If there is no codes col to update omited rows will be filled with NA.
#' @param no_match_code If provided code records that did not get any match with it. 
#' @param merge_existing_codes Whether to merge newly detected codes with existing. Options are:
#' @eval detect_patterns(return_merge_codes_description = TRUE)
#' @param codes_col_name If provided use it as a name for codes column (new if it does not exist or the one to update) in results.
#' @param codes_col_name_suffix If `codes_col_name` is not provided use this suffix to append to `col` name or `x_atomic_name`  (see`harmonize_options`).
#' @param return_only_codes If toggled on then just return codes vector.
#' @param return_only_first_detected_code If TRUE then return only codes for the first detected pattern. If FALSE return list of all matched codes. Default is TRUE. (Currently does affect performance)
#' 
#' @eval escape_regex_for_type(return_docs = TRUE)
#' @inheritDotParams harmonize_options
#' 
#' @return The updated `x` table with codes column or just codes if `return_only_codes` is set.
#'
#' @import stringi stringr magrittr
#' 
#' @export
detect_patterns <- function(x
                          , patterns
                          , patterns_col = 1
                          , patterns_codes_col = 2
                          , patterns_type = "fixed"
                          , patterns_type_col = NULL
                          , patterns_codes = NULL
                          , output_codes_col_name = "{col_name_}coded"
                          , codes_omitted_rows_value = NULL
                          , no_match_code = NULL
                          , merge_existing_codes = "replace_all"
                          , return_only_codes = FALSE
                          , return_only_first_detected_code = FALSE
                          , return_merge_codes_description = FALSE 
                          , ...) {
    ## check some arguments that are not checked elsewhere
    merge_existing_codes_choices =
        c("replace_all" = "Replace everything (entire column) with new codes."
        , "replace_empty" = "Code only records (i.e., rows) for which existing codes are empty (i.e., empty string, NA, empty list)"
        , "append_to_existing" = "Merge with existing codes appending new ones to the end"
        , "prepend_to_existing" = "Merge with existing codes prepending new ones to the front")
    checkmate::assert_choice(merge_existing_codes
                           , choices = names(merge_existing_codes_choices))
    if(return_merge_codes_description) return(mapply(\(desc, name) paste("*", name, "-", desc)
                                                   , merge_existing_codes_choices
                                                   , names(merge_existing_codes_choices)))
    checkmate::assert_atomic(no_match_code, min.len = 0, max.len = 1)
    checkmate::assert_flag(return_only_codes)
    checkmate::assert_flag(return_only_first_detected_code)
    ## set rows and excule those that coded
    rows <- get_col_and_rows()$rows
    if(merge_existing_codes == "replace_empty") {
        ## why does it returns previous values without forcing x? magic?
        force(x)
        rows <- get_target(x
                         , rows = NULL
                         , output_col_name = output_codes_col_name
                         , output_placement = "append_to_x"
                         , return_null_for_new_col = TRUE) |>
            harmonize_is_data_empty() |>
            and_rows(rows, x)
    }
    ## --------------------------------------------------------------------------------
    ## get vectors
    x_vector <- get_target(x, rows = rows)
    ## check if there is something to code
    if(length(x_vector) == 0) return(x)
    ## types (choices are checked in escape_regex_for_types)
    types_vector <- get_vector(patterns, patterns_type_col
                             , fallback_value = patterns_type)
    if(length(types_vector) == 0) return(x)
    ## get patterns excaped according to types.vector
    patterns_vector <-
        get_vector(patterns, patterns_col) |>
        escape_regex_for_types(types_vector)
    if(length(patterns_vector) == 0) return(x)
    ## get codes
    codes_vector <- get_vector(patterns, patterns_codes_col
                             , fallback_value = patterns_codes
                             , fallback_value_supersedes = TRUE)
    if(length(patterns_vector) == 0) return(x)
    ## --------------------------------------------------------------------------------
    ## detect and clean up
    x_inset_vector <-
        mapply(\(pattern, pattern_type, code)
            switch(pattern_type
                 , "fixed" = stringi::stri_detect_fixed(x_vector, pattern)
                 , "exact" = x_vector == pattern
                 , "trim_exact" = stringi::stri_trim_both(x_vector) == pattern
                 , stringi::stri_detect_regex(x_vector, pattern)
                   ) |> ifelse(code, NA)
             , patterns_vector
             , types_vector
             , codes_vector
             , SIMPLIFY = FALSE
             , USE.NAMES = FALSE) |>
        transpose_list_of_vectors() |>
        lapply(harmonize_omit_empty)
    ## code unmached records if needed
    if(!is.null(no_match_code)) {
        x_inset_vector <- 
            ifelse(lapply(x_inset_vector, length) == 0
                 , no_match_code
                 , x_inset_vector)
    }
    ## check if only first detected code is needed
    if(return_only_first_detected_code) {
        x_inset_vector <-
            lapply(x_inset_vector
                 , \(x) if(length(x) > 1) x[1] else x)
    }     ## merge if there is something to merge with
    if(merge_existing_codes %in% c("append_to_existing", "prepend_to_existing")) {
        x_codes_vector <- get_target(x
                                   , rows = rows
                                   , output_col_name = output_codes_col_name
                                   , output_placement = "append_to_x"
                                   , return_null_for_new_col = TRUE)
        if(!is.null(x_codes_vector)) {
            if(merge_existing_codes == "prepend_to_existing") {
                x_inset_vector <- mapply(c, x_codes_vector, x_inset_vector
                                       , SIMPLIFY = FALSE
                                       , USE.NAMES = FALSE)
            } else {
                x_inset_vector <- mapply(c, x_inset_vector, x_codes_vector
                                       , SIMPLIFY = FALSE
                                       , USE.NAMES = FALSE)
            }
        }
    }
    x_inset_vector <- unlist_if_possible(x_inset_vector)
    ## append codes (if prepend allowed it will break target col inference)
    inset_target(x_inset_vector, x
               , rows = rows
               , output_placement = "append_to_x"
               , output_col_name = output_codes_col_name
               , omitted_rows_value = codes_omitted_rows_value
               , omitted_rows_value_for_new_col = NA_character_
               , return_only_target_col = return_only_codes)
}
## --------<<  detect_patterns:1 ends here


