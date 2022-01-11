## -------->>  [[file:../harmonizer.src.org::*harmonize.detect][harmonize.detect:1]]
##' transpose list of vectors
transpose_list_of_vectors <- function(l) {
    do.call(Map, c(f = list(c), l))
}


#' This function is basically meant for coding names based on certain pattern
#'
#' Optionally matches only at the beginning or at the end of the string.
#' 
#' @param x Vector or table to detect in.
#' @param patterns Accepts both vector or table. If patterns it is table can also include replacements column.
#' @param patterns.col If patterns is not a vector specifies which column to use. Default is 1.
#' @param patterns.as.codes If set then use patterns as codes. In that case codes are ignored and there is no need to provide them. Default is FALSE.
#' @param patterns.codes.col If patterns is table which column to use as codes column. Default is 2.
#' @param patterns.type Specifies kind(s) of patterns. Default is "fixed" (calling code{\link[stringi]{stri_replace_all_fixed}}). Other options are "begins", "ends" - which means that it should only match fixed pattern at the beginning of the string or at the and. Another possible value is "regex" (calling code{\link[stringi]{stri_replace_all_regex}}). Another possible options are "exact" and "trim.exact" which matches a pattern string exactly or its trimmed version (i.e., white spaces are ignored at the beginning and at the end of the pattern)
#' @param patterns.type.col Column in patterns table where you can specify types of patterns for each pattern. If set then `patterns.type` is ignored. Default is NULL
#' @param codes If provided use it as codes. Should be the same length as patterns. Default is NULL.
#' @param codes.name If provided use it as a name for codes column in results. It also will work as `x.codes.col` if `x.codes.merge` or `x.codes.update.empyty` are set. In case if `x.codes.merge` or `x.codes.update.empyty` are set and names(`x`) does not have column name `codes.name` it will reset both `x.codes.merge` and `x.codes.update.empyty` to FALSE and will (pre)append a new column `codes.name`.
#' @param codes.suffix If `codes.name` is not provided use this suffix to append to `x.col` name or `x.atomic.name` if x is vector (see harmonize.x function).
#' @param codes.omitted.val If `x.rows` is set. Use this value to fill the omitted rows. Default is vector of NAs of `x` length. When we update existing codes column (i.e., `x.codes.col` is set and valid) and the value is not set explicitly it resets to NULL which means that we use initial values for omitted rows.
#' @param x.codes.col If `x` is table, which column to use for making/merging/adding newly detected codes. Default is NULL. When it is not set explicitly and `x.codes.merge` or `x.codes.update.empty` are set it will first try to use `codes.name` if it is provided or if not will set the last column of `x` as `x.codes.col`.
#' @param x.codes.update.empty If set then detect and add new codes only for records (rows) that were not yet coded (i.e., related codes are either "", NA or length == 0).
#' @param x.codes.merge If set then merge (append or prepend) new codes to existing ones. It might turn the codes column to list of vectors.
#' @param x.codes.merge.prepend If set then it will prepend new codes if not then it will append (default). It is ignored if `x.codes.merge` is not set.
#' @param return.only.codes If set then just return codes vector. Default is FALSE. Basically when it is set it changes `return.x.cols` to 0.
#' @param return.only.first.detected.code If TRUE then return only codes for the first detected pattern. If FALSE return list of vectors of all matched codes. Default is TRUE.
#' 
#' @inheritDotParams harmonize.x
#' 
#' @return The updated `x` table with codes column or just codes if `return.only.codes` is set.
#'
#' @import stringi stringr magrittr
#' 
#' @export
harmonize_detect <- function(x
                           , patterns
                           , patterns_col = 1
                           , patterns_codes_col = 2
                           , patterns_type = "fixed"
                           , patterns_type_col = NULL
                           , codes = NULL
                           , codes_name = NA
                           , codes_suffix = "coded"
                           , codes_omitted_val = NA
                           , x_codes_col = NULL
                           , x_codes_update_empty = FALSE
                           , x_codes_merge = FALSE
                           , x_codes_merge_prepend = FALSE
                           , return_only_codes = FALSE
                           , return_only_first_detected_code = FALSE
                           , ...) {
    ## set x.rows.codes.update for dots.and("x.rows")
    x_rows_codes_update <- rep(TRUE, harmonize_data_length(x)) # by defaults updates all codes
    ## harmonize.detect..check.args()                           # also sets x.rows.codes.update
    ## - check x.codes.col (should not be the same as x.col)
    if(harmonize_is_ok_col(x_codes_col, x
                         , ban.values = dots.default("x.col", 1))) {
        x_codes_col %<>% switch(is.numeric(.) + 1, match(., names(x)), .)
        ## use x.codes.col as codes.omitted.val if it is not set
        if(missing(codes.omitted.val)) codes.omitted.val <- NULL
    } else if(x_codes_merge | x_codes_update_empty) {
        ## set x.codes.col as last one
        x.codes.col <- harmonize_data_width(x)
        ## check codes.names just in case
        harmonize_is_ok_type(codes_name, x.length = 1
                           , type = "character"
                           , allow.null = FALSE)
        if(codes_name %in% names(x)) {
            ## set as x.codes.col as codes.name
            x.codes.col <- match(codes_name, names(x))
            if(missing(codes.omitted.val)) codes.omitted.val <- NULL
        } else if(!is.na(codes_name)) {
            ## if codes.name is provided and it is a new name then append codes
            x.codes.merge <- FALSE
            x.codes.update.empty <- FALSE
        } else {
            ## use x.codes.col as codes.omitted.val if it is not set
            if(missing(codes.omitted.val)) codes.omitted.val <- NULL
        }
    } else {
        ## set x.codes.col as last one anyway
        x.codes.col <- harmonize_data_width(x)
    }
    ## set x.rows.codes.update for dots.and("x.rows")
    if(x_codes_update_empty) {
        x.rows.codes.update <- harmonize_is_data_empty(x[[x_codes_col]])
        x.codes.merge <- FALSE # nothing to merge with if codes are empty
    }
    ## --------------------------------------------------------------------------------
    rows_to_code <-  get_options(select_args = "rows") & x_rows_codes_update
    x_vector <- get_target(x, rows = rows_to_code)
    ## check if there is something to code
    if(length(x_vector) == 0) return(character(0))
    ## types (choices are checked in escape_regex_for_types)
    types_vector <- get_vector(patterns, patterns_type_col
                             , fallback_value = patterns_type)
    ## get patterns excaped according to types.vector
    patterns_vector <-
        get_vector(patterns, patterns_col) |>
        escape_regex_for_types(types_vector)
    ## get codes
    codes_vector <- get_vector(patterns, patterns_codes_col
                             , fallback_value = codes
                             , fallback_value_ignored_if_col = FALSE)
    ## get existing codes (to add to)
    x_codes_vector <- get_vector(x, x_codes_col
                               , rows = rows_to_code
                               , fallback_value = codes_omitted_val)
    ## --------------------------------------------------------------------------------
    ## if there is something to detect in
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
        transpose_list_of_vectors |>
        ## remove empty codes
        lapply(harmonize_omit_empty)
    ## check if only first detected code is needed
    if(return_only_first_detected_code) {
        x_inset_vector <- lapply(x_inset_vector
                               , \(x) if(length(x) == 0) NULL
                                      else x[1])
    }
    ## check if we need to merge
    if(x_codes_merge) {
        if(x_codes_merge_prepend) {
            x_inset_vector <- Map(c, x_codes_vector, x_inset_vector)
        } else {
            x_inset_vector <- Map(c, x_inset_vector, x_codes_vector)
        }
    }

    x_inset_vector |>
        harmonize.unlist.column |>

    ## inset codes
    harmonize.x.dots(x, x_inset_vector
                   , x.rows = dots.and("x.rows", x_rows_codes_update)
                   , x.col = x_codes_col
                   , x.col.update = if(return_only_codes) FALSE
                                    else dots.default("x.col.update"
                                                    , x_codes_update_empty | x_codes_merge)
                   , inset.omitted.val = codes_omitted_val
                   , inset.name = codes_name
                   , inset.suffix = codes_suffix
                   , inset.append = dots.default("inset.append", !codes.prepend)
                   , return.x.cols = if(return_only_codes) NULL
                                     else dots.default("return.x.cols"
                                                     , 1:harmonize_data_width(x)))
    ## only append codes (if prepend allowed it will break target col inference)
    inset_target(x_inset_vector, x
               , rows = rows_to_code
               , col = codes_col)
}
## --------<<  harmonize.detect:1 ends here


