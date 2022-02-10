## -------->>  [[file:../harmonizer.src.org::*replace_patterns][replace_patterns:1]]
replace_exact <- function(x
                        , patterns
                        , replacements) {
    if ((requireNamespace("fastmatch", quietly = TRUE))) {
        match <- fastmatch::fmatch
    }
    matches <- match(x, patterns)
    x[!is.na(matches)] <- replacements[matches][!is.na(matches)]
    return(x)
}

stri_replace_do <- function(str, arg_list) {
    do.call(stringi::stri_replace, c(list(str), arg_list))
}

replace_fixed_by_mode <- function(x
                                , patterns
                                , replacements
                                , modes) {
    Reduce(stri_replace_do
             , Map(list
                 , fixed = patterns
                 , replacement = replacements
                 , mode = modes)
             , init = x)
}

replace_regex_by_mode <- function(x
                                , patterns
                                , replacements
                                , modes) {
    Reduce(stri_replace_do
             , Map(list
                 , regex = patterns
                 , replacement = replacements
                 , mode = modes)
             , init = x)
}



##' A wrapper for string replacement and cbinding some columns.
##'
##' Optionally matches only at the beginning or at the end of the
##' string.
##' 
##' @param x Vector or table to harmonize.
##' @param patterns Accepts both vector or table. If patterns is a
##'     table can also include replacements column.
##' @param patterns_col If patterns is not a vector which column to
##'     use. Default is 1.
##' @param patterns_mode Mode of matching. Could be one of c("all",
##'     "first", "last"). The default is "all" (it is 2x faster than
##'     "first" and "last" because of handy stri_replace_all_*
##'     functions). Also possible to pass a vector (same length as
##'     patterns)
##' @param patterns_mode_col Column in patterns table with the mode of
##'     matching
##' @param patterns_type Type of pattern for matching. Default is
##'     "fixed" (calling
##'     code{\link[stringi]{stri_replace_all_fixed}}). Other options
##'     are:
##' @eval escape_regex_for_type(return_docs = TRUE)
##' @param patterns_type_col Column with the type of pattern in case
##'     when patterns should have different types
##' @param patterns_replacements_col If patterns is not a vector and
##'     includes replacements which column to use for
##'     replacements. Default is 2.
##' @param replacements If patterns does not have column with
##'     replacements provide it here.
##' @inheritDotParams harmonize_options
##'
##' @return If nothing was indicated to cbind to results then it
##'     returns harmonized vector. If something is needs to be cbind
##'     then it returns data.table
##' @import stringi stringr magrittr
##' 
##' @export
replace_patterns <- function(x
                            , patterns
                            , patterns_col = 1
                            , patterns_mode = "all"
                            , patterns_mode_col = NULL
                            , patterns_type = "fixed"
                            , patterns_type_col = NULL
                            , patterns_replacements_col = 2
                            , replacements = if(is.atomic(patterns)) "" else NULL
                            , ...) {
    ## get vectors and check arguments
    x_vector <- get_target(x)
    ## types (choices are checked in escape_regex_for_types)
    types_vector <- get_vector(patterns, patterns_type_col
                             , fallback_value = patterns_type)
    ## get patterns and escape regex if types are heterogeneous
    patterns_vector <-
        get_vector(patterns, patterns_col) |>
        escape_regex_for_types(types_vector)
    ## modes
    modes_vector <- get_vector(patterns, patterns_mode_col
                             , fallback_value = patterns_mode
                             , choices = c("all", "first", "last"))
    ## replacements
    replacements_vector <- get_vector(patterns, patterns_replacements_col
                                    , fallback_value = replacements
                                    , fallback_value_supersedes = TRUE)
    ## conditions are organized from fastest to slowest replace procedures
    if (all(types_vector == "exact") || all(types_vector == "trim_exact")) {
        if (all(types_vector == "trim_exact")) {
            x_vector <- stringi::stri_trim_both(x_vector)
        }
        x_vector <- replace_exact(x_vector
                                , patterns_vector
                                , replacements_vector)
    } else if (all(modes_vector == "all")) {
        if (all(types_vector == "fixed")) {
            x_vector <-
                stringi::stri_replace_all_fixed(x_vector
                                              , patterns_vector
                                              , replacements_vector
                                              , vectorize_all = FALSE)
        } else {
            x_vector <-
                stringi::stri_replace_all_regex(x_vector
                                              , patterns_vector
                                              , replacements_vector
                                              , vectorize_all = FALSE)
        }
    } else if (all(types_vector == "fixed")) {
        x_vector <- replace_fixed_by_mode(x_vector
                            , patterns_vector
                            , replacements_vector
                            , modes_vector)
    } else {
        x_vector <- replace_regex_by_mode(x_vector
                                        , patterns_vector
                                        , replacements_vector
                                        , modes_vector)
    }
    inset_target(x_vector, x)
}
## --------<<  replace_patterns:1 ends here


