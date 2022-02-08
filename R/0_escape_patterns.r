## -------->>  [[file:../harmonizer.src.org::*harmonize_escape_regex][harmonize_escape_regex:1]]
##' Escapes special for regex characters
##' @param string character vector
##' @return character vector with all special to regex characters escaped
##'
##' @export
escape_regex <- function(string) stringi::stri_replace_all_regex(string, "(\\W)", "\\\\$1")


##' Escapes special for different types of pattern
##' @param string character vector (patterns)
##' @param type How the characters in patterns should be 'escaped' for
##'     proper matching. The options are:
##' @param escape_fixed Whether to escape string for 'fixed' and
##'     'exact' types and just trimmed string for "trim_exact". Default is FALSE
##' @param return_docs Return character vector of documentation for
##'     each type (for using with roxygen2 documentations in many
##'     places)
##' @param return_choices Return character vector of available escape
##'     types
##' @return character vector with all special to regex characters
##'     escaped
##' @eval escape_regex_for_type(return_docs = TRUE)
##' @md
escape_regex_for_type <- function(string
                                , type
                                , escape_fixed = FALSE
                                , return_docs = FALSE
                                , return_choices = FALSE) {
    types_specs <- list(
        list(type = "fixed"
           , docs = "Match pattern string as it is within the target vector"
           , func = \(string, escape_fixed) {
               if(escape_fixed) escape_regex(string)
               else string
           })
      , list(type = "begins"
           , docs = "Match pattern string as it is in the beggining of the target vector"
           , func = \(string, escape_fixed) {
               paste0("^", escape_regex(string))
           })
      , list(type = "trim_begins"
           , docs = "Match pattern string as it is in the beginning of the target vector ignoring preceding white-spaces"
           , func = \(string, escape_fixed) {
               paste0("^\\s*", escape_regex(string))
           })
      , list(type = "ends"
           , docs = "Match pattern string as it is in the end of the target vector"
           , func = \(string, escape_fixed) {
               paste0(escape_regex(string), "$")
           })
      , list(type = "trim_ends"
           , docs = "Match pattern string as it is in the end of the target vector ignoring leading white-spaces"
           , func = \(string, escape_fixed) {
               paste0(escape_regex(string), "\\s*$")
           })
      , list(type = "exact"
           , docs = "Match pattern string exactly (i.e., match equal strings)"
           , func = \(string, escape_fixed) {
               if(escape_fixed) paste0("^", escape_regex(string), "$")
               else string
           })
      , list(type = "trim_exact"
           , docs = "Match pattern string exactly (i.e., match equal strings) ignoring surrounding white-spaces"
           , func = \(string, escape_fixed) {
               if(escape_fixed) paste0("^\\s*", escape_regex(string), "\\s*$")
               else stringi::stri_trim_both(string)
           })
      , list(type = "regex"
           , docs = "Match regex pattern"
           , func = \(string, escape_fixed) {
               string
           }))
    if(return_docs) {
        return(sapply(types_specs, \(x) paste("* ", x$type, " - ", x$docs)))
    }
    if(return_choices) {
        return(sapply(types_specs, `[[`, "type"))
    }
    types_funcs <- lapply(types_specs, `[[`, "func")
    names(types_funcs) <- lapply(types_specs, `[[`, "type")
    ## this will return the right func and call it
    do.call(switch, c(list(EXPR = type), types_funcs))(string, escape_fixed)
}

##' Escapes special for regex characters conditionally
##' @param patterns character vector
##' @param escape_fixed Whether to escape strings for 'fixed' and
##'     'exact' types in case all types are the same
##'     (length(unique(types)) == 1). The "trim_exact" types get
##'     trimed if not escaped. Otherwise if types heterogenious
##' @param types character vector of the same length as `strings` with
##'     instructions whether and how to to escape regex
##'     characters. Options are:
##' @eval escape_regex_for_type(return_docs = TRUE)
##' @return string with all special to regex characters escaped
##'
##' @import stringr
escape_regex_for_types <- function(patterns, types, escape_fixed = FALSE) {
    choices <- escape_regex_for_type(return_choices = TRUE)
    if(length(types) == 1 || length(unique(types)) == 1) {
        type <- types[1]
        checkmate::assert_choice(type, choices, fmatch = TRUE)
        escape_regex_for_type(patterns, type, escape_fixed = escape_fixed)
    } else {
        checkmate::assert_subset(types, choices, fmatch = TRUE)
        mapply(\(p, t) escape_regex_for_type(p, t, escape_fixed = TRUE)
             , patterns , types
             , SIMPLIFY = TRUE)
    }
}
## --------<<  harmonize_escape_regex:1 ends here


